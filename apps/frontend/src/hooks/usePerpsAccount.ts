import { useCallback, useEffect, useMemo, useRef } from 'react'
import { zeroAddress } from 'viem'
import { useReadContracts } from 'wagmi'
import {
  ERC20_ABI,
  PERPS_CFD_ENGINE_ABI,
  PERPS_CFD_ENGINE_ACCOUNT_LENS_ABI,
  PERPS_MARGIN_CLEARINGHOUSE_ABI,
  PERPS_ORDER_ROUTER_ABI,
  PERPS_PUBLIC_LENS_ABI,
} from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { usePerpsIdentity } from '../perps-aa'
import { formatDisplayDxyPrice, formatPerpsUsdc, formatSignedPerpsUsdc, oraclePriceToDisplayDxyPrice, perpsSideToDirection, sizeDeltaToNotionalUsdc } from '../utils/perps'

interface ContractResult {
  status: 'failure' | 'success'
  result?: unknown
}

const PERPS_DYNAMIC_REFETCH_INTERVAL_MS = 15_000
const PERPS_CONFIG_STALE_TIME_MS = 5 * 60_000
const PERPS_CONFIG_GC_TIME_MS = Number.POSITIVE_INFINITY

export interface PerpsPendingOrder {
  orderId: bigint
  side: number
  direction: 'long' | 'short'
  sizeDelta: bigint
  marginDeltaUsdc: bigint
  acceptablePrice: bigint
  isReduceOnly: boolean
  status: number
  estimatedNotionalUsdc?: bigint
  commitTime?: bigint
  expiryTime?: bigint
}

export interface PerpsPosition {
  exists: boolean
  side: number
  direction: 'long' | 'short'
  size: bigint
  entryPrice: bigint
  marginUsdc: bigint
  unrealizedPnlUsdc: bigint
  maintenanceMarginUsdc: bigint
  liquidatable: boolean
  estimatedNotionalUsdc?: bigint
  entryNotionalUsdc?: bigint
  dxyExposureUsdc?: bigint
  displayDxyPrice?: bigint
  liquidationPrice?: bigint
  pendingCarryUsdc?: bigint
}

function readResult(data: readonly ContractResult[] | undefined, index: number): unknown {
  const item = data?.[index]
  if (item?.status !== 'success') return undefined
  return item.result
}

function tupleValue(value: unknown, index: number, key: string): unknown {
  if (value && typeof value === 'object' && key in value) {
    return (value as Record<string, unknown>)[key]
  }

  if (Array.isArray(value)) {
    return value[index]
  }

  return undefined
}

function parsePosition(value: unknown, markPrice?: bigint): PerpsPosition | undefined {
  if (!value) return undefined

  const exists = Boolean(tupleValue(value, 0, 'exists'))
  const side = Number(tupleValue(value, 1, 'side') ?? 0)
  const size = tupleValue(value, 2, 'size') as bigint | undefined ?? 0n
  const displayDxyPrice = oraclePriceToDisplayDxyPrice(markPrice)

  return {
    exists,
    side,
    direction: perpsSideToDirection(side),
    size,
    entryPrice: tupleValue(value, 3, 'entryPrice') as bigint | undefined ?? 0n,
    marginUsdc: tupleValue(value, 4, 'marginUsdc') as bigint | undefined ?? 0n,
    unrealizedPnlUsdc: tupleValue(value, 5, 'unrealizedPnlUsdc') as bigint | undefined ?? 0n,
    maintenanceMarginUsdc: tupleValue(value, 6, 'maintenanceMarginUsdc') as bigint | undefined ?? 0n,
    liquidatable: Boolean(tupleValue(value, 7, 'liquidatable')),
    estimatedNotionalUsdc: sizeDeltaToNotionalUsdc(size, markPrice),
    entryNotionalUsdc: sizeDeltaToNotionalUsdc(size, tupleValue(value, 3, 'entryPrice') as bigint | undefined ?? 0n),
    dxyExposureUsdc: sizeDeltaToNotionalUsdc(size, displayDxyPrice),
    displayDxyPrice,
  }
}

function parsePendingOrders(value: unknown, markPrice?: bigint): PerpsPendingOrder[] {
  if (!Array.isArray(value)) return []

  return value.map((item) => {
    const side = Number(tupleValue(item, 1, 'side') ?? 0)
    const sizeDelta = tupleValue(item, 2, 'sizeDelta') as bigint | undefined ?? 0n

    return {
      orderId: tupleValue(item, 0, 'orderId') as bigint | undefined ?? 0n,
      side,
      direction: perpsSideToDirection(side),
      sizeDelta,
      marginDeltaUsdc: tupleValue(item, 3, 'marginDeltaUsdc') as bigint | undefined ?? 0n,
      acceptablePrice: tupleValue(item, 4, 'acceptablePrice') as bigint | undefined ?? 0n,
      isReduceOnly: Boolean(tupleValue(item, 5, 'isReduceOnly')),
      status: Number(tupleValue(item, 6, 'status') ?? 0),
      estimatedNotionalUsdc: sizeDeltaToNotionalUsdc(sizeDelta, markPrice),
    }
  })
}

function parsePendingOrderCommitTime(value: unknown): bigint | undefined {
  const pending = tupleValue(value, 0, 'pending')
  const commitTime = tupleValue(pending, 6, 'commitTime')
  if (typeof commitTime === 'bigint') return commitTime
  if (typeof commitTime === 'number') return BigInt(commitTime)
  if (typeof commitTime === 'string') return BigInt(commitTime)
  return undefined
}

const USDC_TO_TOKEN_SCALE = 10n ** 20n

function readBigInt(value: unknown, index: number, key: string): bigint | undefined {
  const raw = tupleValue(value, index, key)
  if (typeof raw === 'bigint') return raw
  if (typeof raw === 'number') return BigInt(raw)
  if (typeof raw === 'string') return BigInt(raw)
  return undefined
}

function derivePendingCarryUsdc({
  terminalReachableUsdc,
  unrealizedPnlUsdc,
  netEquityUsdc,
  vpiAccrued,
}: {
  terminalReachableUsdc?: bigint
  unrealizedPnlUsdc?: bigint
  netEquityUsdc?: bigint
  vpiAccrued?: bigint
}): bigint | undefined {
  if (
    terminalReachableUsdc === undefined ||
    unrealizedPnlUsdc === undefined ||
    netEquityUsdc === undefined
  ) {
    return undefined
  }

  const vpiClawbackUsdc = vpiAccrued !== undefined && vpiAccrued < 0n ? -vpiAccrued : 0n
  const pendingCarryUsdc = terminalReachableUsdc - vpiClawbackUsdc + unrealizedPnlUsdc - netEquityUsdc

  return pendingCarryUsdc > 0n ? pendingCarryUsdc : 0n
}

function isLiquidatableAtPrice({
  capPrice,
  entryPrice,
  maintenanceMarginBps,
  side,
  size,
  terminalReachableUsdc,
  vpiAccrued,
  price,
}: {
  capPrice: bigint
  entryPrice: bigint
  maintenanceMarginBps: bigint
  side: number
  size: bigint
  terminalReachableUsdc: bigint
  vpiAccrued: bigint
  price: bigint
}): boolean {
  const clampedPrice = price > capPrice ? capPrice : price
  const isBull = side === 0
  const isProfit = isBull ? clampedPrice <= entryPrice : clampedPrice >= entryPrice
  const priceDiff = isProfit
    ? isBull ? entryPrice - clampedPrice : clampedPrice - entryPrice
    : isBull ? clampedPrice - entryPrice : entryPrice - clampedPrice
  const pnlUsdc = (size * priceDiff) / USDC_TO_TOKEN_SCALE
  const signedPnlUsdc = isProfit ? pnlUsdc : -pnlUsdc
  const vpiClawbackUsdc = vpiAccrued < 0n ? -vpiAccrued : 0n
  const equityUsdc = terminalReachableUsdc - vpiClawbackUsdc + signedPnlUsdc
  const currentNotionalUsdc = (size * clampedPrice) / USDC_TO_TOKEN_SCALE
  const maintenanceMarginUsdc = (currentNotionalUsdc * maintenanceMarginBps) / 10_000n

  return equityUsdc <= maintenanceMarginUsdc
}

function findLiquidationPrice({
  capPrice,
  entryPrice,
  maintenanceMarginBps,
  side,
  size,
  terminalReachableUsdc,
  vpiAccrued,
}: {
  capPrice: bigint | undefined
  entryPrice: bigint
  maintenanceMarginBps: bigint | undefined
  side: number
  size: bigint
  terminalReachableUsdc: bigint | undefined
  vpiAccrued: bigint | undefined
}): bigint | undefined {
  if (
    capPrice === undefined ||
    capPrice <= 0n ||
    maintenanceMarginBps === undefined ||
    maintenanceMarginBps <= 0n ||
    size <= 0n ||
    terminalReachableUsdc === undefined
  ) {
    return undefined
  }

  const liquidationArgs = {
    capPrice,
    entryPrice,
    maintenanceMarginBps,
    side,
    size,
    terminalReachableUsdc,
    vpiAccrued: vpiAccrued ?? 0n,
  }
  const liquidatableAtZero = isLiquidatableAtPrice({ ...liquidationArgs, price: 0n })
  const liquidatableAtCap = isLiquidatableAtPrice({ ...liquidationArgs, price: capPrice })

  if (side === 0) {
    if (!liquidatableAtCap) return undefined
    if (liquidatableAtZero) return 0n

    let low = 0n
    let high = capPrice
    while (low < high) {
      const mid = (low + high) / 2n
      if (isLiquidatableAtPrice({ ...liquidationArgs, price: mid })) {
        high = mid
      } else {
        low = mid + 1n
      }
    }
    return high
  }

  if (!liquidatableAtZero) return undefined
  if (liquidatableAtCap) return capPrice

  let low = 0n
  let high = capPrice
  while (low < high) {
    const mid = (low + high + 1n) / 2n
    if (isLiquidatableAtPrice({ ...liquidationArgs, price: mid })) {
      low = mid
    } else {
      high = mid - 1n
    }
  }
  return low
}

export function usePerpsAccount(markPrice?: bigint) {
  const {
    ownerAddress,
    accountAddress,
    status: identityStatus,
  } = usePerpsIdentity()
  const isConnected = ownerAddress !== undefined
  const account = accountAddress ?? zeroAddress
  const owner = ownerAddress ?? zeroAddress
  const lastSuccessfulPositionRef = useRef<{
    account: string
    position: PerpsPosition
  } | undefined>(undefined)

  const {
    data: dynamicContractData,
    isLoading: isDynamicContractsLoading,
    error: dynamicContractsError,
    refetch: refetchDynamicContracts,
  } = useReadContracts({
    contracts: [
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getTraderAccount',
        args: [account],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getPosition',
        args: [account],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getPendingOrders',
        args: [account],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.usdc,
        abi: ERC20_ABI,
        functionName: 'balanceOf',
        args: [account],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.usdc,
        abi: ERC20_ABI,
        functionName: 'balanceOf',
        args: [owner],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.usdc,
        abi: ERC20_ABI,
        functionName: 'allowance',
        args: [account, PERPS_ARBITRUM_SEPOLIA.marginClearinghouse],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.marginClearinghouse,
        abi: PERPS_MARGIN_CLEARINGHOUSE_ABI,
        functionName: 'getFreeBuyingPowerUsdc',
        args: [account],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngineAccountLens,
        abi: PERPS_CFD_ENGINE_ACCOUNT_LENS_ABI,
        functionName: 'getAccountLedgerSnapshot',
        args: [account],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'isFadWindow',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'positions',
        args: [account],
      },
    ],
    query: {
      enabled: isConnected && accountAddress !== undefined,
      refetchInterval: PERPS_DYNAMIC_REFETCH_INTERVAL_MS,
    },
  })
  // Engine configuration changes atomically behind a 48-hour timelock. Keep
  // this batch aligned with usePerpsMarket so both hooks share one cached read.
  const {
    data: engineConfigurationData,
    isLoading: isEngineConfigurationLoading,
    error: engineConfigurationError,
    refetch: refetchEngineConfiguration,
  } = useReadContracts({
    contracts: [
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'riskParams',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'executionFeeBps',
      },
    ],
    query: {
      enabled: isConnected && accountAddress !== undefined,
      staleTime: PERPS_CONFIG_STALE_TIME_MS,
      gcTime: PERPS_CONFIG_GC_TIME_MS,
      refetchOnWindowFocus: true,
      refetchOnReconnect: true,
    },
  })
  // Router configuration is finalized atomically by its own timelocked admin.
  const {
    data: routerConfigurationData,
    isLoading: isRouterConfigurationLoading,
    error: routerConfigurationError,
    refetch: refetchRouterConfiguration,
  } = useReadContracts({
    contracts: [
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'minOpenNotionalUsdc',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'maxPendingOrders',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'maxOrderAge',
      },
    ],
    query: {
      enabled: isConnected && accountAddress !== undefined,
      staleTime: PERPS_CONFIG_STALE_TIME_MS,
      gcTime: PERPS_CONFIG_GC_TIME_MS,
      refetchOnWindowFocus: true,
      refetchOnReconnect: true,
    },
  })
  // CAP_PRICE is an immutable constructor value for this engine deployment.
  const {
    data: immutableContractData,
    isLoading: isImmutableContractLoading,
    error: immutableContractError,
  } = useReadContracts({
    contracts: [
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'CAP_PRICE',
      },
    ],
    query: {
      enabled: isConnected && accountAddress !== undefined,
      staleTime: Number.POSITIVE_INFINITY,
      gcTime: Number.POSITIVE_INFINITY,
      refetchOnWindowFocus: false,
      refetchOnReconnect: false,
    },
  })

  const isLoading =
    isDynamicContractsLoading ||
    isEngineConfigurationLoading ||
    isRouterConfigurationLoading ||
    isImmutableContractLoading
  const error =
    dynamicContractsError ??
    engineConfigurationError ??
    routerConfigurationError ??
    immutableContractError
  const refetch = useCallback(
    () => Promise.all([
      refetchDynamicContracts(),
      refetchEngineConfiguration(),
      refetchRouterConfiguration(),
    ]),
    [refetchDynamicContracts, refetchEngineConfiguration, refetchRouterConfiguration]
  )

  const basicPendingOrders = useMemo(
    () => parsePendingOrders(readResult(dynamicContractData, 2), markPrice),
    [dynamicContractData, markPrice]
  )

  const { data: pendingOrderViewsData, isLoading: pendingOrderViewsLoading } = useReadContracts({
    contracts: basicPendingOrders.map((order) => ({
      chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
      address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'getPendingOrderView',
      args: [order.orderId],
    } as const)),
    query: {
      enabled: isConnected && accountAddress !== undefined && basicPendingOrders.length > 0,
      refetchInterval: PERPS_DYNAMIC_REFETCH_INTERVAL_MS,
    },
  })

  const freshAccount = useMemo(() => {
    const accountView = readResult(dynamicContractData, 0)
    const position = parsePosition(readResult(dynamicContractData, 1), markPrice)
    const tradingAccountUsdc = readResult(dynamicContractData, 3) as bigint | undefined
    const ownerWalletUsdc = readResult(dynamicContractData, 4) as bigint | undefined
    const marginAllowanceUsdc = readResult(dynamicContractData, 5) as bigint | undefined
    const freeBuyingPowerUsdc = readResult(dynamicContractData, 6) as bigint | undefined
    const accountLedgerSnapshot = readResult(dynamicContractData, 7)
    const isFadWindow = readResult(dynamicContractData, 8) as boolean | undefined
    const enginePosition = readResult(dynamicContractData, 9)
    const riskParams = readResult(engineConfigurationData, 0)
    const maxPendingOrders = readResult(routerConfigurationData, 1) as bigint | undefined
    const maxOrderAge = readResult(routerConfigurationData, 2) as bigint | undefined
    const capPrice = readResult(immutableContractData, 0) as bigint | undefined
    const withdrawableUsdc = tupleValue(accountView, 1, 'withdrawableUsdc') as bigint | undefined
    const equityUsdc = tupleValue(accountView, 0, 'equityUsdc') as bigint | undefined
    const terminalReachableUsdc = readBigInt(accountLedgerSnapshot, 12, 'terminalReachableUsdc')
    const traderClaimBalanceUsdc = readBigInt(accountLedgerSnapshot, 9, 'traderClaimBalanceUsdc')
    const snapshotUnrealizedPnlUsdc = readBigInt(accountLedgerSnapshot, 20, 'unrealizedPnlUsdc')
    const netEquityUsdc = readBigInt(accountLedgerSnapshot, 21, 'netEquityUsdc')
    const maintenanceMarginBps = isFadWindow
      ? readBigInt(riskParams, 4, 'fadMarginBps')
      : readBigInt(riskParams, 2, 'maintMarginBps')
    const vpiAccrued = readBigInt(enginePosition, 6, 'vpiAccrued')
    const pendingCarryUsdc = derivePendingCarryUsdc({
      terminalReachableUsdc,
      unrealizedPnlUsdc: snapshotUnrealizedPnlUsdc,
      netEquityUsdc,
      vpiAccrued,
    })
    const liquidationPrice = position?.exists
      ? findLiquidationPrice({
          capPrice,
          entryPrice: position.entryPrice,
          maintenanceMarginBps,
          side: position.side,
          size: position.size,
          terminalReachableUsdc,
          vpiAccrued,
        })
      : undefined
    const positionWithLiquidationPrice = position === undefined
      ? undefined
      : { ...position, liquidationPrice, pendingCarryUsdc }
    const pendingOrders = basicPendingOrders.map((order, index) => {
      const commitTime = parsePendingOrderCommitTime(readResult(pendingOrderViewsData, index))
      const expiryTime = commitTime !== undefined && maxOrderAge !== undefined
        ? commitTime + maxOrderAge
        : undefined

      return {
        ...order,
        commitTime,
        expiryTime,
      }
    })
    const firstPendingOrderExpiryTime = pendingOrders
      .map((order) => order.expiryTime)
      .filter((expiryTime): expiryTime is bigint => expiryTime !== undefined)
      .sort((a, b) => a < b ? -1 : a > b ? 1 : 0)[0]
    const firstPendingOrderId = pendingOrders
      .filter((order) => order.expiryTime !== undefined)
      .sort((a, b) => {
        const aExpiry = a.expiryTime ?? 0n
        const bExpiry = b.expiryTime ?? 0n
        return aExpiry < bExpiry ? -1 : aExpiry > bExpiry ? 1 : 0
      })[0]?.orderId
    const accountHasOpenPosition = accountView === undefined
      ? undefined
      : Boolean(tupleValue(accountView, 4, 'hasOpenPosition'))

    return {
      address: accountAddress,
      ownerAddress,
      accountAddress,
      identityStatus,
      isConnected,
      isLoading,
      isPendingOrderDetailsLoading: pendingOrderViewsLoading,
      error,
      refetchDynamic: refetchDynamicContracts,
      refetch,
      walletUsdc: ownerWalletUsdc,
      ownerWalletUsdc,
      tradingAccountUsdc,
      marginAllowanceUsdc,
      equityUsdc,
      freeBuyingPowerUsdc,
      withdrawableUsdc,
      traderClaimBalanceUsdc,
      pendingOrderMarginUsdc: tupleValue(accountView, 2, 'pendingOrderMarginUsdc') as bigint | undefined,
      pendingExecutionBountyUsdc: tupleValue(accountView, 3, 'pendingExecutionBountyUsdc') as bigint | undefined,
      maxPendingOrders,
      maxOrderAge,
      firstPendingOrderId,
      firstPendingOrderExpiryTime,
      accountHasOpenPosition,
      hasOpenPosition: Boolean(accountHasOpenPosition) && Boolean(positionWithLiquidationPrice?.exists),
      liquidatable: Boolean(tupleValue(accountView, 5, 'liquidatable')) || Boolean(positionWithLiquidationPrice?.liquidatable),
      position: positionWithLiquidationPrice,
      pendingOrders,
      display: {
        walletUsdc: formatPerpsUsdc(ownerWalletUsdc),
        ownerWalletUsdc: formatPerpsUsdc(ownerWalletUsdc),
        tradingAccountUsdc: formatPerpsUsdc(tradingAccountUsdc),
        availableToTrade: formatPerpsUsdc(freeBuyingPowerUsdc ?? withdrawableUsdc),
        equity: formatPerpsUsdc(equityUsdc),
        positionNotional: formatPerpsUsdc(positionWithLiquidationPrice?.estimatedNotionalUsdc),
        entryPrice: formatDisplayDxyPrice(positionWithLiquidationPrice?.entryPrice),
        pnl: formatSignedPerpsUsdc(positionWithLiquidationPrice?.unrealizedPnlUsdc),
      },
    }
  }, [accountAddress, basicPendingOrders, dynamicContractData, engineConfigurationData, error, identityStatus, immutableContractData, isConnected, isLoading, markPrice, ownerAddress, pendingOrderViewsData, pendingOrderViewsLoading, refetch, refetchDynamicContracts, routerConfigurationData])

  useEffect(() => {
    if (!isConnected || freshAccount.position === undefined) return

    lastSuccessfulPositionRef.current = {
      account,
      position: freshAccount.position,
    }
  }, [account, freshAccount.position, isConnected])

  const stablePosition = freshAccount.position ?? (
    freshAccount.accountHasOpenPosition !== false &&
    lastSuccessfulPositionRef.current?.account === account
      ? lastSuccessfulPositionRef.current.position
      : undefined
  )

  return useMemo(() => {
    const { accountHasOpenPosition, ...accountData } = freshAccount

    return {
      ...accountData,
      hasOpenPosition: accountHasOpenPosition === undefined
        ? Boolean(stablePosition?.exists)
        : accountHasOpenPosition && Boolean(stablePosition?.exists),
      liquidatable: freshAccount.liquidatable || Boolean(stablePosition?.liquidatable),
      position: stablePosition,
      display: {
        ...freshAccount.display,
        positionNotional: formatPerpsUsdc(stablePosition?.estimatedNotionalUsdc),
        entryPrice: formatDisplayDxyPrice(stablePosition?.entryPrice),
        pnl: formatSignedPerpsUsdc(stablePosition?.unrealizedPnlUsdc),
      },
    }
  }, [freshAccount, stablePosition])
}
