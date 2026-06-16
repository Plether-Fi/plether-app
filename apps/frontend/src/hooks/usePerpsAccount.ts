import { useMemo } from 'react'
import { type Address, zeroAddress } from 'viem'
import { useAccount, useReadContracts } from 'wagmi'
import {
  ERC20_ABI,
  PERPS_CFD_ENGINE_ABI,
  PERPS_CFD_ENGINE_ACCOUNT_LENS_ABI,
  PERPS_MARGIN_CLEARINGHOUSE_ABI,
  PERPS_ORDER_ROUTER_ABI,
  PERPS_PUBLIC_LENS_ABI,
} from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { formatDisplayDxyPrice, formatPerpsUsdc, formatSignedPerpsUsdc, oraclePriceToDisplayDxyPrice, perpsSideToDirection, sizeDeltaToNotionalUsdc } from '../utils/perps'

interface ContractResult {
  status: 'failure' | 'success'
  result?: unknown
}

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
  const { address, isConnected } = useAccount()
  const account = address ?? zeroAddress

  const { data, isLoading, error, refetch } = useReadContracts({
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
        functionName: 'riskParams',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'CAP_PRICE',
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
      enabled: isConnected,
      refetchInterval: 15_000,
    },
  })

  const basicPendingOrders = useMemo(
    () => parsePendingOrders(readResult(data, 2), markPrice),
    [data, markPrice]
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
      enabled: isConnected && basicPendingOrders.length > 0,
      refetchInterval: 15_000,
    },
  })

  return useMemo(() => {
    const accountView = readResult(data, 0)
    const position = parsePosition(readResult(data, 1), markPrice)
    const walletUsdc = readResult(data, 3) as bigint | undefined
    const marginAllowanceUsdc = readResult(data, 4) as bigint | undefined
    const freeBuyingPowerUsdc = readResult(data, 5) as bigint | undefined
    const maxPendingOrders = readResult(data, 6) as bigint | undefined
    const maxOrderAge = readResult(data, 7) as bigint | undefined
    const accountLedgerSnapshot = readResult(data, 8)
    const riskParams = readResult(data, 9)
    const capPrice = readResult(data, 10) as bigint | undefined
    const isFadWindow = readResult(data, 11) as boolean | undefined
    const enginePosition = readResult(data, 12)
    const withdrawableUsdc = tupleValue(accountView, 1, 'withdrawableUsdc') as bigint | undefined
    const equityUsdc = tupleValue(accountView, 0, 'equityUsdc') as bigint | undefined
    const terminalReachableUsdc = readBigInt(accountLedgerSnapshot, 12, 'terminalReachableUsdc')
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

    return {
      address: address as Address | undefined,
      isConnected,
      isLoading: isLoading || pendingOrderViewsLoading,
      error,
      refetch,
      walletUsdc,
      marginAllowanceUsdc,
      equityUsdc,
      freeBuyingPowerUsdc,
      withdrawableUsdc,
      pendingOrderMarginUsdc: tupleValue(accountView, 2, 'pendingOrderMarginUsdc') as bigint | undefined,
      pendingExecutionBountyUsdc: tupleValue(accountView, 3, 'pendingExecutionBountyUsdc') as bigint | undefined,
      maxPendingOrders,
      maxOrderAge,
      firstPendingOrderId,
      firstPendingOrderExpiryTime,
      hasOpenPosition: Boolean(tupleValue(accountView, 4, 'hasOpenPosition')) && Boolean(positionWithLiquidationPrice?.exists),
      liquidatable: Boolean(tupleValue(accountView, 5, 'liquidatable')) || Boolean(positionWithLiquidationPrice?.liquidatable),
      position: positionWithLiquidationPrice,
      pendingOrders,
      display: {
        walletUsdc: formatPerpsUsdc(walletUsdc),
        availableToTrade: formatPerpsUsdc(freeBuyingPowerUsdc ?? withdrawableUsdc),
        equity: formatPerpsUsdc(equityUsdc),
        positionNotional: formatPerpsUsdc(positionWithLiquidationPrice?.estimatedNotionalUsdc),
        entryPrice: formatDisplayDxyPrice(positionWithLiquidationPrice?.entryPrice),
        pnl: formatSignedPerpsUsdc(positionWithLiquidationPrice?.unrealizedPnlUsdc),
      },
    }
  }, [address, basicPendingOrders, data, error, isConnected, isLoading, markPrice, pendingOrderViewsData, pendingOrderViewsLoading, refetch])
}
