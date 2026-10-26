import { useCallback, useEffect, useMemo, useRef } from 'react'
import { multicall3Abi, zeroAddress } from 'viem'
import { arbitrumSepolia } from 'viem/chains'
import { parsePositionProtection } from '../contracts/positionProtection'
import { useReadContracts } from 'wagmi'
import {
  ERC20_ABI,
  PERPS_CFD_ENGINE_ABI,
  PERPS_CFD_ENGINE_ACCOUNT_LENS_ABI,
  PERPS_HOUSE_POOL_ABI,
  PERPS_MARGIN_CLEARINGHOUSE_ABI,
  PERPS_ORDER_LIFECYCLE_BOOK_ABI,
  PERPS_ORDER_ROUTER_ABI,
  PERPS_PUBLIC_LENS_ABI,
} from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { usePerpsIdentity } from '../perps-aa'
import { findLiquidationThreshold, projectCarry, type LiquidationThreshold } from '../utils/perpsRisk'
import { useInvalidatePerpsSnapshot, usePerpsSnapshotInvalidated } from './usePerpsSnapshotInvalidated'
import { calculatePendingCarryAssets } from '../utils/perpsCarry'
import { formatDisplayDxyPrice, formatPerpsAssets, formatSignedPerpsAssets, oraclePriceToDisplayDxyPrice, perpsSideToDirection, sizeDeltaToNotionalAssets } from '../utils/perps'

interface ContractResult {
  status: 'failure' | 'success'
  result?: unknown
}

const PERPS_DYNAMIC_REFETCH_INTERVAL_MS = 15_000
const PERPS_CONFIG_STALE_TIME_MS = 5 * 60_000
const PERPS_CONFIG_GC_TIME_MS = Number.POSITIVE_INFINITY

function sideCarryContracts(side: bigint) {
  const engine = {
    chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
    address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
    abi: PERPS_CFD_ENGINE_ABI,
    args: [side],
  } as const
  return [
    { ...engine, functionName: 'sideCarryIndex' },
    { ...engine, functionName: 'sideCarryTimestamp' },
    { ...engine, functionName: 'sideBorrowBaseAssets' },
  ] as const
}

export interface PerpsPendingOrder {
  orderId: bigint
  side: number
  direction: 'long' | 'short'
  sizeDelta: bigint
  marginDeltaAssets: bigint
  acceptablePrice: bigint
  isReduceOnly: boolean
  status: number
  estimatedNotionalAssets?: bigint
  commitTime?: bigint
  expiryTime?: bigint
}

export interface PerpsPosition {
  exists: boolean
  side: number
  direction: 'long' | 'short'
  size: bigint
  entryPrice: bigint
  marginAssets: bigint
  unrealizedPnlAssets: bigint
  maintenanceMarginAssets: bigint
  liquidatable: boolean
  estimatedNotionalAssets?: bigint
  entryNotionalAssets?: bigint
  dxyExposureAssets?: bigint
  displayDxyPrice?: bigint
  liquidationPrice?: bigint
  liquidationThreshold?: LiquidationThreshold
  riskStatus?: 'ready' | 'unavailable'
  capPrice?: bigint
  positionEquityAssets?: bigint
  uncoveredCarryAssets?: bigint
  vpiReserveUnderfunded?: boolean
  pendingCarryAssets?: bigint
  /** Signed net VPI accumulated over the position lifecycle. Positive is net paid; negative is provisional credit. */
  vpiAccrued?: bigint
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
    marginAssets: tupleValue(value, 4, 'marginAssets') as bigint | undefined ?? 0n,
    unrealizedPnlAssets: tupleValue(value, 5, 'unrealizedPnlAssets') as bigint | undefined ?? 0n,
    maintenanceMarginAssets: tupleValue(value, 6, 'maintenanceMarginAssets') as bigint | undefined ?? 0n,
    liquidatable: Boolean(tupleValue(value, 7, 'liquidatable')),
    estimatedNotionalAssets: sizeDeltaToNotionalAssets(size, markPrice),
    entryNotionalAssets: sizeDeltaToNotionalAssets(size, tupleValue(value, 3, 'entryPrice') as bigint | undefined ?? 0n),
    dxyExposureAssets: sizeDeltaToNotionalAssets(size, displayDxyPrice),
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
      marginDeltaAssets: tupleValue(item, 3, 'marginDeltaAssets') as bigint | undefined ?? 0n,
      acceptablePrice: tupleValue(item, 4, 'acceptablePrice') as bigint | undefined ?? 0n,
      isReduceOnly: Boolean(tupleValue(item, 5, 'isReduceOnly')),
      status: Number(tupleValue(item, 6, 'status') ?? 0),
      estimatedNotionalAssets: sizeDeltaToNotionalAssets(sizeDelta, markPrice),
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

function readBigInt(value: unknown, index: number, key: string): bigint | undefined {
  const raw = tupleValue(value, index, key)
  if (typeof raw === 'bigint') return raw
  if (typeof raw === 'number') return BigInt(raw)
  if (typeof raw === 'string') return BigInt(raw)
  return undefined
}

function readPendingCarryAssets(data: readonly ContractResult[] | undefined): bigint | undefined {
  const side = readBigInt(readResult(data, 9), 4, 'side')
  if (side !== 0n && side !== 1n) return undefined
  const sideOffset = side === 0n ? 16 : 19
  const carryState = readResult(data, 11)

  // netEquityAssets is price-risk equity, not carry-adjusted account equity.
  // Subtracting it from settlement would incorrectly label free collateral as carry.
  return calculatePendingCarryAssets({
    borrowBaseAssets: readBigInt(carryState, 0, 'borrowBaseAssets'),
    lastCarryIndex: readBigInt(carryState, 1, 'lastCarryIndex'),
    unsettledCarryAssets: readResult(data, 12) as bigint | undefined,
    baseCarryBps: readBigInt(readResult(data, 13), 5, 'baseCarryBps'),
    poolAssets: readResult(data, 14) as bigint | undefined,
    blockTimestamp: readResult(data, 15) as bigint | undefined,
    sideCarryIndex: readResult(data, sideOffset) as bigint | undefined,
    sideCarryTimestamp: readResult(data, sideOffset + 1) as bigint | undefined,
    sideBorrowBaseAssets: readResult(data, sideOffset + 2) as bigint | undefined,
  })
}

export function usePerpsAccount(markPrice?: bigint) {
  const {
    ownerAddress,
    accountAddress,
    status: identityStatus,
    manifest,
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
    queryKey: dynamicQueryKey,
    refetch: refetchDynamicContracts,
  } = useReadContracts({
    // Keep the position checkpoint, both side indexes, rate, assets and chain time
    // together in one multicall, including across position-side changes.
    batchSize: 0,
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
        address: PERPS_ARBITRUM_SEPOLIA.settlementToken,
        abi: ERC20_ABI,
        functionName: 'balanceOf',
        args: [account],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.settlementToken,
        abi: ERC20_ABI,
        functionName: 'balanceOf',
        args: [owner],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.settlementToken,
        abi: ERC20_ABI,
        functionName: 'allowance',
        args: [account, PERPS_ARBITRUM_SEPOLIA.marginClearinghouse],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.marginClearinghouse,
        abi: PERPS_MARGIN_CLEARINGHOUSE_ABI,
        functionName: 'getFreeBuyingPowerAssets',
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
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getActivePositionProtection',
        args: [account],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'positionCarryState',
        args: [account],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'unsettledCarryAssets',
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
        address: PERPS_ARBITRUM_SEPOLIA.housePool,
        abi: PERPS_HOUSE_POOL_ABI,
        functionName: 'totalAssets',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: arbitrumSepolia.contracts.multicall3.address,
        abi: multicall3Abi,
        functionName: 'getCurrentBlockTimestamp',
      },
      ...sideCarryContracts(0n),
      ...sideCarryContracts(1n),
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'positionEntryCostAssetAtoms',
        args: [account],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'lastMarkPrice',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.marginClearinghouse,
        abi: PERPS_MARGIN_CLEARINGHOUSE_ABI,
        functionName: 'vpiRebateReserveAssets',
        args: [account],
      },
    ],
    query: {
      enabled: isConnected && accountAddress !== undefined,
      refetchInterval: PERPS_DYNAMIC_REFETCH_INTERVAL_MS,
    },
  })
  const snapshotInvalidated = usePerpsSnapshotInvalidated(dynamicQueryKey)
  const invalidateSnapshot = useInvalidatePerpsSnapshot(dynamicQueryKey)
  const refreshDynamic = useCallback(async () => {
    await invalidateSnapshot()
    return refetchDynamicContracts()
  }, [invalidateSnapshot, refetchDynamicContracts])
  // Engine configuration changes atomically behind a 48-hour timelock. Keep
  // this batch aligned with usePerpsMarket so both hooks share one cached read.
  const {
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
        functionName: 'minOpenNotionalAssets',
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
        functionName: 'maxExecutionWindowSeconds',
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
      refreshDynamic(),
      refetchEngineConfiguration(),
      refetchRouterConfiguration(),
    ]),
    [refreshDynamic, refetchEngineConfiguration, refetchRouterConfiguration]
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
  const {
    data: pendingOrderPoliciesData,
    isLoading: pendingOrderPoliciesLoading,
  } = useReadContracts({
    contracts: manifest?.orderLifecycleBook
      ? basicPendingOrders.map((order) => ({
          chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
          address: manifest.orderLifecycleBook,
          abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI,
          functionName: 'orderTiming',
          args: [order.orderId],
        } as const))
      : [],
    query: {
      enabled: isConnected &&
        accountAddress !== undefined &&
        manifest?.orderLifecycleBook !== undefined &&
        basicPendingOrders.length > 0,
      refetchInterval: PERPS_DYNAMIC_REFETCH_INTERVAL_MS,
    },
  })

  const freshAccount = useMemo(() => {
    const accountView = readResult(dynamicContractData, 0)
    const accountMarkPrice = readResult(dynamicContractData, 23) as bigint | undefined
    const position = parsePosition(readResult(dynamicContractData, 1), accountMarkPrice)
    const tradingAccountAssets = readResult(dynamicContractData, 3) as bigint | undefined
    const ownerWalletAssets = readResult(dynamicContractData, 4) as bigint | undefined
    const marginAllowanceAssets = readResult(dynamicContractData, 5) as bigint | undefined
    const freeBuyingPowerAssets = readResult(dynamicContractData, 6) as bigint | undefined
    const accountLedgerSnapshot = readResult(dynamicContractData, 7)
    const isFadWindow = readResult(dynamicContractData, 8) as boolean | undefined
    const enginePosition = readResult(dynamicContractData, 9)
    const activeProtection = readResult(dynamicContractData, 10)
    const riskParams = readResult(dynamicContractData, 13)
    const maxPendingOrders = readResult(routerConfigurationData, 1) as bigint | undefined
    const maxExecutionWindowSeconds = readResult(routerConfigurationData, 2) as bigint | undefined
    const capPrice = readResult(immutableContractData, 0) as bigint | undefined
    const withdrawableAssets = tupleValue(accountView, 1, 'withdrawableAssets') as bigint | undefined
    const equityAssets = tupleValue(accountView, 0, 'equityAssets') as bigint | undefined
    const settlementBalanceAssets = readBigInt(accountLedgerSnapshot, 0, 'settlementBalanceAssets')
    const freeSettlementAssets = readBigInt(accountLedgerSnapshot, 1, 'freeSettlementAssets')
    const positionEquityAssets = readBigInt(accountLedgerSnapshot, 22, 'netEquityAssets')
    const traderClaimBalanceAssets = readBigInt(accountLedgerSnapshot, 9, 'traderClaimBalanceAssets')
    const maintenanceMarginBps = isFadWindow
      ? readBigInt(riskParams, 4, 'fadMarginBps')
      : readBigInt(riskParams, 2, 'maintMarginBps')
    const vpiAccrued = readBigInt(enginePosition, 6, 'vpiAccrued')
    const pendingCarryAssets = readPendingCarryAssets(dynamicContractData)
    const entryCostAssetAtoms = readResult(dynamicContractData, 22) as bigint | undefined
    const vpiReserveAssets = readResult(dynamicContractData, 24) as bigint | undefined
    const snapshotCurrent = !snapshotInvalidated && !dynamicContractsError
    const riskReady = snapshotCurrent && accountView !== undefined && accountLedgerSnapshot !== undefined &&
      accountMarkPrice !== undefined && accountMarkPrice > 0n && capPrice !== undefined && isFadWindow !== undefined &&
      maintenanceMarginBps !== undefined && entryCostAssetAtoms !== undefined &&
      traderClaimBalanceAssets !== undefined && positionEquityAssets !== undefined &&
      pendingCarryAssets !== undefined && freeSettlementAssets !== undefined &&
      vpiAccrued !== undefined && vpiReserveAssets !== undefined
    const carryProjection = position && pendingCarryAssets !== undefined && freeSettlementAssets !== undefined
      ? projectCarry(position.marginAssets, freeSettlementAssets, pendingCarryAssets)
      : undefined
    const liquidationThreshold = findLiquidationThreshold(position?.exists && riskReady && carryProjection ? {
      capPrice, entryCostAssetAtoms, maintenanceMarginBps, side: position.side,
      size: position.size, positionMarginAssets: carryProjection.positionMarginAssets, traderClaimBalanceAssets,
    } : undefined)
    const liquidationPrice = liquidationThreshold.status === 'boundary' ? liquidationThreshold.price : undefined
    const positionWithLiquidationPrice = position === undefined ? undefined : {
      ...position, liquidationPrice, liquidationThreshold, capPrice,
      liquidatable: position.liquidatable || Boolean(tupleValue(accountView, 5, 'liquidatable')),
      riskStatus: riskReady && liquidationThreshold.status !== 'unavailable' ? 'ready' as const : 'unavailable' as const,
      positionEquityAssets: riskReady ? positionEquityAssets : undefined,
      pendingCarryAssets: snapshotCurrent ? pendingCarryAssets : undefined,
      vpiAccrued,
      uncoveredCarryAssets: riskReady ? carryProjection?.uncoveredCarryAssets : undefined,
      vpiReserveUnderfunded: riskReady ? vpiAccrued < 0n && vpiReserveAssets < -vpiAccrued : undefined,
    }
    const pendingOrders = basicPendingOrders.map((order, index) => {
      const commitTime = parsePendingOrderCommitTime(readResult(pendingOrderViewsData, index))
      const pendingPolicy = readResult(pendingOrderPoliciesData, index)
      const executionDeadline = readBigInt(pendingPolicy, 3, 'executionDeadline')
      const expiryTime = executionDeadline !== undefined && executionDeadline > 0n ? executionDeadline : undefined

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
      isPendingOrderPolicyLoading: pendingOrderPoliciesLoading,
      error,
      refetchDynamic: refreshDynamic,
      refetch,
      walletAssets: ownerWalletAssets,
      ownerWalletAssets,
      tradingAccountAssets,
      marginAllowanceAssets,
      equityAssets: snapshotCurrent ? equityAssets : undefined,
      positionEquityAssets: position?.exists && riskReady ? positionEquityAssets : undefined,
      settlementBalanceAssets: snapshotCurrent ? settlementBalanceAssets : undefined,
      freeSettlementAssets: snapshotCurrent ? freeSettlementAssets : undefined,
      carryProjection: snapshotCurrent ? carryProjection : undefined,
      snapshotStatus: snapshotCurrent && accountView !== undefined && accountLedgerSnapshot !== undefined &&
        position !== undefined && (!position.exists || riskReady) ? 'ready' as const : 'unavailable' as const,
      freeBuyingPowerAssets: snapshotCurrent ? freeBuyingPowerAssets : undefined,
      withdrawableAssets: snapshotCurrent ? withdrawableAssets : undefined,
      traderClaimBalanceAssets: snapshotCurrent ? traderClaimBalanceAssets : undefined,
      pendingOrderMarginAssets: tupleValue(accountView, 2, 'pendingOrderMarginAssets') as bigint | undefined,
      pendingExecutionBountyAssets: tupleValue(accountView, 3, 'pendingExecutionBountyAssets') as bigint | undefined,
      maxPendingOrders,
      maxExecutionWindowSeconds,
      activePositionProtectionId:
        readBigInt(activeProtection, 0, 'protectionId') ?? 0n,
      activePositionProtection: parsePositionProtection(activeProtection),
      capPrice,
      activePositionProtectionStatus:
        Number(tupleValue(activeProtection, 15, 'status') ?? 0),
      firstPendingOrderId,
      firstPendingOrderExpiryTime,
      accountHasOpenPosition,
      hasOpenPosition: Boolean(accountHasOpenPosition) && Boolean(positionWithLiquidationPrice?.exists),
      liquidatable: Boolean(tupleValue(accountView, 5, 'liquidatable')) || Boolean(positionWithLiquidationPrice?.liquidatable),
      position: positionWithLiquidationPrice,
      pendingOrders,
      display: {
        walletAssets: formatPerpsAssets(ownerWalletAssets),
        ownerWalletAssets: formatPerpsAssets(ownerWalletAssets),
        tradingAccountAssets: formatPerpsAssets(tradingAccountAssets),
        availableToTrade: formatPerpsAssets(snapshotCurrent ? freeBuyingPowerAssets ?? withdrawableAssets : undefined),
        equity: formatPerpsAssets(snapshotCurrent ? equityAssets : undefined),
        positionNotional: formatPerpsAssets(positionWithLiquidationPrice?.estimatedNotionalAssets),
        entryPrice: formatDisplayDxyPrice(positionWithLiquidationPrice?.entryPrice),
        pnl: formatSignedPerpsAssets(positionWithLiquidationPrice?.unrealizedPnlAssets),
      },
    }
  }, [snapshotInvalidated, dynamicContractsError, accountAddress, basicPendingOrders, dynamicContractData, error, identityStatus, immutableContractData, isConnected, isLoading, ownerAddress, pendingOrderPoliciesData, pendingOrderPoliciesLoading, pendingOrderViewsData, pendingOrderViewsLoading, refetch, refreshDynamic, routerConfigurationData])

  useEffect(() => {
    if (!isConnected || freshAccount.position === undefined) return

    lastSuccessfulPositionRef.current = {
      account,
      position: freshAccount.position,
    }
  }, [account, freshAccount.position, isConnected])

  return useMemo(() => {
    const stablePosition = freshAccount.position ?? (
      freshAccount.accountHasOpenPosition !== false &&
      lastSuccessfulPositionRef.current?.account === account
        ? {
            ...lastSuccessfulPositionRef.current.position,
            liquidationPrice: undefined,
            liquidationThreshold: { status: 'unavailable' as const },
            riskStatus: 'unavailable' as const,
            positionEquityAssets: undefined,
            pendingCarryAssets: undefined,
            uncoveredCarryAssets: undefined,
            vpiReserveUnderfunded: undefined,
          }
        : undefined
    )

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
        positionNotional: formatPerpsAssets(stablePosition?.estimatedNotionalAssets),
        entryPrice: formatDisplayDxyPrice(stablePosition?.entryPrice),
        pnl: formatSignedPerpsAssets(stablePosition?.unrealizedPnlAssets),
      },
    }
  }, [freshAccount, account])
}
