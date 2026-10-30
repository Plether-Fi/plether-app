import { useMemo } from 'react'
import { formatUnits } from 'viem'
import { useReadContracts } from 'wagmi'
import {
  usePerpsBasketHistory,
  usePerpsBasketLatest,
  usePerpsMarketStats,
} from '../api'
import { PERPS_CFD_ENGINE_ABI, PERPS_HOUSE_POOL_ABI, PERPS_ORDER_ROUTER_ABI, PERPS_PUBLIC_LENS_ABI } from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { PERPS_DECIMALS, PERPS_POSITION_SIZE_TO_ASSETS_SCALE, PERPS_PROTOCOL_PHASE } from '../contracts/perpsConstants'
import type { PerpsMarketPhase } from '../utils/perpsMarketSchedule'
import { formatDisplayDxyPrice, perpsOracleFreshnessFromTimestamp } from '../utils/perps'
import {
  computeBasketComponentPriceChanges,
  computeBasketDisplayPriceChange,
} from '../utils/dxyBasketChart'
import { formatCompactNumber } from '../utils/formatters'
import { calculatePerpsDirectionalLimit } from '../utils/perpsDirectionalLimit'
import { calculatePerpsPoolCapital } from '../utils/perpsPoolCapital'
import { DXY_COMPONENT_CHANGE_HISTORY_INTERVAL_SECONDS } from '../components/dxyBasketChartConfig'

const WAD = 10n ** 18n
const ORACLE_FRESH_SECONDS = 60
const PERPS_DYNAMIC_REFETCH_INTERVAL_MS = 15_000
const PERPS_CONFIG_STALE_TIME_MS = 5 * 60_000
const PERPS_CONFIG_GC_TIME_MS = Number.POSITIVE_INFINITY

interface ContractResult {
  status: 'failure' | 'success'
  result?: unknown
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

function formatCompactAssets(amount: bigint | undefined): string | undefined {
  if (amount === undefined) return undefined
  return formatCompactNumber(Number(formatUnits(amount, PERPS_DECIMALS.ASSETS)))
}

function parseBigIntString(value: string | undefined): bigint | undefined {
  if (!value) return undefined
  try {
    return BigInt(value)
  } catch {
    return undefined
  }
}

function formatBpsAsPercent(bps: bigint | undefined): string | undefined {
  if (bps === undefined) return undefined
  return `${(Number(bps) / 100).toLocaleString('en-US', {
    maximumFractionDigits: 2,
    minimumFractionDigits: 0,
  })}%`
}

function formatPercentChange(value: number | undefined): string | undefined {
  if (value === undefined || !Number.isFinite(value)) return undefined
  const sign = value > 0 ? '+' : ''
  return `${sign}${(value * 100).toFixed(2)}%`
}

function percentChangeTone(value: number | undefined): 'positive' | 'negative' | 'default' | undefined {
  if (value === undefined || !Number.isFinite(value)) return undefined
  if (value > 0) return 'positive'
  if (value < 0) return 'negative'
  return 'default'
}

function openInterestNotionalAssets(openInterest: bigint | undefined, markPrice: bigint | undefined): bigint | undefined {
  if (openInterest === undefined || markPrice === undefined) return undefined
  return (openInterest * markPrice) / PERPS_POSITION_SIZE_TO_ASSETS_SCALE
}

function openCapacityAssets({
  selectedOpenInterestAssets,
  oppositeOpenInterestAssets,
  poolAssets,
  maxSkewRatio,
}: {
  selectedOpenInterestAssets: bigint | undefined
  oppositeOpenInterestAssets: bigint | undefined
  poolAssets: bigint | undefined
  maxSkewRatio: bigint | undefined
}): bigint | undefined {
  if (
    selectedOpenInterestAssets === undefined ||
    oppositeOpenInterestAssets === undefined ||
    poolAssets === undefined ||
    maxSkewRatio === undefined
  ) {
    return undefined
  }

  const maxSkewAssets = (poolAssets * maxSkewRatio) / WAD
  return maxSkewAssets + oppositeOpenInterestAssets > selectedOpenInterestAssets
    ? maxSkewAssets + oppositeOpenInterestAssets - selectedOpenInterestAssets
    : 0n
}

function minNewPositionNotionalAssets(
  minBountyAssets: bigint | undefined,
  bountyBps: bigint | undefined
): bigint | undefined {
  if (minBountyAssets === undefined || bountyBps === undefined || bountyBps === 0n) return undefined
  return (minBountyAssets * 10_000n + bountyBps - 1n) / bountyBps
}

function protocolPhaseToMarketPhase(
  phase: number | undefined,
  tradingActive: boolean | undefined,
  oracleFrozen: boolean | undefined,
  fadWindow: boolean | undefined
): PerpsMarketPhase {
  if (phase === PERPS_PROTOCOL_PHASE.DEGRADED) return 'degraded'
  if (!tradingActive || phase === PERPS_PROTOCOL_PHASE.CONFIGURING) return 'closed'
  if (oracleFrozen || fadWindow) return 'close-only'
  return 'open'
}

export function usePerpsMarket() {
  const {
    data: latestBasket,
    isLoading: isLatestBasketLoading,
    isError: isLatestBasketError,
    refetch: refetchLatestBasket,
  } = usePerpsBasketLatest()
  const {
    data: basketComponentHistory24h,
    isLoading: isBasketComponentHistory24hLoading,
    refetch: refetchBasketComponentHistory24h,
  } = usePerpsBasketHistory(
    '24h',
    DXY_COMPONENT_CHANGE_HISTORY_INTERVAL_SECONDS,
    true
  )
  const {
    data: marketStats,
    isLoading: isMarketStatsLoading,
    refetch: refetchMarketStats,
  } = usePerpsMarketStats()
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
        functionName: 'getProtocolStatus',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.housePool,
        abi: PERPS_HOUSE_POOL_ABI,
        functionName: 'getPoolLiquidityView',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'sides',
        args: [0n],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'sides',
        args: [1n],
      },
    ],
    query: {
      refetchInterval: PERPS_DYNAMIC_REFETCH_INTERVAL_MS,
    },
  })
  // Engine configuration changes atomically behind a 48-hour timelock. Keep
  // this query identical in usePerpsAccount so wagmi shares one cached read.
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
      staleTime: PERPS_CONFIG_STALE_TIME_MS,
      gcTime: PERPS_CONFIG_GC_TIME_MS,
      refetchOnWindowFocus: true,
      refetchOnReconnect: true,
    },
  })
  // Router configuration is finalized atomically by its own timelocked admin.
  // The matching account hook uses this exact batch and therefore shares it.
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
      staleTime: PERPS_CONFIG_STALE_TIME_MS,
      gcTime: PERPS_CONFIG_GC_TIME_MS,
      refetchOnWindowFocus: true,
      refetchOnReconnect: true,
    },
  })

  const isLoading =
    isDynamicContractsLoading ||
    isEngineConfigurationLoading ||
    isRouterConfigurationLoading
  const error =
    dynamicContractsError ?? engineConfigurationError ?? routerConfigurationError

  return useMemo(() => {
    const protocolStatus = readResult(dynamicContractData, 0)
    const poolLiquidity = readResult(dynamicContractData, 1)
    const bullSide = readResult(dynamicContractData, 2)
    const bearSide = readResult(dynamicContractData, 3)
    const riskParams = readResult(engineConfigurationData, 0)
    const executionFeeBps = readResult(engineConfigurationData, 1) as bigint | undefined
    const minOpenNotionalAssets = readResult(routerConfigurationData, 0) as bigint | undefined

    const markPrice = tupleValue(protocolStatus, 1, 'lastMarkPrice') as bigint | undefined
    const lastMarkTime = tupleValue(protocolStatus, 2, 'lastMarkTime') as bigint | number | undefined
    const phaseValue = tupleValue(protocolStatus, 0, 'phase') as number | bigint | undefined
    const tradingActive = tupleValue(protocolStatus, 5, 'tradingActive') as boolean | undefined
    const oracleFrozen = tupleValue(protocolStatus, 3, 'oracleFrozen') as boolean | undefined
    const fadWindow = tupleValue(protocolStatus, 4, 'fadWindow') as boolean | undefined
    const poolAssets = tupleValue(poolLiquidity, 0, 'totalAssets') as bigint | undefined
    const freeAssets = tupleValue(poolLiquidity, 1, 'freeAssets') as bigint | undefined
    const seniorPrincipalAssets = tupleValue(poolLiquidity, 5, 'seniorPrincipalAssets') as bigint | undefined
    const juniorPrincipalAssets = tupleValue(poolLiquidity, 6, 'juniorPrincipalAssets') as bigint | undefined
    const seniorHighWaterMarkAssets = tupleValue(poolLiquidity, 7, 'seniorHighWaterMarkAssets') as bigint | undefined
    const bullOpenInterest = tupleValue(bullSide, 1, 'openInterest') as bigint | undefined
    const bearOpenInterest = tupleValue(bearSide, 1, 'openInterest') as bigint | undefined
    const maxSkewRatio = tupleValue(riskParams, 1, 'maxSkewRatio') as bigint | undefined
    const maintMarginBps = tupleValue(riskParams, 2, 'maintMarginBps') as bigint | undefined
    const initialMarginBps = tupleValue(riskParams, 3, 'initMarginBps') as bigint | undefined
    const fadMarginBps = tupleValue(riskParams, 4, 'fadMarginBps') as bigint | undefined
    const maintenanceMarginBps = fadWindow ? fadMarginBps : maintMarginBps
    const baseCarryBps = tupleValue(riskParams, 5, 'baseCarryBps') as bigint | undefined
    const minBountyAssets = tupleValue(riskParams, 6, 'minBountyAssets') as bigint | undefined
    const bountyBps = tupleValue(riskParams, 7, 'bountyBps') as bigint | undefined
    const bullOpenInterestAssets = openInterestNotionalAssets(bullOpenInterest, markPrice)
    const bearOpenInterestAssets = openInterestNotionalAssets(bearOpenInterest, markPrice)
    const directionalLimit = calculatePerpsDirectionalLimit({
      longOpenInterestAssets: bullOpenInterestAssets,
      shortOpenInterestAssets: bearOpenInterestAssets,
      poolAssets,
      maxSkewRatio,
    })
    const poolCapital = calculatePerpsPoolCapital({
      juniorPrincipalAssets,
      seniorPrincipalAssets,
      seniorHighWaterMarkAssets,
    })
    // The component-rich hourly response already carries the basket price at
    // every point. Reuse it for the headline change instead of polling a
    // second, duplicate 24-hour history endpoint once per client.
    const priceChange24hValue = computeBasketDisplayPriceChange(
      basketComponentHistory24h?.data.points,
      latestBasket?.data
    )
    const basketComponentPriceChanges = computeBasketComponentPriceChanges(
      basketComponentHistory24h?.data.points,
      latestBasket?.data
    )
    const volume24hAssets = parseBigIntString(marketStats?.data.volume24hAssets)
    const longOpenCapacityAssets = openCapacityAssets({
      selectedOpenInterestAssets: bullOpenInterestAssets,
      oppositeOpenInterestAssets: bearOpenInterestAssets,
      poolAssets,
      maxSkewRatio,
    })
    const shortOpenCapacityAssets = openCapacityAssets({
      selectedOpenInterestAssets: bearOpenInterestAssets,
      oppositeOpenInterestAssets: bullOpenInterestAssets,
      poolAssets,
      maxSkewRatio,
    })

    const phase = phaseValue === undefined ? undefined : Number(phaseValue)
    const marketPhase = protocolPhaseToMarketPhase(phase, tradingActive, oracleFrozen, fadWindow)
    const {
      freshness: oracleFreshness,
      publishTime: oracleFreshnessTime,
    } = perpsOracleFreshnessFromTimestamp({
      publishTime: lastMarkTime,
      isChecking: isLoading,
      freshSeconds: ORACLE_FRESH_SECONDS,
    })

    return {
      chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
      addresses: PERPS_ARBITRUM_SEPOLIA,
      raw: {
        markPrice,
        poolAssets,
        freeAssets,
        seniorPrincipalAssets,
        juniorPrincipalAssets,
        seniorHighWaterMarkAssets,
        bullOpenInterest,
        bearOpenInterest,
        bullOpenInterestAssets,
        bearOpenInterestAssets,
        longOpenCapacityAssets,
        shortOpenCapacityAssets,
        maxSkewRatio,
        maintenanceMarginBps,
        initialMarginBps,
        minOpenNotionalAssets,
        minNewPositionNotionalAssets: minNewPositionNotionalAssets(minBountyAssets, bountyBps),
        baseCarryBps,
        executionFeeBps,
        basketComponents: latestBasket?.data.components,
      },
      oraclePrice: formatDisplayDxyPrice(markPrice) === '--' ? undefined : formatDisplayDxyPrice(markPrice),
      latestBasket: latestBasket?.data,
      basketComponentPriceChanges,
      isBasketComponentsLoading:
        isLatestBasketLoading || isBasketComponentHistory24hLoading,
      isBasketComponentsError: isLatestBasketError,
      oracleFreshness,
      oracleFreshnessTime,
      longOpenInterest: formatCompactAssets(bullOpenInterestAssets),
      shortOpenInterest: formatCompactAssets(bearOpenInterestAssets),
      directionalLimit: directionalLimit
        ? {
            usagePercent: directionalLimit.usagePercent,
            side: directionalLimit.side,
            totalExposure: formatCompactAssets(
              directionalLimit.side === 'short' ? bearOpenInterestAssets : bullOpenInterestAssets
            ),
            netExposure: formatCompactAssets(directionalLimit.netExposureAssets),
            limit: formatCompactAssets(directionalLimit.limitAssets),
          }
        : undefined,
      priceChange24h: formatPercentChange(priceChange24hValue),
      priceChange24hTone: percentChangeTone(priceChange24hValue),
      volume24h: formatCompactAssets(volume24hAssets),
      availableLiquidity: formatCompactAssets(freeAssets),
      poolCapital: poolCapital
        ? {
            ...poolCapital,
            juniorPrincipal: formatCompactAssets(juniorPrincipalAssets),
            seniorPrincipal: formatCompactAssets(seniorPrincipalAssets),
            seniorImpairment: formatCompactAssets(poolCapital.seniorImpairmentAssets),
          }
        : undefined,
      costOfCarry: formatBpsAsPercent(baseCarryBps),
      executionFeeBps,
      marketPhase,
      tradingActive: tradingActive ?? false,
      oracleFrozen: oracleFrozen ?? false,
      fadWindow: fadWindow ?? false,
      isLoading,
      isStatsLoading: isBasketComponentHistory24hLoading || isMarketStatsLoading,
      error,
      refetchDynamic: refetchDynamicContracts,
      refetch: () => {
        void refetchDynamicContracts()
        void refetchEngineConfiguration()
        void refetchRouterConfiguration()
        void refetchLatestBasket()
        void refetchBasketComponentHistory24h()
        void refetchMarketStats()
      },
    }
  }, [
    basketComponentHistory24h,
    dynamicContractData,
    engineConfigurationData,
    error,
    isBasketComponentHistory24hLoading,
    isLatestBasketError,
    isLatestBasketLoading,
    isLoading,
    isMarketStatsLoading,
    latestBasket,
    marketStats,
    refetchBasketComponentHistory24h,
    refetchDynamicContracts,
    refetchEngineConfiguration,
    refetchLatestBasket,
    refetchMarketStats,
    refetchRouterConfiguration,
    routerConfigurationData,
  ])
}
