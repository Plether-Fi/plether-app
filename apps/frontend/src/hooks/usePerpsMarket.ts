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
import { PERPS_DECIMALS, PERPS_POSITION_SIZE_TO_USDC_SCALE, PERPS_PROTOCOL_PHASE } from '../contracts/perpsConstants'
import type { PerpsMarketPhase } from '../utils/perpsMarketSchedule'
import { formatDisplayDxyPrice, perpsOracleFreshnessFromTimestamp } from '../utils/perps'
import { computeBasketDisplayPriceChange } from '../utils/dxyBasketChart'
import { formatCompactNumber } from '../utils/formatters'

const WAD = 10n ** 18n
const ORACLE_FRESH_SECONDS = 60

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

function formatCompactUsdc(amount: bigint | undefined): string | undefined {
  if (amount === undefined) return undefined
  return formatCompactNumber(Number(formatUnits(amount, PERPS_DECIMALS.USDC)))
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

function openInterestNotionalUsdc(openInterest: bigint | undefined, markPrice: bigint | undefined): bigint | undefined {
  if (openInterest === undefined || markPrice === undefined) return undefined
  return (openInterest * markPrice) / PERPS_POSITION_SIZE_TO_USDC_SCALE
}

function openCapacityUsdc({
  selectedOpenInterestUsdc,
  oppositeOpenInterestUsdc,
  poolAssetsUsdc,
  maxSkewRatio,
}: {
  selectedOpenInterestUsdc: bigint | undefined
  oppositeOpenInterestUsdc: bigint | undefined
  poolAssetsUsdc: bigint | undefined
  maxSkewRatio: bigint | undefined
}): bigint | undefined {
  if (
    selectedOpenInterestUsdc === undefined ||
    oppositeOpenInterestUsdc === undefined ||
    poolAssetsUsdc === undefined ||
    maxSkewRatio === undefined
  ) {
    return undefined
  }

  const maxSkewUsdc = (poolAssetsUsdc * maxSkewRatio) / WAD
  return maxSkewUsdc + oppositeOpenInterestUsdc > selectedOpenInterestUsdc
    ? maxSkewUsdc + oppositeOpenInterestUsdc - selectedOpenInterestUsdc
    : 0n
}

function minNewPositionNotionalUsdc(
  minBountyUsdc: bigint | undefined,
  bountyBps: bigint | undefined
): bigint | undefined {
  if (minBountyUsdc === undefined || bountyBps === undefined || bountyBps === 0n) return undefined
  return (minBountyUsdc * 10_000n + bountyBps - 1n) / bountyBps
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
    refetch: refetchLatestBasket,
  } = usePerpsBasketLatest()
  const {
    data: basketHistory24h,
    isLoading: isBasketHistory24hLoading,
    refetch: refetchBasketHistory24h,
  } = usePerpsBasketHistory('24h', 60)
  const {
    data: marketStats,
    isLoading: isMarketStatsLoading,
    refetch: refetchMarketStats,
  } = usePerpsMarketStats()
  const { data, isLoading, error, refetch: refetchContracts } = useReadContracts({
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
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'minOpenNotionalUsdc',
      },
    ],
    query: {
      refetchInterval: 15_000,
    },
  })

  return useMemo(() => {
    const protocolStatus = readResult(data, 0)
    const poolLiquidity = readResult(data, 1)
    const bullSide = readResult(data, 2)
    const bearSide = readResult(data, 3)
    const riskParams = readResult(data, 4)
    const executionFeeBps = readResult(data, 5) as bigint | undefined
    const minOpenNotionalUsdc = readResult(data, 6) as bigint | undefined

    const markPrice = tupleValue(protocolStatus, 1, 'lastMarkPrice') as bigint | undefined
    const lastMarkTime = tupleValue(protocolStatus, 2, 'lastMarkTime') as bigint | number | undefined
    const phaseValue = tupleValue(protocolStatus, 0, 'phase') as number | bigint | undefined
    const tradingActive = tupleValue(protocolStatus, 5, 'tradingActive') as boolean | undefined
    const oracleFrozen = tupleValue(protocolStatus, 3, 'oracleFrozen') as boolean | undefined
    const fadWindow = tupleValue(protocolStatus, 4, 'fadWindow') as boolean | undefined
    const poolAssetsUsdc = tupleValue(poolLiquidity, 0, 'totalAssetsUsdc') as bigint | undefined
    const freeUsdc = tupleValue(poolLiquidity, 1, 'freeUsdc') as bigint | undefined
    const bullOpenInterest = tupleValue(bullSide, 1, 'openInterest') as bigint | undefined
    const bearOpenInterest = tupleValue(bearSide, 1, 'openInterest') as bigint | undefined
    const maxSkewRatio = tupleValue(riskParams, 1, 'maxSkewRatio') as bigint | undefined
    const maintMarginBps = tupleValue(riskParams, 2, 'maintMarginBps') as bigint | undefined
    const fadMarginBps = tupleValue(riskParams, 4, 'fadMarginBps') as bigint | undefined
    const maintenanceMarginBps = fadWindow ? fadMarginBps : maintMarginBps
    const baseCarryBps = tupleValue(riskParams, 5, 'baseCarryBps') as bigint | undefined
    const minBountyUsdc = tupleValue(riskParams, 6, 'minBountyUsdc') as bigint | undefined
    const bountyBps = tupleValue(riskParams, 7, 'bountyBps') as bigint | undefined
    const bullOpenInterestUsdc = openInterestNotionalUsdc(bullOpenInterest, markPrice)
    const bearOpenInterestUsdc = openInterestNotionalUsdc(bearOpenInterest, markPrice)
    const priceChange24hValue = computeBasketDisplayPriceChange(basketHistory24h?.data.points, latestBasket?.data)
    const volume24hUsdc = parseBigIntString(marketStats?.data.volume24hUsdc)
    const longOpenCapacityUsdc = openCapacityUsdc({
      selectedOpenInterestUsdc: bullOpenInterestUsdc,
      oppositeOpenInterestUsdc: bearOpenInterestUsdc,
      poolAssetsUsdc,
      maxSkewRatio,
    })
    const shortOpenCapacityUsdc = openCapacityUsdc({
      selectedOpenInterestUsdc: bearOpenInterestUsdc,
      oppositeOpenInterestUsdc: bullOpenInterestUsdc,
      poolAssetsUsdc,
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
        poolAssetsUsdc,
        freeUsdc,
        bullOpenInterest,
        bearOpenInterest,
        bullOpenInterestUsdc,
        bearOpenInterestUsdc,
        longOpenCapacityUsdc,
        shortOpenCapacityUsdc,
        maxSkewRatio,
        maintenanceMarginBps,
        minOpenNotionalUsdc,
        minNewPositionNotionalUsdc: minNewPositionNotionalUsdc(minBountyUsdc, bountyBps),
        baseCarryBps,
        executionFeeBps,
        basketComponents: latestBasket?.data.components,
      },
      oraclePrice: formatDisplayDxyPrice(markPrice) === '--' ? undefined : formatDisplayDxyPrice(markPrice),
      latestBasket: latestBasket?.data,
      oracleFreshness,
      oracleFreshnessTime,
      longOpenInterest: formatCompactUsdc(bullOpenInterestUsdc),
      shortOpenInterest: formatCompactUsdc(bearOpenInterestUsdc),
      priceChange24h: formatPercentChange(priceChange24hValue),
      priceChange24hTone: percentChangeTone(priceChange24hValue),
      volume24h: formatCompactUsdc(volume24hUsdc),
      availableLiquidity: formatCompactUsdc(freeUsdc),
      costOfCarry: formatBpsAsPercent(baseCarryBps),
      executionFeeBps,
      marketPhase,
      tradingActive: tradingActive ?? false,
      oracleFrozen: oracleFrozen ?? false,
      fadWindow: fadWindow ?? false,
      isLoading,
      isStatsLoading: isBasketHistory24hLoading || isMarketStatsLoading,
      error,
      refetch: () => {
        void refetchContracts()
        void refetchLatestBasket()
        void refetchBasketHistory24h()
        void refetchMarketStats()
      },
    }
  }, [
    basketHistory24h,
    data,
    error,
    isBasketHistory24hLoading,
    isLoading,
    isMarketStatsLoading,
    latestBasket,
    marketStats,
    refetchBasketHistory24h,
    refetchContracts,
    refetchLatestBasket,
    refetchMarketStats,
  ])
}
