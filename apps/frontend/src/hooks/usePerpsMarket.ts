import { useMemo } from 'react'
import { formatUnits } from 'viem'
import { useReadContracts } from 'wagmi'
import { PERPS_CFD_ENGINE_ABI, PERPS_HOUSE_POOL_ABI, PERPS_PUBLIC_LENS_ABI } from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { PERPS_DECIMALS, PERPS_POSITION_SIZE_TO_USDC_SCALE, PERPS_PROTOCOL_PHASE } from '../contracts/perpsConstants'
import type { PerpsMarketPhase } from '../components/PerpsMarketStatePanel'

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

function compactNumber(value: number): string {
  if (!Number.isFinite(value)) return '--'
  if (value >= 1_000_000) return `${(value / 1_000_000).toFixed(1).replace(/\.0$/, '')}M`
  if (value >= 1_000) return `${(value / 1_000).toFixed(1).replace(/\.0$/, '')}K`

  return value.toLocaleString('en-US', {
    maximumFractionDigits: 2,
  }).replaceAll(',', ' ')
}

function formatPrice(price: bigint | undefined): string | undefined {
  if (price === undefined || price === 0n) return undefined

  return Number(formatUnits(price, PERPS_DECIMALS.PRICE)).toLocaleString('en-US', {
    minimumFractionDigits: 4,
    maximumFractionDigits: 4,
  }).replaceAll(',', ' ')
}

function formatCompactUsdc(amount: bigint | undefined): string | undefined {
  if (amount === undefined) return undefined
  return compactNumber(Number(formatUnits(amount, PERPS_DECIMALS.USDC)))
}

function formatBpsAsPercent(bps: bigint | undefined): string | undefined {
  if (bps === undefined) return undefined
  return `${(Number(bps) / 100).toLocaleString('en-US', {
    maximumFractionDigits: 2,
    minimumFractionDigits: 0,
  })}%`
}

function openInterestNotionalUsdc(openInterest: bigint | undefined, markPrice: bigint | undefined): bigint | undefined {
  if (openInterest === undefined || markPrice === undefined) return undefined
  return (openInterest * markPrice) / PERPS_POSITION_SIZE_TO_USDC_SCALE
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
  const { data, isLoading, error, refetch } = useReadContracts({
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

    const markPrice = tupleValue(protocolStatus, 1, 'lastMarkPrice') as bigint | undefined
    const phaseValue = tupleValue(protocolStatus, 0, 'phase') as number | bigint | undefined
    const tradingActive = tupleValue(protocolStatus, 5, 'tradingActive') as boolean | undefined
    const oracleFrozen = tupleValue(protocolStatus, 3, 'oracleFrozen') as boolean | undefined
    const fadWindow = tupleValue(protocolStatus, 4, 'fadWindow') as boolean | undefined
    const lastMarkTime = tupleValue(protocolStatus, 2, 'lastMarkTime') as bigint | number | undefined
    const markFresh = tupleValue(poolLiquidity, 8, 'markFresh') as boolean | undefined
    const freeUsdc = tupleValue(poolLiquidity, 1, 'freeUsdc') as bigint | undefined
    const bullOpenInterest = tupleValue(bullSide, 1, 'openInterest') as bigint | undefined
    const bearOpenInterest = tupleValue(bearSide, 1, 'openInterest') as bigint | undefined
    const baseCarryBps = tupleValue(riskParams, 5, 'baseCarryBps') as bigint | undefined

    const phase = phaseValue === undefined ? undefined : Number(phaseValue)
    const marketPhase = protocolPhaseToMarketPhase(phase, tradingActive, oracleFrozen, fadWindow)
    const hasStoredMark = markPrice !== undefined && markPrice > 0n && lastMarkTime !== undefined && Number(lastMarkTime) > 0
    const oracleFresh = hasStoredMark && (markFresh ?? !oracleFrozen)

    return {
      chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
      addresses: PERPS_ARBITRUM_SEPOLIA,
      oraclePrice: formatPrice(markPrice),
      oracleFreshness: oracleFresh ? 'fresh' as const : 'stale' as const,
      lastMarkTime: lastMarkTime === undefined ? undefined : Number(lastMarkTime),
      longOpenInterest: formatCompactUsdc(openInterestNotionalUsdc(bullOpenInterest, markPrice)),
      shortOpenInterest: formatCompactUsdc(openInterestNotionalUsdc(bearOpenInterest, markPrice)),
      availableLiquidity: formatCompactUsdc(freeUsdc),
      costOfCarry: formatBpsAsPercent(baseCarryBps),
      executionFeeBps,
      marketPhase,
      tradingActive: tradingActive ?? false,
      oracleFrozen: oracleFrozen ?? false,
      fadWindow: fadWindow ?? false,
      isLoading,
      error,
      refetch,
    }
  }, [data, error, isLoading, refetch])
}
