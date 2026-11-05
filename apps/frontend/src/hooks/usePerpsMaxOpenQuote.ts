import { useQuery } from '@tanstack/react-query'
import type { Address } from 'viem'
import { usePublicClient } from 'wagmi'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { quotePerpsMaxOpen } from '../contracts/perpsMaxOpenQuote'

const MAX_OPEN_STALE_TIME_MS = 10_000
const MAX_OPEN_REFETCH_INTERVAL_MS = 15_000

export function usePerpsMaxOpenQuote({
  enabled, account, side, availableAssets, oraclePrice, publishTime, protectionRewardsAssets, selectedMaxLeverageBps,
}: {
  enabled: boolean
  account?: Address
  side: number
  availableAssets?: bigint
  oraclePrice?: bigint
  publishTime?: number
  protectionRewardsAssets?: bigint
  selectedMaxLeverageBps: number
}) {
  const client = usePublicClient({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID })
  const canRead = enabled && account !== undefined && availableAssets !== undefined &&
    oraclePrice !== undefined && oraclePrice > 0n && publishTime !== undefined && publishTime > 0 &&
    protectionRewardsAssets !== undefined && Number.isSafeInteger(selectedMaxLeverageBps) && selectedMaxLeverageBps > 0
  // Price and balance updates may retain the last number for display only.
  // Never carry it across a different account, direction, or user-selected risk settings.
  const displayKey = JSON.stringify([PERPS_ARBITRUM_SEPOLIA_CHAIN_ID, PERPS_ARBITRUM_SEPOLIA,
    account, side, protectionRewardsAssets?.toString(), selectedMaxLeverageBps])
  const result = useQuery<Awaited<ReturnType<typeof quotePerpsMaxOpen>>>({
    // Time remains a readiness gate, not a new quote identity. Polling and
    // explicit trading invalidations refresh chain state without clearing a
    // result solely because the same price has a newer publication timestamp.
    queryKey: ['perps', 'max-open', PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
      PERPS_ARBITRUM_SEPOLIA.cfdEngineLens, PERPS_ARBITRUM_SEPOLIA.cfdEngine, PERPS_ARBITRUM_SEPOLIA.orderRouter,
      account, side, availableAssets?.toString(), oraclePrice?.toString(),
      protectionRewardsAssets?.toString(), selectedMaxLeverageBps],
    enabled: canRead,
    meta: { maxOpenDisplayKey: displayKey },
    placeholderData: (previous, previousQuery) => canRead &&
      previousQuery?.meta?.maxOpenDisplayKey === displayKey && previousQuery.state.status === 'success'
      ? previous : undefined,
    staleTime: MAX_OPEN_STALE_TIME_MS,
    gcTime: 5 * 60_000,
    refetchInterval: MAX_OPEN_REFETCH_INTERVAL_MS,
    retry: false,
    queryFn: async ({ signal }) => {
      if (!canRead) throw new Error('Maximum size inputs are unavailable')
      // The helper reads the bounty and immutable cap in the same eth_call as
      // the quote, so neither a configuration round trip nor stale config is needed.
      return quotePerpsMaxOpen(client, {
        account, side, availableAssets, oraclePrice, protectionRewardsAssets,
        selectedMaxLeverageBps, signal,
      })
    },
  })
  const displayData = canRead && !result.isError ? result.data : undefined
  const data = result.isPlaceholderData ? undefined : displayData
  return {
    quote: data?.quote,
    displayQuote: displayData?.quote,
    marginDelta: data?.marginDelta,
    error: canRead ? result.error : undefined,
    isPending: canRead && (result.isLoading || result.isPlaceholderData),
    isFetching: canRead && result.isFetching,
  }
}
