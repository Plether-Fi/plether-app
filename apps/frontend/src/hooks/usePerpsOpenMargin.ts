import { useQuery } from '@tanstack/react-query'
import { usePublicClient } from 'wagmi'
import type { Address } from 'viem'
import { quotePerpsOpenMargin, type PerpsOpenMarginPreview } from '../contracts/perpsOpenMargin'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'

export function usePerpsOpenMargin(input: {
  enabled: boolean
  account?: Address
  side: number
  sizeDelta: bigint
  marginBudget?: bigint
  oraclePrice?: bigint
  selectedMaxLeverageBps: number
}) {
  const client = usePublicClient({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID })
  const enabled = input.enabled && input.account !== undefined && input.marginBudget !== undefined && input.oraclePrice !== undefined
  // Price and balance updates may retain the last quote for display while the
  // refreshed quote loads. Never carry it across a different account, side,
  // size, or user-selected leverage.
  const displayKey = JSON.stringify([PERPS_ARBITRUM_SEPOLIA_CHAIN_ID, PERPS_ARBITRUM_SEPOLIA.cfdEngineLens,
    input.account, input.side, input.sizeDelta.toString(), input.selectedMaxLeverageBps])
  const result = useQuery<PerpsOpenMarginPreview>({
    queryKey: ['perps', 'open-margin', PERPS_ARBITRUM_SEPOLIA_CHAIN_ID, PERPS_ARBITRUM_SEPOLIA.cfdEngineLens, input.account, input.side,
      input.sizeDelta.toString(), input.marginBudget?.toString(), input.oraclePrice?.toString(), input.selectedMaxLeverageBps],
    enabled,
    meta: { openMarginDisplayKey: displayKey },
    placeholderData: (previous, previousQuery) => enabled &&
      previousQuery?.meta?.openMarginDisplayKey === displayKey && previousQuery.state.status === 'success'
      ? previous : undefined,
    staleTime: 10_000,
    refetchInterval: 15_000,
    retry: false,
    queryFn: ({ signal }) => {
      if (!input.account || input.marginBudget === undefined || input.oraclePrice === undefined) {
        throw new Error('Opening margin inputs are unavailable')
      }
      return quotePerpsOpenMargin(client, { account: input.account, side: input.side, sizeDelta: input.sizeDelta,
        marginBudget: input.marginBudget, oraclePrice: input.oraclePrice, selectedMaxLeverageBps: input.selectedMaxLeverageBps, signal })
    },
  })
  const displayPreview = enabled && !result.isError ? result.data : undefined
  const preview = result.isPlaceholderData ? undefined : displayPreview
  return {
    preview,
    displayPreview,
    error: enabled ? result.error : undefined,
    isPending: input.enabled && (!enabled || result.isPending || result.isPlaceholderData),
    isFetching: enabled && result.isFetching,
  }
}
