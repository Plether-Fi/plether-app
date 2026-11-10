import { useQuery } from '@tanstack/react-query'
import { usePublicClient } from 'wagmi'
import type { Address } from 'viem'
import { quotePerpsOpenMargin } from '../contracts/perpsOpenMargin'
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
  const result = useQuery({
    queryKey: ['perps', 'open-margin', PERPS_ARBITRUM_SEPOLIA_CHAIN_ID, PERPS_ARBITRUM_SEPOLIA.cfdEngineLens, input.account, input.side,
      input.sizeDelta.toString(), input.marginBudget?.toString(), input.oraclePrice?.toString(), input.selectedMaxLeverageBps],
    enabled,
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
  return { preview: enabled ? result.data : undefined, error: enabled ? result.error : undefined,
    isPending: input.enabled && (!enabled || result.isPending) }
}
