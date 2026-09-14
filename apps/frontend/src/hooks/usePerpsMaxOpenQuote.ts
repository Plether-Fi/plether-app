import { useQuery } from '@tanstack/react-query'
import type { Address } from 'viem'
import { usePublicClient } from 'wagmi'
import { PERPS_CFD_ENGINE_ABI, PERPS_CFD_ENGINE_LENS_ABI, PERPS_ORDER_ROUTER_ABI } from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { constrainPerpsMaxOpenPreview, perpsMaxOpenMarginBudget } from '../contracts/perpsMaxOpen'

export function usePerpsMaxOpenQuote({
  enabled, account, side, availableUsdc, oraclePrice, publishTime, protectionRewardsUsdc, selectedMaxLeverageBps,
}: {
  enabled: boolean
  account?: Address
  side: number
  availableUsdc?: bigint
  oraclePrice?: bigint
  publishTime?: number
  protectionRewardsUsdc?: bigint
  selectedMaxLeverageBps: number
}) {
  const client = usePublicClient({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID })
  const canRead = enabled && account !== undefined && availableUsdc !== undefined &&
    oraclePrice !== undefined && oraclePrice > 0n && publishTime !== undefined && publishTime > 0 &&
    protectionRewardsUsdc !== undefined && Number.isInteger(selectedMaxLeverageBps) && selectedMaxLeverageBps > 0
  const result = useQuery({
    queryKey: ['perps', 'max-open', PERPS_ARBITRUM_SEPOLIA_CHAIN_ID, account, side, availableUsdc?.toString(),
      oraclePrice?.toString(), publishTime, protectionRewardsUsdc?.toString(), selectedMaxLeverageBps],
    enabled: canRead,
    refetchInterval: 15_000,
    retry: false,
    queryFn: async ({ signal }) => {
      if (!canRead) throw new Error('Maximum size inputs are unavailable')
      const blockNumber = await client.getBlockNumber()
      const [maximumBounty, capPrice] = await Promise.all([
        client.readContract({ address: PERPS_ARBITRUM_SEPOLIA.orderRouter, abi: PERPS_ORDER_ROUTER_ABI,
          functionName: 'maxOpenOrderExecutionBountyUsdc', blockNumber }),
        client.readContract({ address: PERPS_ARBITRUM_SEPOLIA.cfdEngine, abi: PERPS_CFD_ENGINE_ABI,
          functionName: 'CAP_PRICE', blockNumber }),
      ])
      signal.throwIfAborted()
      const marginDelta = perpsMaxOpenMarginBudget(availableUsdc, maximumBounty, protectionRewardsUsdc)
      if (marginDelta <= 0n) return { marginDelta, quote: undefined }
      const quote = await client.readContract({
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens, abi: PERPS_CFD_ENGINE_LENS_ABI,
        functionName: 'quoteMaxOpen', args: [account, side, marginDelta, oraclePrice, BigInt(publishTime)], blockNumber,
      })
      signal.throwIfAborted()
      if (quote.maxSizeDelta === 0n || !quote.preview.valid) return { marginDelta, quote }
      const constrained = await constrainPerpsMaxOpenPreview({
        quote, selectedMaxLeverageBps, oraclePrice, capPrice, signal,
        preview: (sizeDelta) => client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens, abi: PERPS_CFD_ENGINE_LENS_ABI,
          functionName: 'previewOpen', args: [account, side, sizeDelta, marginDelta, oraclePrice, BigInt(publishTime)], blockNumber,
        }),
      })
      return { marginDelta, quote: { ...quote, maxSizeDelta: constrained?.sizeDelta ?? 0n,
        preview: constrained?.value ?? quote.preview } }
    },
  })
  const data = canRead && !result.isError ? result.data : undefined
  return {
    quote: data?.quote,
    marginDelta: data?.marginDelta,
    error: canRead ? result.error : undefined,
    isPending: canRead && result.isLoading,
    isFetching: canRead && result.isFetching,
  }
}
