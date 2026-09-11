import type { Address } from 'viem'
import { useReadContracts } from 'wagmi'
import { PERPS_CFD_ENGINE_LENS_ABI, PERPS_ORDER_ROUTER_ABI } from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { perpsMaxOpenMarginBudget } from '../contracts/perpsMaxOpen'

export function usePerpsMaxOpenQuote({
  enabled, account, side, availableUsdc, oraclePrice, publishTime, protectionRewardsUsdc,
}: {
  enabled: boolean
  account?: Address
  side: number
  availableUsdc?: bigint
  oraclePrice?: bigint
  publishTime?: number
  protectionRewardsUsdc?: bigint
}) {
  const canRead = enabled && account !== undefined && availableUsdc !== undefined &&
    oraclePrice !== undefined && oraclePrice > 0n && publishTime !== undefined && publishTime > 0 &&
    protectionRewardsUsdc !== undefined
  const bounty = useReadContracts({
    contracts: [{
      chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
      address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'maxOpenOrderExecutionBountyUsdc',
    }],
    query: { enabled: canRead, refetchInterval: 15_000 },
  })
  const maximumBounty = bounty.data?.[0]?.status === 'success' ? bounty.data[0].result : undefined
  const marginDelta = canRead && maximumBounty !== undefined
    ? perpsMaxOpenMarginBudget(availableUsdc, maximumBounty, protectionRewardsUsdc)
    : undefined
  const canQuote = canRead && marginDelta !== undefined && marginDelta > 0n
  const result = useReadContracts({
    contracts: canQuote ? [{
      chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
      address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens,
      abi: PERPS_CFD_ENGINE_LENS_ABI,
      functionName: 'quoteMaxOpen',
      args: [account, side, marginDelta, oraclePrice, BigInt(publishTime)],
    }] : [],
    query: { enabled: canQuote, refetchInterval: 15_000, retry: false },
  })
  const quote = canQuote && !bounty.isError && !result.isError && result.data?.[0]?.status === 'success'
    ? result.data[0].result : undefined
  const error = canRead ? bounty.error ?? bounty.data?.[0]?.error ??
    (canQuote ? result.error ?? result.data?.[0]?.error : undefined) : undefined
  return {
    quote,
    marginDelta,
    error,
    isPending: canRead && !error && (bounty.isLoading || (canQuote && result.isLoading)),
    isFetching: canRead && (bounty.isFetching || (canQuote && result.isFetching)),
  }
}
