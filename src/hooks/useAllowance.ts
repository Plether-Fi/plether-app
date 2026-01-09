import { useAccount, useReadContract } from 'wagmi'
import { type Address } from 'viem'
import { ERC20_ABI } from '../contracts/abis'

export function useAllowance(tokenAddress: Address, spenderAddress: Address) {
  const { address } = useAccount()

  const { data, isLoading, error, refetch } = useReadContract({
    address: tokenAddress,
    abi: ERC20_ABI,
    functionName: 'allowance',
    args: [address!, spenderAddress],
    query: {
      enabled: !!address,
    },
  })

  return {
    allowance: data ?? 0n,
    isLoading,
    error,
    refetch,
  }
}
