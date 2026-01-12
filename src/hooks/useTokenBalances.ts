import { useAccount, useReadContracts } from 'wagmi'
import { ERC20_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'

export function useTokenBalances() {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const { data, isLoading, error, refetch } = useReadContracts({
    contracts: addresses ? [
      {
        address: addresses.USDC,
        abi: ERC20_ABI,
        functionName: 'balanceOf',
        args: [address!],
      },
      {
        address: addresses.DXY_BEAR,
        abi: ERC20_ABI,
        functionName: 'balanceOf',
        args: [address!],
      },
      {
        address: addresses.DXY_BULL,
        abi: ERC20_ABI,
        functionName: 'balanceOf',
        args: [address!],
      },
      {
        address: addresses.SDXY_BEAR,
        abi: ERC20_ABI,
        functionName: 'balanceOf',
        args: [address!],
      },
      {
        address: addresses.SDXY_BULL,
        abi: ERC20_ABI,
        functionName: 'balanceOf',
        args: [address!],
      },
    ] : [],
    query: {
      enabled: !!address && !!addresses,
    },
  })

  return {
    usdcBalance: data?.[0]?.result ?? 0n,
    bearBalance: data?.[1]?.result ?? 0n,
    bullBalance: data?.[2]?.result ?? 0n,
    sBearBalance: data?.[3]?.result ?? 0n,
    sBullBalance: data?.[4]?.result ?? 0n,
    isLoading,
    error,
    refetch,
  }
}
