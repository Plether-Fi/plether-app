import { useAccount, useReadContract } from 'wagmi'
import { BASKET_ORACLE_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'

export function useBasketOraclePrice() {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const { data: roundData, isLoading: priceLoading, error: priceError, refetch } = useReadContract({
    address: addresses?.BASKET_ORACLE,
    abi: BASKET_ORACLE_ABI,
    functionName: 'latestRoundData',
    query: {
      enabled: !!addresses,
    },
  })

  const { data: decimals } = useReadContract({
    address: addresses?.BASKET_ORACLE,
    abi: BASKET_ORACLE_ABI,
    functionName: 'decimals',
    query: {
      enabled: !!addresses,
    },
  })

  const price = roundData?.[1] ?? 0n
  const updatedAt = roundData?.[3] ?? 0n

  return {
    price: price < 0n ? 0n : price,
    decimals: decimals ?? 8,
    lastUpdated: updatedAt,
    isLoading: priceLoading,
    error: priceError,
    refetch,
  }
}
