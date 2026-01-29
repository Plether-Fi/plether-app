import { useAccount, useReadContract } from 'wagmi'
import { PLETH_CORE_ABI, BASKET_ORACLE_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'

export function useTokenPrices() {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const { data: roundData, isLoading: priceLoading } = useReadContract({
    address: addresses?.BASKET_ORACLE,
    abi: BASKET_ORACLE_ABI,
    functionName: 'latestRoundData',
    query: {
      enabled: !!addresses,
    },
  })

  const { data: cap, isLoading: capLoading } = useReadContract({
    address: addresses?.SYNTHETIC_SPLITTER,
    abi: PLETH_CORE_ABI,
    functionName: 'CAP',
    query: {
      enabled: !!addresses,
    },
  })

  // Oracle price has 8 decimals
  const oraclePrice = roundData?.[1] ?? 0n
  const capValue = cap ?? 0n

  // BEAR price = CAP - oracle (bearish = profits when plDXY falls)
  // BULL price = oracle (bullish = profits when plDXY rises)
  const bearPrice = capValue > oraclePrice ? capValue - oraclePrice : 0n
  const bullPrice = oraclePrice > 0n ? oraclePrice : 0n

  return {
    bearPrice,  // 8 decimals
    bullPrice,  // 8 decimals
    oraclePrice,
    cap: capValue,
    isLoading: priceLoading || capLoading,
  }
}
