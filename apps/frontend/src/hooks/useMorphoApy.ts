import { useAccount, useReadContract } from 'wagmi'
import { useMemo } from 'react'
import { keccak256, encodeAbiParameters } from 'viem'
import { LEVERAGE_ROUTER_ABI, MORPHO_ABI, MORPHO_IRM_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'

const SECONDS_PER_YEAR = 365n * 24n * 60n * 60n
const WAD = 10n ** 18n

export function useMorphoApy(side: 'BEAR' | 'BULL') {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = side === 'BEAR' ? addresses?.LEVERAGE_ROUTER : addresses?.BULL_LEVERAGE_ROUTER

  const { data: morphoAddress } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'MORPHO',
    query: { enabled: !!routerAddress },
  })

  const { data: marketParamsRaw } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'marketParams',
    query: { enabled: !!routerAddress },
  })

  const marketId = useMemo(() => {
    if (!marketParamsRaw) return undefined
    const [loanToken, collateralToken, oracle, irm, lltv] = marketParamsRaw
    return keccak256(
      encodeAbiParameters(
        [
          { type: 'address' },
          { type: 'address' },
          { type: 'address' },
          { type: 'address' },
          { type: 'uint256' },
        ],
        [loanToken, collateralToken, oracle, irm, lltv]
      )
    )
  }, [marketParamsRaw])

  const { data: marketData, isLoading: marketLoading } = useReadContract({
    address: morphoAddress,
    abi: MORPHO_ABI,
    functionName: 'market',
    args: [marketId ?? '0x'],
    query: { enabled: !!morphoAddress && !!marketId },
  })

  const irmAddress = marketParamsRaw?.[3]

  const marketParams = useMemo(() => {
    if (!marketParamsRaw) return undefined
    return {
      loanToken: marketParamsRaw[0],
      collateralToken: marketParamsRaw[1],
      oracle: marketParamsRaw[2],
      irm: marketParamsRaw[3],
      lltv: marketParamsRaw[4],
    }
  }, [marketParamsRaw])

  const marketStruct = useMemo(() => {
    if (!marketData) return undefined
    return {
      totalSupplyAssets: marketData[0],
      totalSupplyShares: marketData[1],
      totalBorrowAssets: marketData[2],
      totalBorrowShares: marketData[3],
      lastUpdate: marketData[4],
      fee: marketData[5],
    }
  }, [marketData])

  const { data: borrowRatePerSecond, isLoading: rateLoading } = useReadContract({
    address: irmAddress,
    abi: MORPHO_IRM_ABI,
    functionName: 'borrowRateView',
    args: marketParams && marketStruct ? [marketParams, marketStruct] : undefined,
    query: { enabled: !!irmAddress && !!marketParams && !!marketStruct },
  })

  const { supplyApy, borrowApy, utilization } = useMemo(() => {
    if (!marketStruct || !borrowRatePerSecond) {
      return { supplyApy: 0, borrowApy: 0, utilization: 0 }
    }

    const { totalSupplyAssets, totalBorrowAssets, fee } = marketStruct

    if (totalSupplyAssets === 0n) {
      return { supplyApy: 0, borrowApy: 0, utilization: 0 }
    }

    const utilizationWad = (totalBorrowAssets * WAD) / totalSupplyAssets
    const utilization = Number(utilizationWad) / 1e18

    const borrowRatePerYear = borrowRatePerSecond * SECONDS_PER_YEAR
    const borrowApy = Number(borrowRatePerYear) / 1e18

    const feeWad = fee
    const supplyRatePerYear = (borrowRatePerYear * utilizationWad * (WAD - feeWad)) / WAD / WAD
    const supplyApy = Number(supplyRatePerYear) / 1e18

    return { supplyApy, borrowApy, utilization }
  }, [marketStruct, borrowRatePerSecond])

  return {
    supplyApy,
    borrowApy,
    utilization,
    isLoading: marketLoading || rateLoading,
  }
}
