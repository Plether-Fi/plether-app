import { useAccount, useReadContract } from 'wagmi'
import { useMemo } from 'react'
import { keccak256, encodeAbiParameters, type Address } from 'viem'
import { LEVERAGE_ROUTER_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'

export interface MarketParams {
  loanToken: Address
  collateralToken: Address
  oracle: Address
  irm: Address
  lltv: bigint
}

export function useMarketConfig(side: 'BEAR' | 'BULL') {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = side === 'BEAR' ? addresses?.LEVERAGE_ROUTER : addresses?.BULL_LEVERAGE_ROUTER

  const { data: morphoAddress } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'MORPHO',
    query: { enabled: !!routerAddress },
  })

  const { data: marketParams } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'marketParams',
    query: { enabled: !!routerAddress },
  })

  const marketId = useMemo(() => {
    if (!marketParams) return undefined
    const [loanToken, collateralToken, oracle, irm, lltv] = marketParams
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
  }, [marketParams])

  const marketParamsStruct: MarketParams | undefined = useMemo(() => {
    if (!marketParams) return undefined
    const [loanToken, collateralToken, oracle, irm, lltv] = marketParams
    return { loanToken, collateralToken, oracle, irm, lltv }
  }, [marketParams])

  return {
    morphoAddress,
    marketParams: marketParamsStruct,
    marketId,
    routerAddress,
  }
}
