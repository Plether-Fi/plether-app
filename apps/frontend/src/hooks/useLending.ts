import { useAccount, useReadContract } from 'wagmi'
import { useCallback } from 'react'
import { zeroAddress } from 'viem'
import { Result } from 'better-result'
import { MORPHO_ABI, ERC20_ABI, MORPHO_ORACLE_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'
import { type TransactionError } from '../utils/errors'
import { NotConnectedError } from './usePlethCore'
import { useMarketConfig } from './useMarketConfig'
import { useContractTransaction } from './useContractTransaction'
export type { MarketParams } from './useMarketConfig'

export type LendingError = NotConnectedError | TransactionError

export function useLendingPosition(side: 'BEAR' | 'BULL') {
  const { address } = useAccount()
  const { morphoAddress, marketId } = useMarketConfig(side)
  const queryAddress = address ?? zeroAddress

  const {
    data: position,
    isLoading,
    error,
    refetch,
  } = useReadContract({
    address: morphoAddress,
    abi: MORPHO_ABI,
    functionName: 'position',
    args: [marketId ?? '0x', queryAddress],
    query: { enabled: !!address && !!morphoAddress && !!marketId },
  })

  const { data: marketData, refetch: refetchMarket } = useReadContract({
    address: morphoAddress,
    abi: MORPHO_ABI,
    functionName: 'market',
    args: [marketId ?? '0x'],
    query: { enabled: !!morphoAddress && !!marketId },
  })

  const supplyShares = position?.[0] ?? 0n
  const borrowShares = position?.[1] ?? 0n
  const collateral = position?.[2] ?? 0n

  const totalSupplyAssets = marketData?.[0] ?? 0n
  const totalSupplyShares = marketData?.[1] ?? 0n
  const totalBorrowAssets = marketData?.[2] ?? 0n
  const totalBorrowShares = marketData?.[3] ?? 0n

  const suppliedAssets = totalSupplyShares > 0n
    ? (supplyShares * totalSupplyAssets) / totalSupplyShares
    : 0n

  const borrowedAssets = totalBorrowShares > 0n
    ? (borrowShares * totalBorrowAssets) / totalBorrowShares
    : 0n

  const refetchAll = useCallback(async () => {
    await Promise.all([refetch(), refetchMarket()])
  }, [refetch, refetchMarket])

  return {
    supplyShares,
    borrowShares,
    collateral,
    suppliedAssets,
    borrowedAssets,
    totalSupplyAssets,
    totalBorrowAssets,
    isLoading,
    error,
    refetch: refetchAll,
    morphoAddress,
    marketId,
  }
}

export function useCombinedLendingPosition() {
  const bearPosition = useLendingPosition('BEAR')
  const bullPosition = useLendingPosition('BULL')

  const totalSupplied = bearPosition.suppliedAssets + bullPosition.suppliedAssets
  const totalBorrowed = bearPosition.borrowedAssets + bullPosition.borrowedAssets

  const refetch = useCallback(async () => {
    await Promise.all([bearPosition.refetch(), bullPosition.refetch()])
  }, [bearPosition, bullPosition])

  return {
    bearPosition,
    bullPosition,
    totalSupplied,
    totalBorrowed,
    isLoading: bearPosition.isLoading || bullPosition.isLoading,
    refetch,
  }
}

export function useLendingMarketInfo(side: 'BEAR' | 'BULL') {
  const { morphoAddress, marketId } = useMarketConfig(side)

  const { data: marketData, isLoading } = useReadContract({
    address: morphoAddress,
    abi: MORPHO_ABI,
    functionName: 'market',
    args: [marketId ?? '0x'],
    query: { enabled: !!morphoAddress && !!marketId },
  })

  const totalSupplyAssets = marketData?.[0] ?? 0n
  const totalBorrowAssets = marketData?.[2] ?? 0n

  const utilization = totalSupplyAssets > 0n
    ? Number((totalBorrowAssets * 10000n) / totalSupplyAssets) / 100
    : 0

  return {
    totalSupplyAssets,
    totalBorrowAssets,
    utilization,
    isLoading,
  }
}

export function useAvailableToBorrow(side: 'BEAR' | 'BULL') {
  const { address } = useAccount()
  const { morphoAddress, marketId, marketParams } = useMarketConfig(side)
  const queryAddress = address ?? zeroAddress

  const { data: position } = useReadContract({
    address: morphoAddress,
    abi: MORPHO_ABI,
    functionName: 'position',
    args: [marketId ?? '0x', queryAddress],
    query: { enabled: !!address && !!morphoAddress && !!marketId },
  })

  const { data: marketData } = useReadContract({
    address: morphoAddress,
    abi: MORPHO_ABI,
    functionName: 'market',
    args: [marketId ?? '0x'],
    query: { enabled: !!morphoAddress && !!marketId },
  })

  const { data: oraclePrice } = useReadContract({
    address: marketParams?.oracle,
    abi: MORPHO_ORACLE_ABI,
    functionName: 'price',
    query: { enabled: !!marketParams?.oracle },
  })

  const collateral = position?.[2] ?? 0n
  const borrowShares = position?.[1] ?? 0n
  const totalBorrowAssets = marketData?.[2] ?? 0n
  const totalBorrowShares = marketData?.[3] ?? 0n
  const lltv = marketParams?.lltv ?? 0n
  const price = oraclePrice ?? 0n

  const currentDebt = totalBorrowShares > 0n
    ? (borrowShares * totalBorrowAssets) / totalBorrowShares
    : 0n

  // Collateral USD value: collateral * oraclePrice / 10^39
  // oraclePrice is 1e24 scale (36 + 6 loan dec - 18 collateral dec), collateral is 21 effective dec
  // Result: 21 + 24 - 39 = 6 decimals (USDC)
  const collateralUsd = price > 0n
    ? (collateral * price) / 10n ** 39n
    : 0n

  // Morpho max borrow: collateral * oraclePrice * lltv / 10^57
  // lltv is 1e18 scale, so 39 + 18 = 57
  const maxBorrow = lltv > 0n && price > 0n
    ? (collateral * price * lltv) / 10n ** 57n
    : 0n

  const availableToBorrow = maxBorrow > currentDebt ? maxBorrow - currentDebt : 0n

  return {
    collateral,
    collateralUsd,
    currentDebt,
    maxBorrow,
    availableToBorrow,
    lltv,
  }
}

export function useSupply(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const { morphoAddress, marketParams, marketId } = useMarketConfig(side)
  const { sendTransaction, ...txState } = useContractTransaction()

  const supply = async (assets: bigint): Promise<Result<`0x${string}`, LendingError>> => {
    if (!morphoAddress || !marketParams || !address || !addresses) {
      return Result.err(new NotConnectedError())
    }

    return sendTransaction(
      { type: 'supply', title: `Supplying USDC to ${side} market`,
        steps: [{ label: 'Supply USDC' }, { label: 'Confirming onchain (~12s)' }] },
      { address: morphoAddress, abi: MORPHO_ABI, functionName: 'supply',
        args: [marketParams, assets, 0n, address, '0x'] },
    )
  }

  return { supply, ...txState, morphoAddress, marketId }
}

export function useWithdraw(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const { morphoAddress, marketParams } = useMarketConfig(side)
  const { sendTransaction, ...txState } = useContractTransaction()

  const withdraw = async (assets: bigint): Promise<Result<`0x${string}`, LendingError>> => {
    if (!morphoAddress || !marketParams || !address || !addresses) {
      return Result.err(new NotConnectedError())
    }

    return sendTransaction(
      { type: 'withdraw', title: `Withdrawing USDC from ${side} market`,
        steps: [{ label: 'Withdraw USDC' }, { label: 'Confirming onchain (~12s)' }] },
      { address: morphoAddress, abi: MORPHO_ABI, functionName: 'withdraw',
        args: [marketParams, assets, 0n, address, address] },
    )
  }

  return { withdraw, ...txState }
}

export function useBorrow(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const { morphoAddress, marketParams } = useMarketConfig(side)
  const { sendTransaction, ...txState } = useContractTransaction()

  const borrow = async (assets: bigint): Promise<Result<`0x${string}`, LendingError>> => {
    if (!morphoAddress || !marketParams || !address || !addresses) {
      return Result.err(new NotConnectedError())
    }

    return sendTransaction(
      { type: 'borrow', title: `Borrowing USDC from ${side} market`,
        steps: [{ label: 'Borrow USDC' }, { label: 'Confirming onchain (~12s)' }] },
      { address: morphoAddress, abi: MORPHO_ABI, functionName: 'borrow',
        args: [marketParams, assets, 0n, address, address] },
    )
  }

  return { borrow, ...txState }
}

export function useRepay(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const { morphoAddress, marketParams, marketId } = useMarketConfig(side)
  const { sendTransaction, ...txState } = useContractTransaction()

  const { data: marketData } = useReadContract({
    address: morphoAddress,
    abi: MORPHO_ABI,
    functionName: 'market',
    args: [marketId ?? '0x'],
    query: { enabled: !!morphoAddress && !!marketId },
  })

  const totalBorrowAssets = marketData?.[2] ?? 0n
  const totalBorrowShares = marketData?.[3] ?? 0n

  const assetsToShares = useCallback((assets: bigint): bigint => {
    if (totalBorrowAssets === 0n) return assets
    return (assets * totalBorrowShares) / totalBorrowAssets
  }, [totalBorrowAssets, totalBorrowShares])

  const repay = async (assets: bigint): Promise<Result<`0x${string}`, LendingError>> => {
    if (!morphoAddress || !marketParams || !address || !addresses) {
      return Result.err(new NotConnectedError())
    }

    const shares = assetsToShares(assets)

    return sendTransaction(
      { type: 'repay', title: `Repaying USDC to ${side} market`,
        steps: [{ label: 'Repay USDC' }, { label: 'Confirming onchain (~12s)' }] },
      { address: morphoAddress, abi: MORPHO_ABI, functionName: 'repay',
        args: [marketParams, 0n, shares, address, '0x'] },
    )
  }

  return { repay, ...txState }
}

export function useUsdcAllowanceForMorpho(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const { morphoAddress } = useMarketConfig(side)
  const queryAddress = address ?? zeroAddress

  const { data: allowance, refetch } = useReadContract({
    address: addresses?.USDC,
    abi: ERC20_ABI,
    functionName: 'allowance',
    args: [queryAddress, morphoAddress ?? zeroAddress],
    query: { enabled: !!address && !!addresses && !!morphoAddress },
  })

  return {
    allowance: allowance ?? 0n,
    refetch,
    morphoAddress,
  }
}
