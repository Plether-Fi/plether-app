import { useAccount, useReadContract, useWriteContract, useWaitForTransactionReceipt } from 'wagmi'
import { useRef, useEffect, useMemo, useCallback } from 'react'
import { zeroAddress, keccak256, encodeAbiParameters, type Address } from 'viem'
import { Result } from 'better-result'
import { LEVERAGE_ROUTER_ABI, MORPHO_ABI, ERC20_ABI, MORPHO_ORACLE_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'
import { useTransactionStore } from '../stores/transactionStore'
import {
  parseTransactionError,
  getErrorMessage,
  type TransactionError,
} from '../utils/errors'
import { NotConnectedError } from './usePlethCore'

export type LendingError = NotConnectedError | TransactionError

export interface MarketParams {
  loanToken: Address
  collateralToken: Address
  oracle: Address
  irm: Address
  lltv: bigint
}

function useMarketConfig(side: 'BEAR' | 'BULL') {
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

  // Collateral USD value: collateral * oraclePrice / 10^51
  // oraclePrice is 1e36 scale, collateral is splDXY shares (18 dec + 1000x offset = 21 effective)
  // Result: 21 + 36 - 51 = 6 decimals (USDC)
  const collateralUsd = price > 0n
    ? (collateral * price) / 10n ** 51n
    : 0n

  // Morpho max borrow: collateral * oraclePrice * lltv / 10^69
  // lltv is 1e18 scale, so 51 + 18 = 69
  const maxBorrow = lltv > 0n && price > 0n
    ? (collateral * price * lltv) / 10n ** 69n
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
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)
  const txIdRef = useRef<string | null>(null)

  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess, isError, error: receiptError } = useWaitForTransactionReceipt({
    hash,
  })

  useEffect(() => {
    if (isSuccess && txIdRef.current) {
      updateTransaction(txIdRef.current, { status: 'success' })
      txIdRef.current = null
    }
  }, [isSuccess, updateTransaction])

  useEffect(() => {
    if (isError && txIdRef.current) {
      const txError = parseTransactionError(receiptError)
      updateTransaction(txIdRef.current, {
        status: 'failed',
        errorMessage: getErrorMessage(txError),
      })
      txIdRef.current = null
    }
  }, [isError, receiptError, updateTransaction])

  const supply = async (assets: bigint): Promise<Result<`0x${string}`, LendingError>> => {
    if (!morphoAddress || !marketParams || !address || !addresses) {
      return Result.err(new NotConnectedError())
    }

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'supply',
      status: 'pending',
      hash: undefined,
      title: `Supplying USDC to ${side} market`,
      steps: [{ label: 'Supply USDC', status: 'pending' }],
    })

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
          writeContract(
            {
              address: morphoAddress,
              abi: MORPHO_ABI,
              functionName: 'supply',
              args: [marketParams, assets, 0n, address, '0x'],
            },
            {
              onSuccess: (hash) => {
                updateTransaction(txId, { hash, status: 'confirming' })
                resolve(hash)
              },
              onError: (err) => {
                const txError = parseTransactionError(err)
                updateTransaction(txId, {
                  status: 'failed',
                  errorMessage: getErrorMessage(txError),
                })
                txIdRef.current = null
                reject(txError)
              },
            }
          )
        }),
      catch: (err) => {
        if (err instanceof Error && '_tag' in err) {
          return err as TransactionError
        }
        const txError = parseTransactionError(err)
        updateTransaction(txId, {
          status: 'failed',
          errorMessage: getErrorMessage(txError),
        })
        txIdRef.current = null
        return txError
      },
    })
  }

  return {
    supply,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
    morphoAddress,
    marketId,
  }
}

export function useWithdraw(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const { morphoAddress, marketParams } = useMarketConfig(side)
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)
  const txIdRef = useRef<string | null>(null)

  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess, isError, error: receiptError } = useWaitForTransactionReceipt({
    hash,
  })

  useEffect(() => {
    if (isSuccess && txIdRef.current) {
      updateTransaction(txIdRef.current, { status: 'success' })
      txIdRef.current = null
    }
  }, [isSuccess, updateTransaction])

  useEffect(() => {
    if (isError && txIdRef.current) {
      const txError = parseTransactionError(receiptError)
      updateTransaction(txIdRef.current, {
        status: 'failed',
        errorMessage: getErrorMessage(txError),
      })
      txIdRef.current = null
    }
  }, [isError, receiptError, updateTransaction])

  const withdraw = async (assets: bigint): Promise<Result<`0x${string}`, LendingError>> => {
    if (!morphoAddress || !marketParams || !address || !addresses) {
      return Result.err(new NotConnectedError())
    }

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'withdraw',
      status: 'pending',
      hash: undefined,
      title: `Withdrawing USDC from ${side} market`,
      steps: [{ label: 'Withdraw USDC', status: 'pending' }],
    })

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
          writeContract(
            {
              address: morphoAddress,
              abi: MORPHO_ABI,
              functionName: 'withdraw',
              args: [marketParams, assets, 0n, address, address],
            },
            {
              onSuccess: (hash) => {
                updateTransaction(txId, { hash, status: 'confirming' })
                resolve(hash)
              },
              onError: (err) => {
                const txError = parseTransactionError(err)
                updateTransaction(txId, {
                  status: 'failed',
                  errorMessage: getErrorMessage(txError),
                })
                txIdRef.current = null
                reject(txError)
              },
            }
          )
        }),
      catch: (err) => {
        if (err instanceof Error && '_tag' in err) {
          return err as TransactionError
        }
        const txError = parseTransactionError(err)
        updateTransaction(txId, {
          status: 'failed',
          errorMessage: getErrorMessage(txError),
        })
        txIdRef.current = null
        return txError
      },
    })
  }

  return {
    withdraw,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}

export function useBorrow(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const { morphoAddress, marketParams } = useMarketConfig(side)
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)
  const txIdRef = useRef<string | null>(null)

  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess, isError, error: receiptError } = useWaitForTransactionReceipt({
    hash,
  })

  useEffect(() => {
    if (isSuccess && txIdRef.current) {
      updateTransaction(txIdRef.current, { status: 'success' })
      txIdRef.current = null
    }
  }, [isSuccess, updateTransaction])

  useEffect(() => {
    if (isError && txIdRef.current) {
      const txError = parseTransactionError(receiptError)
      updateTransaction(txIdRef.current, {
        status: 'failed',
        errorMessage: getErrorMessage(txError),
      })
      txIdRef.current = null
    }
  }, [isError, receiptError, updateTransaction])

  const borrow = async (assets: bigint): Promise<Result<`0x${string}`, LendingError>> => {
    if (!morphoAddress || !marketParams || !address || !addresses) {
      return Result.err(new NotConnectedError())
    }

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'borrow',
      status: 'pending',
      hash: undefined,
      title: `Borrowing USDC from ${side} market`,
      steps: [{ label: 'Borrow USDC', status: 'pending' }],
    })

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
          writeContract(
            {
              address: morphoAddress,
              abi: MORPHO_ABI,
              functionName: 'borrow',
              args: [marketParams, assets, 0n, address, address],
            },
            {
              onSuccess: (hash) => {
                updateTransaction(txId, { hash, status: 'confirming' })
                resolve(hash)
              },
              onError: (err) => {
                const txError = parseTransactionError(err)
                updateTransaction(txId, {
                  status: 'failed',
                  errorMessage: getErrorMessage(txError),
                })
                txIdRef.current = null
                reject(txError)
              },
            }
          )
        }),
      catch: (err) => {
        if (err instanceof Error && '_tag' in err) {
          return err as TransactionError
        }
        const txError = parseTransactionError(err)
        updateTransaction(txId, {
          status: 'failed',
          errorMessage: getErrorMessage(txError),
        })
        txIdRef.current = null
        return txError
      },
    })
  }

  return {
    borrow,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}

export function useRepay(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const { morphoAddress, marketParams, marketId } = useMarketConfig(side)
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)
  const txIdRef = useRef<string | null>(null)

  const { data: marketData } = useReadContract({
    address: morphoAddress,
    abi: MORPHO_ABI,
    functionName: 'market',
    args: [marketId ?? '0x'],
    query: { enabled: !!morphoAddress && !!marketId },
  })

  const totalBorrowAssets = marketData?.[2] ?? 0n
  const totalBorrowShares = marketData?.[3] ?? 0n

  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess, isError, error: receiptError } = useWaitForTransactionReceipt({
    hash,
  })

  useEffect(() => {
    if (isSuccess && txIdRef.current) {
      updateTransaction(txIdRef.current, { status: 'success' })
      txIdRef.current = null
    }
  }, [isSuccess, updateTransaction])

  useEffect(() => {
    if (isError && txIdRef.current) {
      const txError = parseTransactionError(receiptError)
      updateTransaction(txIdRef.current, {
        status: 'failed',
        errorMessage: getErrorMessage(txError),
      })
      txIdRef.current = null
    }
  }, [isError, receiptError, updateTransaction])

  const assetsToShares = useCallback((assets: bigint): bigint => {
    if (totalBorrowAssets === 0n) return assets
    return (assets * totalBorrowShares) / totalBorrowAssets
  }, [totalBorrowAssets, totalBorrowShares])

  const repay = async (assets: bigint): Promise<Result<`0x${string}`, LendingError>> => {
    if (!morphoAddress || !marketParams || !address || !addresses) {
      return Result.err(new NotConnectedError())
    }

    const shares = assetsToShares(assets)

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'repay',
      status: 'pending',
      hash: undefined,
      title: `Repaying USDC to ${side} market`,
      steps: [{ label: 'Repay USDC', status: 'pending' }],
    })

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
          writeContract(
            {
              address: morphoAddress,
              abi: MORPHO_ABI,
              functionName: 'repay',
              args: [marketParams, 0n, shares, address, '0x'],
            },
            {
              onSuccess: (hash) => {
                updateTransaction(txId, { hash, status: 'confirming' })
                resolve(hash)
              },
              onError: (err) => {
                const txError = parseTransactionError(err)
                updateTransaction(txId, {
                  status: 'failed',
                  errorMessage: getErrorMessage(txError),
                })
                txIdRef.current = null
                reject(txError)
              },
            }
          )
        }),
      catch: (err) => {
        if (err instanceof Error && '_tag' in err) {
          return err as TransactionError
        }
        const txError = parseTransactionError(err)
        updateTransaction(txId, {
          status: 'failed',
          errorMessage: getErrorMessage(txError),
        })
        txIdRef.current = null
        return txError
      },
    })
  }

  return {
    repay,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
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
