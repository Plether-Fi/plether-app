import { useAccount, useReadContract, useWriteContract, useWaitForTransactionReceipt } from 'wagmi'
import { useRef, useEffect } from 'react'
import { zeroAddress } from 'viem'
import { Result } from 'better-result'
import { LEVERAGE_ROUTER_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'
import { useTransactionStore } from '../stores/transactionStore'
import {
  parseTransactionError,
  getErrorMessage,
  type TransactionError,
} from '../utils/errors'
import { NotConnectedError } from './usePlethCore'

export type LeverageError = NotConnectedError | TransactionError

export function useLeveragePosition(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = side === 'BEAR' ? addresses?.LEVERAGE_ROUTER : addresses?.BULL_LEVERAGE_ROUTER
  const queryAddress = address ?? zeroAddress

  const { data, isLoading, error, refetch } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'getPosition',
    args: [queryAddress],
    query: {
      enabled: !!address && !!routerAddress,
    },
  })

  const { data: healthFactor } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'getHealthFactor',
    args: [queryAddress],
    query: {
      enabled: !!address && !!routerAddress && !!data && data[0] > 0n,
    },
  })

  const { data: liquidationPrice } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'getLiquidationPrice',
    args: [queryAddress],
    query: {
      enabled: !!address && !!routerAddress && !!data && data[0] > 0n,
    },
  })

  return {
    collateral: data?.[0] ?? 0n,
    debt: data?.[1] ?? 0n,
    leverage: data?.[2] ?? 0n,
    healthFactor: healthFactor ?? 0n,
    liquidationPrice: liquidationPrice ?? 0n,
    hasPosition: (data?.[0] ?? 0n) > 0n,
    isLoading,
    error,
    refetch,
  }
}

export function usePreviewOpenLeverage(side: 'BEAR' | 'BULL', principal: bigint, leverage: bigint) {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = side === 'BEAR' ? addresses?.LEVERAGE_ROUTER : addresses?.BULL_LEVERAGE_ROUTER

  const { data, isLoading, error, refetch } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'previewOpenLeverage',
    args: [principal, leverage],
    query: {
      enabled: !!routerAddress && principal > 0n && leverage > 0n,
    },
  })

  return {
    positionSize: data?.[0] ?? 0n,
    debt: data?.[1] ?? 0n,
    liquidationPrice: data?.[2] ?? 0n,
    isLoading,
    error,
    refetch,
  }
}

export function useOpenLeverage(side: 'BEAR' | 'BULL') {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = side === 'BEAR' ? addresses?.LEVERAGE_ROUTER : addresses?.BULL_LEVERAGE_ROUTER
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

  const openPosition = async (
    principal: bigint,
    leverage: bigint,
    maxSlippageBps: bigint,
    deadline: bigint
  ): Promise<Result<`0x${string}`, LeverageError>> => {
    if (!routerAddress) {
      return Result.err(new NotConnectedError())
    }

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'leverage',
      status: 'pending',
      hash: undefined,
      description: `Opening ${side} leverage position`,
    })

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
          writeContract(
            {
              address: routerAddress,
              abi: LEVERAGE_ROUTER_ABI,
              functionName: 'openLeverage',
              args: [principal, leverage, maxSlippageBps, deadline],
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
    openPosition,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}

export function useCloseLeverage(side: 'BEAR' | 'BULL') {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = side === 'BEAR' ? addresses?.LEVERAGE_ROUTER : addresses?.BULL_LEVERAGE_ROUTER
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

  const closePosition = async (
    debtToRepay: bigint,
    collateralToWithdraw: bigint,
    maxSlippageBps: bigint,
    deadline: bigint
  ): Promise<Result<`0x${string}`, LeverageError>> => {
    if (!routerAddress) {
      return Result.err(new NotConnectedError())
    }

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'leverage',
      status: 'pending',
      hash: undefined,
      description: `Closing ${side} leverage position`,
    })

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
          writeContract(
            {
              address: routerAddress,
              abi: LEVERAGE_ROUTER_ABI,
              functionName: 'closeLeverage',
              args: [debtToRepay, collateralToWithdraw, maxSlippageBps, deadline],
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
    closePosition,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}

export function useAdjustCollateral(side: 'BEAR' | 'BULL') {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = side === 'BEAR' ? addresses?.LEVERAGE_ROUTER : addresses?.BULL_LEVERAGE_ROUTER
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

  const addCollateral = async (amount: bigint): Promise<Result<`0x${string}`, LeverageError>> => {
    if (!routerAddress) {
      return Result.err(new NotConnectedError())
    }

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'leverage',
      status: 'pending',
      hash: undefined,
      description: 'Adding collateral',
    })

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
          writeContract(
            {
              address: routerAddress,
              abi: LEVERAGE_ROUTER_ABI,
              functionName: 'addCollateral',
              args: [amount],
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

  const removeCollateral = async (amount: bigint): Promise<Result<`0x${string}`, LeverageError>> => {
    if (!routerAddress) {
      return Result.err(new NotConnectedError())
    }

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'leverage',
      status: 'pending',
      hash: undefined,
      description: 'Removing collateral',
    })

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
          writeContract(
            {
              address: routerAddress,
              abi: LEVERAGE_ROUTER_ABI,
              functionName: 'removeCollateral',
              args: [amount],
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
    addCollateral,
    removeCollateral,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}
