import { useAccount, useReadContract, useWriteContract, useWaitForTransactionReceipt } from 'wagmi'
import { useRef, useEffect } from 'react'
import { Result } from 'better-result'
import { PLETH_CORE_ABI } from '../contracts/abis'
import { getAddresses, DEFAULT_CHAIN_ID } from '../contracts/addresses'
import { useTransactionStore } from '../stores/transactionStore'
import {
  parseTransactionError,
  getErrorMessage,
  type TransactionError,
} from '../utils/errors'

export class NotConnectedError extends Error {
  readonly _tag = 'NotConnectedError' as const
  constructor() {
    super('Wallet not connected or wrong network')
  }
}

export type MintError = NotConnectedError | TransactionError
export type BurnError = NotConnectedError | TransactionError

export function usePlethCoreStatus() {
  const { chainId } = useAccount()
  const addresses = getAddresses(chainId ?? DEFAULT_CHAIN_ID)

  const { data, isLoading, error, refetch } = useReadContract({
    address: addresses.SYNTHETIC_SPLITTER,
    abi: PLETH_CORE_ABI,
    functionName: 'currentStatus',
  })

  return {
    status: data ?? 0,
    isLoading,
    error,
    refetch,
  }
}

export function usePlethCoreSystemStatus() {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const { data, isLoading, error, refetch } = useReadContract({
    address: addresses?.SYNTHETIC_SPLITTER,
    abi: PLETH_CORE_ABI,
    functionName: 'getSystemStatus',
    query: {
      enabled: !!addresses,
    },
  })

  return {
    status: data?.[0] ?? 0,
    totalSupply: data?.[1] ?? 0n,
    oraclePrice: data?.[2] ?? 0n,
    isLoading,
    error,
    refetch,
  }
}

export function usePreviewMint(pairAmount: bigint) {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const { data, isLoading, error, refetch } = useReadContract({
    address: addresses?.SYNTHETIC_SPLITTER,
    abi: PLETH_CORE_ABI,
    functionName: 'previewMint',
    args: [pairAmount],
    query: {
      enabled: !!addresses && pairAmount > 0n,
    },
  })

  return {
    usdcRequired: data?.[0] ?? 0n,
    isLoading,
    error,
    refetch,
  }
}

export function usePreviewBurn(pairAmount: bigint) {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const { data, isLoading, error, refetch } = useReadContract({
    address: addresses?.SYNTHETIC_SPLITTER,
    abi: PLETH_CORE_ABI,
    functionName: 'previewBurn',
    args: [pairAmount],
    query: {
      enabled: !!addresses && pairAmount > 0n,
    },
  })

  return {
    usdcToReturn: data?.[0] ?? 0n,
    isLoading,
    error,
    refetch,
  }
}

export function useMint() {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
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

  const mint = async (pairAmount: bigint): Promise<Result<`0x${string}`, MintError>> => {
    if (!addresses) {
      return Result.err(new NotConnectedError())
    }

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'mint',
      status: 'pending',
      hash: undefined,
      title: 'Minting plDXY-BEAR + plDXY-BULL',
      steps: [{ label: 'Mint pairs', status: 'pending' }],
    })

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
          writeContract(
            {
              address: addresses.SYNTHETIC_SPLITTER,
              abi: PLETH_CORE_ABI,
              functionName: 'mint',
              args: [pairAmount],
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
    mint,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}

export function useBurn() {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
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

  const burn = async (pairAmount: bigint): Promise<Result<`0x${string}`, BurnError>> => {
    if (!addresses) {
      return Result.err(new NotConnectedError())
    }

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'burn',
      status: 'pending',
      hash: undefined,
      title: 'Redeeming plDXY-BEAR + plDXY-BULL for USDC',
      steps: [{ label: 'Redeem pairs', status: 'pending' }],
    })

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
          writeContract(
            {
              address: addresses.SYNTHETIC_SPLITTER,
              abi: PLETH_CORE_ABI,
              functionName: 'burn',
              args: [pairAmount],
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
    burn,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}
