import { useWriteContract, useWaitForTransactionReceipt } from 'wagmi'
import { useRef, useEffect } from 'react'
import { type Abi } from 'viem'
import { Result } from 'better-result'
import { useTransactionStore, type TransactionType } from '../stores/transactionStore'
import {
  parseTransactionError,
  getErrorMessage,
  type TransactionError,
} from '../utils/errors'

interface TransactionConfig {
  type: TransactionType
  title: string
  steps: { label: string }[]
}

interface ContractCallConfig {
  address: `0x${string}`
  abi: Abi
  functionName: string
  args: readonly unknown[]
}

export function useContractTransaction() {
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)
  const setStepInProgress = useTransactionStore((s) => s.setStepInProgress)
  const setStepSuccess = useTransactionStore((s) => s.setStepSuccess)
  const txIdRef = useRef<string | null>(null)

  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess, isError, error: receiptError } = useWaitForTransactionReceipt({
    hash,
  })

  useEffect(() => {
    if (isSuccess && txIdRef.current && hash) {
      setStepSuccess(txIdRef.current, hash)
      txIdRef.current = null
    }
  }, [isSuccess, hash, setStepSuccess])

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

  const sendTransaction = (
    txConfig: TransactionConfig,
    callConfig: ContractCallConfig,
  ): Promise<Result<`0x${string}`, TransactionError>> => {
    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: txConfig.type,
      status: 'pending',
      hash: undefined,
      title: txConfig.title,
      steps: txConfig.steps.map((s) => ({ label: s.label, status: 'pending' as const })),
    })
    setStepInProgress(txId, 0)

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
          writeContract(
            callConfig as Parameters<typeof writeContract>[0],
            {
              onSuccess: (hash) => {
                setStepInProgress(txId, 1)
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

  return { sendTransaction, isPending, isConfirming, isSuccess, error, reset, hash }
}
