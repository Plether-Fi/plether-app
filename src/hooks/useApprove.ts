import { useWriteContract, useWaitForTransactionReceipt } from 'wagmi'
import { useRef, useEffect } from 'react'
import { type Address } from 'viem'
import { Result } from 'better-result'
import { ERC20_ABI } from '../contracts/abis'
import { useTransactionStore } from '../stores/transactionStore'
import {
  parseTransactionError,
  getErrorMessage,
  type TransactionError,
} from '../utils/errors'

export type ApproveError = TransactionError

export function useApprove(tokenAddress: Address, spenderAddress: Address) {
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)
  const txIdRef = useRef<string | null>(null)

  const { writeContract, data: hash, isPending, error: writeError, reset: resetWrite } = useWriteContract()

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

  const approve = async (amount: bigint): Promise<Result<`0x${string}`, ApproveError>> => {
    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'approve',
      status: 'pending',
      hash: undefined,
      title: 'Approving token spend',
      steps: [{ label: 'Approve', status: 'pending' }],
    })

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
          writeContract(
            {
              address: tokenAddress,
              abi: ERC20_ABI,
              functionName: 'approve',
              args: [spenderAddress, amount],
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

  const reset = () => {
    resetWrite()
  }

  return {
    approve,
    isPending,
    isConfirming,
    isSuccess,
    error: writeError ?? receiptError,
    reset,
  }
}
