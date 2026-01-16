import { useWriteContract, useWaitForTransactionReceipt } from 'wagmi'
import { useRef, useEffect } from 'react'
import { type Address } from 'viem'
import { ERC20_ABI } from '../contracts/abis'
import { useTransactionStore } from '../stores/transactionStore'
import { parseTransactionError } from '../utils/errors'

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
      updateTransaction(txIdRef.current, {
        status: 'failed',
        errorMessage: parseTransactionError(receiptError),
      })
      txIdRef.current = null
    }
  }, [isError, receiptError, updateTransaction])

  const approve = async (amount: bigint) => {
    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'approve',
      status: 'pending',
      hash: undefined,
      description: 'Approving token spend',
    })

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
        },
        onError: (err) => {
          updateTransaction(txId, {
            status: 'failed',
            errorMessage: parseTransactionError(err),
          })
          txIdRef.current = null
        },
      }
    )
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
