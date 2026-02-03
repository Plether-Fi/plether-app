import { useWriteContract, usePublicClient } from 'wagmi'
import { useState, useCallback } from 'react'
import { type Address } from 'viem'
import { Result } from 'better-result'
import { ERC20_ABI } from '../contracts/abis'
import { useTransactionStore } from '../stores/transactionStore'
import { useTransactionModal } from './useTransactionModal'
import {
  parseTransactionError,
  getErrorMessage,
  type TransactionError,
} from '../utils/errors'

export type ApproveError = TransactionError

export function useApprove(tokenAddress: Address, spenderAddress: Address) {
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const setStepInProgress = useTransactionStore((s) => s.setStepInProgress)
  const setStepSuccess = useTransactionStore((s) => s.setStepSuccess)
  const setStepError = useTransactionStore((s) => s.setStepError)
  const txModal = useTransactionModal()
  const publicClient = usePublicClient()

  const { writeContractAsync, isPending, error: writeError, reset: resetWrite } = useWriteContract()

  const [isConfirming, setIsConfirming] = useState(false)
  const [isSuccess, setIsSuccess] = useState(false)

  const approve = useCallback(async (amount: bigint): Promise<Result<`0x${string}`, ApproveError>> => {
    const txId = crypto.randomUUID()
    addTransaction({
      id: txId,
      type: 'approve',
      status: 'pending',
      hash: undefined,
      title: 'Approving USDC',
      steps: [{ label: 'Approve spending', status: 'pending' }],
    })
    txModal.open({ transactionId: txId })
    setStepInProgress(txId, 0)

    setIsSuccess(false)
    setIsConfirming(false)

    return Result.tryPromise({
      try: async () => {
        const hash = await writeContractAsync({
          address: tokenAddress,
          abi: ERC20_ABI,
          functionName: 'approve',
          args: [spenderAddress, amount],
        })

        setIsConfirming(true)

        const receipt = await publicClient.waitForTransactionReceipt({ hash })

        if (receipt.status === 'reverted') {
          throw new Error('Transaction reverted')
        }

        setStepSuccess(txId, hash)
        setIsConfirming(false)
        setIsSuccess(true)

        return hash
      },
      catch: (err) => {
        setIsConfirming(false)
        const txError = err instanceof Error && '_tag' in err
          ? err as TransactionError
          : parseTransactionError(err)
        setStepError(txId, 0, getErrorMessage(txError))
        return txError
      },
    })
  }, [tokenAddress, spenderAddress, addTransaction, setStepInProgress, setStepSuccess, setStepError, txModal, writeContractAsync, publicClient])

  const reset = useCallback(() => {
    resetWrite()
    setIsConfirming(false)
    setIsSuccess(false)
  }, [resetWrite])

  return {
    approve,
    isPending,
    isConfirming,
    isSuccess,
    error: writeError,
    reset,
  }
}
