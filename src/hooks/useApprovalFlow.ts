import { useState, useRef, useCallback } from 'react'
import { type Address } from 'viem'
import { useWriteContract, usePublicClient } from 'wagmi'
import { Result } from 'better-result'
import { useAllowance } from './useAllowance'
import { useTransactionStore } from '../stores/transactionStore'
import { useTransactionModal } from './useTransactionModal'
import { ERC20_ABI } from '../contracts/abis'
import {
  parseTransactionError,
  getErrorMessage,
} from '../utils/errors'

type FlowState = 'idle' | 'approving' | 'executing'

export interface TxContext {
  txId: string
  stepIndex: number
}

interface UseApprovalFlowOptions {
  tokenAddress: Address
  spenderAddress: Address
  actionTitle: string
  actionStepLabel: string
}

export function useApprovalFlow({
  tokenAddress,
  spenderAddress,
  actionTitle,
  actionStepLabel,
}: UseApprovalFlowOptions) {
  const { allowance, refetch: refetchAllowance } = useAllowance(tokenAddress, spenderAddress)
  const { writeContractAsync } = useWriteContract()
  const publicClient = usePublicClient()

  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const setStepInProgress = useTransactionStore((s) => s.setStepInProgress)
  const setStepSuccess = useTransactionStore((s) => s.setStepSuccess)
  const setStepError = useTransactionStore((s) => s.setStepError)
  const txModal = useTransactionModal()

  const [flowState, setFlowState] = useState<FlowState>('idle')
  const [approvePending, setApprovePending] = useState(false)
  const txIdRef = useRef<string | null>(null)

  const execute = useCallback(async (
    amount: bigint,
    action: (amount: bigint, txContext: TxContext) => Promise<unknown>,
  ) => {
    const needsApproval = allowance < amount

    const txId = crypto.randomUUID()
    txIdRef.current = txId

    const steps = needsApproval
      ? [
          { label: 'Approve spending', status: 'pending' as const },
          { label: actionStepLabel, status: 'pending' as const },
        ]
      : [{ label: actionStepLabel, status: 'pending' as const }]

    addTransaction({
      id: txId,
      type: 'leverage',
      status: 'pending',
      hash: undefined,
      title: actionTitle,
      steps,
    })
    txModal.open({ transactionId: txId })

    if (needsApproval) {
      setFlowState('approving')
      setApprovePending(true)
      setStepInProgress(txId, 0)

      const approveResult = await Result.tryPromise({
        try: async () => {
          const hash = await writeContractAsync({
            address: tokenAddress,
            abi: ERC20_ABI,
            functionName: 'approve',
            args: [spenderAddress, amount],
          })

          const receipt = await publicClient.waitForTransactionReceipt({ hash })
          if (receipt.status === 'reverted') {
            throw new Error('Approval reverted')
          }

          return hash
        },
        catch: (err) => parseTransactionError(err),
      })

      setApprovePending(false)

      if (Result.isError(approveResult)) {
        setStepError(txId, 0, getErrorMessage(approveResult.error))
        setFlowState('idle')
        return
      }

      void refetchAllowance()

      // Don't call setStepSuccess here - it marks ALL steps complete
      // setStepInProgress(1) will mark step 0 as completed and step 1 as in_progress
      setFlowState('executing')
      setStepInProgress(txId, 1)
      await action(amount, { txId, stepIndex: 1 })
    } else {
      setFlowState('executing')
      setStepInProgress(txId, 0)
      await action(amount, { txId, stepIndex: 0 })
    }

    setFlowState('idle')
    txIdRef.current = null
  }, [allowance, actionTitle, actionStepLabel, addTransaction, setStepInProgress, setStepSuccess, setStepError, txModal, writeContractAsync, publicClient, tokenAddress, spenderAddress, refetchAllowance])

  const reset = useCallback(() => {
    setFlowState('idle')
    setApprovePending(false)
    txIdRef.current = null
  }, [])

  const checkNeedsApproval = useCallback((amount: bigint) => amount > 0n && allowance < amount, [allowance])

  return {
    execute,
    reset,
    needsApproval: checkNeedsApproval,
    flowState,
    isApproving: flowState === 'approving',
    isExecuting: flowState === 'executing',
    isPending: flowState === 'approving' || flowState === 'executing',
    approvePending,
  }
}
