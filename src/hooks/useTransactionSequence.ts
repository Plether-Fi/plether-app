import { useState, useCallback, useRef } from 'react'
import { usePublicClient } from 'wagmi'
import { useTransactionModal } from './useTransactionModal'
import { parseTransactionError, getErrorMessage } from '../utils/errors'

class TransactionRevertError extends Error {
  data?: string
  constructor(message: string, data?: string) {
    super(message)
    this.name = 'TransactionRevertError'
    this.data = data
  }
}

export interface TransactionStep {
  label: string
  action: () => Promise<`0x${string}` | undefined>
}

export interface SequenceConfig {
  title: string
  buildSteps: () => TransactionStep[]
  onSuccess?: (hash: string) => void
}

type SequenceStatus = 'idle' | 'running' | 'success' | 'error'

interface SequenceState {
  status: SequenceStatus
  currentStepIndex: number
  error: string | null
  hash: string | null
}

const initialState: SequenceState = {
  status: 'idle',
  currentStepIndex: -1,
  error: null,
  hash: null,
}

export function useTransactionSequence() {
  const publicClient = usePublicClient()
  const txModal = useTransactionModal()
  const [state, setState] = useState<SequenceState>(initialState)
  const abortRef = useRef<{ aborted: boolean }>({ aborted: false })

  const reset = useCallback(() => {
    abortRef.current.aborted = true
    setState(initialState)
  }, [])

  const isAborted = () => abortRef.current.aborted

  const execute = useCallback(async (config: SequenceConfig) => {
    abortRef.current = { aborted: false }
    const { title, buildSteps, onSuccess } = config
    const steps = buildSteps()

    const modalSteps = steps.flatMap(s => [s.label, 'Confirming...'])
    txModal.open({
      title,
      steps: modalSteps,
      onRetry: () => {
        reset()
        void execute(config)
      },
    })

    setState({ status: 'running', currentStepIndex: 0, error: null, hash: null })

    let lastHash: string | null = null

    for (let i = 0; i < steps.length; i++) {
      if (isAborted()) return

      const step = steps[i]
      const modalStepBase = i * 2

      txModal.setStepInProgress(modalStepBase)
      setState(s => ({ ...s, currentStepIndex: i }))

      try {
        const hash = await step.action()

        if (isAborted()) return
        if (!hash) {
          throw new Error('Transaction was not submitted')
        }

        lastHash = hash

        txModal.setStepInProgress(modalStepBase + 1)

        const receipt = await publicClient.waitForTransactionReceipt({ hash })

        if (isAborted()) return

        if (receipt.status === 'reverted') {
          // Simulate the failed tx to get the revert reason
          const tx = await publicClient.getTransaction({ hash })
          if (tx.to) {
            await publicClient.call({
              to: tx.to,
              data: tx.input,
              account: tx.from,
              value: tx.value,
              blockNumber: receipt.blockNumber,
            })
          }
          throw new TransactionRevertError('Transaction reverted')
        }

      } catch (err) {
        if (isAborted()) return

        const error = parseTransactionError(err)
        const message = getErrorMessage(error)

        setState(s => ({ ...s, status: 'error', error: message }))
        txModal.setError(modalStepBase, message)
        return
      }
    }

    if (lastHash) {
      setState({ status: 'success', currentStepIndex: steps.length, error: null, hash: lastHash })
      txModal.setSuccess(lastHash)
      onSuccess?.(lastHash)
    }
  }, [publicClient, txModal, reset])

  return {
    execute,
    reset,
    status: state.status,
    currentStepIndex: state.currentStepIndex,
    isIdle: state.status === 'idle',
    isRunning: state.status === 'running',
    isSuccess: state.status === 'success',
    isError: state.status === 'error',
    error: state.error,
    hash: state.hash,
  }
}
