import { useState, useCallback, useRef } from 'react'
import { usePublicClient } from 'wagmi'
import { useTransactionModal } from './useTransactionModal'
import { useTransactionStore, type TransactionType } from '../stores/transactionStore'
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
  type?: TransactionType
  buildSteps: () => TransactionStep[]
  onSuccess?: (hash: string) => void
  showModal?: boolean
}

type SequenceStatus = 'idle' | 'running' | 'success' | 'error'
type SequencePhase = 'idle' | 'awaiting_wallet' | 'confirming_onchain' | 'complete' | 'error'

interface SequenceState {
  status: SequenceStatus
  phase: SequencePhase
  currentStepIndex: number
  steps: string[]
  error: string | null
  hash: string | null
}

const initialState: SequenceState = {
  status: 'idle',
  phase: 'idle',
  currentStepIndex: -1,
  steps: [],
  error: null,
  hash: null,
}

export function useTransactionSequence() {
  const publicClient = usePublicClient()
  const txModal = useTransactionModal()
  const txStore = useTransactionStore()
  const [state, setState] = useState<SequenceState>(initialState)
  const abortRef = useRef<{ aborted: boolean }>({ aborted: false })

  const reset = useCallback(() => {
    abortRef.current.aborted = true
    setState(initialState)
  }, [])

  const isAborted = () => abortRef.current.aborted

  const execute = useCallback(async (config: SequenceConfig) => {
    abortRef.current = { aborted: false }
    const { title, type, buildSteps, onSuccess, showModal = true } = config
    const steps = buildSteps()
    const transactionId = crypto.randomUUID()

    const modalSteps = steps.flatMap(s => [s.label, 'Confirming onchain (~12s)'])

    // The shared transaction store powers the legacy sheet and its pending badge.
    // Embedded flows own their complete lifecycle, so they must not register there.
    if (showModal) {
      txStore.addTransaction({
        id: transactionId,
        type: type ?? 'mint',
        status: 'pending',
        title,
        steps: modalSteps.map(label => ({ label, status: 'pending' as const })),
      })
      txModal.open({
        transactionId,
        onRetry: () => {
          reset()
          void execute(config)
        },
      })
    }

    setState({
      status: 'running',
      phase: 'awaiting_wallet',
      currentStepIndex: 0,
      steps: steps.map(({ label }) => label),
      error: null,
      hash: null,
    })

    let lastHash: string | null = null

    for (let i = 0; i < steps.length; i++) {
      if (isAborted()) return

      const step = steps[i]
      const modalStepBase = i * 2

      if (showModal) txStore.setStepInProgress(transactionId, modalStepBase)
      setState(s => ({
        ...s,
        phase: 'awaiting_wallet',
        currentStepIndex: i,
      }))

      try {
        const hash = await step.action()

        if (isAborted()) return
        if (!hash) {
          throw new Error('Transaction was not submitted')
        }

        lastHash = hash

        if (showModal) txStore.setStepInProgress(transactionId, modalStepBase + 1)
        setState(s => ({ ...s, phase: 'confirming_onchain', hash }))

        const receipt = await publicClient.waitForTransactionReceipt({ hash })

        if (isAborted()) return

        if (receipt.status === 'reverted') {
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

        setState(s => ({ ...s, status: 'error', phase: 'error', error: message }))
        if (showModal) txStore.setStepError(transactionId, modalStepBase, message)
        return
      }
    }

    if (lastHash) {
      setState({
        status: 'success',
        phase: 'complete',
        currentStepIndex: steps.length,
        steps: steps.map(({ label }) => label),
        error: null,
        hash: lastHash,
      })
      if (showModal) txStore.setStepSuccess(transactionId, lastHash)
      onSuccess?.(lastHash)
    }
  }, [publicClient, txModal, txStore, reset])

  return {
    execute,
    reset,
    status: state.status,
    phase: state.phase,
    steps: state.steps,
    currentStepIndex: state.currentStepIndex,
    isIdle: state.status === 'idle',
    isRunning: state.status === 'running',
    isSuccess: state.status === 'success',
    isError: state.status === 'error',
    error: state.error,
    hash: state.hash,
  }
}
