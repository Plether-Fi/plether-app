import { create } from 'zustand'
import { devtools } from 'zustand/middleware'
import { type LoadingStep } from '../components/ui/LoadingScreen'

interface TransactionModalState {
  isOpen: boolean
  title: string
  steps: LoadingStep[]
  errorMessage?: string
  transactionHash?: string
  onRetry?: () => void

  open: (config: {
    title: string
    steps: string[]
    onRetry?: () => void
  }) => void
  close: () => void
  setStepStatus: (index: number, status: LoadingStep['status']) => void
  setStepInProgress: (index: number) => void
  setError: (stepIndex: number, message: string) => void
  setSuccess: (hash: string) => void
  reset: () => void
}

export const useTransactionModal = create<TransactionModalState>()(devtools((set) => ({
  isOpen: false,
  title: '',
  steps: [],
  errorMessage: undefined,
  transactionHash: undefined,
  onRetry: undefined,

  open: ({ title, steps, onRetry }) =>
    set({
      isOpen: true,
      title,
      steps: steps.map((label) => ({ label, status: 'pending' as const })),
      errorMessage: undefined,
      transactionHash: undefined,
      onRetry,
    }),

  close: () =>
    set({
      isOpen: false,
      title: '',
      steps: [],
      errorMessage: undefined,
      transactionHash: undefined,
      onRetry: undefined,
    }),

  setStepStatus: (index, status) =>
    set((state) => ({
      steps: state.steps.map((step, i) =>
        i === index ? { ...step, status } : step
      ),
    })),

  setStepInProgress: (index) =>
    set((state) => ({
      steps: state.steps.map((step, i) => {
        if (i < index) return { ...step, status: 'completed' as const }
        if (i === index) return { ...step, status: 'in_progress' as const }
        return step
      }),
    })),

  setError: (stepIndex, message) =>
    set((state) => ({
      steps: state.steps.map((step, i) =>
        i === stepIndex ? { ...step, status: 'error' as const } : step
      ),
      errorMessage: message,
    })),

  setSuccess: (hash) =>
    set((state) => ({
      steps: state.steps.map((step) => ({ ...step, status: 'completed' as const })),
      transactionHash: hash,
    })),

  reset: () =>
    set({
      isOpen: false,
      title: '',
      steps: [],
      errorMessage: undefined,
      transactionHash: undefined,
      onRetry: undefined,
    }),
}), { name: 'TransactionModal' }))
