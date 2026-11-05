import { create } from 'zustand'
import { devtools, persist } from 'zustand/middleware'
import { STORAGE_KEYS } from '../config/constants'
import { reportTransactionFailure } from '../analytics/transactionErrors'

export type TransactionStatus = 'pending' | 'confirming' | 'success' | 'failed'
export type TransactionType = 'mint' | 'burn' | 'swap' | 'stake' | 'unstake' | 'leverage' | 'lend' | 'approve' | 'supply' | 'withdraw' | 'borrow' | 'repay'

export interface TransactionStep {
  label: string
  status: 'pending' | 'in_progress' | 'confirming' | 'completed' | 'error'
}

export interface Transaction {
  id: string
  type: TransactionType
  status: TransactionStatus
  title: string
  steps: TransactionStep[]
  hash?: string
  errorMessage?: string
  supportReference?: string
  errorCode?: string
  timestamp: number
  chainId?: number
}

interface TransactionState {
  transactions: Transaction[]
  activeOperations: Record<string, string>

  addTransaction: (tx: Omit<Transaction, 'timestamp'>) => void
  updateTransaction: (id: string, update: Partial<Omit<Transaction, 'id' | 'timestamp'>>, cause?: unknown) => void
  removeTransaction: (id: string) => void
  clearTransactions: () => void
  cleanupOldTransactions: () => void

  setStepInProgress: (id: string, stepIndex: number) => void
  setStepConfirming: (id: string, stepIndex: number, hash: string) => void
  setStepError: (id: string, stepIndex: number, errorMessage: string, cause?: unknown) => void
  setStepSuccess: (id: string, hash: string) => void

  setActiveOperation: (key: string, txId: string) => void
  clearActiveOperation: (key: string) => void
  getActiveOperation: (key: string) => string | undefined
}

export const useTransactionStore = create<TransactionState>()(
  devtools(
    persist(
      (set, get) => ({
        transactions: [],
        activeOperations: {},

        addTransaction: (tx) =>
          set((state) => ({
            transactions: [
              ...state.transactions,
              { ...tx, timestamp: Date.now() },
            ],
          })),

        updateTransaction: (id, update, cause) => {
          const previous = get().transactions.find(tx => tx.id === id)
          if (previous && (update.status === 'failed' || (previous.status === 'failed' && update.errorMessage !== undefined))) {
            const failure = reportTransactionFailure(cause ?? update.errorMessage, update.errorMessage, {
              surface: 'wallet', action: previous.type, stage: 'transaction', attemptId: previous.supportReference ?? id,
            })
            const steps = update.steps ?? previous.steps
            const activeIndex = steps.findIndex(step => step.status === 'in_progress' || step.status === 'confirming')
            const pendingIndex = steps.findIndex(step => step.status === 'pending')
            const errorIndex = activeIndex >= 0 ? activeIndex : pendingIndex >= 0 ? pendingIndex : steps.length - 1
            update = { ...update, errorMessage: failure.message, supportReference: failure.supportReference, errorCode: failure.errorCode,
              steps: steps.some(step => step.status === 'error') ? steps
                : steps.map((step, index) => index === errorIndex ? { ...step, status: 'error' } : step),
            }
          }
          set((state) => ({
            transactions: state.transactions.map((tx) =>
              tx.id === id ? { ...tx, ...update } : tx
            ),
          }))
        },

        removeTransaction: (id) =>
          set((state) => ({
            transactions: state.transactions.filter((tx) => tx.id !== id),
          })),

        clearTransactions: () => set({ transactions: [] }),

        cleanupOldTransactions: () =>
          set((state) => {
            const oneHourAgo = Date.now() - 60 * 60 * 1000
            const twentyFourHoursAgo = Date.now() - 24 * 60 * 60 * 1000
            return {
              transactions: state.transactions.filter((tx) => {
                if (tx.status === 'pending' || tx.status === 'confirming') {
                  return tx.timestamp > oneHourAgo
                }
                return tx.timestamp > twentyFourHoursAgo
              }),
            }
          }),

        setStepInProgress: (id, stepIndex) => {
          const tx = get().transactions.find((t) => t.id === id)
          if (!tx) return

          const newSteps = tx.steps.map((step, i) => {
            if (i < stepIndex) return { ...step, status: 'completed' as const }
            if (i === stepIndex) {
              if (step.status === 'completed') return step
              return { ...step, status: 'in_progress' as const }
            }
            return step
          })

          set((state) => ({
            transactions: state.transactions.map((t) =>
              t.id === id ? { ...t, steps: newSteps, status: 'confirming' } : t
            ),
          }))
        },

        setStepConfirming: (id, stepIndex, hash) => {
          const tx = get().transactions.find((t) => t.id === id)
          if (!tx) return

          const newSteps = tx.steps.map((step, i) => {
            if (i < stepIndex) return { ...step, status: 'completed' as const }
            if (i === stepIndex) return { ...step, status: 'confirming' as const }
            return step
          })

          set((state) => ({
            transactions: state.transactions.map((t) =>
              t.id === id ? { ...t, steps: newSteps, status: 'confirming', hash } : t
            ),
          }))
        },

        setStepError: (id, stepIndex, errorMessage, cause) => {
          const tx = get().transactions.find((t) => t.id === id)
          if (!tx) return

          const inProgressIndex = tx.steps.findIndex((s) => s.status === 'in_progress')
          const errorIndex = inProgressIndex >= 0 ? inProgressIndex : Math.min(stepIndex, tx.steps.length - 1)

          const newSteps = tx.steps.map((step, i) => {
            if (i < errorIndex) return { ...step, status: 'completed' as const }
            if (i === errorIndex) return { ...step, status: 'error' as const }
            return step
          })

          get().updateTransaction(id, { steps: newSteps, status: 'failed', errorMessage }, cause)
        },

        setStepSuccess: (id, hash) => {
          const tx = get().transactions.find((t) => t.id === id)
          if (!tx) return

          const newSteps = tx.steps.map((step) => ({
            ...step,
            status: 'completed' as const,
          }))

          set((state) => ({
            transactions: state.transactions.map((t) =>
              t.id === id ? { ...t, steps: newSteps, status: 'success', hash } : t
            ),
          }))
        },

        setActiveOperation: (key, txId) =>
          set((state) => ({
            activeOperations: { ...state.activeOperations, [key]: txId },
          })),

        clearActiveOperation: (key) =>
          set((state) => {
            const { [key]: _removed, ...rest } = state.activeOperations
            void _removed
            return { activeOperations: rest }
          }),

        getActiveOperation: (key) => get().activeOperations[key],
      }),
      {
        name: STORAGE_KEYS.PENDING_TXS,
      }
    ),
    { name: 'TransactionStore' }
  )
)

// Legacy alias for backward compatibility during migration
export type PendingTransaction = Transaction
export const usePendingTransactions = () => useTransactionStore((s) => s.transactions)

// Expose for debugging in browser console
if (typeof window !== 'undefined') {
  (window as unknown as { __txStore: typeof useTransactionStore }).__txStore = useTransactionStore
}
