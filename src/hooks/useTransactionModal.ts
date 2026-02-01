import { create } from 'zustand'
import { devtools } from 'zustand/middleware'
import { useTransactionStore, type Transaction } from '../stores/transactionStore'

interface TransactionModalState {
  isOpen: boolean
  currentIndex: number
  retryCallbacks: Record<string, () => void>

  open: (config: { transactionId: string; onRetry?: () => void }) => void
  close: () => void
  reset: () => void
  navigatePrev: () => void
  navigateNext: () => void
  viewTransaction: (index: number) => void
  getRetryCallback: (transactionId: string) => (() => void) | undefined
}

function isTransactionInProgress(tx: Transaction | null): boolean {
  if (!tx) return false
  return tx.status === 'pending' || tx.status === 'confirming'
}

const store = create<TransactionModalState>()(devtools((set, get) => ({
  isOpen: false,
  currentIndex: -1,
  retryCallbacks: {},

  open: ({ transactionId, onRetry }) => {
    const txStore = useTransactionStore.getState()
    const txIndex = txStore.transactions.findIndex(t => t.id === transactionId)

    set(state => ({
      isOpen: true,
      currentIndex: txIndex >= 0 ? txIndex : txStore.transactions.length - 1,
      retryCallbacks: onRetry
        ? { ...state.retryCallbacks, [transactionId]: onRetry }
        : state.retryCallbacks,
    }))
  },

  close: () => { set({ isOpen: false }) },

  reset: () => {
    set({
      isOpen: false,
      currentIndex: -1,
      retryCallbacks: {},
    })
  },

  navigatePrev: () => {
    const { currentIndex } = get()
    if (currentIndex > 0) {
      set({ currentIndex: currentIndex - 1 })
    }
  },

  navigateNext: () => {
    const { currentIndex } = get()
    const txStore = useTransactionStore.getState()

    if (currentIndex < txStore.transactions.length - 1) {
      set({ currentIndex: currentIndex + 1 })
    }
  },

  viewTransaction: (index) => {
    const txStore = useTransactionStore.getState()

    if (index >= 0 && index < txStore.transactions.length) {
      set({ currentIndex: index })
    }
  },

  getRetryCallback: (transactionId) => get().retryCallbacks[transactionId],
}), { name: 'TransactionModal' }))

export const useTransactionModal = store

export function useCurrentTransaction() {
  const { currentIndex } = useTransactionModal()
  const transactions = useTransactionStore((s) => s.transactions)

  if (currentIndex >= 0 && currentIndex < transactions.length) {
    return transactions[currentIndex]
  }
  return null
}

export function useIsCurrentTransactionInProgress() {
  const tx = useCurrentTransaction()
  return isTransactionInProgress(tx)
}

export { isTransactionInProgress }

if (typeof window !== 'undefined') {
  (window as unknown as { __txModal: typeof store }).__txModal = store
}
