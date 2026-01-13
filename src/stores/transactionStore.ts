import { create } from 'zustand'
import { devtools, persist } from 'zustand/middleware'
import { STORAGE_KEYS } from '../config/constants'

export type TransactionStatus = 'pending' | 'confirming' | 'success' | 'failed'
export type TransactionType = 'mint' | 'burn' | 'swap' | 'stake' | 'unstake' | 'leverage' | 'lend' | 'approve'

export interface PendingTransaction {
  id: string
  hash?: string
  type: TransactionType
  status: TransactionStatus
  description: string
  timestamp: number
  chainId?: number
  errorMessage?: string
}

interface TransactionState {
  pendingTransactions: PendingTransaction[]
  addTransaction: (tx: Omit<PendingTransaction, 'timestamp'>) => void
  updateTransaction: (id: string, update: Partial<PendingTransaction>) => void
  removeTransaction: (id: string) => void
  clearTransactions: () => void
  cleanupOldTransactions: () => void
}

export const useTransactionStore = create<TransactionState>()(
  devtools(
    persist(
      (set) => ({
        pendingTransactions: [],

        addTransaction: (tx) =>
          set((state) => ({
            pendingTransactions: [
              ...state.pendingTransactions,
              { ...tx, timestamp: Date.now() },
            ],
          })),

        updateTransaction: (id, update) =>
          set((state) => ({
            pendingTransactions: state.pendingTransactions.map((tx) =>
              tx.id === id ? { ...tx, ...update } : tx
            ),
          })),

        removeTransaction: (id) =>
          set((state) => ({
            pendingTransactions: state.pendingTransactions.filter(
              (tx) => tx.id !== id
            ),
          })),

        clearTransactions: () => set({ pendingTransactions: [] }),

        cleanupOldTransactions: () =>
          set((state) => {
            const oneHourAgo = Date.now() - 60 * 60 * 1000
            return {
              pendingTransactions: state.pendingTransactions.filter(
                (tx) =>
                  (tx.status === 'pending' || tx.status === 'confirming') &&
                  tx.timestamp > oneHourAgo
              ),
            }
          }),
      }),
      {
        name: STORAGE_KEYS.PENDING_TXS,
      }
    ),
    { name: 'TransactionStore' }
  )
)
