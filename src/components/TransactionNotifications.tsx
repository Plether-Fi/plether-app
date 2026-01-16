import { useEffect, useRef, useState, useCallback } from 'react'
import { useTransactionStore, type PendingTransaction } from '../stores/transactionStore'
import { ToastContainer, type ToastType } from './ui/Toast'

interface ToastItem {
  id: string
  type: ToastType
  title: string
  message?: string
  txHash?: string
}

const TX_TYPE_LABELS: Record<string, string> = {
  mint: 'Mint',
  burn: 'Redeem',
  swap: 'Swap',
  stake: 'Stake',
  unstake: 'Unstake',
  leverage: 'Leverage',
  lend: 'Lend',
  approve: 'Approval',
}

export function TransactionNotifications() {
  const [toasts, setToasts] = useState<ToastItem[]>([])
  const processedTxsRef = useRef<Set<string>>(new Set())
  const pendingTransactions = useTransactionStore((state) => state.pendingTransactions)

  const addToast = useCallback((toast: Omit<ToastItem, 'id'>) => {
    const id = `toast-${String(Date.now())}-${Math.random().toString(36).slice(2)}`
    setToasts((prev) => [...prev, { ...toast, id }])
  }, [])

  const removeToast = useCallback((id: string) => {
    setToasts((prev) => prev.filter((t) => t.id !== id))
  }, [])

  useEffect(() => {
    pendingTransactions.forEach((tx: PendingTransaction) => {
      const txKey = `${tx.id}-${tx.status}`

      if (processedTxsRef.current.has(txKey)) return

      if (tx.status === 'success') {
        processedTxsRef.current.add(txKey)
        const label = TX_TYPE_LABELS[tx.type] || tx.type
        addToast({
          type: 'success',
          title: `${label} Successful`,
          message: tx.description,
          txHash: tx.hash,
        })
      } else if (tx.status === 'failed') {
        processedTxsRef.current.add(txKey)
        const label = TX_TYPE_LABELS[tx.type] || tx.type
        addToast({
          type: 'error',
          title: `${label} Failed`,
          message: tx.errorMessage ?? tx.description,
        })
      }
    })
  }, [pendingTransactions, addToast])

  useEffect(() => {
    const interval = setInterval(() => {
      const currentIds = new Set(pendingTransactions.map((tx: PendingTransaction) => tx.id))
      processedTxsRef.current.forEach((key) => {
        const txId = key.split('-')[0]
        if (!currentIds.has(txId)) {
          processedTxsRef.current.delete(key)
        }
      })
    }, 60000)

    return () => { clearInterval(interval); }
  }, [pendingTransactions])

  return <ToastContainer toasts={toasts} onClose={removeToast} />
}
