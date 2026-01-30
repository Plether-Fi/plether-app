import { useTransactionStore } from '../stores/transactionStore'

export function PendingTxBadge() {
  const transactions = useTransactionStore((s) => s.transactions)
  const activeTxs = transactions.filter(
    tx => tx.status === 'pending' || tx.status === 'confirming'
  )
  const count = activeTxs.length

  if (count === 0) return null

  return (
    <div className="flex items-center gap-2 px-3 py-1.5 bg-cyber-bright-blue/20 border border-cyber-bright-blue/50 rounded-full shadow-sm shadow-cyber-bright-blue/20">
      <div className="w-4 h-4 relative">
        <div className="absolute inset-0 rounded-full border-2 border-cyber-bright-blue/30 border-t-cyber-bright-blue animate-spin" />
      </div>
      <span className="text-sm text-cyber-bright-blue font-medium">
        {count} pending
      </span>
    </div>
  )
}
