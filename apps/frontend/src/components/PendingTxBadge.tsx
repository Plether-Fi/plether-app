import { useTransactionStore } from '../stores/transactionStore'
import { useTransactionModal } from '../hooks/useTransactionModal'

export function PendingTxBadge() {
  const transactions = useTransactionStore((s) => s.transactions)
  const { open } = useTransactionModal()

  const activeTxs = transactions.filter(
    tx => tx.status === 'pending' || tx.status === 'confirming'
  )
  const count = activeTxs.length

  if (count === 0) return null

  const handleClick = () => {
    const firstPendingTx = activeTxs[0] as { id: string }
    open({ transactionId: firstPendingTx.id })
  }

  return (
    <button
      onClick={handleClick}
      className="flex items-center gap-2 px-3 py-1.5 bg-cyber-bright-blue/20 border border-cyber-bright-blue/50 rounded-full shadow-sm shadow-cyber-bright-blue/20 hover:bg-cyber-bright-blue/30 transition-colors cursor-pointer"
    >
      <div className="w-4 h-4 relative">
        <div className="absolute inset-0 rounded-full border-2 border-cyber-bright-blue/30 border-t-cyber-bright-blue animate-spin" />
      </div>
      <span className="text-sm text-cyber-bright-blue font-medium">
        {count} pending
      </span>
    </button>
  )
}
