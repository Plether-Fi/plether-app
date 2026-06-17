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
      className="flex cursor-pointer items-center gap-2 border border-[#FFAB96] bg-[#FFAB96]/20 px-3 py-1.5 transition-colors hover:bg-[#FFAB96]/30"
    >
      <div className="w-4 h-4 relative">
        <div className="absolute inset-0 rounded-full border-2 border-[#FFAB96]/30 border-t-[#FFAB96] animate-spin" />
      </div>
      <span className="text-sm font-medium text-[#FFAB96]">
        {count} pending
      </span>
    </button>
  )
}
