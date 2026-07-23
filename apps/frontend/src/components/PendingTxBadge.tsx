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
      type="button"
      aria-label={`${count.toString()} pending ${count === 1 ? 'transaction' : 'transactions'}`}
      onClick={handleClick}
      className="flex h-11 shrink-0 cursor-pointer items-center justify-center gap-1.5 border border-[#FFAB96] bg-[#FFAB96]/20 px-2 transition-colors hover:bg-[#FFAB96]/30 sm:w-auto sm:gap-2 sm:px-3"
    >
      <div className="w-4 h-4 relative">
        <div className="absolute inset-0 rounded-full border-2 border-[#FFAB96]/30 border-t-[#FFAB96] animate-spin" />
      </div>
      <span className="text-sm font-medium text-[#FFAB96] sm:hidden">{count}</span>
      <span className="hidden text-sm font-medium text-[#FFAB96] sm:inline">
        {count} pending
      </span>
    </button>
  )
}
