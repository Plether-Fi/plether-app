import { useEffect } from 'react'
import { createPortal } from 'react-dom'
import { useAccount } from 'wagmi'
import { LoadingScreen } from './ui/LoadingScreen'
import { useTransactionModal, useCurrentTransaction } from '../hooks/useTransactionModal'
import { useTransactionStore } from '../stores/transactionStore'
import { getExplorerTxUrl } from '../utils/explorer'

function TransactionNavigator({
  currentIndex,
  total,
  onPrev,
  onNext,
}: {
  currentIndex: number
  total: number
  onPrev: () => void
  onNext: () => void
}) {
  if (total <= 1) return null

  return (
    <div className="flex items-center justify-center gap-3 py-3 border-b border-cyber-border-glow/30">
      <button
        onClick={onPrev}
        disabled={currentIndex === 0}
        className="p-1 text-cyber-text-secondary hover:text-cyber-bright-blue disabled:opacity-30 disabled:cursor-not-allowed transition-colors"
      >
        <span className="material-symbols-outlined text-lg">chevron_left</span>
      </button>
      <span className="text-cyber-text-secondary text-sm">
        Transaction {currentIndex + 1}/{total}
      </span>
      <button
        onClick={onNext}
        disabled={currentIndex === total - 1}
        className="p-1 text-cyber-text-secondary hover:text-cyber-bright-blue disabled:opacity-30 disabled:cursor-not-allowed transition-colors"
      >
        <span className="material-symbols-outlined text-lg">chevron_right</span>
      </button>
    </div>
  )
}

export function TransactionModal() {
  const { chainId } = useAccount()
  const {
    isOpen,
    currentIndex,
    getRetryCallback,
    close,
    reset,
    navigatePrev,
    navigateNext,
  } = useTransactionModal()

  const transactions = useTransactionStore((s) => s.transactions)
  const currentTx = useCurrentTransaction()

  useEffect(() => {
    const handleEscape = (e: KeyboardEvent) => {
      if (e.key === 'Escape') close()
    }
    if (isOpen) {
      document.addEventListener('keydown', handleEscape)
    }
    return () => { document.removeEventListener('keydown', handleEscape); }
  }, [isOpen, close])

  const retryCallback = currentTx ? getRetryCallback(currentTx.id) : undefined
  const canRetry = currentTx?.status === 'failed' && retryCallback

  const handleRetry = () => {
    reset()
    retryCallback?.()
  }

  const transactionUrl = currentTx?.hash
    ? getExplorerTxUrl(chainId, currentTx.hash)
    : undefined

  if (!isOpen || !currentTx) return null

  return createPortal(
    <>
      {/* Backdrop - only covers content below header */}
      <div
        className="fixed inset-0 top-[78px] z-40 bg-cyber-bg/60 backdrop-blur-sm"
        onClick={close}
      />

      {/* Modal - fixed to right side below header, aligned with header content */}
      <div className="fixed top-[78px] left-0 right-0 z-50 pointer-events-none animate-in fade-in slide-in-from-top-2 duration-200">
        <div className="max-w-7xl mx-auto px-6 lg:px-8 flex justify-end">
          <div className="pointer-events-auto w-full max-w-md bg-cyber-surface-dark border border-cyber-border-glow/50 shadow-2xl shadow-cyber-border-glow/20">
            <TransactionNavigator
              currentIndex={currentIndex}
              total={transactions.length}
              onPrev={navigatePrev}
              onNext={navigateNext}
            />
            <LoadingScreen
              title={currentTx.title}
              steps={currentTx.steps}
              errorMessage={currentTx.errorMessage}
              transactionUrl={transactionUrl}
              onClose={close}
              onRetry={canRetry ? handleRetry : undefined}
            />
          </div>
        </div>
      </div>
    </>,
    document.body
  )
}
