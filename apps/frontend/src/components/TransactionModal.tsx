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
    <div className="flex items-center justify-center gap-3 py-3 border-b border-brand-border/30">
      <button
        type="button"
        aria-label="Previous transaction"
        onClick={onPrev}
        disabled={currentIndex === 0}
        className="inline-flex h-11 w-11 items-center justify-center text-content-secondary transition-colors hover:text-[#FFAB96] disabled:cursor-not-allowed disabled:opacity-30"
      >
        <span className="material-symbols-outlined text-lg">chevron_left</span>
      </button>
      <span className="text-content-secondary text-sm">
        Transaction {currentIndex + 1}/{total}
      </span>
      <button
        type="button"
        aria-label="Next transaction"
        onClick={onNext}
        disabled={currentIndex === total - 1}
        className="inline-flex h-11 w-11 items-center justify-center text-content-secondary transition-colors hover:text-[#FFAB96] disabled:cursor-not-allowed disabled:opacity-30"
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

  useEffect(() => {
    if (!isOpen) return undefined

    const previousOverflow = document.body.style.overflow
    document.body.style.overflow = 'hidden'

    return () => {
      document.body.style.overflow = previousOverflow
    }
  }, [isOpen])

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
      {/* Backdrop */}
      <div
        className="fixed inset-0 z-[60] cursor-pointer bg-app-bg/65 backdrop-blur-md"
        onClick={close}
      />

      {/* Viewport-safe transaction sheet */}
      <div className="pointer-events-none fixed inset-0 z-[70] flex items-end justify-center sm:items-start sm:justify-end sm:p-4">
        <div className="flex max-h-dvh w-full max-w-md flex-col sm:max-h-[calc(100dvh-2rem)]">
          <div
            className="pointer-events-auto min-h-0 overflow-y-auto overscroll-contain border border-brand-border/50 bg-surface-panel/95 shadow-2xl shadow-app-bg/50 backdrop-blur-xl"
            role="dialog"
            aria-modal="true"
            aria-label={currentTx.title}
          >
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
