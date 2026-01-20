import { useEffect } from 'react'
import { createPortal } from 'react-dom'
import { useAccount } from 'wagmi'
import { LoadingScreen } from './ui/LoadingScreen'
import { useTransactionModal } from '../hooks/useTransactionModal'
import { getExplorerTxUrl } from '../utils/explorer'

export function TransactionModal() {
  const { chainId } = useAccount()
  const {
    isOpen,
    title,
    steps,
    errorMessage,
    transactionHash,
    onRetry,
    close,
    reset,
  } = useTransactionModal()

  useEffect(() => {
    const handleEscape = (e: KeyboardEvent) => {
      if (e.key === 'Escape') close()
    }
    if (isOpen) {
      document.addEventListener('keydown', handleEscape)
    }
    return () => { document.removeEventListener('keydown', handleEscape); }
  }, [isOpen, close])

  const handleRetry = () => {
    reset()
    onRetry?.()
  }

  const transactionUrl = transactionHash
    ? getExplorerTxUrl(chainId, transactionHash)
    : undefined

  if (!isOpen) return null

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
            <LoadingScreen
              title={title}
              steps={steps}
              errorMessage={errorMessage}
              transactionUrl={transactionUrl}
              onClose={close}
              onRetry={onRetry ? handleRetry : undefined}
            />
          </div>
        </div>
      </div>
    </>,
    document.body
  )
}
