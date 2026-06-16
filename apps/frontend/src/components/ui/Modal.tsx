import { type ReactNode, useEffect } from 'react'
import { createPortal } from 'react-dom'

interface ModalProps {
  isOpen: boolean
  onClose: () => void
  title?: string
  headerContent?: ReactNode
  showCloseButton?: boolean
  children: ReactNode
  size?: 'sm' | 'md' | 'lg'
}

const sizeStyles = {
  sm: 'max-w-sm',
  md: 'max-w-md',
  lg: 'max-w-lg',
}

export function Modal({
  isOpen,
  onClose,
  title,
  headerContent,
  showCloseButton = true,
  children,
  size = 'md',
}: ModalProps) {
  useEffect(() => {
    if (isOpen) {
      document.body.style.overflow = 'hidden'
    } else {
      document.body.style.overflow = ''
    }
    return () => {
      document.body.style.overflow = ''
    }
  }, [isOpen])

  useEffect(() => {
    const handleEscape = (e: KeyboardEvent) => {
      if (e.key === 'Escape') onClose()
    }
    if (isOpen) {
      document.addEventListener('keydown', handleEscape)
    }
    return () => { document.removeEventListener('keydown', handleEscape); }
  }, [isOpen, onClose])

  if (!isOpen) return null

  return createPortal(
    <div className="fixed inset-0 z-50 flex items-center justify-center">
      {/* Backdrop */}
      <div
        className="absolute inset-0 cursor-pointer bg-cyber-bg"
        onClick={onClose}
      />

      {/* Modal Content */}
      <div
        className={`
          relative w-full ${sizeStyles[size]} mx-4
          bg-cyber-surface-dark  border border-cyber-border-glow/50
        `}
      >
        {(title || headerContent) && (
          <div className="relative border-b border-cyber-border-glow/30 px-6 py-4">
            {title ? (
              <div className="flex items-center justify-between">
                <h2 className="text-lg font-semibold text-cyber-text-primary">{title}</h2>
                {showCloseButton ? (
                  <button
                    onClick={onClose}
                    className="text-cyber-text-secondary hover:text-[#FFAB96] transition-colors"
                  >
                    <span className="material-symbols-outlined">close</span>
                  </button>
                ) : null}
              </div>
            ) : showCloseButton ? (
              <button
                onClick={onClose}
                className="absolute right-4 top-3 text-cyber-text-secondary hover:text-[#FFAB96] transition-colors"
              >
                <span className="material-symbols-outlined">close</span>
              </button>
            ) : null}
            {headerContent ? <div className={title ? 'mt-4' : showCloseButton ? 'pr-8' : ''}>{headerContent}</div> : null}
          </div>
        )}

        {/* Body */}
        <div className="p-6">{children}</div>
      </div>
    </div>,
    document.body
  )
}
