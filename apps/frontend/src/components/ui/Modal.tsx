import { type ReactNode, useEffect } from 'react'
import { createPortal } from 'react-dom'

interface ModalProps {
  isOpen: boolean
  onClose: () => void
  title?: string
  headerContent?: ReactNode
  showCloseButton?: boolean
  closeOnBackdrop?: boolean
  closeOnEscape?: boolean
  children: ReactNode
  size?: 'sm' | 'md' | 'lg' | 'xl'
  placement?: 'center' | 'right'
  contentClassName?: string
  bodyClassName?: string
}

const sizeStyles = {
  sm: 'max-w-sm',
  md: 'max-w-md',
  lg: 'max-w-lg',
  xl: 'max-w-3xl',
}

const placementStyles = {
  center: 'items-center justify-center p-4',
  right: 'items-start justify-end p-4',
}

export function Modal({
  isOpen,
  onClose,
  title,
  headerContent,
  showCloseButton = true,
  closeOnBackdrop = true,
  closeOnEscape = true,
  children,
  size = 'md',
  placement = 'center',
  contentClassName = '',
  bodyClassName = 'p-6',
}: ModalProps) {
  const hasHeader = title !== undefined || headerContent !== undefined
  const placementClass = placementStyles[placement]

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
      if (e.key === 'Escape' && closeOnEscape) onClose()
    }
    if (isOpen) {
      document.addEventListener('keydown', handleEscape)
    }
    return () => { document.removeEventListener('keydown', handleEscape); }
  }, [closeOnEscape, isOpen, onClose])

  if (!isOpen) return null

  return createPortal(
    <div className={`fixed inset-0 z-50 flex ${placementClass}`}>
      {/* Backdrop */}
      <div
        className={`absolute inset-0 bg-cyber-bg ${closeOnBackdrop ? 'cursor-pointer' : ''}`}
        onClick={closeOnBackdrop ? onClose : undefined}
      />

      {/* Modal Content */}
      <div
        className={`
          relative flex max-h-[calc(100dvh-2rem)] w-full ${sizeStyles[size]} flex-col
          bg-cyber-surface-dark  border border-cyber-border-glow/50
          ${contentClassName}
        `}
        role="dialog"
        aria-modal="true"
      >
        {hasHeader ? (
          <div className="relative shrink-0 border-b border-cyber-border-glow/30 px-6 py-4">
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
        ) : null}

        {/* Body */}
        <div className={`min-h-0 overflow-y-auto overscroll-contain ${bodyClassName}`}>{children}</div>
      </div>
    </div>,
    document.body
  )
}
