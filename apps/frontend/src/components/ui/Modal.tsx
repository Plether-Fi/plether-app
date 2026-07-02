import { type ReactNode, useCallback, useEffect, useRef } from 'react'
import { createPortal } from 'react-dom'
import { trackPerpsModalClosed, trackPerpsModalOpened, type PerpsCloseReason } from '../../analytics/perps'
import type { AnalyticsProperties } from '../../analytics/client'

interface ModalProps {
  isOpen: boolean
  onClose: (reason?: PerpsCloseReason) => void
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
  analyticsId?: string
  analyticsSurface?: string
  analyticsProperties?: AnalyticsProperties
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
  analyticsId,
  analyticsSurface = 'perps',
  analyticsProperties,
}: ModalProps) {
  const hasHeader = title !== undefined || headerContent !== undefined
  const placementClass = placementStyles[placement]
  const openedAtRef = useRef<number | undefined>(undefined)
  const closeReasonRef = useRef<PerpsCloseReason>('state_change')
  const analyticsPropertiesRef = useRef<AnalyticsProperties | undefined>(analyticsProperties)

  useEffect(() => {
    analyticsPropertiesRef.current = analyticsProperties
  }, [analyticsProperties])

  useEffect(() => {
    if (!isOpen || !analyticsId) return undefined

    openedAtRef.current = performance.now()
    closeReasonRef.current = 'state_change'
    trackPerpsModalOpened(analyticsId, {
      surface: analyticsSurface,
      ...analyticsPropertiesRef.current,
    })

    return () => {
      const openedAt = openedAtRef.current
      openedAtRef.current = undefined
      if (openedAt === undefined) return

      trackPerpsModalClosed(analyticsId, {
        surface: analyticsSurface,
        close_reason: closeReasonRef.current,
        duration_ms: Math.max(0, Math.round(performance.now() - openedAt)),
        ...analyticsPropertiesRef.current,
      })
    }
  }, [analyticsId, analyticsSurface, isOpen])

  const handleClose = useCallback((reason: PerpsCloseReason) => {
    closeReasonRef.current = reason
    onClose(reason)
  }, [onClose])

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
      if (e.key === 'Escape' && closeOnEscape) handleClose('escape')
    }
    if (isOpen) {
      document.addEventListener('keydown', handleEscape)
    }
    return () => { document.removeEventListener('keydown', handleEscape); }
  }, [closeOnEscape, handleClose, isOpen])

  if (!isOpen) return null

  return createPortal(
    <div className={`fixed inset-0 z-50 flex ${placementClass}`}>
      {/* Backdrop */}
      <div
        className={`absolute inset-0 bg-app-bg/85 backdrop-blur-sm ${closeOnBackdrop ? 'cursor-pointer' : ''}`}
        onClick={closeOnBackdrop ? () => { handleClose('backdrop') } : undefined}
      />

      {/* Modal Content */}
      <div
        className={`
          relative flex max-h-[calc(100dvh-2rem)] w-full ${sizeStyles[size]} flex-col
          bg-surface-panel  border border-brand-border/50
          ${contentClassName}
        `}
        role="dialog"
        aria-modal="true"
      >
        {hasHeader ? (
          <div className="relative shrink-0 border-b border-brand-border/30 px-6 py-4">
            {title ? (
              <div className="flex items-center justify-between">
                <h2 className="text-lg font-semibold text-content-primary">{title}</h2>
                {showCloseButton ? (
                  <button
                    onClick={() => { handleClose('close_button') }}
                    className="text-content-secondary hover:text-[#FFAB96] transition-colors"
                  >
                    <span className="material-symbols-outlined">close</span>
                  </button>
                ) : null}
              </div>
            ) : showCloseButton ? (
              <button
                onClick={() => { handleClose('close_button') }}
                className="absolute right-4 top-3 text-content-secondary hover:text-[#FFAB96] transition-colors"
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
