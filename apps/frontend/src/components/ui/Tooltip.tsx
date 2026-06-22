import { type CSSProperties, type ReactNode, useCallback, useEffect, useId, useLayoutEffect, useRef, useState } from 'react'
import { createPortal } from 'react-dom'

interface TooltipProps {
  content: ReactNode
  children: ReactNode
  position?: 'top' | 'bottom' | 'bottom-end' | 'left' | 'right'
  className?: string
}

interface TooltipCoordinates {
  left: number
  top: number
}

const TOOLTIP_GAP_PX = 8
const VIEWPORT_MARGIN_PX = 8

export function Tooltip({ content, children, position = 'top', className = '' }: TooltipProps) {
  const whitespaceClass = className.includes('whitespace-') ? '' : 'whitespace-nowrap'
  const tooltipId = useId()
  const triggerRef = useRef<HTMLDivElement>(null)
  const tooltipRef = useRef<HTMLDivElement>(null)
  const [isVisible, setIsVisible] = useState(false)
  const [coordinates, setCoordinates] = useState<TooltipCoordinates>({ left: VIEWPORT_MARGIN_PX, top: VIEWPORT_MARGIN_PX })

  const updatePosition = useCallback(() => {
    const triggerElement = triggerRef.current
    const tooltipElement = tooltipRef.current
    if (!triggerElement || !tooltipElement) return

    const triggerRect = triggerElement.getBoundingClientRect()
    const tooltipRect = tooltipElement.getBoundingClientRect()
    const viewportWidth = window.innerWidth
    const viewportHeight = window.innerHeight

    let left = triggerRect.left + (triggerRect.width - tooltipRect.width) / 2
    let top = triggerRect.top - tooltipRect.height - TOOLTIP_GAP_PX

    if (position === 'bottom') {
      top = triggerRect.bottom + TOOLTIP_GAP_PX
    } else if (position === 'bottom-end') {
      left = triggerRect.right - tooltipRect.width
      top = triggerRect.bottom + TOOLTIP_GAP_PX
    } else if (position === 'left') {
      left = triggerRect.left - tooltipRect.width - TOOLTIP_GAP_PX
      top = triggerRect.top + (triggerRect.height - tooltipRect.height) / 2
    } else if (position === 'right') {
      left = triggerRect.right + TOOLTIP_GAP_PX
      top = triggerRect.top + (triggerRect.height - tooltipRect.height) / 2
    }

    if (top < VIEWPORT_MARGIN_PX && position === 'top') {
      top = triggerRect.bottom + TOOLTIP_GAP_PX
    } else if (top + tooltipRect.height > viewportHeight - VIEWPORT_MARGIN_PX && (position === 'bottom' || position === 'bottom-end')) {
      top = triggerRect.top - tooltipRect.height - TOOLTIP_GAP_PX
    }

    if (left < VIEWPORT_MARGIN_PX && position === 'left') {
      left = triggerRect.right + TOOLTIP_GAP_PX
    } else if (left + tooltipRect.width > viewportWidth - VIEWPORT_MARGIN_PX && position === 'right') {
      left = triggerRect.left - tooltipRect.width - TOOLTIP_GAP_PX
    }

    const maxLeft = Math.max(VIEWPORT_MARGIN_PX, viewportWidth - tooltipRect.width - VIEWPORT_MARGIN_PX)
    const maxTop = Math.max(VIEWPORT_MARGIN_PX, viewportHeight - tooltipRect.height - VIEWPORT_MARGIN_PX)

    setCoordinates({
      left: Math.min(Math.max(left, VIEWPORT_MARGIN_PX), maxLeft),
      top: Math.min(Math.max(top, VIEWPORT_MARGIN_PX), maxTop),
    })
  }, [position])

  useLayoutEffect(() => {
    if (!isVisible) return
    updatePosition()
  }, [isVisible, updatePosition, content, className])

  useEffect(() => {
    if (!isVisible) return undefined

    const handleUpdate = () => {
      updatePosition()
    }

    window.addEventListener('resize', handleUpdate)
    window.addEventListener('scroll', handleUpdate, true)

    return () => {
      window.removeEventListener('resize', handleUpdate)
      window.removeEventListener('scroll', handleUpdate, true)
    }
  }, [isVisible, updatePosition])

  const tooltipStyle: CSSProperties = {
    left: coordinates.left,
    maxHeight: `calc(100vh - ${(VIEWPORT_MARGIN_PX * 2).toString()}px)`,
    maxWidth: `calc(100vw - ${(VIEWPORT_MARGIN_PX * 2).toString()}px)`,
    top: coordinates.top,
  }

  return (
    <div
      ref={triggerRef}
      className="relative inline-flex"
      aria-describedby={isVisible ? tooltipId : undefined}
      onBlur={() => {
        setIsVisible(false)
      }}
      onFocus={() => {
        setIsVisible(true)
      }}
      onMouseEnter={() => {
        setIsVisible(true)
      }}
      onMouseLeave={() => {
        setIsVisible(false)
      }}
    >
      {children}
      {isVisible
        ? createPortal(
          <div
            ref={tooltipRef}
            id={tooltipId}
            role="tooltip"
            style={tooltipStyle}
            className={`
              pointer-events-none fixed z-[1000] box-border min-w-0 overflow-y-auto break-words border border-cyber-border-glow/50 ${whitespaceClass}
              bg-cyber-bg px-3 py-2 text-sm normal-case text-cyber-text-primary
              ${className}
            `}
          >
            {content}
          </div>,
          document.body
        )
        : null}
    </div>
  )
}
