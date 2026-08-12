import {
  type CSSProperties,
  type FocusEvent,
  type ReactNode,
  useCallback,
  useEffect,
  useId,
  useLayoutEffect,
  useRef,
  useState,
} from 'react'
import { createPortal } from 'react-dom'
import { DocsLink } from './DocsLink'

export interface TooltipDocsLink {
  href: string
  title: string
}

interface TooltipProps {
  content: ReactNode
  children: ReactNode
  position?: 'top' | 'bottom' | 'bottom-end' | 'left' | 'right'
  className?: string
  docsLink?: TooltipDocsLink
}

interface TooltipCoordinates {
  left: number
  top: number
}

const TOOLTIP_GAP_PX = 8
const TOOLTIP_HIDE_DELAY_MS = 300
const VIEWPORT_MARGIN_PX = 8

export function Tooltip({
  content,
  children,
  position = 'top',
  className = '',
  docsLink,
}: TooltipProps) {
  const whitespaceClass = className.includes('whitespace-') ? '' : 'whitespace-normal'
  const tooltipId = useId()
  const triggerRef = useRef<HTMLDivElement>(null)
  const tooltipRef = useRef<HTMLDivElement>(null)
  const hideTimerRef = useRef<number | undefined>(undefined)
  const [isVisible, setIsVisible] = useState(false)
  const [coordinates, setCoordinates] = useState<TooltipCoordinates>({ left: VIEWPORT_MARGIN_PX, top: VIEWPORT_MARGIN_PX })

  const clearHideTimer = useCallback(() => {
    if (hideTimerRef.current === undefined) return
    window.clearTimeout(hideTimerRef.current)
    hideTimerRef.current = undefined
  }, [])

  const showTooltip = useCallback(() => {
    clearHideTimer()
    setIsVisible(true)
  }, [clearHideTimer])

  const scheduleHideTooltip = useCallback(() => {
    clearHideTimer()
    hideTimerRef.current = window.setTimeout(() => {
      hideTimerRef.current = undefined
      setIsVisible(false)
    }, TOOLTIP_HIDE_DELAY_MS)
  }, [clearHideTimer])

  const handleBlur = useCallback((event: FocusEvent<HTMLElement>) => {
    const nextTarget = event.relatedTarget
    if (
      nextTarget instanceof Node
      && (triggerRef.current?.contains(nextTarget) || tooltipRef.current?.contains(nextTarget))
    ) {
      return
    }
    scheduleHideTooltip()
  }, [scheduleHideTooltip])

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

  useEffect(() => clearHideTimer, [clearHideTimer])

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
      onBlur={handleBlur}
      onFocus={showTooltip}
      onMouseEnter={showTooltip}
      onMouseLeave={scheduleHideTooltip}
    >
      {children}
      {isVisible
        ? createPortal(
          <div
            ref={tooltipRef}
            id={tooltipId}
            role="tooltip"
            style={tooltipStyle}
            onBlur={handleBlur}
            onFocus={showTooltip}
            onMouseEnter={showTooltip}
            onMouseLeave={scheduleHideTooltip}
            className={`
              pointer-events-auto fixed z-[1000] box-border min-w-0 overflow-y-auto break-words border border-brand-border/50 ${whitespaceClass}
              bg-app-bg px-3 py-2 text-sm normal-case text-content-primary
              ${className}
            `}
          >
            <div>{content}</div>
            {docsLink ? (
              <DocsLink
                href={docsLink.href}
                title={docsLink.title}
                className="mt-2 inline-block"
              >
                <span>Read: </span>
                <span className="italic">{docsLink.title}</span>
              </DocsLink>
            ) : null}
          </div>,
          document.body
        )
        : null}
    </div>
  )
}
