import { useCallback, useEffect, useState } from 'react'

/**
 * Tracks a container element's live CSS content-box width via ResizeObserver
 * so SVG charts render at 100% width with a 1:1 coordinate system and zero
 * horizontal letterboxing or aspect-ratio distortion.
 *
 * Uses a callback ref so conditionally rendered or mode-switched chart wrappers
 * always attach an observer immediately on mount.
 */
export function useContainerWidth(
  fallbackWidth: number,
): [(node: HTMLElement | null) => void, number] {
  const [element, setElement] = useState<HTMLElement | null>(null)
  const [width, setWidth] = useState(fallbackWidth)

  const refCallback = useCallback((node: HTMLElement | null) => {
    setElement(node)
  }, [])

  useEffect(() => {
    if (!element) return

    const update = () => {
      let innerWidth = element.clientWidth
      if (typeof window !== 'undefined' && typeof window.getComputedStyle === 'function') {
        const style = window.getComputedStyle(element)
        const padLeft = parseFloat(style.paddingLeft) || 0
        const padRight = parseFloat(style.paddingRight) || 0
        innerWidth -= padLeft + padRight
      }
      const next = Math.round(innerWidth)
      if (next > 0) {
        setWidth(next)
      }
    }

    update()
    if (typeof ResizeObserver === 'undefined') return
    const observer = new ResizeObserver(update)
    observer.observe(element)
    return () => {
      observer.disconnect()
    }
  }, [element])

  return [refCallback, width]
}
