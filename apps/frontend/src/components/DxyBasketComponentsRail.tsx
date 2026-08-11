import { useCallback, useEffect, useRef, useState } from 'react'
import type { BasketComponentPrice } from '../api'
import { DocsLink, Skeleton, Tooltip, type TooltipDocsLink } from './ui'

const COMPONENT_PRICE_FRESH_SECONDS = 10 * 60
const COMPONENT_CARD_CLASS_NAME =
  'min-h-[64px] min-w-[10.5rem] flex-1 basis-0 snap-start border border-brand-border/20 bg-app-bg px-3 py-2'
const COMPONENT_RAIL_CLASS_NAME =
  'basket-components-rail flex max-w-full touch-pan-x snap-x snap-mandatory gap-2 overflow-x-auto overscroll-x-contain focus-visible:outline focus-visible:outline-1 focus-visible:outline-offset-2 focus-visible:outline-content-secondary'

function toOraclePrice(raw: string): number {
  return Number(raw) / 1e8
}

function formatCompactPrice(value: number): string {
  return value.toLocaleString('en-US', {
    minimumFractionDigits: 3,
    maximumFractionDigits: 3,
  })
}

function formatPercent(value: number | null | undefined): string {
  if (value == null) return '--'
  const sign = value > 0 ? '+' : ''
  return `${sign}${(value * 100).toFixed(2)}%`
}

function formatUpdateAge(ageSeconds: number): string {
  if (!Number.isFinite(ageSeconds) || ageSeconds < 0) return 'unknown age'
  if (ageSeconds < 60) return `${ageSeconds.toString()}s ago`

  const minutes = Math.floor(ageSeconds / 60)
  const seconds = ageSeconds % 60
  if (minutes < 60) return seconds > 0 ? `${minutes.toString()}m ${seconds.toString()}s ago` : `${minutes.toString()}m ago`

  const hours = Math.floor(minutes / 60)
  const remainingMinutes = minutes % 60
  if (hours < 24) return remainingMinutes > 0 ? `${hours.toString()}h ${remainingMinutes.toString()}m ago` : `${hours.toString()}h ago`

  const days = Math.floor(hours / 24)
  const remainingHours = hours % 24
  return remainingHours > 0 ? `${days.toString()}d ${remainingHours.toString()}h ago` : `${days.toString()}d ago`
}

function freshnessTooltip(publishTime: number | undefined, nowSeconds: number): string | undefined {
  if (!publishTime) return undefined
  return `updated ${formatUpdateAge(Math.max(0, nowSeconds - publishTime))}`
}

function componentPrice(component: BasketComponentPrice): string {
  return formatCompactPrice(toOraclePrice(component.price))
}

function componentWeight(component: BasketComponentPrice): string {
  return `${(component.weightBps / 100).toFixed(1)}%`
}

function componentChangeClass(value: number): string {
  if (Math.abs(value) < 0.00005) return 'text-content-secondary/70'
  return value > 0 ? 'text-positive/75' : 'text-brand-orange/75'
}

function componentKey(component: BasketComponentPrice): string {
  return component.feedId || component.symbol
}

function ComponentFreshnessDot({ publishTime, nowSeconds }: { publishTime?: number; nowSeconds: number }) {
  const tooltip = freshnessTooltip(publishTime, nowSeconds)
  if (!tooltip) return null

  const ageSeconds = Math.max(0, nowSeconds - (publishTime ?? nowSeconds))
  const isFresh = ageSeconds <= COMPONENT_PRICE_FRESH_SECONDS

  return (
    <Tooltip content={tooltip} position="top">
      <span
        className={`h-2 w-2 shrink-0 rounded-full ${isFresh ? 'bg-positive' : 'bg-brand-orange'}`}
        aria-label={isFresh ? 'Price fresh' : 'Price stale'}
        tabIndex={0}
      />
    </Tooltip>
  )
}

export interface DxyBasketComponentsRailProps {
  components?: readonly BasketComponentPrice[]
  priceChanges?: Partial<Record<string, number>>
  isLoading?: boolean
  isError?: boolean
  nowSeconds?: number
  docsLink?: TooltipDocsLink
}

export function DxyBasketComponentsRail({
  components = [],
  priceChanges = {},
  isLoading = false,
  isError = false,
  nowSeconds: controlledNowSeconds,
  docsLink,
}: DxyBasketComponentsRailProps) {
  const railRef = useRef<HTMLDivElement>(null)
  const [liveNowSeconds, setLiveNowSeconds] = useState(() => Math.floor(Date.now() / 1000))
  const [canScrollLeft, setCanScrollLeft] = useState(false)
  const [canScrollRight, setCanScrollRight] = useState(false)

  useEffect(() => {
    if (controlledNowSeconds !== undefined) return undefined

    const interval = window.setInterval(() => {
      setLiveNowSeconds(Math.floor(Date.now() / 1000))
    }, 5_000)

    return () => {
      window.clearInterval(interval)
    }
  }, [controlledNowSeconds])

  const nowSeconds = controlledNowSeconds ?? liveNowSeconds
  const hasComponents = components.length > 0

  const updateScrollState = useCallback(() => {
    const rail = railRef.current
    if (!rail) return

    const maxScrollLeft = Math.max(0, rail.scrollWidth - rail.clientWidth)
    const nextCanScrollLeft = rail.scrollLeft > 1
    const nextCanScrollRight = rail.scrollLeft < maxScrollLeft - 1
    setCanScrollLeft((current) => current === nextCanScrollLeft ? current : nextCanScrollLeft)
    setCanScrollRight((current) => current === nextCanScrollRight ? current : nextCanScrollRight)
  }, [])

  useEffect(() => {
    if (!hasComponents) return undefined

    updateScrollState()
    window.addEventListener('resize', updateScrollState)
    const resizeObserver = typeof ResizeObserver === 'undefined'
      ? undefined
      : new ResizeObserver(updateScrollState)
    if (railRef.current) resizeObserver?.observe(railRef.current)

    return () => {
      window.removeEventListener('resize', updateScrollState)
      resizeObserver?.disconnect()
    }
  }, [components.length, hasComponents, updateScrollState])

  const scrollRail = (direction: -1 | 1) => {
    const rail = railRef.current
    if (!rail) return

    rail.scrollBy({
      behavior: 'smooth',
      left: direction * Math.max(168, rail.clientWidth * 0.75),
    })
  }

  if (!hasComponents && isLoading) {
    return (
      <div
        className={COMPONENT_RAIL_CLASS_NAME}
        aria-label="Basket components"
        aria-busy="true"
        role="status"
      >
        <span className="sr-only">Loading basket components</span>
        {[0, 1, 2].map((item) => (
          <div key={item} className={COMPONENT_CARD_CLASS_NAME} aria-hidden="true">
            <div className="flex items-center justify-between gap-3">
              <Skeleton width={82} height={16} />
              <Skeleton width={36} height={14} />
            </div>
            <Skeleton width={112} height={22} className="mt-2" />
          </div>
        ))}
      </div>
    )
  }

  if (!hasComponents) {
    return (
      <div
        className="border border-brand-border/20 bg-app-bg px-3 py-4 text-sm text-content-secondary"
        role="status"
      >
        {isError ? 'Basket components unavailable.' : 'No basket components available.'}
      </div>
    )
  }

  return (
    <div className="min-w-0">
      <div className="relative min-w-0">
        <div
          ref={railRef}
          className={COMPONENT_RAIL_CLASS_NAME}
          aria-label="Basket components"
          role="list"
          tabIndex={0}
          onScroll={updateScrollState}
          onKeyDown={(event) => {
            if (event.key !== 'ArrowLeft' && event.key !== 'ArrowRight') return
            event.preventDefault()
            scrollRail(event.key === 'ArrowLeft' ? -1 : 1)
          }}
        >
          {components.map((component) => {
            const key = componentKey(component)
            const priceChange = priceChanges[key]

            return (
              <article key={key} className={COMPONENT_CARD_CLASS_NAME} role="listitem">
                <div className="flex items-center justify-between gap-2">
                  <div className="flex min-w-0 items-center gap-2">
                    <ComponentFreshnessDot publishTime={component.publishTime} nowSeconds={nowSeconds} />
                    <span className="truncate text-sm font-semibold text-content-primary">{component.symbol}</span>
                  </div>
                  <span className="shrink-0 text-xs text-content-secondary">{componentWeight(component)}</span>
                </div>
                <div className="mt-2 flex min-w-0 items-baseline gap-2">
                  <span className="text-lg font-semibold text-brand-peach">{componentPrice(component)}</span>
                  {priceChange !== undefined ? (
                    <span
                      className={`shrink-0 text-[11px] font-medium ${componentChangeClass(priceChange)}`}
                      title="24h change"
                      aria-label={`24 hour change ${formatPercent(priceChange)}`}
                    >
                      {formatPercent(priceChange)}
                    </span>
                  ) : null}
                </div>
              </article>
            )
          })}
        </div>

        {canScrollLeft ? (
          <div className="pointer-events-none absolute inset-y-0 left-0 z-10 flex w-10 items-center bg-gradient-to-r from-surface-panel via-surface-panel/90 to-transparent pl-1">
            <button
              type="button"
              className="pointer-events-auto flex h-7 w-7 items-center justify-center border border-brand-border/35 bg-app-bg/95 text-content-secondary shadow-[0_6px_16px_-8px_rgba(0,0,0,0.9)] transition-colors hover:border-brand-border/60 hover:text-content-primary focus-visible:outline focus-visible:outline-1 focus-visible:outline-offset-1 focus-visible:outline-content-secondary"
              aria-label="Previous basket components"
              onClick={() => { scrollRail(-1) }}
            >
              <span className="material-symbols-outlined !text-[18px] !leading-none" aria-hidden="true">chevron_left</span>
            </button>
          </div>
        ) : null}

        {canScrollRight ? (
          <div className="pointer-events-none absolute inset-y-0 right-0 z-10 flex w-10 items-center justify-end bg-gradient-to-l from-surface-panel via-surface-panel/90 to-transparent pr-1">
            <button
              type="button"
              className="pointer-events-auto flex h-7 w-7 items-center justify-center border border-brand-border/35 bg-app-bg/95 text-content-secondary shadow-[0_6px_16px_-8px_rgba(0,0,0,0.9)] transition-colors hover:border-brand-border/60 hover:text-content-primary focus-visible:outline focus-visible:outline-1 focus-visible:outline-offset-1 focus-visible:outline-content-secondary"
              aria-label="Next basket components"
              onClick={() => { scrollRail(1) }}
            >
              <span className="material-symbols-outlined !text-[18px] !leading-none" aria-hidden="true">chevron_right</span>
            </button>
          </div>
        ) : null}
      </div>

      <div className="mt-3 flex flex-wrap items-start justify-between gap-2 border-t border-brand-border/20 pt-3 text-[11px] leading-4 text-content-secondary">
        <p className="min-w-0 flex-1">
          These six Pyth FX feeds form the basket behind plDXY. Percentages are reference
          coefficients, not live weights. Green means the foreign currency strengthened against
          USD, which pushes the displayed dollar-oriented plDXY price down. Component values are
          not execution quotes.
        </p>
        {docsLink ? (
          <DocsLink href={docsLink.href} title={docsLink.title} className="shrink-0">
            Learn more
          </DocsLink>
        ) : null}
      </div>
    </div>
  )
}
