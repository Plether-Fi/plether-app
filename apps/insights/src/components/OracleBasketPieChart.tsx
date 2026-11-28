import { useMemo, useState } from 'react'
import type { OracleBasketComponent } from '../api'
import { formatPrice } from '../utils/format'

export type BasketChartMode = 'value' | 'weight'

interface OracleBasketPieChartProps {
  components: OracleBasketComponent[]
  basketPrice: string | null
  reconstructedBasketPrice?: string | null
}

interface SliceItem {
  symbol: string
  feedSymbol: string
  feedId: string
  color: string
  targetWeightBps: number
  effectiveShareBps: number | null
  activeBps: number
  fraction: number
  contributionNum: number | null
  normalizedPriceNum: number | null
  basePriceNum: number | null
  changeFromBaseBps: number | null
  inverted: boolean
  startAngle: number
  endAngle: number
}

const CURRENCY_PALETTE: Record<string, string> = {
  'EUR/USD': '#FFB78B',
  'JPY/USD': '#7CC4FA',
  'GBP/USD': '#6EE7B7',
  'CAD/USD': '#FCD34D',
  'SEK/USD': '#C4B5FD',
  'CHF/USD': '#FB7185',
}

const FALLBACK_COLORS = ['#FFB78B', '#7CC4FA', '#6EE7B7', '#FCD34D', '#C4B5FD', '#FB7185']

function parseEightDec(value: string | null | undefined): number | null {
  if (!value || !/^\d+$/.test(value)) return null
  const num = Number(value) / 1e8
  return Number.isFinite(num) ? num : null
}

function polarToCartesian(cx: number, cy: number, radius: number, angleDeg: number) {
  const rad = ((angleDeg - 90) * Math.PI) / 180
  return {
    x: cx + radius * Math.cos(rad),
    y: cy + radius * Math.sin(rad),
  }
}

function describeDonutArc(
  cx: number,
  cy: number,
  outerR: number,
  innerR: number,
  startAngle: number,
  endAngle: number,
): string {
  const sweep = endAngle - startAngle
  if (sweep <= 0.1) return ''
  // Clamp near-full circle so SVG arc renders reliably if only 1 component exists
  const clampedEnd = sweep >= 359.8 ? startAngle + 359.8 : endAngle
  const outerStart = polarToCartesian(cx, cy, outerR, startAngle)
  const outerEnd = polarToCartesian(cx, cy, outerR, clampedEnd)
  const innerEnd = polarToCartesian(cx, cy, innerR, clampedEnd)
  const innerStart = polarToCartesian(cx, cy, innerR, startAngle)
  const largeArc = clampedEnd - startAngle > 180 ? 1 : 0

  return [
    `M ${outerStart.x.toFixed(2)} ${outerStart.y.toFixed(2)}`,
    `A ${String(outerR)} ${String(outerR)} 0 ${String(largeArc)} 1 ${outerEnd.x.toFixed(2)} ${outerEnd.y.toFixed(2)}`,
    `L ${innerEnd.x.toFixed(2)} ${innerEnd.y.toFixed(2)}`,
    `A ${String(innerR)} ${String(innerR)} 0 ${String(largeArc)} 0 ${innerStart.x.toFixed(2)} ${innerStart.y.toFixed(2)}`,
    'Z',
  ].join(' ')
}

function formatBpsPercent(bps: number | null): string {
  if (bps == null || !Number.isFinite(bps)) return '—'
  return `${(bps / 100).toFixed(2)}%`
}

function formatSignedBpsPercent(bps: number | null): string {
  if (bps == null || !Number.isFinite(bps)) return '—'
  const sign = bps > 0 ? '+' : ''
  return `${sign}${(bps / 100).toFixed(2)}%`
}

export function OracleBasketPieChart({
  components,
  basketPrice,
  reconstructedBasketPrice,
}: OracleBasketPieChartProps) {
  const hasEffectiveShares = components.some(
    (c) => c.effectiveShareBps != null && c.effectiveShareBps > 0,
  )
  const [mode, setMode] = useState<BasketChartMode>(hasEffectiveShares ? 'value' : 'weight')
  const [hoveredSymbol, setHoveredSymbol] = useState<string | null>(null)
  const [pinnedSymbol, setPinnedSymbol] = useState<string | null>(null)

  const activeMode: BasketChartMode = hasEffectiveShares ? mode : 'weight'
  const focusedSymbol = hoveredSymbol ?? pinnedSymbol

  const slices = useMemo<SliceItem[]>(() => {
    const rawItems = components.map((comp, idx) => {
      const targetWeightBps = comp.weightBps ?? 0
      const effectiveShareBps = comp.effectiveShareBps ?? null
      const activeBps =
        activeMode === 'value' && effectiveShareBps != null ? effectiveShareBps : targetWeightBps
      return {
        symbol: comp.symbol,
        feedSymbol: comp.feedSymbol,
        feedId: comp.feedId,
        color: CURRENCY_PALETTE[comp.symbol] ?? FALLBACK_COLORS[idx % FALLBACK_COLORS.length],
        targetWeightBps,
        effectiveShareBps,
        activeBps: Math.max(0, activeBps),
        contributionNum: parseEightDec(comp.contribution),
        normalizedPriceNum: parseEightDec(comp.price),
        basePriceNum: parseEightDec(comp.basePrice),
        changeFromBaseBps: comp.changeFromBaseBps ?? null,
        inverted: comp.inverted,
      }
    })

    const total = rawItems.reduce((acc, item) => acc + item.activeBps, 0)
    const gapDeg = rawItems.length > 1 ? 1.1 : 0
    const usableDeg = Math.max(0, 360 - gapDeg * rawItems.length)

    return rawItems.reduce<SliceItem[]>((acc, item) => {
      const startAngle =
        acc.length > 0 ? acc[acc.length - 1].endAngle + gapDeg : 0
      const fraction = total > 0 ? item.activeBps / total : 0
      const sweep = fraction * usableDeg
      const endAngle = startAngle + sweep
      acc.push({
        ...item,
        fraction,
        startAngle,
        endAngle,
      })
      return acc
    }, [])
  }, [components, activeMode])

  const activeSlice = useMemo(
    () => (focusedSymbol ? slices.find((s) => s.symbol === focusedSymbol) ?? null : null),
    [slices, focusedSymbol],
  )

  const rawBasketNum = parseEightDec(reconstructedBasketPrice ?? basketPrice)
  const displayMark = basketPrice ? formatPrice(basketPrice) : '—'

  if (slices.length === 0) {
    return (
      <div className="p-6 text-center text-sm text-content-tertiary">
        No FX component weights recorded for this oracle update.
      </div>
    )
  }

  return (
    <div className="space-y-5 p-4 sm:p-6">
      <div className="flex flex-wrap items-start justify-between gap-3">
        <div className="min-w-0">
          <h2 className="text-base font-semibold text-content-primary sm:text-lg">
            DXY Currency Basket Composition
          </h2>
          <p className="mt-0.5 text-xs text-content-tertiary">
            {activeMode === 'value'
              ? 'Realized value share of each currency in this snapshot’s raw basket index'
              : 'Canonical contract target weights summing to 100.00% across 6 FX pairs'}
          </p>
        </div>

        <div
          className="inline-flex flex-wrap border border-brand-border/35 bg-surface-canvas p-0.5 font-mono text-[11px]"
          role="group"
          aria-label="Basket pie chart mode"
        >
          <button
            type="button"
            disabled={!hasEffectiveShares}
            onClick={() => {
              setMode('value')
            }}
            className={`px-2.5 py-1 transition-colors ${
              activeMode === 'value'
                ? 'bg-brand-peach/20 font-semibold text-brand-peach'
                : 'text-content-secondary hover:text-content-primary disabled:cursor-not-allowed disabled:opacity-40'
            }`}
          >
            Effective Value Share
          </button>
          <button
            type="button"
            onClick={() => {
              setMode('weight')
            }}
            className={`px-2.5 py-1 transition-colors ${
              activeMode === 'weight'
                ? 'bg-brand-peach/20 font-semibold text-brand-peach'
                : 'text-content-secondary hover:text-content-primary'
            }`}
          >
            Target Weights
          </button>
        </div>
      </div>

      <div className="grid items-center gap-6 md:grid-cols-[232px_minmax(0,1fr)]">
        <div className="relative mx-auto flex w-full max-w-[232px] items-center justify-center">
          <svg
            viewBox="0 0 240 240"
            className="h-auto w-full overflow-visible"
            role="img"
            aria-label={
              activeMode === 'value'
                ? 'Pie chart of effective DXY currency basket value shares'
                : 'Pie chart of target DXY currency basket weights'
            }
          >
            <circle
              cx="120"
              cy="120"
              r="83"
              fill="none"
              stroke="rgba(255, 255, 255, 0.04)"
              strokeWidth="36"
            />
            {slices.map((slice) => {
              const isFocused = activeSlice?.symbol === slice.symbol
              const isDimmed = activeSlice != null && !isFocused
              const path = describeDonutArc(
                120,
                120,
                isFocused ? 106 : 100,
                63,
                slice.startAngle,
                slice.endAngle,
              )
              return (
                <path
                  key={slice.feedId}
                  d={path}
                  fill={slice.color}
                  fillOpacity={isDimmed ? 0.35 : 0.94}
                  stroke={isFocused ? '#FFFFFF' : 'rgba(13, 10, 12, 0.7)'}
                  strokeWidth={isFocused ? 1.75 : 1}
                  className="cursor-pointer transition-all duration-150 outline-none focus-visible:stroke-white focus-visible:stroke-2"
                  tabIndex={0}
                  role="button"
                  aria-label={`${slice.symbol} ${formatBpsPercent(slice.activeBps)}`}
                  aria-pressed={pinnedSymbol === slice.symbol}
                  onMouseEnter={() => {
                    setHoveredSymbol(slice.symbol)
                  }}
                  onMouseLeave={() => {
                    setHoveredSymbol(null)
                  }}
                  onFocus={() => {
                    setHoveredSymbol(slice.symbol)
                  }}
                  onBlur={() => {
                    setHoveredSymbol(null)
                  }}
                  onClick={() => {
                    setPinnedSymbol((prev) => (prev === slice.symbol ? null : slice.symbol))
                  }}
                  onKeyDown={(e) => {
                    if (e.key === 'Enter' || e.key === ' ') {
                      e.preventDefault()
                      setPinnedSymbol((prev) => (prev === slice.symbol ? null : slice.symbol))
                    }
                  }}
                />
              )
            })}
          </svg>

          <div className="pointer-events-none absolute inset-0 flex flex-col items-center justify-center px-8 text-center">
            {activeSlice ? (
              <>
                <span
                  className="font-mono text-[11px] font-semibold uppercase tracking-[0.12em]"
                  style={{ color: activeSlice.color }}
                >
                  {activeSlice.symbol}
                </span>
                <span className="mt-0.5 font-mono text-2xl font-bold tabular-nums text-content-primary">
                  {formatBpsPercent(activeSlice.activeBps)}
                </span>
                <span className="mt-0.5 font-mono text-[11px] tabular-nums text-content-secondary">
                  {activeSlice.contributionNum != null
                    ? `${activeSlice.contributionNum.toFixed(5)} raw`
                    : `${(activeSlice.targetWeightBps / 100).toFixed(1)}% weight`}
                </span>
                {activeSlice.changeFromBaseBps != null ? (
                  <span
                    className={`mt-0.5 font-mono text-[10px] font-semibold tabular-nums ${
                      activeSlice.changeFromBaseBps > 0
                        ? 'text-positive'
                        : activeSlice.changeFromBaseBps < 0
                          ? 'text-brand-orange'
                          : 'text-content-tertiary'
                    }`}
                  >
                    {formatSignedBpsPercent(activeSlice.changeFromBaseBps)} vs base
                  </span>
                ) : null}
              </>
            ) : (
              <>
                <span className="font-mono text-[10px] uppercase tracking-[0.14em] text-content-tertiary">
                  plDXY Mark
                </span>
                <span className="mt-0.5 font-mono text-xl font-bold tabular-nums text-content-primary">
                  {displayMark}
                </span>
                {rawBasketNum != null ? (
                  <span className="mt-0.5 font-mono text-[11px] tabular-nums text-content-secondary">
                    {rawBasketNum.toFixed(5)} raw
                  </span>
                ) : null}
                <span className="mt-1 font-mono text-[10px] uppercase tracking-[0.1em] text-brand-peach/80">
                  {activeMode === 'value' ? '6-FX Value Share' : '6-FX Target Wt'}
                </span>
              </>
            )}
          </div>
        </div>

        <ul className="min-w-0 space-y-2" aria-label="Currency basket share breakdown">
          {slices.map((slice) => {
            const isFocused = activeSlice?.symbol === slice.symbol
            const driftFromTargetBps =
              slice.effectiveShareBps != null
                ? slice.effectiveShareBps - slice.targetWeightBps
                : null
            return (
              <li key={slice.feedId}>
                <button
                  type="button"
                  onMouseEnter={() => {
                    setHoveredSymbol(slice.symbol)
                  }}
                  onMouseLeave={() => {
                    setHoveredSymbol(null)
                  }}
                  onFocus={() => {
                    setHoveredSymbol(slice.symbol)
                  }}
                  onBlur={() => {
                    setHoveredSymbol(null)
                  }}
                  onClick={() => {
                    setPinnedSymbol((prev) => (prev === slice.symbol ? null : slice.symbol))
                  }}
                  aria-pressed={pinnedSymbol === slice.symbol}
                  className={`w-full border px-3 py-2 text-left transition-colors ${
                    isFocused
                      ? 'border-brand-peach/55 bg-brand-peach/10'
                      : 'border-brand-border/20 bg-surface-canvas/45 hover:border-brand-border/40'
                  }`}
                >
                  <div className="flex flex-wrap items-center justify-between gap-x-3 gap-y-1">
                    <div className="flex min-w-0 items-center gap-2">
                      <span
                        className="h-2.5 w-2.5 shrink-0"
                        style={{ backgroundColor: slice.color }}
                        aria-hidden="true"
                      />
                      <span className="font-mono text-xs font-semibold text-content-primary">
                        {slice.symbol}
                      </span>
                      <span className="font-mono text-[11px] text-content-tertiary">
                        {slice.feedSymbol}
                        {slice.inverted ? ' (inv)' : ''}
                      </span>
                    </div>

                    <div className="flex items-baseline gap-2.5 font-mono text-xs tabular-nums">
                      <span className="font-semibold text-content-primary">
                        {formatBpsPercent(slice.activeBps)}
                      </span>
                      {activeMode === 'value' && driftFromTargetBps != null ? (
                        <span className="text-[11px] text-content-tertiary">
                          ({formatSignedBpsPercent(driftFromTargetBps)} vs target)
                        </span>
                      ) : slice.effectiveShareBps != null ? (
                        <span className="text-[11px] text-content-tertiary">
                          (eff {formatBpsPercent(slice.effectiveShareBps)})
                        </span>
                      ) : null}
                    </div>
                  </div>

                  <div className="mt-1.5 flex flex-wrap items-center justify-between gap-x-3 gap-y-0.5 font-mono text-[11px] tabular-nums text-content-tertiary">
                    <span>
                      Contribution:{' '}
                      <strong className="font-medium text-content-secondary">
                        {slice.contributionNum != null ? slice.contributionNum.toFixed(5) : '—'}
                      </strong>
                    </span>
                    <span
                      className={
                        slice.changeFromBaseBps != null
                          ? slice.changeFromBaseBps > 0
                            ? 'text-positive'
                            : slice.changeFromBaseBps < 0
                              ? 'text-brand-orange'
                              : 'text-content-tertiary'
                          : 'text-content-tertiary'
                      }
                    >
                      {slice.changeFromBaseBps != null
                        ? `${formatSignedBpsPercent(slice.changeFromBaseBps)} vs base`
                        : 'Base drift —'}
                    </span>
                  </div>

                  <div className="mt-1.5 h-1 w-full bg-brand-border/20">
                    <div
                      className="h-full transition-all duration-150"
                      style={{
                        width: `${String(Math.max(2, Math.round(slice.fraction * 100)))}%`,
                        backgroundColor: slice.color,
                      }}
                    />
                  </div>
                </button>
              </li>
            )
          })}
        </ul>
      </div>
    </div>
  )
}
