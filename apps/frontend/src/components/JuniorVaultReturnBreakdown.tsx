import { useId, useLayoutEffect, useRef, useState } from 'react'
import type { JuniorReturnAttribution, VaultHistory } from '../api/types'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { buildJuniorReturnBreakdown, juniorReturnPeriod, type JuniorReturnPeriod } from '../utils/juniorReturnAttribution'
import { Spinner } from './ui/Spinner'

const signed = (value: number, digits = 3) => new Intl.NumberFormat('en-US', {
  minimumFractionDigits: digits, maximumFractionDigits: digits, signDisplay: 'exceptZero',
}).format(value)
const usdg = (raw: string | null) => raw === null ? 'Unavailable' : `${new Intl.NumberFormat('en-US', {
  minimumFractionDigits: 2, maximumFractionDigits: 2, signDisplay: 'exceptZero',
}).format(Number(raw) / 1e6)} USDG`

/** Use the same seven-day response as the share-price chart and its metrics. */
export function JuniorVaultReturnBreakdownSection({ history, isError }: { history?: VaultHistory; isError?: boolean }) {
  const matches = history?.range === '7d' && history.deployment.chainId === PERPS_ARBITRUM_SEPOLIA_CHAIN_ID
    && history.deployment.housePool.toLowerCase() === PERPS_ARBITRUM_SEPOLIA.housePool.toLowerCase()
    && history.deployment.juniorVault.toLowerCase() === PERPS_ARBITRUM_SEPOLIA.juniorVault.toLowerCase()
    && history.deployment.seniorVault.toLowerCase() === PERPS_ARBITRUM_SEPOLIA.seniorVault.toLowerCase()
  return <JuniorVaultReturnBreakdown
    period={matches ? juniorReturnPeriod(history) : undefined}
    attribution={matches ? history.junior.returnAttribution : undefined}
    isError={isError}
  />
}

export function JuniorVaultReturnBreakdown({ period, attribution, isLoading, isError }: {
  period?: JuniorReturnPeriod
  attribution?: JuniorReturnAttribution | null
  isLoading?: boolean
  isError?: boolean
}) {
  const headingId = useId()
  const tooltipId = useId()
  const [activeId, setActiveId] = useState<string | null>(null)
  const viewportRef = useRef<HTMLDivElement>(null)
  const [availableWidth, setAvailableWidth] = useState(0)
  const data = buildJuniorReturnBreakdown(period, attribution)
  const bars = data?.bars
  const hasBars = Boolean(bars)
  const selected = bars?.find((bar) => bar.id === activeId)
  // Keep labels readable on mobile, and grow the canvas width without scaling its height.
  const width = Math.max(880, (bars?.length ?? 9) * 88 + 72, Math.floor(availableWidth))

  useLayoutEffect(() => {
    const viewport = viewportRef.current
    if (!hasBars || !viewport) return
    const measure = () => { setAvailableWidth(viewport.getBoundingClientRect().width) }
    measure()
    const observer = typeof ResizeObserver === 'undefined' ? undefined : new ResizeObserver(measure)
    observer?.observe(viewport)
    window.addEventListener('resize', measure)
    return () => {
      observer?.disconnect()
      window.removeEventListener('resize', measure)
    }
  }, [hasBars])

  const left = 72
  const right = width - 20
  const top = 45
  const bottom = 280
  const low = Math.min(0, ...bars?.flatMap((bar) => [bar.start, bar.end]) ?? [])
  const high = Math.max(0, ...bars?.flatMap((bar) => [bar.start, bar.end]) ?? [])
  const span = Math.max(high - low, 0.01)
  const minimum = low < 0 ? low - span * 0.08 : 0
  const maximum = high + span * 0.18
  const y = (value: number) => bottom - (value - minimum) / (maximum - minimum) * (bottom - top)
  const step = (right - left) / (bars?.length ?? 9)
  const barWidth = step * 0.64

  return <figure aria-labelledby={headingId} className="overflow-hidden border border-brand-border/30 bg-surface-panel text-content-primary">
    <div className="flex flex-wrap items-start justify-between gap-4 border-b border-brand-border/25 panel-padding">
      <div>
        <h3 id={headingId} className="text-lg font-semibold">Return breakdown</h3>
        <p className="mt-1 text-sm text-content-secondary">How income, trading outcomes and deductions add up to your share-price return.</p>
        <p className="mt-2 text-xs text-content-secondary">Percentage-point contributions · after maintenance dilution</p>
      </div>
    </div>
    {bars ? <>
      <div className="panel-padding">
        <div ref={viewportRef} className="overflow-x-auto overflow-y-hidden" tabIndex={0} role="region" aria-label="Return waterfall; scroll horizontally to see all components">
          <svg viewBox={`0 0 ${String(width)} 350`} width={width} height={350} className="block" role="group" aria-label="Junior share-price return waterfall">
            <title>Junior return contributions in percentage points</title>
            <text x={left} y={17} className="fill-content-secondary" fontSize={11}>Return contribution (%)</text>
            {Array.from({ length: 6 }, (_, index) => minimum + (maximum - minimum) * index / 5).map((value) => <g key={value}>
              <line x1={left} x2={right} y1={y(value)} y2={y(value)} className="stroke-brand-border/15" />
              <text x={left - 10} y={y(value) + 4} textAnchor="end" className="fill-content-secondary" fontSize={11}>{signed(value, 2)}%</text>
            </g>)}
            <line x1={left} x2={right} y1={y(0)} y2={y(0)} className="stroke-brand-border/30" />
            {bars.map((bar, index) => {
              const x = left + index * step + (step - barWidth) / 2
              const barTop = y(Math.max(bar.start, bar.end))
              const height = Math.max(1.5, Math.abs(y(bar.start) - y(bar.end)))
              return <g key={bar.id} tabIndex={0} role="button"
                aria-label={`${bar.label}: ${signed(bar.contributionPp, 6)} percentage points; ${usdg(bar.amountUsdg)}. ${bar.description}`}
                aria-describedby={activeId === bar.id ? tooltipId : undefined}
                aria-pressed={activeId === bar.id}
                className="cursor-pointer outline-none"
                onPointerEnter={(event) => { if (event.pointerType === 'mouse') setActiveId(bar.id) }}
                onPointerLeave={(event) => { if (event.pointerType === 'mouse') setActiveId(null) }}
                onFocus={() => { setActiveId(bar.id) }}
                onBlur={() => { setActiveId(null) }}
                onClick={() => { setActiveId(bar.id) }}
                onKeyDown={(event) => {
                  if (event.key === 'Escape') setActiveId(null)
                  if (event.key === 'Enter' || event.key === ' ') { event.preventDefault(); setActiveId(bar.id) }
                  const target = event.key === 'ArrowRight' ? event.currentTarget.nextElementSibling : event.key === 'ArrowLeft' ? event.currentTarget.previousElementSibling : null
                  if (target instanceof SVGElement && target.getAttribute('role') === 'button') { event.preventDefault(); target.focus() }
                }}>
                <rect x={x - 8} y={top - 10} width={barWidth + 16} height={290} className={activeId === bar.id ? 'fill-brand-peach/5 stroke-brand-border/40' : 'fill-transparent stroke-none'} />
                {index < bars.length - 2 && <line x1={x + barWidth} x2={x + step} y1={y(bar.end)} y2={y(bar.end)} className="stroke-brand-border/30" strokeDasharray="3 3" />}
                <rect data-bar={bar.id} data-start={bar.start} data-end={bar.end} x={x} y={barTop} width={barWidth} height={height}
                  fill={bar.id === 'unexplained' ? 'transparent' : bar.color} stroke={bar.color} strokeDasharray={bar.id === 'unexplained' ? '4 3' : undefined} />
                <text x={x + barWidth / 2} y={barTop - 8} textAnchor="middle" className="fill-content-primary" fontSize={12} fontWeight={600}>{signed(bar.contributionPp)}%</text>
                <text x={x + barWidth / 2} y={305} textAnchor="middle" className="fill-content-secondary" fontSize={12}>
                  {bar.lines.map((line, lineIndex) => <tspan key={line} x={x + barWidth / 2} dy={lineIndex === 0 ? 0 : 16}>{line}</tspan>)}
                </text>
              </g>
            })}
          </svg>
        </div>
      </div>
      <div className="min-h-20 border-t border-brand-border/25 bg-app-bg panel-padding" id={tooltipId} role="status" aria-live="polite">
        {selected ? <>
          <div className="flex flex-wrap items-baseline justify-between gap-2">
            <p className="text-sm font-semibold" style={{ color: selected.color }}>{selected.label}</p>
            <p className="font-mono text-sm">{signed(selected.contributionPp, 6)} pp <span className="mx-2 text-content-secondary">·</span> {usdg(selected.amountUsdg)}</p>
          </div>
          <p className="mt-2 text-xs leading-relaxed text-content-secondary">{selected.description}</p>
        </> : <p className="text-xs leading-relaxed text-content-secondary">Hover, tap or focus a bar to see its contribution and USDG amount. On smaller screens, scroll horizontally to explore all components.</p>}
      </div>
      {data.residualPp !== null && <p role="alert" className="border-t border-brand-border/25 panel-padding text-sm text-brand-orange">
        Unexplained reconciliation difference: {signed(data.residualPp, 8)} pp. Shown separately; not assigned to trader PnL or open-position valuation. Inspect the outlined bar for the USDG difference.
      </p>}
      {data.notes.length > 0 && <ul className="list-disc border-t border-brand-border/25 panel-padding text-xs leading-relaxed text-content-secondary">
        {data.notes.map((note) => <li key={note} className="ml-4">{note}</li>)}
      </ul>}
    </> : <div className="panel-padding text-sm leading-relaxed text-content-secondary" role="status">
      {isLoading ? <Spinner /> : data?.reason ?? (isError ? 'Return history is temporarily unavailable.' : 'Complete, fresh diluted share-price history is not yet available for the selected period.')}
    </div>}
    {isError && data && <p className="panel-padding-x pb-6 text-xs text-brand-orange">Refresh failed. Showing the last received reporting period.</p>}
  </figure>
}
