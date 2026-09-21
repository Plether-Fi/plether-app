import { useEffect, useId, useRef, useState } from 'react'
import type { VaultHistory } from '../api'
import { Spinner } from './ui/Spinner'

const SEVEN_DAYS = 7 * 24 * 3600
const usd = new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD', maximumFractionDigits: 2 })
const compactUsd = new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD', notation: 'compact', maximumFractionDigits: 1 })
const date = (timestamp: number) => new Date(timestamp * 1000).toLocaleDateString('en-US', { month: 'short', day: 'numeric', timeZone: 'UTC' })
const timestampLabel = (timestamp: number) => `${new Date(timestamp * 1000).toLocaleString('en-US', { month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit', timeZone: 'UTC' })} UTC`

function assetValue(raw: string | null | undefined): number | null {
  if (typeof raw !== 'string' || !/^\d+$/.test(raw)) return null
  const value = Number(raw) / 1e6
  return Number.isFinite(value) ? value : null
}

export function VaultAssetChart({ tranche, history, isLoading, isError }: {
  tranche: 'senior' | 'junior'
  history?: VaultHistory
  isLoading?: boolean
  isError?: boolean
}) {
  const summaryId = useId()
  const svgRef = useRef<SVGSVGElement>(null)
  const [width, setWidth] = useState(720)
  const [now, setNow] = useState(() => Date.now() / 1000)
  const [activeTimestamp, setActiveTimestamp] = useState<number | null>(null)
  const end = Math.floor((now - 120) / 3600) * 3600
  const start = end - SEVEN_DAYS
  const points = history?.range === '7d' ? history[tranche].points.flatMap((point) => {
    const total = assetValue(point.totalAssets)
    const locked = point.markFresh ? assetValue(point.lockedAssets) : null
    if (total === null || !Number.isFinite(point.timestamp) || point.timestamp < start || point.timestamp > end) return []
    return [{ timestamp: point.timestamp, total, locked: locked !== null && locked <= total ? locked : null }]
  }).sort((a, b) => a.timestamp - b.timestamp) : []
  const hasPoints = points.length > 0
  const selected = points.find((point) => point.timestamp === activeTimestamp) ?? points.at(-1)
  const height = 260
  const left = 64
  const right = width - 16
  const top = 18
  const bottom = height - 34
  const maximum = Math.max(1, ...points.map((point) => point.total)) * 1.1
  const x = (timestamp: number) => left + (timestamp - start) / SEVEN_DAYS * (right - left)
  const y = (value: number) => bottom - value / maximum * (bottom - top)
  const path = (series: 'total' | 'locked') => {
    let previous: number | undefined
    return points.map((point) => {
      const value = point[series]
      if (value === null) { previous = undefined; return '' }
      const command = previous === undefined || point.timestamp - previous > 5400 ? 'M' : 'L'
      previous = point.timestamp
      return `${command}${x(point.timestamp).toFixed(2)},${y(value).toFixed(2)}`
    }).join(' ')
  }

  useEffect(() => {
    const timer = window.setInterval(() => { setNow(Date.now() / 1000) }, 60_000)
    const measure = () => {
      const measured = svgRef.current?.getBoundingClientRect().width
      if (measured && measured > 0) setWidth(Math.max(280, measured))
    }
    measure()
    const observer = typeof ResizeObserver === 'undefined' ? undefined : new ResizeObserver(measure)
    if (svgRef.current) observer?.observe(svgRef.current)
    window.addEventListener('resize', measure)
    return () => { window.clearInterval(timer); observer?.disconnect(); window.removeEventListener('resize', measure) }
  }, [hasPoints])

  function selectPoint(clientX: number) {
    const bounds = svgRef.current?.getBoundingClientRect()
    if (!bounds?.width || points.length === 0) return
    const target = start + (((clientX - bounds.left) / bounds.width * width - left) / (right - left)) * SEVEN_DAYS
    const nearest = points.reduce((a, b) => Math.abs(a.timestamp - target) < Math.abs(b.timestamp - target) ? a : b)
    setActiveTimestamp(nearest.timestamp)
  }

  return (
    <figure className="border border-brand-border/30 bg-surface-panel">
      <div className="flex items-start justify-between gap-4 border-b border-brand-border/25 panel-padding">
        <div>
          <h3 className="text-lg font-semibold text-content-primary">Total assets and locked value</h3>
          <p className="mt-1 text-sm text-content-secondary">Locked value is the portion of vault assets unavailable for withdrawal under the pool’s withdrawal limit.</p>
        </div>
        <span className="shrink-0 border border-brand-border/30 bg-app-bg px-3 py-1.5 text-xs font-semibold text-content-secondary">7d</span>
      </div>
      <div className="panel-padding">
        <ul aria-label="Chart legend" className="flex flex-wrap gap-x-8 gap-y-3">
          <li className="flex items-center gap-2 text-xs text-content-secondary"><span className="h-0.5 w-5 bg-brand-peach" />Total assets (TVL)</li>
          <li className="flex items-center gap-2 text-xs text-content-secondary"><span className="w-5 border-t-2 border-dashed border-brand-orange" />Locked value</li>
        </ul>
        {!hasPoints ? (
          <div className="flex min-h-56 items-center justify-center text-sm text-content-secondary" role="status">
            {isLoading ? <Spinner /> : isError ? 'Asset history is temporarily unavailable.' : 'Asset history is being collected. The last 7 days will appear here as data becomes available.'}
          </div>
        ) : (
          <>
            <div className="relative mt-4">
              <svg
                ref={svgRef}
                viewBox={`0 0 ${String(width)} ${String(height)}`}
                className="block h-[260px] w-full"
                role="img"
                tabIndex={0}
                aria-label={`${tranche === 'senior' ? 'Senior' : 'Junior'} Vault 7-day total assets and locked value chart`}
                aria-describedby={summaryId}
                aria-keyshortcuts="ArrowLeft ArrowRight Home End Escape"
                onFocus={() => { setActiveTimestamp(points.at(-1)?.timestamp ?? null) }}
                onBlur={() => { setActiveTimestamp(null) }}
                onPointerDown={(event) => { selectPoint(event.clientX) }}
                onPointerMove={(event) => { if (event.pointerType === 'mouse' || event.buttons > 0) selectPoint(event.clientX) }}
                onPointerLeave={() => { setActiveTimestamp(null) }}
                onKeyDown={(event) => {
                  if (event.key === 'Escape') {
                    setActiveTimestamp(null)
                    return
                  }
                  const index = Math.max(0, points.findIndex((point) => point.timestamp === selected?.timestamp))
                  const next = event.key === 'Home' ? 0 : event.key === 'End' ? points.length - 1
                    : event.key === 'ArrowLeft' ? Math.max(0, index - 1)
                      : event.key === 'ArrowRight' ? Math.min(points.length - 1, index + 1) : undefined
                  if (next !== undefined) { event.preventDefault(); setActiveTimestamp(points[next].timestamp) }
                }}
                style={{ touchAction: 'pan-y' }}
              >
                <title>Total assets (TVL) and locked value in USDC over 7 days</title>
                {[0, maximum / 2, maximum].map((value) => (
                  <g key={value}>
                    <line x1={left} x2={right} y1={y(value)} y2={y(value)} stroke="rgba(255,171,150,0.12)" strokeWidth={1} />
                    <text x={left - 10} y={y(value) + 4} textAnchor="end" className="fill-content-secondary text-[11px]">{compactUsd.format(value)}</text>
                  </g>
                ))}
                {[start, start + SEVEN_DAYS / 2, end].map((timestamp, index) => (
                  <text key={timestamp} x={x(timestamp)} y={height - 8} textAnchor={index === 0 ? 'start' : index === 2 ? 'end' : 'middle'} className="fill-content-secondary text-[11px]">{date(timestamp)}</text>
                ))}
                <path d={path('total')} fill="none" className="stroke-brand-peach" strokeWidth={2} />
                <path d={path('locked')} fill="none" className="stroke-brand-orange" strokeWidth={2} strokeDasharray="6 4" />
                {(['total', 'locked'] as const).map((series) => points.map((point, index) => {
                  const value = point[series]
                  const previous = index > 0 ? points[index - 1] : undefined
                  const next = points.at(index + 1)
                  const connectsBefore = previous?.[series] != null && point.timestamp - previous.timestamp <= 5400
                  const connectsAfter = next?.[series] != null && next.timestamp - point.timestamp <= 5400
                  return value !== null && !connectsBefore && !connectsAfter ? (
                    <circle key={`${series}-${String(point.timestamp)}`} cx={x(point.timestamp)} cy={y(value)} r={3} className={series === 'total' ? 'fill-brand-peach' : 'fill-brand-orange'} />
                  ) : null
                }))}
                {selected && activeTimestamp !== null && (
                  <g>
                    <line x1={x(selected.timestamp)} x2={x(selected.timestamp)} y1={top} y2={bottom} className="stroke-content-secondary/50" strokeDasharray="3 4" />
                    <circle cx={x(selected.timestamp)} cy={y(selected.total)} r={4} className="fill-brand-peach" />
                    {selected.locked !== null && <circle cx={x(selected.timestamp)} cy={y(selected.locked)} r={4} className="fill-brand-orange" />}
                  </g>
                )}
              </svg>
              {selected && activeTimestamp !== null && (
                <div
                  className="pointer-events-none absolute z-10 w-60 max-w-[calc(100%-1rem)] border border-brand-border/40 bg-app-bg px-3 py-2 shadow-xl"
                  style={{
                    left: `clamp(8px, calc(${String(x(selected.timestamp) / width * 100)}% ${x(selected.timestamp) < width / 2 ? '+ 12px' : '- 252px'}), calc(100% - 248px))`,
                    top: `${String(y(selected.total) / height * 100)}%`,
                    transform: y(selected.total) <= height / 2 ? 'translateY(12px)' : 'translateY(calc(-100% - 12px))',
                  }}
                  role="status"
                  aria-live="polite"
                >
                  <p className="text-[10px] font-semibold uppercase tracking-[0.12em] text-content-secondary">
                    {timestampLabel(selected.timestamp)}
                  </p>
                  <dl className="mt-2 space-y-2 text-xs">
                    <div className="flex items-center justify-between gap-3">
                      <dt className="flex items-center gap-2 text-content-secondary"><span className="h-0.5 w-3 bg-brand-peach" />TVL</dt>
                      <dd className="font-mono font-semibold text-content-primary">{usd.format(selected.total)}</dd>
                    </div>
                    <div className="flex items-center justify-between gap-3">
                      <dt className="flex items-center gap-2 text-content-secondary"><span className="w-3 border-t-2 border-dashed border-brand-orange" />Locked</dt>
                      <dd className="font-mono font-semibold text-content-primary">{selected.locked === null ? 'Unavailable' : usd.format(selected.locked)}</dd>
                    </div>
                  </dl>
                  <p className="mt-2 text-[10px] text-content-secondary">Values in USDC</p>
                </div>
              )}
            </div>
            <p id={summaryId} className="sr-only">
              {selected && `${timestampLabel(selected.timestamp)} · TVL ${usd.format(selected.total)} · Locked ${selected.locked === null ? 'unavailable' : usd.format(selected.locked)}`} · USDC
            </p>
          </>
        )}
        {hasPoints && (isError === true || !history?.coverage.complete) && (
          <p className="mt-3 text-xs text-content-secondary">{isError ? 'History refresh is temporarily unavailable. Showing the last received observations.' : 'Partial history. Gaps indicate missing observations or unavailable locked values.'}</p>
        )}
      </div>
    </figure>
  )
}
