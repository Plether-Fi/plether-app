import { useMemo, useState } from 'react'
import {
  formatLeverageX100,
  type ExposureTimelinePoint,
} from '../api/positions'
import { useContainerWidth } from '../hooks/useContainerWidth'
import { formatCompactAssets, formatUtc } from '../utils/format'
import { EmptyState, Panel } from './ui'

interface WalletExposureChartProps {
  points: ExposureTimelinePoint[]
  releaseVersion?: string | null
}

type ExposureChartMode = 'notional' | 'leverage'

const DEFAULT_WIDTH = 1040
const HEIGHT = 236
const PAD_LEFT = 12
const PAD_RIGHT = 76
const PAD_TOP = 18
const PAD_BOTTOM = 28

function parseAssetNum(raw: string): number {
  const n = Number(raw)
  return Number.isFinite(n) ? n / 1_000_000 : 0
}

function formatAxisNotional(value: number): string {
  const abs = Math.abs(value)
  const sign = value > 0 ? '+' : value < 0 ? '-' : ''
  if (abs >= 1_000_000) return `${sign}${(abs / 1_000_000).toFixed(1)}M`
  if (abs >= 1_000) return `${sign}${(abs / 1_000).toFixed(1)}K`
  return `${sign}${abs.toFixed(0)}`
}

function shortDate(unixSeconds: number, showTime: boolean): string {
  const d = new Date(unixSeconds * 1000)
  return d.toLocaleDateString('en-US', {
    month: 'short',
    day: 'numeric',
    ...(showTime ? { hour: '2-digit', minute: '2-digit', hour12: false } : {}),
    timeZone: 'UTC',
  })
}

export function WalletExposureChart({ points, releaseVersion }: WalletExposureChartProps) {
  const [mode, setMode] = useState<ExposureChartMode>('notional')
  const [hoveredIdx, setHoveredIdx] = useState<number | null>(null)
  const [chartBoxRef, measuredWidth] = useContainerWidth(DEFAULT_WIDTH)
  const chartWidth = Math.max(320, measuredWidth)

  const plotW = chartWidth - PAD_LEFT - PAD_RIGHT
  const plotH = HEIGHT - PAD_TOP - PAD_BOTTOM

  const chart = useMemo(() => {
    if (points.length === 0) return null
    const minTs = points[0].timestamp
    const maxTs = points[points.length - 1].timestamp
    const spanTs = Math.max(1, maxTs - minTs)
    const showTime = spanTs <= 3 * 86_400

    const xOf = (ts: number, idx: number) =>
      points.length === 1
        ? PAD_LEFT + plotW / 2
        : PAD_LEFT + ((ts - minTs) / spanTs) * plotW + (spanTs === 1 ? (idx / points.length) * plotW : 0)

    if (mode === 'notional') {
      const vals = points.map((p) => parseAssetNum(p.signedNotionalAssets))
      const maxAbs = Math.max(...vals.map((v) => Math.abs(v)), 100)
      const paddedMax = maxAbs * 1.15
      const zeroY = PAD_TOP + plotH / 2
      const yOf = (val: number) => zeroY - (val / paddedMax) * (plotH / 2)

      const coords = points.map((p, i) => ({
        x: xOf(p.timestamp, i),
        y: yOf(vals[i]),
        val: vals[i],
        pt: p,
      }))

      // Build step-after line & split positive/negative fills cleanly
      let linePath = `M${coords[0].x.toFixed(1)},${coords[0].y.toFixed(1)}`
      for (let i = 1; i < coords.length; i += 1) {
        linePath += ` H${coords[i].x.toFixed(1)} V${coords[i].y.toFixed(1)}`
      }

      const posSegments: string[] = []
      const negSegments: string[] = []
      for (let i = 0; i < coords.length - 1; i += 1) {
        const a = coords[i]
        const b = coords[i + 1]
        if (a.val > 0) {
          posSegments.push(
            `M${a.x.toFixed(1)},${zeroY.toFixed(1)} V${a.y.toFixed(1)} H${b.x.toFixed(1)} V${zeroY.toFixed(1)} Z`,
          )
        } else if (a.val < 0) {
          negSegments.push(
            `M${a.x.toFixed(1)},${zeroY.toFixed(1)} V${a.y.toFixed(1)} H${b.x.toFixed(1)} V${zeroY.toFixed(1)} Z`,
          )
        }
      }

      return {
        kind: 'notional' as const,
        coords,
        linePath,
        posSegments,
        negSegments,
        zeroY,
        showTime,
        yTicks: [
          { y: yOf(paddedMax * 0.85), label: `${formatAxisNotional(paddedMax * 0.85)} L` },
          { y: zeroY, label: 'FLAT 0' },
          { y: yOf(-paddedMax * 0.85), label: `${formatAxisNotional(-paddedMax * 0.85)} S` },
        ],
      }
    }

    // Leverage mode
    const vals = points.map((p) => p.leverageX100 / 100)
    const maxLev = Math.max(...vals, 5)
    const ceiling = Math.ceil((maxLev * 1.15) / 5) * 5
    const yOf = (val: number) => PAD_TOP + plotH - (val / Math.max(ceiling, 1)) * plotH

    const coords = points.map((p, i) => ({
      x: xOf(p.timestamp, i),
      y: yOf(vals[i]),
      val: vals[i],
      pt: p,
    }))

    let linePath = `M${coords[0].x.toFixed(1)},${coords[0].y.toFixed(1)}`
    for (let i = 1; i < coords.length; i += 1) {
      linePath += ` H${coords[i].x.toFixed(1)} V${coords[i].y.toFixed(1)}`
    }
    const baseLineY = PAD_TOP + plotH
    const areaPath =
      `${linePath} L${coords[coords.length - 1].x.toFixed(1)},${baseLineY.toFixed(1)} L${coords[0].x.toFixed(1)},${baseLineY.toFixed(1)} Z`

    return {
      kind: 'leverage' as const,
      coords,
      linePath,
      areaPath,
      zeroY: baseLineY,
      showTime,
      yTicks: [
        { y: yOf(ceiling), label: `${String(ceiling)}×` },
        { y: yOf(ceiling / 2), label: `${(ceiling / 2).toFixed(0)}×` },
        { y: baseLineY, label: '0×' },
      ],
    }
  }, [points, mode, plotW, plotH])

  const activeIdx =
    hoveredIdx !== null && hoveredIdx >= 0 && hoveredIdx < points.length
      ? hoveredIdx
      : points.length - 1
  const activePt = points[activeIdx]

  return (
    <Panel className="p-5 sm:p-6">
      <div className="flex flex-col justify-between gap-3 sm:flex-row sm:items-start">
        <div>
          <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
            Market Exposure &amp; Leverage Over Time
          </p>
          <p className="mt-1 text-xs text-content-tertiary">
            {mode === 'notional'
              ? `Signed plDXY-PERP notional exposure (${releaseVersion ?? 'current'}): above zero is Long, below zero is Short`
              : `Effective position leverage (notional ÷ locked margin) across every fill and margin top-up`}
          </p>
        </div>

        <div className="flex items-center gap-1" role="group" aria-label="Exposure chart mode">
          {(
            [
              ['notional', 'Net Exposure (USDG)'],
              ['leverage', 'Leverage (×)'],
            ] as const
          ).map(([id, label]) => (
            <button
              key={id}
              type="button"
              aria-pressed={mode === id}
              onClick={() => {
                setMode(id)
                setHoveredIdx(null)
              }}
              className={`border px-3 py-1.5 text-xs font-semibold transition-colors ${
                mode === id
                  ? 'border-brand-orange bg-brand-orange text-content-primary'
                  : 'border-brand-border/35 bg-app-bg text-content-secondary hover:border-brand-peach/60 hover:text-content-primary'
              }`}
            >
              {label}
            </button>
          ))}
        </div>
      </div>

      {!chart || points.length === 0 ? (
        <div className="mt-4">
          <EmptyState
            title="No exposure history yet"
            message="This Trading Account has not opened a plDXY-PERP position on the active release."
          />
        </div>
      ) : (
        <div className="mt-4 space-y-3">
          {/* Calm single-line telemetry readout */}
          <div className="flex flex-wrap items-center justify-between gap-3 border border-brand-border/20 bg-app-bg px-3.5 py-2 text-xs">
            <div className="flex flex-wrap items-center gap-3">
              <span className="font-mono text-content-tertiary">
                {formatUtc(activePt.occurredAt)}
              </span>
              <span
                className={`border px-1.5 py-0.5 font-mono text-[11px] font-semibold uppercase ${
                  activePt.side === 'long'
                    ? 'border-positive/40 bg-positive/10 text-positive'
                    : activePt.side === 'short'
                      ? 'border-brand-orange/40 bg-brand-orange/10 text-brand-orange'
                      : 'border-brand-border/30 bg-surface-panel text-content-tertiary'
                }`}
              >
                {activePt.side}
              </span>
              <span className="text-content-secondary">{activePt.eventType}</span>
            </div>
            <div className="flex flex-wrap items-center gap-4 font-mono tabular-nums">
              <span>
                <span className="text-content-tertiary">Notional </span>
                <span className="font-semibold text-content-primary">
                  {activePt.side === 'flat'
                    ? '0.00 USDG'
                    : formatCompactAssets(activePt.absNotionalAssets)}
                </span>
              </span>
              <span>
                <span className="text-content-tertiary">Leverage </span>
                <span className="font-semibold text-brand-peach">
                  {activePt.side === 'flat' ? '0×' : formatLeverageX100(activePt.leverageX100)}
                </span>
              </span>
              <span>
                <span className="text-content-tertiary">Margin </span>
                <span className="text-content-primary">
                  {formatCompactAssets(activePt.marginAssets)}
                </span>
              </span>
            </div>
          </div>

          <div ref={chartBoxRef} className="w-full border border-brand-border/20 bg-app-bg p-2">
            <svg
              viewBox={`0 0 ${String(chartWidth)} ${String(HEIGHT)}`}
              style={{ height: `${String(HEIGHT)}px` }}
              className="block w-full overflow-visible"
              role="img"
              aria-label={
                mode === 'notional'
                  ? 'Wallet signed notional exposure over time'
                  : 'Wallet effective leverage over time'
              }
              onMouseLeave={() => { setHoveredIdx(null) }}
            >
              {/* Grid lines & right axis labels */}
              {chart.yTicks.map((tick) => (
                <g key={tick.label}>
                  <line
                    x1={PAD_LEFT}
                    y1={tick.y}
                    x2={chartWidth - PAD_RIGHT}
                    y2={tick.y}
                    stroke="currentColor"
                    className={
                      tick.label.startsWith('FLAT')
                        ? 'text-brand-border/50'
                        : 'text-brand-border/20'
                    }
                    strokeDasharray={tick.label.startsWith('FLAT') ? '4 4' : undefined}
                    strokeWidth={1}
                  />
                  <text
                    x={chartWidth - PAD_RIGHT + 8}
                    y={tick.y + 3}
                    className="fill-content-tertiary font-mono text-[10px]"
                  >
                    {tick.label}
                  </text>
                </g>
              ))}

              {/* Shaded exposure fills */}
              {chart.kind === 'notional' ? (
                <>
                  {chart.posSegments.map((d, idx) => (
                    <path key={`p-${String(idx)}`} d={d} fill="#34d399" fillOpacity={0.22} />
                  ))}
                  {chart.negSegments.map((d, idx) => (
                    <path key={`n-${String(idx)}`} d={d} fill="#ff572d" fillOpacity={0.22} />
                  ))}
                </>
              ) : (
                <path d={chart.areaPath} fill="#ffab96" fillOpacity={0.18} />
              )}

              {/* Step line */}
              <path
                d={chart.linePath}
                fill="none"
                stroke={mode === 'notional' ? '#f5efe6' : '#ffab96'}
                strokeWidth={2}
                strokeLinejoin="round"
              />

              {/* Event dots */}
              {chart.coords.map((c, idx) => {
                const isSelected = idx === activeIdx
                const fill =
                  mode === 'leverage'
                    ? '#ffab96'
                    : c.pt.side === 'long'
                      ? '#34d399'
                      : c.pt.side === 'short'
                        ? '#ff572d'
                        : '#9a8c8f'
                return (
                  <g key={`${String(c.pt.timestamp)}-${String(idx)}`}>
                    {isSelected ? (
                      <line
                        x1={c.x}
                        y1={PAD_TOP}
                        x2={c.x}
                        y2={PAD_TOP + plotH}
                        stroke="#ffab96"
                        strokeOpacity={0.45}
                        strokeDasharray="3 3"
                      />
                    ) : null}
                    <circle
                      cx={c.x}
                      cy={c.y}
                      r={isSelected ? 4.5 : 2.5}
                      fill={fill}
                      stroke="#14080e"
                      strokeWidth={1.25}
                    />
                    <rect
                      x={c.x - 14}
                      y={PAD_TOP}
                      width={28}
                      height={plotH}
                      fill="transparent"
                      className="cursor-crosshair"
                      onMouseEnter={() => { setHoveredIdx(idx) }}
                    />
                  </g>
                )
              })}

              {/* X-axis start / mid / end labels */}
              {[0, Math.floor((chart.coords.length - 1) / 2), chart.coords.length - 1]
                .filter((idx, i, arr) => arr.indexOf(idx) === i && idx >= 0 && idx < chart.coords.length)
                .map((idx) => {
                  const c = chart.coords[idx]
                  return (
                    <text
                      key={`x-${String(idx)}`}
                      x={c.x}
                      y={HEIGHT - 6}
                      textAnchor={idx === 0 ? 'start' : idx === chart.coords.length - 1 ? 'end' : 'middle'}
                      className="fill-content-tertiary font-mono text-[10px]"
                    >
                      {shortDate(c.pt.timestamp, chart.showTime)}
                    </text>
                  )
                })}
            </svg>
          </div>
        </div>
      )}
    </Panel>
  )
}
