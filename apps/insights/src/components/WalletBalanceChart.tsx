import { useId, useMemo, useState } from 'react'
import {
  sliceBalanceHistory,
  type BalanceHistoryPoint,
  type BalanceTimeframe,
} from '../api/wallets'
import { formatAssets, formatCompactAssets, formatSignedAssets, formatUtc } from '../utils/format'
import { EmptyState, LoadingState, Panel } from './ui'

interface WalletBalanceChartProps {
  points: BalanceHistoryPoint[]
  observedAt?: string | null
  releaseVersion?: string | null
  loading?: boolean
  awaitingDeploy?: boolean
}

const TIMEFRAMES: { id: BalanceTimeframe; label: string }[] = [
  { id: '7d', label: '7D' },
  { id: '30d', label: '30D' },
  { id: '90d', label: '90D' },
  { id: 'all', label: 'ALL' },
]

const CHART_WIDTH = 1120
const CHART_HEIGHT = 276
const PAD_LEFT = 12
const PAD_RIGHT = 88
const PAD_TOP = 18
const PAD_BOTTOM = 30

function parseAssetUnits(raw: string): number {
  const numeric = Number(raw)
  return Number.isFinite(numeric) ? numeric / 1_000_000 : 0
}

function formatShortDate(unixSeconds: number, showTime: boolean): string {
  const date = new Date(unixSeconds * 1000)
  return date.toLocaleDateString('en-US', {
    month: 'short',
    day: 'numeric',
    ...(showTime ? { hour: '2-digit', minute: '2-digit', hour12: false } : {}),
    timeZone: 'UTC',
  })
}

function formatDeltaSummary(startAssets: bigint, endAssets: bigint): {
  deltaText: string
  pctText: string | null
  positive: boolean
  negative: boolean
} {
  const delta = endAssets - startAssets
  const sign = delta > 0n ? '+' : ''
  const deltaText = `${sign}${formatAssets(delta.toString())}`
  let pctText: string | null = null
  if (startAssets > 0n) {
    const pct = (Number(delta) / Number(startAssets)) * 100
    if (Number.isFinite(pct)) {
      pctText = `${pct >= 0 ? '+' : ''}${pct.toFixed(2)}%`
    }
  }
  return {
    deltaText,
    pctText,
    positive: delta > 0n,
    negative: delta < 0n,
  }
}

function buildStepAfterPath(coords: { x: number; y: number }[]): string {
  if (coords.length === 0) return ''
  let path = `M${coords[0].x.toFixed(2)},${coords[0].y.toFixed(2)}`
  for (let i = 1; i < coords.length; i += 1) {
    path += ` H${coords[i].x.toFixed(2)} V${coords[i].y.toFixed(2)}`
  }
  return path
}

function markerFill(pt: BalanceHistoryPoint, strokeColor: string): string {
  if (pt.eventType === 'Deposit') return '#34d399'
  if (pt.eventType === 'Withdraw' || pt.eventType === 'Liquidated') return '#ff572d'
  if (pt.eventType === 'Open' || pt.eventType === 'Close') return '#ffab96'
  return strokeColor
}

export function WalletBalanceChart({
  points,
  observedAt,
  releaseVersion,
  loading = false,
  awaitingDeploy = false,
}: WalletBalanceChartProps) {
  const [timeframe, setTimeframe] = useState<BalanceTimeframe>('30d')
  const [hoveredIndex, setHoveredIndex] = useState<number | null>(null)
  const gradientId = useId()

  const nowUnix = useMemo(() => {
    if (observedAt) {
      const parsed = Date.parse(observedAt)
      if (Number.isFinite(parsed)) return Math.floor(parsed / 1000)
    }
    return undefined
  }, [observedAt])

  const series = useMemo(
    () => sliceBalanceHistory(points, timeframe, nowUnix),
    [points, timeframe, nowUnix],
  )

  const geometry = useMemo(() => {
    if (series.length === 0) return null
    const values = series.map((pt) => parseAssetUnits(pt.balanceAssets))
    const rawMin = Math.min(...values)
    const rawMax = Math.max(...values)
    const span = rawMax - rawMin
    const yPad = span === 0 ? Math.max(rawMax * 0.08, 10) : span * 0.1
    const minVal = Math.max(0, rawMin - yPad)
    const maxVal = rawMax + yPad
    const rangeVal = Math.max(maxVal - minVal, 1)

    const minTs = series[0].timestamp
    const maxTs = series[series.length - 1].timestamp
    const tsSpan = Math.max(maxTs - minTs, 1)

    const plotW = CHART_WIDTH - PAD_LEFT - PAD_RIGHT
    const plotH = CHART_HEIGHT - PAD_TOP - PAD_BOTTOM

    const coords = series.map((pt, idx) => {
      const val = values[idx]
      const xRatio = series.length === 1 ? 0.5 : (pt.timestamp - minTs) / tsSpan
      const x = PAD_LEFT + xRatio * plotW
      const y = PAD_TOP + (1 - (val - minVal) / rangeVal) * plotH
      const prevAssets = idx > 0 ? BigInt(series[idx - 1].balanceAssets) : null
      const stepDeltaAssets =
        prevAssets !== null ? (BigInt(pt.balanceAssets) - prevAssets).toString() : null
      return { x, y, pt, val, stepDeltaAssets }
    })

    const linePath = buildStepAfterPath(coords)
    const baselineY = PAD_TOP + plotH
    const areaPath =
      coords.length >= 2
        ? `${linePath} L${coords[coords.length - 1].x.toFixed(2)},${baselineY.toFixed(2)} L${coords[0].x.toFixed(2)},${baselineY.toFixed(2)} Z`
        : ''

    const startRefY = coords[0].y

    const yTicks = [0, 0.25, 0.5, 0.75, 1].map((t) => {
      const val = minVal + (1 - t) * rangeVal
      const y = PAD_TOP + t * plotH
      return { y, rawAssets: String(Math.round(val * 1_000_000)) }
    })

    const xTicks =
      series.length >= 2
        ? [0, 0.25, 0.5, 0.75, 1].map((t) => ({
            x: PAD_LEFT + t * plotW,
            ts: Math.round(minTs + t * (maxTs - minTs)),
          }))
        : [{ x: PAD_LEFT + plotW / 2, ts: minTs }]

    const startBig = BigInt(series[0].balanceAssets)
    const endBig = BigInt(series[series.length - 1].balanceAssets)
    const minBig = series.reduce(
      (acc, pt) => (BigInt(pt.balanceAssets) < acc ? BigInt(pt.balanceAssets) : acc),
      startBig,
    )
    const maxBig = series.reduce(
      (acc, pt) => (BigInt(pt.balanceAssets) > acc ? BigInt(pt.balanceAssets) : acc),
      startBig,
    )

    return {
      coords,
      linePath,
      areaPath,
      startRefY,
      yTicks,
      xTicks,
      plotRight: CHART_WIDTH - PAD_RIGHT,
      plotBottom: PAD_TOP + plotH,
      delta: formatDeltaSummary(startBig, endBig),
      startAssets: series[0].balanceAssets,
      endAssets: series[series.length - 1].balanceAssets,
      minAssets: minBig.toString(),
      maxAssets: maxBig.toString(),
    }
  }, [series])

  const activePoint =
    geometry && hoveredIndex !== null && hoveredIndex < geometry.coords.length
      ? geometry.coords[hoveredIndex]
      : null

  const strokeColor = geometry?.delta.negative ? '#ff572d' : '#34d399'

  function handlePointerMove(event: React.PointerEvent<SVGSVGElement>) {
    if (!geometry || geometry.coords.length === 0) return
    const rect = event.currentTarget.getBoundingClientRect()
    if (rect.width <= 0) return
    const relX = ((event.clientX - rect.left) / rect.width) * CHART_WIDTH
    let closestIdx = 0
    let closestDist = Infinity
    for (let i = 0; i < geometry.coords.length; i += 1) {
      const dist = Math.abs(geometry.coords[i].x - relX)
      if (dist < closestDist) {
        closestDist = dist
        closestIdx = i
      }
    }
    setHoveredIndex(closestIdx)
  }

  return (
    <Panel className="overflow-hidden">
      <div className="flex flex-col justify-between gap-3 border-b border-brand-border/15 px-5 py-4 sm:flex-row sm:items-center">
        <div>
          <div className="flex flex-wrap items-center gap-2.5">
            <h2 className="text-base font-semibold">Wallet balance history</h2>
            {geometry ? (
              <span
                className={`font-mono text-xs font-semibold tabular-nums ${
                  geometry.delta.positive
                    ? 'text-positive'
                    : geometry.delta.negative
                      ? 'text-brand-orange'
                      : 'text-content-secondary'
                }`}
              >
                {geometry.delta.deltaText}
                {geometry.delta.pctText ? ` (${geometry.delta.pctText})` : ''}
              </span>
            ) : null}
          </div>
          <p className="mt-0.5 text-xs text-content-tertiary">
            {activePoint ? (
              <>
                <span className="font-mono text-content-primary">
                  {formatAssets(activePoint.pt.balanceAssets)}
                </span>
                {activePoint.stepDeltaAssets && activePoint.stepDeltaAssets !== '0' ? (
                  <span
                    className={`font-mono ${
                      activePoint.stepDeltaAssets.startsWith('-')
                        ? 'text-brand-orange'
                        : 'text-positive'
                    }`}
                  >
                    {' '}
                    ({formatSignedAssets(activePoint.stepDeltaAssets)})
                  </span>
                ) : null}
                {activePoint.pt.netEquityAssets && activePoint.pt.netEquityAssets !== '0' ? (
                  <span>
                    {' '}
                    · Net equity{' '}
                    <span className="font-mono text-content-primary">
                      {formatAssets(activePoint.pt.netEquityAssets)}
                    </span>
                  </span>
                ) : null}
                <span>
                  {' '}
                  · {activePoint.pt.eventType} · {formatUtc(activePoint.pt.occurredAt)}
                </span>
              </>
            ) : geometry ? (
              <>
                Start{' '}
                <span className="font-mono text-content-secondary">
                  {formatCompactAssets(geometry.startAssets)}
                </span>{' '}
                · Latest{' '}
                <span className="font-mono text-content-primary">
                  {formatCompactAssets(geometry.endAssets)}
                </span>{' '}
                · Range{' '}
                <span className="font-mono text-content-secondary">
                  {formatCompactAssets(geometry.minAssets)} – {formatCompactAssets(geometry.maxAssets)}
                </span>{' '}
                · <span className="font-mono text-content-secondary">{geometry.coords.length} pts</span>
              </>
            ) : (
              'Canonical settlement & trader-claim balance over time'
            )}
          </p>
        </div>

        <div
          role="group"
          aria-label="Balance chart timeframe"
          className="inline-flex shrink-0 border border-brand-border/30 bg-app-bg p-0.5"
        >
          {TIMEFRAMES.map((tf) => {
            const active = timeframe === tf.id
            return (
              <button
                key={tf.id}
                type="button"
                aria-pressed={active}
                onClick={() => {
                  setTimeframe(tf.id)
                  setHoveredIndex(null)
                }}
                className={`cursor-pointer px-3 py-1 font-mono text-xs font-semibold transition-colors ${
                  active
                    ? 'bg-brand-orange text-content-primary'
                    : 'text-content-secondary hover:text-content-primary'
                }`}
              >
                {tf.label}
              </button>
            )
          })}
        </div>
      </div>

      {loading ? (
        <div className="p-5">
          <LoadingState rows={3} />
        </div>
      ) : geometry && geometry.coords.length > 0 ? (
        <div className="px-4 pb-3 pt-4 sm:px-5">
          <svg
            viewBox={`0 0 ${String(CHART_WIDTH)} ${String(CHART_HEIGHT)}`}
            role="img"
            aria-label={`Wallet balance chart over ${timeframe}`}
            className="h-64 w-full select-none overflow-visible sm:h-72"
            onPointerMove={handlePointerMove}
            onPointerLeave={() => {
              setHoveredIndex(null)
            }}
          >
            <defs>
              <linearGradient id={gradientId} x1="0" y1="0" x2="0" y2="1">
                <stop offset="0%" stopColor={strokeColor} stopOpacity="0.24" />
                <stop offset="100%" stopColor={strokeColor} stopOpacity="0.01" />
              </linearGradient>
            </defs>

            {geometry.xTicks.map((tick, idx) => (
              <line
                key={`vx-${String(idx)}`}
                x1={tick.x}
                x2={tick.x}
                y1={PAD_TOP}
                y2={geometry.plotBottom}
                stroke="currentColor"
                strokeOpacity="0.06"
              />
            ))}

            {geometry.yTicks.map((tick, idx) => (
              <g key={`hy-${String(idx)}`}>
                <line
                  x1={PAD_LEFT}
                  x2={geometry.plotRight}
                  y1={tick.y}
                  y2={tick.y}
                  stroke="currentColor"
                  strokeOpacity={idx === 0 || idx === geometry.yTicks.length - 1 ? '0.14' : '0.09'}
                  strokeDasharray={idx === geometry.yTicks.length - 1 ? undefined : '3 4'}
                />
                <text
                  x={geometry.plotRight + 10}
                  y={tick.y + 3.5}
                  textAnchor="start"
                  className="fill-content-tertiary font-mono text-[10px]"
                >
                  {formatCompactAssets(tick.rawAssets)}
                </text>
              </g>
            ))}

            {geometry.coords.length >= 2 ? (
              <line
                x1={PAD_LEFT}
                x2={geometry.plotRight}
                y1={geometry.startRefY}
                y2={geometry.startRefY}
                stroke="#ffab96"
                strokeOpacity="0.3"
                strokeDasharray="4 4"
              />
            ) : null}

            {geometry.areaPath ? (
              <path d={geometry.areaPath} fill={`url(#${gradientId})`} />
            ) : null}

            {geometry.coords.length >= 2 ? (
              <path
                d={geometry.linePath}
                fill="none"
                stroke={strokeColor}
                strokeWidth="2.2"
                strokeLinejoin="round"
                strokeLinecap="round"
              />
            ) : null}

            {geometry.coords.length <= 60
              ? geometry.coords.map((c, idx) => {
                  const isHovered = hoveredIndex === idx
                  const isLast = idx === geometry.coords.length - 1
                  if (isHovered || isLast) return null
                  return (
                    <circle
                      key={`pt-${String(idx)}`}
                      cx={c.x}
                      cy={c.y}
                      r={2.8}
                      fill={markerFill(c.pt, strokeColor)}
                      fillOpacity={0.85}
                      stroke="#1a0912"
                      strokeWidth="1.2"
                    />
                  )
                })
              : null}

            {activePoint ? (
              <g>
                <line
                  x1={activePoint.x}
                  x2={activePoint.x}
                  y1={PAD_TOP}
                  y2={geometry.plotBottom}
                  stroke={strokeColor}
                  strokeOpacity="0.45"
                  strokeDasharray="2 3"
                />
                <line
                  x1={PAD_LEFT}
                  x2={geometry.plotRight}
                  y1={activePoint.y}
                  y2={activePoint.y}
                  stroke={strokeColor}
                  strokeOpacity="0.3"
                  strokeDasharray="2 3"
                />
                <circle
                  cx={activePoint.x}
                  cy={activePoint.y}
                  r="8"
                  fill={strokeColor}
                  fillOpacity="0.2"
                />
                <circle
                  cx={activePoint.x}
                  cy={activePoint.y}
                  r="4.5"
                  fill={strokeColor}
                  stroke="#1a0912"
                  strokeWidth="2"
                />
              </g>
            ) : (
              <g>
                <circle
                  cx={geometry.coords[geometry.coords.length - 1].x}
                  cy={geometry.coords[geometry.coords.length - 1].y}
                  r="7"
                  fill={strokeColor}
                  fillOpacity="0.2"
                />
                <circle
                  cx={geometry.coords[geometry.coords.length - 1].x}
                  cy={geometry.coords[geometry.coords.length - 1].y}
                  r="3.8"
                  fill={strokeColor}
                  stroke="#1a0912"
                  strokeWidth="1.6"
                />
              </g>
            )}

            {geometry.xTicks.map((tick, idx) => (
              <text
                key={`xl-${String(idx)}`}
                x={tick.x}
                y={CHART_HEIGHT - 6}
                textAnchor={
                  idx === 0
                    ? 'start'
                    : idx === geometry.xTicks.length - 1
                      ? 'end'
                      : 'middle'
                }
                className="fill-content-tertiary font-mono text-[10px]"
              >
                {formatShortDate(tick.ts, timeframe === '7d')}
              </text>
            ))}
          </svg>
          <p className="mt-1 text-[11px] text-content-tertiary">
            Settlement &amp; trader-claim balance reconstructed from canonical{' '}
            {releaseVersion ?? 'v2.0.0'} execution receipts, deposits/withdrawals, and the live
            CfdEngineAccountLens anchor. Step transitions reflect exact on-chain event timestamps.
          </p>
        </div>
      ) : (
        <EmptyState
          title={
            awaitingDeploy
              ? 'Wallet balance history awaiting Stage 3 backend'
              : 'No balance history yet'
          }
          message={
            awaitingDeploy
              ? 'Balance history will populate once the account inspector backend endpoint is deployed.'
              : 'Deposits, withdrawals, and executed trade receipts for this Trading Account will appear here.'
          }
        />
      )}
    </Panel>
  )
}
