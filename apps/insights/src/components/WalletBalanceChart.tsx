import { useId, useMemo, useState } from 'react'
import {
  buildCumulativePnlSeries,
  sliceBalanceHistory,
  type AccountActivityRecord,
  type BalanceHistoryPoint,
  type BalanceTimeframe,
} from '../api/wallets'
import { useContainerWidth } from '../hooks/useContainerWidth'
import { formatAssets, formatCompactAssets, formatSignedAssets, formatUtc } from '../utils/format'
import { EmptyState, LoadingState, Panel } from './ui'

interface WalletBalanceChartProps {
  points: BalanceHistoryPoint[]
  activity?: AccountActivityRecord[]
  observedAt?: string | null
  releaseVersion?: string | null
  loading?: boolean
  awaitingDeploy?: boolean
}

type ChartMode = 'balance' | 'pnl'

const TIMEFRAMES: { id: BalanceTimeframe; label: string }[] = [
  { id: '7d', label: '7D' },
  { id: '30d', label: '30D' },
  { id: '90d', label: '90D' },
  { id: 'all', label: 'ALL' },
]

const DEFAULT_CHART_WIDTH = 1120
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
  activity = [],
  observedAt,
  releaseVersion,
  loading = false,
  awaitingDeploy = false,
}: WalletBalanceChartProps) {
  const [mode, setMode] = useState<ChartMode>('balance')
  const [timeframe, setTimeframe] = useState<BalanceTimeframe>('30d')
  const [hoveredIndex, setHoveredIndex] = useState<number | null>(null)
  const gradientId = useId().replace(/:/g, '')
  const [chartBoxRef, measuredWidth] = useContainerWidth(DEFAULT_CHART_WIDTH)
  const chartWidth = Math.max(320, measuredWidth)

  const nowUnix = useMemo(() => {
    if (observedAt) {
      const parsed = Date.parse(observedAt)
      if (Number.isFinite(parsed)) return Math.floor(parsed / 1000)
    }
    return undefined
  }, [observedAt])

  const balanceSeries = useMemo(
    () => sliceBalanceHistory(points, timeframe, nowUnix),
    [points, timeframe, nowUnix],
  )

  const pnlSeries = useMemo(
    () => buildCumulativePnlSeries(activity, timeframe, nowUnix),
    [activity, timeframe, nowUnix],
  )

  const balanceGeometry = useMemo(() => {
    if (balanceSeries.length === 0) return null
    const values = balanceSeries.map((pt) => parseAssetUnits(pt.balanceAssets))
    const rawMin = Math.min(...values)
    const rawMax = Math.max(...values)
    const span = rawMax - rawMin
    const yPad = span === 0 ? Math.max(rawMax * 0.08, 10) : span * 0.1
    const minVal = Math.max(0, rawMin - yPad)
    const maxVal = rawMax + yPad
    const rangeVal = Math.max(maxVal - minVal, 1)

    const minTs = balanceSeries[0].timestamp
    const maxTs = balanceSeries[balanceSeries.length - 1].timestamp
    const tsSpan = Math.max(maxTs - minTs, 1)

    const plotW = chartWidth - PAD_LEFT - PAD_RIGHT
    const plotH = CHART_HEIGHT - PAD_TOP - PAD_BOTTOM

    const coords = balanceSeries.map((pt, idx) => {
      const val = values[idx]
      const xRatio = balanceSeries.length === 1 ? 0.5 : (pt.timestamp - minTs) / tsSpan
      const x = PAD_LEFT + xRatio * plotW
      const y = PAD_TOP + (1 - (val - minVal) / rangeVal) * plotH
      const prevAssets = idx > 0 ? BigInt(balanceSeries[idx - 1].balanceAssets) : null
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
      balanceSeries.length >= 2
        ? [0, 0.25, 0.5, 0.75, 1].map((t) => ({
            x: PAD_LEFT + t * plotW,
            ts: Math.round(minTs + t * (maxTs - minTs)),
          }))
        : [{ x: PAD_LEFT + plotW / 2, ts: minTs }]

    const startBig = BigInt(balanceSeries[0].balanceAssets)
    const endBig = BigInt(balanceSeries[balanceSeries.length - 1].balanceAssets)
    const minBig = balanceSeries.reduce(
      (acc, pt) => (BigInt(pt.balanceAssets) < acc ? BigInt(pt.balanceAssets) : acc),
      startBig,
    )
    const maxBig = balanceSeries.reduce(
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
      plotRight: chartWidth - PAD_RIGHT,
      plotBottom: PAD_TOP + plotH,
      delta: formatDeltaSummary(startBig, endBig),
      startAssets: balanceSeries[0].balanceAssets,
      endAssets: balanceSeries[balanceSeries.length - 1].balanceAssets,
      minAssets: minBig.toString(),
      maxAssets: maxBig.toString(),
    }
  }, [balanceSeries, chartWidth])

  const pnlGeometry = useMemo(() => {
    if (pnlSeries.points.length === 0) return null
    const cumValues = pnlSeries.points.map((pt) => parseAssetUnits(pt.cumulativePnlAssets))
    const tradeValues = pnlSeries.points.map((pt) => parseAssetUnits(pt.tradePnlAssets))
    const rawMin = Math.min(0, ...cumValues)
    const rawMax = Math.max(0, ...cumValues)
    const span = rawMax - rawMin
    const yPad = span === 0 ? Math.max(Math.abs(rawMax) * 0.15, 25) : span * 0.14
    const minVal = rawMin - yPad
    const maxVal = rawMax + yPad
    const rangeVal = Math.max(maxVal - minVal, 1)

    const minTs = pnlSeries.points[0].timestamp
    const maxTs = pnlSeries.points[pnlSeries.points.length - 1].timestamp
    const tsSpan = Math.max(maxTs - minTs, 1)

    const plotW = chartWidth - PAD_LEFT - PAD_RIGHT
    const plotH = CHART_HEIGHT - PAD_TOP - PAD_BOTTOM
    const zeroY = PAD_TOP + (1 - (0 - minVal) / rangeVal) * plotH
    const maxAbsTrade = Math.max(...tradeValues.map((v) => Math.abs(v)), 1)
    const maxBarHeight = plotH * 0.34

    const coords = pnlSeries.points.map((pt, idx) => {
      const cumVal = cumValues[idx]
      const tradeVal = tradeValues[idx]
      const xRatio = pnlSeries.points.length === 1 ? 0.5 : (pt.timestamp - minTs) / tsSpan
      const x = PAD_LEFT + xRatio * plotW
      const y = PAD_TOP + (1 - (cumVal - minVal) / rangeVal) * plotH
      const rawBarH = (Math.abs(tradeVal) / maxAbsTrade) * maxBarHeight
      const barH = tradeVal === 0 ? 0 : Math.max(rawBarH, 4)
      const barY = tradeVal >= 0 ? Math.max(PAD_TOP, zeroY - barH) : zeroY
      const clampedBarH =
        tradeVal >= 0 ? zeroY - barY : Math.min(PAD_TOP + plotH - zeroY, barH)
      return { x, y, pt, cumVal, tradeVal, barY, barH: clampedBarH }
    })

    const linePath = buildStepAfterPath(coords)
    const areaPath =
      coords.length >= 2
        ? `${linePath} L${coords[coords.length - 1].x.toFixed(2)},${zeroY.toFixed(2)} L${coords[0].x.toFixed(2)},${zeroY.toFixed(2)} Z`
        : ''

    const yTicks = [0, 0.25, 0.5, 0.75, 1].map((t) => {
      const val = minVal + (1 - t) * rangeVal
      const y = PAD_TOP + t * plotH
      return { y, rawAssets: String(Math.round(val * 1_000_000)) }
    })

    const xTicks =
      pnlSeries.points.length >= 2
        ? [0, 0.25, 0.5, 0.75, 1].map((t) => ({
            x: PAD_LEFT + t * plotW,
            ts: Math.round(minTs + t * (maxTs - minTs)),
          }))
        : [{ x: PAD_LEFT + plotW / 2, ts: minTs }]

    const totalBig = BigInt(pnlSeries.totalPnlAssets)
    const totalDecided = pnlSeries.wins + pnlSeries.losses
    const winRatePct = totalDecided > 0 ? ((pnlSeries.wins / totalDecided) * 100).toFixed(1) : null

    return {
      coords,
      linePath,
      areaPath,
      zeroY,
      yTicks,
      xTicks,
      plotRight: chartWidth - PAD_RIGHT,
      plotBottom: PAD_TOP + plotH,
      positive: totalBig > 0n,
      negative: totalBig < 0n,
      winRatePct,
    }
  }, [pnlSeries, chartWidth])

  const activeBalancePoint =
    mode === 'balance' &&
    balanceGeometry &&
    hoveredIndex !== null &&
    hoveredIndex < balanceGeometry.coords.length
      ? balanceGeometry.coords[hoveredIndex]
      : null

  const activePnlPoint =
    mode === 'pnl' &&
    pnlGeometry &&
    hoveredIndex !== null &&
    hoveredIndex < pnlGeometry.coords.length
      ? pnlGeometry.coords[hoveredIndex]
      : null

  const strokeColor =
    mode === 'balance'
      ? balanceGeometry?.delta.negative
        ? '#ff572d'
        : '#34d399'
      : pnlGeometry?.negative
        ? '#ff572d'
        : '#34d399'

  function handlePointerMove(event: React.PointerEvent<SVGSVGElement>) {
    const activeCoords = mode === 'balance' ? balanceGeometry?.coords : pnlGeometry?.coords
    if (!activeCoords || activeCoords.length === 0) return
    const rect = event.currentTarget.getBoundingClientRect()
    if (rect.width <= 0) return
    const relX = ((event.clientX - rect.left) / rect.width) * chartWidth
    let closestIdx = 0
    let closestDist = Infinity
    for (let i = 0; i < activeCoords.length; i += 1) {
      const dist = Math.abs(activeCoords[i].x - relX)
      if (dist < closestDist) {
        closestDist = dist
        closestIdx = i
      }
    }
    setHoveredIndex(closestIdx)
  }

  const hasPnlOption = activity.length > 0 || pnlSeries.points.length > 0

  return (
    <Panel className="overflow-hidden">
      <div className="flex flex-col justify-between gap-3 border-b border-brand-border/15 px-5 py-4 lg:flex-row lg:items-center">
        <div>
          <div className="flex flex-wrap items-center gap-2.5">
            <h2 className="text-base font-semibold">
              {mode === 'balance' ? 'Wallet balance history' : 'Cumulative realized PnL'}
            </h2>
            {mode === 'balance' && balanceGeometry ? (
              <span
                className={`font-mono text-xs font-semibold tabular-nums ${
                  balanceGeometry.delta.positive
                    ? 'text-positive'
                    : balanceGeometry.delta.negative
                      ? 'text-brand-orange'
                      : 'text-content-secondary'
                }`}
              >
                {balanceGeometry.delta.deltaText}
                {balanceGeometry.delta.pctText ? ` (${balanceGeometry.delta.pctText})` : ''}
              </span>
            ) : null}
            {mode === 'pnl' && pnlGeometry ? (
              <span
                className={`font-mono text-xs font-semibold tabular-nums ${
                  pnlGeometry.positive
                    ? 'text-positive'
                    : pnlGeometry.negative
                      ? 'text-brand-orange'
                      : 'text-content-secondary'
                }`}
              >
                {formatSignedAssets(pnlSeries.totalPnlAssets)}
              </span>
            ) : null}
          </div>

          <p className="mt-0.5 text-xs text-content-tertiary">
            {mode === 'balance' ? (
              activeBalancePoint ? (
                <>
                  <span className="font-mono text-content-primary">
                    {formatAssets(activeBalancePoint.pt.balanceAssets)}
                  </span>
                  {activeBalancePoint.stepDeltaAssets && activeBalancePoint.stepDeltaAssets !== '0' ? (
                    <span
                      className={`font-mono ${
                        activeBalancePoint.stepDeltaAssets.startsWith('-')
                          ? 'text-brand-orange'
                          : 'text-positive'
                      }`}
                    >
                      {' '}
                      ({formatSignedAssets(activeBalancePoint.stepDeltaAssets)})
                    </span>
                  ) : null}
                  {activeBalancePoint.pt.netEquityAssets && activeBalancePoint.pt.netEquityAssets !== '0' ? (
                    <span>
                      {' '}
                      · Net equity{' '}
                      <span className="font-mono text-content-primary">
                        {formatAssets(activeBalancePoint.pt.netEquityAssets)}
                      </span>
                    </span>
                  ) : null}
                  <span>
                    {' '}
                    · {activeBalancePoint.pt.eventType} · {formatUtc(activeBalancePoint.pt.occurredAt)}
                  </span>
                </>
              ) : balanceGeometry ? (
                <>
                  Start{' '}
                  <span className="font-mono text-content-secondary">
                    {formatCompactAssets(balanceGeometry.startAssets)}
                  </span>{' '}
                  · Latest{' '}
                  <span className="font-mono text-content-primary">
                    {formatCompactAssets(balanceGeometry.endAssets)}
                  </span>{' '}
                  · Range{' '}
                  <span className="font-mono text-content-secondary">
                    {formatCompactAssets(balanceGeometry.minAssets)} –{' '}
                    {formatCompactAssets(balanceGeometry.maxAssets)}
                  </span>{' '}
                  ·{' '}
                  <span className="font-mono text-content-secondary">
                    {balanceGeometry.coords.length} pts
                  </span>
                </>
              ) : (
                'Canonical settlement & trader-claim balance over time'
              )
            ) : activePnlPoint ? (
              <>
                Cum.{' '}
                <span className="font-mono text-content-primary">
                  {formatSignedAssets(activePnlPoint.pt.cumulativePnlAssets)}
                </span>{' '}
                · Fill{' '}
                <span
                  className={`font-mono ${
                    activePnlPoint.pt.tradePnlAssets.startsWith('-')
                      ? 'text-brand-orange'
                      : 'text-positive'
                  }`}
                >
                  {formatSignedAssets(activePnlPoint.pt.tradePnlAssets)}
                </span>{' '}
                · {activePnlPoint.pt.activityType}
                {activePnlPoint.pt.side ? ` ${activePnlPoint.pt.side.toUpperCase()}` : ''} ·{' '}
                {formatUtc(activePnlPoint.pt.occurredAt)}
              </>
            ) : pnlGeometry ? (
              <>
                Win rate{' '}
                <span className="font-mono text-content-primary">
                  {pnlSeries.wins}W / {pnlSeries.losses}L
                  {pnlGeometry.winRatePct ? ` (${pnlGeometry.winRatePct}%)` : ''}
                </span>
                {pnlSeries.bestPnlAssets ? (
                  <>
                    {' '}
                    · Best{' '}
                    <span className="font-mono text-positive">
                      {formatSignedAssets(pnlSeries.bestPnlAssets)}
                    </span>
                  </>
                ) : null}
                {pnlSeries.worstPnlAssets ? (
                  <>
                    {' '}
                    · Worst{' '}
                    <span className="font-mono text-brand-orange">
                      {formatSignedAssets(pnlSeries.worstPnlAssets)}
                    </span>
                  </>
                ) : null}{' '}
                ·{' '}
                <span className="font-mono text-content-secondary">
                  {pnlSeries.points.length} closes
                </span>
              </>
            ) : (
              'Directional realized PnL across closed positions (excludes cashflows)'
            )}
          </p>
        </div>

        <div className="flex flex-wrap items-center gap-2">
          {hasPnlOption ? (
            <div
              role="group"
              aria-label="Chart metric mode"
              className="inline-flex shrink-0 border border-brand-border/30 bg-app-bg p-0.5"
            >
              <button
                type="button"
                aria-pressed={mode === 'balance'}
                onClick={() => {
                  setMode('balance')
                  setHoveredIndex(null)
                }}
                className={`cursor-pointer px-2.5 py-1 text-xs font-semibold transition-colors ${
                  mode === 'balance'
                    ? 'bg-surface-raised text-content-primary'
                    : 'text-content-secondary hover:text-content-primary'
                }`}
              >
                Balance
              </button>
              <button
                type="button"
                aria-pressed={mode === 'pnl'}
                onClick={() => {
                  setMode('pnl')
                  setHoveredIndex(null)
                }}
                className={`cursor-pointer px-2.5 py-1 text-xs font-semibold transition-colors ${
                  mode === 'pnl'
                    ? 'bg-surface-raised text-content-primary'
                    : 'text-content-secondary hover:text-content-primary'
                }`}
              >
                Cumulative PnL
              </button>
            </div>
          ) : null}

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
      </div>

      {loading ? (
        <div className="p-5">
          <LoadingState rows={3} />
        </div>
      ) : mode === 'balance' ? (
        balanceGeometry && balanceGeometry.coords.length > 0 ? (
          <div ref={chartBoxRef} className="px-4 pb-3 pt-4 sm:px-5">
            <svg
              viewBox={`0 0 ${String(chartWidth)} ${String(CHART_HEIGHT)}`}
              style={{ height: `${String(CHART_HEIGHT)}px` }}
              role="img"
              aria-label={`Wallet balance chart over ${timeframe}`}
              className="block w-full select-none overflow-visible"
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

              {balanceGeometry.xTicks.map((tick, idx) => (
                <line
                  key={`vx-${String(idx)}`}
                  x1={tick.x}
                  x2={tick.x}
                  y1={PAD_TOP}
                  y2={balanceGeometry.plotBottom}
                  stroke="currentColor"
                  strokeOpacity="0.06"
                />
              ))}

              {balanceGeometry.yTicks.map((tick, idx) => (
                <g key={`hy-${String(idx)}`}>
                  <line
                    x1={PAD_LEFT}
                    x2={balanceGeometry.plotRight}
                    y1={tick.y}
                    y2={tick.y}
                    stroke="currentColor"
                    strokeOpacity={idx === 0 || idx === balanceGeometry.yTicks.length - 1 ? '0.14' : '0.09'}
                    strokeDasharray={idx === balanceGeometry.yTicks.length - 1 ? undefined : '3 4'}
                  />
                  <text
                    x={balanceGeometry.plotRight + 10}
                    y={tick.y + 3.5}
                    textAnchor="start"
                    className="fill-content-tertiary font-mono text-[10px]"
                  >
                    {formatCompactAssets(tick.rawAssets)}
                  </text>
                </g>
              ))}

              {balanceGeometry.coords.length >= 2 ? (
                <line
                  x1={PAD_LEFT}
                  x2={balanceGeometry.plotRight}
                  y1={balanceGeometry.startRefY}
                  y2={balanceGeometry.startRefY}
                  stroke="#ffab96"
                  strokeOpacity="0.3"
                  strokeDasharray="4 4"
                />
              ) : null}

              {balanceGeometry.areaPath ? (
                <path d={balanceGeometry.areaPath} fill={`url(#${gradientId})`} />
              ) : null}

              {balanceGeometry.coords.length >= 2 ? (
                <path
                  d={balanceGeometry.linePath}
                  fill="none"
                  stroke={strokeColor}
                  strokeWidth="2.2"
                  strokeLinejoin="round"
                  strokeLinecap="round"
                />
              ) : null}

              {balanceGeometry.coords.length <= 60
                ? balanceGeometry.coords.map((c, idx) => {
                    const isHovered = hoveredIndex === idx
                    const isLast = idx === balanceGeometry.coords.length - 1
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

              {activeBalancePoint ? (
                <g>
                  <line
                    x1={activeBalancePoint.x}
                    x2={activeBalancePoint.x}
                    y1={PAD_TOP}
                    y2={balanceGeometry.plotBottom}
                    stroke={strokeColor}
                    strokeOpacity="0.45"
                    strokeDasharray="2 3"
                  />
                  <line
                    x1={PAD_LEFT}
                    x2={balanceGeometry.plotRight}
                    y1={activeBalancePoint.y}
                    y2={activeBalancePoint.y}
                    stroke={strokeColor}
                    strokeOpacity="0.3"
                    strokeDasharray="2 3"
                  />
                  <circle
                    cx={activeBalancePoint.x}
                    cy={activeBalancePoint.y}
                    r="8"
                    fill={strokeColor}
                    fillOpacity="0.2"
                  />
                  <circle
                    cx={activeBalancePoint.x}
                    cy={activeBalancePoint.y}
                    r="4.5"
                    fill={strokeColor}
                    stroke="#1a0912"
                    strokeWidth="2"
                  />
                </g>
              ) : (
                <g>
                  <circle
                    cx={balanceGeometry.coords[balanceGeometry.coords.length - 1].x}
                    cy={balanceGeometry.coords[balanceGeometry.coords.length - 1].y}
                    r="7"
                    fill={strokeColor}
                    fillOpacity="0.2"
                  />
                  <circle
                    cx={balanceGeometry.coords[balanceGeometry.coords.length - 1].x}
                    cy={balanceGeometry.coords[balanceGeometry.coords.length - 1].y}
                    r="3.8"
                    fill={strokeColor}
                    stroke="#1a0912"
                    strokeWidth="1.6"
                  />
                </g>
              )}

              {balanceGeometry.xTicks.map((tick, idx) => (
                <text
                  key={`xl-${String(idx)}`}
                  x={tick.x}
                  y={CHART_HEIGHT - 6}
                  textAnchor={
                    idx === 0
                      ? 'start'
                      : idx === balanceGeometry.xTicks.length - 1
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
        )
      ) : pnlGeometry && pnlGeometry.coords.length > 0 ? (
        <div ref={chartBoxRef} className="px-4 pb-3 pt-4 sm:px-5">
          <svg
            viewBox={`0 0 ${String(chartWidth)} ${String(CHART_HEIGHT)}`}
            style={{ height: `${String(CHART_HEIGHT)}px` }}
            role="img"
            aria-label={`Cumulative realized PnL chart over ${timeframe}`}
            className="block w-full select-none overflow-visible"
            onPointerMove={handlePointerMove}
            onPointerLeave={() => {
              setHoveredIndex(null)
            }}
          >
            <defs>
              {(() => {
                const plotH = CHART_HEIGHT - PAD_TOP - PAD_BOTTOM
                const splitPct = Math.max(
                  0,
                  Math.min(100, ((pnlGeometry.zeroY - PAD_TOP) / plotH) * 100),
                )
                return (
                  <>
                    <linearGradient
                      id={`${gradientId}-pnl-stroke`}
                      gradientUnits="userSpaceOnUse"
                      x1="0"
                      y1={PAD_TOP}
                      x2="0"
                      y2={PAD_TOP + plotH}
                    >
                      <stop offset="0%" stopColor="#34d399" />
                      <stop offset={`${splitPct.toFixed(3)}%`} stopColor="#34d399" />
                      <stop offset={`${splitPct.toFixed(3)}%`} stopColor="#ff572d" />
                      <stop offset="100%" stopColor="#ff572d" />
                    </linearGradient>
                    <linearGradient
                      id={gradientId}
                      gradientUnits="userSpaceOnUse"
                      x1="0"
                      y1={PAD_TOP}
                      x2="0"
                      y2={PAD_TOP + plotH}
                    >
                      <stop offset="0%" stopColor="#34d399" stopOpacity="0.22" />
                      <stop offset={`${splitPct.toFixed(3)}%`} stopColor="#34d399" stopOpacity="0.02" />
                      <stop offset={`${splitPct.toFixed(3)}%`} stopColor="#ff572d" stopOpacity="0.02" />
                      <stop offset="100%" stopColor="#ff572d" stopOpacity="0.22" />
                    </linearGradient>
                  </>
                )
              })()}
            </defs>

            {pnlGeometry.xTicks.map((tick, idx) => (
              <line
                key={`pvx-${String(idx)}`}
                x1={tick.x}
                x2={tick.x}
                y1={PAD_TOP}
                y2={pnlGeometry.plotBottom}
                stroke="currentColor"
                strokeOpacity="0.06"
              />
            ))}

            {pnlGeometry.yTicks.map((tick, idx) => (
              <g key={`phy-${String(idx)}`}>
                <line
                  x1={PAD_LEFT}
                  x2={pnlGeometry.plotRight}
                  y1={tick.y}
                  y2={tick.y}
                  stroke="currentColor"
                  strokeOpacity={idx === 0 || idx === pnlGeometry.yTicks.length - 1 ? '0.14' : '0.08'}
                  strokeDasharray={idx === pnlGeometry.yTicks.length - 1 ? undefined : '3 4'}
                />
                <text
                  x={pnlGeometry.plotRight + 10}
                  y={tick.y + 3.5}
                  textAnchor="start"
                  className="fill-content-tertiary font-mono text-[10px]"
                >
                  {formatCompactAssets(tick.rawAssets)}
                </text>
              </g>
            ))}

            <line
              x1={PAD_LEFT}
              x2={pnlGeometry.plotRight}
              y1={pnlGeometry.zeroY}
              y2={pnlGeometry.zeroY}
              stroke="#ffab96"
              strokeOpacity="0.38"
              strokeDasharray="4 4"
            />

            {pnlGeometry.coords.map((c, idx) => {
              if (c.barH <= 0) return null
              const barColor = c.tradeVal >= 0 ? '#34d399' : '#ff572d'
              const isHovered = hoveredIndex === idx
              return (
                <rect
                  key={`bar-${c.pt.id}`}
                  x={c.x - 4}
                  y={c.barY}
                  width={8}
                  height={c.barH}
                  fill={barColor}
                  fillOpacity={isHovered ? 0.72 : 0.32}
                />
              )
            })}

            {pnlGeometry.areaPath ? (
              <path d={pnlGeometry.areaPath} fill={`url(#${gradientId})`} />
            ) : null}

            {pnlGeometry.coords.length >= 2 ? (
              <path
                d={pnlGeometry.linePath}
                fill="none"
                stroke={`url(#${gradientId}-pnl-stroke)`}
                strokeWidth="2.2"
                strokeLinejoin="round"
                strokeLinecap="round"
              />
            ) : null}

            {pnlGeometry.coords.map((c, idx) => {
              const isHovered = hoveredIndex === idx
              const isLast = idx === pnlGeometry.coords.length - 1
              if (isHovered || isLast) return null
              return (
                <circle
                  key={`ppt-${c.pt.id}`}
                  cx={c.x}
                  cy={c.y}
                  r={3}
                  fill={c.tradeVal >= 0 ? '#34d399' : '#ff572d'}
                  stroke="#1a0912"
                  strokeWidth="1.2"
                />
              )
            })}

            {activePnlPoint ? (
              <g>
                <line
                  x1={activePnlPoint.x}
                  x2={activePnlPoint.x}
                  y1={PAD_TOP}
                  y2={pnlGeometry.plotBottom}
                  stroke={strokeColor}
                  strokeOpacity="0.45"
                  strokeDasharray="2 3"
                />
                <line
                  x1={PAD_LEFT}
                  x2={pnlGeometry.plotRight}
                  y1={activePnlPoint.y}
                  y2={activePnlPoint.y}
                  stroke={strokeColor}
                  strokeOpacity="0.3"
                  strokeDasharray="2 3"
                />
                <circle
                  cx={activePnlPoint.x}
                  cy={activePnlPoint.y}
                  r="8"
                  fill={strokeColor}
                  fillOpacity="0.2"
                />
                <circle
                  cx={activePnlPoint.x}
                  cy={activePnlPoint.y}
                  r="4.5"
                  fill={strokeColor}
                  stroke="#1a0912"
                  strokeWidth="2"
                />
              </g>
            ) : (
              <g>
                <circle
                  cx={pnlGeometry.coords[pnlGeometry.coords.length - 1].x}
                  cy={pnlGeometry.coords[pnlGeometry.coords.length - 1].y}
                  r="7"
                  fill={strokeColor}
                  fillOpacity="0.2"
                />
                <circle
                  cx={pnlGeometry.coords[pnlGeometry.coords.length - 1].x}
                  cy={pnlGeometry.coords[pnlGeometry.coords.length - 1].y}
                  r="3.8"
                  fill={strokeColor}
                  stroke="#1a0912"
                  strokeWidth="1.6"
                />
              </g>
            )}

            {pnlGeometry.xTicks.map((tick, idx) => (
              <text
                key={`pxl-${String(idx)}`}
                x={tick.x}
                y={CHART_HEIGHT - 6}
                textAnchor={
                  idx === 0
                    ? 'start'
                    : idx === pnlGeometry.xTicks.length - 1
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
            Cumulative directional realized PnL across indexed Close and Liquidation fills (bars
            show relative per-trade wins/losses; deposits and withdrawals are excluded).
          </p>
        </div>
      ) : (
        <EmptyState
          title="No closed trades in this window"
          message="Switch to a wider timeframe (90D or ALL) or view the Balance chart to inspect deposits and transfers."
        />
      )}
    </Panel>
  )
}
