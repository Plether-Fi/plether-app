import { useId, useMemo, useState } from 'react'
import {
  classifyLiquidationBuffer,
  formatLeverageX100,
  rawToDisplayPrice,
  type PositionLifecycleDetail,
  type PositionPricePoint,
  type PositionProtectionEvent,
  type PositionStepEvent,
} from '../api/positions'
import { useContainerWidth } from '../hooks/useContainerWidth'
import {
  formatCompactAssets,
  formatPrice,
  formatSignedAssets,
  formatUtc,
} from '../utils/format'
import { EmptyState, Panel } from './ui'

interface PositionLifecycleChartProps {
  position: PositionLifecycleDetail
  priceSeries: PositionPricePoint[]
  events: PositionStepEvent[]
  protections: PositionProtectionEvent[]
}

type LifecycleChartMode = 'price' | 'pnl' | 'leverage'

const DEFAULT_WIDTH = 1040
const HEIGHT = 260
const PAD_LEFT = 12
const PAD_RIGHT = 74
const PAD_TOP = 18
const PAD_BOTTOM = 28

function parseUsdg(raw: string): number {
  const n = Number(raw)
  return Number.isFinite(n) ? n / 1_000_000 : 0
}

function shortTime(unixSeconds: number, showDate: boolean): string {
  const d = new Date(unixSeconds * 1000)
  if (showDate) {
    return d.toLocaleDateString('en-US', {
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
      hour12: false,
      timeZone: 'UTC',
    })
  }
  return d.toLocaleTimeString('en-US', {
    hour: '2-digit',
    minute: '2-digit',
    hour12: false,
    timeZone: 'UTC',
  })
}

function actionDotColor(action: string): string {
  if (action === 'Open' || action === 'Scale-in') return '#34d399'
  if (action === 'Add margin') return '#f5c451'
  if (action === 'Liquidated') return '#ff572d'
  return '#ffab96'
}

function extractProtectionLevels(protections: PositionProtectionEvent[]): {
  tpPrice: number | null
  slPrice: number | null
} {
  let tpPrice: number | null = null
  let slPrice: number | null = null
  for (const ev of protections) {
    const snap = ev.snapshot
    const triggerRaw =
      typeof snap.triggerPrice === 'string'
        ? snap.triggerPrice
        : typeof snap.triggerPrice === 'number'
          ? String(snap.triggerPrice)
          : null
    const display = rawToDisplayPrice(triggerRaw)
    if (display == null) continue
    const kind = typeof snap.kind === 'string' ? snap.kind.toLowerCase() : ''
    if (kind.includes('profit') || kind === 'tp' || kind === '0') {
      tpPrice = display
    } else if (kind.includes('loss') || kind === 'sl' || kind === '1') {
      slPrice = display
    }
  }
  return { tpPrice, slPrice }
}

export function PositionLifecycleChart({
  position,
  priceSeries,
  events,
  protections,
}: PositionLifecycleChartProps) {
  const [mode, setMode] = useState<LifecycleChartMode>('price')
  const [hoveredIdx, setHoveredIdx] = useState<number | null>(null)
  const [chartBoxRef, measuredWidth] = useContainerWidth(DEFAULT_WIDTH)
  const gradBaseId = useId().replace(/:/g, '')
  const chartWidth = Math.max(320, measuredWidth)

  const plotW = chartWidth - PAD_LEFT - PAD_RIGHT
  const plotH = HEIGHT - PAD_TOP - PAD_BOTTOM

  const series = useMemo(() => {
    if (priceSeries.length > 0) return priceSeries
    // Fallback to fill events if candle series is empty
    return events
      .filter((ev) => ev.price != null)
      .map((ev) => ({
        timestamp: ev.timestamp,
        occurredAt: ev.occurredAt,
        rawPrice: ev.price ?? position.entryPrice,
        rawHigh: ev.price ?? position.entryPrice,
        rawLow: ev.price ?? position.entryPrice,
        unrealizedPnlAssets: ev.pnlAssets ?? '0',
        positionSize: ev.resultingSize,
        leverageX100: ev.resultingLeverageX100,
        liquidationPrice: ev.resultingLiquidationPrice ?? position.liquidationPrice,
        distanceToLiquidationBps: null,
      }))
  }, [priceSeries, events, position])

  const entryDisplay = rawToDisplayPrice(position.entryPrice)
  const liqDisplay = rawToDisplayPrice(position.liquidationPrice)
  const mfeDisplay = rawToDisplayPrice(position.mfePrice)
  const maeDisplay = rawToDisplayPrice(position.maePrice)
  const { tpPrice, slPrice } = useMemo(
    () => extractProtectionLevels(protections),
    [protections],
  )

  const chart = useMemo(() => {
    if (series.length === 0) return null
    const minTs = series[0].timestamp
    const maxTs = series[series.length - 1].timestamp
    const spanTs = Math.max(1, maxTs - minTs)
    const showDate = spanTs >= 18 * 3600

    const xOf = (ts: number) =>
      series.length === 1
        ? PAD_LEFT + plotW / 2
        : PAD_LEFT + Math.max(0, Math.min(1, (ts - minTs) / spanTs)) * plotW

    if (mode === 'price') {
      const prices = series.map((pt) => rawToDisplayPrice(pt.rawPrice) ?? entryDisplay ?? 1)
      const highs = series.map((pt) => rawToDisplayPrice(pt.rawLow) ?? rawToDisplayPrice(pt.rawPrice) ?? 1)
      const lows = series.map((pt) => rawToDisplayPrice(pt.rawHigh) ?? rawToDisplayPrice(pt.rawPrice) ?? 1)
      const coreValues = [
        ...prices,
        ...highs,
        ...lows,
        ...(entryDisplay != null ? [entryDisplay] : []),
        ...(mfeDisplay != null ? [mfeDisplay] : []),
        ...(maeDisplay != null ? [maeDisplay] : []),
      ]
      let minP = Math.min(...coreValues)
      let maxP = Math.max(...coreValues)
      const baseSpan = Math.max(maxP - minP, 0.002)

      // Include TP/SL/Liq lines on chart only if reasonably near price action (keeps curve readable!)
      const liqPlotVal =
        liqDisplay != null &&
        liqDisplay >= minP - baseSpan * 1.1 &&
        liqDisplay <= maxP + baseSpan * 1.1
          ? liqDisplay
          : null
      const tpPlotVal =
        tpPrice != null && tpPrice >= minP - baseSpan * 1.1 && tpPrice <= maxP + baseSpan * 1.1
          ? tpPrice
          : null
      const slPlotVal =
        slPrice != null && slPrice >= minP - baseSpan * 1.1 && slPrice <= maxP + baseSpan * 1.1
          ? slPrice
          : null

      if (liqPlotVal != null) {
        minP = Math.min(minP, liqPlotVal)
        maxP = Math.max(maxP, liqPlotVal)
      }
      if (tpPlotVal != null) {
        minP = Math.min(minP, tpPlotVal)
        maxP = Math.max(maxP, tpPlotVal)
      }
      if (slPlotVal != null) {
        minP = Math.min(minP, slPlotVal)
        maxP = Math.max(maxP, slPlotVal)
      }

      const span = Math.max(maxP - minP, 0.002)
      const padMin = minP - span * 0.14
      const padMax = maxP + span * 0.14
      const yOf = (p: number) => PAD_TOP + plotH - ((p - padMin) / (padMax - padMin)) * plotH

      const coords = series.map((pt, i) => ({
        x: xOf(pt.timestamp),
        y: yOf(prices[i]),
        yHigh: yOf(highs[i]),
        yLow: yOf(lows[i]),
        pt,
      }))

      const linePath = coords
        .map((c, i) => `${i === 0 ? 'M' : 'L'}${c.x.toFixed(1)},${c.y.toFixed(1)}`)
        .join(' ')

      const bandPath =
        coords.length > 1
          ? [
              ...coords.map((c, i) => `${i === 0 ? 'M' : 'L'}${c.x.toFixed(1)},${c.yHigh.toFixed(1)}`),
              ...coords
                .slice()
                .reverse()
                .map((c) => `L${c.x.toFixed(1)},${c.yLow.toFixed(1)}`),
              'Z',
            ].join(' ')
          : ''

      const eventDots = events
        .map((ev) => {
          let disp = rawToDisplayPrice(ev.price)
          if (disp == null && series.length > 0) {
            // Place price-less events (e.g. Add margin) on the live mark curve at their timestamp
            let nearest = series[0]
            let bestDiff = Math.abs(nearest.timestamp - ev.timestamp)
            for (const pt of series) {
              const diff = Math.abs(pt.timestamp - ev.timestamp)
              if (diff < bestDiff) {
                bestDiff = diff
                nearest = pt
              }
            }
            disp = rawToDisplayPrice(nearest.rawPrice)
          }
          const finalDisp = disp ?? entryDisplay
          if (finalDisp == null) return null
          return {
            ev,
            x: xOf(ev.timestamp),
            y: yOf(finalDisp),
            color: actionDotColor(ev.action),
          }
        })
        .filter((d): d is NonNullable<typeof d> => d !== null)

      const refLinesRaw: { y: number; label: string; color: string; dashed?: boolean }[] = []
      if (entryDisplay != null) {
        refLinesRaw.push({
          y: yOf(entryDisplay),
          label: `Entry ${entryDisplay.toFixed(4)}`,
          color: '#ffab96',
          dashed: true,
        })
      }
      if (liqPlotVal != null) {
        refLinesRaw.push({
          y: yOf(liqPlotVal),
          label: `Liq ${liqPlotVal.toFixed(4)}`,
          color: '#ff572d',
          dashed: true,
        })
      }
      if (tpPlotVal != null) {
        refLinesRaw.push({
          y: yOf(tpPlotVal),
          label: `TP ${tpPlotVal.toFixed(4)}`,
          color: '#34d399',
          dashed: true,
        })
      }
      if (slPlotVal != null) {
        refLinesRaw.push({
          y: yOf(slPlotVal),
          label: `SL ${slPlotVal.toFixed(4)}`,
          color: '#f5c451',
          dashed: true,
        })
      }

      // De-collide left-aligned reference text labels vertically (min 13px spacing)
      // while preserving exact dashed line positions.
      const sortedRef = [...refLinesRaw].sort((a, b) => a.y - b.y)
      const refLines: { y: number; labelY: number; label: string; color: string; dashed?: boolean }[] = []
      let prevLabelY = PAD_TOP - 4
      for (const rl of sortedRef) {
        const desiredY = Math.max(PAD_TOP + 10, rl.y - 4)
        const labelY = Math.min(PAD_TOP + plotH - 2, Math.max(desiredY, prevLabelY + 13))
        prevLabelY = labelY
        refLines.push({ ...rl, labelY })
      }

      const yTicks = [0, 0.5, 1].map((frac) => {
        const val = padMin + (padMax - padMin) * frac
        return { y: yOf(val), label: val.toFixed(4) }
      })

      const entryY = entryDisplay != null ? yOf(entryDisplay) : null
      const areaPath =
        entryY != null && coords.length > 1
          ? [
              `M${coords[0].x.toFixed(1)},${entryY.toFixed(1)}`,
              ...coords.map((c) => `L${c.x.toFixed(1)},${c.y.toFixed(1)}`),
              `L${coords[coords.length - 1].x.toFixed(1)},${entryY.toFixed(1)}`,
              'Z',
            ].join(' ')
          : ''

      return {
        coords,
        linePath,
        areaPath,
        bandPath,
        eventDots,
        refLines,
        yTicks,
        showDate,
        zeroY: null as number | null,
        splitBaselineY: entryY,
      }
    }

    if (mode === 'pnl') {
      const vals = series.map((pt) => parseUsdg(pt.unrealizedPnlAssets))
      const minV = Math.min(...vals, 0)
      const maxV = Math.max(...vals, 0)
      const span = Math.max(maxV - minV, 10)
      const padMin = minV - span * 0.14
      const padMax = maxV + span * 0.14
      const yOf = (v: number) => PAD_TOP + plotH - ((v - padMin) / (padMax - padMin)) * plotH
      const zeroY = yOf(0)

      const coords = series.map((pt, i) => ({
        x: xOf(pt.timestamp),
        y: yOf(vals[i]),
        yHigh: yOf(vals[i]),
        yLow: yOf(vals[i]),
        pt,
      }))
      const linePath = coords
        .map((c, i) => `${i === 0 ? 'M' : 'L'}${c.x.toFixed(1)},${c.y.toFixed(1)}`)
        .join(' ')
      const areaPath =
        coords.length > 1
          ? [
              `M${coords[0].x.toFixed(1)},${zeroY.toFixed(1)}`,
              ...coords.map((c) => `L${c.x.toFixed(1)},${c.y.toFixed(1)}`),
              `L${coords[coords.length - 1].x.toFixed(1)},${zeroY.toFixed(1)}`,
              'Z',
            ].join(' ')
          : ''

      const yTicks = [padMax, 0, padMin].map((v) => ({
        y: yOf(v),
        label: `${v > 0 ? '+' : ''}${v.toFixed(0)}`,
      }))

      return {
        coords,
        linePath,
        areaPath,
        bandPath: '',
        eventDots: [],
        refLines: [],
        yTicks,
        showDate,
        zeroY,
        splitBaselineY: zeroY,
      }
    }

    // Leverage mode
    const vals = series.map((pt) => pt.leverageX100 / 100)
    const maxLev = Math.max(...vals, 5)
    const ceiling = Math.ceil((maxLev * 1.15) / 5) * 5
    const yOf = (v: number) => PAD_TOP + plotH - (v / Math.max(ceiling, 1)) * plotH

    const coords = series.map((pt, i) => ({
      x: xOf(pt.timestamp),
      y: yOf(vals[i]),
      yHigh: yOf(vals[i]),
      yLow: yOf(vals[i]),
      pt,
    }))
    let linePath = `M${coords[0].x.toFixed(1)},${coords[0].y.toFixed(1)}`
    for (let i = 1; i < coords.length; i += 1) {
      linePath += ` H${coords[i].x.toFixed(1)} V${coords[i].y.toFixed(1)}`
    }
    const yTicks = [ceiling, ceiling / 2, 0].map((v) => ({
      y: yOf(v),
      label: `${v.toFixed(0)}×`,
    }))

    return {
      coords,
      linePath,
      areaPath: '',
      bandPath: '',
      eventDots: [],
      refLines: [],
      yTicks,
      showDate,
      zeroY: yOf(0),
      splitBaselineY: null as number | null,
    }
  }, [series, events, mode, entryDisplay, liqDisplay, mfeDisplay, maeDisplay, tpPrice, slPrice, plotW, plotH])

  const activeIdx =
    hoveredIdx !== null && hoveredIdx >= 0 && hoveredIdx < series.length
      ? hoveredIdx
      : series.length - 1
  const activePt = series.at(activeIdx)
  const activeLiqBuf = classifyLiquidationBuffer(activePt?.distanceToLiquidationBps)

  return (
    <Panel className="p-5 sm:p-6">
      <div className="flex flex-col justify-between gap-3 sm:flex-row sm:items-start">
        <div>
          <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
            Position Price Path &amp; Risk Trajectory
          </p>
          <p className="mt-1 text-xs text-content-tertiary">
            {mode === 'price'
              ? 'Real indexed plDXY oracle mark curve with fill markers, entry level, and liquidation / TP-SL boundaries'
              : mode === 'pnl'
                ? 'Mark-to-market unrealized PnL progression (USDG) over the position lifespan'
                : 'Effective position leverage (×) across scale-ins, margin additions, and price moves'}
          </p>
        </div>

        <div className="flex items-center gap-1" role="group" aria-label="Position chart view">
          {(
            [
              ['price', 'Price & Levels'],
              ['pnl', 'Unrealized PnL'],
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

      {!chart || !activePt ? (
        <div className="mt-4">
          <EmptyState
            title="Price path unavailable"
            message="No historical basket oracle observations overlapped this position window."
          />
        </div>
      ) : (
        <div className="mt-4 space-y-3">
          {/* Calm single-line readout bar */}
          <div className="flex flex-wrap items-center justify-between gap-3 border border-brand-border/20 bg-app-bg px-3.5 py-2 text-xs">
            <span className="font-mono text-content-tertiary">
              {formatUtc(activePt.occurredAt)}
            </span>
            <div className="flex flex-wrap items-center gap-4 font-mono tabular-nums">
              <span>
                <span className="text-content-tertiary">Mark </span>
                <span className="font-semibold text-content-primary">
                  {formatPrice(activePt.rawPrice)}
                </span>
              </span>
              <span>
                <span className="text-content-tertiary">uPnL </span>
                <span
                  className={
                    parseUsdg(activePt.unrealizedPnlAssets) >= 0
                      ? 'font-semibold text-positive'
                      : 'font-semibold text-brand-orange'
                  }
                >
                  {formatSignedAssets(activePt.unrealizedPnlAssets)}
                </span>
              </span>
              <span>
                <span className="text-content-tertiary">Lev </span>
                <span className="font-semibold text-brand-peach">
                  {formatLeverageX100(activePt.leverageX100)}
                </span>
              </span>
              <span>
                <span className="text-content-tertiary">Liq buf </span>
                <span className="text-content-primary">{activeLiqBuf.text}</span>
              </span>
            </div>
          </div>

          <div ref={chartBoxRef} className="w-full border border-brand-border/20 bg-app-bg p-2">
            {(() => {
              const splitY = chart.splitBaselineY
              const hasSplit = splitY != null && (mode === 'pnl' || mode === 'price')
              const splitPct =
                splitY != null
                  ? Math.max(0, Math.min(100, ((splitY - PAD_TOP) / plotH) * 100))
                  : 50
              const aboveColor =
                mode === 'price' && position.side === 'short' ? '#ff572d' : '#34d399'
              const belowColor =
                mode === 'price' && position.side === 'short' ? '#34d399' : '#ff572d'
              const strokeGradId = `${gradBaseId}-stroke`
              const fillGradId = `${gradBaseId}-fill`
              const activeDotColor =
                mode === 'leverage'
                  ? '#ffab96'
                  : parseUsdg(activePt.unrealizedPnlAssets) < 0
                    ? '#ff572d'
                    : '#34d399'

              return (
                <svg
                  viewBox={`0 0 ${String(chartWidth)} ${String(HEIGHT)}`}
                  style={{ height: `${String(HEIGHT)}px` }}
                  className="block w-full overflow-visible"
                  role="img"
                  aria-label={`Position ${mode} trajectory chart`}
                  onMouseLeave={() => { setHoveredIdx(null) }}
                >
                  {hasSplit ? (
                    <defs>
                      <linearGradient
                        id={strokeGradId}
                        gradientUnits="userSpaceOnUse"
                        x1="0"
                        y1={PAD_TOP}
                        x2="0"
                        y2={PAD_TOP + plotH}
                      >
                        <stop offset="0%" stopColor={aboveColor} />
                        <stop offset={`${splitPct.toFixed(3)}%`} stopColor={aboveColor} />
                        <stop offset={`${splitPct.toFixed(3)}%`} stopColor={belowColor} />
                        <stop offset="100%" stopColor={belowColor} />
                      </linearGradient>
                      <linearGradient
                        id={fillGradId}
                        gradientUnits="userSpaceOnUse"
                        x1="0"
                        y1={PAD_TOP}
                        x2="0"
                        y2={PAD_TOP + plotH}
                      >
                        <stop offset="0%" stopColor={aboveColor} stopOpacity="0.22" />
                        <stop offset={`${splitPct.toFixed(3)}%`} stopColor={aboveColor} stopOpacity="0.02" />
                        <stop offset={`${splitPct.toFixed(3)}%`} stopColor={belowColor} stopOpacity="0.02" />
                        <stop offset="100%" stopColor={belowColor} stopOpacity="0.24" />
                      </linearGradient>
                    </defs>
                  ) : null}

                  {/* Subtle Y gridlines */}
                  {chart.yTicks.map((tick, i) => (
                    <g key={`yt-${String(i)}`}>
                      <line
                        x1={PAD_LEFT}
                        y1={tick.y}
                        x2={chartWidth - PAD_RIGHT}
                        y2={tick.y}
                        stroke="currentColor"
                        className="text-brand-border/20"
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

                  {/* Zero line in PnL mode */}
                  {chart.zeroY != null ? (
                    <line
                      x1={PAD_LEFT}
                      y1={chart.zeroY}
                      x2={chartWidth - PAD_RIGHT}
                      y2={chart.zeroY}
                      stroke="#9a8c8f"
                      strokeOpacity={0.45}
                      strokeDasharray="4 4"
                    />
                  ) : null}

                  {/* Reference price levels (Entry / Liq / TP / SL) */}
                  {chart.refLines.map((rl) => (
                    <g key={rl.label}>
                      <line
                        x1={PAD_LEFT}
                        y1={rl.y}
                        x2={chartWidth - PAD_RIGHT}
                        y2={rl.y}
                        stroke={rl.color}
                        strokeOpacity={0.65}
                        strokeDasharray={rl.dashed ? '5 4' : undefined}
                        strokeWidth={1.25}
                      />
                      <text
                        x={PAD_LEFT + 6}
                        y={rl.labelY}
                        fill={rl.color}
                        className="font-mono text-[10px] font-semibold"
                      >
                        {rl.label}
                      </text>
                    </g>
                  ))}

                  {/* High/Low candle band */}
                  {chart.bandPath ? (
                    <path d={chart.bandPath} fill="#ffab96" fillOpacity={0.09} />
                  ) : null}

                  {/* Translucent split green / orange area to baseline */}
                  {hasSplit && chart.areaPath ? (
                    <path d={chart.areaPath} fill={`url(#${fillGradId})`} />
                  ) : null}

                  {/* Primary line: split green above profit threshold / brand orange below */}
                  <path
                    d={chart.linePath}
                    fill="none"
                    stroke={hasSplit ? `url(#${strokeGradId})` : '#ffab96'}
                    strokeWidth={2}
                    strokeLinejoin="round"
                  />

                  {/* Event markers (Open / Scale-in / Margin + / Partial Close / Close / Liq) */}
                  {chart.eventDots.map((dot) => (
                    <g key={dot.ev.id}>
                      <circle
                        cx={dot.x}
                        cy={dot.y}
                        r={5.5}
                        fill={dot.color}
                        stroke="#14080e"
                        strokeWidth={1.75}
                      >
                        <title>
                          {`${dot.ev.action} @ ${formatPrice(dot.ev.price)} (${formatUtc(dot.ev.occurredAt)})`}
                        </title>
                      </circle>
                    </g>
                  ))}

                  {/* Active crosshair & hover columns */}
                  {chart.coords[activeIdx] ? (
                    <g>
                      <line
                        x1={chart.coords[activeIdx].x}
                        y1={PAD_TOP}
                        x2={chart.coords[activeIdx].x}
                        y2={PAD_TOP + plotH}
                        stroke="#ffab96"
                        strokeOpacity={0.45}
                        strokeDasharray="3 3"
                      />
                      <circle
                        cx={chart.coords[activeIdx].x}
                        cy={chart.coords[activeIdx].y}
                        r={4.5}
                        fill={activeDotColor}
                        stroke="#14080e"
                        strokeWidth={1.5}
                      />
                    </g>
                  ) : null}

                  {chart.coords.map((c, idx) => (
                    <rect
                      key={`hit-${String(idx)}`}
                      x={c.x - Math.max(4, plotW / Math.max(chart.coords.length * 2, 1))}
                      y={PAD_TOP}
                      width={Math.max(8, plotW / Math.max(chart.coords.length, 1))}
                      height={plotH}
                      fill="transparent"
                      className="cursor-crosshair"
                      onMouseEnter={() => { setHoveredIdx(idx) }}
                    />
                  ))}

                  {/* X-axis timestamps */}
                  {[0, Math.floor((chart.coords.length - 1) / 2), chart.coords.length - 1]
                    .filter((idx, i, arr) => arr.indexOf(idx) === i && idx >= 0 && idx < chart.coords.length)
                    .map((idx) => {
                      const c = chart.coords[idx]
                      return (
                        <text
                          key={`xt-${String(idx)}`}
                          x={c.x}
                          y={HEIGHT - 8}
                          textAnchor={idx === 0 ? 'start' : idx === chart.coords.length - 1 ? 'end' : 'middle'}
                          className="fill-content-tertiary font-mono text-[10px]"
                        >
                          {shortTime(c.pt.timestamp, chart.showDate)}
                        </text>
                      )
                    })}
                </svg>
              )
            })()}
          </div>

          {/* Clean compact legend */}
          <div className="flex flex-wrap items-center justify-between gap-2 text-[11px] text-content-tertiary">
            <div className="flex flex-wrap items-center gap-4">
              <span className="inline-flex items-center gap-1.5">
                <span className="inline-block h-2.5 w-2.5 rounded-full bg-positive" />
                Open / Scale-in
              </span>
              <span className="inline-flex items-center gap-1.5">
                <span className="inline-block h-2.5 w-2.5 rounded-full bg-brand-yellow" />
                Add margin
              </span>
              <span className="inline-flex items-center gap-1.5">
                <span className="inline-block h-2.5 w-2.5 rounded-full bg-brand-peach" />
                Partial / Close
              </span>
              <span className="inline-flex items-center gap-1.5">
                <span className="inline-block h-2.5 w-2.5 rounded-full bg-brand-orange" />
                Liquidated
              </span>
            </div>
            <span className="font-mono">
              Peak notional {formatCompactAssets(position.peakNotionalAssets)}
            </span>
          </div>
        </div>
      )}
    </Panel>
  )
}
