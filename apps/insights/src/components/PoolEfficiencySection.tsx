import { useMemo, useState } from 'react'
import { InsightsApiError } from '../api/client'
import {
  formatPoolBpsPercent,
  formatTakeRateBpsX100,
  formatTurnoverX100,
  type LiquidityEfficiencyResponse,
  type PoolEfficiencyBucket,
} from '../api/liquidity'
import { formatCompactAssets, formatSignedAssets } from '../utils/format'
import { EmptyState, ErrorState, LoadingState, Panel } from './ui'

type EfficiencyWindow = '7d' | '30d'

function parseAssetsNum(raw: string | null | undefined): number {
  if (!raw || !/^-?\d+$/.test(raw)) return 0
  const n = Number(raw) / 1_000_000
  return Number.isFinite(n) ? n : 0
}

function formatCompactDollars(dollars: number): string {
  const abs = Math.abs(dollars)
  const sign = dollars < 0 ? '−' : ''
  if (abs >= 1_000_000_000) return `${sign}${(abs / 1_000_000_000).toFixed(2)}B`
  if (abs >= 1_000_000) return `${sign}${(abs / 1_000_000).toFixed(2)}M`
  if (abs >= 1_000) return `${sign}${(abs / 1_000).toFixed(1)}K`
  return `${sign}${abs.toFixed(0)}`
}

function formatBucketLabel(iso: string, window: EfficiencyWindow): string {
  const date = new Date(iso)
  if (Number.isNaN(date.getTime())) return iso
  const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
  const day = String(date.getUTCDate()).padStart(2, '0')
  const mon = months[date.getUTCMonth()]
  if (window === '7d') {
    const hh = String(date.getUTCHours()).padStart(2, '0')
    return `${day} ${mon} ${hh}:00`
  }
  return `${day} ${mon}`
}

function TurnoverVolumeChart({
  buckets,
  window,
  avgTurnoverX100,
}: {
  buckets: PoolEfficiencyBucket[]
  window: EfficiencyWindow
  avgTurnoverX100: number | null
}) {
  const [hoveredIdx, setHoveredIdx] = useState<number | null>(null)

  const chart = useMemo(() => {
    const W = 600
    const H = 224
    const padL = 52
    const padR = 54
    const padT = 18
    const padB = 28
    const plotW = W - padL - padR
    const plotH = H - padT - padB

    const volumes = buckets.map((b) => parseAssetsNum(b.volumeAssets))
    const turnovers = buckets.map((b) =>
      b.annualizedTurnoverDailyX100 != null ? b.annualizedTurnoverDailyX100 / 100 : null,
    )
    const maxVol = Math.max(1, ...volumes)
    const validTurnovers = turnovers.filter((v): v is number => v != null)
    const avgTurn = avgTurnoverX100 != null ? avgTurnoverX100 / 100 : null
    const maxTurn = Math.max(
      1,
      ...validTurnovers,
      avgTurn ?? 0,
    ) * 1.12

    const count = Math.max(1, buckets.length)
    const stepX = plotW / count
    const barW = Math.max(3, Math.min(18, stepX * 0.56))

    const points = buckets.map((b, i) => {
      const cx = padL + stepX * i + stepX / 2
      const vol = volumes[i]
      const barH = (vol / maxVol) * plotH
      const barY = padT + plotH - barH
      const turn = turnovers[i]
      const turnY = turn != null ? padT + plotH - (turn / maxTurn) * plotH : null
      return { cx, barX: cx - barW / 2, barY, barH, turn, turnY, bucket: b }
    })

    const linePts = points.filter((p): p is typeof p & { turnY: number } => p.turnY != null)
    const linePath = linePts
      .map((p, idx) => `${idx === 0 ? 'M' : 'L'}${p.cx.toFixed(1)},${p.turnY.toFixed(1)}`)
      .join(' ')
    const areaPath =
      linePts.length >= 2
        ? `${linePath} L${linePts[linePts.length - 1].cx.toFixed(1)},${String(padT + plotH)} L${linePts[0].cx.toFixed(1)},${String(padT + plotH)} Z`
        : ''
    const avgY = avgTurn != null ? padT + plotH - (avgTurn / maxTurn) * plotH : null

    return { W, H, padL, padR, padT, padB, plotW, plotH, barW, maxVol, maxTurn, points, linePath, areaPath, avgY, avgTurn }
  }, [buckets, avgTurnoverX100])

  const activeIdx = hoveredIdx != null && hoveredIdx < buckets.length ? hoveredIdx : buckets.length - 1
  const active = buckets.at(activeIdx)
  const activePt = chart.points.at(activeIdx)

  return (
    <Panel className="flex flex-col p-4 sm:p-5">
      <div>
        <h3 className="text-sm font-semibold text-content-primary">
          Pool Turnover (Vol / Pool) &amp; Volume
        </h3>
        <p className="mt-0.5 text-[11px] text-content-tertiary">
          {window === '7d'
            ? '6h traded volume bars · annualized daily turnover multiple (×/day)'
            : 'Daily traded volume bars · daily Volume / Pool TVL multiple (×/day)'}
        </p>
        <div className="mt-2 flex flex-wrap items-center gap-x-3 gap-y-1 font-mono text-[10px] text-content-tertiary">
          <span className="inline-flex items-center gap-1">
            <span className="h-2 w-2 bg-positive" /> Turnover (×/d)
          </span>
          <span className="inline-flex items-center gap-1">
            <span className="h-2 w-2 bg-brand-peach/45" /> Volume
          </span>
          <span className="inline-flex items-center gap-1">
            <span className="h-0.5 w-3 border-t border-dashed border-brand-peach" /> Period avg
          </span>
        </div>
      </div>

      {active ? (
        <div className="mt-3 flex flex-wrap items-center justify-between gap-x-3 gap-y-1 border border-brand-border/20 bg-app-bg/60 px-3 py-2 font-mono text-[11px] tabular-nums">
          <span className="whitespace-nowrap text-content-tertiary">
            {formatBucketLabel(active.bucketStart, window)} UTC
          </span>
          <div className="flex flex-wrap items-center gap-x-2.5 gap-y-0.5 whitespace-nowrap">
            <span className="font-semibold text-positive">
              {formatTurnoverX100(active.annualizedTurnoverDailyX100)}/d
            </span>
            <span className="text-content-primary">
              Vol {formatCompactAssets(active.volumeAssets)}
            </span>
            <span className="text-content-secondary">
              Pool {active.poolTvlAssets ? formatCompactAssets(active.poolTvlAssets) : '—'}
            </span>
            {active.takeRateBpsX100 != null ? (
              <span className="text-brand-peach">
                {formatTakeRateBpsX100(active.takeRateBpsX100)}
              </span>
            ) : null}
          </div>
        </div>
      ) : null}

      <div className="mt-auto pt-3">
        <svg
          viewBox={`0 0 ${String(chart.W)} ${String(chart.H)}`}
          className="h-auto w-full select-none"
          role="img"
          aria-label="Pool turnover multiple and traded volume over time"
          onMouseLeave={() => {
            setHoveredIdx(null)
          }}
        >
          <defs>
            <linearGradient id="turnover-area-grad" x1="0" y1="0" x2="0" y2="1">
              <stop offset="0%" stopColor="#00ff99" stopOpacity="0.22" />
              <stop offset="100%" stopColor="#00ff99" stopOpacity="0.01" />
            </linearGradient>
          </defs>

          {/* Horizontal gridlines + Y labels */}
          {[0, 0.5, 1].map((frac) => {
            const y = chart.padT + chart.plotH * (1 - frac)
            const turnLbl = `${(chart.maxTurn * frac).toFixed(chart.maxTurn >= 20 ? 0 : 1)}×`
            const volLbl = formatCompactDollars(chart.maxVol * frac)
            return (
              <g key={frac}>
                <line
                  x1={chart.padL}
                  y1={y}
                  x2={chart.W - chart.padR}
                  y2={y}
                  stroke="#ffab96"
                  strokeOpacity={frac === 0 ? 0.22 : 0.1}
                  strokeWidth="1"
                />
                <text
                  x={chart.padL - 8}
                  y={y + 3.5}
                  textAnchor="end"
                  fill="#00ff99"
                  fillOpacity="0.85"
                  fontFamily="ui-monospace, SFMono-Regular, monospace"
                  fontSize="10"
                >
                  {turnLbl}
                </text>
                <text
                  x={chart.W - chart.padR + 8}
                  y={y + 3.5}
                  textAnchor="start"
                  fill="#aa98a0"
                  fontFamily="ui-monospace, SFMono-Regular, monospace"
                  fontSize="10"
                >
                  {volLbl}
                </text>
              </g>
            )
          })}

          {/* Average turnover reference line */}
          {chart.avgY != null && chart.avgTurn != null ? (
            <g>
              <line
                x1={chart.padL}
                y1={chart.avgY}
                x2={chart.W - chart.padR}
                y2={chart.avgY}
                stroke="#ffab96"
                strokeOpacity="0.45"
                strokeWidth="1"
                strokeDasharray="4 4"
              />
              <text
                x={chart.padL + 4}
                y={Math.max(chart.padT + 10, chart.avgY - 4)}
                fill="#ffab96"
                fontFamily="ui-monospace, SFMono-Regular, monospace"
                fontSize="9.5"
              >
                Avg {chart.avgTurn >= 100 ? chart.avgTurn.toFixed(1) : chart.avgTurn.toFixed(2)}×
              </text>
            </g>
          ) : null}

          {/* Volume bars */}
          {chart.points.map((pt, i) => (
            <rect
              key={pt.bucket.bucketStart}
              x={pt.barX}
              y={pt.barY}
              width={chart.barW}
              height={Math.max(0, pt.barH)}
              fill="#ffab96"
              fillOpacity={i === activeIdx ? 0.62 : 0.28}
            />
          ))}

          {/* Turnover area + line */}
          {chart.areaPath ? <path d={chart.areaPath} fill="url(#turnover-area-grad)" /> : null}
          {chart.linePath ? (
            <path
              d={chart.linePath}
              fill="none"
              stroke="#00ff99"
              strokeWidth="2.2"
              strokeLinejoin="round"
              strokeLinecap="round"
            />
          ) : null}

          {/* Active hover crosshair & point */}
          {activePt ? (
            <g>
              <line
                x1={activePt.cx}
                y1={chart.padT}
                x2={activePt.cx}
                y2={chart.padT + chart.plotH}
                stroke="#fff5f9"
                strokeOpacity="0.25"
                strokeWidth="1"
                strokeDasharray="2 2"
              />
              {activePt.turnY != null ? (
                <>
                  <circle cx={activePt.cx} cy={activePt.turnY} r="6" fill="#00ff99" fillOpacity="0.24" />
                  <circle cx={activePt.cx} cy={activePt.turnY} r="3.2" fill="#00ff99" />
                </>
              ) : null}
            </g>
          ) : null}

          {/* X-axis ticks (start, mid, end) */}
          {buckets.length > 0
            ? [0, Math.floor((buckets.length - 1) / 2), buckets.length - 1]
                .filter((v, idx, arr) => arr.indexOf(v) === idx)
                .map((idx) => {
                  const pt = chart.points.at(idx)
                  if (!pt) return null
                  return (
                    <text
                      key={idx}
                      x={pt.cx}
                      y={chart.H - 8}
                      textAnchor={idx === 0 ? 'start' : idx === buckets.length - 1 ? 'end' : 'middle'}
                      fill="#aa98a0"
                      fontFamily="ui-monospace, SFMono-Regular, monospace"
                      fontSize="10"
                    >
                      {formatBucketLabel(pt.bucket.bucketStart, window)}
                    </text>
                  )
                })
            : null}

          {/* Invisible hover columns */}
          {chart.points.map((pt, i) => {
            const colW = chart.plotW / Math.max(1, buckets.length)
            return (
              <rect
                key={`hit-${pt.bucket.bucketStart}`}
                x={chart.padL + i * colW}
                y={chart.padT}
                width={colW}
                height={chart.plotH}
                fill="transparent"
                className="cursor-crosshair"
                onMouseEnter={() => {
                  setHoveredIdx(i)
                }}
              />
            )
          })}
        </svg>
      </div>
    </Panel>
  )
}

function OpenInterestSkewChart({
  buckets,
  window,
}: {
  buckets: PoolEfficiencyBucket[]
  window: EfficiencyWindow
}) {
  const [hoveredIdx, setHoveredIdx] = useState<number | null>(null)

  const chart = useMemo(() => {
    const W = 600
    const H = 224
    const padL = 58
    const padR = 18
    const padT = 18
    const padB = 28
    const plotW = W - padL - padR
    const plotH = H - padT - padB

    const parsed = buckets.map((b) => {
      const longOi = parseAssetsNum(b.longOiAssets)
      const shortOi = parseAssetsNum(b.shortOiAssets)
      const grossOi = longOi + shortOi
      const netSkewSigned = parseAssetsNum(b.netSkewAssets)
      const absSkew = Math.abs(netSkewSigned)
      const poolTvl = b.poolTvlAssets ? parseAssetsNum(b.poolTvlAssets) : null
      const juniorTvl = b.juniorTvlAssets ? parseAssetsNum(b.juniorTvlAssets) : null
      return { b, longOi, shortOi, grossOi, netSkewSigned, absSkew, poolTvl, juniorTvl }
    })

    const maxY = Math.max(
      1,
      ...parsed.map((p) => Math.max(p.grossOi, p.poolTvl ?? 0, p.absSkew)),
    ) * 1.12

    const count = Math.max(1, parsed.length)
    const xFor = (i: number) =>
      count === 1 ? padL + plotW / 2 : padL + (i / (count - 1)) * plotW
    const yFor = (val: number) => padT + plotH - (val / maxY) * plotH
    const baseY = padT + plotH

    const pts = parsed.map((p, i) => ({
      ...p,
      x: xFor(i),
      longY: yFor(p.longOi),
      grossY: yFor(p.grossOi),
      skewY: yFor(p.absSkew),
      poolY: p.poolTvl != null ? yFor(p.poolTvl) : null,
      juniorY: p.juniorTvl != null ? yFor(p.juniorTvl) : null,
    }))

    const buildLine = (coords: { x: number; y: number }[]) =>
      coords.map((c, idx) => `${idx === 0 ? 'M' : 'L'}${c.x.toFixed(1)},${c.y.toFixed(1)}`).join(' ')

    const longAreaPath =
      pts.length >= 2
        ? `${buildLine(pts.map((p) => ({ x: p.x, y: p.longY })))} L${pts[pts.length - 1].x.toFixed(1)},${String(baseY)} L${pts[0].x.toFixed(1)},${String(baseY)} Z`
        : ''
    const shortAreaPath =
      pts.length >= 2
        ? `${buildLine(pts.map((p) => ({ x: p.x, y: p.grossY })))} ${[...pts]
            .reverse()
            .map((p) => `L${p.x.toFixed(1)},${p.longY.toFixed(1)}`)
            .join(' ')} Z`
        : ''
    const grossLinePath = buildLine(pts.map((p) => ({ x: p.x, y: p.grossY })))
    const skewLinePath = buildLine(pts.map((p) => ({ x: p.x, y: p.skewY })))
    const poolLinePts = pts
      .filter((p): p is typeof p & { poolY: number } => p.poolY != null)
      .map((p) => ({ x: p.x, y: p.poolY }))
    const poolLinePath = buildLine(poolLinePts)
    const juniorLinePts = pts
      .filter((p): p is typeof p & { juniorY: number } => p.juniorY != null)
      .map((p) => ({ x: p.x, y: p.juniorY }))
    const juniorLinePath = buildLine(juniorLinePts)

    return {
      W,
      H,
      padL,
      padR,
      padT,
      padB,
      plotW,
      plotH,
      maxY,
      pts,
      longAreaPath,
      shortAreaPath,
      grossLinePath,
      skewLinePath,
      poolLinePath,
      poolLinePts,
      juniorLinePath,
      juniorLinePts,
    }
  }, [buckets])

  const activeIdx = hoveredIdx != null && hoveredIdx < buckets.length ? hoveredIdx : buckets.length - 1
  const active = buckets.at(activeIdx)
  const activePt = chart.pts.at(activeIdx)

  return (
    <Panel className="flex flex-col p-4 sm:p-5">
      <div>
        <h3 className="text-sm font-semibold text-content-primary">
          Open Interest &amp; HousePool Skew
        </h3>
        <p className="mt-0.5 text-[11px] text-content-tertiary">
          Stacked Long + Short OI vs. HousePool TVL capacity &amp; |Net Skew| directional risk
        </p>
        <div className="mt-2 flex flex-wrap items-center gap-x-3 gap-y-1 font-mono text-[10px] text-content-tertiary">
          <span className="inline-flex items-center gap-1">
            <span className="h-2 w-2 bg-positive/80" /> Long
          </span>
          <span className="inline-flex items-center gap-1">
            <span className="h-2 w-2 bg-brand-orange/80" /> Short
          </span>
          <span className="inline-flex items-center gap-1">
            <span className="h-0.5 w-3 bg-brand-yellow" /> |Net Skew|
          </span>
          <span className="inline-flex items-center gap-1">
            <span className="h-0.5 w-3 border-t border-dashed border-brand-peach" /> Pool TVL
          </span>
        </div>
      </div>

      {active ? (
        <div className="mt-3 flex flex-wrap items-center justify-between gap-x-3 gap-y-1 border border-brand-border/20 bg-app-bg/60 px-3 py-2 font-mono text-[11px] tabular-nums">
          <span className="whitespace-nowrap text-content-tertiary">
            {formatBucketLabel(active.bucketEnd, window)} UTC
          </span>
          <div className="flex flex-wrap items-center gap-x-2.5 gap-y-0.5 whitespace-nowrap">
            <span className="text-positive">
              L {formatCompactAssets(active.longOiAssets).replace(' USDG', '')}
            </span>
            <span className="text-brand-orange">
              S {formatCompactAssets(active.shortOiAssets).replace(' USDG', '')}
            </span>
            <span className="font-semibold text-brand-yellow">
              Skew {formatSignedAssets(active.netSkewAssets).replace(' USDG', '')}
            </span>
            {active.grossOiToPoolBps != null ? (
              <span className="text-content-secondary">
                OI/Pool {formatPoolBpsPercent(active.grossOiToPoolBps)}
              </span>
            ) : null}
          </div>
        </div>
      ) : null}

      <div className="mt-auto pt-3">
        <svg
          viewBox={`0 0 ${String(chart.W)} ${String(chart.H)}`}
          className="h-auto w-full select-none"
          role="img"
          aria-label="Open interest and HousePool skew over time"
          onMouseLeave={() => {
            setHoveredIdx(null)
          }}
        >
          {/* Horizontal gridlines + Y labels */}
          {[0, 0.5, 1].map((frac) => {
            const y = chart.padT + chart.plotH * (1 - frac)
            return (
              <g key={frac}>
                <line
                  x1={chart.padL}
                  y1={y}
                  x2={chart.W - chart.padR}
                  y2={y}
                  stroke="#ffab96"
                  strokeOpacity={frac === 0 ? 0.22 : 0.1}
                  strokeWidth="1"
                />
                <text
                  x={chart.padL - 8}
                  y={y + 3.5}
                  textAnchor="end"
                  fill="#aa98a0"
                  fontFamily="ui-monospace, SFMono-Regular, monospace"
                  fontSize="10"
                >
                  {formatCompactDollars(chart.maxY * frac)}
                </text>
              </g>
            )
          })}

          {/* Stacked Long + Short OI areas */}
          {chart.longAreaPath ? (
            <path d={chart.longAreaPath} fill="#00ff99" fillOpacity="0.22" />
          ) : null}
          {chart.shortAreaPath ? (
            <path d={chart.shortAreaPath} fill="#ff572d" fillOpacity="0.24" />
          ) : null}
          {chart.grossLinePath ? (
            <path
              d={chart.grossLinePath}
              fill="none"
              stroke="#fff5f9"
              strokeOpacity="0.45"
              strokeWidth="1.2"
            />
          ) : null}

          {/* Junior TVL subtle dotted reference */}
          {chart.juniorLinePath ? (
            <path
              d={chart.juniorLinePath}
              fill="none"
              stroke="#aa98a0"
              strokeOpacity="0.5"
              strokeWidth="1"
              strokeDasharray="2 3"
            />
          ) : null}

          {/* HousePool TVL dashed capacity line */}
          {chart.poolLinePath ? (
            <g>
              <path
                d={chart.poolLinePath}
                fill="none"
                stroke="#ffab96"
                strokeOpacity="0.8"
                strokeWidth="1.5"
                strokeDasharray="5 4"
              />
              {chart.poolLinePts.length > 0 ? (
                <text
                  x={chart.padL + 4}
                  y={Math.max(chart.padT + 10, chart.poolLinePts[0].y - 5)}
                  fill="#ffab96"
                  fontFamily="ui-monospace, SFMono-Regular, monospace"
                  fontSize="9.5"
                >
                  Pool TVL
                </text>
              ) : null}
            </g>
          ) : null}

          {/* |Net Skew| line */}
          {chart.skewLinePath ? (
            <path
              d={chart.skewLinePath}
              fill="none"
              stroke="#f2cf71"
              strokeWidth="2.1"
              strokeLinejoin="round"
              strokeLinecap="round"
            />
          ) : null}

          {/* Active hover crosshair & dots */}
          {activePt ? (
            <g>
              <line
                x1={activePt.x}
                y1={chart.padT}
                x2={activePt.x}
                y2={chart.padT + chart.plotH}
                stroke="#fff5f9"
                strokeOpacity="0.25"
                strokeWidth="1"
                strokeDasharray="2 2"
              />
              <circle cx={activePt.x} cy={activePt.grossY} r="3" fill="#fff5f9" />
              <circle cx={activePt.x} cy={activePt.skewY} r="5.5" fill="#f2cf71" fillOpacity="0.25" />
              <circle cx={activePt.x} cy={activePt.skewY} r="3" fill="#f2cf71" />
            </g>
          ) : null}

          {/* X-axis ticks */}
          {buckets.length > 0
            ? [0, Math.floor((buckets.length - 1) / 2), buckets.length - 1]
                .filter((v, idx, arr) => arr.indexOf(v) === idx)
                .map((idx) => {
                  const pt = chart.pts.at(idx)
                  if (!pt) return null
                  return (
                    <text
                      key={idx}
                      x={pt.x}
                      y={chart.H - 8}
                      textAnchor={idx === 0 ? 'start' : idx === buckets.length - 1 ? 'end' : 'middle'}
                      fill="#aa98a0"
                      fontFamily="ui-monospace, SFMono-Regular, monospace"
                      fontSize="10"
                    >
                      {formatBucketLabel(pt.b.bucketEnd, window)}
                    </text>
                  )
                })
            : null}

          {/* Invisible hover columns */}
          {chart.pts.map((pt, i) => {
            const colW = chart.plotW / Math.max(1, buckets.length)
            return (
              <rect
                key={`oi-hit-${pt.b.bucketEnd}`}
                x={Math.max(chart.padL, pt.x - colW / 2)}
                y={chart.padT}
                width={colW}
                height={chart.plotH}
                fill="transparent"
                className="cursor-crosshair"
                onMouseEnter={() => {
                  setHoveredIdx(i)
                }}
              />
            )
          })}
        </svg>
      </div>
    </Panel>
  )
}

export interface PoolEfficiencySectionProps {
  data: LiquidityEfficiencyResponse | undefined
  isLoading: boolean
  isError: boolean
  error: Error | null
  onRetry?: () => void
}

export function PoolEfficiencySection({
  data,
  isLoading,
  isError,
  error,
  onRetry,
}: PoolEfficiencySectionProps) {
  const [window, setWindow] = useState<EfficiencyWindow>('7d')

  const endpointMissing = error instanceof InsightsApiError && error.status === 404
  const summary = data?.summary ?? null
  const activeSeries = data ? (window === '7d' ? data.series7d : data.series30d) : []
  const avgTurnoverX100 =
    window === '7d'
      ? (summary?.turnover7dAvgDailyX100 ?? null)
      : (summary?.turnover30dAvgDailyX100 ?? null)

  const netSkewSigned = summary ? BigInt(summary.currentNetSkewAssets) : 0n
  const netSkewDirection =
    netSkewSigned > 0n ? 'Net Long' : netSkewSigned < 0n ? 'Net Short' : 'Balanced'
  const juniorBufferTone =
    summary?.netSkewToJuniorBps != null && summary.netSkewToJuniorBps >= 10000
      ? 'text-brand-orange'
      : summary?.netSkewToJuniorBps != null && summary.netSkewToJuniorBps >= 7500
        ? 'text-brand-yellow'
        : 'text-content-primary'

  return (
    <section aria-labelledby="pool-efficiency-title" id="pool-efficiency" className="space-y-4">
      <div className="flex flex-wrap items-end justify-between gap-3">
        <div>
          <h2 id="pool-efficiency-title" className="text-xl font-semibold">
            Pool Capital Efficiency &amp; Open Interest
          </h2>
          <p className="mt-1 text-xs text-content-tertiary">
            Traded notional turnover against HousePool TVL (Senior + Junior), fee velocity, and
            directional HousePool skew
          </p>
        </div>

        <div className="flex flex-wrap items-center gap-3">
          {summary?.poolTvlAssets ? (
            <span className="font-mono text-xs text-content-secondary">
              Pool TVL{' '}
              <strong className="text-content-primary">
                {formatCompactAssets(summary.poolTvlAssets)}
              </strong>
              {summary.seniorTvlAssets && summary.juniorTvlAssets
                ? ` (${formatCompactAssets(summary.seniorTvlAssets).replace(' USDG', '')} Sr / ${formatCompactAssets(summary.juniorTvlAssets).replace(' USDG', '')} Jr)`
                : ''}
            </span>
          ) : null}

          <div
            role="group"
            aria-label="Capital efficiency chart timeframe"
            className="inline-flex border border-brand-border/35 bg-app-bg p-0.5 font-mono text-xs"
          >
            <button
              type="button"
              onClick={() => {
                setWindow('7d')
              }}
              aria-pressed={window === '7d'}
              className={`cursor-pointer px-2.5 py-1 font-semibold transition-colors ${
                window === '7d'
                  ? 'bg-brand-orange text-content-primary'
                  : 'text-content-secondary hover:text-content-primary'
              }`}
            >
              7D
            </button>
            <button
              type="button"
              onClick={() => {
                setWindow('30d')
              }}
              aria-pressed={window === '30d'}
              className={`cursor-pointer px-2.5 py-1 font-semibold transition-colors ${
                window === '30d'
                  ? 'bg-brand-orange text-content-primary'
                  : 'text-content-secondary hover:text-content-primary'
              }`}
            >
              30D
            </button>
          </div>
        </div>
      </div>

      {isLoading ? (
        <Panel>
          <LoadingState rows={4} />
        </Panel>
      ) : endpointMissing ? (
        <div
          role="status"
          className="border border-brand-border/30 bg-surface-panel px-4 py-3 text-xs text-content-secondary"
        >
          Pool capital efficiency &amp; OI history (
          <code className="font-mono">/api/insights/v1/liquidity/efficiency</code>) will populate once
          the updated backend is deployed.
        </div>
      ) : isError && !data ? (
        <ErrorState
          title="Pool capital efficiency unavailable"
          message={error?.message ?? 'Could not load pool efficiency telemetry.'}
          onRetry={onRetry}
        />
      ) : !summary ? (
        <Panel>
          <EmptyState
            title="No pool efficiency history yet"
            message="Volume-to-pool turnover and open interest charts will appear once vault snapshots and trade activity are indexed."
          />
        </Panel>
      ) : (
        <div className="space-y-4">
          {/* Unified Hairline Efficiency & Risk Strip */}
          <Panel>
            <dl className="grid grid-cols-1 divide-y divide-brand-border/15 sm:grid-cols-2 lg:grid-cols-5 lg:divide-x lg:divide-y-0">
              <div className="px-4 py-3.5">
                <dt className="text-[11px] font-semibold uppercase tracking-[0.1em] text-brand-peach">
                  24h Pool Turnover
                </dt>
                <dd className="mt-1 font-mono text-2xl font-bold tabular-nums text-positive">
                  {formatTurnoverX100(summary.turnover24hX100)}
                </dd>
                <p className="mt-1 font-mono text-[11px] tabular-nums text-content-tertiary">
                  7d avg {formatTurnoverX100(summary.turnover7dAvgDailyX100)} · 30d{' '}
                  {formatTurnoverX100(summary.turnover30dAvgDailyX100)}
                </p>
              </div>

              <div className="px-4 py-3.5">
                <dt className="text-[11px] font-semibold uppercase tracking-[0.1em] text-content-tertiary">
                  Gross OI / Pool
                </dt>
                <dd className="mt-1 font-mono text-2xl font-bold tabular-nums text-content-primary">
                  {formatPoolBpsPercent(summary.grossOiToPoolBps)}
                </dd>
                <p className="mt-1 font-mono text-[11px] tabular-nums text-content-tertiary">
                  {formatCompactAssets(summary.currentGrossOiAssets)} open
                </p>
              </div>

              <div className="px-4 py-3.5">
                <dt className="text-[11px] font-semibold uppercase tracking-[0.1em] text-content-tertiary">
                  Net Skew / Pool
                </dt>
                <dd
                  className={`mt-1 font-mono text-2xl font-bold tabular-nums ${
                    netSkewSigned > 0n
                      ? 'text-positive'
                      : netSkewSigned < 0n
                        ? 'text-brand-orange'
                        : 'text-content-primary'
                  }`}
                >
                  {formatPoolBpsPercent(summary.netSkewToPoolBps)}
                </dd>
                <p className="mt-1 font-mono text-[11px] tabular-nums text-content-tertiary">
                  {formatSignedAssets(summary.currentNetSkewAssets)} ({netSkewDirection})
                </p>
              </div>

              <div className="px-4 py-3.5">
                <dt className="text-[11px] font-semibold uppercase tracking-[0.1em] text-content-tertiary">
                  Junior Skew Buffer
                </dt>
                <dd className={`mt-1 font-mono text-2xl font-bold tabular-nums ${juniorBufferTone}`}>
                  {formatPoolBpsPercent(summary.netSkewToJuniorBps)}
                </dd>
                <p className="mt-1 font-mono text-[11px] tabular-nums text-content-tertiary">
                  |Net skew| vs{' '}
                  {summary.juniorTvlAssets ? formatCompactAssets(summary.juniorTvlAssets) : 'Junior'}
                </p>
              </div>

              <div className="px-4 py-3.5">
                <dt className="text-[11px] font-semibold uppercase tracking-[0.1em] text-content-tertiary">
                  7d Fee Velocity
                </dt>
                <dd className="mt-1 font-mono text-2xl font-bold tabular-nums text-brand-peach">
                  {summary.annualizedFeeYield7dBps != null
                    ? `${(summary.annualizedFeeYield7dBps / 100).toFixed(1)}%`
                    : '—'}
                </dd>
                <p className="mt-1 font-mono text-[11px] tabular-nums text-content-tertiary">
                  APR · {formatTakeRateBpsX100(summary.effectiveTakeRate7dBpsX100)} take-rate
                </p>
              </div>
            </dl>
          </Panel>

          {/* Paired Time-Series Charts */}
          {activeSeries.length > 0 ? (
            <div className="grid gap-4 lg:grid-cols-2">
              <TurnoverVolumeChart
                buckets={activeSeries}
                window={window}
                avgTurnoverX100={avgTurnoverX100}
              />
              <OpenInterestSkewChart buckets={activeSeries} window={window} />
            </div>
          ) : null}
        </div>
      )}
    </section>
  )
}
