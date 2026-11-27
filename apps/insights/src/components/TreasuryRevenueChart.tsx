import { useState } from 'react'
import { formatSponsoredEthWei } from '../api/sponsored'
import type { DailyRevenuePoint } from '../api/treasury'
import { formatAssets } from '../utils/format'

export interface TreasuryRevenueChartProps {
  series: DailyRevenuePoint[]
}

function toNumericUsdg(raw: string): number {
  const parsed = Number(raw)
  return Number.isFinite(parsed) && parsed > 0 ? parsed / 1_000_000 : 0
}

function shortDayLabel(isoDay: string): string {
  const parts = isoDay.split('-')
  if (parts.length !== 3) return isoDay
  const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
  const mIdx = Number(parts[1]) - 1
  const day = Number(parts[2])
  const month = months[mIdx]
  if (!month || !Number.isFinite(day)) return isoDay
  return `${month} ${String(day)}`
}

export function TreasuryRevenueChart({ series }: TreasuryRevenueChartProps) {
  const [range, setRange] = useState<'7d' | '30d'>('30d')
  const [hoveredIdx, setHoveredIdx] = useState<number | null>(null)

  const visible = range === '7d' ? series.slice(-7) : series
  const activeIdx =
    hoveredIdx !== null && hoveredIdx >= 0 && hoveredIdx < visible.length
      ? hoveredIdx
      : visible.length - 1
  const activePoint = visible[activeIdx]

  const dailyValues = visible.map((p) => toNumericUsdg(p.executionFeeAssets))
  const cumValues = visible.map((p) => toNumericUsdg(p.cumulativeExecutionFeeAssets))
  const maxDaily = Math.max(...dailyValues, 0.01)
  const minCum = cumValues.length > 0 ? Math.min(...cumValues) : 0
  const maxCum = Math.max(...cumValues, minCum + 0.01)
  const cumSpan = Math.max(maxCum - minCum, 0.01)

  const width = 760
  const height = 210
  const padLeft = 10
  const padRight = 10
  const padTop = 18
  const padBottom = 26
  const plotW = width - padLeft - padRight
  const plotH = height - padTop - padBottom

  const barStep = visible.length > 0 ? plotW / visible.length : plotW
  const barWidth = Math.max(4, Math.min(34, barStep * 0.64))

  const cumCoords = visible.map((p, idx) => {
    const x = padLeft + idx * barStep + barStep / 2
    const val = toNumericUsdg(p.cumulativeExecutionFeeAssets)
    const y = padTop + plotH - ((val - minCum) / cumSpan) * (plotH * 0.88)
    return { x, y }
  })
  const polylinePoints = cumCoords.map((c) => `${c.x.toFixed(1)},${c.y.toFixed(1)}`).join(' ')

  return (
    <div className="space-y-4">
      <div className="flex flex-wrap items-center justify-between gap-3">
        <div>
          <h2 className="text-base font-semibold text-content-primary">
            Daily protocol execution fees &amp; cumulative curve
          </h2>
          <p className="mt-0.5 text-xs text-content-tertiary">
            UTC daily assessed execution fees (bars) overlaid with cumulative protocol fee revenue (line)
          </p>
        </div>

        <div className="flex items-center gap-1" role="group" aria-label="Revenue chart window">
          {(['7d', '30d'] as const).map((opt) => (
            <button
              key={opt}
              type="button"
              onClick={() => {
                setRange(opt)
                setHoveredIdx(null)
              }}
              aria-pressed={range === opt}
              className={`border px-2.5 py-1 font-mono text-xs font-semibold transition-colors ${
                range === opt
                  ? 'border-brand-orange bg-brand-orange text-content-primary'
                  : 'border-brand-border/35 bg-app-bg text-content-secondary hover:border-brand-peach/60 hover:text-content-primary'
              }`}
            >
              {opt}
            </button>
          ))}
        </div>
      </div>

      {visible.length === 0 ? (
        <div className="border border-brand-border/20 bg-app-bg px-4 py-10 text-center text-xs text-content-tertiary">
          No daily fee history indexed yet.
        </div>
      ) : (
        <>
          <div className="border border-brand-border/20 bg-app-bg p-3">
            <svg
              viewBox={`0 0 ${String(width)} ${String(height)}`}
              className="h-52 w-full overflow-visible"
              role="img"
              aria-label={`Daily protocol execution fees over the last ${range}`}
              onMouseLeave={() => { setHoveredIdx(null) }}
            >
              {/* Subtle horizontal grid lines */}
              {[0, 0.25, 0.5, 0.75, 1].map((frac) => {
                const y = padTop + plotH * (1 - frac)
                return (
                  <line
                    key={frac}
                    x1={padLeft}
                    y1={y}
                    x2={width - padRight}
                    y2={y}
                    stroke="rgba(255,255,255,0.06)"
                    strokeWidth="1"
                  />
                )
              })}

              {/* Daily fee bars */}
              {visible.map((point, idx) => {
                const val = dailyValues[idx] ?? 0
                const barH = val > 0 ? Math.max(3, (val / maxDaily) * (plotH * 0.85)) : 1
                const x = padLeft + idx * barStep + (barStep - barWidth) / 2
                const y = padTop + plotH - barH
                const isSelected = idx === activeIdx
                return (
                  <g
                    key={point.dayUtc}
                    className="cursor-pointer"
                    onMouseEnter={() => { setHoveredIdx(idx) }}
                    onClick={() => { setHoveredIdx(idx) }}
                  >
                    <rect
                      x={padLeft + idx * barStep}
                      y={padTop}
                      width={barStep}
                      height={plotH}
                      fill="transparent"
                    />
                    <rect
                      x={x}
                      y={y}
                      width={barWidth}
                      height={barH}
                      rx={1}
                      fill={
                        isSelected
                          ? '#FF6B35'
                          : val > 0
                            ? 'rgba(255, 107, 53, 0.52)'
                            : 'rgba(255, 255, 255, 0.08)'
                      }
                    />
                  </g>
                )
              })}

              {/* Cumulative revenue polyline */}
              {cumCoords.length > 1 ? (
                <polyline
                  fill="none"
                  stroke="#F5B895"
                  strokeWidth="2"
                  strokeLinejoin="round"
                  points={polylinePoints}
                />
              ) : null}

              {/* Active point dot on cumulative line */}
              {cumCoords[activeIdx] ? (
                <circle
                  cx={cumCoords[activeIdx].x}
                  cy={cumCoords[activeIdx].y}
                  r={4}
                  fill="#F5B895"
                  stroke="#0C0C0D"
                  strokeWidth="2"
                />
              ) : null}

              {/* X-axis edge and midpoint labels */}
              {[0, Math.floor((visible.length - 1) / 2), visible.length - 1]
                .filter((i, pos, arr) => arr.indexOf(i) === pos && i >= 0 && i < visible.length)
                .map((idx) => {
                  const pt = visible[idx]
                  const cx = padLeft + idx * barStep + barStep / 2
                  const anchor = idx === 0 ? 'start' : idx === visible.length - 1 ? 'end' : 'middle'
                  return (
                    <text
                      key={pt.dayUtc}
                      x={idx === 0 ? padLeft : idx === visible.length - 1 ? width - padRight : cx}
                      y={height - 6}
                      textAnchor={anchor}
                      fill="rgba(255,255,255,0.48)"
                      fontSize="10"
                      fontFamily="ui-monospace, SFMono-Regular, Menlo, monospace"
                    >
                      {shortDayLabel(pt.dayUtc)}
                    </text>
                  )
                })}
            </svg>

            <div className="mt-2 flex flex-wrap items-center justify-between gap-2 border-t border-brand-border/15 pt-2.5 text-[11px] text-content-tertiary">
              <div className="flex flex-wrap items-center gap-4">
                <span className="inline-flex items-center gap-1.5">
                  <span className="inline-block h-2.5 w-2.5 bg-brand-orange/80" />
                  Daily execution fees (USDG)
                </span>
                <span className="inline-flex items-center gap-1.5">
                  <span className="inline-block h-0.5 w-3.5 bg-brand-peach" />
                  Cumulative fees (USDG)
                </span>
              </div>
              <span className="font-mono">
                Peak day: {formatAssets(String(Math.round(maxDaily * 1_000_000)))}
              </span>
            </div>
          </div>

          <div className="grid grid-cols-2 gap-2 border border-brand-border/20 bg-app-bg p-3 font-mono text-xs sm:grid-cols-3 lg:grid-cols-6">
            <div>
              <span className="block text-[10px] uppercase tracking-wider text-content-tertiary">
                UTC Day
              </span>
              <span className="font-semibold text-content-primary">{activePoint.dayUtc}</span>
            </div>
            <div>
              <span className="block text-[10px] uppercase tracking-wider text-content-tertiary">
                Daily Fees
              </span>
              <span className="font-semibold text-positive">
                {formatAssets(activePoint.executionFeeAssets)}
              </span>
            </div>
            <div>
              <span className="block text-[10px] uppercase tracking-wider text-content-tertiary">
                Cumulative
              </span>
              <span className="text-brand-peach">
                {formatAssets(activePoint.cumulativeExecutionFeeAssets)}
              </span>
            </div>
            <div>
              <span className="block text-[10px] uppercase tracking-wider text-content-tertiary">
                Volume ({activePoint.executedOrders.toLocaleString('en-US')} fills)
              </span>
              <span className="text-content-secondary">
                {formatAssets(activePoint.volumeAssets)}
              </span>
            </div>
            <div>
              <span className="block text-[10px] uppercase tracking-wider text-content-tertiary">
                Close Assist
              </span>
              <span className="text-content-secondary">
                {formatAssets(activePoint.closeAssistanceAssets)}
              </span>
            </div>
            <div>
              <span className="block text-[10px] uppercase tracking-wider text-content-tertiary">
                Gas Subsidized
              </span>
              <span className="text-content-secondary">
                {formatSponsoredEthWei(activePoint.sponsoredGasWei, 5)}
              </span>
            </div>
          </div>
        </>
      )}
    </div>
  )
}
