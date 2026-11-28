import { useMemo, useState } from 'react'
import { Link } from 'react-router-dom'
import { InsightsApiError } from '../api/client'
import {
  formatDurationCompact,
  formatLeverageX100,
  formatPositionBpsPercent,
  type FadDangerStatus,
  type FadPositionRiskRecord,
  type MarketFadRiskResponse,
} from '../api/positions'
import {
  formatCompactAssets,
  formatPrice,
  formatSignedAssets,
  formatUtc,
} from '../utils/format'
import { StatusPill, type StatusTone } from './StatusPill'
import { TraderIdentity } from './TraderIdentity'
import { EmptyState, ErrorState, LoadingState, Panel } from './ui'

type DangerFilter = 'endangered' | 'would_liquidate' | 'critical' | 'at_risk' | 'all'

export interface FadLiquidationWatchlistSectionProps {
  data: MarketFadRiskResponse | undefined
  isLoading: boolean
  isError: boolean
  error?: Error | null
  onRetry?: () => void
}

function dangerPillMeta(status: FadDangerStatus): { tone: StatusTone; label: string } {
  switch (status) {
    case 'liquidatable_now':
      return { tone: 'blocked', label: 'Liquidatable Now' }
    case 'would_liquidate_at_fad':
      return { tone: 'blocked', label: 'Would Liquidate' }
    case 'critical':
      return { tone: 'degraded', label: 'Critical ≤0.5%' }
    case 'at_risk':
      return { tone: 'degraded', label: 'At Risk ≤1.5%' }
    case 'safe':
      return { tone: 'live', label: 'Safe' }
  }
}

function parseAssetsNum(raw: string | null | undefined): number {
  if (!raw || !/^-?\d+$/.test(raw)) return 0
  const n = Number(raw)
  return Number.isFinite(n) ? n : 0
}

function formatBpsPct(bps: number): string {
  return `${(bps / 100).toFixed(2)}%`
}

function matchesFilter(pos: FadPositionRiskRecord, filter: DangerFilter): boolean {
  switch (filter) {
    case 'endangered':
      return pos.dangerStatus !== 'safe'
    case 'would_liquidate':
      return (
        pos.dangerStatus === 'liquidatable_now' || pos.dangerStatus === 'would_liquidate_at_fad'
      )
    case 'critical':
      return pos.dangerStatus === 'critical'
    case 'at_risk':
      return pos.dangerStatus === 'at_risk'
    case 'all':
      return true
  }
}

export function FadLiquidationWatchlistSection({
  data,
  isLoading,
  isError,
  error,
  onRetry,
}: FadLiquidationWatchlistSectionProps) {
  const [filter, setFilter] = useState<DangerFilter>('endangered')
  const [search, setSearch] = useState('')

  const awaitingDeploy = error instanceof InsightsApiError && error.status === 404

  const filteredPositions = useMemo(() => {
    const positions = data?.positions ?? []
    const needle = search.trim().toLowerCase()
    return positions.filter((pos) => {
      if (!matchesFilter(pos, filter)) return false
      if (!needle) return true
      return (
        pos.account.toLowerCase().includes(needle) ||
        (pos.discordUsername?.toLowerCase().includes(needle) ?? false)
      )
    })
  }, [data?.positions, filter, search])

  if (awaitingDeploy || (!isLoading && !isError && !data)) {
    return null
  }

  const cal = data?.calendar
  const thresholds = data?.thresholds
  const summary = data?.summary

  const immediateCount =
    (summary?.liquidatableNowCount ?? 0) + (summary?.wouldLiquidateCount ?? 0)
  const immediateLongNum = parseAssetsNum(summary?.immediateLiquidationLongNotionalAssets)
  const immediateShortNum = parseAssetsNum(summary?.immediateLiquidationShortNotionalAssets)
  const immediateTotalNum = immediateLongNum + immediateShortNum

  const endangeredLongNum = parseAssetsNum(summary?.endangeredLongNotionalAssets)
  const endangeredShortNum = parseAssetsNum(summary?.endangeredShortNotionalAssets)
  const skewLongNum = immediateTotalNum > 0 ? immediateLongNum : endangeredLongNum
  const skewShortNum = immediateTotalNum > 0 ? immediateShortNum : endangeredShortNum
  const skewTotalNum = skewLongNum + skewShortNum
  const skewLongPct = skewTotalNum > 0 ? (skewLongNum / skewTotalNum) * 100 : 50
  const skewShortPct = skewTotalNum > 0 ? (skewShortNum / skewTotalNum) * 100 : 50

  const countdownLabel = cal
    ? cal.fadWindow
      ? cal.currentFadEndsAtUtc
        ? `FAD Active · Ends ${formatUtc(cal.currentFadEndsAtUtc)}`
        : 'FAD Window Active'
      : cal.secondsUntilNextFad != null
        ? `Next FAD in ${formatDurationCompact(cal.secondsUntilNextFad)}`
        : 'Schedule pending'
    : 'Loading schedule'

  const filterTabs: { id: DangerFilter; label: string; count: number }[] = [
    { id: 'endangered', label: 'Endangered', count: summary?.endangeredCount ?? 0 },
    { id: 'would_liquidate', label: 'Would Liquidate', count: immediateCount },
    { id: 'critical', label: 'Critical ≤0.5%', count: summary?.criticalCount ?? 0 },
    { id: 'at_risk', label: 'At Risk ≤1.5%', count: summary?.atRiskCount ?? 0 },
    { id: 'all', label: 'All Open', count: summary?.totalOpenPositions ?? 0 },
  ]

  return (
    <section id="fad-watchlist" aria-labelledby="fad-watchlist-title" className="space-y-4">
      <div className="flex flex-wrap items-baseline justify-between gap-3">
        <div>
          <h2 id="fad-watchlist-title" className="text-xl font-semibold">
            Next FAD Liquidation Danger Watchlist
          </h2>
          <p className="mt-1 text-xs text-content-tertiary">
            Live on-chain position stress test against Friday Afternoon Deleverage (FAD) margin
            step-up
            {thresholds
              ? ` (${formatBpsPct(thresholds.maintenanceMarginBps)} → ${formatBpsPct(thresholds.fadMarginBps)} · max ${formatLeverageX100(thresholds.maxFadLeverageX100)} eff. lev)`
              : ''}
            , including projected carry accrual to FAD start
          </p>
        </div>
        {cal ? (
          <div className="flex flex-wrap items-center gap-2">
            <StatusPill
              tone={cal.oracleFrozen ? 'blocked' : cal.fadWindow ? 'degraded' : 'live'}
              label={countdownLabel}
            />
            {thresholds ? (
              <span className="border border-brand-peach/35 bg-brand-peach/10 px-2.5 py-0.5 font-mono text-[11px] font-semibold text-brand-peach">
                {formatBpsPct(thresholds.maintenanceMarginBps)} →{' '}
                {formatBpsPct(thresholds.fadMarginBps)} ·{' '}
                {formatLeverageX100(thresholds.maxFadLeverageX100)} max lev
              </span>
            ) : null}
            <Link
              to="/oracle#fx-calendar"
              className="font-mono text-xs font-semibold text-brand-peach hover:underline"
            >
              FX freeze calendar →
            </Link>
          </div>
        ) : null}
      </div>

      {isLoading ? (
        <Panel>
          <LoadingState rows={4} />
        </Panel>
      ) : isError || !data || !cal || !thresholds || !summary ? (
        <ErrorState
          title="FAD liquidation watchlist unavailable"
          message={error?.message ?? 'Could not evaluate open positions against FAD margin rules.'}
          onRetry={onRetry}
        />
      ) : (
        <Panel className="overflow-hidden">
          {/* Asymmetric 12-Col Risk Strip (No Nested Card Boxes) */}
          <div className="grid gap-5 border-b border-brand-border/20 p-5 lg:grid-cols-12 lg:items-center">
            {/* Left Hero: Immediate FAD Liquidation Exposure */}
            <div className="space-y-3 lg:col-span-6 lg:border-r lg:border-brand-border/20 lg:pr-6">
              <div className="flex flex-wrap items-center justify-between gap-2">
                <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                  Would Liquidate at FAD
                </p>
                <span className="font-mono text-xs tabular-nums text-content-tertiary">
                  Mark {formatPrice(data.markPrice)} · Carry {formatBpsPct(thresholds.baseCarryBps)}{' '}
                  APR
                </span>
              </div>
              <div className="flex flex-wrap items-baseline gap-x-3 gap-y-1">
                <span
                  className={`font-mono text-3xl font-bold tracking-tight tabular-nums sm:text-4xl ${
                    immediateCount > 0 ? 'text-brand-orange' : 'text-positive'
                  }`}
                >
                  {String(immediateCount)} position{immediateCount === 1 ? '' : 's'}
                </span>
                <span className="font-mono text-sm font-semibold tabular-nums text-content-secondary">
                  {formatCompactAssets(summary.immediateLiquidationNotionalAssets)} notional
                  {summary.liquidatableNowCount > 0
                    ? ` (${String(summary.liquidatableNowCount)} now)`
                    : ''}
                </span>
              </div>
              <div className="flex flex-wrap items-center gap-x-4 gap-y-1 border-t border-brand-border/15 pt-2.5 font-mono text-xs tabular-nums text-content-tertiary">
                <span>
                  <strong
                    className={
                      summary.endangeredCount > 0 ? 'text-brand-yellow' : 'text-positive'
                    }
                  >
                    {String(summary.endangeredCount)} / {String(summary.totalOpenPositions)}
                  </strong>{' '}
                  open endangered (&le;1.5% buf)
                </span>
                <span>
                  <strong className="text-content-primary">
                    {formatCompactAssets(summary.totalEndangeredNotionalAssets)}
                  </strong>{' '}
                  total endangered OI
                </span>
              </div>
            </div>

            {/* Right Column: Cascade Skew Bar + Hairline-Divided Key Figures */}
            <div className="space-y-4 lg:col-span-6">
              <div className="space-y-1.5">
                <div className="flex flex-wrap items-baseline justify-between gap-2 font-mono text-xs">
                  <span className="text-content-tertiary">
                    Cascade Skew ({immediateTotalNum > 0 ? 'Immediate liqs' : 'Endangered OI'})
                  </span>
                  {skewTotalNum > 0 ? (
                    <span className="tabular-nums">
                      <strong className="text-positive">
                        L {formatCompactAssets(String(Math.round(skewLongNum)))}
                      </strong>
                      <span className="mx-1.5 text-content-tertiary">·</span>
                      <strong className="text-brand-orange">
                        S {formatCompactAssets(String(Math.round(skewShortNum)))}
                      </strong>
                    </span>
                  ) : (
                    <span className="text-positive">Zero cascade exposure</span>
                  )}
                </div>
                <div className="flex h-2 w-full overflow-hidden border border-brand-border/20 bg-app-bg">
                  {skewTotalNum > 0 ? (
                    <>
                      {skewLongPct > 0 ? (
                        <div
                          className="h-full bg-positive/80"
                          style={{ width: `${skewLongPct.toFixed(1)}%` }}
                        />
                      ) : null}
                      {skewShortPct > 0 ? (
                        <div
                          className="h-full bg-brand-orange/80"
                          style={{ width: `${skewShortPct.toFixed(1)}%` }}
                        />
                      ) : null}
                    </>
                  ) : (
                    <div className="h-full w-full bg-positive/25" />
                  )}
                </div>
              </div>

              <dl className="divide-y divide-brand-border/15 border-t border-brand-border/15 font-mono text-xs">
                <div className="flex flex-wrap items-baseline justify-between gap-2 py-2">
                  <dt className="text-content-tertiary">Total margin top-up needed</dt>
                  <dd className="font-semibold tabular-nums text-brand-peach">
                    {formatCompactAssets(summary.totalMarginTopUpNeededAssets)}
                  </dd>
                </div>
                <div className="flex flex-wrap items-baseline justify-between gap-2 pt-2">
                  <dt className="text-content-tertiary">Next FAD start / freeze</dt>
                  <dd className="tabular-nums text-content-primary">
                    {cal.nextFadStartsAtUtc ? formatUtc(cal.nextFadStartsAtUtc) : '—'}
                    {cal.nextFreezeStartsAtUtc
                      ? ` → freeze ${formatUtc(cal.nextFreezeStartsAtUtc)}`
                      : ''}
                  </dd>
                </div>
              </dl>
            </div>
          </div>

          {/* Filter Tabs & Search Bar */}
          <div className="flex flex-wrap items-center justify-between gap-3 border-b border-brand-border/20 bg-app-bg/40 px-5 py-3">
            <div
              className="flex flex-wrap items-center gap-1.5"
              role="tablist"
              aria-label="Filter FAD watchlist"
            >
              {filterTabs.map((tab) => {
                const active = filter === tab.id
                return (
                  <button
                    key={tab.id}
                    type="button"
                    role="tab"
                    aria-selected={active}
                    onClick={() => {
                      setFilter(tab.id)
                    }}
                    className={`border px-2.5 py-1 font-mono text-xs transition-colors ${
                      active
                        ? 'border-brand-peach/60 bg-brand-peach/15 font-semibold text-brand-peach'
                        : 'border-brand-border/25 bg-app-bg text-content-secondary hover:border-brand-border/50 hover:text-content-primary'
                    }`}
                  >
                    {tab.label} ({String(tab.count)})
                  </button>
                )
              })}
            </div>

            <div className="flex items-center gap-2">
              <input
                type="search"
                value={search}
                onChange={(e) => {
                  setSearch(e.target.value)
                }}
                placeholder="Filter wallet or @handle…"
                aria-label="Filter FAD watchlist by wallet or Discord handle"
                className="w-56 border border-brand-border/30 bg-app-bg px-2.5 py-1 font-mono text-xs text-content-primary placeholder:text-content-tertiary focus:border-brand-peach focus:outline-none"
              />
            </div>
          </div>

          {/* Watchlist Table */}
          {filteredPositions.length === 0 ? (
            <div className="p-6">
              <EmptyState
                title={
                  summary.totalOpenPositions === 0
                    ? 'No open perpetual positions'
                    : filter === 'endangered' && !search.trim()
                      ? 'All open positions clear the FAD margin requirement'
                      : 'No matching positions for current filter'
                }
                message={
                  summary.totalOpenPositions === 0
                    ? 'All scanned Trading Accounts are currently flat on plDXY-PERP.'
                    : filter === 'endangered' && !search.trim()
                      ? `Every one of the ${String(summary.totalOpenPositions)} active open position${summary.totalOpenPositions === 1 ? '' : 's'} has >1.50% buffer above the ${formatBpsPct(thresholds.fadMarginBps)} FAD maintenance threshold.`
                      : 'Try switching to "All Open" or clearing your search filter.'
                }
              />
            </div>
          ) : (
            <div className="overflow-x-auto">
              <table className="w-full min-w-[920px] text-left text-sm">
                <caption className="sr-only">
                  Open perpetual positions ranked by FAD liquidation risk
                </caption>
                <thead className="border-b border-brand-border/25 bg-app-bg/50 text-xs font-medium text-content-secondary">
                  <tr>
                    <th scope="col" className="whitespace-nowrap py-3 pl-5 pr-3">Trader</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3">Side &amp; Notional</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3 text-right">Eff. Lev</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3 text-right">
                      Equity at FAD / Req
                    </th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3 text-right">
                      FAD Liq &amp; Buffer
                    </th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3 text-right">
                      Required Action
                    </th>
                    <th scope="col" className="whitespace-nowrap py-3 pl-3 pr-5 text-right">
                      Danger Tier
                    </th>
                  </tr>
                </thead>
                <tbody>
                  {filteredPositions.map((pos) => {
                    const pill = dangerPillMeta(pos.dangerStatus)
                    const overMaxFadLev = pos.effectiveLeverageX100 > thresholds.maxFadLeverageX100
                    const needsTopUp = parseAssetsNum(pos.marginNeededToSurviveAssets) > 0
                    const carryNum = parseAssetsNum(pos.projectedCarryToFadAssets)

                    return (
                      <tr
                        key={pos.account}
                        className={`border-b border-brand-border/15 transition-colors last:border-0 hover:bg-app-bg/35 ${
                          pos.dangerStatus === 'liquidatable_now' ||
                          pos.dangerStatus === 'would_liquidate_at_fad'
                            ? 'bg-brand-orange/[0.04]'
                            : ''
                        }`}
                      >
                        <td className="whitespace-nowrap py-3 pl-5 pr-3">
                          <TraderIdentity
                            address={pos.account}
                            discordUsername={pos.discordUsername}
                          />
                        </td>
                        <td className="whitespace-nowrap px-3 py-3 font-mono text-xs tabular-nums">
                          <span
                            className={`mr-2 inline-block border px-1.5 py-0.5 text-[10px] font-semibold uppercase ${
                              pos.side === 'long'
                                ? 'border-positive/40 bg-positive/10 text-positive'
                                : 'border-brand-orange/40 bg-brand-orange/10 text-brand-orange'
                            }`}
                          >
                            {pos.side}
                          </span>
                          <span className="font-semibold text-content-primary">
                            {formatCompactAssets(pos.notionalAssets)}
                          </span>
                        </td>
                        <td
                          className={`whitespace-nowrap px-3 py-3 text-right font-mono text-xs font-semibold tabular-nums ${
                            overMaxFadLev ? 'text-brand-orange' : 'text-content-primary'
                          }`}
                        >
                          {formatLeverageX100(pos.effectiveLeverageX100)}
                        </td>
                        <td className="whitespace-nowrap px-3 py-3 text-right font-mono text-xs tabular-nums">
                          <div className="font-semibold text-content-primary">
                            {formatCompactAssets(pos.projectedEquityAtFadAssets)}{' '}
                            <span className="font-normal text-content-tertiary">
                              / {formatCompactAssets(pos.fadRequiredMarginAssets)}
                            </span>
                          </div>
                          {carryNum > 0 ? (
                            <div className="text-[10px] text-content-tertiary">
                              incl. −{formatCompactAssets(pos.projectedCarryToFadAssets)} carry
                            </div>
                          ) : null}
                        </td>
                        <td className="whitespace-nowrap px-3 py-3 text-right font-mono text-xs tabular-nums">
                          <div className="font-semibold text-content-primary">
                            {pos.fadLiquidationPrice ? formatPrice(pos.fadLiquidationPrice) : '—'}
                          </div>
                          <div
                            className={`text-[11px] ${
                              pos.distanceToFadLiquidationBps != null &&
                              pos.distanceToFadLiquidationBps <= 50
                                ? 'text-brand-orange'
                                : pos.distanceToFadLiquidationBps != null &&
                                    pos.distanceToFadLiquidationBps <= 150
                                  ? 'text-brand-yellow'
                                  : 'text-positive'
                            }`}
                          >
                            {pos.distanceToFadLiquidationBps != null
                              ? `${formatPositionBpsPercent(pos.distanceToFadLiquidationBps)} away`
                              : 'No liq price'}
                          </div>
                        </td>
                        <td className="whitespace-nowrap px-3 py-3 text-right font-mono text-xs tabular-nums">
                          {needsTopUp ? (
                            <span className="font-semibold text-brand-orange">
                              Add &ge; {formatCompactAssets(pos.marginNeededToSurviveAssets)}
                            </span>
                          ) : (
                            <span className="text-positive">
                              {formatSignedAssets(pos.fadMarginBufferAssets)} buf
                            </span>
                          )}
                        </td>
                        <td className="whitespace-nowrap py-3 pl-3 pr-5 text-right">
                          <StatusPill tone={pill.tone} label={pill.label} />
                        </td>
                      </tr>
                    )
                  })}
                </tbody>
              </table>
            </div>
          )}
        </Panel>
      )}
    </section>
  )
}
