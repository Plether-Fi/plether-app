import { useMemo, useState } from 'react'
import { Link } from 'react-router-dom'
import {
  classifyLiquidationBuffer,
  describeDirectionalBias,
  formatPositionBpsPercent,
  formatDurationCompact,
  formatLeverageX100,
  positionPath,
  type ActiveExposureSnapshot,
  type PositionLifecycleSummary,
  type WalletExposureResponse,
} from '../api/positions'
import {
  formatCompactAssets,
  formatPrice,
  formatRoi,
  formatSignedAssets,
  formatUtc,
} from '../utils/format'
import { OraclePriceLink } from './OraclePriceLink'
import { StatCard } from './StatCard'
import { StatusPill, type StatusTone } from './StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel, Pnl } from './ui'
import { WalletExposureChart } from './WalletExposureChart'

type PositionFilter = 'all' | 'open' | 'closed' | 'liquidated' | 'long' | 'short'

function parseAssetsFloat(raw: string | null | undefined): number {
  if (!raw) return 0
  const n = Number(raw)
  return Number.isFinite(n) ? n / 1_000_000 : 0
}

function positionStatusTone(status: PositionLifecycleSummary['status']): StatusTone {
  if (status === 'open') return 'live'
  if (status === 'liquidated') return 'blocked'
  return 'unknown'
}

export function ExcursionMiniBar({
  maeAssets,
  mfeAssets,
  actualAssets,
  size = 'sm',
}: {
  maeAssets: string
  mfeAssets: string
  actualAssets: string
  size?: 'sm' | 'lg'
}) {
  const mae = Math.min(0, parseAssetsFloat(maeAssets))
  const mfe = Math.max(0, parseAssetsFloat(mfeAssets))
  const actual = parseAssetsFloat(actualAssets)
  const scale = Math.max(Math.abs(mae), Math.abs(mfe), Math.abs(actual), 1)

  const maePct = Math.min(50, (Math.abs(mae) / scale) * 50)
  const mfePct = Math.min(50, (mfe / scale) * 50)
  const actualOffset = Math.max(-50, Math.min(50, (actual / scale) * 50))
  const markerLeft = 50 + actualOffset

  const heightClass = size === 'lg' ? 'h-3' : 'h-2'
  const widthClass = size === 'lg' ? 'w-full' : 'w-24'

  return (
    <div className={size === 'lg' ? 'space-y-2' : 'space-y-1'}>
      <div
        className={`relative ${widthClass} ${heightClass} border border-brand-border/25 bg-app-bg`}
        title={`MAE ${formatCompactAssets(maeAssets)} · Actual ${formatSignedAssets(actualAssets)} · MFE +${formatCompactAssets(mfeAssets)}`}
      >
        {/* Adverse bar (left of center) */}
        {maePct > 0 ? (
          <div
            className="absolute top-0 bottom-0 bg-brand-orange/65"
            style={{ right: '50%', width: `${maePct.toFixed(1)}%` }}
          />
        ) : null}
        {/* Favorable bar (right of center) */}
        {mfePct > 0 ? (
          <div
            className="absolute top-0 bottom-0 bg-positive/65"
            style={{ left: '50%', width: `${mfePct.toFixed(1)}%` }}
          />
        ) : null}
        {/* Center zero hairline */}
        <span className="absolute top-0 bottom-0 left-1/2 w-px -translate-x-1/2 bg-brand-border/70" />
        {/* Actual outcome marker */}
        <span
          className="absolute -top-0.5 -bottom-0.5 w-1 -translate-x-1/2 bg-content-primary"
          style={{ left: `${markerLeft.toFixed(1)}%` }}
        />
      </div>
      {size === 'sm' ? (
        <div className="flex items-center justify-between gap-2 font-mono text-[10px] tabular-nums">
          <span className="text-brand-orange">
            {mae < 0 ? formatCompactAssets(maeAssets).replace(' USDG', '') : '0'}
          </span>
          <span className="text-positive">
            {mfe > 0 ? `+${formatCompactAssets(mfeAssets).replace(' USDG', '')}` : '0'}
          </span>
        </div>
      ) : null}
    </div>
  )
}

export function ActivePositionRiskStrip({
  address,
  exposure,
}: {
  address: string
  exposure: ActiveExposureSnapshot
}) {
  const buffer = classifyLiquidationBuffer(exposure.distanceToLiquidationBps)
  const bufferPctForBar =
    exposure.distanceToLiquidationBps != null
      ? Math.max(4, Math.min(100, (exposure.distanceToLiquidationBps / 1000) * 100))
      : 0

  const barToneClass =
    buffer.tone === 'negative'
      ? 'bg-brand-orange'
      : buffer.tone === 'warning'
        ? 'bg-brand-yellow'
        : 'bg-positive'

  const textToneClass =
    buffer.tone === 'negative'
      ? 'text-brand-orange'
      : buffer.tone === 'warning'
        ? 'text-brand-yellow'
        : buffer.tone === 'positive'
          ? 'text-positive'
          : 'text-content-tertiary'

  return (
    <div className="border-t border-brand-border/20 bg-app-bg/45 px-5 py-3.5">
      <div className="flex flex-wrap items-center justify-between gap-2 text-xs">
        <div className="flex flex-wrap items-center gap-3">
          <span className="font-semibold uppercase tracking-wider text-content-tertiary">
            Liquidation proximity
          </span>
          <span className="border border-brand-border/25 bg-app-bg px-1.5 py-0.5 font-mono text-[11px] font-semibold text-brand-peach">
            {formatLeverageX100(exposure.effectiveLeverageX100)}
          </span>
          <span className="font-mono text-[11px] text-content-secondary">
            Mark{' '}
            <OraclePriceLink className="text-content-primary">
              {formatPrice(exposure.markPrice)}
            </OraclePriceLink>{' '}
            · Liq{' '}
            {exposure.liquidationPrice ? formatPrice(exposure.liquidationPrice) : 'Out of range'}
          </span>
        </div>
        <div className="flex items-center gap-3">
          <span className={`font-mono text-xs font-semibold tabular-nums ${textToneClass}`}>
            {buffer.text} buffer
          </span>
          <Link
            to={positionPath(address, exposure.positionId)}
            className="text-xs font-semibold text-brand-peach hover:underline"
          >
            Inspect lifecycle →
          </Link>
        </div>
      </div>
      <div className="mt-2 h-1.5 w-full overflow-hidden bg-app-bg ring-1 ring-inset ring-brand-border/20">
        <div
          className={`h-full transition-all ${barToneClass}`}
          style={{ width: `${bufferPctForBar.toFixed(1)}%` }}
        />
      </div>
    </div>
  )
}

interface WalletExposureSectionProps {
  address: string
  data: WalletExposureResponse | undefined
  isLoading: boolean
  isError: boolean
  awaitingDeploy: boolean
  errorMessage?: string
  releaseVersion?: string | null
  onRetry: () => void
}

export function WalletExposureSection({
  address,
  data,
  isLoading,
  isError,
  awaitingDeploy,
  errorMessage,
  releaseVersion,
  onRetry,
}: WalletExposureSectionProps) {
  const [filter, setFilter] = useState<PositionFilter>('all')

  const filteredPositions = useMemo(() => {
    const list = data?.positions ?? []
    if (filter === 'all') return list
    if (filter === 'long' || filter === 'short') {
      return list.filter((p) => p.side === filter)
    }
    return list.filter((p) => p.status === filter)
  }, [data, filter])

  if (isLoading) {
    return (
      <Panel>
        <LoadingState rows={4} />
      </Panel>
    )
  }

  if (awaitingDeploy) {
    return (
      <Panel>
        <EmptyState
          title="Market exposure analytics awaiting backend deploy"
          message="Deploy the updated backend containing /api/insights/v1/wallets/:address/exposure to unlock directional bias, leverage history, MFE/MAE excursions, and position lifecycle drill-downs."
        />
      </Panel>
    )
  }

  if (isError || !data) {
    return (
      <ErrorState
        title="Market exposure telemetry unavailable"
        message={errorMessage ?? 'Could not load wallet exposure summary.'}
        onRetry={onRetry}
      />
    )
  }

  const { summary, activeExposure, positions, exposureTimeline } = data
  const bias = describeDirectionalBias(summary.longNotionalShareBps)
  const longPct =
    summary.longNotionalShareBps != null ? summary.longNotionalShareBps / 100 : null
  const shortPct =
    summary.shortNotionalShareBps != null ? summary.shortNotionalShareBps / 100 : null
  const tightestLiq = classifyLiquidationBuffer(summary.tightestLiquidationBps)

  return (
    <div className="space-y-6">
      {/* Row 1: 4 clean headline insight cards */}
      <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
        <StatCard
          label="Directional bias"
          value={
            longPct != null
              ? longPct >= 50
                ? `${longPct.toFixed(0)}% Long`
                : `${(100 - longPct).toFixed(0)}% Short`
              : 'Flat'
          }
          tone={bias.tone}
          detail={`${bias.label} · ${String(summary.longPositions)} Long / ${String(summary.shortPositions)} Short positions`}
        />
        <StatCard
          label="Leverage profile"
          value={
            summary.avgEntryLeverageX100 != null
              ? `${formatLeverageX100(summary.avgEntryLeverageX100)} avg`
              : '—'
          }
          tone="accent"
          detail={`Peak ${formatLeverageX100(summary.maxLeverageX100)} · Active ${summary.currentLeverageX100 > 0 ? formatLeverageX100(summary.currentLeverageX100) : 'Flat'}`}
        />
        <StatCard
          label="Avg excursion (MFE / MAE)"
          value={
            summary.avgMfeAssets != null && summary.avgMaeAssets != null
              ? `+${formatCompactAssets(summary.avgMfeAssets).replace(' USDG', '')} / ${formatCompactAssets(summary.avgMaeAssets)}`
              : '—'
          }
          tone="neutral"
          detail={
            summary.winRateBps != null
              ? `Win rate ${formatPositionBpsPercent(summary.winRateBps)} (${String(summary.winCount)}W / ${String(summary.lossCount)}L)`
              : `${String(summary.totalPositions)} position${summary.totalPositions === 1 ? '' : 's'} tracked`
          }
        />
        <StatCard
          label="Tightest liq buffer"
          value={tightestLiq.text}
          tone={tightestLiq.tone}
          detail={`${String(summary.liquidatedPositions)} liquidation${summary.liquidatedPositions === 1 ? '' : 's'} · Avg hold ${formatDurationCompact(summary.avgHoldSeconds)}`}
        />
      </div>

      {/* Active position highlight strip (only when open) */}
      {activeExposure ? (
        <Panel>
          <div className="flex flex-wrap items-center justify-between gap-3 px-5 py-3.5">
            <div className="flex flex-wrap items-center gap-2.5">
              <StatusPill
                tone={activeExposure.liquidatable ? 'blocked' : 'live'}
                label={activeExposure.liquidatable ? 'Liquidatable' : 'Active exposure'}
              />
              <span
                className={`font-mono text-sm font-semibold uppercase ${
                  activeExposure.side === 'long' ? 'text-positive' : 'text-brand-orange'
                }`}
              >
                {activeExposure.side} {activeExposure.market}
              </span>
              <span className="font-mono text-xs text-content-secondary">
                Notional {formatCompactAssets(activeExposure.notionalAssets)} · Held{' '}
                {formatDurationCompact(activeExposure.durationSeconds)}
              </span>
            </div>
            <div className="flex items-center gap-4 font-mono text-xs tabular-nums">
              <span>
                <span className="text-content-tertiary">uPnL </span>
                <Pnl value={activeExposure.unrealizedPnlAssets} />
              </span>
              <span className="text-content-tertiary">
                MFE <span className="text-positive">+{formatCompactAssets(activeExposure.mfePnlAssets)}</span>
              </span>
              <span className="text-content-tertiary">
                MAE <span className="text-brand-orange">{formatCompactAssets(activeExposure.maePnlAssets)}</span>
              </span>
            </div>
          </div>
          <ActivePositionRiskStrip address={address} exposure={activeExposure} />
        </Panel>
      ) : null}

      {/* Row 2: Directional Bias Visual Split + Exposure/Leverage Over Time */}
      {longPct != null && shortPct != null ? (
        <Panel className="p-5 sm:p-6">
          <div className="flex flex-wrap items-baseline justify-between gap-2">
            <div>
              <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                Long vs Short Directional Split
              </p>
              <p className="mt-0.5 text-xs text-content-tertiary">
                Peak notional volume, realized PnL, and total time-in-market by direction
              </p>
            </div>
            <span className="font-mono text-xs text-content-secondary">
              Peak position {formatCompactAssets(summary.peakNotionalAssets)}
            </span>
          </div>

          <div className="mt-4 flex h-3 w-full overflow-hidden border border-brand-border/25 bg-app-bg">
            {longPct > 0 ? (
              <div
                className="h-full bg-positive/75"
                style={{ width: `${longPct.toFixed(2)}%` }}
                title={`Long share: ${longPct.toFixed(1)}%`}
              />
            ) : null}
            {shortPct > 0 ? (
              <div
                className="h-full bg-brand-orange/75"
                style={{ width: `${shortPct.toFixed(2)}%` }}
                title={`Short share: ${shortPct.toFixed(1)}%`}
              />
            ) : null}
          </div>

          <div className="mt-4 grid gap-3 sm:grid-cols-2">
            <div className="flex items-center justify-between border border-positive/25 bg-positive/[0.04] px-4 py-3 text-xs">
              <div>
                <p className="font-semibold text-positive">
                  Long · {longPct.toFixed(1)}% ({summary.longPositions})
                </p>
                <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
                  {formatCompactAssets(summary.longNotionalAssets)} notional ·{' '}
                  {formatDurationCompact(summary.longHoldSeconds)} held
                </p>
              </div>
              <div className="text-right font-mono">
                <p className="text-[10px] uppercase tracking-wider text-content-tertiary">
                  Realized PnL
                </p>
                <p className="mt-0.5 text-sm font-semibold">
                  <Pnl value={summary.longRealizedPnlAssets} />
                </p>
              </div>
            </div>

            <div className="flex items-center justify-between border border-brand-orange/25 bg-brand-orange/[0.04] px-4 py-3 text-xs">
              <div>
                <p className="font-semibold text-brand-orange">
                  Short · {shortPct.toFixed(1)}% ({summary.shortPositions})
                </p>
                <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
                  {formatCompactAssets(summary.shortNotionalAssets)} notional ·{' '}
                  {formatDurationCompact(summary.shortHoldSeconds)} held
                </p>
              </div>
              <div className="text-right font-mono">
                <p className="text-[10px] uppercase tracking-wider text-content-tertiary">
                  Realized PnL
                </p>
                <p className="mt-0.5 text-sm font-semibold">
                  <Pnl value={summary.shortRealizedPnlAssets} />
                </p>
              </div>
            </div>
          </div>
        </Panel>
      ) : null}

      <WalletExposureChart points={exposureTimeline} releaseVersion={releaseVersion} />

      {/* Row 3: Reconstructed Position Lifecycles Table */}
      <section aria-labelledby="wallet-positions-title" className="space-y-3">
        <div className="flex flex-col justify-between gap-3 lg:flex-row lg:items-end">
          <div className="max-w-lg">
            <h2 id="wallet-positions-title" className="text-xl font-semibold">
              Position lifecycles (open → close)
            </h2>
            <p className="mt-1 text-xs text-content-tertiary">
              Inspect any position’s price chart, MFE/MAE excursion anatomy, leverage curve,
              liquidation proximity, and step-by-step fills
            </p>
          </div>
          <div className="flex shrink-0 flex-wrap gap-1.5" role="group" aria-label="Filter positions">
            {(
              [
                ['all', `All (${String(positions.length)})`],
                ['open', 'Open'],
                ['closed', 'Closed'],
                ['liquidated', 'Liquidated'],
                ['long', 'Long'],
                ['short', 'Short'],
              ] as const
            ).map(([opt, label]) => (
              <button
                key={opt}
                type="button"
                aria-pressed={filter === opt}
                onClick={() => { setFilter(opt) }}
                className={`border px-2.5 py-1 text-xs font-semibold transition-colors ${
                  filter === opt
                    ? 'border-brand-orange bg-brand-orange text-content-primary'
                    : 'border-brand-border/30 bg-surface-panel text-content-secondary hover:border-brand-peach/50 hover:text-content-primary'
                }`}
              >
                {label}
              </button>
            ))}
          </div>
        </div>

        <Panel>
          {filteredPositions.length > 0 ? (
            <div className="overflow-x-auto">
              <table className="w-full min-w-[780px] text-left text-sm">
                <caption className="sr-only">
                  Reconstructed Trading Account position lifecycles with MFE/MAE and liquidation proximity
                </caption>
                <thead className="border-b border-brand-border/25 bg-app-bg/40 text-xs font-medium text-content-secondary">
                  <tr>
                    <th scope="col" className="whitespace-nowrap py-3 pl-4 pr-3">Position</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3">Entry → Exit</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3">Peak Size &amp; Lev</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3">Excursion</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3">Liq Buffer</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3 text-right">Net PnL</th>
                    <th scope="col" className="whitespace-nowrap py-3 pl-3 pr-4 text-right">Details</th>
                  </tr>
                </thead>
                <tbody>
                  {filteredPositions.map((pos) => {
                    const liq = classifyLiquidationBuffer(
                      pos.status === 'open'
                        ? (pos.currentLiquidationBps ?? pos.closestLiquidationBps)
                        : pos.closestLiquidationBps,
                    )
                    const liqToneClass =
                      liq.tone === 'negative'
                        ? 'text-brand-orange'
                        : liq.tone === 'warning'
                          ? 'text-brand-yellow'
                          : liq.tone === 'positive'
                            ? 'text-positive'
                            : 'text-content-tertiary'

                    return (
                      <tr
                        key={pos.positionId}
                        className="border-b border-brand-border/15 transition-colors last:border-0 hover:bg-app-bg/35"
                      >
                        <td className="whitespace-nowrap py-3.5 pl-4 pr-3 align-top">
                          <div className="flex items-center gap-2">
                            <span
                              className={`font-mono text-xs font-semibold uppercase ${
                                pos.side === 'long' ? 'text-positive' : 'text-brand-orange'
                              }`}
                            >
                              {pos.side}
                            </span>
                            <StatusPill tone={positionStatusTone(pos.status)} label={pos.status} />
                          </div>
                          <p className="mt-1 font-mono text-[11px] text-content-tertiary">
                            {formatUtc(pos.openedAt)} · {formatDurationCompact(pos.durationSeconds)}
                          </p>
                        </td>
                        <td className="whitespace-nowrap px-3 py-3.5 align-top font-mono text-xs tabular-nums">
                          <p className="text-content-primary">
                            {formatPrice(pos.entryPrice)} →{' '}
                            {pos.exitPrice ? (
                              formatPrice(pos.exitPrice)
                            ) : data.currentMarkPrice ? (
                              <OraclePriceLink>{formatPrice(data.currentMarkPrice)} mark</OraclePriceLink>
                            ) : (
                              'Open'
                            )}
                          </p>
                          <p className="mt-1 text-[11px] text-content-tertiary">
                            {String(pos.fillCount)} fill{pos.fillCount === 1 ? '' : 's'}
                            {pos.scaleInCount > 0 ? ` · ${String(pos.scaleInCount)} scale-in` : ''}
                            {pos.partialCloseCount > 0 ? ` · ${String(pos.partialCloseCount)} partial` : ''}
                          </p>
                        </td>
                        <td className="whitespace-nowrap px-3 py-3.5 align-top font-mono text-xs tabular-nums">
                          <p className="font-semibold text-content-primary">
                            {formatCompactAssets(pos.peakNotionalAssets)}
                          </p>
                          <p className="mt-1 text-[11px] text-brand-peach">
                            {formatLeverageX100(pos.entryLeverageX100)} →{' '}
                            {formatLeverageX100(pos.maxLeverageX100)} max
                          </p>
                        </td>
                        <td className="whitespace-nowrap px-3 py-3.5 align-top">
                          <ExcursionMiniBar
                            maeAssets={pos.maePnlAssets}
                            mfeAssets={pos.mfePnlAssets}
                            actualAssets={pos.netPnlAssets}
                          />
                        </td>
                        <td className="whitespace-nowrap px-3 py-3.5 align-top font-mono text-xs tabular-nums">
                          <p className={`font-semibold ${liqToneClass}`}>
                            {pos.status === 'liquidated' ? '0.00% (Liq)' : liq.text}
                          </p>
                          <p className="mt-1 text-[11px] text-content-tertiary">
                            {pos.liquidationPrice ? `Liq ${formatPrice(pos.liquidationPrice)}` : 'Safe'}
                          </p>
                        </td>
                        <td className="whitespace-nowrap px-3 py-3.5 align-top text-right font-mono text-xs tabular-nums">
                          <p className="font-semibold">
                            <Pnl value={pos.netPnlAssets} />
                          </p>
                          {pos.roiBps != null ? (
                            <p className="mt-1 text-[11px] text-content-tertiary">
                              {formatRoi(pos.roiBps)} ROI
                            </p>
                          ) : null}
                        </td>
                        <td className="whitespace-nowrap py-3.5 pl-3 pr-4 align-top text-right">
                          <Link
                            to={positionPath(address, pos.positionId)}
                            className="inline-block border border-brand-border/40 px-2.5 py-1 text-xs font-semibold text-brand-peach transition-colors hover:border-brand-peach hover:bg-brand-peach/10"
                          >
                            Lifecycle →
                          </Link>
                        </td>
                      </tr>
                    )
                  })}
                </tbody>
              </table>
            </div>
          ) : (
            <EmptyState
              title="No matching positions"
              message="Try clearing the position filter to view all reconstructed position lifecycles."
            />
          )}
        </Panel>
      </section>
    </div>
  )
}
