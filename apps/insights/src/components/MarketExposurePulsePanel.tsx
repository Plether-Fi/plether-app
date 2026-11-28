import { Link } from 'react-router-dom'
import {
  formatPoolBpsPercent,
  formatTurnoverX100,
  type PoolEfficiencySummary,
} from '../api/liquidity'
import {
  classifyLiquidationBuffer,
  formatDurationCompact,
  formatLeverageX100,
  positionPath,
  type FadRiskSummary,
  type GlobalMarketExposureResponse,
} from '../api/positions'
import {
  formatCompactAssets,
  formatPrice,
  formatSignedAssets,
} from '../utils/format'
import { OraclePriceLink } from './OraclePriceLink'
import { TraderIdentity } from './TraderIdentity'
import { EmptyState, LoadingState, Panel, Pnl } from './ui'

interface MarketExposurePulsePanelProps {
  data: GlobalMarketExposureResponse | undefined
  efficiencySummary?: PoolEfficiencySummary | null
  fadRiskSummary?: FadRiskSummary | null
  isLoading: boolean
  isError: boolean
}

export function MarketExposurePulsePanel({
  data,
  efficiencySummary,
  fadRiskSummary,
  isLoading,
  isError,
}: MarketExposurePulsePanelProps) {
  const summary = data?.summary
  const positions = data?.positions ?? []
  const longPct = summary?.longShareBps != null ? summary.longShareBps / 100 : null
  const shortPct = summary?.shortShareBps != null ? summary.shortShareBps / 100 : null
  const immediateFadCount =
    (fadRiskSummary?.liquidatableNowCount ?? 0) + (fadRiskSummary?.wouldLiquidateCount ?? 0)

  return (
    <section aria-labelledby="pulse-market-exposure-title" className="space-y-3">
      <div className="flex flex-wrap items-baseline justify-between gap-3">
        <div>
          <h2 id="pulse-market-exposure-title" className="text-lg font-semibold">
            Live market exposure &amp; top active positions
          </h2>
          <p className="mt-1 text-xs text-content-tertiary">
            Real-time Long vs. Short open interest skew across active plDXY-PERP accounts
          </p>
        </div>
        <div className="flex flex-wrap items-center gap-x-3 gap-y-1 font-mono text-xs text-content-secondary">
          {data?.currentMarkPrice ? (
            <span>
              Mark{' '}
              <OraclePriceLink className="font-semibold">
                {formatPrice(data.currentMarkPrice)}
              </OraclePriceLink>
              {summary?.avgLeverageX100 != null
                ? ` · Avg lev ${formatLeverageX100(summary.avgLeverageX100)}`
                : ''}
            </span>
          ) : null}
          {efficiencySummary?.turnover24hX100 != null ? (
            <span>
              24h Vol/Pool{' '}
              <strong className="text-positive">
                {formatTurnoverX100(efficiencySummary.turnover24hX100)}
              </strong>
              {efficiencySummary.grossOiToPoolBps != null
                ? ` · OI/Pool ${formatPoolBpsPercent(efficiencySummary.grossOiToPoolBps)}`
                : ''}
            </span>
          ) : null}
          {immediateFadCount > 0 && fadRiskSummary ? (
            <Link
              to="/execution#fad-watchlist"
              className="border border-brand-orange/45 bg-brand-orange/15 px-2 py-0.5 font-mono text-[11px] font-semibold text-brand-orange transition-colors hover:bg-brand-orange/25"
            >
              FAD risk: {String(immediateFadCount)} pos (
              {formatCompactAssets(fadRiskSummary.immediateLiquidationNotionalAssets)})
            </Link>
          ) : null}
          <Link
            to="/liquidity#pool-efficiency"
            className="font-semibold text-brand-peach hover:underline"
          >
            Pool turnover &amp; OI charts →
          </Link>
        </div>
      </div>

      <Panel>
        {isLoading ? (
          <LoadingState rows={3} />
        ) : isError || !summary ? (
          <EmptyState
            title="Live market exposure unavailable"
            message="Global open-interest exposure will appear here once /api/insights/v1/market/exposure is reachable."
          />
        ) : summary.openPositionCount === 0 ? (
          <EmptyState
            title="No active perpetual positions"
            message="All indexed Trading Accounts are currently flat on plDXY-PERP."
          />
        ) : (
          <div>
            {/* Clean Global Long / Short Skew Header */}
            <div className="border-b border-brand-border/20 p-5">
              <div className="flex flex-wrap items-center justify-between gap-3 text-xs">
                <div className="flex flex-wrap items-center gap-4 font-mono">
                  <span className="font-semibold text-positive">
                    Long {longPct != null ? `${longPct.toFixed(1)}%` : '—'} (
                    {formatCompactAssets(summary.longOpenInterestAssets)} · {String(summary.longCount)})
                  </span>
                  <span className="font-semibold text-brand-orange">
                    Short {shortPct != null ? `${shortPct.toFixed(1)}%` : '—'} (
                    {formatCompactAssets(summary.shortOpenInterestAssets)} · {String(summary.shortCount)})
                  </span>
                </div>
                <div className="font-mono text-content-secondary">
                  Net skew:{' '}
                  <span
                    className={
                      BigInt(summary.netSkewAssets) > 0n
                        ? 'font-semibold text-positive'
                        : BigInt(summary.netSkewAssets) < 0n
                          ? 'font-semibold text-brand-orange'
                          : 'text-content-primary'
                    }
                  >
                    {formatSignedAssets(summary.netSkewAssets)}
                  </span>
                </div>
              </div>

              {longPct != null && shortPct != null ? (
                <div className="mt-3 flex h-2.5 w-full overflow-hidden border border-brand-border/25 bg-app-bg">
                  {longPct > 0 ? (
                    <div
                      className="h-full bg-positive/75"
                      style={{ width: `${longPct.toFixed(2)}%` }}
                    />
                  ) : null}
                  {shortPct > 0 ? (
                    <div
                      className="h-full bg-brand-orange/75"
                      style={{ width: `${shortPct.toFixed(2)}%` }}
                    />
                  ) : null}
                </div>
              ) : null}
            </div>

            {/* Top Active Positions Table */}
            <div className="overflow-x-auto">
              <table className="w-full min-w-[780px] text-left text-sm">
                <caption className="sr-only">
                  Largest active plDXY-PERP open positions by notional exposure
                </caption>
                <thead className="border-b border-brand-border/25 bg-app-bg/40 text-xs font-medium text-content-secondary">
                  <tr>
                    <th scope="col" className="whitespace-nowrap py-3 pl-4 pr-3">Trader</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3">Side</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3">Notional &amp; Lev</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3">Entry → Mark</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3">Liq Buffer</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3 text-right">Unrealized PnL</th>
                    <th scope="col" className="whitespace-nowrap py-3 pl-3 pr-4 text-right">Position</th>
                  </tr>
                </thead>
                <tbody>
                  {positions.map((pos) => {
                    const liq = classifyLiquidationBuffer(pos.distanceToLiquidationBps)
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
                        key={`${pos.account}-${pos.positionId}`}
                        className="border-b border-brand-border/15 transition-colors last:border-0 hover:bg-app-bg/35"
                      >
                        <td className="whitespace-nowrap py-3 pl-4 pr-3">
                          <TraderIdentity
                            address={pos.account}
                            discordUsername={pos.discordUsername}
                            to={`/wallets/${pos.account}?tab=exposure`}
                            avatarSize={18}
                          />
                        </td>
                        <td className="whitespace-nowrap px-3 py-3 font-mono text-xs font-semibold uppercase">
                          <span className={pos.side === 'long' ? 'text-positive' : 'text-brand-orange'}>
                            {pos.side}
                          </span>
                          <span className="ml-1.5 font-normal lowercase text-content-tertiary">
                            {formatDurationCompact(pos.durationSeconds)}
                          </span>
                        </td>
                        <td className="whitespace-nowrap px-3 py-3 font-mono text-xs tabular-nums">
                          <span className="font-semibold text-content-primary">
                            {formatCompactAssets(pos.notionalAssets)}
                          </span>
                          <span className="ml-1.5 text-brand-peach">
                            {formatLeverageX100(pos.effectiveLeverageX100)}
                          </span>
                        </td>
                        <td className="whitespace-nowrap px-3 py-3 font-mono text-xs tabular-nums text-content-secondary">
                          {formatPrice(pos.entryPrice)} →{' '}
                          <OraclePriceLink>{formatPrice(pos.markPrice)}</OraclePriceLink>
                        </td>
                        <td className={`whitespace-nowrap px-3 py-3 font-mono text-xs font-semibold tabular-nums ${liqToneClass}`}>
                          {liq.text}
                        </td>
                        <td className="whitespace-nowrap px-3 py-3 text-right font-mono text-xs font-semibold tabular-nums">
                          <Pnl value={pos.unrealizedPnlAssets} />
                        </td>
                        <td className="whitespace-nowrap py-3 pl-3 pr-4 text-right">
                          <Link
                            to={positionPath(pos.account, pos.positionId)}
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
          </div>
        )}
      </Panel>
    </section>
  )
}
