import { useState } from 'react'
import { Link } from 'react-router-dom'
import {
  InsightsApiError,
  formatRateBps,
  tradeToWalletActivity,
  useExecutionQuality,
  useExecutionTrades,
  type ProtocolTradeRecord,
} from '../api'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { PositionBreakdown } from '../components/PositionBreakdown'
import { StatCard } from '../components/StatCard'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel, Pnl } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { explorerTxUrl, resolvePerpsChain } from '../utils/chain'
import { formatCompactAssets, formatUtc, shortAddress } from '../utils/format'

type TradeTypeFilter = 'all' | 'Open' | 'Close' | 'Liquidated'

function isEndpointNotDeployed(error: unknown): boolean {
  return error instanceof InsightsApiError && error.status === 404
}

function windowLabel(seconds: number): string {
  if (seconds === 3600) return '1 hour'
  if (seconds === 86400) return '24 hours'
  return `${String(seconds)}s`
}

function tradeTone(activityType: ProtocolTradeRecord['activityType']): StatusTone {
  if (activityType === 'Open') return 'live'
  if (activityType === 'Close') return 'unknown'
  if (activityType === 'Liquidated') return 'blocked'
  return 'unknown'
}

function formatLatencyPair(p50: number | null, p95: number | null, unit: string): string | undefined {
  if (p50 === null && p95 === null) return undefined
  const left = p50 !== null ? `${String(p50)}${unit}` : '—'
  const right = p95 !== null ? `${String(p95)}${unit}` : '—'
  return `${left} / ${right}`
}

export function ExecutionPage() {
  const protocol = useProtocolContext()
  const qualityQuery = useExecutionQuality()
  const tradesQuery = useExecutionTrades(30)
  const [tradeFilter, setTradeFilter] = useState<TradeTypeFilter>('all')

  const quality = qualityQuery.data
  const tradesData = tradesQuery.data
  const chainId =
    quality?.network.chainId ?? tradesData?.network.chainId ?? protocol.chainId
  const chain = resolvePerpsChain(chainId)
  const releaseVersion =
    quality?.network.releaseVersion ?? tradesData?.network.releaseVersion ?? protocol.releaseVersion

  const awaitingDeploy =
    isEndpointNotDeployed(qualityQuery.error) || isEndpointNotDeployed(tradesQuery.error)

  const cohort24h = quality?.orderReliability.find((row) => row.windowSeconds === 86400) ?? null
  const cohort1h = quality?.orderReliability.find((row) => row.windowSeconds === 3600) ?? null
  const latencySec = quality?.executionLatency.seconds ?? null
  const latencyBlk = quality?.executionLatency.blocks ?? null

  const bannerTone: StatusTone = awaitingDeploy
    ? 'unknown'
    : qualityQuery.isError && tradesQuery.isError
      ? 'blocked'
      : cohort24h
        ? cohort24h.expiryRateBps !== null && cohort24h.expiryRateBps > 500
          ? 'degraded'
          : 'ready'
        : qualityQuery.isLoading
          ? 'unknown'
          : 'stale'

  const bannerLabel = awaitingDeploy
    ? 'Awaiting Stage 2 backend deploy'
    : qualityQuery.isError && tradesQuery.isError
      ? 'Execution telemetry unavailable'
      : cohort24h
        ? cohort24h.expiryRateBps !== null && cohort24h.expiryRateBps > 500
          ? 'Elevated order expirations'
          : 'Execution pipeline healthy'
        : qualityQuery.isLoading
          ? 'Loading execution telemetry'
          : 'No execution cohorts yet'

  const visibleTrades =
    tradesData?.trades.filter((row) =>
      tradeFilter === 'all' ? true : row.activityType === tradeFilter,
    ) ?? []

  return (
    <div className="space-y-8">
      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={
          quality?.liquidations.indexedThroughBlock && quality.liquidations.indexedThroughBlock !== '0'
            ? quality.liquidations.indexedThroughBlock
            : protocol.blockNumber
        }
        observedAt={quality?.observedAt ?? protocol.observedAt}
        tone={bannerTone}
        statusLabel={bannerLabel}
        note={
          quality
            ? `Canonical cohorts through ${formatUtc(quality.cohortEndAt)}`
            : 'Order reliability, protections, liquidations & protocol tape'
        }
      />

      <header className="flex flex-col justify-between gap-4 sm:flex-row sm:items-end">
        <div className="max-w-3xl space-y-3">
          <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">
            Trader Execution &amp; Market Forensics
          </p>
          <h1 className="text-3xl font-semibold tracking-tight sm:text-4xl">
            Order Reliability, Protections, Liquidations &amp; Trade Tape
          </h1>
          <p className="text-sm leading-6 text-content-secondary">
            Protocol-wide execution telemetry beyond the main app’s price chart and trade ticket—
            auditing canonical keeper fill/expiration cohorts, commit-to-execution latency, TP/SL
            protection coverage, liquidation activity, and every executed fill across all Trading
            Accounts.
          </p>
        </div>
        <a
          href={chain.tradeAppUrl}
          className="shrink-0 border border-brand-border/40 px-4 py-2.5 text-sm font-semibold text-brand-peach hover:bg-brand-peach/10"
        >
          Open Trading App ↗
        </a>
      </header>

      {awaitingDeploy ? (
        <div
          role="status"
          className="border border-brand-yellow/40 bg-brand-yellow/10 px-4 py-3 text-sm text-brand-yellow"
        >
          <strong>Connected backend does not serve Stage 2 Execution endpoints yet (HTTP 404).</strong>{' '}
          Deploy the updated backend containing{' '}
          <code className="font-mono text-xs">/api/insights/v1/execution/quality</code> and{' '}
          <code className="font-mono text-xs">/api/insights/v1/execution/trades</code> to stream
          live execution cohorts and the protocol trade tape.
        </div>
      ) : null}

      <section aria-labelledby="execution-kpis-title" className="space-y-4">
        <h2 id="execution-kpis-title" className="text-xl font-semibold">
          Execution quality snapshot
        </h2>
        <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
          <StatCard
            label="Order fill rate (24h)"
            loading={qualityQuery.isLoading}
            unavailable={cohort24h?.fillRateBps == null}
            value={cohort24h ? formatRateBps(cohort24h.fillRateBps) : undefined}
            tone={
              cohort24h?.expiryRateBps != null && cohort24h.expiryRateBps > 500
                ? 'warning'
                : 'positive'
            }
            detail={
              cohort24h
                ? `1h fill: ${formatRateBps(cohort1h?.fillRateBps)} · 24h expired: ${formatRateBps(cohort24h.expiryRateBps)} (${String(cohort24h.totalOrders)} orders)`
                : 'Canonical keeper cohort (shifted 120s)'
            }
          />
          <StatCard
            label="Execution latency (p50 / p95)"
            loading={qualityQuery.isLoading}
            unavailable={!latencySec || latencySec.sampleCount === 0}
            value={
              latencySec ? formatLatencyPair(latencySec.p50, latencySec.p95, 's') : undefined
            }
            tone="accent"
            detail={
              latencyBlk && latencyBlk.sampleCount > 0
                ? `Blocks: ${formatLatencyPair(latencyBlk.p50, latencyBlk.p95, ' blk') ?? '—'} · ${latencySec?.sampleCount.toLocaleString('en-US') ?? '0'} fills`
                : 'Commit block to execution confirmation'
            }
          />
          <StatCard
            label="Active TP/SL protections"
            loading={qualityQuery.isLoading}
            unavailable={!quality}
            value={quality?.protections.activeProtections.toLocaleString('en-US')}
            detail={
              quality
                ? `Triggered: ${String(quality.protections.triggered24h)} · Cancelled: ${String(quality.protections.cancelled24h)} · Expired: ${String(quality.protections.expired24h)}`
                : 'PositionProtectionBook live state'
            }
          />
          <StatCard
            label="Liquidations & volume (24h)"
            loading={qualityQuery.isLoading}
            unavailable={!quality}
            value={quality?.liquidations.liquidations24h.toLocaleString('en-US')}
            detail={
              quality
                ? `24h volume: ${formatCompactAssets(quality.volume24hAssets)} · Bounties: ${formatCompactAssets(quality.liquidations.bountiesPaid24hAssets)}`
                : 'Liquidated positions, bounties & 24h volume'
            }
          />
        </div>
      </section>

      <section aria-labelledby="cohorts-title" className="space-y-4">
        <div className="flex flex-wrap items-baseline justify-between gap-2">
          <div>
            <h2 id="cohorts-title" className="text-xl font-semibold">
              Canonical order reliability cohorts
            </h2>
            <p className="mt-1 text-xs text-content-tertiary">
              Half-open commitment windows shifted 120s before observation · Expirations count
              canonical terminal reason 2
            </p>
          </div>
          {quality ? (
            <span className="font-mono text-xs text-content-secondary">
              Pending keeper queue: {quality.pendingOrderQueue.toLocaleString('en-US')}
            </span>
          ) : null}
        </div>

        <Panel>
          {qualityQuery.isLoading ? (
            <LoadingState rows={2} />
          ) : qualityQuery.isError && !awaitingDeploy ? (
            <ErrorState
              title="Order reliability unavailable"
              message={qualityQuery.error.message}
              onRetry={() => void qualityQuery.refetch()}
            />
          ) : quality && quality.orderReliability.length > 0 ? (
            <div className="overflow-x-auto">
              <table className="w-full min-w-[780px] text-left text-sm">
                <caption className="sr-only">Canonical keeper order reliability windows</caption>
                <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                  <tr>
                    <th scope="col" className="px-4 py-3.5">Window</th>
                    <th scope="col" className="px-4 py-3.5">Committed</th>
                    <th scope="col" className="px-4 py-3.5">Executed</th>
                    <th scope="col" className="px-4 py-3.5">Fill rate</th>
                    <th scope="col" className="px-4 py-3.5">Expired (r=2)</th>
                    <th scope="col" className="px-4 py-3.5">Other failed</th>
                    <th scope="col" className="px-4 py-3.5">Pending</th>
                    <th scope="col" className="px-4 py-3.5">Affected accounts</th>
                  </tr>
                </thead>
                <tbody>
                  {quality.orderReliability.map((cohort) => (
                    <tr
                      key={cohort.windowSeconds}
                      className="border-b border-brand-border/15 last:border-0"
                    >
                      <td className="px-4 py-3.5 font-semibold">{windowLabel(cohort.windowSeconds)}</td>
                      <td className="px-4 py-3.5 font-mono tabular-nums">
                        {cohort.totalOrders.toLocaleString('en-US')}
                      </td>
                      <td className="px-4 py-3.5 font-mono text-positive tabular-nums">
                        {cohort.executedOrders.toLocaleString('en-US')}
                      </td>
                      <td className="px-4 py-3.5 font-mono tabular-nums">
                        {formatRateBps(cohort.fillRateBps)}
                      </td>
                      <td className="px-4 py-3.5 font-mono text-brand-orange tabular-nums">
                        {cohort.expiredOrders.toLocaleString('en-US')} (
                        {formatRateBps(cohort.expiryRateBps)})
                      </td>
                      <td className="px-4 py-3.5 font-mono tabular-nums">
                        {cohort.otherFailedOrders.toLocaleString('en-US')}
                      </td>
                      <td className="px-4 py-3.5 font-mono tabular-nums">
                        {cohort.pendingOrders.toLocaleString('en-US')}
                      </td>
                      <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                        {cohort.affectedAccounts}/{cohort.activeAccounts} (repeat:{' '}
                        {cohort.repeatAffectedAccounts})
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          ) : (
            <EmptyState
              title="No reliability cohorts available"
              message="Canonical 1-hour and 24-hour order reliability metrics will appear here once /api/insights/v1/execution/quality is live."
            />
          )}
        </Panel>
      </section>

      <section aria-labelledby="trade-tape-title" className="space-y-4">
        <div className="flex flex-col justify-between gap-3 sm:flex-row sm:items-end">
          <div>
            <h2 id="trade-tape-title" className="text-xl font-semibold">
              Protocol-wide trade &amp; liquidation tape
            </h2>
            <p className="mt-1 text-xs text-content-tertiary">
              Every executed Open, Close, and Liquidation across all Trading Accounts
            </p>
          </div>
          <div className="flex flex-wrap gap-1.5" role="group" aria-label="Filter trade tape">
            {([
              ['all', 'All fills'],
              ['Open', 'Opens'],
              ['Close', 'Closes'],
              ['Liquidated', 'Liquidations'],
            ] as const).map(([option, label]) => (
              <button
                key={option}
                type="button"
                aria-pressed={tradeFilter === option}
                onClick={() => {
                  setTradeFilter(option)
                }}
                className={`border px-3 py-1.5 text-xs font-semibold transition-colors ${
                  tradeFilter === option
                    ? 'border-brand-orange bg-brand-orange text-content-primary'
                    : 'border-brand-border/30 bg-surface-panel text-content-secondary hover:border-brand-orange/50 hover:text-content-primary'
                }`}
              >
                {label}
              </button>
            ))}
          </div>
        </div>

        <Panel>
          {tradesQuery.isLoading ? (
            <LoadingState rows={5} />
          ) : tradesQuery.isError && !awaitingDeploy ? (
            <ErrorState
              title="Trade tape unavailable"
              message={tradesQuery.error.message}
              onRetry={() => void tradesQuery.refetch()}
            />
          ) : visibleTrades.length > 0 ? (
            <div className="overflow-x-auto">
              <table className="w-full min-w-[960px] text-left text-sm">
                <caption className="sr-only">Recent protocol-wide executed trades and liquidations</caption>
                <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                  <tr>
                    <th scope="col" className="px-4 py-3.5">Time (UTC)</th>
                    <th scope="col" className="px-4 py-3.5">Type</th>
                    <th scope="col" className="px-4 py-3.5">Side</th>
                    <th scope="col" className="px-4 py-3.5">Trading Account</th>
                    <th scope="col" className="px-4 py-3.5">Amount</th>
                    <th scope="col" className="px-4 py-3.5">Realized PnL</th>
                    <th scope="col" className="px-4 py-3.5">VPI / Spread</th>
                    <th scope="col" className="px-4 py-3.5">Latency</th>
                    <th scope="col" className="px-4 py-3.5">Receipt</th>
                    <th scope="col" className="px-4 py-3.5">Tx</th>
                  </tr>
                </thead>
                <tbody>
                  {visibleTrades.map((trade) => {
                    const hasReceipt =
                      trade.execution != null &&
                      (trade.activityType === 'Open' || trade.activityType === 'Close')
                    return (
                      <tr key={trade.id} className="border-b border-brand-border/15 last:border-0">
                        <td className="px-4 py-3.5 text-xs text-content-secondary">
                          {formatUtc(trade.occurredAt)}
                        </td>
                        <td className="px-4 py-3.5">
                          <StatusPill tone={tradeTone(trade.activityType)} label={trade.activityType} />
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs uppercase">
                          {trade.side ? (
                            <span
                              className={
                                trade.side === 'long'
                                  ? 'text-positive'
                                  : trade.side === 'short'
                                    ? 'text-brand-orange'
                                    : 'text-content-tertiary'
                              }
                            >
                              {trade.side}
                            </span>
                          ) : (
                            '—'
                          )}
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs">
                          <Link
                            to={`/wallets/${trade.account}`}
                            title={trade.account}
                            className="text-brand-peach hover:underline"
                          >
                            {shortAddress(trade.account)}
                          </Link>
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                          {formatCompactAssets(trade.amountAssets)}
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs">
                          {trade.pnlAssets != null ? <Pnl value={trade.pnlAssets} /> : '—'}
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                          {trade.vpiAssets != null
                            ? formatCompactAssets(trade.vpiAssets)
                            : trade.frozenCloseSpreadAssets != null
                              ? `Spread ${formatCompactAssets(trade.frozenCloseSpreadAssets)}`
                              : '—'}
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                          {trade.latencySeconds != null ? `${String(trade.latencySeconds)}s` : '—'}
                        </td>
                        <td className="px-4 py-3.5">
                          {hasReceipt ? (
                            <PositionBreakdown
                              item={tradeToWalletActivity(trade)}
                              chainId={chainId}
                              compact
                            />
                          ) : (
                            <span className="font-mono text-xs text-content-tertiary">—</span>
                          )}
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs">
                          <a
                            href={explorerTxUrl(trade.txHash, chainId)}
                            target="_blank"
                            rel="noreferrer"
                            className="text-brand-peach hover:underline"
                          >
                            {shortAddress(trade.txHash)} ↗
                          </a>
                        </td>
                      </tr>
                    )
                  })}
                </tbody>
              </table>
            </div>
          ) : (
            <EmptyState
              title="No matching fills"
              message={
                tradesData
                  ? 'No recent executions match the selected filter.'
                  : 'Protocol-wide fills and liquidations will appear here once /api/insights/v1/execution/trades is available.'
              }
            />
          )}
        </Panel>
      </section>

      <div className="flex flex-wrap items-center justify-between gap-4 border-t border-brand-border/20 pt-4 text-sm font-semibold">
        <Link to="/keepers" className="text-brand-peach hover:underline">
          Inspect permissionless on-chain keepers →
        </Link>
        <div className="flex flex-wrap gap-4">
          <Link to="/divisions" className="text-content-secondary hover:text-brand-peach">
            Live Divisions rankings →
          </Link>
          <Link to="/" className="text-content-secondary hover:text-brand-peach">
            ← Back to Protocol Pulse
          </Link>
        </div>
      </div>
    </div>
  )
}
