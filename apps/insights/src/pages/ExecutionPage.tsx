import { useState } from 'react'
import { Link } from 'react-router-dom'
import {
  InsightsApiError,
  formatRateBps,
  transactionPath,
  useExecutionQuality,
  useExecutionTrades,
  useMarketFadRisk,
  type ProtocolTradeRecord,
  type TransactionRouteState,
} from '../api'
import { ArbiscanButton } from '../components/ActionButtons'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { FadLiquidationWatchlistSection } from '../components/FadLiquidationWatchlistSection'
import { TraderIdentity } from '../components/TraderIdentity'
import {
  ExecutionLatencyWhisker,
  OrderOutcomeProportionCard,
} from '../components/ExecutionVisuals'
import { OrderProtectionsInlineChecklist } from '../components/OrderProtectionsChecklist'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel, Pnl } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { arbiscanLinkKey, resolvePerpsChain } from '../utils/chain'
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

export function ExecutionPage() {
  const protocol = useProtocolContext()
  const qualityQuery = useExecutionQuality()
  const tradesQuery = useExecutionTrades(30)
  const fadRiskQuery = useMarketFadRisk()
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

      <header className="max-w-3xl space-y-3">
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

      <section aria-labelledby="execution-kpis-title">
        <h2 id="execution-kpis-title" className="sr-only">
          Execution quality snapshot
        </h2>
        <Panel className="p-4 sm:p-6">
          {qualityQuery.isLoading ? (
            <LoadingState rows={3} />
          ) : !quality ? (
            <div className="flex flex-wrap items-center justify-between gap-3 font-mono text-xs text-content-tertiary">
              <span>24h volume &amp; order reliability telemetry: <strong className="text-content-secondary">Unavailable</strong></span>
              <span>TP/SL protections &amp; liquidations: <strong className="text-content-secondary">Unavailable</strong></span>
            </div>
          ) : (
            <>
              <div className="grid gap-5 lg:grid-cols-12 lg:items-center">
                {/* Primary Hero: 24h Notional Volume & Active Scope */}
                <div className="space-y-3 lg:col-span-6 lg:border-r lg:border-brand-border/20 lg:pr-6">
                  <div>
                    <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                      24h Notional Volume
                    </p>
                    <p className="mt-1 font-mono text-3xl font-bold tracking-tight tabular-nums text-content-primary sm:text-4xl">
                      {formatCompactAssets(quality.volume24hAssets)}
                    </p>
                  </div>
                  <div className="flex flex-wrap items-center gap-x-4 gap-y-1 border-t border-brand-border/15 pt-2.5 font-mono text-xs tabular-nums text-content-tertiary">
                    <span>
                      <strong className="text-content-primary">
                        {quality.protections.created24h.toLocaleString('en-US')}
                      </strong>{' '}
                      TP/SL created (24h)
                    </span>
                    <span>
                      <strong className="text-content-primary">
                        {quality.liquidations.watchedCandidates.toLocaleString('en-US')}
                      </strong>{' '}
                      liquidation watch
                    </span>
                    {cohort24h ? (
                      <span>
                        <strong className="text-content-primary">
                          {cohort24h.activeAccounts.toLocaleString('en-US')}
                        </strong>{' '}
                        active traders (24h)
                      </span>
                    ) : null}
                  </div>
                </div>

                {/* Secondary Operational Strip: Queue, TP/SL & Liquidations */}
                <dl className="grid grid-cols-1 gap-3 sm:grid-cols-3 lg:col-span-6 lg:grid-cols-1 lg:gap-0 lg:divide-y lg:divide-brand-border/15">
                  <div className="flex items-baseline justify-between gap-3 py-1 lg:py-2.5">
                    <dt className="text-xs text-content-tertiary">Pending keeper queue</dt>
                    <dd className="font-mono text-sm font-semibold tabular-nums text-content-primary">
                      {quality.pendingOrderQueue.toLocaleString('en-US')}
                    </dd>
                  </div>
                  <div className="flex flex-wrap items-baseline justify-between gap-x-3 gap-y-0.5 py-1 lg:py-2.5">
                    <dt className="text-xs text-content-tertiary">Active TP/SL protections</dt>
                    <dd className="text-right font-mono tabular-nums">
                      <span className="text-sm font-semibold text-content-primary">
                        {quality.protections.activeProtections.toLocaleString('en-US')}
                      </span>
                      <span className="ml-2 text-[11px] text-content-tertiary">
                        {String(quality.protections.triggered24h)} trig ·{' '}
                        {String(quality.protections.cancelled24h)} cancel ·{' '}
                        {String(quality.protections.expired24h)} exp
                      </span>
                    </dd>
                  </div>
                  <div className="flex flex-wrap items-baseline justify-between gap-x-3 gap-y-0.5 py-1 lg:py-2.5">
                    <dt className="text-xs text-content-tertiary">Liquidations (24h)</dt>
                    <dd className="text-right font-mono tabular-nums">
                      <span
                        className={`text-sm font-semibold ${
                          quality.liquidations.liquidations24h > 0
                            ? 'text-brand-yellow'
                            : 'text-content-primary'
                        }`}
                      >
                        {quality.liquidations.liquidations24h.toLocaleString('en-US')}
                      </span>
                      <span className="ml-2 text-[11px] text-content-tertiary">
                        Bounties {formatCompactAssets(quality.liquidations.bountiesPaid24hAssets)}
                      </span>
                    </dd>
                  </div>
                </dl>
              </div>
            </>
          )}
        </Panel>
      </section>

      <FadLiquidationWatchlistSection
        data={fadRiskQuery.data}
        isLoading={fadRiskQuery.isLoading}
        isError={fadRiskQuery.isError}
        error={fadRiskQuery.error}
        onRetry={() => void fadRiskQuery.refetch()}
      />

      <section aria-labelledby="cohorts-title" className="space-y-4">
        <div className="flex flex-wrap items-baseline justify-between gap-2">
          <div>
            <h2 id="cohorts-title" className="text-xl font-semibold">
              Canonical order reliability &amp; latency
            </h2>
            <p className="mt-1 text-xs text-content-tertiary">
              Half-open commitment windows shifted 120s before observation · Expirations count
              canonical terminal reason 2
            </p>
          </div>
        </div>

        {quality && quality.orderReliability.length > 0 ? (
          <div className="grid gap-4 lg:grid-cols-2">
            <Panel className="space-y-3 p-5">
              <div>
                <h3 className="text-base font-semibold">Order outcome breakdown</h3>
                <p className="mt-0.5 text-xs text-content-tertiary">
                  Executed vs. expired (r=2) and failed orders across 1h &amp; 24h cohorts
                </p>
              </div>
              <div className="space-y-3">
                {quality.orderReliability.map((cohort) => (
                  <OrderOutcomeProportionCard key={cohort.windowSeconds} cohort={cohort} />
                ))}
              </div>
            </Panel>

            <ExecutionLatencyWhisker
              seconds={quality.executionLatency.seconds}
              blocks={quality.executionLatency.blocks}
            />
          </div>
        ) : null}

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
                    <th scope="col" className="px-4 py-3.5">Protections</th>
                    <th scope="col" className="px-4 py-3.5">Latency</th>
                    <th scope="col" className="px-4 py-3.5">Receipt</th>
                    <th scope="col" className="px-4 py-3.5">Tx</th>
                  </tr>
                </thead>
                <tbody>
                  {(() => {
                    const seenTxKeys = new Set<string>()
                    return visibleTrades.map((trade) => {
                      const hasReceipt =
                        trade.execution != null &&
                        (trade.activityType === 'Open' || trade.activityType === 'Close')
                      const txKey = arbiscanLinkKey('tx', trade.txHash, chainId)
                      const showArbiscan = !seenTxKeys.has(txKey)
                      if (showArbiscan) seenTxKeys.add(txKey)
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
                          <TraderIdentity address={trade.account} avatarSize={16} showCopy />
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
                        <td className="px-4 py-3.5">
                          {trade.activityType === 'Open' ? (
                            <OrderProtectionsInlineChecklist protections={trade.orderProtections} />
                          ) : (
                            <span className="font-mono text-xs text-content-tertiary">—</span>
                          )}
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                          {trade.latencySeconds != null ? `${String(trade.latencySeconds)}s` : '—'}
                        </td>
                        <td className="px-4 py-3.5">
                          <div className="flex flex-wrap items-center gap-1.5">
                            <Link
                              to={transactionPath(trade.txHash, trade.id)}
                              state={
                                {
                                  trade,
                                  chainId,
                                  releaseVersion,
                                  from: '/execution',
                                  fromLabel: 'Back to Execution',
                                } satisfies TransactionRouteState
                              }
                              className="inline-block border border-brand-border/40 px-2.5 py-1 text-xs font-semibold text-brand-peach transition-colors hover:bg-brand-peach/10"
                            >
                              {hasReceipt ? 'View breakdown' : 'View details'}
                            </Link>
                            <Link
                              to={`/wallets/${trade.account.toLowerCase()}?tab=exposure`}
                              className="inline-block border border-brand-border/25 px-2 py-1 text-xs font-medium text-content-secondary transition-colors hover:border-brand-peach/50 hover:text-brand-peach"
                              title={`Inspect ${shortAddress(trade.account)} market exposure & position lifecycles`}
                            >
                              Exposure →
                            </Link>
                          </div>
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs">
                          <div className="inline-flex items-center gap-1.5 text-content-secondary">
                            <span title={trade.txHash}>{shortAddress(trade.txHash)}</span>
                            {showArbiscan ? (
                              <ArbiscanButton kind="tx" value={trade.txHash} chainId={chainId} />
                            ) : null}
                          </div>
                        </td>
                      </tr>
                    )
                  })})()}
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
    </div>
  )
}
