import { Link, useParams } from 'react-router-dom'
import { InsightsApiError, isValidOracleUpdateId, useOracleUpdateDetail } from '../api'
import { CopyButton } from '../components/ActionButtons'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { PageBreadcrumbBar } from '../components/PageBreadcrumbBar'
import { OracleBasketPieChart } from '../components/OracleBasketPieChart'
import { OraclePriceLink } from '../components/OraclePriceLink'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { resolvePerpsChain } from '../utils/chain'
import {
  formatPrice,
  formatTimeSecondsUtc,
  formatUtcWithSeconds,
  shortAddress,
} from '../utils/format'

const CURRENCY_DOT_COLORS: Record<string, string> = {
  'EUR/USD': '#FFB78B',
  'JPY/USD': '#7CC4FA',
  'GBP/USD': '#6EE7B7',
  'CAD/USD': '#FCD34D',
  'SEK/USD': '#C4B5FD',
  'CHF/USD': '#FB7185',
}

function statusTone(status: string | undefined): StatusTone {
  if (status === 'admitted') return 'live'
  if (status === 'warning') return 'degraded'
  if (status === 'degraded') return 'blocked'
  return 'unknown'
}

function statusLabel(status: string | undefined): string {
  if (status === 'admitted') return 'Admitted'
  if (status === 'warning') return 'Spread warning'
  if (status === 'degraded') return 'Guard alert'
  return 'Unknown'
}

function checkTone(status: string): StatusTone {
  if (status === 'passed') return 'live'
  if (status === 'warning') return 'degraded'
  if (status === 'failed') return 'blocked'
  return 'unknown'
}

function checkGlyph(status: string): string {
  if (status === 'passed') return '✓'
  if (status === 'warning') return '!'
  if (status === 'failed') return '✕'
  return '○'
}

function sourceLabel(source: string): string {
  if (source === 'backend_hermes_latest_v2') return 'Latest Stream (Hermes v2)'
  if (source === 'backend_hermes') return 'Latest Stream (Legacy)'
  if (source.includes('reveal')) return 'Reveal Backfill'
  return source
}

function formatEightDec(raw: string | null | undefined, digits = 5): string {
  if (!raw || !/^\d+$/.test(raw)) return '—'
  const num = Number(raw) / 1e8
  if (!Number.isFinite(num)) return '—'
  return num.toFixed(digits)
}

function formatRawPythPrice(raw: string, exponent: number | null): string {
  if (!/^-?\d+$/.test(raw)) return raw
  if (exponent == null || exponent === 0) return raw
  const num = Number(raw) * 10 ** exponent
  if (!Number.isFinite(num)) return raw
  return num.toFixed(Math.min(6, Math.max(2, Math.abs(exponent) - 3)))
}

function formatSignedBps(bps: number | null | undefined): string {
  if (bps == null || !Number.isFinite(bps)) return '—'
  const sign = bps > 0 ? '+' : ''
  return `${sign}${String(bps)} bps`
}

function formatSignedPercentFromBps(bps: number | null | undefined): string {
  if (bps == null || !Number.isFinite(bps)) return '—'
  const sign = bps > 0 ? '+' : ''
  return `${sign}${(bps / 100).toFixed(2)}%`
}

function shortFeedId(feedId: string): string {
  if (feedId.length <= 16) return feedId
  return `${feedId.slice(0, 10)}…${feedId.slice(-6)}`
}

export function OracleUpdateDetailPage() {
  const { id = 'latest' } = useParams<{ id: string }>()
  const protocol = useProtocolContext()
  const isValid = isValidOracleUpdateId(id)
  const detailQuery = useOracleUpdateDetail(id)
  const data = detailQuery.data

  const isNotFound =
    detailQuery.error instanceof InsightsApiError && detailQuery.error.status === 404

  const chainId = data?.network.chainId ?? protocol.chainId
  const chain = resolvePerpsChain(chainId)
  const releaseVersion = data?.network.releaseVersion ?? protocol.releaseVersion

  const bannerTone: StatusTone = !isValid
    ? 'blocked'
    : isNotFound
      ? 'unknown'
      : detailQuery.isError
        ? 'blocked'
        : data
          ? statusTone(data.update.status)
          : 'unknown'

  const bannerLabel = !isValid
    ? 'Invalid oracle update identifier'
    : isNotFound
      ? 'Oracle update not found'
      : detailQuery.isError
        ? 'Oracle update telemetry unavailable'
        : data
          ? `Oracle Update #${data.update.id} · ${statusLabel(data.update.status)}`
          : 'Loading oracle price update'

  const maxConfLimit = data?.thresholds.maxConfidenceRatioBps ?? 50
  const adverseMultText =
    data?.thresholds.adverseConfidenceMultiplierBps != null
      ? `${(data.thresholds.adverseConfidenceMultiplierBps / 10000).toFixed(2)}x adverse mult`
      : null

  return (
    <div className="space-y-6 sm:space-y-8">
      <PageBreadcrumbBar
        items={[
          { label: 'Markets', to: '/execution' },
          { label: 'Oracle', to: '/oracle' },
          { label: data ? `Update #${data.update.id}` : id === 'latest' ? 'Latest Update' : `Update #${id}` },
        ]}
        rightSlot={
          data ? (
            <nav
              aria-label="Adjacent oracle updates"
              className="flex flex-wrap items-center gap-1.5 font-mono text-xs"
            >
              {data.navigation.previousId ? (
                <Link
                  to={`/oracle/updates/${data.navigation.previousId}`}
                  className="border border-brand-border/35 bg-surface-panel px-2 py-1 text-content-secondary transition-colors hover:border-brand-peach/60 hover:text-brand-peach"
                >
                  ← Prev #{data.navigation.previousId}
                </Link>
              ) : (
                <span className="border border-brand-border/20 px-2 py-1 text-content-tertiary opacity-50">
                  ← Prev
                </span>
              )}

              {data.navigation.nextId ? (
                <>
                  <Link
                    to={`/oracle/updates/${data.navigation.nextId}`}
                    className="border border-brand-border/35 bg-surface-panel px-2 py-1 text-content-secondary transition-colors hover:border-brand-peach/60 hover:text-brand-peach"
                  >
                    Next #{data.navigation.nextId} →
                  </Link>
                  <Link
                    to="/oracle/updates/latest"
                    className="border border-brand-peach/45 bg-brand-peach/10 px-2 py-1 font-semibold text-brand-peach transition-colors hover:bg-brand-peach/20"
                  >
                    Latest
                  </Link>
                </>
              ) : (
                <span className="border border-positive/40 bg-positive/10 px-2 py-1 font-semibold text-positive">
                  Latest update
                </span>
              )}
            </nav>
          ) : undefined
        }
      />

      <DataFreshnessBanner
        tone={bannerTone}
        statusLabel={bannerLabel}
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={null}
        observedAt={data?.observedAt ?? protocol.observedAt}
        note="Signed 6-currency Pyth Hermes FX basket snapshot & component weight forensics"
      />

      {!isValid ? (
        <ErrorState
          title="Invalid oracle update ID"
          message={`"${id}" is not a valid oracle update payload ID. Use a positive integer ID or "latest".`}
        />
      ) : detailQuery.isLoading ? (
        <Panel>
          <LoadingState rows={5} />
        </Panel>
      ) : isNotFound ? (
        <Panel className="p-6">
          <EmptyState
            title={`Oracle update #${id} was not found`}
            message="This payload ID does not exist in the admitted Pyth basket ledger on this network. Inspect the latest admitted update or return to Oracle Health."
          />
          <div className="mt-4 flex justify-center gap-3 font-mono text-xs">
            <Link
              to="/oracle/updates/latest"
              className="border border-brand-peach/50 bg-brand-peach/15 px-3 py-1.5 font-semibold text-brand-peach hover:bg-brand-peach/25"
            >
              Open Latest Update →
            </Link>
            <Link
              to="/oracle"
              className="border border-brand-border/35 px-3 py-1.5 text-content-secondary hover:text-content-primary"
            >
              Oracle Health
            </Link>
          </div>
        </Panel>
      ) : detailQuery.isError ? (
        <ErrorState
          title="Unable to load oracle price update"
          message={detailQuery.error.message}
          onRetry={() => void detailQuery.refetch()}
        />
      ) : data ? (
        <>
          <header className="space-y-3">
            <div className="flex flex-wrap items-center gap-2.5">
              <p className="font-mono text-xs font-semibold uppercase tracking-[0.16em] text-brand-peach">
                Oracle Price Update · Payload #{data.update.id}
              </p>
              <StatusPill
                tone={statusTone(data.update.status)}
                label={statusLabel(data.update.status)}
              />
              <span className="border border-brand-border/30 bg-surface-panel px-2 py-0.5 font-mono text-[11px] text-content-secondary">
                {sourceLabel(data.update.source)}
              </span>
            </div>

            <div className="flex flex-wrap items-baseline justify-between gap-4">
              <div className="flex flex-wrap items-baseline gap-3">
                <h1 className="font-mono text-2xl font-bold tracking-tight tabular-nums sm:text-3xl">
                  {data.update.basketPrice ? `${formatPrice(data.update.basketPrice)} plDXY` : 'plDXY —'}
                </h1>
                {data.update.displayDeltaBps != null ? (
                  <span
                    className={`border px-2 py-0.5 font-mono text-xs font-semibold tabular-nums ${
                      data.update.displayDeltaBps > 0
                        ? 'border-positive/40 bg-positive/10 text-positive'
                        : data.update.displayDeltaBps < 0
                          ? 'border-brand-orange/40 bg-brand-orange/10 text-brand-orange'
                          : 'border-brand-border/30 bg-surface-panel text-content-secondary'
                    }`}
                  >
                    {formatSignedBps(data.update.displayDeltaBps)} vs prev
                  </span>
                ) : null}
              </div>

              <div className="flex flex-wrap items-center gap-x-4 gap-y-1 font-mono text-xs text-content-tertiary">
                <span>
                  Published:{' '}
                  <strong className="font-medium text-content-secondary">
                    {formatUtcWithSeconds(data.update.maxPublishTime)}
                  </strong>
                </span>
                <span>
                  Ingested:{' '}
                  <strong className="font-medium text-content-secondary">
                    {formatUtcWithSeconds(data.update.fetchedAt)}
                  </strong>
                </span>
              </div>
            </div>

            <p className="max-w-3xl text-sm text-content-secondary">
              Atomic 6-currency Pyth Hermes basket payload admitted into the Plether oracle ledger
              (publish window{' '}
              <span className="font-mono text-xs text-content-primary">
                {formatTimeSecondsUtc(data.update.minPublishTime)} →{' '}
                {formatTimeSecondsUtc(data.update.maxPublishTime)} UTC
              </span>
              , {String(data.update.vaaCount)} signed VAA payload
              {data.update.vaaCount === 1 ? '' : 's'}).
            </p>
          </header>

          {/* Unified Oracle Update Hero Panel */}
          <Panel className="overflow-hidden">
            <div className="grid gap-5 p-4 sm:p-6 lg:grid-cols-12 lg:items-center">
              <div className="lg:col-span-6 lg:border-r lg:border-brand-border/20 lg:pr-6">
                <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                  plDXY Display Mark
                </p>
                <p
                  className={`mt-1 font-mono text-3xl font-bold tracking-tight tabular-nums sm:text-4xl ${
                    data.update.displayDeltaBps != null && data.update.displayDeltaBps > 0
                      ? 'text-positive'
                      : data.update.displayDeltaBps != null && data.update.displayDeltaBps < 0
                        ? 'text-brand-orange'
                        : 'text-content-primary'
                  }`}
                >
                  {data.update.basketPrice ? formatPrice(data.update.basketPrice) : 'Unavailable'}
                </p>
                <p className="mt-1 font-mono text-xs text-content-tertiary">
                  {data.update.basketPrice
                    ? `Raw basket ${formatEightDec(data.update.basketPrice)} · Cap ${formatEightDec(data.update.displayPriceCap, 2)}`
                    : 'Raw basket unavailable'}
                </p>
              </div>

              <div className="space-y-3 lg:col-span-6">
                <div className="space-y-1.5">
                  <div className="flex flex-wrap items-baseline justify-between gap-2 font-mono text-xs">
                    <span className="text-content-tertiary">6-Feed Publish Spread</span>
                    <span
                      className={`font-semibold tabular-nums ${
                        data.update.spreadSeconds > data.thresholds.maxComponentDivergenceSeconds
                          ? 'text-brand-orange'
                          : 'text-positive'
                      }`}
                    >
                      {String(data.update.spreadSeconds)}s /{' '}
                      {String(data.thresholds.maxComponentDivergenceSeconds)}s
                      <span className="ml-2 font-normal text-content-tertiary">
                        · lag {String(data.update.lagSeconds)}s · {String(data.update.vaaCount)} VAA
                        {data.update.vaaCount === 1 ? '' : 's'}
                      </span>
                    </span>
                  </div>
                  <div className="h-2 w-full overflow-hidden border border-brand-border/20 bg-app-bg">
                    <div
                      className={
                        data.update.spreadSeconds > data.thresholds.maxComponentDivergenceSeconds
                          ? 'h-full bg-brand-orange'
                          : 'h-full bg-brand-peach'
                      }
                      style={{
                        width: `${String(
                          data.thresholds.maxComponentDivergenceSeconds > 0
                            ? Math.min(
                                100,
                                Math.round(
                                  (data.update.spreadSeconds /
                                    data.thresholds.maxComponentDivergenceSeconds) *
                                    100,
                                ),
                              )
                            : 0,
                        )}%`,
                      }}
                    />
                  </div>
                </div>

                <dl className="grid grid-cols-1 gap-3 border-t border-brand-border/15 pt-3 font-mono text-xs sm:grid-cols-2">
                  <div>
                    <dt className="text-content-tertiary">Peak Component Conf</dt>
                    <dd
                      className={`mt-0.5 text-base font-bold tabular-nums ${
                        data.update.maxComponentConfidenceBps != null &&
                        data.update.maxComponentConfidenceBps > maxConfLimit
                          ? 'text-brand-orange'
                          : 'text-positive'
                      }`}
                    >
                      {data.update.maxComponentConfidenceBps != null
                        ? `${String(data.update.maxComponentConfidenceBps)} bps`
                        : 'Unavailable'}
                    </dd>
                    <p className="text-[11px] text-content-tertiary">
                      Limit {String(maxConfLimit)} bps{adverseMultText ? ` · ${adverseMultText}` : ''}
                    </p>
                  </div>
                  <div>
                    <dt className="text-content-tertiary">Orders Settled on Update</dt>
                    <dd className="mt-0.5 text-base font-bold tabular-nums text-content-primary">
                      {data.settledOrders.length.toLocaleString('en-US')}
                    </dd>
                    <p className="text-[11px] text-content-tertiary">
                      {data.settledOrders.length > 0
                        ? `Matched on (${formatTimeSecondsUtc(data.update.minPublishTime)}–${formatTimeSecondsUtc(data.update.maxPublishTime)})`
                        : 'No orders finalized on this tick'}
                    </p>
                  </div>
                </dl>
              </div>
            </div>
          </Panel>

          <section
            aria-label="Basket weights pie chart and value-guard verification"
            className="grid gap-6 lg:grid-cols-12"
          >
            <Panel className="min-w-0 lg:col-span-7">
              <OracleBasketPieChart
                components={data.components}
                basketPrice={data.update.basketPrice}
                reconstructedBasketPrice={data.update.reconstructedBasketPrice}
              />
            </Panel>

            <div className="min-w-0 space-y-6 lg:col-span-5">
              <Panel className="p-4 sm:p-5">
                <div className="flex flex-wrap items-center justify-between gap-2">
                  <h2 className="text-base font-semibold text-content-primary">
                    Basket Transform &amp; Verification
                  </h2>
                  {data.update.reconstructionDiffRaw != null ? (
                    <StatusPill
                      tone={data.update.reconstructionDiffRaw <= 6 ? 'live' : 'degraded'}
                      label={
                        data.update.reconstructionDiffRaw === 0
                          ? 'Exact sum match'
                          : `±${String(data.update.reconstructionDiffRaw)} raw diff`
                      }
                    />
                  ) : null}
                </div>

                <div className="mt-3 border border-brand-border/20 bg-surface-canvas/70 px-3 py-2.5 font-mono text-xs text-content-secondary">
                  <div className="text-[11px] uppercase tracking-[0.12em] text-content-tertiary">
                    Immutable v1 Display Formula
                  </div>
                  <div className="mt-1 text-content-primary">
                    plDXY = {formatEightDec(data.update.displayPriceCap, 5)} −{' '}
                    {formatEightDec(data.update.basketPrice)} ={' '}
                    <span className="font-semibold text-brand-peach">
                      {formatPrice(data.update.basketPrice)}
                    </span>
                  </div>
                </div>

                <dl className="mt-3 grid grid-cols-2 gap-3 font-mono text-xs">
                  <div className="border border-brand-border/15 bg-surface-canvas/40 p-2.5">
                    <dt className="text-[11px] text-content-tertiary">Stored raw basket</dt>
                    <dd className="mt-0.5 font-semibold tabular-nums text-content-primary">
                      {formatEightDec(data.update.basketPrice)}
                    </dd>
                  </div>
                  <div className="border border-brand-border/15 bg-surface-canvas/40 p-2.5">
                    <dt className="text-[11px] text-content-tertiary">Σ 6-FX contributions</dt>
                    <dd className="mt-0.5 font-semibold tabular-nums text-content-primary">
                      {formatEightDec(data.update.reconstructedBasketPrice)}
                    </dd>
                  </div>
                  <div className="border border-brand-border/15 bg-surface-canvas/40 p-2.5">
                    <dt className="text-[11px] text-content-tertiary">Previous plDXY</dt>
                    <dd className="mt-0.5 font-semibold tabular-nums text-content-secondary">
                      {data.update.previousBasketPrice ? (
                        data.navigation.previousId ? (
                          <OraclePriceLink updateId={data.navigation.previousId}>
                            {formatPrice(data.update.previousBasketPrice)}
                          </OraclePriceLink>
                        ) : (
                          formatPrice(data.update.previousBasketPrice)
                        )
                      ) : (
                        '—'
                      )}
                    </dd>
                  </div>
                  <div className="border border-brand-border/15 bg-surface-canvas/40 p-2.5">
                    <dt className="text-[11px] text-content-tertiary">Target weight sum</dt>
                    <dd className="mt-0.5 font-semibold tabular-nums text-content-secondary">
                      {(data.update.totalTargetWeightBps / 100).toFixed(2)}%
                    </dd>
                  </div>
                </dl>
              </Panel>

              <Panel>
                <div className="flex items-center justify-between border-b border-brand-border/20 px-4 py-3 sm:px-5">
                  <h2 className="text-base font-semibold text-content-primary">
                    Snapshot Guard Checks
                  </h2>
                  <span className="font-mono text-xs text-content-tertiary">
                    {data.admissionChecks.filter((c) => c.status === 'passed').length}/
                    {data.admissionChecks.length} passed
                  </span>
                </div>
                <ul className="divide-y divide-brand-border/15" aria-label="Snapshot admission checks">
                  {data.admissionChecks.map((check) => (
                    <li
                      key={check.key}
                      className="flex flex-wrap items-center justify-between gap-2 px-4 py-2.5 sm:px-5"
                    >
                      <div className="flex min-w-0 items-center gap-2.5">
                        <span
                          aria-hidden="true"
                          className={`inline-flex h-5 w-5 shrink-0 items-center justify-center border font-mono text-[11px] font-bold ${
                            check.status === 'passed'
                              ? 'border-positive/50 bg-positive/15 text-positive'
                              : check.status === 'warning'
                                ? 'border-brand-yellow/55 bg-brand-yellow/15 text-brand-yellow'
                                : check.status === 'failed'
                                  ? 'border-brand-orange/60 bg-brand-orange/15 text-brand-orange'
                                  : 'border-brand-border/35 text-content-tertiary'
                          }`}
                        >
                          {checkGlyph(check.status)}
                        </span>
                        <div className="min-w-0">
                          <div className="text-xs font-semibold text-content-primary">
                            {check.label}
                          </div>
                          <div className="font-mono text-[11px] text-content-tertiary">
                            {check.threshold}
                          </div>
                        </div>
                      </div>
                      <div className="flex shrink-0 items-center gap-2 font-mono text-xs">
                        <span className="text-content-primary">{check.currentValue}</span>
                        <StatusPill tone={checkTone(check.status)} label={check.status} />
                      </div>
                    </li>
                  ))}
                </ul>
              </Panel>
            </div>
          </section>

          <section aria-labelledby="update-components-title" className="space-y-4">
            <div className="flex flex-wrap items-baseline justify-between gap-2">
              <div>
                <h2 id="update-components-title" className="text-xl font-semibold">
                  6-Feed Pyth FX Component Breakdown
                </h2>
                <p className="mt-1 text-xs text-content-tertiary">
                  Signed Pyth price points, inverted FX normalization, base-price drift, basket
                  value contribution, and per-feed confidence vs {String(maxConfLimit)} bps limit
                </p>
              </div>
              <span className="font-mono text-xs text-content-tertiary">
                {String(data.components.length)} / 6 feeds persisted
              </span>
            </div>

            <Panel>
              {data.components.length > 0 ? (
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[980px] text-left text-sm">
                    <caption className="sr-only">
                      Signed Pyth FX component prices and weights for oracle update #{data.update.id}
                    </caption>
                    <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                      <tr>
                        <th scope="col" className="px-4 py-3.5">Pair</th>
                        <th scope="col" className="px-4 py-3.5">Pyth Feed</th>
                        <th scope="col" className="px-4 py-3.5">Target / Eff Share</th>
                        <th scope="col" className="px-4 py-3.5">Normalized FX</th>
                        <th scope="col" className="px-4 py-3.5">vs Base</th>
                        <th scope="col" className="px-4 py-3.5">Contribution</th>
                        <th scope="col" className="px-4 py-3.5">Confidence</th>
                        <th scope="col" className="px-4 py-3.5">Publish Time</th>
                        <th scope="col" className="px-4 py-3.5">Pyth Feed ID</th>
                      </tr>
                    </thead>
                    <tbody>
                      {data.components.map((comp) => {
                        const pct =
                          comp.confidenceBps != null && maxConfLimit > 0
                            ? Math.min(100, Math.round((comp.confidenceBps / maxConfLimit) * 100))
                            : 0
                        const driftExceeded =
                          comp.driftFromMinSeconds != null &&
                          comp.driftFromMinSeconds > data.thresholds.maxComponentDivergenceSeconds
                        const dotColor = CURRENCY_DOT_COLORS[comp.symbol] ?? '#FFB78B'
                        return (
                          <tr
                            key={comp.feedId}
                            className="border-b border-brand-border/15 last:border-0"
                          >
                            <td className="px-4 py-3.5">
                              <div className="flex items-center gap-2 font-mono text-xs font-semibold text-content-primary">
                                <span
                                  className="h-2.5 w-2.5 shrink-0"
                                  style={{ backgroundColor: dotColor }}
                                  aria-hidden="true"
                                />
                                <span>{comp.symbol}</span>
                              </div>
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs text-content-secondary">
                              <div>
                                {comp.feedSymbol}
                                {comp.inverted ? ' (inv)' : ''}
                              </div>
                              <div className="text-[11px] text-content-tertiary">
                                Raw: {formatRawPythPrice(comp.rawPrice, comp.exponent)}
                              </div>
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                              <div className="font-semibold text-content-primary">
                                {comp.effectiveShareBps != null
                                  ? `${(comp.effectiveShareBps / 100).toFixed(2)}%`
                                  : '—'}
                              </div>
                              <div className="text-[11px] text-content-tertiary">
                                Target:{' '}
                                {comp.weightBps != null
                                  ? `${(comp.weightBps / 100).toFixed(1)}%`
                                  : '—'}
                              </div>
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                              <div className="font-semibold text-content-primary">
                                {formatEightDec(comp.price)}
                              </div>
                              <div className="text-[11px] text-content-tertiary">
                                Base: {formatEightDec(comp.basePrice)}
                              </div>
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs font-semibold tabular-nums">
                              <span
                                className={
                                  comp.changeFromBaseBps != null
                                    ? comp.changeFromBaseBps > 0
                                      ? 'text-positive'
                                      : comp.changeFromBaseBps < 0
                                        ? 'text-brand-orange'
                                        : 'text-content-secondary'
                                    : 'text-content-tertiary'
                                }
                              >
                                {formatSignedPercentFromBps(comp.changeFromBaseBps)}
                              </span>
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs tabular-nums text-content-primary">
                              {formatEightDec(comp.contribution)}
                            </td>
                            <td className="px-4 py-3.5">
                              <div className="flex items-center gap-2 font-mono text-xs tabular-nums">
                                <span
                                  className={
                                    comp.confidenceExceeded
                                      ? 'font-semibold text-brand-orange'
                                      : 'text-content-primary'
                                  }
                                >
                                  {comp.confidenceBps != null
                                    ? `${String(comp.confidenceBps)} bps`
                                    : '—'}
                                </span>
                                <div className="h-1.5 w-16 bg-brand-border/20">
                                  <div
                                    className={`h-full ${
                                      comp.confidenceExceeded ? 'bg-brand-orange' : 'bg-positive'
                                    }`}
                                    style={{ width: `${String(Math.max(pct, 4))}%` }}
                                  />
                                </div>
                              </div>
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                              <div className="text-content-secondary">
                                {formatTimeSecondsUtc(comp.publishTime)}
                              </div>
                              <div
                                className={`text-[11px] ${
                                  driftExceeded
                                    ? 'font-semibold text-brand-orange'
                                    : 'text-content-tertiary'
                                }`}
                              >
                                {comp.driftFromMinSeconds != null
                                  ? `+${String(comp.driftFromMinSeconds)}s drift`
                                  : '—'}
                              </div>
                            </td>
                            <td className="px-4 py-3.5">
                              <div className="flex items-center gap-2 font-mono text-xs text-content-tertiary">
                                <span>{shortFeedId(comp.feedId)}</span>
                                <CopyButton
                                  value={comp.feedId}
                                  ariaLabel={`Copy ${comp.symbol} Pyth feed ID`}
                                />
                              </div>
                            </td>
                          </tr>
                        )
                      })}
                    </tbody>
                  </table>
                </div>
              ) : (
                <EmptyState
                  title="Component prices unavailable"
                  message="This historical payload row did not store component FX snapshots."
                />
              )}
            </Panel>
          </section>

          <section aria-labelledby="update-orders-title" className="space-y-4">
            <div className="flex flex-wrap items-baseline justify-between gap-2">
              <div>
                <h2 id="update-orders-title" className="text-xl font-semibold">
                  Orders Settled With This Price Update
                </h2>
                <p className="mt-1 text-xs text-content-tertiary">
                  Perps orders finalized on-chain using this exact Pyth publish window (
                  {formatTimeSecondsUtc(data.update.minPublishTime)} →{' '}
                  {formatTimeSecondsUtc(data.update.maxPublishTime)} UTC)
                </p>
              </div>
              <span className="font-mono text-xs text-content-tertiary">
                {data.settledOrders.length.toLocaleString('en-US')} order
                {data.settledOrders.length === 1 ? '' : 's'}
              </span>
            </div>

            <Panel>
              {data.settledOrders.length > 0 ? (
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[780px] text-left text-sm">
                    <caption className="sr-only">
                      Perps orders settled with oracle price update #{data.update.id}
                    </caption>
                    <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                      <tr>
                        <th scope="col" className="px-4 py-3.5">Order</th>
                        <th scope="col" className="px-4 py-3.5">Trader</th>
                        <th scope="col" className="px-4 py-3.5">Side</th>
                        <th scope="col" className="px-4 py-3.5">Status</th>
                        <th scope="col" className="px-4 py-3.5">Execution Price</th>
                        <th scope="col" className="px-4 py-3.5">Oracle Mark</th>
                        <th scope="col" className="px-4 py-3.5">Finalized (UTC)</th>
                        <th scope="col" className="px-4 py-3.5">Transaction</th>
                      </tr>
                    </thead>
                    <tbody>
                      {data.settledOrders.map((order) => {
                        const sideLower = order.side?.toLowerCase()
                        const isLong = sideLower === 'long' || sideLower === 'bull'
                        const isShort = sideLower === 'short' || sideLower === 'bear'
                        return (
                          <tr
                            key={order.orderId}
                            className="border-b border-brand-border/15 last:border-0"
                          >
                            <td className="px-4 py-3.5 font-mono text-xs font-semibold text-content-primary">
                              #{order.orderId}
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs">
                              {order.account ? (
                                <Link
                                  to={`/wallets/${order.account}`}
                                  className="text-brand-peach hover:underline"
                                >
                                  {shortAddress(order.account)}
                                </Link>
                              ) : (
                                <span className="text-content-tertiary">—</span>
                              )}
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs font-semibold uppercase">
                              <span
                                className={
                                  isLong
                                    ? 'text-positive'
                                    : isShort
                                      ? 'text-brand-orange'
                                      : 'text-content-secondary'
                                }
                              >
                                {order.side ?? '—'}
                              </span>
                            </td>
                            <td className="px-4 py-3.5">
                              <StatusPill
                                tone={order.status === 'Executed' ? 'live' : 'degraded'}
                                label={order.status}
                              />
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs tabular-nums text-content-primary">
                              {order.executionPrice
                                ? `${formatPrice(order.executionPrice)} plDXY`
                                : '—'}
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs tabular-nums text-content-secondary">
                              {order.executionOraclePrice
                                ? `${formatPrice(order.executionOraclePrice)} plDXY`
                                : '—'}
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs text-content-secondary">
                              {order.terminalTimestamp
                                ? formatUtcWithSeconds(order.terminalTimestamp)
                                : '—'}
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs">
                              {order.txHash ? (
                                <Link
                                  to={`/tx/${order.txHash}`}
                                  className="text-brand-peach hover:underline"
                                >
                                  {shortAddress(order.txHash)} →
                                </Link>
                              ) : (
                                <span className="text-content-tertiary">—</span>
                              )}
                            </td>
                          </tr>
                        )
                      })}
                    </tbody>
                  </table>
                </div>
              ) : (
                <EmptyState
                  title="No orders settled on this oracle update"
                  message="BasketWorker continuously caches admitted Pyth updates every few seconds; orders only appear here when an on-chain keeper execution consumed this exact publish window."
                />
              )}
            </Panel>
          </section>
        </>
      ) : null}
    </div>
  )
}
