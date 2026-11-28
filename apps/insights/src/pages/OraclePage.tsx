import { Link } from 'react-router-dom'
import { InsightsApiError, formatRateBps, useOracleHealth } from '../api'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { OracleHealthBlocks } from '../components/OracleHealthBlocks'
import { OraclePriceLink } from '../components/OraclePriceLink'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { resolvePerpsChain } from '../utils/chain'
import { formatPrice, formatUtc, formatUtcWithSeconds } from '../utils/format'

function overallTone(status: string | undefined): StatusTone {
  if (status === 'healthy') return 'live'
  if (status === 'stale' || status === 'degraded') return 'degraded'
  if (status === 'frozen') return 'blocked'
  return 'unknown'
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

function formatNormalizedFxPrice(raw: string): string {
  if (!/^\d+$/.test(raw)) return raw
  const num = Number(raw) / 1e8
  if (!Number.isFinite(num)) return raw
  return num.toFixed(5)
}

function formatReasonLabel(code: string): string {
  return code
    .split('_')
    .map((part) => (part ? part[0].toUpperCase() + part.slice(1) : ''))
    .join(' ')
}

export function OraclePage() {
  const protocol = useProtocolContext()
  const oracleQuery = useOracleHealth()
  const data = oracleQuery.data
  const awaitingDeploy =
    oracleQuery.error instanceof InsightsApiError && oracleQuery.error.status === 404

  const chainId = data?.network.chainId ?? protocol.chainId
  const chain = resolvePerpsChain(chainId)
  const releaseVersion = data?.network.releaseVersion ?? protocol.releaseVersion

  const bannerTone: StatusTone = awaitingDeploy
    ? 'unknown'
    : oracleQuery.isError
      ? 'blocked'
      : data
        ? overallTone(data.overallStatus)
        : 'unknown'

  const bannerLabel = awaitingDeploy
    ? 'Awaiting Oracle Health backend deploy'
    : oracleQuery.isError
      ? 'Oracle telemetry unavailable'
      : data
        ? `Oracle ${data.overallStatus} · ${data.regime} regime`
        : 'Loading oracle telemetry'

  const maxConfLimit = data?.thresholds.maxConfidenceRatioBps ?? 50

  return (
    <div className="space-y-8">
      <DataFreshnessBanner
        tone={bannerTone}
        statusLabel={bannerLabel}
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={null}
        observedAt={data?.observedAt ?? protocol.observedAt}
        note="PletherOracle 6-feed Pyth FX basket admission & value rejection monitors"
      />

      <header className="flex flex-wrap items-end justify-between gap-4">
        <div>
          <p className="text-xs font-semibold uppercase tracking-[0.16em] text-brand-peach">
            Oracle Admission &amp; Rejection Health
          </p>
          <h1 className="mt-1 text-2xl font-bold tracking-tight sm:text-3xl">
            Pyth FX Basket &amp; Value-Guard Telemetry
          </h1>
          <p className="mt-2 max-w-2xl text-sm text-content-secondary">
            Live verification of the 6-currency Pyth Hermes basket feeding{' '}
            <code className="font-mono text-xs text-content-primary">PletherOracle</code>,
            chronological admission health blocks, and every value-rejection guard enforced before
            prices enter execution.
          </p>
        </div>
        <div className="flex flex-wrap items-center gap-3 text-xs font-semibold">
          <Link
            to={`/oracle/updates/${data?.live.latestUpdateId ?? 'latest'}`}
            className="border border-brand-peach/50 bg-brand-peach/15 px-3 py-1.5 font-mono text-brand-peach transition-colors hover:bg-brand-peach/25"
          >
            Inspect latest update →
          </Link>
          <Link to="/execution" className="text-brand-peach hover:underline">
            Execution quality →
          </Link>
          <Link to="/infra" className="text-content-secondary hover:text-brand-peach hover:underline">
            Infrastructure →
          </Link>
        </div>
      </header>

      {oracleQuery.isLoading ? (
        <Panel>
          <LoadingState rows={4} />
        </Panel>
      ) : oracleQuery.isError && !awaitingDeploy ? (
        <ErrorState
          title="Unable to load oracle health"
          message={oracleQuery.error.message}
          onRetry={() => void oracleQuery.refetch()}
        />
      ) : awaitingDeploy ? (
        <Panel className="p-6">
          <EmptyState
            title="Oracle Health endpoint awaiting backend deployment"
            message="Deploy GET /api/insights/v1/oracle/health to inspect chronological Pyth basket admission blocks, value-rejection counts, and live 6-feed FX component checks."
          />
        </Panel>
      ) : data ? (
        <>
          <Panel className="p-4 sm:p-6">
            <div className="grid gap-5 lg:grid-cols-12 lg:items-center">
              {/* Left: Live plDXY Mark + 6-FX Feed Heartbeat Strip */}
              <div className="space-y-3 lg:col-span-6 lg:border-r lg:border-brand-border/20 lg:pr-6">
                <div className="flex flex-wrap items-center justify-between gap-2">
                  <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                    plDXY Live Mark
                  </p>
                  <StatusPill
                    tone={
                      data.regime === 'Live'
                        ? 'live'
                        : data.regime === 'Frozen'
                          ? 'blocked'
                          : 'degraded'
                    }
                    label={
                      data.live.ageSeconds != null
                        ? `${data.regime} · ${String(data.live.ageSeconds)}s old`
                        : data.regime
                    }
                  />
                </div>
                <div className="flex flex-wrap items-baseline justify-between gap-2">
                  {data.live.basketPrice ? (
                    <OraclePriceLink
                      updateId={data.live.latestUpdateId}
                      className={`font-mono text-3xl font-bold tracking-tight tabular-nums sm:text-4xl ${
                        data.regime === 'Live'
                          ? 'text-positive'
                          : data.regime === 'Frozen'
                            ? 'text-brand-orange'
                            : 'text-content-primary'
                      }`}
                    >
                      {formatPrice(data.live.basketPrice)}
                    </OraclePriceLink>
                  ) : (
                    <span className="font-mono text-3xl font-bold tracking-tight tabular-nums text-content-primary sm:text-4xl">
                      Unavailable
                    </span>
                  )}
                  {data.live.latestUpdateId ? (
                    <Link
                      to={`/oracle/updates/${data.live.latestUpdateId}`}
                      className="font-mono text-xs font-semibold text-brand-peach hover:underline"
                    >
                      Inspect basket weights →
                    </Link>
                  ) : null}
                </div>
                {data.components.length > 0 ? (
                  <div
                    className="grid grid-cols-3 gap-1.5 sm:grid-cols-6"
                    aria-label="6-feed FX component status"
                  >
                    {data.components.map((comp) => {
                      const dotClass = comp.confidenceExceeded
                        ? 'bg-brand-orange'
                        : comp.driftFromMinSeconds != null &&
                            comp.driftFromMinSeconds > data.thresholds.maxComponentDivergenceSeconds
                          ? 'bg-brand-yellow'
                          : 'bg-positive'
                      return (
                        <Link
                          key={comp.feedId}
                          to={`/oracle/updates/${data.live.latestUpdateId ?? 'latest'}`}
                          className="inline-flex items-center justify-center gap-1 border border-brand-border/25 bg-app-bg px-1.5 py-1 font-mono text-[11px] text-content-secondary transition-colors hover:border-brand-peach/50 hover:text-content-primary"
                        >
                          <span className={`h-1.5 w-1.5 shrink-0 rounded-full ${dotClass}`} />
                          <span className="font-semibold text-content-primary">
                            {comp.symbol.split('/')[0]}
                          </span>
                          {comp.weightBps != null ? (
                            <span className="text-content-tertiary">
                              {(comp.weightBps / 100).toFixed(1)}%
                            </span>
                          ) : null}
                        </Link>
                      )
                    })}
                  </div>
                ) : null}
              </div>

              {/* Right: Policy Headroom Gauges */}
              <div className="space-y-3.5 lg:col-span-6">
                <div className="space-y-1.5">
                  <div className="flex flex-wrap items-baseline justify-between gap-2 font-mono text-xs">
                    <span className="text-content-tertiary">24h Admission Rate</span>
                    <span className="tabular-nums">
                      <strong
                        className={
                          data.summary24h.rejectedUpdates > 0 ? 'text-brand-yellow' : 'text-positive'
                        }
                      >
                        {formatRateBps(data.summary24h.admissionRateBps)}
                      </strong>{' '}
                      <span className="text-content-tertiary">
                        · {data.summary24h.admittedUpdates.toLocaleString('en-US')} ok ·{' '}
                        {data.summary24h.rejectedUpdates.toLocaleString('en-US')} rej
                      </span>
                    </span>
                  </div>
                  <div className="flex h-2 w-full overflow-hidden border border-brand-border/20 bg-app-bg">
                    {(() => {
                      const total = data.summary24h.admittedUpdates + data.summary24h.rejectedUpdates
                      const okPct = total > 0 ? (data.summary24h.admittedUpdates / total) * 100 : 100
                      return (
                        <>
                          <div className="bg-positive" style={{ width: `${String(okPct)}%` }} />
                          {data.summary24h.rejectedUpdates > 0 ? (
                            <div
                              className="bg-brand-orange"
                              style={{ width: `${String(Math.max(1, 100 - okPct))}%` }}
                            />
                          ) : null}
                        </>
                      )
                    })()}
                  </div>
                  <div className="flex flex-wrap justify-between gap-2 font-mono text-[11px] text-content-tertiary">
                    <span>
                      {data.summary24h.latestAdmittedUpdates.toLocaleString('en-US')} latest ·{' '}
                      {data.summary24h.revealAdmittedUpdates.toLocaleString('en-US')} reveal
                    </span>
                    <span>
                      {data.summary24h.staleMarketClosedPolls.toLocaleString('en-US')} market-closed stale
                    </span>
                  </div>
                </div>

                <div className="space-y-1.5">
                  <div className="flex flex-wrap items-baseline justify-between gap-2 font-mono text-xs">
                    <span className="text-content-tertiary">24h Max 6-Feed Spread</span>
                    <span className="tabular-nums text-content-primary">
                      <strong>
                        {data.summary24h.maxSpreadSeconds != null
                          ? `${String(data.summary24h.maxSpreadSeconds)}s`
                          : '—'}
                      </strong>{' '}
                      <span className="text-content-tertiary">
                        / {String(data.thresholds.maxComponentDivergenceSeconds)}s limit
                        {data.summary24h.maxLagSeconds != null
                          ? ` · max lag ${String(data.summary24h.maxLagSeconds)}s`
                          : ''}
                      </span>
                    </span>
                  </div>
                  <div className="h-2 w-full overflow-hidden border border-brand-border/20 bg-app-bg">
                    <div
                      className={
                        data.summary24h.maxSpreadSeconds != null &&
                        data.summary24h.maxSpreadSeconds > data.thresholds.maxComponentDivergenceSeconds
                          ? 'h-full bg-brand-orange'
                          : 'h-full bg-brand-peach'
                      }
                      style={{
                        width: `${String(
                          data.summary24h.maxSpreadSeconds != null &&
                            data.thresholds.maxComponentDivergenceSeconds > 0
                            ? Math.min(
                                100,
                                Math.round(
                                  (data.summary24h.maxSpreadSeconds /
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
              </div>
            </div>
          </Panel>

          <OracleHealthBlocks
            window1h={data.windows.window1h.timeline}
            window24h={data.windows.window24h.timeline}
            divergenceLimitSeconds={data.thresholds.maxComponentDivergenceSeconds}
          />

          <section aria-labelledby="oracle-guards-title" className="space-y-4">
            <div className="flex flex-wrap items-baseline justify-between gap-2">
              <div>
                <h2 id="oracle-guards-title" className="text-xl font-semibold">
                  Value-Rejection &amp; Admission Guard Checklist
                </h2>
                <p className="mt-1 text-xs text-content-tertiary">
                  Every guard enforced by BasketWorker and PletherOracle before a Pyth FX update is
                  admitted for order execution or liquidations
                </p>
              </div>
              <span className="font-mono text-xs text-content-tertiary">
                {data.admissionChecks.filter((c) => c.status === 'passed').length}/
                {data.admissionChecks.length} checks passing
              </span>
            </div>

            <Panel>
              <ul className="divide-y divide-brand-border/15" aria-label="Oracle admission guards">
                {data.admissionChecks.map((check) => (
                  <li
                    key={check.key}
                    className="flex flex-col justify-between gap-3 px-5 py-3.5 sm:flex-row sm:items-center"
                  >
                    <div className="flex min-w-0 items-start gap-3">
                      <span
                        aria-hidden="true"
                        className={`mt-0.5 inline-flex h-5 w-5 shrink-0 items-center justify-center border font-mono text-xs font-bold ${
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
                        <div className="flex flex-wrap items-center gap-2">
                          <span className="text-sm font-semibold text-content-primary">
                            {check.label}
                          </span>
                          <StatusPill tone={checkTone(check.status)} label={check.status} />
                        </div>
                        <p className="mt-0.5 text-xs text-content-tertiary">{check.description}</p>
                      </div>
                    </div>
                    <div className="flex shrink-0 items-center gap-6 pl-8 font-mono text-xs sm:pl-0">
                      <div className="text-right">
                        <div className="text-content-primary">{check.currentValue}</div>
                        <div className="text-[11px] text-content-tertiary">{check.threshold}</div>
                      </div>
                      <div className="min-w-[84px] text-right">
                        <div
                          className={
                            check.rejections24h > 0
                              ? 'font-semibold text-brand-orange'
                              : 'text-content-tertiary'
                          }
                        >
                          {check.rejections24h.toLocaleString('en-US')} rej
                        </div>
                        <div className="text-[11px] text-content-tertiary">24h</div>
                      </div>
                    </div>
                  </li>
                ))}
              </ul>
            </Panel>
          </section>

          <section aria-labelledby="oracle-components-title" className="space-y-4">
            <div className="flex flex-wrap items-baseline justify-between gap-2">
              <div>
                <h2 id="oracle-components-title" className="text-xl font-semibold">
                  6-Feed Pyth FX Basket Components
                </h2>
                <p className="mt-1 text-xs text-content-tertiary">
                  Latest signed component prices, confidence-interval ratio vs{' '}
                  {String(maxConfLimit)} bps limit, and per-feed publish drift
                </p>
              </div>
              <div className="flex flex-wrap items-center gap-3 font-mono text-xs">
                {data.live.latestPublishTime ? (
                  <span className="text-content-tertiary">
                    Published {formatUtc(data.live.latestPublishTime)}
                  </span>
                ) : null}
                <Link
                  to={`/oracle/updates/${data.live.latestUpdateId ?? 'latest'}`}
                  className="font-semibold text-brand-peach hover:underline"
                >
                  Open basket pie chart &amp; detail →
                </Link>
              </div>
            </div>

            <Panel>
              {data.components.length > 0 ? (
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[780px] text-left text-sm">
                    <caption className="sr-only">Six-feed Pyth FX basket component metrics</caption>
                    <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                      <tr>
                        <th scope="col" className="px-4 py-3.5">Pair</th>
                        <th scope="col" className="px-4 py-3.5">Pyth Feed</th>
                        <th scope="col" className="px-4 py-3.5">Weight</th>
                        <th scope="col" className="px-4 py-3.5">Normalized FX</th>
                        <th scope="col" className="px-4 py-3.5">Confidence Ratio</th>
                        <th scope="col" className="px-4 py-3.5">Publish Drift</th>
                        <th scope="col" className="px-4 py-3.5">Status</th>
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
                        return (
                          <tr
                            key={comp.feedId}
                            className="border-b border-brand-border/15 last:border-0"
                          >
                            <td className="px-4 py-3.5 font-mono text-xs font-semibold text-content-primary">
                              {comp.symbol}
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs text-content-secondary">
                              {comp.feedSymbol}
                              {comp.inverted ? ' (inv)' : ''}
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                              {comp.weightBps != null
                                ? `${(comp.weightBps / 100).toFixed(1)}%`
                                : '—'}
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs tabular-nums text-content-primary">
                              {formatNormalizedFxPrice(comp.price)}
                            </td>
                            <td className="px-4 py-3.5">
                              <div className="flex items-center gap-2.5 font-mono text-xs tabular-nums">
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
                                <div className="h-1.5 w-20 bg-brand-border/20">
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
                              <span
                                className={
                                  driftExceeded ? 'font-semibold text-brand-orange' : 'text-content-secondary'
                                }
                              >
                                {comp.driftFromMinSeconds != null
                                  ? `+${String(comp.driftFromMinSeconds)}s`
                                  : '—'}
                              </span>
                            </td>
                            <td className="px-4 py-3.5">
                              <StatusPill
                                tone={comp.confidenceExceeded || driftExceeded ? 'blocked' : 'live'}
                                label={
                                  comp.confidenceExceeded
                                    ? 'Conf wide'
                                    : driftExceeded
                                      ? 'Diverged'
                                      : 'Admitted'
                                }
                              />
                            </td>
                          </tr>
                        )
                      })}
                    </tbody>
                  </table>
                </div>
              ) : (
                <EmptyState
                  title="No component payload cached"
                  message="Component FX feeds will appear once the basket worker admits a paired 6-feed Pyth payload."
                />
              )}
            </Panel>
          </section>

          {data.recentUpdates && data.recentUpdates.length > 0 ? (
            <section aria-labelledby="oracle-recent-updates-title" className="space-y-4">
              <div className="flex flex-wrap items-baseline justify-between gap-2">
                <div>
                  <h2 id="oracle-recent-updates-title" className="text-xl font-semibold">
                    Recent Admitted Oracle Updates
                  </h2>
                  <p className="mt-1 text-xs text-content-tertiary">
                    Click any admitted Pyth basket payload to inspect its 6-currency weight pie
                    chart, exact FX quotes, guard checks, and settled orders
                  </p>
                </div>
                <span className="font-mono text-xs text-content-tertiary">
                  Latest {String(data.recentUpdates.length)} payloads
                </span>
              </div>

              <Panel>
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[740px] text-left text-sm">
                    <caption className="sr-only">Recent admitted Pyth basket oracle updates</caption>
                    <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                      <tr>
                        <th scope="col" className="px-4 py-3.5">Payload</th>
                        <th scope="col" className="px-4 py-3.5">Published (UTC)</th>
                        <th scope="col" className="px-4 py-3.5">plDXY Mark</th>
                        <th scope="col" className="px-4 py-3.5">Spread / Lag</th>
                        <th scope="col" className="px-4 py-3.5">Source</th>
                        <th scope="col" className="px-4 py-3.5">Orders Settled</th>
                        <th scope="col" className="px-4 py-3.5 text-right">Detail</th>
                      </tr>
                    </thead>
                    <tbody>
                      {data.recentUpdates.map((upd) => (
                        <tr
                          key={upd.id}
                          className="border-b border-brand-border/15 last:border-0"
                        >
                          <td className="px-4 py-3.5 font-mono text-xs font-semibold">
                            <Link
                              to={`/oracle/updates/${upd.id}`}
                              className="text-brand-peach hover:underline"
                            >
                              #{upd.id}
                            </Link>
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs text-content-secondary">
                            {formatUtcWithSeconds(upd.maxPublishTime)}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs font-semibold tabular-nums text-content-primary">
                            {upd.basketPrice ? (
                              <OraclePriceLink updateId={upd.id}>
                                {formatPrice(upd.basketPrice)} plDXY
                              </OraclePriceLink>
                            ) : (
                              '—'
                            )}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums text-content-secondary">
                            <span
                              className={
                                upd.spreadSeconds >= data.thresholds.maxComponentDivergenceSeconds - 1
                                  ? 'font-semibold text-brand-yellow'
                                  : ''
                              }
                            >
                              {String(upd.spreadSeconds)}s spread
                            </span>
                            {' · '}
                            {String(upd.lagSeconds)}s lag
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs text-content-tertiary">
                            {upd.source === 'backend_hermes_latest_v2' ? 'Latest v2' : upd.source}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            {upd.ordersSettledCount > 0 ? (
                              <span className="font-semibold text-positive">
                                {upd.ordersSettledCount.toLocaleString('en-US')}
                              </span>
                            ) : (
                              <span className="text-content-tertiary">0</span>
                            )}
                          </td>
                          <td className="px-4 py-3.5 text-right font-mono text-xs">
                            <Link
                              to={`/oracle/updates/${upd.id}`}
                              className="font-semibold text-brand-peach hover:underline"
                            >
                              Inspect →
                            </Link>
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              </Panel>
            </section>
          ) : null}

          <section aria-labelledby="oracle-rejections-title" className="space-y-4">
            <div className="flex flex-wrap items-baseline justify-between gap-2">
              <div>
                <h2 id="oracle-rejections-title" className="text-xl font-semibold">
                  Recent Rejected &amp; Stale Oracle Checks (24h)
                </h2>
                <p className="mt-1 text-xs text-content-tertiary">
                  Sanitized audit trail of Pyth updates rejected by BasketWorker or PletherOracle
                  value guards, plus signed stale polls during FX market close
                </p>
              </div>
              {data.rejectionBreakdown.length > 0 ? (
                <div className="flex flex-wrap gap-1.5">
                  {data.rejectionBreakdown.map((item) => (
                    <span
                      key={`${item.reasonCode}-${item.outcome}`}
                      className={`border px-2 py-0.5 font-mono text-[11px] ${
                        item.outcome === 'rejected'
                          ? 'border-brand-orange/40 bg-brand-orange/10 text-brand-orange'
                          : 'border-brand-yellow/40 bg-brand-yellow/10 text-brand-yellow'
                      }`}
                    >
                      {formatReasonLabel(item.reasonCode)}: {item.count24h}
                    </span>
                  ))}
                </div>
              ) : null}
            </div>

            <Panel>
              {data.recentRejections.length > 0 ? (
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[760px] text-left text-sm">
                    <caption className="sr-only">Recent rejected and stale oracle update checks</caption>
                    <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                      <tr>
                        <th scope="col" className="px-4 py-3.5">Time (UTC)</th>
                        <th scope="col" className="px-4 py-3.5">Route</th>
                        <th scope="col" className="px-4 py-3.5">Outcome</th>
                        <th scope="col" className="px-4 py-3.5">Guard Reason</th>
                        <th scope="col" className="px-4 py-3.5">Spread / Age</th>
                        <th scope="col" className="px-4 py-3.5">Detail</th>
                      </tr>
                    </thead>
                    <tbody>
                      {data.recentRejections.map((ev) => (
                        <tr key={ev.id} className="border-b border-brand-border/15 last:border-0">
                          <td className="px-4 py-3.5 text-xs text-content-secondary">
                            {formatUtc(ev.occurredAt)}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs uppercase text-content-secondary">
                            {ev.route}
                          </td>
                          <td className="px-4 py-3.5">
                            <StatusPill
                              tone={ev.outcome === 'rejected' ? 'blocked' : 'degraded'}
                              label={
                                ev.outcome === 'rejected' ? 'Rejected' : 'Stale (FX closed)'
                              }
                            />
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs text-content-primary">
                            {formatReasonLabel(ev.reasonCode)}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums text-content-secondary">
                            {ev.spreadSeconds != null || ev.ageSeconds != null
                              ? `${ev.spreadSeconds != null ? `${String(ev.spreadSeconds)}s spread` : '—'} · ${ev.ageSeconds != null ? `${String(ev.ageSeconds)}s age` : '—'}`
                              : '—'}
                          </td>
                          <td className="px-4 py-3.5 text-xs text-content-secondary">{ev.detail}</td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              ) : (
                <EmptyState
                  title="Zero rejected oracle updates in the last 24 hours"
                  message="Every Pyth FX basket update polled by BasketWorker cleared all 6 PletherOracle admission checks cleanly."
                />
              )}
            </Panel>
          </section>
        </>
      ) : null}
    </div>
  )
}
