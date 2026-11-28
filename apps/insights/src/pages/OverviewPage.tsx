import { Link } from 'react-router-dom'
import {
  formatLatencyMs,
  infraHealthLabel,
  infraHealthTone,
  useInsightsStatus,
  useMarketExposure,
  useOverview,
} from '../api'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { MarketExposurePulsePanel } from '../components/MarketExposurePulsePanel'
import { StatusPill } from '../components/StatusPill'
import { TraderIdentity } from '../components/TraderIdentity'
import { EmptyState, Panel } from '../components/ui'
import { WalletSearch } from '../components/WalletSearch'
import { CurrentBadge } from '../divisions/CurrentBadge'
import { useDivisions, useDivisionsStats, useLiveDivisions } from '../divisions/query'
import { resolvePerpsChain, vaultsAppUrl } from '../utils/chain'
import { formatRoi } from '../utils/format'

function formatBpsRate(bps: number | null | undefined): string {
  if (bps == null || !Number.isFinite(bps)) return '—'
  return `${(bps / 100).toFixed(2)}%`
}

export function OverviewPage() {
  const statusQuery = useInsightsStatus()
  const overviewQuery = useOverview()
  const marketExposureQuery = useMarketExposure()
  const liveQuery = useLiveDivisions()
  const publicationQuery = useDivisions()
  const statsQuery = useDivisionsStats()

  const overview = overviewQuery.data
  const chain = resolvePerpsChain(overview?.network.chainId ?? statusQuery.data?.chainId)
  const live = liveQuery.data?.status === 'ready' ? liveQuery.data : null
  const publication = publicationQuery.data
  const earnedTiers = new Map(
    publication?.standings.map((row) => [row.address.toLowerCase(), row.tier]),
  )

  const blockNumber =
    (overview?.headBlock ? Number(overview.headBlock) : null) ??
    live?.snapshotBlock ??
    statusQuery.data?.latestIndexedBlock ??
    null
  const observedAt =
    overview?.observedAt ?? live?.snapshotAt ?? statusQuery.data?.latestIndexedAt ?? null
  const releaseVersion =
    overview?.network.releaseVersion ??
    live?.scoringRelease?.version ??
    publication?.scoringRelease?.version ??
    null
  const assetSymbol =
    overview?.network.settlementAssetSymbol ??
    live?.scoringRelease?.assetSymbol ??
    chain.defaultAssetSymbol

  const pulseTone =
    overview?.infra.overallStatus === 'down'
      ? 'blocked'
      : overview?.infra.overallStatus === 'degraded'
        ? 'stale'
        : statusQuery.isError && liveQuery.isError && !overview
          ? 'blocked'
          : overview?.infra.overallStatus === 'healthy' || statusQuery.data?.healthy || live
            ? 'live'
            : statusQuery.isLoading || liveQuery.isLoading
              ? 'unknown'
              : 'stale'

  const pulseLabel =
    overview?.infra.overallStatus === 'down'
      ? 'Core infrastructure degraded'
      : overview?.infra.overallStatus === 'degraded'
        ? 'Protocol telemetry degraded'
        : pulseTone === 'live'
          ? 'Protocol telemetry live'
          : pulseTone === 'blocked'
            ? 'Telemetry unavailable'
            : pulseTone === 'stale'
              ? 'Syncing snapshots'
              : 'Checking status'

  const topStandings = live?.standings?.slice(0, 5) ?? []

  return (
    <div className="space-y-6 sm:space-y-8">
      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={blockNumber}
        observedAt={observedAt}
        tone={pulseTone}
        statusLabel={pulseLabel}
        note={`Settlement asset: ${assetSymbol}`}
      />

      <section className="min-w-0 border border-brand-border/30 bg-surface-panel p-4 sm:p-8">
        <div className="grid gap-6 lg:grid-cols-[1.35fr_1fr] lg:items-end">
          <div className="min-w-0">
            <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
              {chain.name} · Perps Analytics
            </p>
            <h1 className="mt-2.5 text-2xl font-semibold tracking-tight sm:text-4xl">
              Plether Insights
            </h1>
            <p className="mt-3 max-w-2xl text-sm leading-6 text-content-secondary sm:text-base">
              Verifiable execution quality, LP settlement forensics, permissionless keeper activity,
              sponsored AA health, AWS infrastructure telemetry, and public Trading Account audits.
            </p>
            <div className="mt-5 flex flex-wrap gap-2">
              <Link
                to="/execution"
                className="border border-brand-orange bg-brand-orange px-3 py-2 text-xs font-semibold text-content-primary transition-colors hover:bg-brand-peach hover:text-app-bg sm:px-4 sm:text-sm"
              >
                Execution quality
              </Link>
              <Link
                to="/liquidity"
                className="border border-brand-border/40 px-3 py-2 text-xs font-semibold text-content-primary transition-colors hover:border-brand-peach hover:bg-brand-peach/10 sm:px-4 sm:text-sm"
              >
                LP settlement ops
              </Link>
              <Link
                to="/infra"
                className="border border-brand-border/40 px-3 py-2 text-xs font-semibold text-content-primary transition-colors hover:border-brand-peach hover:bg-brand-peach/10 sm:px-4 sm:text-sm"
              >
                AWS infra health
              </Link>
            </div>
          </div>

          <div className="min-w-0 border border-brand-border/25 bg-app-bg p-3.5 sm:p-4">
            <p className="text-xs font-medium text-content-secondary">
              Inspect any Trading Account
            </p>
            <WalletSearch className="mt-2.5" />
            <p className="mt-2 text-xs text-content-tertiary">
              View live balance history, closed position receipts, LP requests, and shareable PnL cards.
            </p>
          </div>
        </div>
      </section>

      <section aria-labelledby="pulse-metrics-title">
        <h2 id="pulse-metrics-title" className="sr-only">
          Protocol snapshot
        </h2>
        <Panel className="overflow-hidden">
          <div className="flex flex-wrap items-center justify-between gap-2 border-b border-brand-border/20 bg-app-bg/50 px-4 py-2.5 font-mono text-xs text-content-secondary sm:px-5">
            <div className="flex flex-wrap items-center gap-x-3 gap-y-1">
              <span>
                Block{' '}
                <strong className="text-content-primary tabular-nums">
                  {blockNumber !== null ? Number(blockNumber).toLocaleString('en-US') : '—'}
                </strong>
              </span>
              {releaseVersion ? (
                <span>
                  Core{' '}
                  <strong className="text-positive">{releaseVersion}</strong>
                </span>
              ) : null}
              <span className="text-content-tertiary">
                {overview?.infra.rpcReachable
                  ? `Nitro RPC ${formatLatencyMs(overview.infra.rpcLatencyMs)}`
                  : statusQuery.data?.healthy
                    ? 'Indexer synced'
                    : live?.snapshotBlock
                      ? 'Divisions snapshot synced'
                      : 'Checking heartbeat'}
              </span>
            </div>
            <Link to="/protocol" className="font-semibold text-brand-peach hover:underline">
              Verify {assetSymbol} contracts →
            </Link>
          </div>

          <dl className="grid grid-cols-1 divide-y divide-brand-border/15 sm:grid-cols-2 sm:divide-y-0 lg:grid-cols-4 lg:divide-x">
            <div className="p-4 sm:p-5">
              <dt className="text-xs font-medium text-content-tertiary">Active traders (week)</dt>
              <dd className="mt-1.5 font-mono text-2xl font-bold tabular-nums text-brand-peach">
                {liveQuery.isLoading ? (
                  <span className="skeleton inline-block h-7 w-20 align-middle" />
                ) : live?.walletCount != null ? (
                  live.walletCount.toLocaleString('en-US')
                ) : (
                  <span className="text-sm font-normal text-content-tertiary">Unavailable</span>
                )}
              </dd>
              <p className="mt-1 text-xs text-content-tertiary">Current scoring week accounts</p>
            </div>

            <div className="border-brand-border/15 p-4 sm:border-l sm:p-5 lg:border-l-0">
              <dt className="text-xs font-medium text-content-tertiary">24h order fill rate</dt>
              <dd
                className={`mt-1.5 font-mono text-2xl font-bold tabular-nums ${
                  overview?.execution.fillRate24hBps != null && overview.execution.fillRate24hBps >= 9800
                    ? 'text-positive'
                    : 'text-content-primary'
                }`}
              >
                {overviewQuery.isLoading ? (
                  <span className="skeleton inline-block h-7 w-20 align-middle" />
                ) : overview?.execution.fillRate24hBps != null ? (
                  formatBpsRate(overview.execution.fillRate24hBps)
                ) : (
                  <span className="text-sm font-normal text-content-tertiary">Unavailable</span>
                )}
              </dd>
              <p className="mt-1 font-mono text-xs text-content-tertiary">
                {overview?.execution.orders24h != null
                  ? `${overview.execution.orders24h.toLocaleString('en-US')} orders${
                      overview.execution.latencyP50Seconds != null
                        ? ` · p50 ${String(overview.execution.latencyP50Seconds)}s`
                        : ''
                    }`
                  : 'Canonical 24h keeper cohort'}
              </p>
            </div>

            <div className="border-brand-border/15 p-4 sm:border-t sm:p-5 lg:border-t-0">
              <dt className="text-xs font-medium text-content-tertiary">LP settlements (24h)</dt>
              <dd className="mt-1.5 font-mono text-2xl font-bold tabular-nums text-content-primary">
                {overviewQuery.isLoading ? (
                  <span className="skeleton inline-block h-7 w-20 align-middle" />
                ) : overview?.liquidity.confirmedSettlements24h != null ? (
                  overview.liquidity.confirmedSettlements24h.toLocaleString('en-US')
                ) : (
                  <span className="text-sm font-normal text-content-tertiary">Unavailable</span>
                )}
              </dd>
              <p className="mt-1 font-mono text-xs text-content-tertiary">
                {overview?.liquidity.latestEpoch != null
                  ? `Epoch #${overview.liquidity.latestEpoch.toLocaleString('en-US')}`
                  : 'Hourly HousePool epochs'}
              </p>
            </div>

            <div className="border-brand-border/15 p-4 sm:border-l sm:border-t sm:p-5 lg:border-l-0 lg:border-t-0">
              <dt className="text-xs font-medium text-content-tertiary">Discord-linked wallets</dt>
              <dd className="mt-1.5 font-mono text-2xl font-bold tabular-nums text-content-primary">
                {statsQuery.isLoading ? (
                  <span className="skeleton inline-block h-7 w-20 align-middle" />
                ) : statsQuery.data ? (
                  statsQuery.data.registeredWallets.toLocaleString('en-US')
                ) : (
                  <span className="text-sm font-normal text-content-tertiary">Unavailable</span>
                )}
              </dd>
              <p className="mt-1 text-xs">
                <Link to="/register" className="text-brand-peach hover:underline">
                  Link Discord for Divisions →
                </Link>
              </p>
            </div>
          </dl>
        </Panel>
      </section>

      <section aria-labelledby="pillars-title" className="space-y-3">
        <h2 id="pillars-title" className="text-lg font-semibold">
          Explore analytics
        </h2>
        <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-3">
          <Panel className="flex flex-col justify-between p-5 transition-colors hover:border-brand-peach/50 sm:p-6">
            <div>
              <div className="flex items-center justify-between gap-2">
                <p className="text-xs font-semibold uppercase tracking-[0.12em] text-brand-peach">
                  Traders
                </p>
                {overview?.execution.latencyP95Seconds != null && (
                  <StatusPill
                    tone="live"
                    label={`p95 ${String(overview.execution.latencyP95Seconds)}s`}
                  />
                )}
              </div>
              <h3 className="mt-2 text-lg font-semibold">Execution Quality &amp; Trade Tape</h3>
              <p className="mt-2 text-sm leading-6 text-content-secondary">
                1h and 24h fill rates, commit-to-execution latency percentiles, TP/SL protection
                triggers, and verifiable settlement receipts.
              </p>
            </div>
            <div className="mt-5 flex flex-wrap gap-4 text-sm font-semibold">
              <Link to="/execution" className="text-brand-peach hover:underline hover:underline-offset-4">
                Open Execution →
              </Link>
              <Link to="/divisions" className="text-content-secondary hover:text-content-primary hover:underline hover:underline-offset-4">
                Weekly Divisions →
              </Link>
            </div>
          </Panel>

          <Panel className="flex flex-col justify-between p-5 transition-colors hover:border-brand-peach/50 sm:p-6">
            <div>
              <div className="flex items-center justify-between gap-2">
                <p className="text-xs font-semibold uppercase tracking-[0.12em] text-brand-peach">
                  Liquidity Providers
                </p>
                {overview?.liquidity.readiness && (
                  <StatusPill
                    tone={infraHealthTone(overview.liquidity.readiness)}
                    label={infraHealthLabel(overview.liquidity.readiness)}
                  />
                )}
              </div>
              <h3 className="mt-2 text-lg font-semibold">LP Settlement Ops &amp; Forensics</h3>
              <p className="mt-2 text-sm leading-6 text-content-secondary">
                Hourly LP epoch settlement readiness, decoded blocker and warning masks, snapshot
                freshness, and Junior vault attribution residuals.
              </p>
            </div>
            <div className="mt-5 flex flex-wrap gap-4 text-sm font-semibold">
              <Link to="/liquidity" className="text-brand-peach hover:underline hover:underline-offset-4">
                Open LP Ops →
              </Link>
              <a
                href={vaultsAppUrl(overview?.network.chainId ?? statusQuery.data?.chainId)}
                className="text-content-secondary hover:text-content-primary hover:underline hover:underline-offset-4"
              >
                Vaults on App ↗
              </a>
            </div>
          </Panel>

          <Panel className="flex flex-col justify-between p-5 transition-colors hover:border-brand-peach/50 sm:p-6">
            <div>
              <div className="flex items-center justify-between gap-2">
                <p className="text-xs font-semibold uppercase tracking-[0.12em] text-brand-peach">
                  Infrastructure
                </p>
                {overview && (
                  <StatusPill
                    tone={infraHealthTone(overview.infra.overallStatus)}
                    label={infraHealthLabel(overview.infra.overallStatus)}
                  />
                )}
              </div>
              <h3 className="mt-2 text-lg font-semibold">AWS Stack, Keepers, DB &amp; RPC</h3>
              <p className="mt-2 text-sm leading-6 text-content-secondary">
                Live AWS ECS task health, PostgreSQL RDS pool, Arbitrum Nitro EC2 node, Alto
                bundler, Pyth oracle feeds, and user action readiness.
              </p>
              {overview && (
                <p className="mt-3 font-mono text-xs text-content-tertiary">
                  {overview.infra.healthyServices}/{overview.infra.totalServices} workers healthy
                  {overview.infra.dbPingLatencyMs != null
                    ? ` · DB ${formatLatencyMs(overview.infra.dbPingLatencyMs)}`
                    : ''}
                </p>
              )}
            </div>
            <div className="mt-5 text-sm font-semibold">
              <Link to="/infra" className="text-brand-peach hover:underline hover:underline-offset-4">
                Open AWS Infra →
              </Link>
            </div>
          </Panel>

          <Panel className="flex flex-col justify-between p-5 transition-colors hover:border-brand-peach/50 sm:p-6">
            <div>
              <div className="flex items-center justify-between gap-2">
                <p className="text-xs font-semibold uppercase tracking-[0.12em] text-brand-peach">
                  Gasless AA
                </p>
                {overview?.sponsored.configured && (
                  <StatusPill
                    tone={overview.sponsored.enabled ? 'live' : 'stale'}
                    label={overview.sponsored.enabled ? 'Issuing' : 'Paused'}
                  />
                )}
              </div>
              <h3 className="mt-2 text-lg font-semibold">Sponsored Transactions &amp; Paymaster</h3>
              <p className="mt-2 text-sm leading-6 text-content-secondary">
                ERC-4337 Paymaster EntryPoint deposit runway, hourly and daily gas budgets, AA
                reconciler lag, and live UserOperation receipts.
              </p>
              {overview?.sponsored.configured && (
                <p className="mt-3 font-mono text-xs text-content-tertiary">
                  {overview.sponsored.ops24h.toLocaleString('en-US')} UserOps (24h)
                  {overview.sponsored.successRate24hBps != null
                    ? ` · ${formatBpsRate(overview.sponsored.successRate24hBps)} success`
                    : ''}
                </p>
              )}
            </div>
            <div className="mt-5 text-sm font-semibold">
              <Link to="/sponsored" className="text-brand-peach hover:underline hover:underline-offset-4">
                Open Sponsored →
              </Link>
            </div>
          </Panel>

          <Panel className="flex flex-col justify-between p-5 transition-colors hover:border-brand-peach/50 sm:p-6">
            <div>
              <p className="text-xs font-semibold uppercase tracking-[0.12em] text-brand-peach">
                Keepers
              </p>
              <h3 className="mt-2 text-lg font-semibold">Permissionless Keeper Activity</h3>
              <p className="mt-2 text-sm leading-6 text-content-secondary">
                Every active keeper discovered directly from on-chain fills, liquidations,
                protections, and LP settlements — with latency, bounties, and ETH balances.
              </p>
            </div>
            <div className="mt-5 text-sm font-semibold">
              <Link to="/keepers" className="text-brand-peach hover:underline hover:underline-offset-4">
                View Keepers →
              </Link>
            </div>
          </Panel>

          <Panel className="flex flex-col justify-between p-5 transition-colors hover:border-brand-peach/50 sm:p-6">
            <div>
              <div className="flex items-center justify-between gap-2">
                <p className="text-xs font-semibold uppercase tracking-[0.12em] text-brand-peach">
                  Protocol
                </p>
                {overview && (
                  <StatusPill
                    tone="live"
                    label={`${String(overview.protocol.pinnedContracts)} contracts`}
                  />
                )}
              </div>
              <h3 className="mt-2 text-lg font-semibold">Contracts, Bytecode &amp; Risk Params</h3>
              <p className="mt-2 text-sm leading-6 text-content-secondary">
                Live on-chain bytecode verification, 25 economic risk parameters vs. release
                defaults, and emergency pause coordinator controls.
              </p>
            </div>
            <div className="mt-5 flex flex-wrap gap-4 text-sm font-semibold">
              <Link to="/protocol" className="text-brand-peach hover:underline hover:underline-offset-4">
                Verify Protocol →
              </Link>
              <Link to="/divisions" className="text-content-secondary hover:text-content-primary hover:underline hover:underline-offset-4">
                Open Divisions →
              </Link>
            </div>
          </Panel>
        </div>
      </section>

      <MarketExposurePulsePanel
        data={marketExposureQuery.data}
        isLoading={marketExposureQuery.isLoading}
        isError={marketExposureQuery.isError}
      />

      <section aria-labelledby="pulse-divisions-title" className="space-y-3">
        <div className="flex flex-wrap items-baseline justify-between gap-3">
          <div>
            <h2 id="pulse-divisions-title" className="text-lg font-semibold">
              Live Divisions leaders
            </h2>
            <p className="mt-1 text-xs text-content-tertiary">
              Provisional weekly return on cash-flow-adjusted equity
            </p>
          </div>
          <Link to="/divisions" className="text-sm font-semibold text-brand-peach hover:underline hover:underline-offset-4">
            Full leaderboard →
          </Link>
        </div>
        <Panel>
          {topStandings.length > 0 ? (
            <div className="overflow-x-auto">
              <table className="w-full min-w-[640px] text-left text-sm">
                <caption className="sr-only">Top live weekly Divisions traders</caption>
                <thead className="border-b border-brand-border/25 bg-app-bg/40 text-xs font-medium text-content-secondary">
                  <tr>
                    <th scope="col" className="px-4 py-3">Rank</th>
                    <th scope="col" className="px-4 py-3">Current badge</th>
                    <th scope="col" className="px-4 py-3">Trading Account</th>
                    <th scope="col" className="px-4 py-3">Discord</th>
                    <th scope="col" className="px-4 py-3">Weekly return</th>
                    <th scope="col" className="px-4 py-3">Active days</th>
                  </tr>
                </thead>
                <tbody>
                  {topStandings.map((row) => (
                    <tr
                      key={row.address}
                      className="border-b border-brand-border/15 transition-colors last:border-0 hover:bg-app-bg/35"
                    >
                      <td className="px-4 py-3.5 font-mono text-content-secondary">#{row.rank ?? '—'}</td>
                      <td className="px-4 py-3.5">
                        <CurrentBadge
                          tier={earnedTiers.get(row.address.toLowerCase())}
                          loading={publicationQuery.isLoading}
                          unavailable={publicationQuery.isError && !publication}
                        />
                      </td>
                      <td className="px-4 py-3.5 font-mono">
                        <TraderIdentity address={row.address} avatarSize={18} showCopy />
                      </td>
                      <td className="px-4 py-3.5">
                        {row.discordUsername ? (
                          <span className="font-medium text-content-primary">@{row.discordUsername}</span>
                        ) : (
                          <span className="text-content-tertiary">—</span>
                        )}
                      </td>
                      <td
                        className={`px-4 py-3.5 font-mono font-semibold tabular-nums ${
                          row.weeklyReturnBps != null && row.weeklyReturnBps > 0
                            ? 'text-positive'
                            : row.weeklyReturnBps != null && row.weeklyReturnBps < 0
                              ? 'text-brand-orange'
                              : 'text-content-primary'
                        }`}
                      >
                        {formatRoi(row.weeklyReturnBps)}
                      </td>
                      <td className="px-4 py-3.5 font-mono tabular-nums text-content-secondary">{row.activeDays}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          ) : (
            <EmptyState
              title={liveQuery.isError ? 'Live rankings unavailable' : 'Awaiting live rankings'}
              message={
                liveQuery.isError
                  ? 'Could not reach the live Divisions feed. Check back shortly.'
                  : 'Current-week rankings will appear here as soon as the snapshot worker publishes them.'
              }
            />
          )}
        </Panel>
      </section>
    </div>
  )
}
