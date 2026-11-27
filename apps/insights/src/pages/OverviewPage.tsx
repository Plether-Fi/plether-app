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
import { StatCard } from '../components/StatCard'
import { StatusPill } from '../components/StatusPill'
import { EmptyState, Panel } from '../components/ui'
import { WalletSearch } from '../components/WalletSearch'
import { CurrentBadge } from '../divisions/CurrentBadge'
import { useDivisions, useDivisionsStats, useLiveDivisions } from '../divisions/query'
import { resolvePerpsChain, vaultsAppUrl } from '../utils/chain'
import { formatRoi, shortAddress } from '../utils/format'

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

      <section className="border border-brand-border/30 bg-surface-panel p-6 sm:p-8">
        <div className="grid gap-6 lg:grid-cols-[1.35fr_1fr] lg:items-end">
          <div>
            <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
              {chain.name} · Perps Analytics
            </p>
            <h1 className="mt-2.5 text-3xl font-semibold tracking-tight sm:text-4xl">
              Plether Insights
            </h1>
            <p className="mt-3 max-w-2xl text-sm leading-6 text-content-secondary sm:text-base">
              Verifiable execution quality, LP settlement forensics, permissionless keeper activity,
              sponsored AA health, AWS infrastructure telemetry, and public Trading Account audits.
            </p>
            <div className="mt-5 flex flex-wrap gap-2.5">
              <Link
                to="/execution"
                className="border border-brand-orange bg-brand-orange px-4 py-2 text-sm font-semibold text-content-primary transition-colors hover:bg-brand-peach hover:text-app-bg"
              >
                Execution quality
              </Link>
              <Link
                to="/liquidity"
                className="border border-brand-border/40 px-4 py-2 text-sm font-semibold text-content-primary transition-colors hover:border-brand-peach hover:bg-brand-peach/10"
              >
                LP settlement ops
              </Link>
              <Link
                to="/infra"
                className="border border-brand-border/40 px-4 py-2 text-sm font-semibold text-content-primary transition-colors hover:border-brand-peach hover:bg-brand-peach/10"
              >
                AWS infra health
              </Link>
            </div>
          </div>

          <div className="border border-brand-border/25 bg-app-bg p-4">
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

      <section aria-labelledby="pulse-metrics-title" className="space-y-3">
        <h2 id="pulse-metrics-title" className="text-lg font-semibold">
          Protocol snapshot
        </h2>
        <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
          <StatCard
            label="Indexed block"
            loading={statusQuery.isLoading && liveQuery.isLoading && !overview}
            unavailable={blockNumber === null}
            value={blockNumber !== null ? Number(blockNumber).toLocaleString('en-US') : undefined}
            detail={
              overview?.infra.rpcReachable
                ? `Nitro RPC live (${formatLatencyMs(overview.infra.rpcLatencyMs)})`
                : statusQuery.data?.healthy
                  ? 'Perps history indexer synced'
                  : live?.snapshotBlock
                    ? 'Latest Divisions snapshot block'
                    : statusQuery.isError
                      ? 'Status endpoint unreachable'
                      : 'Waiting for latest block heartbeat'
            }
          />
          <StatCard
            label="Active traders (week)"
            loading={liveQuery.isLoading}
            unavailable={!live}
            value={live?.walletCount?.toLocaleString('en-US')}
            tone="accent"
            detail={
              releaseVersion
                ? `Accounts active on Core ${releaseVersion}`
                : 'Current scoring week accounts'
            }
          />
          <StatCard
            label="Discord-linked wallets"
            loading={statsQuery.isLoading}
            unavailable={statsQuery.isError}
            value={statsQuery.data?.registeredWallets.toLocaleString('en-US')}
            detail={
              <Link to="/register" className="text-brand-peach hover:underline">
                Link Discord for Divisions roles →
              </Link>
            }
          />
          <StatCard
            label="Active release"
            loading={liveQuery.isLoading && publicationQuery.isLoading && !overview}
            unavailable={!releaseVersion}
            value={releaseVersion ?? undefined}
            tone="positive"
            detail={
              <Link to="/protocol" className="text-brand-peach hover:underline">
                Verify contracts &amp; params ({assetSymbol}) →
              </Link>
            }
          />
        </div>
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
                {overview?.execution.fillRate24hBps != null && (
                  <StatusPill
                    tone={overview.execution.fillRate24hBps >= 9800 ? 'live' : 'stale'}
                    label={`${formatBpsRate(overview.execution.fillRate24hBps)} fill (24h)`}
                  />
                )}
              </div>
              <h3 className="mt-2 text-lg font-semibold">Execution Quality &amp; Trade Tape</h3>
              <p className="mt-2 text-sm leading-6 text-content-secondary">
                1h and 24h fill rates, commit-to-execution latency percentiles, TP/SL protection
                triggers, and verifiable settlement receipts.
              </p>
              {overview?.execution.orders24h != null && (
                <p className="mt-3 font-mono text-xs text-content-tertiary">
                  {overview.execution.orders24h.toLocaleString('en-US')} orders (24h)
                  {overview.execution.latencyP50Seconds != null
                    ? ` · p50 ${String(overview.execution.latencyP50Seconds)}s`
                    : ''}
                  {overview.execution.latencyP95Seconds != null
                    ? ` · p95 ${String(overview.execution.latencyP95Seconds)}s`
                    : ''}
                </p>
              )}
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
              {overview?.liquidity.latestEpoch != null && (
                <p className="mt-3 font-mono text-xs text-content-tertiary">
                  Epoch #{overview.liquidity.latestEpoch}
                  {overview.liquidity.confirmedSettlements24h != null
                    ? ` · ${String(overview.liquidity.confirmedSettlements24h)} confirmed (24h)`
                    : ''}
                </p>
              )}
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
                        <Link
                          to={`/wallets/${row.address}`}
                          title={row.address}
                          className="text-brand-peach hover:underline"
                        >
                          {shortAddress(row.address)}
                        </Link>
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
