import { Link } from 'react-router-dom'
import { useInsightsStatus } from '../api'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { StatCard } from '../components/StatCard'
import { EmptyState, Panel } from '../components/ui'
import { WalletSearch } from '../components/WalletSearch'
import { CurrentBadge } from '../divisions/CurrentBadge'
import { useDivisions, useDivisionsStats, useLiveDivisions } from '../divisions/query'
import { resolvePerpsChain, vaultsAppUrl } from '../utils/chain'
import { formatRoi, shortAddress } from '../utils/format'

export function OverviewPage() {
  const statusQuery = useInsightsStatus()
  const liveQuery = useLiveDivisions()
  const publicationQuery = useDivisions()
  const statsQuery = useDivisionsStats()

  const chain = resolvePerpsChain(statusQuery.data?.chainId)
  const live = liveQuery.data?.status === 'ready' ? liveQuery.data : null
  const publication = publicationQuery.data
  const earnedTiers = new Map(
    publication?.standings.map((row) => [row.address.toLowerCase(), row.tier]),
  )

  const blockNumber = live?.snapshotBlock ?? statusQuery.data?.latestIndexedBlock ?? null
  const observedAt = live?.snapshotAt ?? statusQuery.data?.latestIndexedAt ?? null
  const releaseVersion = live?.scoringRelease?.version ?? publication?.scoringRelease?.version ?? null
  const assetSymbol = live?.scoringRelease?.assetSymbol ?? chain.defaultAssetSymbol

  const pulseTone = statusQuery.isError && liveQuery.isError
    ? 'blocked'
    : statusQuery.data?.healthy || live
      ? 'live'
      : statusQuery.isLoading || liveQuery.isLoading
        ? 'unknown'
        : 'stale'

  const pulseLabel =
    pulseTone === 'live'
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
              and public Trading Account audits for Plether Perpetuals.
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
                to="/divisions"
                className="border border-brand-border/40 px-4 py-2 text-sm font-semibold text-content-primary transition-colors hover:border-brand-peach hover:bg-brand-peach/10"
              >
                Plether Divisions
              </Link>
              <a
                href={chain.tradeAppUrl}
                className="border border-brand-border/40 px-4 py-2 text-sm font-semibold text-brand-peach transition-colors hover:border-brand-peach hover:bg-brand-peach/10"
              >
                Open trading app ↗
              </a>
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
            loading={statusQuery.isLoading && liveQuery.isLoading}
            unavailable={blockNumber === null}
            value={blockNumber !== null ? Number(blockNumber).toLocaleString('en-US') : undefined}
            detail={
              statusQuery.data?.healthy
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
            loading={liveQuery.isLoading && publicationQuery.isLoading}
            unavailable={!releaseVersion}
            value={releaseVersion ?? undefined}
            tone="positive"
            detail={`${chain.name} · Settled in ${assetSymbol}`}
          />
        </div>
      </section>

      <section aria-labelledby="pillars-title" className="space-y-3">
        <h2 id="pillars-title" className="text-lg font-semibold">
          Explore analytics
        </h2>
        <div className="grid gap-4 md:grid-cols-2">
          <Panel className="flex flex-col justify-between p-5 transition-colors hover:border-brand-peach/50 sm:p-6">
            <div>
              <p className="text-xs font-semibold uppercase tracking-[0.12em] text-brand-peach">
                Traders
              </p>
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
              <p className="text-xs font-semibold uppercase tracking-[0.12em] text-brand-peach">
                Liquidity Providers
              </p>
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
                href={vaultsAppUrl(statusQuery.data?.chainId)}
                className="text-content-secondary hover:text-content-primary hover:underline hover:underline-offset-4"
              >
                Vaults on App ↗
              </a>
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
              <p className="text-xs font-semibold uppercase tracking-[0.12em] text-brand-peach">
                Competition
              </p>
              <h3 className="mt-2 text-lg font-semibold">Plether Divisions &amp; Badges</h3>
              <p className="mt-2 text-sm leading-6 text-content-secondary">
                Weekly cash-flow-adjusted return rankings, Iron through Diamond tier progression,
                Discord role linking, and shareable trader cards.
              </p>
            </div>
            <div className="mt-5 flex flex-wrap gap-4 text-sm font-semibold">
              <Link to="/divisions" className="text-brand-peach hover:underline hover:underline-offset-4">
                Open Divisions →
              </Link>
              <Link to="/register" className="text-content-secondary hover:text-content-primary hover:underline hover:underline-offset-4">
                Link Discord →
              </Link>
            </div>
          </Panel>
        </div>
      </section>

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
