import { Link } from 'react-router-dom'
import { useInsightsStatus } from '../api'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { StatCard } from '../components/StatCard'
import { StatusPill } from '../components/StatusPill'
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
    <div className="space-y-8">
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

      <section className="relative overflow-hidden border border-brand-border/30 bg-surface-panel p-6 sm:p-10">
        <div
          className="pointer-events-none absolute -right-10 -top-20 text-[18rem] text-brand-peach/5"
          aria-hidden="true"
        >
          ◆
        </div>
        <div className="relative max-w-3xl">
          <p className="text-xs font-semibold uppercase tracking-[0.2em] text-brand-peach">
            {chain.name} · Perps Protocol Pulse
          </p>
          <h1 className="mt-4 text-4xl font-semibold tracking-tight sm:text-5xl">
            Plether Insights
          </h1>
          <p className="mt-4 max-w-2xl text-base leading-7 text-content-secondary sm:text-lg">
            Verifiable execution quality, LP settlement forensics, public Trading Account audits,
            verified contracts, and off-chain infrastructure health—built to complement the main
            trading and vaults app.
          </p>

          <div className="mt-6 max-w-xl">
            <WalletSearch />
            <p className="mt-2 text-xs text-content-tertiary">
              Resolves Trading Account addresses directly for Divisions standing, execution
              receipts, and LP request history.
            </p>
          </div>

          <div className="mt-6 flex flex-wrap gap-3">
            <Link
              to="/execution"
              className="border border-brand-orange bg-brand-orange px-5 py-2.5 text-sm font-semibold hover:bg-brand-peach hover:text-app-bg"
            >
              Execution quality
            </Link>
            <Link
              to="/liquidity"
              className="border border-brand-border/40 px-5 py-2.5 text-sm font-semibold hover:bg-brand-peach/10"
            >
              LP settlement ops
            </Link>
            <Link
              to="/divisions"
              className="border border-brand-border/40 px-5 py-2.5 text-sm font-semibold hover:bg-brand-peach/10"
            >
              Plether Divisions
            </Link>
            <a
              href={chain.tradeAppUrl}
              className="border border-brand-border/40 px-5 py-2.5 text-sm font-semibold text-brand-peach hover:bg-brand-peach/10"
            >
              Open trading app ↗
            </a>
          </div>
        </div>
      </section>

      <section aria-labelledby="pulse-metrics-title" className="space-y-4">
        <div className="flex flex-wrap items-baseline justify-between gap-2">
          <h2 id="pulse-metrics-title" className="text-xl font-semibold">
            Live protocol snapshot
          </h2>
          <span className="text-xs text-content-tertiary">
            Missing feeds remain explicitly unavailable — never coerced to zero
          </span>
        </div>
        <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
          <StatCard
            label="Indexed block"
            loading={statusQuery.isLoading && liveQuery.isLoading}
            unavailable={blockNumber === null}
            value={blockNumber !== null ? Number(blockNumber).toLocaleString('en-US') : undefined}
            detail={
              statusQuery.data?.healthy
                ? 'Perps history indexer healthy'
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
                ? `Traded accounts on Core ${releaseVersion}`
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

      <section aria-labelledby="pillars-title" className="space-y-4">
        <h2 id="pillars-title" className="text-xl font-semibold">
          Explore by perspective
        </h2>
        <div className="grid gap-4 md:grid-cols-2">
          <Panel className="flex flex-col justify-between p-6">
            <div>
              <div className="flex items-center justify-between gap-3">
                <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                  For Traders
                </p>
                <StatusPill tone="unknown" label="Stage 2 API pending" />
              </div>
              <h3 className="mt-3 text-xl font-semibold">Execution Quality &amp; Trade Tape</h3>
              <p className="mt-2 text-sm leading-6 text-content-secondary">
                Audit canonical 1h/24h keeper fill and expiration rates, commit-to-execution
                latency percentiles, TP/SL protection trigger reliability, liquidation splits, and
                protocol-wide receipt breakdowns.
              </p>
            </div>
            <div className="mt-5 flex flex-wrap gap-4 text-sm font-semibold">
              <Link to="/execution" className="text-brand-peach hover:underline">
                Open Execution →
              </Link>
              <Link to="/divisions" className="text-content-secondary hover:text-brand-peach">
                Weekly Divisions →
              </Link>
            </div>
          </Panel>

          <Panel className="flex flex-col justify-between p-6">
            <div>
              <div className="flex items-center justify-between gap-3">
                <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                  For Liquidity Providers
                </p>
                <StatusPill tone="ready" label="Stage 1 wired" />
              </div>
              <h3 className="mt-3 text-xl font-semibold">LP Settlement Ops &amp; Forensics</h3>
              <p className="mt-2 text-sm leading-6 text-content-secondary">
                Monitor hourly LP epoch settlement execution, decoded blocker/warning/fault masks,
                trace-attribution freshness and residual history, and counterparty cash flows.
              </p>
            </div>
            <div className="mt-5 flex flex-wrap gap-4 text-sm font-semibold">
              <Link to="/liquidity" className="text-brand-peach hover:underline">
                Open LP Ops →
              </Link>
              <a
                href={vaultsAppUrl(statusQuery.data?.chainId)}
                className="text-content-secondary hover:text-brand-peach"
              >
                Vaults on App ↗
              </a>
            </div>
          </Panel>

          <Panel className="flex flex-col justify-between p-6">
            <div>
              <div className="flex items-center justify-between gap-3">
                <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                  Protocol Verification
                </p>
                <StatusPill tone="unknown" label="Stage 4 API pending" />
              </div>
              <h3 className="mt-3 text-xl font-semibold">Contracts &amp; Economic Parameters</h3>
              <p className="mt-2 text-sm leading-6 text-content-secondary">
                Verify all deployed Core contracts, runtime code hashes, live vs. manifest
                margin/VPI/carry/fee parameters, Senior caps, and EmergencyPauseCoordinator state.
              </p>
            </div>
            <div className="mt-5 text-sm font-semibold">
              <Link to="/protocol" className="text-brand-peach hover:underline">
                Inspect Protocol →
              </Link>
            </div>
          </Panel>

          <Panel className="flex flex-col justify-between p-6">
            <div>
              <div className="flex items-center justify-between gap-3">
                <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                  System Reliability
                </p>
                <StatusPill tone="unknown" label="Stage 5 API pending" />
              </div>
              <h3 className="mt-3 text-xl font-semibold">Infrastructure &amp; Action Readiness</h3>
              <p className="mt-2 text-sm leading-6 text-content-secondary">
                Live user-action readiness (Deposit, Open, Close, TP/SL Protection), keeper and
                indexer block lags, Pyth oracle health, Managed AA (Alto bundler &amp; Paymaster),
                and self-hosted Arbitrum RPC status.
              </p>
            </div>
            <div className="mt-5 flex flex-wrap gap-4 text-sm font-semibold">
              <Link to="/infra" className="text-brand-peach hover:underline">
                Check Infrastructure →
              </Link>
              <Link to="/keepers" className="text-content-secondary hover:text-brand-peach">
                Monitor Keepers &amp; ETH balances →
              </Link>
            </div>
          </Panel>
        </div>
      </section>

      <section aria-labelledby="pulse-divisions-title" className="space-y-4">
        <div className="flex flex-wrap items-baseline justify-between gap-3">
          <div>
            <h2 id="pulse-divisions-title" className="text-xl font-semibold">
              Live Divisions leaders
            </h2>
            <p className="mt-1 text-xs text-content-tertiary">
              Provisional weekly return on cash-flow-adjusted equity · Updated from live snapshots
            </p>
          </div>
          <Link to="/divisions" className="text-sm font-semibold text-brand-peach hover:underline">
            Full Divisions leaderboard →
          </Link>
        </div>
        <Panel>
          {topStandings.length > 0 ? (
            <div className="overflow-x-auto">
              <table className="w-full min-w-[640px] text-left text-sm">
                <caption className="sr-only">Top live weekly Divisions traders</caption>
                <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                  <tr>
                    <th scope="col" className="px-4 py-3.5">Rank</th>
                    <th scope="col" className="px-4 py-3.5">Current badge</th>
                    <th scope="col" className="px-4 py-3.5">Trading Account</th>
                    <th scope="col" className="px-4 py-3.5">Discord</th>
                    <th scope="col" className="px-4 py-3.5">Weekly return</th>
                    <th scope="col" className="px-4 py-3.5">Active days</th>
                  </tr>
                </thead>
                <tbody>
                  {topStandings.map((row) => (
                    <tr
                      key={row.address}
                      className="border-b border-brand-border/15 last:border-0"
                    >
                      <td className="px-4 py-3.5 font-mono">{row.rank ?? '—'}</td>
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
                          <span>@{row.discordUsername}</span>
                        ) : (
                          <span className="text-content-tertiary">—</span>
                        )}
                      </td>
                      <td className="px-4 py-3.5 font-mono tabular-nums">
                        {formatRoi(row.weeklyReturnBps)}
                      </td>
                      <td className="px-4 py-3.5 font-mono tabular-nums">{row.activeDays}</td>
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
