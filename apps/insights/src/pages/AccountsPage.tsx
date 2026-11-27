import { useMemo, useState } from 'react'
import { Link, useSearchParams } from 'react-router-dom'
import {
  InsightsApiError,
  formatLeverageX100,
  useWalletsDirectory,
  type WalletDirectoryFilter,
  type WalletDirectorySort,
} from '../api'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { StatCard } from '../components/StatCard'
import { TraderIdentity } from '../components/TraderIdentity'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { TierBadge } from '../divisions/Badge'
import { TIERS, type Tier } from '../divisions/rules'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { resolvePerpsChain } from '../utils/chain'
import {
  formatCompactAssets,
  formatPrice,
  formatSignedAssets,
  formatUtc,
} from '../utils/format'

const PAGE_SIZE = 50

const FILTERS: { id: WalletDirectoryFilter; label: string }[] = [
  { id: 'all', label: 'All' },
  { id: 'open', label: 'Open' },
  { id: 'long', label: 'Long' },
  { id: 'short', label: 'Short' },
  { id: 'profitable', label: 'Profitable' },
  { id: 'discord', label: 'Discord' },
]

const SORTS: { id: WalletDirectorySort; label: string }[] = [
  { id: 'notional', label: 'Open Exposure' },
  { id: 'volume', label: 'Total Volume' },
  { id: 'pnl', label: 'Realized PnL' },
  { id: 'trades', label: 'Trades' },
  { id: 'recent', label: 'Last Active' },
]

const VALID_TIERS = new Set<string>(TIERS)

function toTier(raw: string | null): Tier | null {
  if (!raw) return null
  return VALID_TIERS.has(raw) ? (raw as Tier) : null
}

function parseSignedBigInt(raw: string | null | undefined): bigint {
  if (!raw || !/^-?\d+$/.test(raw)) return 0n
  return BigInt(raw)
}

function formatSignedCompactAssets(raw: string | null | undefined): string {
  const val = parseSignedBigInt(raw)
  if (val === 0n) return '0.00 USDG'
  const sign = val > 0n ? '+' : '-'
  const abs = val < 0n ? -val : val
  return `${sign}${formatCompactAssets(abs.toString())}`
}

function formatShortUtc(iso: string): string {
  const ms = Date.parse(iso)
  if (!Number.isFinite(ms)) return formatUtc(iso)
  const d = new Date(ms)
  const day = d.toLocaleDateString('en-GB', { day: '2-digit', month: 'short', timeZone: 'UTC' })
  const time = d.toLocaleTimeString('en-GB', {
    hour: '2-digit',
    minute: '2-digit',
    hour12: false,
    timeZone: 'UTC',
  })
  return `${day}, ${time}`
}

export function AccountsPage() {
  const protocol = useProtocolContext()
  const [searchParams, setSearchParams] = useSearchParams()
  const initialQuery = searchParams.get('q') ?? ''
  const [searchInput, setSearchInput] = useState(initialQuery)
  const [filter, setFilter] = useState<WalletDirectoryFilter>('all')
  const [sort, setSort] = useState<WalletDirectorySort>('notional')
  const [page, setPage] = useState(0)

  // Keep search input synchronized when navigating via header search to /accounts?q=...
  const urlQuery = searchParams.get('q') ?? ''
  const [prevUrlQuery, setPrevUrlQuery] = useState(urlQuery)
  if (urlQuery !== prevUrlQuery) {
    setPrevUrlQuery(urlQuery)
    setSearchInput(urlQuery)
    setPage(0)
  }

  const query = useWalletsDirectory({
    search: searchInput,
    filter,
    sort,
    limit: PAGE_SIZE,
    offset: page * PAGE_SIZE,
  })

  const data = query.data
  const awaitingDeploy = query.error instanceof InsightsApiError && query.error.status === 404
  const chain = resolvePerpsChain(data?.network.chainId ?? protocol.chainId)
  const releaseVersion = data?.network.releaseVersion ?? protocol.releaseVersion

  const summary = data?.summary
  const longOi = parseSignedBigInt(summary?.longOpenInterestAssets)
  const shortOi = parseSignedBigInt(summary?.shortOpenInterestAssets)
  const totalOi = longOi + shortOi
  const netSkew = parseSignedBigInt(summary?.netSkewAssets)

  const { longPct, shortPct } = useMemo(() => {
    if (totalOi <= 0n) return { longPct: 50, shortPct: 50 }
    const l = Number((longOi * 1000n + totalOi / 2n) / totalOi) / 10
    return { longPct: l, shortPct: Math.max(0, Number((100 - l).toFixed(1))) }
  }, [longOi, totalOi])

  const totalMatched = data?.totalMatched ?? 0
  const pageCount = Math.max(1, Math.ceil(totalMatched / PAGE_SIZE))
  const currentPage = Math.min(page, pageCount - 1)

  function handleSearchChange(next: string) {
    setSearchInput(next)
    setPage(0)
    const nextParams = new URLSearchParams(searchParams)
    if (next.trim()) {
      nextParams.set('q', next.trim())
    } else {
      nextParams.delete('q')
    }
    setSearchParams(nextParams, { replace: true })
  }

  return (
    <div className="space-y-6 sm:space-y-8">
      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={protocol.blockNumber}
        observedAt={data?.observedAt ?? protocol.observedAt}
        tone={awaitingDeploy ? 'unknown' : query.isError ? 'blocked' : data ? 'live' : 'unknown'}
        statusLabel={
          awaitingDeploy
            ? 'Awaiting Trading Accounts directory backend deploy'
            : query.isError
              ? 'Trading Accounts directory unavailable'
              : data
                ? 'Accounts directory live'
                : 'Loading accounts'
        }
      />

      <header className="flex flex-col justify-between gap-4 border border-brand-border/30 bg-surface-panel p-5 sm:flex-row sm:items-end sm:p-6">
        <div className="min-w-0">
          <p className="text-xs font-semibold uppercase tracking-[0.16em] text-brand-peach">
            Trading Accounts Directory · {releaseVersion ?? 'v2.0.0'}
          </p>
          <h1 className="mt-1 text-2xl font-bold tracking-tight sm:text-3xl">
            Trading Accounts &amp; Open Exposure
          </h1>
          <p className="mt-1.5 max-w-2xl text-sm text-content-secondary">
            Every active plDXY-PERP Trading Account ranked by live open exposure, volume, realized
            PnL, and recent activity. Click any account to inspect balances, fills, and position
            lifecycles.
          </p>
        </div>

        <div className="flex flex-wrap items-center gap-3 text-xs font-semibold">
          {data?.currentMarkPrice ? (
            <span className="border border-brand-border/30 bg-app-bg px-2.5 py-1.5 font-mono text-content-secondary">
              Mark <span className="text-content-primary">{formatPrice(data.currentMarkPrice)}</span>
            </span>
          ) : null}
          <Link to="/execution" className="text-brand-peach hover:underline">
            Execution tape →
          </Link>
          <Link to="/divisions" className="text-content-secondary hover:text-brand-peach hover:underline">
            Divisions leaderboard →
          </Link>
        </div>
      </header>

      {summary ? (
        <section aria-label="Trading accounts summary" className="space-y-3">
          <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
            <StatCard
              label="Active Trading Accounts"
              value={summary.totalAccounts.toLocaleString('en-US')}
              detail={`${summary.active24hCount.toLocaleString('en-US')} active in last 24h`}
            />
            <StatCard
              label="Canonical Open Interest"
              value={formatCompactAssets(summary.totalOpenInterestAssets)}
              detail={`${summary.accountsWithOpenPosition.toLocaleString('en-US')} open (${summary.longCount.toLocaleString('en-US')} L · ${summary.shortCount.toLocaleString('en-US')} S)`}
            />
            <StatCard
              label="Net Market Skew"
              value={formatSignedCompactAssets(summary.netSkewAssets)}
              tone={netSkew > 0n ? 'positive' : netSkew < 0n ? 'negative' : 'neutral'}
              detail={
                summary.avgLeverageX100 != null
                  ? `Long ${longPct.toFixed(1)}% / Short ${shortPct.toFixed(1)}% · Avg ${formatLeverageX100(summary.avgLeverageX100)}`
                  : `Long ${longPct.toFixed(1)}% / Short ${shortPct.toFixed(1)}%`
              }
            />
            <StatCard
              label="Cumulative Volume"
              value={formatCompactAssets(summary.totalVolumeAssets)}
              detail={`${summary.discordLinkedCount.toLocaleString('en-US')} Discord-linked accounts`}
            />
          </div>

          {totalOi > 0n ? (
            <div className="border border-brand-border/25 bg-surface-panel px-4 py-3">
              <div className="mb-1.5 flex flex-wrap items-center justify-between gap-2 font-mono text-xs">
                <div className="flex flex-wrap items-center gap-3">
                  <span className="font-semibold text-positive">
                    Long {longPct.toFixed(1)}% ({formatCompactAssets(summary.longOpenInterestAssets)} ·{' '}
                    {summary.longCount.toLocaleString('en-US')})
                  </span>
                  <span className="font-semibold text-brand-orange">
                    Short {shortPct.toFixed(1)}% ({formatCompactAssets(summary.shortOpenInterestAssets)} ·{' '}
                    {summary.shortCount.toLocaleString('en-US')})
                  </span>
                </div>
                <span className="text-[11px] text-content-tertiary">
                  Canonical on-chain CfdEngine OI
                </span>
              </div>
              <div className="flex h-2 w-full overflow-hidden bg-app-bg">
                <div
                  className="bg-positive transition-all"
                  style={{ width: `${longPct.toFixed(2)}%` }}
                />
                <div
                  className="bg-brand-orange transition-all"
                  style={{ width: `${shortPct.toFixed(2)}%` }}
                />
              </div>
            </div>
          ) : null}
        </section>
      ) : null}

      <section aria-labelledby="accounts-directory-heading" className="space-y-4">
        <div className="flex flex-col gap-3 lg:flex-row lg:items-center lg:justify-between">
          <div className="flex flex-wrap items-center gap-1.5" role="group" aria-label="Filter trading accounts">
            {FILTERS.map((item) => {
              const active = filter === item.id
              return (
                <button
                  key={item.id}
                  type="button"
                  aria-pressed={active}
                  onClick={() => {
                    setFilter(item.id)
                    setPage(0)
                  }}
                  className={`cursor-pointer border px-3 py-1.5 text-xs font-semibold transition-colors ${
                    active
                      ? 'border-brand-orange bg-brand-orange text-content-primary'
                      : 'border-brand-border/35 bg-surface-panel text-content-secondary hover:border-brand-peach/50 hover:text-content-primary'
                  }`}
                >
                  {item.label}
                </button>
              )
            })}
          </div>

          <div className="flex flex-col gap-2 sm:flex-row sm:items-center">
            <div className="relative w-full sm:w-52">
              <label htmlFor="accounts-filter-search" className="sr-only">
                Filter accounts by address or Discord handle
              </label>
              <input
                id="accounts-filter-search"
                type="search"
                value={searchInput}
                onChange={(event) => {
                  handleSearchChange(event.target.value)
                }}
                placeholder="Filter 0x or @handle"
                className="w-full border border-brand-border/35 bg-app-bg px-3 py-1.5 font-mono text-xs text-content-primary placeholder:font-sans placeholder:text-content-tertiary focus:border-brand-peach/70 focus:outline-none"
              />
            </div>

            <div className="flex items-center gap-1.5">
              <label htmlFor="accounts-sort" className="text-xs text-content-tertiary">
                Sort
              </label>
              <select
                id="accounts-sort"
                value={sort}
                onChange={(event) => {
                  setSort(event.target.value as WalletDirectorySort)
                  setPage(0)
                }}
                className="border border-brand-border/35 bg-surface-panel px-2.5 py-1.5 text-xs font-semibold text-content-primary focus:border-brand-peach/70 focus:outline-none"
              >
                {SORTS.map((opt) => (
                  <option key={opt.id} value={opt.id}>
                    {opt.label}
                  </option>
                ))}
              </select>
            </div>
          </div>
        </div>

        <h2 id="accounts-directory-heading" className="sr-only">
          Trading accounts directory table
        </h2>

        {query.isLoading ? (
          <Panel>
            <LoadingState rows={6} />
          </Panel>
        ) : awaitingDeploy ? (
          <Panel>
            <EmptyState
              title="Trading Accounts directory awaiting backend deploy"
              message="Deploy the updated backend serving GET /api/insights/v1/wallets to browse all active Trading Accounts here."
            />
          </Panel>
        ) : query.isError ? (
          <ErrorState
            title="Unable to load Trading Accounts"
            message={query.error instanceof Error ? query.error.message : 'Unexpected error'}
            onRetry={() => void query.refetch()}
          />
        ) : data && data.items.length > 0 ? (
          <Panel>
            <div className="overflow-x-auto">
              <table className="w-full min-w-[780px] text-left text-sm">
                <caption className="sr-only">
                  Active plDXY-PERP Trading Accounts with live exposure and performance stats
                </caption>
                <thead className="border-b border-brand-border/25 text-xs uppercase tracking-[0.08em] text-content-tertiary">
                  <tr>
                    <th scope="col" className="whitespace-nowrap px-3.5 py-3">Account</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3">Tier</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3">Live Position</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3 text-right">Realized PnL</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3 text-right">Volume</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3 text-right">Trades</th>
                    <th scope="col" className="whitespace-nowrap px-3 py-3 text-right">Last Active</th>
                    <th scope="col" className="whitespace-nowrap px-3.5 py-3 text-right">Exposure</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-brand-border/15">
                  {data.items.map((row) => {
                    const tier = toTier(row.divisionsTier)
                    const pnl = parseSignedBigInt(row.realizedPnlAssets)
                    return (
                      <tr key={row.account} className="hover:bg-brand-peach/[0.03]">
                        <td className="whitespace-nowrap px-3.5 py-3">
                          <TraderIdentity
                            address={row.account}
                            discordUsername={row.discordUsername}
                            showCopy
                          />
                        </td>
                        <td className="whitespace-nowrap px-3 py-3 text-xs">
                          {tier ? (
                            <TierBadge tier={tier} showLabel />
                          ) : (
                            <span className="text-xs text-content-tertiary">—</span>
                          )}
                        </td>
                        <td className="whitespace-nowrap px-3 py-3 font-mono text-xs tabular-nums">
                          {row.openSide === 'flat' ? (
                            <span className="text-content-tertiary">Flat</span>
                          ) : (
                            <div className="inline-flex items-center gap-1.5">
                              <span
                                className={`border px-1.5 py-0.5 text-[10px] font-semibold uppercase ${
                                  row.openSide === 'long'
                                    ? 'border-positive/40 bg-positive/10 text-positive'
                                    : 'border-brand-orange/40 bg-brand-orange/10 text-brand-orange'
                                }`}
                              >
                                {row.openSide}
                              </span>
                              <span className="font-semibold text-content-primary">
                                {formatCompactAssets(row.openNotionalAssets)}
                              </span>
                            </div>
                          )}
                        </td>
                        <td
                          className={`whitespace-nowrap px-3 py-3 text-right font-mono text-xs font-semibold tabular-nums ${
                            pnl > 0n
                              ? 'text-positive'
                              : pnl < 0n
                                ? 'text-brand-orange'
                                : 'text-content-secondary'
                          }`}
                        >
                          {formatSignedAssets(row.realizedPnlAssets)}
                        </td>
                        <td className="whitespace-nowrap px-3 py-3 text-right font-mono text-xs tabular-nums text-content-secondary">
                          {formatCompactAssets(row.volumeAssets)}
                        </td>
                        <td
                          className="whitespace-nowrap px-3 py-3 text-right font-mono text-xs tabular-nums text-content-secondary"
                          title={`${String(row.opensCount)} opens · ${String(row.closesCount)} closes · ${String(row.liquidationsCount)} liquidations`}
                        >
                          {row.totalTrades.toLocaleString('en-US')}
                        </td>
                        <td
                          className="whitespace-nowrap px-3 py-3 text-right font-mono text-xs tabular-nums text-content-tertiary"
                          title={row.lastActivityAt ? formatUtc(row.lastActivityAt) : undefined}
                        >
                          {row.lastActivityAt ? formatShortUtc(row.lastActivityAt) : '—'}
                        </td>
                        <td className="whitespace-nowrap px-3.5 py-3 text-right text-xs font-semibold">
                          <Link
                            to={`/wallets/${row.account}?tab=exposure`}
                            className="text-brand-peach hover:underline"
                          >
                            Exposure →
                          </Link>
                        </td>
                      </tr>
                    )
                  })}
                </tbody>
              </table>
            </div>

            {pageCount > 1 ? (
              <div className="flex flex-wrap items-center justify-between gap-3 border-t border-brand-border/20 px-4 py-3.5 text-xs text-content-secondary">
                <span className="font-mono">
                  {totalMatched.toLocaleString('en-US')} matching accounts · Page{' '}
                  {currentPage + 1} of {pageCount}
                </span>
                <div className="flex items-center gap-2">
                  <button
                    type="button"
                    disabled={currentPage === 0}
                    onClick={() => {
                      setPage((p) => Math.max(0, p - 1))
                    }}
                    className="cursor-pointer border border-brand-border/40 px-3 py-1.5 font-semibold disabled:cursor-not-allowed disabled:opacity-40"
                  >
                    Previous
                  </button>
                  <button
                    type="button"
                    disabled={currentPage + 1 >= pageCount}
                    onClick={() => {
                      setPage((p) => p + 1)
                    }}
                    className="cursor-pointer border border-brand-border/40 px-3 py-1.5 font-semibold disabled:cursor-not-allowed disabled:opacity-40"
                  >
                    Next
                  </button>
                </div>
              </div>
            ) : (
              <div className="border-t border-brand-border/20 px-4 py-2.5 font-mono text-xs text-content-tertiary">
                Showing {data.items.length.toLocaleString('en-US')} of{' '}
                {totalMatched.toLocaleString('en-US')} matching accounts
              </div>
            )}
          </Panel>
        ) : (
          <Panel>
            <EmptyState
              title="No matching Trading Accounts"
              message="Try clearing your search filter or switching to All accounts."
            />
          </Panel>
        )}
      </section>
    </div>
  )
}
