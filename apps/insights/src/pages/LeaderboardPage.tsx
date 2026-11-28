import { useState } from 'react'
import { Link, useSearchParams } from 'react-router-dom'
import { CopyButton } from '../components/ActionButtons'
import { AddressAvatar } from '../components/AddressAvatar'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { TierBadge } from '../divisions/Badge'
import { CurrentBadge } from '../divisions/CurrentBadge'
import { useDivisions, useDivisionsStats, useLiveDivisions } from '../divisions/query'
import { TIERS, TIER_REQUIREMENTS, tradingWeek, type Tier } from '../divisions/rules'
import { useUtcNow } from '../hooks/useUtcNow'
import { formatCountdown, formatUtc, shortAddress } from '../utils/format'

type LiveLeaderboardFilter = 'all' | 'qualifying' | 'discord'
type TierFilter = 'all' | 'unranked' | Tier

const TIER_FILTER_OPTIONS: { id: TierFilter; label: string }[] = [
  { id: 'all', label: 'All tiers' },
  ...[...TIERS].reverse().map((tier) => ({ id: tier, label: tier })),
  { id: 'unranked', label: 'Not yet awarded' },
]

const LIVE_FILTERS: { id: LiveLeaderboardFilter; label: string }[] = [
  { id: 'all', label: 'All' },
  { id: 'qualifying', label: 'Qualifying' },
  { id: 'discord', label: 'Discord Linked' },
]

export function LeaderboardPage() {
  const query = useDivisions()
  const liveQuery = useLiveDivisions()
  const statsQuery = useDivisionsStats()
  const now = useUtcNow(10_000)
  const week = tradingWeek(now)
  const [searchParams] = useSearchParams()
  const urlQuery = searchParams.get('q') ?? ''
  const [search, setSearch] = useState(urlQuery)
  const [prevUrlQuery, setPrevUrlQuery] = useState(urlQuery)
  const [filter, setFilter] = useState<LiveLeaderboardFilter>('all')
  const [tierFilter, setTierFilter] = useState<TierFilter>('all')
  const [livePage, setLivePage] = useState(0)
  if (urlQuery !== prevUrlQuery) {
    setPrevUrlQuery(urlQuery)
    setSearch(urlQuery)
    setLivePage(0)
  }

  const publication = query.data
  const live = liveQuery.data?.status === 'ready' ? liveQuery.data : null
  const earnedTiers = new Map(
    publication?.standings.map((row) => [row.address.toLowerCase(), row.tier]),
  )

  const normalizedSearch = search.trim().toLowerCase()
  const liveStandings =
    live?.standings?.filter((row) => {
      if (filter === 'qualifying' && !(row.activeDays >= 2 && (row.weeklyReturnBps ?? 0) > 0)) {
        return false
      }
      if (filter === 'discord' && !row.discordUsername) {
        return false
      }
      if (tierFilter !== 'all') {
        const earned = earnedTiers.get(row.address.toLowerCase())
        if (tierFilter === 'unranked' ? earned !== undefined : earned !== tierFilter) {
          return false
        }
      }
      if (!normalizedSearch) return true
      return `${row.address} ${row.discordUsername ? `@${row.discordUsername}` : ''}`
        .toLowerCase()
        .includes(normalizedSearch)
    }) ?? []

  const livePageCount = Math.max(1, Math.ceil(liveStandings.length / 100))
  const currentLivePage = Math.min(livePage, livePageCount - 1)
  const visibleLiveStandings = liveStandings.slice(
    currentLivePage * 100,
    (currentLivePage + 1) * 100,
  )

  return (
    <div className="space-y-6 sm:space-y-8">
      <section className="border border-brand-border/30 bg-surface-panel p-5 sm:p-6">
        <div className="flex flex-col justify-between gap-4 lg:flex-row lg:items-end">
          <div className="max-w-3xl">
            <p className="text-xs font-semibold uppercase tracking-[0.16em] text-brand-peach">
              Weekly Ranked Trading{live?.scoringRelease ? ` · ${live.scoringRelease.version}` : ''}
            </p>
            <h1 className="mt-1 text-2xl font-bold tracking-tight sm:text-3xl">
              Plether Divisions
            </h1>
            <p className="mt-2 max-w-2xl text-sm leading-6 text-content-secondary">
              Live weekly returns include every wallet with an executed trade on the contract
              version scored this week, whether or not it has linked Discord. Rankings are
              provisional until the weekly close.
            </p>
          </div>

          <div className="flex flex-wrap items-center gap-3 text-xs font-semibold">
            <Link
              to="/register"
              className="border border-brand-peach/50 bg-brand-peach/10 px-3 py-1.5 text-brand-peach transition-colors hover:border-brand-peach hover:bg-brand-peach/20"
            >
              Link Discord and wallet
            </Link>
            <Link to="/results" className="text-brand-peach hover:underline">
              Last week’s results →
            </Link>
            <Link
              to="/methodology"
              className="text-content-secondary hover:text-brand-peach hover:underline"
            >
              How Divisions work
            </Link>
          </div>
        </div>

        <dl className="mt-5 grid gap-4 border-t border-brand-border/20 pt-5 sm:grid-cols-2 lg:grid-cols-4">
          <div>
            <dt className="text-xs text-content-tertiary">Wallets registered with Discord</dt>
            <dd aria-live="polite" className="mt-1.5 font-mono text-xl font-semibold text-brand-peach">
              {statsQuery.data?.registeredWallets.toLocaleString() ??
                (statsQuery.isError ? 'Unavailable' : '…')}
            </dd>
            {statsQuery.isError ? (
              <button
                type="button"
                onClick={() => void statsQuery.refetch()}
                className="mt-1 text-xs text-brand-peach underline"
                aria-label="Retry Discord registration count"
              >
                Retry
              </button>
            ) : null}
          </div>
          {publication ? (
            <>
              <div>
                <dt className="text-xs text-content-tertiary">Friday snapshot</dt>
                <dd className="mt-1.5 font-mono text-sm font-semibold text-content-primary">
                  {formatUtc(week.closesAt)}
                </dd>
              </div>
              <div>
                <dt className="text-xs text-content-tertiary">
                  {now < Date.parse(week.closesAt) ? 'Time to close' : 'Monday announcement'}
                </dt>
                <dd className="mt-1.5 font-mono text-sm font-semibold text-brand-peach">
                  {now < Date.parse(week.closesAt)
                    ? formatCountdown(week.closesAt, now)
                    : formatUtc(week.publishesAt)}
                </dd>
              </div>
              <div>
                <dt className="text-xs text-content-tertiary">Qualifying week</dt>
                <dd className="mt-1.5 text-sm font-semibold text-content-primary">
                  In profit after fees · 2+ active days
                </dd>
              </div>
            </>
          ) : null}
        </dl>
      </section>

      <section aria-labelledby="live-leaderboard-title" className="space-y-4">
        <div className="flex flex-col justify-between gap-4 lg:flex-row lg:items-end">
          <div>
            <h2 id="live-leaderboard-title" className="text-xl font-semibold">
              Live weekly leaderboard
            </h2>
            <p className="mt-1 text-xs text-content-secondary">
              This week{live?.scoringRelease ? ` · ${live.scoringRelease.version}` : ''} · Ranked by
              weekly return on cash-flow-adjusted account value · Provisional
            </p>
          </div>

          <div className="flex flex-col gap-2 sm:flex-row sm:items-center">
            <div className="flex flex-wrap items-center gap-1" role="group" aria-label="Filter leaderboard">
              {LIVE_FILTERS.map((item) => {
                const active = filter === item.id
                return (
                  <button
                    key={item.id}
                    type="button"
                    aria-pressed={active}
                    onClick={() => {
                      setFilter(item.id)
                      setLivePage(0)
                    }}
                    className={`cursor-pointer border px-2.5 py-1.5 text-xs font-semibold transition-colors ${
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

            <div className="flex items-center gap-1.5">
              <label htmlFor="tier-filter" className="sr-only">
                Filter by tier
              </label>
              <select
                id="tier-filter"
                value={tierFilter}
                onChange={(event) => {
                  setTierFilter(event.target.value as TierFilter)
                  setLivePage(0)
                }}
                className="border border-brand-border/35 bg-surface-panel px-2.5 py-1.5 text-xs font-semibold text-content-primary focus:border-brand-peach/70 focus:outline-none"
              >
                {TIER_FILTER_OPTIONS.map((opt) => (
                  <option key={opt.id} value={opt.id}>
                    {opt.label}
                  </option>
                ))}
              </select>
            </div>

            <div className="w-full sm:w-56">
              <label htmlFor="trader-search" className="sr-only">
                Search traders
              </label>
              <input
                id="trader-search"
                type="search"
                value={search}
                onChange={(event) => {
                  setSearch(event.target.value)
                  setLivePage(0)
                }}
                placeholder="Search wallet or @handle"
                className="w-full border border-brand-border/35 bg-app-bg px-3 py-1.5 text-base placeholder:text-xs placeholder:text-content-tertiary focus:border-brand-peach/70 focus:outline-none sm:text-xs"
              />
            </div>
          </div>
        </div>

        {liveQuery.isLoading ? (
          <Panel>
            <LoadingState />
          </Panel>
        ) : liveQuery.isError ? (
          <ErrorState
            title="Live rankings are unavailable"
            message={liveQuery.error.message}
            onRetry={() => void liveQuery.refetch()}
          />
        ) : live ? (
          <>
            <div className="space-y-1 text-xs text-content-tertiary">
              <p className="font-mono">
                Snapshot {formatUtc(live.snapshotAt ?? '')} · Block {live.snapshotBlock} ·{' '}
                {live.walletCount} wallets
              </p>
              <p>
                Badges are awarded on Monday after the trading week ends. Live returns do not change
                your current badge.{' '}
                {publication
                  ? `Last awarded for the week ending ${formatUtc(publication.week.closesAt)}.`
                  : 'New traders await their first published weekly result.'}
              </p>
            </div>

            <Panel>
              <div className="overflow-x-auto">
                <table className="w-full min-w-[820px] text-left text-sm">
                  <caption className="sr-only">
                    Provisional weekly ranks for current-release wallets
                  </caption>
                  <thead className="border-b border-brand-border/25 text-xs uppercase tracking-[0.08em] text-content-tertiary">
                    <tr>
                      <th scope="col" className="px-4 py-3.5">Rank</th>
                      <th scope="col" className="px-4 py-3.5">Current badge</th>
                      <th scope="col" className="px-4 py-3.5">Wallet</th>
                      <th scope="col" className="px-4 py-3.5">Discord handle</th>
                      <th scope="col" className="px-4 py-3.5 text-right">Weekly return</th>
                      <th scope="col" className="px-4 py-3.5 text-right">Active days</th>
                      <th scope="col" className="px-4 py-3.5 text-right">Explorer</th>
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-brand-border/15">
                    {visibleLiveStandings.map((row) => {
                      const retBps = row.weeklyReturnBps
                      const returnTone =
                        retBps == null
                          ? 'text-content-tertiary'
                          : retBps > 0
                            ? 'text-positive'
                            : retBps < 0
                              ? 'text-brand-orange'
                              : 'text-content-secondary'
                      return (
                        <tr key={row.address} className="hover:bg-brand-peach/[0.03]">
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            {row.rank ?? '—'}
                          </td>
                          <td className="px-4 py-3.5">
                            <CurrentBadge
                              tier={earnedTiers.get(row.address.toLowerCase())}
                              loading={query.isLoading}
                              unavailable={query.isError && !publication}
                            />
                          </td>
                          <td className="whitespace-nowrap px-4 py-3.5 font-mono text-xs">
                            <div className="inline-flex items-center gap-2 whitespace-nowrap">
                              <AddressAvatar address={row.address} size={18} />
                              <Link
                                to={`/divisions/wallets/${row.address}`}
                                title={row.address}
                                aria-label={`View trader ${row.address}`}
                                className="whitespace-nowrap text-brand-peach hover:underline focus-visible:underline"
                              >
                                {shortAddress(row.address)}
                              </Link>
                              <CopyButton value={row.address} />
                            </div>
                          </td>
                          <td className="px-4 py-3.5 text-xs">
                            {row.discordUsername ? (
                              <span className="break-all font-medium text-content-primary">
                                @{row.discordUsername}
                              </span>
                            ) : (
                              <span className="text-content-tertiary" aria-label="Discord not linked">
                                —
                              </span>
                            )}
                          </td>
                          <td
                            className={`px-4 py-3.5 text-right font-mono text-xs font-semibold tabular-nums ${returnTone}`}
                          >
                            {retBps === null
                              ? '—'
                              : `${retBps > 0 ? '+' : ''}${(retBps / 100).toFixed(2)}%`}
                          </td>
                          <td className="px-4 py-3.5 text-right font-mono text-xs tabular-nums">
                            <span
                              className={
                                row.activeDays >= 2 ? 'text-positive' : 'text-content-secondary'
                              }
                            >
                              {row.activeDays}
                            </span>
                          </td>
                          <td className="px-4 py-3.5 text-right text-xs font-semibold">
                            <Link
                              to={`/wallets/${row.address}`}
                              className="text-content-secondary hover:text-brand-peach hover:underline"
                            >
                              Account →
                            </Link>
                          </td>
                        </tr>
                      )
                    })}
                  </tbody>
                </table>
              </div>

              {!liveStandings.length ? (
                <EmptyState
                  title="No matching wallets"
                  message="Try another wallet address, Discord handle, or filter."
                />
              ) : null}

              {livePageCount > 1 ? (
                <div className="flex flex-wrap items-center justify-between gap-3 border-t border-brand-border/20 px-4 py-3.5 text-xs text-content-secondary">
                  <span className="font-mono">
                    {liveStandings.length.toLocaleString()} wallets · Page {currentLivePage + 1} of{' '}
                    {livePageCount}
                  </span>
                  <div className="flex gap-2">
                    <button
                      type="button"
                      disabled={currentLivePage === 0}
                      onClick={() => {
                        setLivePage((page) => page - 1)
                      }}
                      className="cursor-pointer border border-brand-border/40 px-3 py-1.5 font-semibold disabled:cursor-not-allowed disabled:opacity-40"
                    >
                      Previous
                    </button>
                    <button
                      type="button"
                      disabled={currentLivePage + 1 >= livePageCount}
                      onClick={() => {
                        setLivePage((page) => page + 1)
                      }}
                      className="cursor-pointer border border-brand-border/40 px-3 py-1.5 font-semibold disabled:cursor-not-allowed disabled:opacity-40"
                    >
                      Next
                    </button>
                  </div>
                </div>
              ) : null}
            </Panel>
          </>
        ) : (
          <Panel>
            <EmptyState
              title="Preparing live rankings"
              message="The backend is capturing the first complete current-week snapshot. Check back shortly."
            />
          </Panel>
        )}
      </section>

      {publication ? (
        <section aria-labelledby="tiers-title">
          <div className="mb-3 flex flex-wrap items-baseline justify-between gap-2">
            <h2 id="tiers-title" className="text-lg font-semibold">
              Six tiers. A weekly climb.
            </h2>
            <span className="text-xs text-content-tertiary">
              Diamond takes at least five qualifying weeks
            </span>
          </div>
          <div className="grid gap-3 sm:grid-cols-3 lg:grid-cols-6">
            {TIERS.map((tier) => (
              <Panel key={tier} className="p-4">
                <TierBadge tier={tier} size="large" showLabel />
                <p className="mt-3 text-xs leading-5 text-content-secondary">
                  {TIER_REQUIREMENTS[tier]}
                </p>
              </Panel>
            ))}
          </div>
          <p className="mt-3 text-xs leading-5 text-content-secondary">
            Qualify to move up at most one tier. Two consecutive non-qualifying weeks drop you one
            tier; inactive weeks count.
          </p>
        </section>
      ) : null}

      <Panel className="p-5 sm:p-6">
        <div className="flex flex-col justify-between gap-3 sm:flex-row sm:items-center">
          <div>
            <h2 className="text-base font-semibold">
              {publication ? 'Your rank, in the community' : 'Link Discord and your wallet'}
            </h2>
            <p className="mt-1.5 max-w-3xl text-xs leading-5 text-content-secondary">
              {publication ? (
                <>
                  Your badge appears beside your name on the leaderboard.{' '}
                  <Link to="/register" className="text-brand-peach underline">
                    Link Discord and your owner wallet
                  </Link>{' '}
                  to prepare for matching Discord roles. You can rank without linking Discord.
                </>
              ) : (
                <>
                  <Link to="/register" className="text-brand-peach underline">
                    Link Discord and your owner wallet
                  </Link>{' '}
                  to prepare for future community roles. Trading activity appears above whether you
                  link or not.
                </>
              )}
            </p>
            <p className="mt-1 text-xs text-content-tertiary">
              {publication
                ? 'Discord role assignment is coming. Monday announcements will highlight the top three weekly returns, promotions into Gold, Platinum and Diamond, and the Diamond roster.'
                : 'Discord role assignment is coming.'}
            </p>
          </div>
          <Link
            to="/archive/september-2026"
            className="shrink-0 text-xs text-content-tertiary hover:text-brand-peach hover:underline"
          >
            September competition archive →
          </Link>
        </div>
      </Panel>
    </div>
  )
}
