import { useState } from 'react'
import { Link } from 'react-router-dom'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { TierBadge } from '../divisions/Badge'
import { useDivisions, useLiveDivisions } from '../divisions/query'
import { TIERS, TIER_REQUIREMENTS, tradingWeek } from '../divisions/rules'
import { CurrentBadge } from '../divisions/CurrentBadge'
import { useUtcNow } from '../hooks/useUtcNow'
import { formatCountdown, formatUtc, shortAddress } from '../utils/format'

export function LeaderboardPage() {
  const query = useDivisions()
  const liveQuery = useLiveDivisions()
  const now = useUtcNow(10_000)
  const week = tradingWeek(now)
  const [search, setSearch] = useState('')
  const [livePage, setLivePage] = useState(0)
  const publication = query.data
  const live = liveQuery.data?.status === 'ready' ? liveQuery.data : null
  const earnedTiers = new Map(publication?.standings.map((row) => [row.address.toLowerCase(), row.tier]))
  const liveStandings = live?.standings?.filter((row) => `${row.address} ${row.discordUsername ? `@${row.discordUsername}` : ''}`.toLowerCase().includes(search.trim().toLowerCase())) ?? []
  const livePageCount = Math.max(1, Math.ceil(liveStandings.length / 100))
  const currentLivePage = Math.min(livePage, livePageCount - 1)
  const visibleLiveStandings = liveStandings.slice(currentLivePage * 100, (currentLivePage + 1) * 100)

  return <div className="space-y-8">
    <section className="relative overflow-hidden border border-brand-border/30 bg-surface-panel p-6 sm:p-10">
      <div className="absolute -right-10 -top-20 text-[18rem] text-brand-peach/5" aria-hidden="true">◆</div>
      <div className="relative max-w-3xl">
        <p className="text-xs font-semibold uppercase tracking-[0.2em] text-brand-peach">Testnet · Weekly ranked trading</p>
        <h1 className="mt-4 text-4xl font-semibold tracking-tight sm:text-6xl">Plether Divisions</h1>
        <p className="mt-5 max-w-2xl text-lg leading-7 text-content-secondary">Trade consistently. Climb the ranks. Earn your place.</p>
        <p className="mt-3 max-w-2xl text-sm leading-6 text-content-secondary">Live weekly returns include every wallet with an executed trade on the current release, whether or not it has linked Discord. Rankings are provisional until the weekly close.</p>
        <div className="mt-6 flex flex-wrap gap-3"><a href="https://app.sepolia.plether.com" className="border border-brand-orange bg-brand-orange px-5 py-3 text-sm font-semibold hover:bg-brand-peach hover:text-app-bg">Start trading ↗</a><Link to="/register" className="border border-brand-border/40 px-5 py-3 text-sm font-semibold hover:bg-brand-peach/10">Link Discord and wallet</Link><Link to="/methodology" className="border border-brand-border/40 px-5 py-3 text-sm font-semibold hover:bg-brand-peach/10">How Divisions work</Link></div>
      </div>
      {publication ? <dl className="relative mt-8 grid gap-5 border-t border-brand-border/20 pt-6 sm:grid-cols-3">
        <div><dt className="text-xs text-content-tertiary">Friday snapshot</dt><dd className="mt-2 text-sm font-semibold">{formatUtc(week.closesAt)}</dd></div>
        <div><dt className="text-xs text-content-tertiary">{now < Date.parse(week.closesAt) ? 'Time to close' : 'Monday announcement'}</dt><dd className="mt-2 font-mono text-sm text-brand-peach">{now < Date.parse(week.closesAt) ? formatCountdown(week.closesAt, now) : formatUtc(week.publishesAt)}</dd></div>
        <div><dt className="text-xs text-content-tertiary">Qualifying week</dt><dd className="mt-2 text-sm font-semibold">In profit after fees · 2+ active days</dd></div>
      </dl> : null}
    </section>
    <section aria-labelledby="live-leaderboard-title" className="space-y-4">
      <div className="flex flex-col justify-between gap-4 sm:flex-row sm:items-end"><div><h2 id="live-leaderboard-title" className="text-2xl font-semibold">Live weekly leaderboard</h2><p className="mt-1 text-sm text-content-secondary">Current release · Ranked by weekly return on cash-flow-adjusted account value · Provisional</p></div><div className="w-full sm:max-w-xs"><label htmlFor="trader-search" className="sr-only">Search traders</label><input id="trader-search" type="search" value={search} onChange={(event) => { setSearch(event.target.value); setLivePage(0) }} placeholder="Search wallet or Discord handle" className="w-full border border-brand-border/35 bg-app-bg px-4 py-2.5 text-sm placeholder:text-content-tertiary" /></div></div>
      {liveQuery.isLoading ? <Panel><LoadingState /></Panel> : liveQuery.isError ? <ErrorState title="Live rankings are unavailable" message={liveQuery.error.message} onRetry={() => void liveQuery.refetch()} /> : live ? <>
        <p className="text-xs text-content-tertiary">Snapshot {formatUtc(live.snapshotAt ?? '')} · Block {live.snapshotBlock} · {live.walletCount} wallets</p>
        <p className="text-xs text-content-tertiary">Badges are awarded on Monday after the trading week ends. Live returns do not change your current badge. {publication ? `Last awarded for the week ending ${formatUtc(publication.week.closesAt)}.` : 'New traders await their first published weekly result.'}</p>
        <Panel><div className="overflow-x-auto"><table className="w-full min-w-[760px] text-left text-sm"><caption className="sr-only">Provisional weekly ranks for current-release wallets</caption><thead className="border-b border-brand-border/25 text-xs text-content-tertiary"><tr><th scope="col" className="px-4 py-4">Rank</th><th scope="col" className="px-4 py-4">Current badge</th><th scope="col" className="px-4 py-4">Wallet</th><th scope="col" className="px-4 py-4">Discord handle</th><th scope="col" className="px-4 py-4">Weekly return</th><th scope="col" className="px-4 py-4">Active days</th></tr></thead><tbody>{visibleLiveStandings.map((row) => <tr key={row.address} className="border-b border-brand-border/15 last:border-0"><td className="px-4 py-4 font-mono">{row.rank ?? '—'}</td><td className="px-4 py-4"><CurrentBadge tier={earnedTiers.get(row.address.toLowerCase())} loading={query.isLoading} unavailable={query.isError && !publication} /></td><td className="px-4 py-4 font-mono"><Link to={`/divisions/wallets/${row.address}`} title={row.address} aria-label={`View trader ${row.address}`} className="text-brand-peach hover:underline focus-visible:underline">{shortAddress(row.address)}</Link></td><td className="px-4 py-4">{row.discordUsername ? <span className="break-all">@{row.discordUsername}</span> : <span className="text-content-tertiary" aria-label="Discord not linked">—</span>}</td><td className="px-4 py-4 font-mono">{row.weeklyReturnBps === null ? '—' : `${row.weeklyReturnBps > 0 ? '+' : ''}${(row.weeklyReturnBps / 100).toFixed(2)}%`}</td><td className="px-4 py-4">{row.activeDays}</td></tr>)}</tbody></table></div>{!liveStandings.length ? <EmptyState title="No matching wallets" message="Try another wallet address or Discord handle." /> : null}{livePageCount > 1 ? <div className="flex flex-wrap items-center justify-between gap-3 border-t border-brand-border/20 px-4 py-4 text-sm text-content-secondary"><span>{liveStandings.length.toLocaleString()} wallets · Page {currentLivePage + 1} of {livePageCount}</span><div className="flex gap-2"><button type="button" disabled={currentLivePage === 0} onClick={() => { setLivePage((page) => page - 1) }} className="border border-brand-border/40 px-3 py-2 disabled:opacity-40">Previous</button><button type="button" disabled={currentLivePage + 1 >= livePageCount} onClick={() => { setLivePage((page) => page + 1) }} className="border border-brand-border/40 px-3 py-2 disabled:opacity-40">Next</button></div></div> : null}</Panel>
      </> : <Panel><EmptyState title="Preparing live rankings" message="The backend is capturing the first complete current-week snapshot. Check back shortly." /></Panel>}
    </section>
    {publication ? <section aria-labelledby="tiers-title"><div className="mb-4 flex flex-wrap items-baseline justify-between gap-2"><h2 id="tiers-title" className="text-xl font-semibold">Six tiers. A weekly climb.</h2><span className="text-xs text-content-tertiary">Diamond takes at least five qualifying weeks</span></div><div className="grid gap-3 sm:grid-cols-3 lg:grid-cols-6">{TIERS.map((tier) => <Panel key={tier} className="p-4"><TierBadge tier={tier} size="large" showLabel /><p className="mt-3 text-sm leading-6 text-content-secondary">{TIER_REQUIREMENTS[tier]}</p></Panel>)}</div><p className="mt-3 text-sm leading-6 text-content-secondary">Qualify to move up at most one tier. Two consecutive non-qualifying weeks drop you one tier; inactive weeks count.</p></section> : null}
    <Link to="/results" className="inline-block text-sm font-semibold text-brand-peach hover:underline">Last week’s results →</Link>
    {publication ? <Panel className="p-5 sm:p-7"><h2 className="text-lg font-semibold">Your rank, in the community</h2><p className="mt-2 max-w-3xl text-sm leading-6 text-content-secondary">Your badge appears beside your name on the leaderboard. <Link to="/register" className="text-brand-peach underline">Link Discord and your owner wallet</Link> to prepare for matching Discord roles. You can rank without linking Discord.</p><p className="mt-2 text-sm text-content-tertiary">Discord role assignment is coming. Monday announcements will highlight the top three weekly returns, promotions into Gold, Platinum and Diamond, and the Diamond roster.</p></Panel> : <Panel className="p-5 sm:p-7"><h2 className="text-lg font-semibold">Link Discord and your wallet</h2><p className="mt-2 max-w-3xl text-sm leading-6 text-content-secondary"><Link to="/register" className="text-brand-peach underline">Link Discord and your owner wallet</Link> to prepare for future community roles. Trading activity appears above whether you link or not.</p><p className="mt-2 text-sm text-content-tertiary">Discord role assignment is coming.</p></Panel>}
    <Link to="/archive/september-2026" className="inline-block text-sm text-content-tertiary hover:text-brand-peach">September competition archive →</Link>
  </div>
}
