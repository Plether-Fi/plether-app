import { useState } from 'react'
import { Link } from 'react-router-dom'
import { EmptyState, ErrorState, LoadingState, Panel, Pnl } from '../components/ui'
import { TierBadge } from '../divisions/Badge'
import { formatReturn } from '../divisions/engine'
import { useDivisions, useLiveDivisions } from '../divisions/query'
import { TIERS, TIER_REQUIREMENTS, tradingWeek } from '../divisions/rules'
import type { DivisionPublication, DivisionStanding } from '../divisions/types'
import { useUtcNow } from '../hooks/useUtcNow'
import { formatCountdown, formatUtc, shortAddress } from '../utils/format'

function Trader({ standing }: { standing: DivisionStanding }) {
  return <div className="flex flex-wrap items-center gap-2"><div className="min-w-0"><span className="block break-all font-semibold">{standing.displayName ?? shortAddress(standing.address)}</span><span className="font-mono text-xs text-content-tertiary" title={standing.address}>{shortAddress(standing.address)}</span></div><TierBadge tier={standing.tier} /></div>
}

function Announcements({ publication }: { publication: DivisionPublication }) {
  const names = (addresses: string[]) => addresses.map((address) => publication.standings.find((row) => row.address === address)?.displayName ?? shortAddress(address)).join(', ')
  return <Panel className="grid gap-6 p-5 sm:grid-cols-3 sm:p-7">
    <div><h3 className="text-sm font-semibold text-brand-peach">Traders of the Week</h3><p className="mt-2 text-sm leading-6">{names(publication.announcement.tradersOfTheWeek) || 'No profitable traders this week.'}</p><p className="mt-1 text-xs text-content-tertiary">Top 3 weekly percentage returns</p></div>
    <div><h3 className="text-sm font-semibold text-brand-peach">New Gold, Platinum &amp; Diamond</h3><ul className="mt-2 space-y-2 text-sm">{publication.announcement.promotions.map((row) => <li key={row.address}>{names([row.address])} · {row.tier}</li>)}</ul>{!publication.announcement.promotions.length ? <p className="mt-2 text-sm text-content-secondary">No promotions into these tiers this week.</p> : null}</div>
    <div><h3 className="text-sm font-semibold text-sky-200">Diamond roster</h3><p className="mt-2 text-sm leading-6">{names(publication.announcement.diamondRoster) || 'The climb to Diamond is underway.'}</p></div>
  </Panel>
}

function Standings({ standings }: { standings: DivisionStanding[] }) {
  if (!standings.length) return <EmptyState title="No traders found" message="Try a different name or wallet address." />
  return <div className="overflow-x-auto"><table className="w-full min-w-[640px] text-left text-sm">
    <caption className="sr-only">Published weekly percentage P&amp;L and division badges</caption>
    <thead className="border-b border-brand-border/25 text-xs text-content-tertiary"><tr>{['Rank', 'Trader & division', 'Weekly return', 'Net P&L', 'Active days', 'Weekly outcome'].map((label) => <th key={label} scope="col" className="px-4 py-4">{label}</th>)}</tr></thead>
    <tbody>{standings.map((row) => <tr key={row.address} className="border-b border-brand-border/15 last:border-0">
      <td className="px-4 py-5 font-mono">{row.rank ?? '—'}</td><td className="px-4 py-5"><Trader standing={row} /></td>
      <td className="px-4 py-5 font-mono">{formatReturn(row.weeklyPnlUsdc, row.adjustedEquityUsdc)}</td><td className="px-4 py-5"><Pnl value={row.weeklyPnlUsdc} usdcKind="mock" /></td><td className="px-4 py-5">{row.activeDays} / 2</td>
      <td className="px-4 py-5"><span className={row.movement === 'promoted' ? 'text-positive' : 'text-content-secondary'}>{row.movement === 'promoted' ? `↑ ${row.previousTier} → ${row.tier}` : row.movement === 'demoted' ? `↓ ${row.previousTier} → ${row.tier}` : row.qualifying ? 'Qualified · tier held' : 'Non-qualifying'}</span>{row.nonQualifyingWeeks === 1 ? <p className="mt-1 text-xs text-brand-yellow">1 of 2 non-qualifying weeks</p> : null}</td>
    </tr>)}</tbody>
  </table></div>
}

export function LeaderboardPage() {
  const query = useDivisions()
  const liveQuery = useLiveDivisions()
  const now = useUtcNow(10_000)
  const week = tradingWeek(now)
  const [search, setSearch] = useState('')
  const [livePage, setLivePage] = useState(0)
  const publication = query.data
  const live = liveQuery.data?.status === 'ready' ? liveQuery.data : null
  const liveStandings = live?.standings?.filter((row) => row.address.toLowerCase().includes(search.trim().toLowerCase())) ?? []
  const livePageCount = Math.max(1, Math.ceil(liveStandings.length / 100))
  const currentLivePage = Math.min(livePage, livePageCount - 1)
  const visibleLiveStandings = liveStandings.slice(currentLivePage * 100, (currentLivePage + 1) * 100)
  const standings = publication?.standings.filter((row) => `${row.address} ${row.displayName ?? ''}`.toLowerCase().includes(search.trim().toLowerCase())) ?? []

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
      <div className="flex flex-col justify-between gap-4 sm:flex-row sm:items-end"><div><h2 id="live-leaderboard-title" className="text-2xl font-semibold">Live weekly leaderboard</h2><p className="mt-1 text-sm text-content-secondary">Current release · Ranked by weekly return on cash-flow-adjusted account value · Provisional</p></div><div className="w-full sm:max-w-xs"><label htmlFor="trader-search" className="sr-only">Search traders</label><input id="trader-search" type="search" value={search} onChange={(event) => { setSearch(event.target.value); setLivePage(0) }} placeholder="Search wallet" className="w-full border border-brand-border/35 bg-app-bg px-4 py-2.5 text-sm placeholder:text-content-tertiary" /></div></div>
      {liveQuery.isLoading ? <Panel><LoadingState /></Panel> : liveQuery.isError ? <ErrorState title="Live rankings are unavailable" message={liveQuery.error.message} onRetry={() => void liveQuery.refetch()} /> : live ? <>
        <p className="text-xs text-content-tertiary">Snapshot {formatUtc(live.snapshotAt ?? '')} · Block {live.snapshotBlock} · {live.walletCount} wallets</p>
        <Panel><div className="overflow-x-auto"><table className="w-full min-w-[480px] text-left text-sm"><caption className="sr-only">Provisional weekly ranks for current-release wallets</caption><thead className="border-b border-brand-border/25 text-xs text-content-tertiary"><tr><th scope="col" className="px-4 py-4">Rank</th><th scope="col" className="px-4 py-4">Wallet</th><th scope="col" className="px-4 py-4">Weekly return</th><th scope="col" className="px-4 py-4">Active days</th></tr></thead><tbody>{visibleLiveStandings.map((row) => <tr key={row.address} className="border-b border-brand-border/15 last:border-0"><td className="px-4 py-4 font-mono">{row.rank ?? '—'}</td><td className="px-4 py-4 font-mono" title={row.address}>{shortAddress(row.address)}</td><td className="px-4 py-4 font-mono">{row.weeklyReturnBps === null ? '—' : `${row.weeklyReturnBps > 0 ? '+' : ''}${(row.weeklyReturnBps / 100).toFixed(2)}%`}</td><td className="px-4 py-4">{row.activeDays}</td></tr>)}</tbody></table></div>{!liveStandings.length ? <EmptyState title="No matching wallets" message="Try another wallet address." /> : null}{livePageCount > 1 ? <div className="flex flex-wrap items-center justify-between gap-3 border-t border-brand-border/20 px-4 py-4 text-sm text-content-secondary"><span>{liveStandings.length.toLocaleString()} wallets · Page {currentLivePage + 1} of {livePageCount}</span><div className="flex gap-2"><button type="button" disabled={currentLivePage === 0} onClick={() => { setLivePage((page) => page - 1) }} className="border border-brand-border/40 px-3 py-2 disabled:opacity-40">Previous</button><button type="button" disabled={currentLivePage + 1 >= livePageCount} onClick={() => { setLivePage((page) => page + 1) }} className="border border-brand-border/40 px-3 py-2 disabled:opacity-40">Next</button></div></div> : null}</Panel>
      </> : <Panel><EmptyState title="Preparing live rankings" message="The backend is capturing the first complete current-week snapshot. Check back shortly." /></Panel>}
    </section>
    {publication ? <section aria-labelledby="tiers-title"><div className="mb-4 flex flex-wrap items-baseline justify-between gap-2"><h2 id="tiers-title" className="text-xl font-semibold">Six tiers. A weekly climb.</h2><span className="text-xs text-content-tertiary">Diamond takes at least five qualifying weeks</span></div><div className="grid gap-3 sm:grid-cols-3 lg:grid-cols-6">{TIERS.map((tier) => <Panel key={tier} className="p-4"><TierBadge tier={tier} size="large" showLabel /><p className="mt-3 text-sm leading-6 text-content-secondary">{TIER_REQUIREMENTS[tier]}</p></Panel>)}</div><p className="mt-3 text-sm leading-6 text-content-secondary">Qualify to move up at most one tier. Two consecutive non-qualifying weeks drop you one tier; inactive weeks count.</p></section> : null}
    {publication ? <section aria-labelledby="leaderboard-title" className="space-y-4">
      <div><h2 id="leaderboard-title" className="text-2xl font-semibold">Weekly leaderboard</h2><p className="mt-1 text-sm text-content-secondary">Published results · {formatUtc(publication.week.closesAt)}</p></div>
        {query.isError ? <div role="status" className="text-sm text-brand-yellow">Could not refresh Divisions. Showing the last published week. <button type="button" onClick={() => void query.refetch()} className="underline">Retry</button></div> : null}
        <p className="text-sm leading-6 text-content-secondary">Ranked by weekly profit as a percentage of cash-flow-adjusted equity. Badges show your earned tier after this week’s promotions and demotions.</p><Panel><Standings standings={standings} /></Panel><Announcements publication={publication} />
    </section> : null}
    {publication ? <Panel className="p-5 sm:p-7"><h2 className="text-lg font-semibold">Your rank, in the community</h2><p className="mt-2 max-w-3xl text-sm leading-6 text-content-secondary">Your badge appears beside your name on the leaderboard. <Link to="/register" className="text-brand-peach underline">Link Discord and your owner wallet</Link> to prepare for matching Discord roles. You can rank without linking Discord.</p><p className="mt-2 text-sm text-content-tertiary">Discord role assignment is coming. Monday announcements will highlight the top three weekly returns, promotions into Gold, Platinum and Diamond, and the Diamond roster.</p></Panel> : <Panel className="p-5 sm:p-7"><h2 className="text-lg font-semibold">Link Discord and your wallet</h2><p className="mt-2 max-w-3xl text-sm leading-6 text-content-secondary"><Link to="/register" className="text-brand-peach underline">Link Discord and your owner wallet</Link> to prepare for future community roles. Trading activity appears above whether you link or not.</p><p className="mt-2 text-sm text-content-tertiary">Discord role assignment is coming.</p></Panel>}
    <Link to="/archive/september-2026" className="inline-block text-sm text-content-tertiary hover:text-brand-peach">September competition archive →</Link>
  </div>
}
