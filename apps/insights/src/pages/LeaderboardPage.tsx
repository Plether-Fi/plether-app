import { useState } from 'react'
import { Link } from 'react-router-dom'
import { EmptyState, ErrorState, LoadingState, Panel, Pnl } from '../components/ui'
import { TierBadge } from '../divisions/Badge'
import { formatReturn } from '../divisions/engine'
import { useDivisions, useTradedWallets } from '../divisions/query'
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
  const walletsQuery = useTradedWallets()
  const now = useUtcNow(10_000)
  const week = tradingWeek(now)
  const [search, setSearch] = useState('')
  const [walletPage, setWalletPage] = useState(0)
  const publication = query.data
  const standings = publication?.standings.filter((row) => `${row.address} ${row.displayName ?? ''}`.toLowerCase().includes(search.trim().toLowerCase())) ?? []
  const tradedWallets = walletsQuery.data?.traders.filter((row) => row.address.toLowerCase().includes(search.trim().toLowerCase())) ?? []
  const walletPageCount = Math.max(1, Math.ceil(tradedWallets.length / 100))
  const currentWalletPage = Math.min(walletPage, walletPageCount - 1)
  const visibleWallets = tradedWallets.slice(currentWalletPage * 100, (currentWalletPage + 1) * 100)

  return <div className="space-y-8">
    <section className="relative overflow-hidden border border-brand-border/30 bg-surface-panel p-6 sm:p-10">
      <div className="absolute -right-10 -top-20 text-[18rem] text-brand-peach/5" aria-hidden="true">◆</div>
      <div className="relative max-w-3xl">
        <p className="text-xs font-semibold uppercase tracking-[0.2em] text-brand-peach">{publication ? 'Testnet · Weekly ranked trading' : 'Testnet · Trading activity'}</p>
        <h1 className="mt-4 text-4xl font-semibold tracking-tight sm:text-6xl">Plether Divisions</h1>
        <p className="mt-5 max-w-2xl text-lg leading-7 text-content-secondary">{publication ? 'Trade consistently. Climb the ranks. Earn your place.' : 'Follow trading activity across every traded wallet.'}</p>
        <p className="mt-3 max-w-2xl text-sm leading-6 text-content-secondary">{publication ? 'Weekly percentage P&L rewards skilled trading at every bankroll size. Build your public status from Iron to Diamond, one qualifying week at a time.' : 'Wallets appear as their trades are indexed, whether or not they have linked Discord.'}</p>
        <div className="mt-6 flex flex-wrap gap-3"><a href="https://app.sepolia.plether.com" className="border border-brand-orange bg-brand-orange px-5 py-3 text-sm font-semibold hover:bg-brand-peach hover:text-app-bg">Start trading ↗</a><Link to="/register" className="border border-brand-border/40 px-5 py-3 text-sm font-semibold hover:bg-brand-peach/10">Link Discord and wallet</Link><Link to="/methodology" className="border border-brand-border/40 px-5 py-3 text-sm font-semibold hover:bg-brand-peach/10">How Divisions work</Link></div>
      </div>
      {publication ? <dl className="relative mt-8 grid gap-5 border-t border-brand-border/20 pt-6 sm:grid-cols-3">
        <div><dt className="text-xs text-content-tertiary">Friday snapshot</dt><dd className="mt-2 text-sm font-semibold">{formatUtc(week.closesAt)}</dd></div>
        <div><dt className="text-xs text-content-tertiary">{now < Date.parse(week.closesAt) ? 'Time to close' : 'Monday announcement'}</dt><dd className="mt-2 font-mono text-sm text-brand-peach">{now < Date.parse(week.closesAt) ? formatCountdown(week.closesAt, now) : formatUtc(week.publishesAt)}</dd></div>
        <div><dt className="text-xs text-content-tertiary">Qualifying week</dt><dd className="mt-2 text-sm font-semibold">In profit after fees · 2+ active days</dd></div>
      </dl> : null}
    </section>
    {publication ? <section aria-labelledby="tiers-title"><div className="mb-4 flex flex-wrap items-baseline justify-between gap-2"><h2 id="tiers-title" className="text-xl font-semibold">Six tiers. A weekly climb.</h2><span className="text-xs text-content-tertiary">Diamond takes at least five qualifying weeks</span></div><div className="grid gap-3 sm:grid-cols-3 lg:grid-cols-6">{TIERS.map((tier) => <Panel key={tier} className="p-4"><TierBadge tier={tier} size="large" showLabel /><p className="mt-3 text-sm leading-6 text-content-secondary">{TIER_REQUIREMENTS[tier]}</p></Panel>)}</div><p className="mt-3 text-sm leading-6 text-content-secondary">Qualify to move up at most one tier. Two consecutive non-qualifying weeks drop you one tier; inactive weeks count.</p></section> : null}
    <section aria-labelledby="leaderboard-title" className="space-y-4">
      <div className="flex flex-col justify-between gap-4 sm:flex-row sm:items-end"><div><h2 id="leaderboard-title" className="text-2xl font-semibold">{publication ? 'Weekly leaderboard' : 'All traded wallets'}</h2><p className="mt-1 text-sm text-content-secondary">{publication ? `Published results · ${formatUtc(publication.week.closesAt)}` : 'Live trading roster, ordered by most recent executed trade. Discord registration is not required.'}</p></div><div className="w-full sm:max-w-xs"><label htmlFor="trader-search" className="sr-only">Search traders</label><input id="trader-search" type="search" value={search} onChange={(event) => { setSearch(event.target.value); setWalletPage(0) }} placeholder="Search name or wallet" className="w-full border border-brand-border/35 bg-app-bg px-4 py-2.5 text-sm placeholder:text-content-tertiary" /></div></div>
      {publication ? <>
        {query.isError ? <div role="status" className="text-sm text-brand-yellow">Could not refresh Divisions. Showing the last published week. <button type="button" onClick={() => void query.refetch()} className="underline">Retry</button></div> : null}
        <p className="text-sm leading-6 text-content-secondary">Ranked by weekly profit as a percentage of cash-flow-adjusted equity. Badges show your earned tier after this week’s promotions and demotions.</p><Panel><Standings standings={standings} /></Panel><Announcements publication={publication} />
        <div className="pt-4"><h3 className="text-lg font-semibold">All traded wallets</h3><p className="mt-1 text-sm text-content-secondary">Live trading roster, ordered by most recent executed trade. This list does not depend on Discord registration.</p></div>
      </> : null}
      {walletsQuery.isLoading ? <Panel><LoadingState /></Panel> : walletsQuery.isError ? <ErrorState title="Traded wallets are unavailable" message={walletsQuery.error.message} onRetry={() => void walletsQuery.refetch()} /> : <Panel>
        {tradedWallets.length ? <><div className="overflow-x-auto"><table className="w-full min-w-[480px] text-left text-sm"><caption className="sr-only">All wallets with an indexed executed trade</caption><thead className="border-b border-brand-border/25 text-xs text-content-tertiary"><tr><th scope="col" className="px-4 py-4">Wallet</th><th scope="col" className="px-4 py-4">Executed trades</th><th scope="col" className="px-4 py-4">Latest trade</th></tr></thead><tbody>{visibleWallets.map((row) => <tr key={row.address} className="border-b border-brand-border/15 last:border-0"><td className="px-4 py-4 font-mono" title={row.address}>{shortAddress(row.address)}</td><td className="px-4 py-4">{row.executedTrades}</td><td className="px-4 py-4">{formatUtc(row.lastTradeAt)}</td></tr>)}</tbody></table></div><div className="flex flex-wrap items-center justify-between gap-3 border-t border-brand-border/20 px-4 py-4 text-sm text-content-secondary"><span>{tradedWallets.length.toLocaleString()} traded wallets · Page {currentWalletPage + 1} of {walletPageCount}</span><div className="flex gap-2"><button type="button" disabled={currentWalletPage === 0} onClick={() => { setWalletPage((page) => page - 1) }} className="border border-brand-border/40 px-3 py-2 disabled:opacity-40">Previous</button><button type="button" disabled={currentWalletPage + 1 >= walletPageCount} onClick={() => { setWalletPage((page) => page + 1) }} className="border border-brand-border/40 px-3 py-2 disabled:opacity-40">Next</button></div></div></> : <EmptyState title={search ? 'No matching wallets' : 'No traded wallets indexed yet'} message={search ? 'Try another wallet address.' : 'Executed trades will appear here after the indexer confirms them.'} />}
      </Panel>}
    </section>
    {publication ? <Panel className="p-5 sm:p-7"><h2 className="text-lg font-semibold">Your rank, in the community</h2><p className="mt-2 max-w-3xl text-sm leading-6 text-content-secondary">Your badge appears beside your name on the leaderboard. <Link to="/register" className="text-brand-peach underline">Link Discord and your owner wallet</Link> to prepare for matching Discord roles. You can rank without linking Discord.</p><p className="mt-2 text-sm text-content-tertiary">Discord role assignment is coming. Monday announcements will highlight the top three weekly returns, promotions into Gold, Platinum and Diamond, and the Diamond roster.</p></Panel> : <Panel className="p-5 sm:p-7"><h2 className="text-lg font-semibold">Link Discord and your wallet</h2><p className="mt-2 max-w-3xl text-sm leading-6 text-content-secondary"><Link to="/register" className="text-brand-peach underline">Link Discord and your owner wallet</Link> to prepare for future community roles. Trading activity appears above whether you link or not.</p><p className="mt-2 text-sm text-content-tertiary">Discord role assignment is coming.</p></Panel>}
    <Link to="/archive/september-2026" className="inline-block text-sm text-content-tertiary hover:text-brand-peach">September competition archive →</Link>
  </div>
}
