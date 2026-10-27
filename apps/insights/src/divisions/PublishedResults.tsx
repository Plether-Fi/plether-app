import { Link } from 'react-router-dom'
import { EmptyState, Panel, Pnl } from '../components/ui'
import { TierBadge } from './Badge'
import { formatReturn } from './engine'
import type { DivisionPublication, DivisionStanding } from './types'
import { shortAddress } from '../utils/format'

function Trader({ standing }: { standing: DivisionStanding }) {
  return <div className="flex flex-wrap items-center gap-2"><div className="min-w-0"><span className="block break-all font-semibold">{standing.displayName ?? shortAddress(standing.address)}</span><Link to={`/divisions/wallets/${standing.address}`} className="font-mono text-xs text-brand-peach hover:underline focus-visible:underline" title={standing.address} aria-label={`View trader ${standing.address}`}>{shortAddress(standing.address)}</Link></div><TierBadge tier={standing.tier} /></div>
}

export function Announcements({ publication }: { publication: DivisionPublication }) {
  const names = (addresses: string[]) => addresses.map((address) => publication.standings.find((row) => row.address === address)?.displayName ?? shortAddress(address)).join(', ')
  return <Panel className="grid gap-6 p-5 sm:grid-cols-3 sm:p-7">
    <div><h3 className="text-sm font-semibold text-brand-peach">Traders of the Week</h3><p className="mt-2 text-sm leading-6">{names(publication.announcement.tradersOfTheWeek) || 'No profitable traders this week.'}</p><p className="mt-1 text-xs text-content-tertiary">Top 3 weekly percentage returns</p></div>
    <div><h3 className="text-sm font-semibold text-brand-peach">New Gold, Platinum &amp; Diamond</h3><ul className="mt-2 space-y-2 text-sm">{publication.announcement.promotions.map((row) => <li key={row.address}>{names([row.address])} · {row.tier}</li>)}</ul>{!publication.announcement.promotions.length ? <p className="mt-2 text-sm text-content-secondary">No promotions into these tiers this week.</p> : null}</div>
    <div><h3 className="text-sm font-semibold text-sky-200">Diamond roster</h3><p className="mt-2 text-sm leading-6">{names(publication.announcement.diamondRoster) || 'The climb to Diamond is underway.'}</p></div>
  </Panel>
}

export function Standings({ standings }: { standings: DivisionStanding[] }) {
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

