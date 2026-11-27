import { Link } from 'react-router-dom'
import { AddressAvatar } from '../components/AddressAvatar'
import { EmptyState, Panel, Pnl } from '../components/ui'
import { shortAddress } from '../utils/format'
import { TierBadge } from './Badge'
import { formatReturn } from './engine'
import type { DivisionPublication, DivisionStanding } from './types'

export function Announcements({ publication }: { publication: DivisionPublication }) {
  const names = (addresses: string[]) =>
    addresses
      .map(
        (address) =>
          publication.standings.find((row) => row.address === address)?.displayName ??
          shortAddress(address),
      )
      .join(', ')
  return (
    <Panel className="grid gap-6 p-5 sm:grid-cols-3 sm:p-6">
      <div>
        <h3 className="text-xs font-semibold uppercase tracking-[0.12em] text-brand-peach">
          Traders of the Week
        </h3>
        <p className="mt-2 text-sm leading-6">
          {names(publication.announcement.tradersOfTheWeek) || 'No profitable traders this week.'}
        </p>
        <p className="mt-1 text-xs text-content-tertiary">Top 3 weekly percentage returns</p>
      </div>
      <div>
        <h3 className="text-xs font-semibold uppercase tracking-[0.12em] text-brand-peach">
          New Gold, Platinum &amp; Diamond
        </h3>
        <ul className="mt-2 space-y-1.5 text-sm">
          {publication.announcement.promotions.map((row) => (
            <li key={row.address}>
              {names([row.address])} · {row.tier}
            </li>
          ))}
        </ul>
        {!publication.announcement.promotions.length ? (
          <p className="mt-2 text-sm text-content-secondary">
            No promotions into these tiers this week.
          </p>
        ) : null}
      </div>
      <div>
        <h3 className="text-xs font-semibold uppercase tracking-[0.12em] text-sky-200">
          Diamond roster
        </h3>
        <p className="mt-2 text-sm leading-6">
          {names(publication.announcement.diamondRoster) || 'The climb to Diamond is underway.'}
        </p>
      </div>
    </Panel>
  )
}

export function Standings({
  standings,
  linkToCurrentWallet = true,
  assetKind,
}: {
  standings: DivisionStanding[]
  linkToCurrentWallet?: boolean
  assetKind?: 'real' | 'mock'
}) {
  if (!standings.length) {
    return (
      <EmptyState title="No traders found" message="Try a different name or wallet address." />
    )
  }
  return (
    <div className="overflow-x-auto">
      <table className="w-full min-w-[1040px] text-left text-sm">
        <caption className="sr-only">Published weekly percentage P&amp;L and division badges</caption>
        <thead className="border-b border-brand-border/25 text-xs uppercase tracking-[0.08em] text-content-tertiary">
          <tr>
            {[
              'Rank',
              'Badge',
              'Wallet',
              'Discord handle',
              'Weekly return',
              'Active days',
              'Net P&L',
              'Weekly outcome',
            ].map((label) => (
              <th key={label} scope="col" className="px-4 py-3.5">
                {label}
              </th>
            ))}
          </tr>
        </thead>
        <tbody className="divide-y divide-brand-border/15">
          {standings.map((row) => {
            const retText = formatReturn(row.weeklyPnlAssets, row.adjustedEquityAssets)
            const retTone = retText.startsWith('+')
              ? 'text-positive'
              : retText.startsWith('-') || retText.startsWith('−')
                ? 'text-brand-orange'
                : 'text-content-secondary'
            return (
              <tr key={row.address} className="hover:bg-brand-peach/[0.03]">
                <td className="px-4 py-3.5 font-mono text-xs tabular-nums">{row.rank ?? '—'}</td>
                <td className="px-4 py-3.5">
                  <TierBadge tier={row.tier} showLabel />
                </td>
                <td className="px-4 py-3.5 font-mono text-xs">
                  <div className="inline-flex items-center gap-2">
                    <AddressAvatar address={row.address} size={18} />
                    {linkToCurrentWallet ? (
                      <Link
                        to={`/divisions/wallets/${row.address}`}
                        className="text-brand-peach hover:underline focus-visible:underline"
                        title={row.address}
                        aria-label={`View trader ${row.address}`}
                      >
                        {shortAddress(row.address)}
                      </Link>
                    ) : (
                      <span title={row.address}>{shortAddress(row.address)}</span>
                    )}
                  </div>
                </td>
                <td className="px-4 py-3.5 text-xs">
                  {row.displayName ? (
                    <span className="break-all font-medium text-content-primary">
                      @{row.displayName.replace(/^@/, '')}
                    </span>
                  ) : (
                    <span className="text-content-tertiary" aria-label="Discord not linked">
                      —
                    </span>
                  )}
                </td>
                <td className={`px-4 py-3.5 font-mono text-xs font-semibold tabular-nums ${retTone}`}>
                  {retText}
                </td>
                <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                  {row.activeDays} / 2
                </td>
                <td className="px-4 py-3.5">
                  <Pnl value={row.weeklyPnlAssets} assetKind={assetKind} />
                </td>
                <td className="px-4 py-3.5 text-xs">
                  <span
                    className={
                      row.movement === 'promoted'
                        ? 'font-semibold text-positive'
                        : row.movement === 'demoted'
                          ? 'text-brand-orange'
                          : 'text-content-secondary'
                    }
                  >
                    {row.movement === 'promoted'
                      ? `↑ ${row.previousTier} → ${row.tier}`
                      : row.movement === 'demoted'
                        ? `↓ ${row.previousTier} → ${row.tier}`
                        : row.qualifying
                          ? 'Qualified · tier held'
                          : 'Non-qualifying'}
                  </span>
                  {row.nonQualifyingWeeks === 1 ? (
                    <p className="mt-0.5 text-[11px] text-brand-yellow">
                      1 of 2 non-qualifying weeks
                    </p>
                  ) : null}
                </td>
              </tr>
            )
          })}
        </tbody>
      </table>
    </div>
  )
}
