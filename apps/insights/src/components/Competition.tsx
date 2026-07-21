import { useEffect, useState } from 'react'
import type { Competition, Standing } from '../api'
import { formatCompactUsdc, formatRoi, formatUsdc, formatUtc } from '../utils/format'
import { EmptyState, Panel, Pnl, StatusBadge, WalletIdentity } from './ui'

function useCountdown(target: string): string {
  const [now, setNow] = useState(() => Date.now())

  useEffect(() => {
    const timer = window.setInterval(() => {
      setNow(Date.now())
    }, 1_000)
    return () => {
      window.clearInterval(timer)
    }
  }, [])

  const remaining = Math.max(0, new Date(target).getTime() - now)
  const seconds = Math.floor(remaining / 1_000)
  const days = Math.floor(seconds / 86_400)
  const hours = Math.floor((seconds % 86_400) / 3_600)
  const minutes = Math.floor((seconds % 3_600) / 60)
  const finalSeconds = seconds % 60
  return `${String(days)}d ${hours.toString().padStart(2, '0')}h ${minutes.toString().padStart(2, '0')}m ${finalSeconds.toString().padStart(2, '0')}s`
}

function statusLabel(status: Competition['status']): string {
  const labels: Record<Competition['status'], string> = {
    scheduled: 'Starts soon',
    live: 'Live competition',
    ended: 'Trading closed',
    review: 'Under review',
    final: 'Results final',
  }
  return labels[status]
}

export function CompetitionHero({ competition }: { competition: Competition }) {
  const target = competition.status === 'scheduled' ? competition.startsAt : competition.tradingCutoffAt
  const countdown = useCountdown(target)
  const showCountdown = competition.status === 'scheduled' || competition.status === 'live'

  return (
    <div className="relative overflow-hidden border border-brand-border/25 bg-surface-panel px-5 py-7 sm:px-8 sm:py-9">
      <div className="absolute -right-20 -top-28 h-72 w-72 rounded-full bg-brand-orange/10 blur-3xl" aria-hidden="true" />
      <div className="relative grid gap-8 lg:grid-cols-[1fr_auto] lg:items-end">
        <div>
          <div className="mb-4 flex flex-wrap items-center gap-3">
            <span className={`inline-flex items-center gap-2 rounded-full border px-3 py-1 text-xs font-semibold uppercase tracking-wider ${competition.status === 'live' ? 'border-positive/40 bg-positive/10 text-positive' : 'border-brand-yellow/40 bg-brand-yellow/10 text-brand-yellow'}`}>
              <span className={`h-1.5 w-1.5 rounded-full ${competition.status === 'live' ? 'animate-pulse bg-positive' : 'bg-brand-yellow'}`} />
              {statusLabel(competition.status)}
            </span>
            <span className="text-xs uppercase tracking-[0.18em] text-content-tertiary">Arbitrum Sepolia</span>
          </div>
          <h1 className="max-w-3xl text-3xl font-semibold leading-tight sm:text-4xl lg:text-5xl">{competition.name}</h1>
          <p className="mt-4 max-w-2xl text-sm leading-6 text-content-secondary sm:text-base">
            Follow every registered trader’s performance, activity, and prize eligibility from finalized onchain data.
          </p>
        </div>
        <div className="min-w-64 border-l-2 border-brand-orange pl-4 lg:text-right">
          <p className="text-xs font-semibold uppercase tracking-[0.16em] text-content-tertiary">
            {showCountdown ? (competition.status === 'live' ? 'Trading closes in' : 'Trading starts in') : 'Trading closed'}
          </p>
          <p className="mt-1 font-mono text-xl font-semibold tabular-nums text-brand-peach sm:text-2xl">
            {showCountdown ? countdown : formatUtc(competition.tradingCutoffAt)}
          </p>
          {showCountdown ? <p className="mt-2 text-xs text-content-tertiary">Cutoff {formatUtc(competition.tradingCutoffAt)}</p> : null}
        </div>
      </div>
    </div>
  )
}

function prizePool(prizes: Competition['prizes']): string {
  try {
    return formatUsdc(prizes.reduce((total, prize) => total + BigInt(prize.amount), 0n).toString())
  } catch {
    return '1,000.00 USDC'
  }
}

export function CompetitionStats({ competition }: { competition: Competition }) {
  const stats = [
    { label: 'Prize pool', value: prizePool(competition.prizes), accent: true },
    { label: 'Starting balance', value: formatUsdc(competition.startingBalance) },
    { label: 'Prize threshold', value: `+${formatUsdc(competition.pnlEligibilityThreshold)}` },
    { label: 'Minimum activity', value: `${String(competition.minActiveDays)} active days` },
    { label: 'Registered traders', value: competition.participantCount?.toLocaleString() ?? '—' },
  ]

  return (
    <div className="grid grid-cols-2 border-x border-b border-brand-border/25 sm:grid-cols-3 lg:grid-cols-5">
      {stats.map((stat, index) => (
        <div key={stat.label} className={`bg-app-bg/40 px-4 py-4 sm:px-5 ${index > 0 ? 'border-l border-brand-border/15' : ''} ${index >= 2 ? 'border-t border-brand-border/15 sm:border-t-0' : ''} ${index >= 3 ? 'lg:border-t-0' : ''}`}>
          <div className="text-[11px] font-semibold uppercase tracking-[0.14em] text-content-tertiary">{stat.label}</div>
          <div className={`mt-1 text-sm font-semibold tabular-nums sm:text-base ${stat.accent ? 'text-brand-peach' : 'text-content-primary'}`}>{stat.value}</div>
        </div>
      ))}
    </div>
  )
}

function Rank({ value, prizePlace }: { value: number | null; prizePlace: number | null }) {
  return (
    <div className="flex min-w-9 flex-col items-center gap-1 font-mono text-xs font-semibold">
      <span className="text-content-secondary" title="Overall net P&L rank">#{value ?? '—'}</span>
      {prizePlace !== null ? <span className="bg-brand-orange px-1.5 py-0.5 text-content-primary" title={`Prize place ${String(prizePlace)}`}>P{prizePlace}</span> : null}
    </div>
  )
}

function PrizeAward({ standing }: { standing: Standing }) {
  if (standing.prizePlace === null || standing.prizeAmountUsdc === null) return null
  const places = standing.prizePlaces.length > 0 ? standing.prizePlaces : [standing.prizePlace]
  const placeLabel = places.length > 1 ? `Places ${places.join('–')} tie` : `Place #${String(places[0])}`
  return <div className="mt-1 text-[11px] font-semibold uppercase tracking-wide text-brand-yellow">{placeLabel} · {formatUsdc(standing.prizeAmountUsdc)}</div>
}

function DesktopTable({ standings, competitionSlug }: { standings: Standing[]; competitionSlug: string }) {
  return (
    <div className="hidden overflow-x-auto lg:block">
      <table className="w-full min-w-[920px] border-collapse text-left">
        <thead>
          <tr className="border-b border-brand-border/20 text-[11px] font-semibold uppercase tracking-[0.14em] text-content-tertiary">
            <th className="w-20 px-5 py-3">Rank</th>
            <th className="px-3 py-3">Trader</th>
            <th className="px-3 py-3 text-right">Net P&amp;L</th>
            <th className="px-3 py-3 text-right">Net return</th>
            <th className="px-3 py-3 text-right">Volume</th>
            <th className="px-3 py-3 text-right">Trades</th>
            <th className="px-3 py-3 text-right">Active days</th>
            <th className="px-5 py-3 text-right">Eligibility</th>
          </tr>
        </thead>
        <tbody className="divide-y divide-brand-border/15">
          {standings.map((standing) => (
            <tr key={standing.address} className={`transition-colors hover:bg-brand-peach/5 ${standing.prizePlace !== null ? 'bg-brand-yellow/5' : ''}`}>
              <td className="px-5 py-4"><Rank value={standing.rank} prizePlace={standing.prizePlace} /></td>
              <td className="px-3 py-4"><WalletIdentity address={standing.address} displayName={standing.displayName} competitionSlug={competitionSlug} /><PrizeAward standing={standing} /></td>
              <td className="px-3 py-4 text-right font-semibold"><Pnl value={standing.pnl} /></td>
              <td className={`px-3 py-4 text-right text-sm tabular-nums ${standing.roiBps !== null && standing.roiBps >= 0 ? 'text-positive' : 'text-brand-orange'}`}>{formatRoi(standing.roiBps)}</td>
              <td className="px-3 py-4 text-right text-sm tabular-nums text-content-secondary">{formatCompactUsdc(standing.volume)}</td>
              <td className="px-3 py-4 text-right text-sm tabular-nums">{standing.trades}</td>
              <td className="px-3 py-4 text-right text-sm tabular-nums">{standing.activeDays}<span className="text-content-tertiary"> / 5</span></td>
              <td className="px-5 py-4 text-right"><StatusBadge eligible={standing.eligible} label={eligibilityLabel(standing)} /></td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

function MobileList({ standings, competitionSlug }: { standings: Standing[]; competitionSlug: string }) {
  return (
    <div className="divide-y divide-brand-border/15 lg:hidden">
      {standings.map((standing) => (
        <div key={standing.address} className={`px-4 py-4 transition-colors hover:bg-brand-peach/5 ${standing.prizePlace !== null ? 'bg-brand-yellow/5' : ''}`}>
          <div className="flex items-start gap-3">
            <Rank value={standing.rank} prizePlace={standing.prizePlace} />
            <div className="min-w-0 flex-1">
              <div className="flex items-start justify-between gap-3">
                <div className="min-w-0"><WalletIdentity address={standing.address} displayName={standing.displayName} competitionSlug={competitionSlug} /><PrizeAward standing={standing} /></div>
                <div className="text-right">
                  <div className="text-[10px] font-semibold uppercase tracking-wider text-content-tertiary">Net P&amp;L</div>
                  <Pnl value={standing.pnl} className="whitespace-nowrap text-sm font-semibold" />
                </div>
              </div>
              <div className="mt-3 flex items-center justify-between gap-3 text-xs text-content-tertiary">
                <span>{standing.activeDays} active days · {standing.trades} trades</span>
                <StatusBadge eligible={standing.eligible} label={eligibilityLabel(standing)} />
              </div>
            </div>
          </div>
        </div>
      ))}
    </div>
  )
}

export function Leaderboard({ standings, search, competitionSlug }: { standings: Standing[]; search: string; competitionSlug: string }) {
  if (standings.length === 0) {
    return <EmptyState title={search ? 'No matching traders' : 'No standings yet'} message={search ? 'Try a different alias or full wallet address.' : 'Standings will appear after the first finalized trades are indexed.'} />
  }
  return <><DesktopTable standings={standings} competitionSlug={competitionSlug} /><MobileList standings={standings} competitionSlug={competitionSlug} /></>
}

export function LeaderboardTitle({ count }: { count: number }) {
  return (
    <div>
      <h2 className="text-xl font-semibold sm:text-2xl">Leaderboard</h2>
      <p className="mt-1 text-sm text-content-secondary">Overall rank by net account return after trading costs · prize places exclude ineligible traders · {count} {count === 1 ? 'trader' : 'traders'} shown</p>
    </div>
  )
}

export function RulesSummary() {
  return (
    <Panel className="grid gap-px bg-brand-border/20 sm:grid-cols-3">
      <div className="bg-surface-panel p-5"><p className="text-xs font-semibold uppercase tracking-wider text-content-tertiary">Win condition</p><p className="mt-2 text-sm leading-6 text-content-secondary">Finish at a <strong className="text-content-primary">+1% net return or better</strong> after trading costs and log at least five active FX-session days.</p></div>
      <div className="bg-surface-panel p-5"><p className="text-xs font-semibold uppercase tracking-wider text-content-tertiary">Prizes</p><p className="mt-2 text-sm leading-6 text-content-secondary"><strong className="text-content-primary">600.00 / 300.00 / 100.00 USDC</strong> for the top three eligible traders.</p></div>
      <div className="bg-surface-panel p-5"><p className="text-xs font-semibold uppercase tracking-wider text-content-tertiary">Fair play</p><p className="mt-2 text-sm leading-6 text-content-secondary">One wallet per trader. Wash trading, mirrored wallets, and sybil accounts are ineligible.</p></div>
    </Panel>
  )
}

function eligibilityLabel(standing: Standing): string {
  if (standing.eligible) return 'Eligible'
  if (standing.eligibilityStatus === 'pending') return 'Pending review'
  if (standing.eligibilityStatus === 'under_review') return 'Under review'
  return 'Not eligible'
}
