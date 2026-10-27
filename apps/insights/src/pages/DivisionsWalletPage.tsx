import { Link, useParams } from 'react-router-dom'
import { EmptyState, ErrorState, LoadingState, Panel, Pnl } from '../components/ui'
import { WalletMetric as Metric } from '../components/WalletMetric'
import { CurrentBadge } from '../divisions/CurrentBadge'
import { formatReturn } from '../divisions/engine'
import { useDivisions, useLiveDivisions } from '../divisions/query'
import { formatRoi, formatUtc, isWalletAddress, shortAddress } from '../utils/format'

function TraderDetails({ address }: { address: string }) {
  const liveQuery = useLiveDivisions()
  const publicationQuery = useDivisions()
  const live = liveQuery.data?.status === 'ready' ? liveQuery.data : null
  const publication = publicationQuery.data
  const current = live?.standings?.find((row) => row.address.toLowerCase() === address.toLowerCase())
  const awarded = publication?.standings.find((row) => row.address.toLowerCase() === address.toLowerCase())
  const handle = current?.discordUsername ?? awarded?.displayName
  const notFound = live && !current && !awarded && !liveQuery.isError && !publicationQuery.isLoading && !publicationQuery.isError

  return <div className="space-y-6">
    <Link to="/" className="text-sm text-brand-peach hover:underline">← Back to leaderboard</Link>
    <div className="flex flex-col items-start justify-between gap-4 sm:flex-row">
      <div className="min-w-0">
        <p className="text-xs font-semibold uppercase tracking-[0.16em] text-content-tertiary">Plether Divisions · Trader</p>
        <h1 className="mt-2 break-all text-3xl font-semibold sm:text-4xl">{handle ? `@${handle.replace(/^@/, '')}` : shortAddress(address)}</h1>
        <p className="mt-2 break-all font-mono text-xs text-content-secondary sm:text-sm">{address}</p>
        <a href={`https://sepolia.arbiscan.io/address/${address}`} target="_blank" rel="noreferrer" className="mt-2 inline-block text-xs text-brand-peach hover:underline">View on Arbiscan ↗</a>
      </div>
      <div className="space-y-2 sm:text-right">
        <p className="text-xs text-content-tertiary">Current badge</p>
        <CurrentBadge tier={awarded?.tier} loading={publicationQuery.isLoading} unavailable={publicationQuery.isError && !publication} />
      </div>
    </div>
    <p className="text-sm text-content-secondary">Badges are awarded on Monday after the trading week ends. Live returns do not change your current badge.</p>
    {notFound ? <Panel><EmptyState title="Trader not found" message="This wallet has no current-release trading results yet." /></Panel> : null}
    <section aria-labelledby="trader-live-title" className="space-y-3">
      <h2 id="trader-live-title" className="text-xl font-semibold">Live weekly performance</h2>
      {liveQuery.isLoading ? <Panel><LoadingState /></Panel> : liveQuery.isError && !live ? <ErrorState title="Live performance is unavailable" message={liveQuery.error.message} onRetry={() => void liveQuery.refetch()} /> : current && live ? <>
        <p className="text-xs text-content-tertiary">Provisional · Week starting {formatUtc(live.weekStartsAt)} · Snapshot {formatUtc(live.snapshotAt ?? '')}</p>
        {liveQuery.isError ? <p role="status" className="text-sm text-brand-yellow">Could not refresh live performance. Showing the last available snapshot.</p> : null}
        <Panel><dl className="grid grid-cols-1 divide-y divide-brand-border/15 sm:grid-cols-3 sm:divide-y-0">
          <Metric label="Weekly rank">{current.rank ?? '—'}</Metric>
          <Metric label="Weekly return">{formatRoi(current.weeklyReturnBps)}</Metric>
          <Metric label="Active days">{current.activeDays}</Metric>
        </dl></Panel>
      </> : <Panel><EmptyState title={live ? 'No live weekly results' : 'Preparing live rankings'} message={live ? 'No current-week snapshot is available for this wallet.' : 'The backend is preparing the current-week snapshot. Check back shortly.'} /></Panel>}
    </section>
    <section aria-labelledby="trader-awarded-title" className="space-y-3">
      <h2 id="trader-awarded-title" className="text-xl font-semibold">Last published week</h2>
      {publicationQuery.isLoading ? <Panel><LoadingState /></Panel> : publicationQuery.isError && !publication ? <ErrorState title="Published results are unavailable" message={publicationQuery.error.message} onRetry={() => void publicationQuery.refetch()} /> : awarded && publication ? <>
        <p className="text-xs text-content-tertiary">Week ending {formatUtc(publication.week.closesAt)} · Published {formatUtc(publication.publishedAt)}</p>
        {publicationQuery.isError ? <p role="status" className="text-sm text-brand-yellow">Could not refresh published results. Showing the last published week.</p> : null}
        <Panel><dl className="grid grid-cols-1 divide-y divide-brand-border/15 sm:grid-cols-2 lg:grid-cols-4 lg:divide-y-0">
          <Metric label="Final rank">{awarded.rank ?? '—'}</Metric>
          <Metric label="Weekly return">{formatReturn(awarded.weeklyPnlUsdc, awarded.adjustedEquityUsdc)}</Metric>
          <Metric label="Net P&L"><Pnl value={awarded.weeklyPnlUsdc} usdcKind="mock" /></Metric>
          <Metric label="Active days">{awarded.activeDays}</Metric>
        </dl></Panel>
        <p className="text-sm text-content-secondary">{awarded.movement === 'promoted' ? `Promoted from ${awarded.previousTier} to ${awarded.tier}.` : awarded.movement === 'demoted' ? `Demoted from ${awarded.previousTier} to ${awarded.tier}.` : awarded.qualifying ? 'Qualifying week · tier held.' : 'Non-qualifying week.'}</p>
      </> : <Panel><EmptyState title="No published award yet" message="This trader awaits their first completed, published weekly result." /></Panel>}
    </section>
  </div>
}

export function DivisionsWalletPage() {
  const { address = '' } = useParams()
  if (!isWalletAddress(address)) return <ErrorState title="Invalid wallet address" message="Enter a complete 0x-prefixed Ethereum wallet address." />
  return <TraderDetails address={address} />
}
