import { useState } from 'react'
import { Link, useSearchParams } from 'react-router-dom'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { Announcements, Standings } from '../divisions/PublishedResults'
import { useDivisionsHistory } from '../divisions/query'
import { formatUtc } from '../utils/format'

export function WeeklyResultsPage() {
  const [params] = useSearchParams()
  const week = params.get('week')
  const query = useDivisionsHistory(week)
  const [search, setSearch] = useState('')
  const history = query.data
  const publication = history?.publication
  const standings = publication?.standings.filter((row) => `${row.address} ${row.displayName ? `@${row.displayName}` : ''}`.toLowerCase().includes(search.trim().toLowerCase())) ?? []
  const pageLink = (cursor: string) => `/results?week=${encodeURIComponent(cursor)}`
  const buttonClass = 'border border-brand-border/40 px-4 py-2 text-sm font-semibold hover:bg-brand-peach/10'

  return <div className="space-y-6">
    <Link to="/" className="text-sm text-brand-peach hover:underline">← Live leaderboard</Link>
    <div>
      <h1 className="text-3xl font-semibold sm:text-4xl">{week === null ? 'Last week’s results' : 'Past weekly results'}</h1>
      <p className="mt-3 max-w-3xl text-sm leading-6 text-content-secondary">Final weekly rankings and awarded badges. Browse earlier weeks to see their results, promotions, and Traders of the Week.</p>
    </div>
    {query.isLoading ? <Panel><LoadingState /></Panel> : query.isError && !history ? <>
      <ErrorState title="Weekly results are unavailable" message={query.error.message} onRetry={() => void query.refetch()} />
      {week !== null ? <Link to="/results" className="text-sm text-brand-peach underline">Latest results</Link> : null}
    </> : publication ? <>
      <nav aria-label="Results weeks" className="flex flex-wrap items-center justify-between gap-3">
        {history.olderCursor ? <Link to={pageLink(history.olderCursor)} className={buttonClass}>← Older week</Link> : <button type="button" disabled className={`${buttonClass} opacity-40`}>← Older week</button>}
        <div className="flex flex-wrap gap-3">
          {week !== null ? <Link to="/results" className={buttonClass}>Latest results</Link> : null}
          {history.newerCursor ? <Link to={pageLink(history.newerCursor)} className={buttonClass}>Newer week →</Link> : <button type="button" disabled className={`${buttonClass} opacity-40`}>Newer week →</button>}
        </div>
      </nav>
      <section aria-labelledby="results-week-title" className="space-y-4">
        <div className="flex flex-col justify-between gap-4 sm:flex-row sm:items-end">
          <div><h2 id="results-week-title" className="text-xl font-semibold">Week ending {formatUtc(publication.week.closesAt)}</h2><p className="mt-1 text-xs text-content-tertiary">Published {formatUtc(publication.publishedAt)} · {publication.standings.length.toLocaleString()} traders</p></div>
          <div className="w-full sm:max-w-xs"><label htmlFor="results-search" className="sr-only">Search results</label><input id="results-search" type="search" value={search} onChange={(event) => { setSearch(event.target.value) }} placeholder="Search wallet or Discord handle" className="w-full border border-brand-border/35 bg-app-bg px-4 py-2.5 text-sm placeholder:text-content-tertiary" /></div>
        </div>
        {query.isError ? <p role="status" className="text-sm text-brand-yellow">Could not refresh results. Showing the saved week. <button type="button" onClick={() => void query.refetch()} className="underline">Retry</button></p> : null}
        <p className="text-sm leading-6 text-content-secondary">Ranked by weekly profit as a percentage of cash-flow-adjusted equity. Badges show the tier awarded for this completed week.</p>
        <Panel>{publication.standings.length ? <Standings standings={standings} /> : <EmptyState title="No traders this week" message="This week was published with no trading accounts." />}</Panel>
      </section>
      <Announcements publication={publication} />
    </> : <Panel><EmptyState title="No published results yet" message="The first completed week’s results will appear after Monday publication." /></Panel>}
  </div>
}
