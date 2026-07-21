import { useDeferredValue, useState, type SyntheticEvent } from 'react'
import { useNavigate } from 'react-router-dom'
import { useCurrentCompetition, useLeaderboard } from '../api'
import { CompetitionHero, CompetitionStats, Leaderboard, LeaderboardTitle, RulesSummary } from '../components/Competition'
import { ErrorState, LoadingState, Panel, ProvisionalNotice } from '../components/ui'
import { isWalletAddress } from '../utils/format'

function LeaderboardContent({ slug, search }: { slug: string; search: string }) {
  const query = useLeaderboard(slug, search)
  const standings = query.data?.pages.flatMap((page) => page.standings) ?? []
  const provisional = query.data?.pages[0]?.provisional ?? true

  if (query.isLoading) return <Panel><LoadingState rows={7} /></Panel>
  if (query.isError) return <ErrorState message={query.error.message} onRetry={() => void query.refetch()} />

  return (
    <div className="space-y-3">
      {provisional ? <ProvisionalNotice /> : null}
      <div className="border border-brand-peach/30 bg-brand-peach/5 px-4 py-3 text-sm leading-6 text-content-secondary" role="note">
        <strong className="text-content-primary">Ranked by net P&amp;L.</strong> Directional realized and unrealized P&amp;L exclude execution fees, VPI, carry, and execution rewards. Accounts with no activity remain at 0.00 and rank above active accounts whose net return is negative.
      </div>
      <Panel>
        <Leaderboard standings={standings} search={search} competitionSlug={slug} />
      </Panel>
      {query.hasNextPage ? (
        <div className="text-center">
          <button type="button" onClick={() => void query.fetchNextPage()} disabled={query.isFetchingNextPage} className="border border-brand-border/40 px-5 py-2.5 text-sm font-semibold text-content-primary transition-colors hover:border-brand-orange hover:bg-brand-orange/10 disabled:cursor-not-allowed disabled:opacity-60">
            {query.isFetchingNextPage ? 'Loading…' : 'Load more traders'}
          </button>
        </div>
      ) : null}
    </div>
  )
}

export function LeaderboardPage() {
  const competition = useCurrentCompetition()
  const [search, setSearch] = useState('')
  const deferredSearch = useDeferredValue(search.trim())
  const navigate = useNavigate()

  function submitSearch(event: SyntheticEvent<HTMLFormElement>) {
    event.preventDefault()
    const slug = competition.data?.slug
    if (slug && isWalletAddress(search)) {
      void navigate(`/competitions/${encodeURIComponent(slug)}/wallets/${search.trim()}`)
    }
  }

  if (competition.isLoading) {
    return <div className="space-y-8"><div className="skeleton h-64" /><Panel><LoadingState rows={7} /></Panel></div>
  }
  if (competition.isError) {
    return <ErrorState title="Competition data is unavailable" message={competition.error.message} onRetry={() => void competition.refetch()} />
  }
  const competitionData = competition.data
  if (!competitionData) {
    return <ErrorState title="Competition data is unavailable" />
  }

  return (
    <div className="space-y-8">
      <div>
        <CompetitionHero competition={competitionData} />
        <CompetitionStats competition={competitionData} />
      </div>

      <RulesSummary />

      <section aria-labelledby="leaderboard-title" className="space-y-4">
        <div className="flex flex-col gap-4 sm:flex-row sm:items-end sm:justify-between">
          <div id="leaderboard-title"><LeaderboardTitle count={competitionData.participantCount ?? 0} /></div>
          <form onSubmit={submitSearch} role="search" className="flex w-full max-w-md">
            <label htmlFor="trader-search" className="sr-only">Search traders</label>
            <input id="trader-search" type="search" value={search} onChange={(event) => { setSearch(event.target.value) }} placeholder="Search alias or wallet" autoComplete="off" className="min-w-0 flex-1 border border-brand-border/35 bg-app-bg px-4 py-2.5 text-sm text-content-primary placeholder:text-content-tertiary focus:border-brand-peach focus:outline-none" />
            <button type="submit" className="border border-brand-orange bg-brand-orange px-4 py-2.5 text-sm font-semibold hover:bg-brand-peach hover:text-app-bg">Search</button>
          </form>
        </div>
        <LeaderboardContent slug={competitionData.slug} search={deferredSearch} />
      </section>
    </div>
  )
}
