import type { ReactNode } from 'react'
import { Link, NavLink } from 'react-router-dom'
import { useInsightsStatus } from '../api'
import { trackOutboundLinkOpened } from '../analytics/insights'
import { formatUtc } from '../utils/format'

function navClass({ isActive }: { isActive: boolean }): string {
  return [
    'border px-3 py-2 text-sm font-semibold transition-colors sm:px-4',
    isActive
      ? 'border-brand-orange bg-brand-orange text-content-primary'
      : 'border-transparent text-content-secondary hover:border-brand-orange/50 hover:bg-brand-orange/10 hover:text-content-primary',
  ].join(' ')
}

function Header() {
  return (
    <header className="sticky top-0 z-40 border-b border-brand-border/25 bg-surface-panel/95 backdrop-blur">
      <div className="mx-auto flex min-h-16 max-w-7xl items-center justify-between gap-4 px-4 sm:px-6 lg:px-8">
        <Link to="/" className="flex min-w-0 items-center gap-2.5 py-3" aria-label="Plether Insights home">
          <img src="/logomark.svg" alt="" className="h-7 w-7 shrink-0 sm:h-8 sm:w-8" />
          <img src="/logotype.svg" alt="Plether" className="hidden h-6 w-auto sm:block" />
          <span className="hidden border-l border-brand-border/35 pl-2.5 text-sm font-semibold tracking-wide text-brand-peach sm:block sm:text-base">
            Insights
          </span>
        </Link>
        <nav className="flex items-center gap-1" aria-label="Primary navigation">
          <NavLink to="/" end className={navClass}>Leaderboard</NavLink>
          <NavLink to="/methodology" className={navClass}>Methodology</NavLink>
        </nav>
      </div>
    </header>
  )
}

function Footer() {
  const status = useInsightsStatus()
  const statusData = status.data
  const indexedAt = statusData?.latestIndexedAt
  const isLive = statusData?.healthy === true && !status.isError

  return (
    <footer className="mt-auto border-t border-brand-border/20 bg-app-bg-deep/40">
      <div className="mx-auto flex max-w-7xl flex-col gap-4 px-4 py-6 text-xs text-content-tertiary sm:flex-row sm:items-center sm:justify-between sm:px-6 lg:px-8">
        <div className="flex flex-wrap items-center gap-x-4 gap-y-2">
          <span>© 2026 Plether</span>
          <Link to="/methodology" className="text-content-secondary hover:text-brand-peach">Scoring methodology</Link>
          <a href="https://plether.com" onClick={trackOutboundLinkOpened} className="text-content-secondary hover:text-brand-peach">Trade on Plether ↗</a>
        </div>
        <div className="flex items-center gap-2" title={indexedAt ? `Indexed ${formatUtc(indexedAt)}` : undefined}>
          <span className={`h-2 w-2 rounded-full ${isLive ? 'bg-positive' : status.isLoading ? 'bg-brand-yellow' : 'bg-brand-orange'}`} />
          {status.isLoading
            ? 'Checking indexer'
            : isLive
              ? `Indexed through block ${statusData.latestIndexedBlock?.toLocaleString() ?? '—'}`
              : 'Indexer status unavailable'}
        </div>
      </div>
    </footer>
  )
}

export function Layout({ children }: { children: ReactNode }) {
  return (
    <div className="flex min-h-screen flex-col">
      <Header />
      <main className="mx-auto w-full max-w-7xl flex-1 px-4 py-8 sm:px-6 sm:py-10 lg:px-8">
        {children}
      </main>
      <Footer />
    </div>
  )
}
