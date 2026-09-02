import type { ReactNode, SyntheticEvent } from 'react'
import { Link, NavLink, useNavigate } from 'react-router-dom'
import {
  DEFAULT_COMPETITION_SLUG,
  useInsightsStatus,
  type Competition,
} from '../api'
import { formatUtc } from '../utils/format'

function navClass({ isActive }: { isActive: boolean }): string {
  return [
    'shrink-0 border px-3 py-2 text-xs font-semibold transition-colors sm:text-sm',
    isActive
      ? 'border-brand-orange bg-brand-orange text-content-primary'
      : 'border-transparent text-content-secondary hover:border-brand-orange/50 hover:bg-brand-orange/10 hover:text-content-primary',
  ].join(' ')
}

function Header({
  competition,
  explorerEnabled,
  protocolReleaseId,
}: {
  competition?: Competition
  explorerEnabled: boolean
  protocolReleaseId?: string
}) {
  const navigate = useNavigate()
  const registrationOpen = competition?.registration?.status === 'open'
  const competitionSlug = competition?.slug ?? DEFAULT_COMPETITION_SLUG
  const competitionPath = `/competitions/${encodeURIComponent(competitionSlug)}`
  const registrationPath = competition
    ? `${competitionPath}/register`
    : '/register'

  function search(event: SyntheticEvent<HTMLFormElement>) {
    event.preventDefault()
    const rawValue = new FormData(event.currentTarget).get('protocol-search')
    const value = typeof rawValue === 'string' ? rawValue.trim() : ''
    if (!value) return
    if (/^0x[a-fA-F0-9]{64}$/.test(value)) {
      void navigate(`/transactions/${value}`)
    } else if (/^\d+$/.test(value)) {
      void navigate(`/orders/${protocolReleaseId ?? 'current'}/${value}`)
    } else if (/^0x[a-fA-F0-9]{40}$/.test(value)) {
      void navigate(`/transactions?address=${value}`)
    } else {
      void navigate(`/transactions?transactionHash=${encodeURIComponent(value)}`)
    }
  }

  return (
    <header className="sticky top-0 z-40 border-b border-brand-border/25 bg-surface-panel/95 backdrop-blur">
      <div className="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8">
        <div className="flex min-h-16 items-center justify-between gap-3">
          <Link to="/" className="flex min-w-0 items-center gap-2.5 py-3" aria-label="Plether Insights home">
            <img src="/logomark.svg" alt="" className="h-7 w-7 shrink-0 sm:h-8 sm:w-8" />
            <img src="/logotype.svg" alt="Plether" className="hidden h-6 w-auto sm:block" />
            <span className="hidden border-l border-brand-border/35 pl-2.5 text-sm font-semibold tracking-wide text-brand-peach md:block">Insights</span>
          </Link>
          {explorerEnabled ? (
            <form onSubmit={search} role="search" className="flex min-w-0 max-w-xl flex-1 justify-end">
              <label htmlFor="protocol-search" className="sr-only">Search transaction, order, trading account, or keeper</label>
              <input id="protocol-search" name="protocol-search" type="search" placeholder="Tx, order, account, keeper…" autoComplete="off" className="min-w-0 w-full max-w-md border border-brand-border/30 bg-app-bg px-3 py-2 font-mono text-xs text-content-primary placeholder:font-sans placeholder:text-content-tertiary" />
              <button type="submit" className="border border-brand-orange bg-brand-orange px-3 py-2 text-xs font-semibold hover:bg-brand-peach hover:text-app-bg">Search</button>
            </form>
          ) : null}
        </div>
        <nav className="-mx-4 flex items-center gap-1 overflow-x-auto border-t border-brand-border/10 px-4 py-2 sm:mx-0 sm:px-0" aria-label="Primary navigation">
          {explorerEnabled ? (
            <>
              <NavLink to="/" end className={navClass}>Overview</NavLink>
              <NavLink to="/transactions" className={navClass}>Transactions</NavLink>
              <NavLink to="/house-pool" className={navClass}>HousePool</NavLink>
              <NavLink to="/keepers" className={navClass}>Keepers</NavLink>
              <NavLink to="/protocol-wallets" className={navClass}>Wallets</NavLink>
              <NavLink to="/parameters" className={navClass}>Parameters</NavLink>
            </>
          ) : null}
          <NavLink to={competitionPath} className={navClass}>
            {explorerEnabled ? 'Competition' : 'Leaderboard'}
          </NavLink>
          <NavLink to="/methodology" className={navClass}>Methodology</NavLink>
          <NavLink to={registrationPath} className={navClass}>
            {registrationOpen ? 'Enter competition' : 'Application'}
          </NavLink>
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
          <a href="https://app.sepolia.plether.com" className="text-content-secondary hover:text-brand-peach">Trade on Plether testnet ↗</a>
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

export function Layout({
  children,
  competition,
  explorerEnabled = false,
  protocolReleaseId,
}: {
  children: ReactNode
  competition?: Competition
  explorerEnabled?: boolean
  protocolReleaseId?: string
}) {
  return (
    <div className="flex min-h-screen flex-col">
      <Header
        competition={competition}
        explorerEnabled={explorerEnabled}
        protocolReleaseId={protocolReleaseId}
      />
      <main className="mx-auto w-full max-w-7xl flex-1 px-4 py-8 sm:px-6 sm:py-10 lg:px-8">
        {children}
      </main>
      <Footer />
    </div>
  )
}
