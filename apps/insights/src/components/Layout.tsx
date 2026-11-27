import type { ReactNode } from 'react'
import { Link, NavLink } from 'react-router-dom'
import { useInsightsStatus } from '../api'
import { useLiveDivisions } from '../divisions/query'
import { resolvePerpsChain } from '../utils/chain'
import { formatUtc } from '../utils/format'

function navClass({ isActive }: { isActive: boolean }): string {
  return [
    'whitespace-nowrap border px-3 py-2 text-sm font-semibold transition-colors hover:underline hover:underline-offset-4 focus-visible:underline focus-visible:underline-offset-4',
    isActive
      ? 'border-brand-orange bg-brand-orange text-content-primary'
      : 'border-transparent text-content-secondary hover:border-brand-orange/50 hover:bg-brand-orange/15 hover:text-content-primary',
  ].join(' ')
}

function Header() {
  return (
    <header className="sticky top-0 z-40 border-b border-brand-border/30 bg-surface-panel py-2 sm:py-3.5">
      <div className="mx-auto flex max-w-7xl flex-wrap items-center justify-between gap-3 px-4 sm:px-6 lg:flex-nowrap lg:px-8">
        <div className="flex min-w-0 items-center gap-4 lg:gap-8">
          <Link
            to="/"
            className="flex shrink-0 items-center gap-2 py-0.5 transition-opacity hover:opacity-90 sm:gap-2.5"
            aria-label="Plether Insights home"
          >
            <img src="/logomark.svg" alt="" className="h-7 w-7 shrink-0 sm:h-8 sm:w-8" />
            <img src="/logotype.svg" alt="Plether" className="hidden h-5 w-auto sm:block sm:h-6" />
            <span className="whitespace-nowrap border-l border-brand-border/35 pl-2.5 text-sm font-semibold text-brand-peach">
              Insights
            </span>
          </Link>

          <nav
            className="hidden items-center gap-1 lg:flex"
            aria-label="Primary navigation"
          >
            <NavLink to="/" end className={navClass}>Overview</NavLink>
            <NavLink to="/execution" className={navClass}>Execution</NavLink>
            <NavLink to="/oracle" className={navClass}>Oracle</NavLink>
            <NavLink to="/liquidity" className={navClass}>LP Ops</NavLink>
            <NavLink to="/keepers" className={navClass}>Keepers</NavLink>
            <NavLink to="/sponsored" className={navClass}>Sponsored</NavLink>
            <NavLink to="/protocol" className={navClass}>Protocol</NavLink>
            <NavLink to="/infra" className={navClass}>Infra</NavLink>
            <NavLink to="/divisions" className={navClass}>Divisions</NavLink>
          </nav>
        </div>

        <nav
          className="flex w-full items-center gap-1 overflow-x-auto pb-0.5 lg:hidden"
          aria-label="Mobile navigation"
        >
          <NavLink to="/" end className={navClass}>Overview</NavLink>
          <NavLink to="/execution" className={navClass}>Execution</NavLink>
          <NavLink to="/oracle" className={navClass}>Oracle</NavLink>
          <NavLink to="/liquidity" className={navClass}>LP Ops</NavLink>
          <NavLink to="/keepers" className={navClass}>Keepers</NavLink>
          <NavLink to="/sponsored" className={navClass}>Sponsored</NavLink>
          <NavLink to="/protocol" className={navClass}>Protocol</NavLink>
          <NavLink to="/infra" className={navClass}>Infra</NavLink>
          <NavLink to="/divisions" className={navClass}>Divisions</NavLink>
        </nav>
      </div>
    </header>
  )
}

function Footer() {
  const statusQuery = useInsightsStatus()
  const live = useLiveDivisions()
  const chain = resolvePerpsChain(statusQuery.data?.chainId)
  const snapshot = live.data?.status === 'ready' ? live.data : null
  const blockNumber = snapshot?.snapshotBlock ?? statusQuery.data?.latestIndexedBlock ?? null
  const observedAt = snapshot?.snapshotAt ?? statusQuery.data?.latestIndexedAt ?? null
  const healthy = snapshot !== null || statusQuery.data?.healthy === true
  const checking = live.isLoading && statusQuery.isLoading
  const linkClass =
    'text-content-secondary transition-colors hover:text-content-primary hover:underline hover:underline-offset-4'

  return (
    <footer className="mt-auto border-t border-brand-border/30 bg-surface-panel/50">
      <div className="mx-auto flex max-w-7xl flex-col gap-3 px-4 py-5 text-xs text-content-secondary sm:flex-row sm:items-center sm:justify-between sm:px-6 lg:px-8">
        <div className="flex flex-wrap items-center gap-x-4 gap-y-2">
          <span>© 2026 Plether</span>
          <Link to="/results" className={linkClass}>Weekly results</Link>
          <Link to="/methodology" className={linkClass}>Methodology</Link>
          <Link to="/register" className={linkClass}>Link Discord</Link>
          <Link to="/archive/september-2026" className={linkClass}>September archive</Link>
          <a href={chain.tradeAppUrl} className={linkClass}>
            Trade ({chain.shortName}) ↗
          </a>
        </div>
        <div
          className="flex items-center gap-2 font-mono text-content-secondary"
          title={observedAt ? `Observed ${formatUtc(observedAt)}` : undefined}
        >
          <span
            className={`h-2 w-2 rounded-full ${
              healthy ? 'bg-positive' : checking ? 'bg-brand-yellow' : 'bg-brand-orange'
            }`}
          />
          {checking
            ? 'Checking protocol telemetry'
            : blockNumber != null
              ? `${chain.shortName} · Block ${Number(blockNumber).toLocaleString('en-US')}`
              : live.isError && statusQuery.isError
                ? 'Protocol telemetry unavailable'
                : 'Syncing protocol telemetry'}
        </div>
      </div>
    </footer>
  )
}

export function Layout({ children }: { children: ReactNode }) {
  return (
    <div className="flex min-h-screen min-h-dvh flex-col bg-app-bg text-content-primary">
      <Header />
      <main className="mx-auto w-full max-w-7xl flex-1 px-4 py-6 sm:px-6 sm:py-8 lg:px-8">
        {children}
      </main>
      <Footer />
    </div>
  )
}
