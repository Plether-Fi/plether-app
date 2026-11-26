import type { ReactNode } from 'react'
import { Link, NavLink } from 'react-router-dom'
import { useInsightsStatus } from '../api'
import { useLiveDivisions } from '../divisions/query'
import { resolvePerpsChain } from '../utils/chain'
import { formatUtc } from '../utils/format'
import { WalletSearch } from './WalletSearch'

function navClass({ isActive }: { isActive: boolean }): string {
  return [
    'whitespace-nowrap border px-2.5 py-1.5 text-[13px] font-semibold transition-colors',
    isActive
      ? 'border-brand-orange bg-brand-orange text-content-primary'
      : 'border-transparent text-content-secondary hover:border-brand-orange/50 hover:bg-brand-orange/10 hover:text-content-primary',
  ].join(' ')
}

function Header() {
  const statusQuery = useInsightsStatus()
  const chain = resolvePerpsChain(statusQuery.data?.chainId)

  return (
    <header className="sticky top-0 z-40 border-b border-brand-border/25 bg-surface-panel/95 backdrop-blur">
      <div className="mx-auto flex min-h-16 max-w-7xl flex-wrap items-center justify-between gap-3 px-4 py-2 sm:px-6 lg:flex-nowrap lg:px-8">
        <Link
          to="/"
          className="flex shrink-0 items-center gap-2 py-1"
          aria-label="Plether Insights home"
        >
          <img src="/logomark.svg" alt="" className="h-7 w-7 shrink-0" />
          <img src="/logotype.svg" alt="Plether" className="hidden h-5 w-auto sm:block" />
          <span className="whitespace-nowrap border-l border-brand-border/35 pl-2 text-sm font-semibold tracking-wide text-brand-peach">
            Insights
          </span>
          <span className="hidden whitespace-nowrap border border-brand-border/25 bg-app-bg px-2 py-0.5 font-mono text-[11px] text-content-secondary 2xl:inline-block">
            {chain.shortName}
          </span>
        </Link>

        <nav
          className="order-3 flex w-full items-center gap-0.5 overflow-x-auto pb-1 lg:order-2 lg:w-auto lg:overflow-visible lg:pb-0"
          aria-label="Primary navigation"
        >
          <NavLink to="/" end className={navClass}>Overview</NavLink>
          <NavLink to="/execution" className={navClass}>Execution</NavLink>
          <NavLink to="/liquidity" className={navClass}>LP Ops</NavLink>
          <NavLink to="/keepers" className={navClass}>Keepers</NavLink>
          <NavLink to="/divisions" className={navClass}>Divisions</NavLink>
          <NavLink to="/protocol" className={navClass}>Protocol</NavLink>
          <NavLink to="/infra" className={navClass}>Infra</NavLink>
          <a href={chain.tradeAppUrl} className={navClass({ isActive: false })}>Trade ↗</a>
        </nav>

        <div className="order-2 w-full shrink-0 sm:w-56 lg:order-3 lg:w-52 xl:w-56">
          <WalletSearch compact />
        </div>
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

  return (
    <footer className="mt-auto border-t border-brand-border/20 bg-app-bg-deep/40">
      <div className="mx-auto flex max-w-7xl flex-col gap-4 px-4 py-6 text-xs text-content-tertiary sm:flex-row sm:items-center sm:justify-between sm:px-6 lg:px-8">
        <div className="flex flex-wrap items-center gap-x-4 gap-y-2">
          <span>© 2026 Plether</span>
          <Link to="/results" className="text-content-secondary hover:text-brand-peach">Weekly results</Link>
          <Link to="/methodology" className="text-content-secondary hover:text-brand-peach">Divisions methodology</Link>
          <Link to="/register" className="text-content-secondary hover:text-brand-peach">Link Discord</Link>
          <Link to="/archive/september-2026" className="text-content-secondary hover:text-brand-peach">September archive</Link>
          <a href={chain.tradeAppUrl} className="text-content-secondary hover:text-brand-peach">
            Trade on Plether ({chain.shortName}) ↗
          </a>
        </div>
        <div
          className="flex items-center gap-2 font-mono"
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
    <div className="flex min-h-screen flex-col">
      <Header />
      <main className="mx-auto w-full max-w-7xl flex-1 px-4 py-8 sm:px-6 sm:py-10 lg:px-8">
        {children}
      </main>
      <Footer />
    </div>
  )
}
