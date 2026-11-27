import { useEffect, useRef, useState, type ReactNode } from 'react'
import { Link, NavLink, useLocation } from 'react-router-dom'
import { useInsightsStatus } from '../api'
import { useLiveDivisions } from '../divisions/query'
import { resolvePerpsChain } from '../utils/chain'
import { formatUtc } from '../utils/format'
import { GlobalSearch } from './GlobalSearch'

const OPS_ROUTES = [
  {
    to: '/oracle',
    label: 'Oracle',
    description: 'Pyth 6-feed admission & value-guard health',
  },
  {
    to: '/keepers',
    label: 'Keepers',
    description: 'Permissionless keeper fleet, latency & bounties',
  },
  {
    to: '/trading-accounts',
    label: 'Trading Accounts',
    description: 'ZeroDev Kernel v4 smart wallets, ETH vs. sponsored gas & ops',
  },
  {
    to: '/sponsored',
    label: 'Sponsored',
    description: 'ERC-4337 Paymaster runway & UserOp receipts',
  },
  {
    to: '/infra',
    label: 'Infra',
    description: 'AWS ECS workers, RDS pool, Nitro RPC & bundler',
  },
] as const

function navClass({ isActive }: { isActive: boolean }): string {
  return [
    'whitespace-nowrap border px-2 py-1.5 text-[13px] font-semibold transition-colors hover:underline hover:underline-offset-4 focus-visible:underline focus-visible:underline-offset-4',
    isActive
      ? 'border-brand-orange bg-brand-orange text-content-primary'
      : 'border-transparent text-content-secondary hover:border-brand-orange/50 hover:bg-brand-orange/15 hover:text-content-primary',
  ].join(' ')
}

function OperationsDropdown({ align = 'left' }: { align?: 'left' | 'right' }) {
  const location = useLocation()
  const [openAtPath, setOpenAtPath] = useState<string | null>(null)
  const containerRef = useRef<HTMLDivElement>(null)
  const open = openAtPath === location.pathname

  const activeOp = OPS_ROUTES.find(
    (item) => location.pathname === item.to || location.pathname.startsWith(`${item.to}/`),
  )

  useEffect(() => {
    if (!open) return
    function onPointerDown(e: MouseEvent) {
      if (containerRef.current && !containerRef.current.contains(e.target as Node)) {
        setOpenAtPath(null)
      }
    }
    function onKeyDown(e: KeyboardEvent) {
      if (e.key === 'Escape') setOpenAtPath(null)
    }
    document.addEventListener('mousedown', onPointerDown)
    document.addEventListener('keydown', onKeyDown)
    return () => {
      document.removeEventListener('mousedown', onPointerDown)
      document.removeEventListener('keydown', onKeyDown)
    }
  }, [open])

  return (
    <div ref={containerRef} className="relative">
      <button
        type="button"
        aria-haspopup="menu"
        aria-expanded={open}
        onClick={() => {
          setOpenAtPath((prev) => (prev === location.pathname ? null : location.pathname))
        }}
        className={`inline-flex cursor-pointer items-center gap-1 whitespace-nowrap border px-2 py-1.5 text-[13px] font-semibold transition-colors ${
          activeOp
            ? 'border-brand-orange bg-brand-orange text-content-primary'
            : open
              ? 'border-brand-orange/50 bg-brand-orange/15 text-content-primary'
              : 'border-transparent text-content-secondary hover:border-brand-orange/50 hover:bg-brand-orange/15 hover:text-content-primary'
        }`}
      >
        <span>{activeOp ? `Ops · ${activeOp.label}` : 'Operations'}</span>
        <span aria-hidden="true" className="text-[10px] opacity-80">
          {open ? '▴' : '▾'}
        </span>
      </button>

      {open ? (
        <div
          role="menu"
          aria-label="Operations pages"
          className={`absolute top-full z-50 mt-1.5 w-72 border border-brand-border/35 bg-surface-panel p-1.5 shadow-xl ${
            align === 'right' ? 'right-0' : 'left-0'
          }`}
        >
          {OPS_ROUTES.map((item) => {
            const isActive =
              location.pathname === item.to || location.pathname.startsWith(`${item.to}/`)
            return (
              <Link
                key={item.to}
                to={item.to}
                role="menuitem"
                onClick={() => {
                  setOpenAtPath(null)
                }}
                className={`block border px-3 py-2 transition-colors ${
                  isActive
                    ? 'border-brand-orange/60 bg-brand-orange/15 text-content-primary'
                    : 'border-transparent text-content-secondary hover:border-brand-border/30 hover:bg-app-bg hover:text-content-primary'
                }`}
              >
                <div className="flex items-center justify-between text-xs font-semibold">
                  <span>{item.label}</span>
                  {isActive ? (
                    <span className="font-mono text-[10px] text-brand-peach">Active</span>
                  ) : null}
                </div>
                <p className="mt-0.5 text-[11px] leading-4 text-content-tertiary">
                  {item.description}
                </p>
              </Link>
            )
          })}
        </div>
      ) : null}
    </div>
  )
}

function HeaderTelemetryPill() {
  const statusQuery = useInsightsStatus()
  const live = useLiveDivisions()
  const chain = resolvePerpsChain(statusQuery.data?.chainId)
  const snapshot = live.data?.status === 'ready' ? live.data : null
  const blockNumber = snapshot?.snapshotBlock ?? statusQuery.data?.latestIndexedBlock ?? null
  const observedAt = snapshot?.snapshotAt ?? statusQuery.data?.latestIndexedAt ?? null
  const healthy = snapshot !== null || statusQuery.data?.healthy === true
  const checking = live.isLoading && statusQuery.isLoading

  const dotClass = healthy ? 'bg-positive' : checking ? 'bg-brand-yellow' : 'bg-brand-orange'
  const tooltip = [
    chain.name,
    blockNumber != null ? `Block #${Number(blockNumber).toLocaleString('en-US')}` : null,
    observedAt ? `Observed ${formatUtc(observedAt)}` : null,
  ]
    .filter(Boolean)
    .join(' · ')

  const shortNetwork = chain.isTestnet ? 'Sepolia' : 'Arbitrum'

  return (
    <Link
      to="/infra"
      title={tooltip || 'Protocol telemetry status'}
      className="hidden shrink-0 items-center gap-1.5 whitespace-nowrap border border-brand-border/30 bg-app-bg px-2 py-1.5 font-mono text-[11px] text-content-secondary transition-colors hover:border-brand-peach/50 hover:text-content-primary xl:inline-flex"
    >
      <span className={`h-1.5 w-1.5 shrink-0 rounded-full ${dotClass}`} />
      <span className="text-content-primary">{shortNetwork}</span>
    </Link>
  )
}

function Header() {
  return (
    <header className="sticky top-0 z-40 border-b border-brand-border/30 bg-surface-panel py-2 sm:py-3">
      <div className="mx-auto flex max-w-7xl flex-wrap items-center justify-between gap-3 px-4 sm:px-6 lg:flex-nowrap lg:px-8">
        <div className="flex min-w-0 shrink-0 items-center gap-3 xl:gap-5">
          <Link
            to="/"
            className="flex shrink-0 items-center gap-2 py-0.5 transition-opacity hover:opacity-90"
            aria-label="Plether Insights home"
          >
            <img src="/logomark.svg" alt="" className="h-7 w-7 shrink-0 sm:h-7 sm:w-7" />
            <img src="/logotype.svg" alt="Plether" className="hidden h-5 w-auto sm:block" />
            <span className="whitespace-nowrap border-l border-brand-border/35 pl-2 text-sm font-semibold text-brand-peach">
              Insights
            </span>
          </Link>

          <nav
            className="hidden shrink-0 items-center gap-0.5 lg:flex"
            aria-label="Primary navigation"
          >
            <NavLink to="/" end className={navClass}>Overview</NavLink>
            <NavLink to="/execution" className={navClass}>Execution</NavLink>
            <NavLink to="/liquidity" className={navClass}>LP Ops</NavLink>
            <NavLink to="/treasury" className={navClass}>Treasury</NavLink>
            <OperationsDropdown />
            <NavLink to="/protocol" className={navClass}>Protocol</NavLink>
            <NavLink to="/divisions" className={navClass}>Divisions</NavLink>
          </nav>
        </div>

        <div className="flex shrink-0 items-center gap-2">
          <GlobalSearch className="w-40 sm:w-44 lg:w-44 xl:w-52" />
          <HeaderTelemetryPill />
        </div>

        <nav
          className="flex w-full flex-wrap items-center gap-1 pb-0.5 lg:hidden"
          aria-label="Mobile navigation"
        >
          <NavLink to="/" end className={navClass}>Overview</NavLink>
          <NavLink to="/execution" className={navClass}>Execution</NavLink>
          <NavLink to="/liquidity" className={navClass}>LP Ops</NavLink>
          <NavLink to="/treasury" className={navClass}>Treasury</NavLink>
          <OperationsDropdown align="left" />
          <NavLink to="/protocol" className={navClass}>Protocol</NavLink>
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
