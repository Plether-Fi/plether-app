import { useEffect, useState, type ReactNode } from 'react'
import { Link, useLocation } from 'react-router-dom'
import { useInsightsStatus } from '../api'
import { useLiveDivisionsSummary } from '../divisions/query'
import {
  NAV_PILLARS,
  resolveActivePillar,
  resolveSectionSubNav,
} from '../navigation/routes'
import { resolvePerpsChain } from '../utils/chain'
import { formatUtc } from '../utils/format'
import { GlobalSearch } from './GlobalSearch'

function pillarLinkClass(isActive: boolean): string {
  return [
    'whitespace-nowrap border px-2.5 py-1.5 text-[13px] font-semibold transition-colors focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-brand-peach',
    isActive
      ? 'border-brand-orange bg-brand-orange text-content-primary'
      : 'border-transparent text-content-secondary hover:border-brand-orange/45 hover:bg-brand-orange/10 hover:text-content-primary',
  ].join(' ')
}

function HeaderTelemetryPill() {
  const statusQuery = useInsightsStatus()
  const live = useLiveDivisionsSummary()
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
      className="hidden shrink-0 items-center gap-1.5 whitespace-nowrap border border-brand-border/30 bg-app-bg px-2.5 py-1.5 font-mono text-[11px] text-content-secondary transition-colors hover:border-brand-peach/50 hover:text-content-primary xl:inline-flex"
    >
      <span className={`h-1.5 w-1.5 shrink-0 rounded-full ${dotClass}`} />
      <span className="text-content-primary">{shortNetwork}</span>
    </Link>
  )
}

function SectionSubNav() {
  const location = useLocation()
  const { pillar, activeTab, show } = resolveSectionSubNav(location.pathname)

  if (!show || pillar.tabs.length === 0) {
    return null
  }

  return (
    <div className="border-b border-brand-border/25 bg-surface-panel/55">
      <nav
        aria-label={`${pillar.label} sections`}
        className="mx-auto flex max-w-7xl items-center gap-1 overflow-x-auto px-3 sm:px-6 lg:px-8"
      >
        {pillar.tabs.map((tab) => {
          const isCurrent = activeTab?.to === tab.to
          return (
            <Link
              key={tab.to}
              to={tab.to}
              aria-current={isCurrent ? 'page' : undefined}
              className={`relative shrink-0 whitespace-nowrap border-b-2 px-3 py-2.5 text-xs font-semibold transition-colors ${
                isCurrent
                  ? 'border-brand-orange text-content-primary'
                  : 'border-transparent text-content-secondary hover:border-brand-border/50 hover:text-content-primary'
              }`}
            >
              {tab.label}
            </Link>
          )
        })}
      </nav>
    </div>
  )
}

function Header() {
  const location = useLocation()
  const activePillar = resolveActivePillar(location.pathname)
  const [drawerOpenAtPath, setDrawerOpenAtPath] = useState<string | null>(null)
  const mobileOpen = drawerOpenAtPath === location.pathname

  useEffect(() => {
    if (!mobileOpen) return
    function onKeyDown(e: KeyboardEvent) {
      if (e.key === 'Escape') {
        setDrawerOpenAtPath(null)
      }
    }
    document.addEventListener('keydown', onKeyDown)
    return () => {
      document.removeEventListener('keydown', onKeyDown)
    }
  }, [mobileOpen])

  return (
    <header className="sticky top-0 z-40 border-b border-brand-border/30 bg-surface-panel">
      <div className="mx-auto flex max-w-7xl items-center justify-between gap-3 px-3 py-2.5 sm:px-6 lg:px-8">
        <div className="flex min-w-0 shrink-0 items-center gap-4 xl:gap-6">
          <Link
            to="/"
            className="flex shrink-0 items-center gap-2 py-0.5 transition-opacity hover:opacity-90"
            aria-label="Plether Insights home"
          >
            <img src="/logomark.svg" alt="" className="h-7 w-7 shrink-0" />
            <img src="/logotype.svg" alt="Plether" className="hidden h-5 w-auto sm:block" />
            <span className="whitespace-nowrap border-l border-brand-border/35 pl-2 text-sm font-semibold text-brand-peach">
              Insights
            </span>
          </Link>

          <nav
            className="hidden shrink-0 items-center gap-1 lg:flex"
            aria-label="Primary navigation"
          >
            {NAV_PILLARS.map((pillar) => {
              const isActive = activePillar.id === pillar.id
              return (
                <Link
                  key={pillar.id}
                  to={pillar.to}
                  aria-current={isActive ? 'page' : undefined}
                  className={pillarLinkClass(isActive)}
                >
                  {pillar.label}
                </Link>
              )
            })}
          </nav>
        </div>

        <div className="flex min-w-0 flex-1 items-center justify-end gap-2 sm:flex-none">
          <GlobalSearch className="w-full max-w-[220px] sm:w-52 xl:w-60" />
          <HeaderTelemetryPill />
          <button
            type="button"
            aria-label={mobileOpen ? 'Close navigation menu' : 'Open navigation menu'}
            aria-expanded={mobileOpen}
            aria-controls="mobile-nav-drawer"
            onClick={() => {
              setDrawerOpenAtPath((prev) => (prev === location.pathname ? null : location.pathname))
            }}
            className="inline-flex h-8 w-8 shrink-0 cursor-pointer items-center justify-center border border-brand-border/35 bg-app-bg font-mono text-sm text-content-primary transition-colors hover:border-brand-peach hover:text-brand-peach lg:hidden"
          >
            <span aria-hidden="true">{mobileOpen ? '✕' : '☰'}</span>
          </button>
        </div>
      </div>

      {mobileOpen ? (
        <nav
          id="mobile-nav-drawer"
          aria-label="Mobile navigation"
          className="max-h-[calc(100dvh-3.5rem)] overflow-y-auto border-t border-brand-border/25 bg-surface-panel px-4 py-4 lg:hidden"
        >
          <div className="mx-auto max-w-7xl space-y-4">
            {NAV_PILLARS.map((pillar) => {
              const isActivePillar = activePillar.id === pillar.id
              if (pillar.tabs.length === 0) {
                return (
                  <div key={pillar.id}>
                    <Link
                      to={pillar.to}
                      onClick={() => {
                        setDrawerOpenAtPath(null)
                      }}
                      className={`block border px-3 py-2 text-sm font-semibold transition-colors ${
                        isActivePillar
                          ? 'border-brand-orange bg-brand-orange text-content-primary'
                          : 'border-brand-border/25 bg-app-bg text-content-primary hover:border-brand-peach'
                      }`}
                    >
                      {pillar.label}
                    </Link>
                  </div>
                )
              }

              return (
                <div key={pillar.id} className="space-y-1.5">
                  <p className="font-mono text-[11px] font-semibold uppercase tracking-[0.14em] text-brand-peach">
                    {pillar.label}
                  </p>
                  <div className="grid gap-1 sm:grid-cols-2">
                    {pillar.tabs.map((tab) => {
                      const isTabActive =
                        location.pathname === tab.to || location.pathname.startsWith(`${tab.to}/`)
                      return (
                        <Link
                          key={tab.to}
                          to={tab.to}
                          onClick={() => {
                            setDrawerOpenAtPath(null)
                          }}
                          className={`block border px-3 py-2 transition-colors ${
                            isTabActive
                              ? 'border-brand-orange/60 bg-brand-orange/15 text-content-primary'
                              : 'border-brand-border/20 bg-app-bg/60 text-content-secondary hover:border-brand-border/45 hover:text-content-primary'
                          }`}
                        >
                          <div className="flex items-center justify-between text-xs font-semibold">
                            <span>{tab.label}</span>
                            {isTabActive ? (
                              <span className="font-mono text-[10px] text-brand-peach">Active</span>
                            ) : null}
                          </div>
                          <p className="mt-0.5 text-[11px] leading-4 text-content-tertiary">
                            {tab.description}
                          </p>
                        </Link>
                      )
                    })}
                  </div>
                </div>
              )
            })}
          </div>
        </nav>
      ) : null}
    </header>
  )
}

function Footer() {
  const statusQuery = useInsightsStatus()
  const live = useLiveDivisionsSummary()
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
    <div className="flex min-h-screen min-h-dvh flex-col overflow-x-clip bg-app-bg text-content-primary">
      <Header />
      <SectionSubNav />
      <main className="mx-auto w-full max-w-7xl flex-1 px-3 py-5 sm:px-6 sm:py-7 lg:px-8">
        {children}
      </main>
      <Footer />
    </div>
  )
}
