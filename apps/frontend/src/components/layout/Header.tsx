import { Link, useLocation } from 'react-router-dom'
import { ConnectButton } from '../wallet/ConnectButton'
import { PendingTxBadge } from '../PendingTxBadge'
import { isPrimaryAppDeployment } from '../../utils/deployment'

const navLinks = [
  { path: '/', label: 'Perps', perpsSurface: true },
  { path: '/vaults', label: 'Vaults', perpsSurface: true },
]

const leaderboardUrl = 'https://insights.plether.com'

export function Header() {
  const location = useLocation()
  const shouldHidePerps = isPrimaryAppDeployment()
  const visibleNavLinks = shouldHidePerps
    ? navLinks.filter(({ perpsSurface }) => !perpsSurface)
    : navLinks
  const homePath = shouldHidePerps ? '/spot' : '/'

  return (
    <header className="border-b border-brand-border/30 bg-surface-panel py-3 sm:py-4">
      <div className="flex w-full min-w-0 items-center justify-between gap-2 px-3 sm:px-6 lg:px-8">
        <div className="flex min-w-0 items-center gap-4 lg:gap-10">
          <Link to={homePath} className="flex shrink-0 items-center gap-2.5 px-1 py-0.5 transition-opacity hover:opacity-90">
            <img src="/logomark.svg" alt="Plether" className="h-8 w-8" />
            <img src="/logotype.svg" alt="" aria-hidden="true" className="hidden h-7 w-auto min-[430px]:block" />
          </Link>

          <nav className="hidden min-w-0 items-center gap-1 lg:flex">
            {visibleNavLinks.map(({ path, label }) => {
              const isActive = path === '/vaults'
                ? location.pathname === '/vaults' || location.pathname.startsWith('/vaults/')
                : location.pathname === path
              return (
                <Link
                  key={path}
                  to={path}
                  className={`
                    max-w-36 truncate whitespace-nowrap border px-4 py-2 text-sm font-semibold transition-colors hover:underline hover:underline-offset-4 focus-visible:underline focus-visible:underline-offset-4
                    ${
                      isActive
                        ? 'border-[#FF572D] bg-[#FF572D] text-[#FFF5F9]'
                        : 'border-transparent text-content-secondary hover:border-[#FF572D]/50 hover:bg-[#FF572D]/15 hover:text-[#FFF5F9]'
                    }
                  `}
                >
                  {label}
                </Link>
              )
            })}
            {!shouldHidePerps ? (
              <a
                href={leaderboardUrl}
                target="_blank"
                rel="noopener noreferrer"
                className="max-w-36 truncate whitespace-nowrap border border-transparent px-4 py-2 text-sm font-semibold text-content-secondary transition-colors hover:border-[#FF572D]/50 hover:bg-[#FF572D]/15 hover:text-[#FFF5F9] hover:underline hover:underline-offset-4 focus-visible:underline focus-visible:underline-offset-4"
              >
                Leaderboard
              </a>
            ) : null}
          </nav>
        </div>

        <div className="flex min-w-0 items-center justify-end gap-1.5 text-sm sm:gap-3 lg:gap-4">
          <PendingTxBadge />
          <ConnectButton />
        </div>
      </div>
    </header>
  )
}
