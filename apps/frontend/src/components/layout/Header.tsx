import { Link, useLocation } from 'react-router-dom'
import { ConnectButton } from '../wallet/ConnectButton'
import { PendingTxBadge } from '../PendingTxBadge'
import { PriceDisplay } from '../PriceDisplay'
import { isPrimaryAppDeployment } from '../../utils/deployment'

const navLinks = [
  { path: '/', label: 'Perps' },
  { path: '/spot', label: 'Spot' },
  { path: '/stake', label: 'Stake' },
  { path: '/mint', label: 'Mint & Redeem' },
]

export function Header() {
  const location = useLocation()
  const shouldHidePerps = isPrimaryAppDeployment()
  const visibleNavLinks = shouldHidePerps ? navLinks.filter(({ path }) => path !== '/') : navLinks
  const homePath = shouldHidePerps ? '/spot' : '/'

  return (
    <header className="border-b border-brand-border/30 bg-surface-panel py-4">
      <div className="flex w-full min-w-0 items-center justify-between px-6 lg:px-8">
        <div className="flex min-w-0 items-center gap-10">
          <Link to={homePath} className="flex shrink-0 items-center gap-2.5 px-1 py-0.5 transition-opacity hover:opacity-90">
            <img src="/logomark.svg" alt="Plether" className="h-8 w-8" />
            <img src="/logotype.svg" alt="" aria-hidden="true" className="h-7 w-auto" />
          </Link>

          <nav className="hidden min-w-0 items-center gap-1 md:flex">
            {visibleNavLinks.map(({ path, label }) => {
              const isActive = location.pathname === path ||
                (path === '/spot' && ['/spot', '/leverage', '/lending'].includes(location.pathname))
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
          </nav>
        </div>

        <div className="flex min-w-0 items-center gap-4 text-sm">
          <div className="hidden min-w-0 items-center gap-4 lg:flex">
            <PriceDisplay variant="compact" />
          </div>
          <PendingTxBadge />
          <ConnectButton />
        </div>
      </div>
    </header>
  )
}
