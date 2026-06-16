import { Link, useLocation } from 'react-router-dom'
import { ConnectButton } from '../wallet/ConnectButton'
import { PendingTxBadge } from '../PendingTxBadge'
import { PriceDisplay } from '../PriceDisplay'

const navLinks = [
  { path: '/', label: 'Perps' },
  { path: '/spot', label: 'Spot' },
  { path: '/stake', label: 'Stake' },
  { path: '/mint', label: 'Mint & Redeem' },
]

export function Header() {
  const location = useLocation()

  return (
    <header className="border-b border-cyber-border-glow/30 bg-cyber-surface-dark py-4">
      <div className="w-full px-6 lg:px-8 flex items-center justify-between">
        <div className="flex items-center gap-10">
          <Link to="/" className="flex items-center gap-2.5 px-1 py-0.5 transition-opacity hover:opacity-90">
            <img src="/logomark.svg" alt="Plether" className="h-8 w-8" />
            <img src="/logotype.svg" alt="" aria-hidden="true" className="h-7 w-auto" />
          </Link>

          <nav className="hidden md:flex items-center gap-1">
            {navLinks.map(({ path, label }) => {
              const isActive = location.pathname === path ||
                (path === '/spot' && ['/spot', '/leverage', '/lending'].includes(location.pathname))
              return (
                <Link
                  key={path}
                  to={path}
                  className={`
                    border px-4 py-2 text-sm font-semibold transition-colors hover:underline hover:underline-offset-4 focus-visible:underline focus-visible:underline-offset-4
                    ${
                      isActive
                        ? 'border-[#FF572D] bg-[#FF572D] text-[#FFF5F9]'
                        : 'border-transparent text-cyber-text-secondary hover:border-[#FF572D]/50 hover:bg-[#FF572D]/15 hover:text-[#FFF5F9]'
                    }
                  `}
                >
                  {label}
                </Link>
              )
            })}
          </nav>
        </div>

        <div className="flex items-center gap-4 text-sm">
          <div className="hidden lg:flex items-center gap-4">
            <PriceDisplay variant="compact" />
          </div>
          <PendingTxBadge />
          <ConnectButton />
        </div>
      </div>
    </header>
  )
}
