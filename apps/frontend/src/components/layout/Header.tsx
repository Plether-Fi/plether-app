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
    <header className="border-b border-cyber-border-glow/30 bg-cyber-surface-dark py-4 will-change-transform shadow-lg shadow-cyber-border-glow/10">
      <div className="max-w-7xl mx-auto px-6 lg:px-8 flex items-center justify-between">
        <div className="flex items-center gap-10">
          <Link to="/" className="flex items-center gap-2.5">
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
                    px-4 py-2  text-sm font-semibold transition-colors
                    ${
                      isActive
                        ? 'bg-cyber-surface-light text-cyber-neon-green border border-cyber-neon-green/50 shadow-md shadow-cyber-neon-green/10'
                        : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
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
