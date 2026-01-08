import { Link, useLocation } from 'react-router-dom'
import { ConnectButton } from '../wallet/ConnectButton'
import { PendingTxBadge } from '../PendingTxBadge'
import { PriceDisplay } from '../PriceDisplay'

const navLinks = [
  { path: '/', label: 'Trade' },
  { path: '/stake', label: 'Stake' },
  { path: '/mint', label: 'Mint & Redeem' },
]

export function Header() {
  const location = useLocation()

  return (
    <header className="border-b border-cyber-border-glow/30 bg-cyber-surface-dark py-4 sticky top-0 z-50 shadow-lg shadow-cyber-border-glow/10">
      <div className="max-w-7xl mx-auto px-6 lg:px-8 flex items-center justify-between">
        <div className="flex items-center gap-10">
          <Link to="/" className="flex items-center gap-2">
            <img src="/icon.svg" alt="Plether" className="w-12 h-12 -my-2" />
            <span className="text-xl font-semibold tracking-wide text-cyber-neon-green drop-shadow-[0_0_0px_var(--color-cyber-neon-green)] hover:drop-shadow-[0_0_8px_var(--color-cyber-neon-green)] transition-[filter] duration-300">Plether</span>
          </Link>

          <nav className="hidden md:flex items-center gap-1">
            {navLinks.map(({ path, label }) => {
              const isActive = location.pathname === path ||
                (path === '/' && ['/', '/leverage', '/yield'].includes(location.pathname))
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
