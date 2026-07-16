import { Link, useLocation } from 'react-router-dom'
import { isPrimaryAppDeployment } from '../../utils/deployment'

const navLinks = [
  { path: '/', label: 'Perps', icon: 'trending_up', color: 'positive', perpsSurface: true },
  { path: '/vaults', label: 'Vaults', icon: 'account_balance', color: 'brand-peach', perpsSurface: true },
  { path: '/spot', label: 'Spot', icon: 'swap_horiz', color: 'brand-peach', perpsSurface: false },
  { path: '/stake', label: 'Stake', icon: 'paid', color: 'brand-orange', perpsSurface: false },
  { path: '/mint', label: 'Mint', icon: 'add', color: 'positive', perpsSurface: false },
]

const colorStyles: Record<string, { active: string; hover: string }> = {
  'brand-peach': {
    active: 'text-brand-peach bg-brand-peach/10',
    hover: 'hover:text-[#FFAB96]',
  },
  'brand-orange': {
    active: 'text-brand-orange bg-brand-orange/10',
    hover: 'hover:text-brand-orange',
  },
  'positive': {
    active: 'text-positive bg-positive/10',
    hover: 'hover:text-positive',
  },
}

export function MobileNav() {
  const location = useLocation()
  const visibleNavLinks = isPrimaryAppDeployment()
    ? navLinks.filter(({ perpsSurface }) => !perpsSurface)
    : navLinks

  return (
    <nav className="lg:hidden fixed bottom-0 left-0 right-0 z-40 bg-surface-panel border-t border-brand-border/30 safe-area-bottom">
      <div className="flex items-center justify-around h-16">
        {visibleNavLinks.map(({ path, label, icon, color }) => {
          const isActive = location.pathname === path ||
            (path === '/spot' && ['/spot', '/leverage', '/lending'].includes(location.pathname)) ||
            (path === '/vaults' && (location.pathname === '/vaults' || location.pathname.startsWith('/vaults/')))
          const styles = colorStyles[color]
          return (
            <Link
              key={path}
              to={path}
              className={`
                flex min-w-0 flex-1 flex-col items-center gap-1 px-2 py-2 transition-colors
                ${isActive
                  ? styles.active
                  : `text-content-secondary ${styles.hover}`
                }
              `}
            >
              <span className="material-symbols-outlined text-xl">{icon}</span>
              <span className="text-xs font-medium">{label}</span>
            </Link>
          )
        })}
      </div>
    </nav>
  )
}
