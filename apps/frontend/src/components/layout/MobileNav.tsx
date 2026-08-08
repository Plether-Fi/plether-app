import { Link, useLocation } from 'react-router-dom'
import { isPrimaryAppDeployment } from '../../utils/deployment'

const navLinks = [
  { path: '/', label: 'Perps', icon: 'trending_up', color: 'positive' },
  { path: '/vaults', label: 'Vaults', icon: 'account_balance', color: 'brand-peach' },
]

const colorStyles: Record<string, { active: string; hover: string }> = {
  'brand-peach': {
    active: 'text-brand-peach bg-brand-peach/10',
    hover: 'hover:text-[#FFAB96]',
  },
  'positive': {
    active: 'text-positive bg-positive/10',
    hover: 'hover:text-positive',
  },
}

export function MobileNav() {
  const location = useLocation()

  if (isPrimaryAppDeployment()) return null

  return (
    <nav className="safe-area-bottom fixed inset-x-0 bottom-0 z-40 border-t border-brand-border/30 bg-surface-panel lg:hidden">
      <div className="flex min-h-16 items-stretch justify-around">
        {navLinks.map(({ path, label, icon, color }) => {
          const isActive = location.pathname === path ||
            (path === '/vaults' && location.pathname.startsWith('/vaults/'))
          const styles = colorStyles[color]
          return (
            <Link
              key={path}
              to={path}
              className={`
                flex min-h-16 flex-1 flex-col items-center justify-center gap-1 px-4 py-2 transition-colors
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
