import { Link, useLocation } from 'react-router-dom'
import { isPrimaryAppDeployment } from '../../utils/deployment'

const navLinks = [
  { path: '/', label: 'Perps', icon: 'trending_up', color: 'positive' },
]

const colorStyles: Record<string, { active: string; hover: string }> = {
  'positive': {
    active: 'text-positive bg-positive/10',
    hover: 'hover:text-positive',
  },
}

export function MobileNav() {
  const location = useLocation()

  if (isPrimaryAppDeployment()) return null

  return (
    <nav className="lg:hidden fixed bottom-0 left-0 right-0 z-40 bg-surface-panel border-t border-brand-border/30 safe-area-bottom">
      <div className="flex items-center justify-around h-16">
        {navLinks.map(({ path, label, icon, color }) => {
          const isActive = location.pathname === path
          const styles = colorStyles[color]
          return (
            <Link
              key={path}
              to={path}
              className={`
                flex flex-col items-center gap-1 px-4 py-2  transition-colors
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
