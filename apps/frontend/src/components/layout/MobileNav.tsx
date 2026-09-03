import { Link, useLocation } from 'react-router-dom'
import { isPrimaryAppDeployment } from '../../utils/deployment'

const navLinks = [
  { path: '/', label: 'Perps', icon: 'trending_up', color: 'positive' },
  { path: '/vaults', label: 'Vaults', icon: 'account_balance', color: 'brand-peach' },
]

const leaderboardUrl = 'https://insights.plether.com'

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
      <div className="flex min-h-12 items-stretch justify-around">
        {navLinks.map(({ path, label, icon, color }) => {
          const isActive = location.pathname === path ||
            (path === '/vaults' && location.pathname.startsWith('/vaults/'))
          const styles = colorStyles[color]
          return (
            <Link
              key={path}
              to={path}
              className={`
                flex min-h-12 flex-1 flex-col items-center justify-center gap-0.5 px-3 py-1 transition-colors
                ${isActive
                  ? styles.active
                  : `text-content-secondary ${styles.hover}`
                }
              `}
            >
              <span className="material-symbols-outlined text-lg">{icon}</span>
              <span className="text-[11px] font-medium">{label}</span>
            </Link>
          )
        })}
        <a
          href={leaderboardUrl}
          target="_blank"
          rel="noopener noreferrer"
          className="flex min-h-12 flex-1 flex-col items-center justify-center gap-0.5 px-3 py-1 text-content-secondary transition-colors hover:text-brand-yellow"
        >
          <span className="material-symbols-outlined text-lg">leaderboard</span>
          <span className="text-[11px] font-medium">Leaderboard</span>
        </a>
      </div>
    </nav>
  )
}
