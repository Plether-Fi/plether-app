import { Link, useLocation } from 'react-router-dom'

const navLinks = [
  { path: '/', label: 'Trade', icon: 'swap_horiz', color: 'cyber-bright-blue' },
  { path: '/stake', label: 'Stake', icon: 'paid', color: 'cyber-electric-fuchsia' },
  { path: '/mint', label: 'Mint', icon: 'add', color: 'cyber-neon-green' },
]

const colorStyles: Record<string, { active: string; hover: string }> = {
  'cyber-bright-blue': {
    active: 'text-cyber-bright-blue bg-cyber-bright-blue/10',
    hover: 'hover:text-cyber-bright-blue',
  },
  'cyber-electric-fuchsia': {
    active: 'text-cyber-electric-fuchsia bg-cyber-electric-fuchsia/10',
    hover: 'hover:text-cyber-electric-fuchsia',
  },
  'cyber-neon-green': {
    active: 'text-cyber-neon-green bg-cyber-neon-green/10',
    hover: 'hover:text-cyber-neon-green',
  },
}

export function MobileNav() {
  const location = useLocation()

  return (
    <nav className="lg:hidden fixed bottom-0 left-0 right-0 z-40 bg-cyber-surface-dark border-t border-cyber-border-glow/30 safe-area-bottom shadow-lg shadow-cyber-border-glow/10">
      <div className="flex items-center justify-around h-16">
        {navLinks.map(({ path, label, icon, color }) => {
          const isActive = location.pathname === path ||
            (path === '/' && ['/', '/leverage', '/yield'].includes(location.pathname))
          const styles = colorStyles[color]
          return (
            <Link
              key={path}
              to={path}
              className={`
                flex flex-col items-center gap-1 px-4 py-2 rounded-lg transition-colors
                ${isActive
                  ? styles.active
                  : `text-cyber-text-secondary ${styles.hover}`
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
