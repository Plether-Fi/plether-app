import type { Meta, StoryObj } from '@storybook/react-vite'
import { MemoryRouter, Link, useLocation } from 'react-router-dom'

const meta: Meta = {
  title: 'Layout/MobileNav',
  tags: ['autodocs'],
}

export default meta
type Story = StoryObj

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

function MockMobileNav() {
  const location = useLocation()

  return (
    <nav className="bg-cyber-surface-dark border-t border-cyber-border-glow/30 shadow-lg shadow-cyber-border-glow/10">
      <div className="flex items-center justify-around h-16">
        {navLinks.map(({ path, label, icon, color }) => {
          const isActive = location.pathname === path ||
            (path === '/' && ['/', '/leverage', '/lending'].includes(location.pathname))
          const styles = colorStyles[color]
          return (
            <Link
              key={path}
              to={path}
              className={`
                flex flex-col items-center gap-1 px-4 py-2 transition-colors
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

export const OnTradePage: Story = {
  render: () => (
    <MemoryRouter initialEntries={['/']}>
      <MockMobileNav />
    </MemoryRouter>
  ),
}

export const OnStakePage: Story = {
  render: () => (
    <MemoryRouter initialEntries={['/stake']}>
      <MockMobileNav />
    </MemoryRouter>
  ),
}

export const OnMintPage: Story = {
  render: () => (
    <MemoryRouter initialEntries={['/mint']}>
      <MockMobileNav />
    </MemoryRouter>
  ),
}
