import type { Meta, StoryObj } from '@storybook/react-vite'
import { MemoryRouter, Link, useLocation } from 'react-router-dom'

const meta: Meta = {
  title: 'Layout/MobileNav',
  tags: ['autodocs'],
}

export default meta
type Story = StoryObj

const navLinks = [
  { path: '/', label: 'Trade', icon: 'swap_horiz', color: 'brand-peach' },
  { path: '/stake', label: 'Stake', icon: 'paid', color: 'brand-orange' },
  { path: '/mint', label: 'Mint', icon: 'add', color: 'positive' },
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

function MockMobileNav() {
  const location = useLocation()

  return (
    <nav className="bg-surface-panel border-t border-brand-border/30">
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
