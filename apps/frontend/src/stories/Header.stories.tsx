import type { Meta, StoryObj } from '@storybook/react-vite'
import { MemoryRouter, Link, useLocation } from 'react-router-dom'

const meta: Meta = {
  title: 'Layout/Header',
  tags: ['autodocs'],
}

export default meta
type Story = StoryObj

const navLinks = [
  { path: '/', label: 'Trade' },
  { path: '/stake', label: 'Stake' },
  { path: '/mint', label: 'Mint & Redeem' },
]

function MockConnectButton() {
  return (
    <button className="flex items-center gap-2 bg-[#FF572D] text-[#FFF5F9] hover:bg-[#FF572D]/90 px-4 py-2 transition-colors border border-[#FF572D]/25 font-medium text-sm">
      <span className="material-symbols-outlined text-lg">account_balance_wallet</span>
      Connect Wallet
    </button>
  )
}

function MockConnectedButton() {
  return (
    <div className="flex items-center gap-2">
      <span className="px-2 py-0.5 text-xs font-medium border bg-cyber-surface-light text-cyber-text-secondary border-cyber-border-glow/30">
        Mainnet
      </span>
      <button className="flex items-center gap-2 bg-[#FF572D] text-[#FFF5F9] hover:bg-[#FF572D]/90 px-4 py-2 transition-colors border border-[#FF572D]/25 group">
        <div className="w-2 h-2 rounded-full bg-cyber-neon-green" />
        <span className="font-medium text-xs sm:text-sm">0x1234...5678</span>
      </button>
      <button className="p-2 text-cyber-text-secondary transition-colors hover:text-[#FFAB96]" title="Disconnect">
        <span className="material-symbols-outlined text-xl">logout</span>
      </button>
    </div>
  )
}

function MockPriceDisplay() {
  return (
    <div className="flex items-center gap-3">
      <div className="flex items-center gap-1.5">
        <span className="text-cyber-text-secondary text-xs">plDXY</span>
        <span className="text-cyber-text-primary font-medium">108.42</span>
        <span className="text-cyber-neon-green text-xs">+0.12%</span>
      </div>
    </div>
  )
}

function MockPendingTxBadge({ count = 0 }: { count?: number }) {
  if (count === 0) return null
  return (
    <button className="flex items-center gap-1.5 border border-cyber-border-glow/30 bg-cyber-surface-light px-3 py-1.5 transition-colors hover:border-[#FFAB96]/50 hover:bg-[#3B212D] hover:underline hover:underline-offset-4">
      <span className="material-symbols-outlined text-lg text-cyber-bright-blue animate-spin">progress_activity</span>
      <span className="text-sm text-cyber-text-primary">{count} pending</span>
    </button>
  )
}

function MockHeader({ connected = false, pendingTx = 0 }: { connected?: boolean; pendingTx?: number }) {
  const location = useLocation()

  return (
    <header className="border-b border-cyber-border-glow/30 bg-cyber-surface-dark py-4 sticky top-0 z-50">
      <div className="w-full px-6 lg:px-8 flex items-center justify-between">
        <div className="flex items-center gap-10">
          <Link to="/" className="flex items-center gap-2.5">
            <img src="/logomark.svg" alt="Plether" className="h-8 w-8" />
            <img src="/logotype.svg" alt="" aria-hidden="true" className="h-7 w-auto" />
          </Link>

          <nav className="hidden md:flex items-center gap-1">
            {navLinks.map(({ path, label }) => {
              const isActive = location.pathname === path ||
                (path === '/' && ['/', '/leverage', '/lending'].includes(location.pathname))
              return (
                <Link
                  key={path}
                  to={path}
                  className={`
                    px-4 py-2  text-sm font-semibold transition-colors
                    ${
                      isActive
                        ? 'bg-[#FF572D] text-[#FFF5F9] border border-[#FF572D]/20'
                        : 'text-cyber-text-secondary hover:text-[#FFAB96] hover:underline hover:underline-offset-4'
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
            <MockPriceDisplay />
          </div>
          <MockPendingTxBadge count={pendingTx} />
          {connected ? <MockConnectedButton /> : <MockConnectButton />}
        </div>
      </div>
    </header>
  )
}

export const Default: Story = {
  render: () => (
    <MemoryRouter initialEntries={['/']}>
      <MockHeader />
    </MemoryRouter>
  ),
}

export const Connected: Story = {
  render: () => (
    <MemoryRouter initialEntries={['/']}>
      <MockHeader connected />
    </MemoryRouter>
  ),
}

export const WithPendingTransactions: Story = {
  render: () => (
    <MemoryRouter initialEntries={['/']}>
      <MockHeader connected pendingTx={2} />
    </MemoryRouter>
  ),
}

export const OnStakePage: Story = {
  render: () => (
    <MemoryRouter initialEntries={['/stake']}>
      <MockHeader connected />
    </MemoryRouter>
  ),
}

export const OnMintPage: Story = {
  render: () => (
    <MemoryRouter initialEntries={['/mint']}>
      <MockHeader connected />
    </MemoryRouter>
  ),
}
