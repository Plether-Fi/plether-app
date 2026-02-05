import { useState } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { Modal } from '../components/ui'

const meta: Meta = {
  title: 'Wallet/NetworkSwitcher',
  tags: ['autodocs'],
}

export default meta
type Story = StoryObj

const networks = [
  { id: 1, name: 'Ethereum Mainnet', icon: 'diamond' },
  { id: 11155111, name: 'Sepolia Testnet', icon: 'science' },
]

function MockNetworkSwitcher({ initialChainId = 1 }: { initialChainId?: number }) {
  const [isOpen, setIsOpen] = useState(true)
  const [chainId, setChainId] = useState(initialChainId)

  return (
    <>
      <button
        onClick={() => setIsOpen(true)}
        className="px-4 py-2 bg-cyber-surface-light text-cyber-text-primary border border-cyber-border-glow/30"
      >
        Open Network Switcher
      </button>

      <Modal isOpen={isOpen} onClose={() => setIsOpen(false)} title="Select Network" size="sm">
        <div className="space-y-2">
          {networks.map(({ id, name, icon }) => {
            const isActive = chainId === id
            return (
              <button
                key={id}
                onClick={() => {
                  setChainId(id)
                  setIsOpen(false)
                }}
                className={`
                  w-full flex items-center gap-3 px-4 py-3 transition-all
                  ${isActive
                    ? 'bg-cyber-neon-green/20 border border-cyber-neon-green/50 shadow-sm shadow-cyber-neon-green/20'
                    : 'bg-cyber-surface-light border border-cyber-border-glow/30 hover:border-cyber-bright-blue/50 hover:bg-cyber-surface-light/80'
                  }
                `}
              >
                <div className={`w-10 h-10 flex items-center justify-center ${isActive ? 'bg-cyber-neon-green/20' : 'bg-cyber-surface-dark'}`}>
                  <span className={`material-symbols-outlined text-xl ${isActive ? 'text-cyber-neon-green' : 'text-cyber-text-secondary'}`}>
                    {icon}
                  </span>
                </div>
                <div className="text-left flex-1">
                  <p className={`font-medium ${isActive ? 'text-cyber-neon-green' : 'text-cyber-text-primary'}`}>{name}</p>
                  <p className="text-sm text-cyber-text-secondary">Chain ID: {id}</p>
                </div>
                {isActive && (
                  <span className="material-symbols-outlined text-cyber-neon-green">check_circle</span>
                )}
              </button>
            )
          })}
        </div>
      </Modal>
    </>
  )
}

function MockWrongNetworkBanner() {
  return (
    <div className="bg-cyber-electric-fuchsia/20 border-b border-cyber-electric-fuchsia/50 px-4 py-3">
      <div className="flex items-center justify-between gap-4">
        <div className="flex items-center gap-3">
          <span className="material-symbols-outlined text-cyber-electric-fuchsia">warning</span>
          <p className="text-cyber-electric-fuchsia text-sm">
            Please connect to Ethereum Mainnet or Sepolia to use Plether.
          </p>
        </div>
        <button className="flex items-center gap-2 px-4 py-2 bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/80 text-cyber-text-primary text-sm font-medium transition-colors shadow-lg shadow-cyber-electric-fuchsia/20">
          <span className="material-symbols-outlined text-lg">swap_horiz</span>
          Switch to Mainnet
        </button>
      </div>
    </div>
  )
}

export const MainnetSelected: Story = {
  render: () => <MockNetworkSwitcher initialChainId={1} />,
}

export const SepoliaSelected: Story = {
  render: () => <MockNetworkSwitcher initialChainId={11155111} />,
}

export const WrongNetworkBanner: Story = {
  render: () => <MockWrongNetworkBanner />,
}
