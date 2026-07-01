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
        className="px-4 py-2 bg-surface-muted text-content-primary border border-brand-border/30"
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
                    ? 'bg-positive/20 border border-positive/50'
                    : 'bg-surface-muted border border-brand-border/30 hover:border-[#FFAB96]/50 hover:bg-[#3B212D]'
                  }
                `}
              >
                <div className={`w-10 h-10 flex items-center justify-center ${isActive ? 'bg-positive/20' : 'bg-surface-panel'}`}>
                  <span className={`material-symbols-outlined text-xl ${isActive ? 'text-positive' : 'text-content-secondary'}`}>
                    {icon}
                  </span>
                </div>
                <div className="text-left flex-1">
                  <p className={`font-medium ${isActive ? 'text-positive' : 'text-content-primary'}`}>{name}</p>
                  <p className="text-sm text-content-secondary">Chain ID: {id}</p>
                </div>
                {isActive && (
                  <span className="material-symbols-outlined text-positive">check_circle</span>
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
    <div className="bg-brand-orange/20 border-b border-brand-orange/50 px-4 py-3">
      <div className="flex items-center justify-between gap-4">
        <div className="flex items-center gap-3">
          <span className="material-symbols-outlined text-brand-orange">warning</span>
          <p className="text-brand-orange text-sm">
            Please connect to Ethereum Mainnet or Sepolia to use Plether.
          </p>
        </div>
        <button className="flex items-center gap-2 px-4 py-2 bg-brand-orange hover:bg-brand-orange/80 text-content-primary text-sm font-medium transition-colors">
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
