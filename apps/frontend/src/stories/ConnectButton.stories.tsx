import type { Meta, StoryObj } from '@storybook/react-vite'

const meta: Meta = {
  title: 'Wallet/ConnectButton',
  tags: ['autodocs'],
}

export default meta
type Story = StoryObj

function MockDisconnectedButton() {
  return (
    <button className="flex items-center gap-2 bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/80 text-cyber-text-primary px-4 py-2 transition-colors border border-transparent shadow-lg shadow-cyber-electric-fuchsia/20 font-medium text-sm">
      <span className="material-symbols-outlined text-lg">account_balance_wallet</span>
      Connect Wallet
    </button>
  )
}

function MockConnectedButton({ network = 'Mainnet', isWrongNetwork = false }: { network?: string; isWrongNetwork?: boolean }) {
  return (
    <div className="flex items-center gap-2">
      <span className={`
        px-2 py-0.5 text-xs font-medium border
        ${isWrongNetwork
          ? 'bg-cyber-electric-fuchsia/20 text-cyber-electric-fuchsia border-cyber-electric-fuchsia/30'
          : network === 'Sepolia'
            ? 'bg-cyber-warning-bg text-cyber-warning-text border-cyber-warning-text/30'
            : 'bg-cyber-surface-light text-cyber-text-secondary border-cyber-border-glow/30'
        }
      `}>
        {isWrongNetwork ? 'Wrong Network' : network}
      </span>

      <button className="flex items-center gap-2 bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/80 text-cyber-text-primary px-4 py-2 transition-colors border border-transparent shadow-lg shadow-cyber-electric-fuchsia/20 group">
        <div className="w-2 h-2 rounded-full bg-cyber-neon-green shadow-md shadow-cyber-neon-green/50" />
        <span className="font-medium text-xs sm:text-sm">0x1234...5678</span>
      </button>

      <button className="p-2 text-cyber-text-secondary hover:text-cyber-bright-blue transition-colors" title="Disconnect">
        <span className="material-symbols-outlined text-xl">logout</span>
      </button>
    </div>
  )
}

export const Disconnected: Story = {
  render: () => <MockDisconnectedButton />,
}

export const ConnectedMainnet: Story = {
  render: () => <MockConnectedButton network="Mainnet" />,
}

export const ConnectedSepolia: Story = {
  render: () => <MockConnectedButton network="Sepolia" />,
}

export const WrongNetwork: Story = {
  render: () => <MockConnectedButton isWrongNetwork />,
}

export const AllStates: Story = {
  render: () => (
    <div className="space-y-6">
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Disconnected:</p>
        <MockDisconnectedButton />
      </div>
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Connected (Mainnet):</p>
        <MockConnectedButton network="Mainnet" />
      </div>
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Connected (Sepolia):</p>
        <MockConnectedButton network="Sepolia" />
      </div>
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Wrong Network:</p>
        <MockConnectedButton isWrongNetwork />
      </div>
    </div>
  ),
}
