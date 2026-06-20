import type { Meta, StoryObj } from '@storybook/react-vite'

const meta: Meta = {
  title: 'Wallet/ConnectButton',
  tags: ['autodocs'],
}

export default meta
type Story = StoryObj

function MockDisconnectedButton() {
  return (
    <button className="flex items-center gap-2 bg-[#FF572D] text-[#FFF5F9] hover:bg-[#FF572D]/90 px-4 py-2 transition-colors border border-[#FF572D]/25 font-medium text-sm">
      <span className="material-symbols-outlined text-lg">account_balance_wallet</span>
      Connect Wallet
    </button>
  )
}

function MockConnectedButton({
  network = 'Mainnet',
  isWrongNetwork = false,
  needsSwitch = true,
}: {
  network?: string
  isWrongNetwork?: boolean
  needsSwitch?: boolean
}) {
  return (
    <div className="flex items-center gap-4">
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

      {needsSwitch ? (
        <button className="flex cursor-pointer items-center gap-2 border border-[#FFAB96] bg-[#FFAB96] px-3 py-2 text-xs font-semibold text-[#250917] transition-colors hover:border-[#FFAB96] hover:bg-[#250917] hover:text-[#FFAB96] hover:underline hover:underline-offset-4">
          <span className="material-symbols-outlined text-base">swap_horiz</span>
          Switch Network
        </button>
      ) : null}

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

export const Disconnected: Story = {
  render: () => <MockDisconnectedButton />,
}

export const ConnectedMainnet: Story = {
  render: () => <MockConnectedButton network="Mainnet" />,
}

export const ConnectedSepolia: Story = {
  render: () => <MockConnectedButton network="Sepolia" />,
}

export const ConnectedArbitrumSepolia: Story = {
  render: () => <MockConnectedButton network="Arbitrum Sepolia" needsSwitch={false} />,
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
        <p className="text-cyber-text-secondary text-sm mb-2">Connected (Arbitrum Sepolia):</p>
        <MockConnectedButton network="Arbitrum Sepolia" needsSwitch={false} />
      </div>
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Wrong Network:</p>
        <MockConnectedButton isWrongNetwork />
      </div>
    </div>
  ),
}
