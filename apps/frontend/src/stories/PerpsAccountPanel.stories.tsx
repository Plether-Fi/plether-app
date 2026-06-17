import type { Meta, StoryObj } from '@storybook/react-vite'
import { PerpsAccountPanel } from '../components/PerpsAccountPanel'

const meta: Meta<typeof PerpsAccountPanel> = {
  title: 'Perps/Account Panel',
  component: PerpsAccountPanel,
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta
type Story = StoryObj<typeof meta>

export const Default: Story = {
  render: () => (
    <div className="min-h-screen bg-cyber-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl">
        <PerpsAccountPanel />
      </div>
    </div>
  ),
}

export const ConnectedPosition: Story = {
  render: () => (
    <div className="min-h-screen bg-cyber-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl">
        <PerpsAccountPanel
          isConnected
          position={{
            exists: true,
            side: 0,
            direction: 'long',
            size: 0n,
            entryPrice: 98300000n,
            marginUsdc: 400000000n,
            unrealizedPnlUsdc: -250000n,
            maintenanceMarginUsdc: 0n,
            liquidatable: false,
            estimatedNotionalUsdc: 1999920000n,
            entryNotionalUsdc: 2000000000n,
            dxyExposureUsdc: 2069380000n,
            displayDxyPrice: 101700000n,
            liquidationPrice: 110000000n,
            pendingCarryUsdc: 1250000n,
          }}
        />
      </div>
    </div>
  ),
}
