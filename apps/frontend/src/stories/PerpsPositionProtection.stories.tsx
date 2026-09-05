import type { Meta, StoryObj } from '@storybook/react-vite'
import { useState } from 'react'
import { PerpsPositionProtectionPanel } from '../components/PerpsPositionProtection'
import { POSITION_PROTECTION_STATUS } from '../contracts/perpsProtection'

const meta = {
  title: 'Perps/Position Protection',
  component: PerpsPositionProtectionPanel,
  tags: ['autodocs'],
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <div className="min-h-screen bg-app-bg p-4 text-text-primary md:p-8">
        <div className="mx-auto max-w-5xl"><Story /></div>
      </div>
    ),
  ],
  args: {
    id: 7n,
    status: POSITION_PROTECTION_STATUS.Latched,
    linkedOrderId: 11n,
    canRetry: true,
    walletOnNetwork: true,
    onRetry: () => {},
  },
  argTypes: {
    id: { control: false },
    linkedOrderId: { control: false },
    queuedOrderId: { control: false },
    onRetry: { control: false },
  },
} satisfies Meta<typeof PerpsPositionProtectionPanel>

export default meta
type Story = StoryObj<typeof meta>

export const Latched: Story = {
  name: 'Latched · Waiting for retry',
  render: function InteractiveRetry(args) {
    const [queued, setQueued] = useState(false)
    return (
      <PerpsPositionProtectionPanel
        {...args}
        status={queued ? POSITION_PROTECTION_STATUS.Triggered : args.status}
        linkedOrderId={queued ? 19n : args.linkedOrderId}
        queuedOrderId={queued ? 19n : undefined}
        onRetry={() => setQueued(true)}
      />
    )
  },
}

export const Triggered: Story = {
  name: 'Triggered · Close queued',
  args: { status: POSITION_PROTECTION_STATUS.Triggered, linkedOrderId: 19n },
}

export const WalletDisconnected: Story = {
  args: { canRetry: false, walletOnNetwork: false },
}

export const RetrySubmitting: Story = {
  args: { pending: true },
}

export const RetryRace: Story = {
  args: { error: 'Protection is no longer waiting for a retry. Another keeper may have queued its close attempt. Refresh to see its current state.' },
}
