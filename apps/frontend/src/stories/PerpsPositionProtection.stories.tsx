import type { Meta, StoryObj } from '@storybook/react-vite'
import { useState } from 'react'
import { PerpsPositionProtectionPanel } from '../components/PerpsPositionProtection'
import { PerpsAccountPanel } from '../components/PerpsAccountPanel'
import { POSITION_PROTECTION_STATUS } from '../contracts/perpsProtection'
import { PerpsIdentityContext, type PerpsIdentityContextValue } from '../perps-aa'
import type { PerpsPosition } from '../hooks'

const protectedPosition: PerpsPosition = {
  exists: true, side: 0, direction: 'long', size: 2_000n * 10n ** 18n,
  entryPrice: 98_300_000n, marginUsdc: 400_000_000n,
  unrealizedPnlUsdc: 48_250_000n, maintenanceMarginUsdc: 20_000_000n,
  liquidatable: false, estimatedNotionalUsdc: 1_999_920_000n,
  entryNotionalUsdc: 2_000_000_000n, dxyExposureUsdc: 2_069_380_000n,
  displayDxyPrice: 101_700_000n, liquidationPrice: 110_000_000n,
  pendingCarryUsdc: 1_250_000n,
}

const storyIdentity: PerpsIdentityContextValue = {
  status: 'ready',
  ownerAddress: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
  accountAddress: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
  chainId: 421614,
  isAaManifestConfigured: false,
  sponsorshipEnabled: false,
  manifest: null,
  identity: null,
  proposedIdentity: null,
  changedIdentityFields: [],
  error: null,
  confirmIdentityAfterContinuityCheck: () => false,
  reloadIdentity: () => undefined,
}

const meta = {
  title: 'Perps/Position Protection',
  component: PerpsPositionProtectionPanel,
  tags: ['autodocs'],
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <PerpsIdentityContext.Provider value={storyIdentity}>
        <div className="min-h-screen bg-app-bg p-4 text-text-primary md:p-8">
          <div className="mx-auto max-w-5xl">
            <PerpsAccountPanel isConnected position={protectedPosition} positionProtection={<Story />} />
          </div>
        </div>
      </PerpsIdentityContext.Provider>
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
