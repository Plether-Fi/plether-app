import type { Meta, StoryObj } from '@storybook/react-vite'
import { fn } from 'storybook/test'
import { AccountOperationNotice } from '../components/AccountOperationNotice'
import type { SponsoredOperation } from '../perps-aa/operationStore'

const now = Date.UTC(2026, 8, 17, 10)
const operation: SponsoredOperation = {
  id: 'pending-trade',
  ownerAddress: '0x1111111111111111111111111111111111111111',
  accountAddress: '0x2222222222222222222222222222222222222222',
  chainId: 421614,
  accountMode: 'simple',
  manifestVersion: 'perps-aa-arbitrum-sepolia-v2',
  action: 'place-protected-order',
  lane: 'default',
  status: 'outcome-unknown',
  sponsorshipAccepted: true,
  retryCount: 0,
  createdAt: now - 4 * 60 * 60_000,
  updatedAt: now - 60_000,
  statusTimestamps: { 'outcome-unknown': now - 60_000 },
  userOperationHash: `0x${'ab'.repeat(32)}`,
}

const meta = {
  title: 'Perps/AccountOperationNotice',
  component: AccountOperationNotice,
  args: { operation, now, onOpen: fn() },
  decorators: [(Story) => <div className="w-full max-w-[400px] bg-app-bg p-4"><Story /></div>],
  parameters: { layout: 'centered' },
} satisfies Meta<typeof AccountOperationNotice>
export default meta
type Story = StoryObj<typeof meta>

export const NeedsStatusCheck: Story = {}
export const WaitingForWallet: Story = {
  args: { operation: { ...operation, status: 'awaiting-signature', userOperationHash: undefined, createdAt: now - 20_000 } },
}
export const Confirming: Story = {
  args: { operation: { ...operation, status: 'confirming', createdAt: now - 30_000, statusTimestamps: { confirming: now - 10_000 } } },
}
export const SavedTransaction: Story = {
  args: { operation: { ...operation, action: 'cancel-protection', status: 'preparation-pending', userOperationHash: undefined, createdAt: now - 15 * 60_000 } },
}
