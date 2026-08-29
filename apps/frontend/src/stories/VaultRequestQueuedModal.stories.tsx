import type { Meta, StoryObj } from '@storybook/react-vite'
import {
  VaultLifecycleSteps,
  VaultRequestQueuedState,
} from '../pages/Vaults'
import { Modal } from '../components/ui/Modal'

const STORY_TRANSACTION_HASH =
  '0x4cb82000000000000000000000000000000000000000000000000000000015ac'

const meta: Meta<typeof VaultRequestQueuedState> = {
  title: 'Vaults/Request Queued Modal',
  component: VaultRequestQueuedState,
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',
    docs: {
      description: {
        component:
          'The terminal success state shown after a deposit or withdrawal request is confirmed onchain.',
      },
    },
  },
  args: {
    mode: 'deposit',
    targetSettlement: 'Aug 28, 05:00 PM',
    transactionHash: STORY_TRANSACTION_HASH,
    onClose: () => undefined,
    onViewRequest: () => undefined,
  },
  render: (args) => (
    <Modal
      isOpen
      onClose={args.onClose}
      ariaLabel="Deposit flow"
      headerContent={<VaultLifecycleSteps currentStep="queued" />}
      showCloseButton={false}
      size="lg"
      inertBackground
    >
      <VaultRequestQueuedState {...args} />
    </Modal>
  ),
}

export default meta
type Story = StoryObj<typeof meta>

export const DepositQueued: Story = {}

export const WithdrawalQueued: Story = {
  args: {
    mode: 'withdraw',
  },
}
