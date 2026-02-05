import type { Meta, StoryObj } from '@storybook/react-vite'
import { LoadingScreen, type LoadingStep } from '../components/ui/LoadingScreen'

const meta: Meta<typeof LoadingScreen> = {
  title: 'UI/LoadingScreen',
  component: LoadingScreen,
  tags: ['autodocs'],
  args: {
    onClose: () => alert('Close clicked'),
    onRetry: () => alert('Retry clicked'),
    transactionUrl: 'https://etherscan.io/tx/0x1234567890abcdef',
  },
  decorators: [
    (Story) => (
      <div className="bg-cyber-surface-dark max-w-md">
        <Story />
      </div>
    ),
  ],
}

export default meta
type Story = StoryObj<typeof meta>

const defaultSteps: LoadingStep[] = [
  { label: 'Transaction submitted', status: 'completed' },
  { label: 'Awaiting network confirmation (3/5)', status: 'in_progress' },
  { label: 'Finalizing asset transfer', status: 'pending' },
  { label: 'Updating portfolio balances', status: 'pending' },
]

export const Default: Story = {
  args: {
    steps: defaultSteps,
  },
}

export const JustStarted: Story = {
  args: {
    steps: [
      { label: 'Transaction submitted', status: 'in_progress' },
      { label: 'Awaiting network confirmation', status: 'pending' },
      { label: 'Finalizing asset transfer', status: 'pending' },
      { label: 'Updating portfolio balances', status: 'pending' },
    ],
  },
}

export const AlmostDone: Story = {
  args: {
    steps: [
      { label: 'Transaction submitted', status: 'completed' },
      { label: 'Awaiting network confirmation (5/5)', status: 'completed' },
      { label: 'Finalizing asset transfer', status: 'completed' },
      { label: 'Updating portfolio balances', status: 'in_progress' },
    ],
  },
}

export const AllCompleted: Story = {
  args: {
    steps: [
      { label: 'Transaction submitted', status: 'completed' },
      { label: 'Network confirmed', status: 'completed' },
      { label: 'Asset transfer complete', status: 'completed' },
      { label: 'Portfolio updated', status: 'completed' },
    ],
    title: 'Transaction complete!',
  },
}

export const CustomTitle: Story = {
  args: {
    title: 'Approving USDC spend...',
    steps: [
      { label: 'Waiting for wallet signature', status: 'in_progress' },
      { label: 'Broadcasting to network', status: 'pending' },
    ],
  },
}

export const Error: Story = {
  args: {
    title: 'Transaction failed',
    steps: [
      { label: 'Transaction submitted', status: 'completed' },
      { label: 'Awaiting network confirmation', status: 'error' },
      { label: 'Finalizing asset transfer', status: 'pending' },
      { label: 'Updating portfolio balances', status: 'pending' },
    ],
    errorMessage:
      'Transaction reverted: insufficient liquidity in the pool. Please try again with a smaller amount or increase slippage tolerance.',
  },
}

export const ErrorAtStart: Story = {
  args: {
    title: 'Transaction rejected',
    steps: [
      { label: 'Waiting for wallet signature', status: 'error' },
      { label: 'Broadcasting to network', status: 'pending' },
      { label: 'Confirming transaction', status: 'pending' },
    ],
    errorMessage: 'User rejected the transaction in their wallet.',
  },
}
