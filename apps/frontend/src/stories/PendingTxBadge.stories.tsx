import type { Meta, StoryObj } from '@storybook/react-vite'

interface PendingBadgeArgs {
  count: number
}

const meta: Meta<PendingBadgeArgs> = {
  title: 'Components/PendingTxBadge',
  tags: ['autodocs'],
  argTypes: {
    count: {
      control: { type: 'number', min: 0, max: 10 },
      description: 'Number of pending transactions',
    },
  },
}

export default meta
type Story = StoryObj<PendingBadgeArgs>

interface MockPendingBadgeProps {
  count: number
}

function MockPendingBadge({ count }: MockPendingBadgeProps) {
  if (count === 0) return <p className="text-content-secondary text-sm">No pending transactions</p>

  return (
    <div className="flex items-center gap-2 px-3 py-1.5 bg-brand-peach/20 border border-brand-peach/50">
      <div className="w-4 h-4 relative">
        <div className="absolute inset-0 border-2 border-brand-peach/30 border-t-brand-peach animate-spin" />
      </div>
      <span className="text-sm text-brand-peach font-medium">
        {count} pending
      </span>
    </div>
  )
}

export const NoPending: Story = {
  args: {
    count: 0,
  },
  render: (args) => <MockPendingBadge {...args} />,
}

export const OnePending: Story = {
  args: {
    count: 1,
  },
  render: (args) => <MockPendingBadge {...args} />,
}

export const MultiplePending: Story = {
  args: {
    count: 3,
  },
  render: (args) => <MockPendingBadge {...args} />,
}

export const AllStates: Story = {
  render: () => (
    <div className="space-y-4">
      <div>
        <p className="text-content-secondary text-sm mb-2">No pending:</p>
        <MockPendingBadge count={0} />
      </div>
      <div>
        <p className="text-content-secondary text-sm mb-2">1 pending:</p>
        <MockPendingBadge count={1} />
      </div>
      <div>
        <p className="text-content-secondary text-sm mb-2">5 pending:</p>
        <MockPendingBadge count={5} />
      </div>
    </div>
  ),
}
