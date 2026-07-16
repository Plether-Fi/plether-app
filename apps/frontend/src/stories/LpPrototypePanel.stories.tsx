import type { Meta, StoryObj } from '@storybook/react-vite'
import { LpPrototypePanel } from '../components/documentation/LpPrototypePanel'

const meta: Meta<typeof LpPrototypePanel> = {
  title: 'Documentation/LP Interface Prototype',
  component: LpPrototypePanel,
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta
type Story = StoryObj<typeof meta>

function Frame(args: React.ComponentProps<typeof LpPrototypePanel>) {
  return (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl">
        <LpPrototypePanel {...args} />
      </div>
    </div>
  )
}

export const Overview: Story = {
  args: { view: 'overview' },
  render: (args) => <Frame {...args} />,
}

export const DepositPreview: Story = {
  args: { view: 'deposit' },
  render: (args) => <Frame {...args} />,
}

export const PendingDeposit: Story = {
  args: { view: 'pending' },
  render: (args) => <Frame {...args} />,
}

export const Position: Story = {
  args: { view: 'position' },
  render: (args) => <Frame {...args} />,
}

export const WithdrawalPreview: Story = {
  args: { view: 'withdraw' },
  render: (args) => <Frame {...args} />,
}
