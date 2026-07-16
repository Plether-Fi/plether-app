import type { Meta, StoryObj } from '@storybook/react-vite'
import { MarginAccountDocumentationPanel } from '../components/documentation/MarginAccountDocumentationPanel'

const meta: Meta<typeof MarginAccountDocumentationPanel> = {
  title: 'Documentation/Margin Account',
  component: MarginAccountDocumentationPanel,
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta
type Story = StoryObj<typeof meta>

function Frame(args: React.ComponentProps<typeof MarginAccountDocumentationPanel>) {
  return (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-6xl">
        <MarginAccountDocumentationPanel {...args} />
      </div>
    </div>
  )
}

export const Overview: Story = {
  args: { view: 'overview' },
  render: (args) => <Frame {...args} />,
}

export const Deposit: Story = {
  args: { view: 'deposit' },
  render: (args) => <Frame {...args} />,
}

export const PendingReservations: Story = {
  args: { view: 'pending-reservations' },
  render: (args) => <Frame {...args} />,
}

export const AddPositionMargin: Story = {
  args: { view: 'add-position-margin' },
  render: (args) => <Frame {...args} />,
}

export const Withdrawal: Story = {
  args: { view: 'withdrawal' },
  render: (args) => <Frame {...args} />,
}

export const TraderClaim: Story = {
  args: { view: 'trader-claim' },
  render: (args) => <Frame {...args} />,
}
