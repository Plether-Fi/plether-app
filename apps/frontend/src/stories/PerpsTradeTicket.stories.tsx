import type { Meta, StoryObj } from '@storybook/react-vite'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'

const meta: Meta<typeof PerpsTradeTicket> = {
  title: 'Perps/Trade Ticket',
  component: PerpsTradeTicket,
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta
type Story = StoryObj<typeof meta>

function TicketFrame(args: React.ComponentProps<typeof PerpsTradeTicket>) {
  return (
    <div className="min-h-screen bg-cyber-bg p-4 md:p-8">
      <div className="ml-auto max-w-md">
        <PerpsTradeTicket maintenanceMarginBps={100n} {...args} />
      </div>
    </div>
  )
}

export const Compose: Story = {
  args: {
    initialLifecycleState: 'preview',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const PreviewModal: Story = {
  args: {
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const OpenLongPreview: Story = {
  args: {
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'long',
    currentPositionAmount: '0',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const IncreaseLongPreview: Story = {
  args: {
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'long',
    initialSize: '5000',
    currentPositionSide: 'long',
    currentPositionAmount: '8 200',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const ReduceLongPreview: Story = {
  args: {
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'short',
    initialSize: '5000',
    currentPositionSide: 'long',
    currentPositionAmount: '8 200',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const CloseLongPreview: Story = {
  args: {
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'short',
    initialSize: '8 200',
    currentPositionSide: 'long',
    currentPositionAmount: '8 200',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const FlipLongToShortPreview: Story = {
  args: {
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'short',
    initialSize: '10 000',
    currentPositionSide: 'long',
    currentPositionAmount: '8 200',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const FlipShortToLongPreview: Story = {
  args: {
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'long',
    initialSize: '10 000',
    currentPositionSide: 'short',
    currentPositionAmount: '8 200',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const ReduceOnlyPreventsFlipPreview: Story = {
  args: {
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'short',
    initialSize: '10 000',
    initialReduceOnly: true,
    currentPositionSide: 'long',
    currentPositionAmount: '8 200',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const CommitPending: Story = {
  args: {
    initialLifecycleState: 'commitPending',
    initialReviewOpen: true,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const RevealPending: Story = {
  name: 'Finalizing Price',
  args: {
    initialLifecycleState: 'revealPending',
    initialReviewOpen: true,
    showFinalizationProgress: true,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const SelfExecuteAvailable: Story = {
  name: 'Manual Finalization Ready',
  args: {
    initialLifecycleState: 'selfExecuteAvailable',
    initialReviewOpen: true,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const SelfExecutePending: Story = {
  name: 'Finalizing Trade',
  args: {
    initialLifecycleState: 'selfExecutePending',
    initialReviewOpen: true,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const SelfExecuteFailed: Story = {
  name: 'Finalization Failed',
  args: {
    initialLifecycleState: 'selfExecuteFailed',
    initialReviewOpen: true,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const Executed: Story = {
  args: {
    initialLifecycleState: 'executed',
    initialReviewOpen: true,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const Failed: Story = {
  args: {
    initialLifecycleState: 'failed',
    initialReviewOpen: true,
  },
  render: (args) => <TicketFrame {...args} />,
}
