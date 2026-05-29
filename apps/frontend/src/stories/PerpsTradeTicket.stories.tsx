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
        <PerpsTradeTicket {...args} />
      </div>
    </div>
  )
}

export const Compose: Story = {
  args: {
    initialStatus: 'compose',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const Queued: Story = {
  args: {
    initialStatus: 'queued',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const Executed: Story = {
  args: {
    initialStatus: 'executed',
  },
  render: (args) => <TicketFrame {...args} />,
}
