import type { Meta, StoryObj } from '@storybook/react-vite'
import { PerpsMarketStatePanel } from '../components/PerpsMarketStatePanel'

const meta: Meta<typeof PerpsMarketStatePanel> = {
  title: 'Perps/Market State Panel',
  component: PerpsMarketStatePanel,
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta
type Story = StoryObj<typeof meta>

function PanelFrame(args: React.ComponentProps<typeof PerpsMarketStatePanel>) {
  return (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="ml-auto max-w-md">
        <PerpsMarketStatePanel {...args} />
      </div>
    </div>
  )
}

export const OpenThenCloseOnly: Story = {
  args: {
    currentPhase: 'open',
    currentDuration: '1d 12h 35m',
    nextPhase: 'close-only',
    nextDuration: '3h',
  },
  render: (args) => <PanelFrame {...args} />,
}

export const ClosedThenOpen: Story = {
  args: {
    currentPhase: 'closed',
    currentDuration: '8h 15m',
    nextPhase: 'open',
    nextDuration: '1d',
  },
  render: (args) => <PanelFrame {...args} />,
}

export const CloseOnlyThenOpen: Story = {
  args: {
    currentPhase: 'close-only',
    currentDuration: '2h 42m',
    nextPhase: 'open',
    nextDuration: '1d 12h',
  },
  render: (args) => <PanelFrame {...args} />,
}

export const Degraded: Story = {
  args: {
    currentPhase: 'degraded',
    currentDuration: 'until recapitalized',
    nextPhase: 'open',
    nextDuration: 'after recovery',
  },
  render: (args) => <PanelFrame {...args} />,
}
