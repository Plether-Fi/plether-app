import type { Meta, StoryObj } from '@storybook/react-vite'
import { Tooltip, InfoTooltip } from '../components/ui/Tooltip'
import { Button } from '../components/ui/Button'

const meta: Meta<typeof Tooltip> = {
  title: 'UI/Tooltip',
  component: Tooltip,
  tags: ['autodocs'],
  argTypes: {
    position: {
      control: 'select',
      options: ['top', 'bottom', 'left', 'right'],
    },
  },
  decorators: [
    (Story) => (
      <div className="flex items-center justify-center p-20">
        <Story />
      </div>
    ),
  ],
}

export default meta
type Story = StoryObj<typeof meta>

export const Top: Story = {
  args: {
    content: 'Tooltip on top',
    position: 'top',
    children: <Button>Hover me</Button>,
  },
}

export const Bottom: Story = {
  args: {
    content: 'Tooltip on bottom',
    position: 'bottom',
    children: <Button>Hover me</Button>,
  },
}

export const Left: Story = {
  args: {
    content: 'Tooltip on left',
    position: 'left',
    children: <Button>Hover me</Button>,
  },
}

export const Right: Story = {
  args: {
    content: 'Tooltip on right',
    position: 'right',
    children: <Button>Hover me</Button>,
  },
}

export const WithRichContent: Story = {
  args: {
    content: (
      <div>
        <strong className="text-cyber-neon-green">Pro tip:</strong>
        <br />
        Use leverage responsibly
      </div>
    ),
    position: 'top',
    children: <Button variant="secondary">Hover for tip</Button>,
  },
}

export const InfoTooltipExample: Story = {
  render: () => (
    <div className="flex items-center gap-2">
      <span className="text-cyber-text-primary">Health Factor</span>
      <InfoTooltip content="Ratio of collateral to debt. Below 1.0 risks liquidation." />
    </div>
  ),
}

export const AllPositions: Story = {
  render: () => (
    <div className="grid grid-cols-2 gap-8">
      <Tooltip content="Top tooltip" position="top">
        <Button variant="secondary">Top</Button>
      </Tooltip>
      <Tooltip content="Bottom tooltip" position="bottom">
        <Button variant="secondary">Bottom</Button>
      </Tooltip>
      <Tooltip content="Left tooltip" position="left">
        <Button variant="secondary">Left</Button>
      </Tooltip>
      <Tooltip content="Right tooltip" position="right">
        <Button variant="secondary">Right</Button>
      </Tooltip>
    </div>
  ),
}
