import type { Meta, StoryObj } from '@storybook/react-vite'
import { Input } from '../components/ui/Input'
import { Button } from '../components/ui/Button'

const meta: Meta<typeof Input> = {
  title: 'UI/Input',
  component: Input,
  tags: ['autodocs'],
  argTypes: {
    label: { control: 'text' },
    error: { control: 'text' },
    placeholder: { control: 'text' },
    disabled: { control: 'boolean' },
  },
}

export default meta
type Story = StoryObj<typeof meta>

export const Default: Story = {
  args: {
    placeholder: 'Enter value...',
  },
}

export const WithLabel: Story = {
  args: {
    label: 'Amount',
    placeholder: '0.00',
  },
}

export const WithError: Story = {
  args: {
    label: 'Amount',
    placeholder: '0.00',
    error: 'Insufficient balance',
  },
}

export const Disabled: Story = {
  args: {
    label: 'Amount',
    placeholder: '0.00',
    disabled: true,
  },
}

export const WithRightElement: Story = {
  args: {
    label: 'Amount',
    placeholder: '0.00',
    rightElement: (
      <Button size="sm" variant="ghost">
        MAX
      </Button>
    ),
  },
}

export const WithValue: Story = {
  args: {
    label: 'Amount',
    defaultValue: '1,000.00',
    rightElement: <span className="text-cyber-text-secondary">USDC</span>,
  },
}

export const NumberInput: Story = {
  args: {
    label: 'Leverage',
    type: 'number',
    placeholder: '1.0',
    min: 1,
    max: 10,
    step: 0.1,
  },
}
