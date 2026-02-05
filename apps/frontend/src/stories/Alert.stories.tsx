import type { Meta, StoryObj } from '@storybook/react-vite'
import { Alert } from '../components/ui'

const meta: Meta<typeof Alert> = {
  title: 'Components/Alert',
  component: Alert,
  tags: ['autodocs'],
  argTypes: {
    variant: {
      control: 'select',
      options: ['info', 'warning', 'success', 'error'],
      description: 'Alert style variant',
    },
    title: {
      control: 'text',
      description: 'Optional title text',
    },
    children: {
      control: 'text',
      description: 'Alert content',
    },
    icon: {
      control: 'text',
      description: 'Custom Material Symbols icon name (overrides default)',
    },
  },
}

export default meta
type Story = StoryObj<typeof Alert>

export const Info: Story = {
  args: {
    variant: 'info',
    children: 'This is an informational message.',
  },
}

export const Warning: Story = {
  args: {
    variant: 'warning',
    children: 'This is a warning message.',
  },
}

export const Success: Story = {
  args: {
    variant: 'success',
    children: 'Operation completed successfully.',
  },
}

export const Error: Story = {
  args: {
    variant: 'error',
    children: 'An error occurred. Please try again.',
  },
}

export const WithTitle: Story = {
  args: {
    variant: 'warning',
    title: 'Low Health Factor Warning',
    children: 'One or more positions have low health factors and may be at risk of liquidation.',
  },
}

export const CustomIcon: Story = {
  args: {
    variant: 'info',
    icon: 'lightbulb',
    children: 'Pro tip: You can customize the icon for any alert variant.',
  },
}

export const AllVariants: Story = {
  render: () => (
    <div className="space-y-4">
      <Alert variant="info">
        Mint equal amounts of plDXY-BEAR and plDXY-BULL from USDC.
      </Alert>
      <Alert variant="warning" icon="info">
        Redeem equal amounts of plDXY-BEAR and plDXY-BULL to get back USDC.
      </Alert>
      <Alert variant="success">
        Transaction confirmed successfully.
      </Alert>
      <Alert variant="error">
        Transaction failed. Please check your balance and try again.
      </Alert>
      <Alert variant="warning" title="Low Health Factor Warning">
        One or more positions have low health factors and may be at risk of liquidation.
      </Alert>
    </div>
  ),
}
