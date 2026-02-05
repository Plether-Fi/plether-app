import { useState } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { TokenInput } from '../components/TokenInput'

const meta: Meta<typeof TokenInput> = {
  title: 'Components/TokenInput',
  component: TokenInput,
  tags: ['autodocs'],
  argTypes: {
    label: {
      control: 'text',
      description: 'Label displayed above the input',
    },
    error: {
      control: 'text',
      description: 'Error message to display',
    },
    disabled: {
      control: 'boolean',
      description: 'Whether the input is disabled',
    },
    token: {
      control: 'object',
      description: 'Token info with symbol and decimals',
    },
    balance: {
      control: 'text',
      description: 'User balance (as bigint string)',
    },
    balanceLabel: {
      control: 'text',
      description: 'Label for balance display (default: "Balance:")',
    },
  },
}

export default meta
type Story = StoryObj<typeof meta>

function TokenInputWithState(props: Partial<Parameters<typeof TokenInput>[0]>) {
  const [value, setValue] = useState('')
  return (
    <TokenInput
      value={value}
      onChange={setValue}
      token={{ symbol: 'USDC', decimals: 6 }}
      {...props}
    />
  )
}

export const Default: Story = {
  args: {
    token: { symbol: 'USDC', decimals: 6 },
  },
  render: (args) => <TokenInputWithState {...args} />,
}

export const WithLabel: Story = {
  args: {
    label: 'Amount to deposit',
    token: { symbol: 'USDC', decimals: 6 },
  },
  render: (args) => <TokenInputWithState {...args} />,
}

export const WithBalance: Story = {
  args: {
    label: 'Amount',
    token: { symbol: 'USDC', decimals: 6 },
    balance: 1000000000n,
  },
  render: (args) => <TokenInputWithState {...args} />,
}

export const WithError: Story = {
  args: {
    label: 'Amount',
    token: { symbol: 'USDC', decimals: 6 },
    balance: 1000000000n,
    error: 'Insufficient balance',
  },
  render: (args) => <TokenInputWithState {...args} />,
}

export const Disabled: Story = {
  args: {
    label: 'Amount',
    token: { symbol: 'USDC', decimals: 6 },
    balance: 1000000000n,
    disabled: true,
  },
  render: (args) => <TokenInputWithState {...args} />,
}

export const CustomBalanceLabel: Story = {
  args: {
    label: 'Amount to redeem',
    token: { symbol: 'PAIR', decimals: 18 },
    balance: 500000000000000000000n,
    balanceLabel: 'Max:',
  },
  render: (args) => <TokenInputWithState {...args} />,
}

export const DifferentTokens: Story = {
  render: () => (
    <div className="space-y-4">
      <TokenInputWithState
        label="USDC Amount"
        token={{ symbol: 'USDC', decimals: 6 }}
        balance={1000000000n}
      />
      <TokenInputWithState
        label="plDXY-BEAR Amount"
        token={{ symbol: 'plDXY-BEAR', decimals: 18 }}
        balance={500000000000000000000n}
      />
      <TokenInputWithState
        label="plDXY-BULL Amount"
        token={{ symbol: 'plDXY-BULL', decimals: 18 }}
        balance={750000000000000000000n}
      />
    </div>
  ),
}
