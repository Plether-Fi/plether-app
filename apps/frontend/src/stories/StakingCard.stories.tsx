import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, userEvent, within } from 'storybook/test'
import { StakingCard } from '../components/StakingCard'

interface StakingCardArgs {
  side: 'BEAR' | 'BULL'
  tokenBalance: number
  stakedBalance: number
}

const meta: Meta<StakingCardArgs> = {
  title: 'Components/StakingCard',
  tags: ['autodocs'],
  argTypes: {
    side: {
      control: 'radio',
      options: ['BEAR', 'BULL'],
      description: 'Token side to stake',
    },
    tokenBalance: {
      control: { type: 'number', min: 0 },
      description: 'Available token balance',
    },
    stakedBalance: {
      control: { type: 'number', min: 0 },
      description: 'Currently staked shares balance',
    },
  },
}

export default meta
type Story = StoryObj<StakingCardArgs>

function toTokenBigint(value: number): bigint {
  return BigInt(Math.floor(value * 1e18))
}

function toShareBigint(value: number): bigint {
  return BigInt(Math.floor(value * 1e21))
}

export const BearStaking: Story = {
  args: {
    side: 'BEAR',
    tokenBalance: 5000,
    stakedBalance: 1000,
  },
  render: (args) => (
    <div className="max-w-md">
      <StakingCard
        side={args.side}
        tokenBalance={toTokenBigint(args.tokenBalance)}
        stakedBalance={toShareBigint(args.stakedBalance)}
      />
    </div>
  ),
}

export const BullStaking: Story = {
  args: {
    side: 'BULL',
    tokenBalance: 7500,
    stakedBalance: 2000,
  },
  render: (args) => (
    <div className="max-w-md">
      <StakingCard
        side={args.side}
        tokenBalance={toTokenBigint(args.tokenBalance)}
        stakedBalance={toShareBigint(args.stakedBalance)}
      />
    </div>
  ),
}

export const BothCards: Story = {
  render: () => (
    <div className="grid grid-cols-2 gap-6 max-w-4xl">
      <StakingCard side="BULL" tokenBalance={toTokenBigint(7500)} stakedBalance={toShareBigint(2000)} />
      <StakingCard side="BEAR" tokenBalance={toTokenBigint(5000)} stakedBalance={toShareBigint(1000)} />
    </div>
  ),
}

export const StakeBearFlow: Story = {
  args: {
    side: 'BEAR',
    tokenBalance: 5000,
    stakedBalance: 1000,
  },
  render: (args) => (
    <div className="max-w-md">
      <StakingCard
        side={args.side}
        tokenBalance={toTokenBigint(args.tokenBalance)}
        stakedBalance={toShareBigint(args.stakedBalance)}
      />
    </div>
  ),
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)

    await step('Verify Stake mode is selected by default', async () => {
      const stakeButton = canvas.getByRole('button', { name: /^stake$/i })
      expect(stakeButton).toHaveClass('bg-cyber-surface-dark')
    })

    await step('Enter stake amount', async () => {
      const input = canvas.getByPlaceholderText('0.00')
      await userEvent.clear(input)
      await userEvent.type(input, '100')
      expect(input).toHaveValue('100')
    })

    await step('Verify output display shows expected shares', async () => {
      expect(canvas.getByText(/you will receive/i)).toBeInTheDocument()
    })
  },
}

export const UnstakeBullFlow: Story = {
  args: {
    side: 'BULL',
    tokenBalance: 7500,
    stakedBalance: 2000,
  },
  render: (args) => (
    <div className="max-w-md">
      <StakingCard
        side={args.side}
        tokenBalance={toTokenBigint(args.tokenBalance)}
        stakedBalance={toShareBigint(args.stakedBalance)}
      />
    </div>
  ),
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)

    await step('Switch to Unstake mode', async () => {
      const unstakeButton = canvas.getByRole('button', { name: /^unstake$/i })
      await userEvent.click(unstakeButton)
      expect(unstakeButton).toHaveClass('bg-cyber-surface-dark')
    })

    await step('Enter unstake amount', async () => {
      const input = canvas.getByPlaceholderText('0.00')
      await userEvent.clear(input)
      await userEvent.type(input, '50')
      expect(input).toHaveValue('50')
    })
  },
}

export const ModeToggle: Story = {
  args: {
    side: 'BEAR',
    tokenBalance: 5000,
    stakedBalance: 1000,
  },
  render: (args) => (
    <div className="max-w-md">
      <StakingCard
        side={args.side}
        tokenBalance={toTokenBigint(args.tokenBalance)}
        stakedBalance={toShareBigint(args.stakedBalance)}
      />
    </div>
  ),
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)

    await step('Start in Stake mode', async () => {
      const stakeButton = canvas.getByRole('button', { name: /^stake$/i })
      expect(stakeButton).toHaveClass('bg-cyber-surface-dark')
    })

    await step('Switch to Unstake mode', async () => {
      const unstakeButton = canvas.getByRole('button', { name: /^unstake$/i })
      await userEvent.click(unstakeButton)
      expect(unstakeButton).toHaveClass('bg-cyber-surface-dark')
    })

    await step('Switch back to Stake mode', async () => {
      const stakeButton = canvas.getByRole('button', { name: /^stake$/i })
      await userEvent.click(stakeButton)
      expect(stakeButton).toHaveClass('bg-cyber-surface-dark')
    })
  },
}
