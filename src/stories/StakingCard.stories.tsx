import type { Meta, StoryObj } from '@storybook/react-vite'
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
      description: 'Currently staked balance',
    },
  },
}

export default meta
type Story = StoryObj<StakingCardArgs>

function toTokenBigint(value: number): bigint {
  return BigInt(Math.floor(value * 1e18))
}

export const BearStaking: Story = {
  args: {
    side: 'BEAR',
    tokenBalance: 5000,
    stakedBalance: 2500,
  },
  render: (args) => (
    <div className="max-w-md">
      <StakingCard
        side={args.side}
        tokenBalance={toTokenBigint(args.tokenBalance)}
        stakedBalance={toTokenBigint(args.stakedBalance)}
      />
    </div>
  ),
}

export const BullStaking: Story = {
  args: {
    side: 'BULL',
    tokenBalance: 7500,
    stakedBalance: 3000,
  },
  render: (args) => (
    <div className="max-w-md">
      <StakingCard
        side={args.side}
        tokenBalance={toTokenBigint(args.tokenBalance)}
        stakedBalance={toTokenBigint(args.stakedBalance)}
      />
    </div>
  ),
}

export const BothCards: Story = {
  render: () => (
    <div className="grid grid-cols-2 gap-6 max-w-4xl">
      <StakingCard side="BEAR" tokenBalance={toTokenBigint(5000)} stakedBalance={toTokenBigint(2500)} />
      <StakingCard side="BULL" tokenBalance={toTokenBigint(7500)} stakedBalance={toTokenBigint(3000)} />
    </div>
  ),
}
