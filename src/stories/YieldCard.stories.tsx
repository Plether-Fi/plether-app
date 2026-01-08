import type { Meta, StoryObj } from '@storybook/react-vite'
import { YieldCard } from '../components/YieldCard'

interface YieldCardArgs {
  suppliedAmount: number
  borrowedAmount: number
  availableToBorrow: number
  supplyApy: number
  borrowApy: number
  usdcBalance: number
  suppliedBalance: number
}

const meta: Meta<YieldCardArgs> = {
  title: 'Components/YieldCard',
  tags: ['autodocs'],
  argTypes: {
    suppliedAmount: {
      control: { type: 'number', min: 0 },
      description: 'Total supplied amount in USD',
    },
    borrowedAmount: {
      control: { type: 'number', min: 0 },
      description: 'Total borrowed amount in USD',
    },
    availableToBorrow: {
      control: { type: 'number', min: 0 },
      description: 'Available to borrow in USD',
    },
    supplyApy: {
      control: { type: 'number', min: 0, max: 100, step: 0.1 },
      description: 'Supply APY percentage',
    },
    borrowApy: {
      control: { type: 'number', min: 0, max: 100, step: 0.1 },
      description: 'Borrow APY percentage',
    },
    usdcBalance: {
      control: { type: 'number', min: 0 },
      description: 'User USDC wallet balance',
    },
    suppliedBalance: {
      control: { type: 'number', min: 0 },
      description: 'User supplied balance (for withdraw)',
    },
  },
}

export default meta
type Story = StoryObj<YieldCardArgs>

function toUsdBigint(value: number): bigint {
  return BigInt(Math.floor(value * 1e6))
}

export const Default: Story = {
  args: {
    suppliedAmount: 5000,
    borrowedAmount: 1000,
    availableToBorrow: 3000,
    supplyApy: 3.5,
    borrowApy: 5.2,
    usdcBalance: 10000,
    suppliedBalance: 5000,
  },
  render: (args) => (
    <YieldCard
      suppliedAmount={toUsdBigint(args.suppliedAmount)}
      borrowedAmount={toUsdBigint(args.borrowedAmount)}
      availableToBorrow={toUsdBigint(args.availableToBorrow)}
      supplyApy={args.supplyApy}
      borrowApy={args.borrowApy}
      usdcBalance={toUsdBigint(args.usdcBalance)}
      suppliedBalance={toUsdBigint(args.suppliedBalance)}
    />
  ),
}

export const NewUser: Story = {
  args: {
    suppliedAmount: 0,
    borrowedAmount: 0,
    availableToBorrow: 0,
    supplyApy: 3.5,
    borrowApy: 5.2,
    usdcBalance: 10000,
    suppliedBalance: 0,
  },
  render: (args) => (
    <YieldCard
      suppliedAmount={toUsdBigint(args.suppliedAmount)}
      borrowedAmount={toUsdBigint(args.borrowedAmount)}
      availableToBorrow={toUsdBigint(args.availableToBorrow)}
      supplyApy={args.supplyApy}
      borrowApy={args.borrowApy}
      usdcBalance={toUsdBigint(args.usdcBalance)}
      suppliedBalance={toUsdBigint(args.suppliedBalance)}
    />
  ),
}

export const HighUtilization: Story = {
  args: {
    suppliedAmount: 10000,
    borrowedAmount: 8000,
    availableToBorrow: 500,
    supplyApy: 8.5,
    borrowApy: 12.2,
    usdcBalance: 2000,
    suppliedBalance: 10000,
  },
  render: (args) => (
    <YieldCard
      suppliedAmount={toUsdBigint(args.suppliedAmount)}
      borrowedAmount={toUsdBigint(args.borrowedAmount)}
      availableToBorrow={toUsdBigint(args.availableToBorrow)}
      supplyApy={args.supplyApy}
      borrowApy={args.borrowApy}
      usdcBalance={toUsdBigint(args.usdcBalance)}
      suppliedBalance={toUsdBigint(args.suppliedBalance)}
    />
  ),
}
