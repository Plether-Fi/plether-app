import type { Meta, StoryObj } from '@storybook/react-vite'
import { YieldCard } from '../components/YieldCard'

interface YieldCardArgs {
  suppliedAmount: number
  borrowedAmount: number
  availableToBorrow: number
  usdcBalance: number
  suppliedBalance: number
  hasCollateral: boolean
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
    usdcBalance: {
      control: { type: 'number', min: 0 },
      description: 'User USDC wallet balance',
    },
    suppliedBalance: {
      control: { type: 'number', min: 0 },
      description: 'User supplied balance (for withdraw)',
    },
    hasCollateral: {
      control: 'boolean',
      description: 'Whether user has staked collateral for borrowing',
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
    usdcBalance: 10000,
    suppliedBalance: 5000,
    hasCollateral: true,
  },
  render: (args) => (
    <YieldCard
      suppliedAmount={toUsdBigint(args.suppliedAmount)}
      borrowedAmount={toUsdBigint(args.borrowedAmount)}
      availableToBorrow={toUsdBigint(args.availableToBorrow)}
      usdcBalance={toUsdBigint(args.usdcBalance)}
      suppliedBalance={toUsdBigint(args.suppliedBalance)}
      hasCollateral={args.hasCollateral}
    />
  ),
}

export const NewUser: Story = {
  args: {
    suppliedAmount: 0,
    borrowedAmount: 0,
    availableToBorrow: 0,
    usdcBalance: 10000,
    suppliedBalance: 0,
    hasCollateral: false,
  },
  render: (args) => (
    <YieldCard
      suppliedAmount={toUsdBigint(args.suppliedAmount)}
      borrowedAmount={toUsdBigint(args.borrowedAmount)}
      availableToBorrow={toUsdBigint(args.availableToBorrow)}
      usdcBalance={toUsdBigint(args.usdcBalance)}
      suppliedBalance={toUsdBigint(args.suppliedBalance)}
      hasCollateral={args.hasCollateral}
    />
  ),
}

export const HighUtilization: Story = {
  args: {
    suppliedAmount: 10000,
    borrowedAmount: 8000,
    availableToBorrow: 500,
    usdcBalance: 2000,
    suppliedBalance: 10000,
    hasCollateral: true,
  },
  render: (args) => (
    <YieldCard
      suppliedAmount={toUsdBigint(args.suppliedAmount)}
      borrowedAmount={toUsdBigint(args.borrowedAmount)}
      availableToBorrow={toUsdBigint(args.availableToBorrow)}
      usdcBalance={toUsdBigint(args.usdcBalance)}
      suppliedBalance={toUsdBigint(args.suppliedBalance)}
      hasCollateral={args.hasCollateral}
    />
  ),
}

export const NoCollateral: Story = {
  args: {
    suppliedAmount: 5000,
    borrowedAmount: 0,
    availableToBorrow: 0,
    usdcBalance: 10000,
    suppliedBalance: 5000,
    hasCollateral: false,
  },
  render: (args) => (
    <YieldCard
      suppliedAmount={toUsdBigint(args.suppliedAmount)}
      borrowedAmount={toUsdBigint(args.borrowedAmount)}
      availableToBorrow={toUsdBigint(args.availableToBorrow)}
      usdcBalance={toUsdBigint(args.usdcBalance)}
      suppliedBalance={toUsdBigint(args.suppliedBalance)}
      hasCollateral={args.hasCollateral}
    />
  ),
}

export const AtBorrowLimit: Story = {
  args: {
    suppliedAmount: 0,
    borrowedAmount: 5000,
    availableToBorrow: 100,
    usdcBalance: 500,
    suppliedBalance: 0,
    hasCollateral: true,
  },
  render: (args) => (
    <YieldCard
      suppliedAmount={toUsdBigint(args.suppliedAmount)}
      borrowedAmount={toUsdBigint(args.borrowedAmount)}
      availableToBorrow={toUsdBigint(args.availableToBorrow)}
      usdcBalance={toUsdBigint(args.usdcBalance)}
      suppliedBalance={toUsdBigint(args.suppliedBalance)}
      hasCollateral={args.hasCollateral}
    />
  ),
}
