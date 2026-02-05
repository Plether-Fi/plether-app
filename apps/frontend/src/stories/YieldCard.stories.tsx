import type { Meta, StoryObj } from '@storybook/react-vite'
import { YieldCard, type MarketData } from '../components/YieldCard'

interface YieldCardArgs {
  bearSupplied: number
  bearBorrowed: number
  bearAvailable: number
  bearCollateral: number
  bullSupplied: number
  bullBorrowed: number
  bullAvailable: number
  bullCollateral: number
  usdcBalance: number
}

const meta: Meta<YieldCardArgs> = {
  title: 'Components/YieldCard',
  tags: ['autodocs'],
  argTypes: {
    bearSupplied: { control: { type: 'number', min: 0 }, description: 'BEAR market supplied amount' },
    bearBorrowed: { control: { type: 'number', min: 0 }, description: 'BEAR market borrowed amount' },
    bearAvailable: { control: { type: 'number', min: 0 }, description: 'BEAR market available to borrow' },
    bearCollateral: { control: { type: 'number', min: 0 }, description: 'BEAR market collateral value' },
    bullSupplied: { control: { type: 'number', min: 0 }, description: 'BULL market supplied amount' },
    bullBorrowed: { control: { type: 'number', min: 0 }, description: 'BULL market borrowed amount' },
    bullAvailable: { control: { type: 'number', min: 0 }, description: 'BULL market available to borrow' },
    bullCollateral: { control: { type: 'number', min: 0 }, description: 'BULL market collateral value' },
    usdcBalance: { control: { type: 'number', min: 0 }, description: 'User USDC wallet balance' },
  },
}

export default meta
type Story = StoryObj<YieldCardArgs>

function toUsdBigint(value: number): bigint {
  return BigInt(Math.floor(value * 1e6))
}

function argsToMarkets(args: YieldCardArgs): { bearMarket: MarketData; bullMarket: MarketData } {
  return {
    bearMarket: {
      suppliedAmount: toUsdBigint(args.bearSupplied),
      borrowedAmount: toUsdBigint(args.bearBorrowed),
      availableToBorrow: toUsdBigint(args.bearAvailable),
      collateral: toUsdBigint(args.bearCollateral),
    },
    bullMarket: {
      suppliedAmount: toUsdBigint(args.bullSupplied),
      borrowedAmount: toUsdBigint(args.bullBorrowed),
      availableToBorrow: toUsdBigint(args.bullAvailable),
      collateral: toUsdBigint(args.bullCollateral),
    },
  }
}

export const Default: Story = {
  args: {
    bearSupplied: 2500,
    bearBorrowed: 500,
    bearAvailable: 1500,
    bearCollateral: 3000,
    bullSupplied: 2500,
    bullBorrowed: 500,
    bullAvailable: 1500,
    bullCollateral: 3000,
    usdcBalance: 10000,
  },
  render: (args) => {
    const { bearMarket, bullMarket } = argsToMarkets(args)
    return (
      <YieldCard
        bearMarket={bearMarket}
        bullMarket={bullMarket}
        usdcBalance={toUsdBigint(args.usdcBalance)}
      />
    )
  },
}

export const NewUser: Story = {
  args: {
    bearSupplied: 0,
    bearBorrowed: 0,
    bearAvailable: 0,
    bearCollateral: 0,
    bullSupplied: 0,
    bullBorrowed: 0,
    bullAvailable: 0,
    bullCollateral: 0,
    usdcBalance: 10000,
  },
  render: (args) => {
    const { bearMarket, bullMarket } = argsToMarkets(args)
    return (
      <YieldCard
        bearMarket={bearMarket}
        bullMarket={bullMarket}
        usdcBalance={toUsdBigint(args.usdcBalance)}
      />
    )
  },
}

export const BullOnly: Story = {
  args: {
    bearSupplied: 0,
    bearBorrowed: 0,
    bearAvailable: 0,
    bearCollateral: 0,
    bullSupplied: 5000,
    bullBorrowed: 1000,
    bullAvailable: 3000,
    bullCollateral: 5000,
    usdcBalance: 5000,
  },
  render: (args) => {
    const { bearMarket, bullMarket } = argsToMarkets(args)
    return (
      <YieldCard
        bearMarket={bearMarket}
        bullMarket={bullMarket}
        usdcBalance={toUsdBigint(args.usdcBalance)}
      />
    )
  },
}

export const BearOnly: Story = {
  args: {
    bearSupplied: 5000,
    bearBorrowed: 1000,
    bearAvailable: 3000,
    bearCollateral: 5000,
    bullSupplied: 0,
    bullBorrowed: 0,
    bullAvailable: 0,
    bullCollateral: 0,
    usdcBalance: 5000,
  },
  render: (args) => {
    const { bearMarket, bullMarket } = argsToMarkets(args)
    return (
      <YieldCard
        bearMarket={bearMarket}
        bullMarket={bullMarket}
        usdcBalance={toUsdBigint(args.usdcBalance)}
      />
    )
  },
}

export const HighUtilization: Story = {
  args: {
    bearSupplied: 10000,
    bearBorrowed: 8000,
    bearAvailable: 500,
    bearCollateral: 10000,
    bullSupplied: 10000,
    bullBorrowed: 9000,
    bullAvailable: 200,
    bullCollateral: 10000,
    usdcBalance: 2000,
  },
  render: (args) => {
    const { bearMarket, bullMarket } = argsToMarkets(args)
    return (
      <YieldCard
        bearMarket={bearMarket}
        bullMarket={bullMarket}
        usdcBalance={toUsdBigint(args.usdcBalance)}
      />
    )
  },
}
