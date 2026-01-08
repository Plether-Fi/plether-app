import type { Meta, StoryObj } from '@storybook/react-vite'
import { LeverageCard } from '../components/LeverageCard'

interface LeverageCardArgs {
  usdcBalance: number
}

const meta: Meta<LeverageCardArgs> = {
  title: 'Components/LeverageCard',
  tags: ['autodocs'],
  argTypes: {
    usdcBalance: {
      control: { type: 'number', min: 0 },
      description: 'USDC wallet balance',
    },
  },
}

export default meta
type Story = StoryObj<LeverageCardArgs>

function toUsdcBigint(value: number): bigint {
  return BigInt(Math.floor(value * 1e6))
}

export const Default: Story = {
  args: {
    usdcBalance: 10000,
  },
  render: (args) => (
    <LeverageCard usdcBalance={toUsdcBigint(args.usdcBalance)} />
  ),
}

export const HighBalance: Story = {
  args: {
    usdcBalance: 100000,
  },
  render: (args) => (
    <LeverageCard usdcBalance={toUsdcBigint(args.usdcBalance)} />
  ),
}

export const LowBalance: Story = {
  args: {
    usdcBalance: 100,
  },
  render: (args) => (
    <LeverageCard usdcBalance={toUsdcBigint(args.usdcBalance)} />
  ),
}
