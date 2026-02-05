import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, userEvent, within } from 'storybook/test'
import { TradeCard } from '../components/TradeCard'

interface TradeCardArgs {
  usdcBalance: number
  bearBalance: number
  bullBalance: number
}

const meta: Meta<TradeCardArgs> = {
  title: 'Components/TradeCard',
  tags: ['autodocs'],
  argTypes: {
    usdcBalance: {
      control: { type: 'number', min: 0 },
      description: 'USDC wallet balance',
    },
    bearBalance: {
      control: { type: 'number', min: 0 },
      description: 'plDXY-BEAR wallet balance',
    },
    bullBalance: {
      control: { type: 'number', min: 0 },
      description: 'plDXY-BULL wallet balance',
    },
  },
}

export default meta
type Story = StoryObj<TradeCardArgs>

function toUsdcBigint(value: number): bigint {
  return BigInt(Math.floor(value * 1e6))
}

function toTokenBigint(value: number): bigint {
  return BigInt(Math.floor(value * 1e18))
}

export const Default: Story = {
  args: {
    usdcBalance: 10000,
    bearBalance: 500,
    bullBalance: 500,
  },
  render: (args) => (
    <TradeCard
      usdcBalance={toUsdcBigint(args.usdcBalance)}
      bearBalance={toTokenBigint(args.bearBalance)}
      bullBalance={toTokenBigint(args.bullBalance)}
    />
  ),
}

export const HighBalances: Story = {
  args: {
    usdcBalance: 100000,
    bearBalance: 5000,
    bullBalance: 7500,
  },
  render: (args) => (
    <TradeCard
      usdcBalance={toUsdcBigint(args.usdcBalance)}
      bearBalance={toTokenBigint(args.bearBalance)}
      bullBalance={toTokenBigint(args.bullBalance)}
    />
  ),
}

export const LowBalances: Story = {
  args: {
    usdcBalance: 100,
    bearBalance: 10,
    bullBalance: 25,
  },
  render: (args) => (
    <TradeCard
      usdcBalance={toUsdcBigint(args.usdcBalance)}
      bearBalance={toTokenBigint(args.bearBalance)}
      bullBalance={toTokenBigint(args.bullBalance)}
    />
  ),
}

export const BuyBearFlow: Story = {
  args: {
    usdcBalance: 10000,
    bearBalance: 500,
    bullBalance: 500,
  },
  render: (args) => (
    <TradeCard
      usdcBalance={toUsdcBigint(args.usdcBalance)}
      bearBalance={toTokenBigint(args.bearBalance)}
      bullBalance={toTokenBigint(args.bullBalance)}
    />
  ),
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)

    await step('Verify Buy mode is selected by default', async () => {
      const buyButton = canvas.getByRole('button', { name: /buy/i })
      expect(buyButton).toHaveClass('bg-cyber-surface-dark')
    })

    await step('Select plDXY-BEAR token', async () => {
      const bearButton = canvas.getByRole('button', { name: /pldxy-bear/i })
      await userEvent.click(bearButton)
      expect(bearButton).toHaveClass('border-cyber-electric-fuchsia')
    })

    await step('Enter amount to buy', async () => {
      const input = canvas.getByPlaceholderText('0.00')
      await userEvent.clear(input)
      await userEvent.type(input, '100')
      expect(input).toHaveValue('100')
    })

    await step('Expand swap details', async () => {
      const detailsButton = canvas.getByRole('button', { name: /swap details/i })
      await userEvent.click(detailsButton)
      expect(canvas.getByText(/route/i)).toBeInTheDocument()
    })
  },
}

export const SellBullFlow: Story = {
  args: {
    usdcBalance: 10000,
    bearBalance: 500,
    bullBalance: 500,
  },
  render: (args) => (
    <TradeCard
      usdcBalance={toUsdcBigint(args.usdcBalance)}
      bearBalance={toTokenBigint(args.bearBalance)}
      bullBalance={toTokenBigint(args.bullBalance)}
    />
  ),
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)

    await step('Switch to Sell mode', async () => {
      const sellButton = canvas.getByRole('button', { name: /^sell$/i })
      await userEvent.click(sellButton)
      expect(sellButton).toHaveClass('bg-cyber-surface-dark')
    })

    await step('Select plDXY-BULL token', async () => {
      const bullButton = canvas.getByRole('button', { name: /pldxy-bull/i })
      await userEvent.click(bullButton)
      expect(bullButton).toHaveClass('border-cyber-neon-green')
    })

    await step('Enter amount to sell', async () => {
      const input = canvas.getByPlaceholderText('0.00')
      await userEvent.clear(input)
      await userEvent.type(input, '50')
      expect(input).toHaveValue('50')
    })
  },
}

export const TokenToggle: Story = {
  args: {
    usdcBalance: 10000,
    bearBalance: 500,
    bullBalance: 500,
  },
  render: (args) => (
    <TradeCard
      usdcBalance={toUsdcBigint(args.usdcBalance)}
      bearBalance={toTokenBigint(args.bearBalance)}
      bullBalance={toTokenBigint(args.bullBalance)}
    />
  ),
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)

    await step('Start with BULL selected', async () => {
      const bullButton = canvas.getByRole('button', { name: /pldxy-bull/i })
      expect(bullButton).toHaveClass('border-cyber-neon-green')
    })

    await step('Switch to BULL', async () => {
      const bullButton = canvas.getByRole('button', { name: /pldxy-bull/i })
      await userEvent.click(bullButton)
      expect(bullButton).toHaveClass('border-cyber-neon-green')
    })

    await step('Switch back to BEAR', async () => {
      const bearButton = canvas.getByRole('button', { name: /pldxy-bear/i })
      await userEvent.click(bearButton)
      expect(bearButton).toHaveClass('border-cyber-electric-fuchsia')
    })
  },
}

export const ModeToggle: Story = {
  args: {
    usdcBalance: 10000,
    bearBalance: 500,
    bullBalance: 500,
  },
  render: (args) => (
    <TradeCard
      usdcBalance={toUsdcBigint(args.usdcBalance)}
      bearBalance={toTokenBigint(args.bearBalance)}
      bullBalance={toTokenBigint(args.bullBalance)}
    />
  ),
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)

    await step('Start in Buy mode', async () => {
      const buyButton = canvas.getByRole('button', { name: /^buy$/i })
      expect(buyButton).toHaveClass('bg-cyber-surface-dark')
    })

    await step('Switch to Sell mode', async () => {
      const sellButton = canvas.getByRole('button', { name: /^sell$/i })
      await userEvent.click(sellButton)
      expect(sellButton).toHaveClass('bg-cyber-surface-dark')
    })

    await step('Switch back to Buy mode', async () => {
      const buyButton = canvas.getByRole('button', { name: /^buy$/i })
      await userEvent.click(buyButton)
      expect(buyButton).toHaveClass('bg-cyber-surface-dark')
    })
  },
}
