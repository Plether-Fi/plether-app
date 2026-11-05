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
      expect(buyButton).toBeVisible()
      expect(canvas.getByLabelText('You pay')).toBeVisible()
    })

    await step('Select plDXY-BEAR token', async () => {
      const bearButton = canvas.getByRole('button', { name: /pldxy-bear/i })
      await userEvent.click(bearButton)
      expect(bearButton).toHaveClass('border-brand-orange')
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
      expect(canvas.getByText('USDC → Curve → plDXY-BEAR')).toBeVisible()
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
      expect(canvas.getByLabelText('You sell')).toBeVisible()
      expect(canvas.queryByLabelText('You pay')).not.toBeInTheDocument()
    })

    await step('Select plDXY-BULL token', async () => {
      const bullButton = canvas.getByRole('button', { name: /pldxy-bull/i })
      await userEvent.click(canvas.getByRole('button', { name: /pldxy-bear/i }))
      await userEvent.click(bullButton)
      await userEvent.click(canvas.getByRole('button', { name: /swap details/i }))
      expect(canvas.getByText('plDXY-BULL → ZapRouter → USDC')).toBeVisible()
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

    await step('Start with the BULL purchase route', async () => {
      await userEvent.click(canvas.getByRole('button', { name: /swap details/i }))
      expect(canvas.getByText('USDC → ZapRouter → plDXY-BULL')).toBeVisible()
    })

    await step('Switch to BEAR', async () => {
      await userEvent.click(canvas.getByRole('button', { name: /pldxy-bear/i }))
      expect(canvas.getByText('USDC → Curve → plDXY-BEAR')).toBeVisible()
      expect(canvas.queryByText('USDC → ZapRouter → plDXY-BULL')).not.toBeInTheDocument()
    })

    await step('Switch back to BULL', async () => {
      await userEvent.click(canvas.getByRole('button', { name: /pldxy-bull/i }))
      expect(canvas.getByText('USDC → ZapRouter → plDXY-BULL')).toBeVisible()
      expect(canvas.queryByText('USDC → Curve → plDXY-BEAR')).not.toBeInTheDocument()
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
      const input = canvas.getByLabelText('You pay')
      await userEvent.type(input, '100')
      expect(input).toHaveValue('100')
      await userEvent.click(canvas.getByRole('button', { name: /swap details/i }))
      expect(canvas.getByText('USDC → ZapRouter → plDXY-BULL')).toBeVisible()
    })

    await step('Switch to Sell mode', async () => {
      const sellButton = canvas.getByRole('button', { name: /^sell$/i })
      await userEvent.click(sellButton)
      const input = canvas.getByLabelText('You sell')
      expect(input).toHaveValue('')
      expect(canvas.queryByLabelText('You pay')).not.toBeInTheDocument()
      expect(canvas.getByText('plDXY-BULL → ZapRouter → USDC')).toBeVisible()
      await userEvent.type(input, '50')
    })

    await step('Switch back to Buy mode', async () => {
      const buyButton = canvas.getByRole('button', { name: /^buy$/i })
      await userEvent.click(buyButton)
      expect(canvas.getByLabelText('You pay')).toHaveValue('')
      expect(canvas.queryByLabelText('You sell')).not.toBeInTheDocument()
      expect(canvas.getByText('USDC → ZapRouter → plDXY-BULL')).toBeVisible()
    })
  },
}
