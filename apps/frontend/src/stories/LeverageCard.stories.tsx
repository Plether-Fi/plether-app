import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, userEvent, within } from 'storybook/test'
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

export const OpenBearPosition: Story = {
  args: {
    usdcBalance: 10000,
  },
  render: (args) => (
    <LeverageCard usdcBalance={toUsdcBigint(args.usdcBalance)} />
  ),
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)

    await step('Verify BEAR side is selected by default', async () => {
      const bearButton = canvas.getByRole('button', { name: /dxy-bear/i })
      expect(bearButton).toHaveClass('border-cyber-electric-fuchsia')
    })

    await step('Enter collateral amount', async () => {
      const input = canvas.getByPlaceholderText('0.00')
      await userEvent.clear(input)
      await userEvent.type(input, '500')
      expect(input).toHaveValue('500')
    })

    await step('Adjust leverage slider', async () => {
      const slider = canvas.getByRole('slider')
      await userEvent.click(slider)
      expect(slider).toBeInTheDocument()
    })

    await step('Verify position preview is shown', async () => {
      expect(canvas.getByText(/position preview/i)).toBeInTheDocument()
      expect(canvas.getByText(/position size/i)).toBeInTheDocument()
      expect(canvas.getByText(/liquidation price/i)).toBeInTheDocument()
    })
  },
}

export const OpenBullPosition: Story = {
  args: {
    usdcBalance: 10000,
  },
  render: (args) => (
    <LeverageCard usdcBalance={toUsdcBigint(args.usdcBalance)} />
  ),
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)

    await step('Switch to BULL side', async () => {
      const bullButton = canvas.getByRole('button', { name: /dxy-bull/i })
      await userEvent.click(bullButton)
      expect(bullButton).toHaveClass('border-cyber-neon-green')
    })

    await step('Enter collateral amount', async () => {
      const input = canvas.getByPlaceholderText('0.00')
      await userEvent.clear(input)
      await userEvent.type(input, '1000')
      expect(input).toHaveValue('1000')
    })
  },
}

export const SideToggle: Story = {
  args: {
    usdcBalance: 10000,
  },
  render: (args) => (
    <LeverageCard usdcBalance={toUsdcBigint(args.usdcBalance)} />
  ),
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)

    await step('Start with BEAR selected', async () => {
      const bearButton = canvas.getByRole('button', { name: /dxy-bear/i })
      expect(bearButton).toHaveClass('border-cyber-electric-fuchsia')
    })

    await step('Switch to BULL', async () => {
      const bullButton = canvas.getByRole('button', { name: /dxy-bull/i })
      await userEvent.click(bullButton)
      expect(bullButton).toHaveClass('border-cyber-neon-green')
    })

    await step('Switch back to BEAR', async () => {
      const bearButton = canvas.getByRole('button', { name: /dxy-bear/i })
      await userEvent.click(bearButton)
      expect(bearButton).toHaveClass('border-cyber-electric-fuchsia')
    })
  },
}
