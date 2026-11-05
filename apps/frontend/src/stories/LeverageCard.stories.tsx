import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, fireEvent, userEvent, within } from 'storybook/test'
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

    await step('Select BEAR from the default BULL side', async () => {
      const bullButton = canvas.getByRole('button', { name: /dxy-bull/i })
      expect(bullButton).toHaveClass('border-positive')
      const bearButton = canvas.getByRole('button', { name: /dxy-bear/i })
      await userEvent.click(bearButton)
      expect(bearButton).toHaveClass('border-brand-orange')
      expect(bullButton).not.toHaveClass('border-positive')
    })

    await step('Enter collateral amount', async () => {
      const input = canvas.getByPlaceholderText('0.00')
      await userEvent.clear(input)
      await userEvent.type(input, '500')
      expect(input).toHaveValue('500')
    })

    await step('Adjust leverage slider', async () => {
      const slider = canvas.getByRole('slider')
      fireEvent.change(slider, { target: { value: '3' } })
      expect(slider).toHaveValue('3')
      expect(canvas.getByText('3.0x')).toBeVisible()
    })

    await step('Verify position value, equity and debt readouts are visible', async () => {
      expect(canvas.getByText('PREVIEW')).toBeVisible()
      expect(canvas.getByText('Position Value')).toBeVisible()
      expect(canvas.getByText('Your Equity')).toBeVisible()
      expect(canvas.getByText('Debt', { exact: true })).toBeVisible()
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
      const bearButton = canvas.getByRole('button', { name: /dxy-bear/i })
      await userEvent.click(bearButton)
      expect(bearButton).toHaveClass('border-brand-orange')
      const bullButton = canvas.getByRole('button', { name: /dxy-bull/i })
      await userEvent.click(bullButton)
      expect(bullButton).toHaveClass('border-positive')
      expect(bearButton).not.toHaveClass('border-brand-orange')
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

    await step('Start with BULL selected', async () => {
      expect(canvas.getByRole('button', { name: /dxy-bull/i })).toHaveClass('border-positive')
      expect(canvas.getByRole('button', { name: /dxy-bear/i })).not.toHaveClass('border-brand-orange')
    })

    await step('Switch to BEAR', async () => {
      const bearButton = canvas.getByRole('button', { name: /dxy-bear/i })
      await userEvent.click(bearButton)
      expect(bearButton).toHaveClass('border-brand-orange')
      expect(canvas.getByRole('button', { name: /dxy-bull/i })).not.toHaveClass('border-positive')
    })

    await step('Switch back to BULL', async () => {
      const bullButton = canvas.getByRole('button', { name: /dxy-bull/i })
      await userEvent.click(bullButton)
      expect(bullButton).toHaveClass('border-positive')
      expect(canvas.getByRole('button', { name: /dxy-bear/i })).not.toHaveClass('border-brand-orange')
    })
  },
}
