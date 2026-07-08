import type { Meta, StoryObj } from '@storybook/react-vite'
import { PositionsSection } from '../components/PositionsSection'
import type { LeveragePosition } from '../types'

const meta: Meta<typeof PositionsSection> = {
  title: 'Components/PositionsSection',
  component: PositionsSection,
  tags: ['autodocs'],
  args: {
    isLoading: false,
    isClosing: false,
    onAdjust: () => {},
    onClose: () => {},
  },
}

export default meta
type Story = StoryObj<typeof PositionsSection>

function usdc(value: number): bigint {
  return BigInt(Math.floor(value * 1e6))
}

const lowHealthBearPosition: LeveragePosition = {
  id: 'bear-low-health',
  side: 'BEAR',
  leverage: 5.02,
  size: usdc(499.3),
  collateral: usdc(99.3),
  entryPrice: usdc(1),
  liquidationPrice: usdc(0.85),
  healthFactor: 1.14,
  pnl: usdc(0),
  pnlPercentage: 0,
}

const healthyBullPosition: LeveragePosition = {
  id: 'bull-healthy',
  side: 'BULL',
  leverage: 2,
  size: usdc(10000),
  collateral: usdc(5000),
  entryPrice: usdc(103),
  liquidationPrice: usdc(95),
  healthFactor: 1.8,
  pnl: -usdc(750),
  pnlPercentage: -7.5,
}

export const LowHealthWarning: Story = {
  args: {
    positions: [lowHealthBearPosition],
  },
}

export const HealthyPositions: Story = {
  args: {
    positions: [healthyBullPosition],
  },
}

export const Empty: Story = {
  args: {
    positions: [],
  },
}

export const Loading: Story = {
  args: {
    positions: [],
    isLoading: true,
  },
}
