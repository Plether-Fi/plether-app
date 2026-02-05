import type { Meta, StoryObj } from '@storybook/react-vite'
import { PositionCard } from '../components/PositionCard'
import type { LeveragePosition } from '../types'

interface PositionCardArgs {
  side: 'BEAR' | 'BULL'
  leverage: number
  size: number
  collateral: number
  pnl: number
  pnlPercentage: number
  healthFactor: number
}

const meta: Meta<PositionCardArgs> = {
  title: 'Components/PositionCard',
  tags: ['autodocs'],
  argTypes: {
    side: {
      control: 'radio',
      options: ['BEAR', 'BULL'],
      description: 'Position side',
    },
    leverage: {
      control: { type: 'number', min: 1, max: 10 },
      description: 'Leverage multiplier',
    },
    size: {
      control: { type: 'number', min: 0 },
      description: 'Position size in USD',
    },
    collateral: {
      control: { type: 'number', min: 0 },
      description: 'Collateral amount in USD',
    },
    pnl: {
      control: { type: 'number' },
      description: 'Profit/Loss in USD (can be negative)',
    },
    pnlPercentage: {
      control: { type: 'number', min: -100, max: 1000 },
      description: 'PnL percentage',
    },
    healthFactor: {
      control: { type: 'number', min: 0, max: 10, step: 0.1 },
      description: 'Health factor (>1.5 healthy, >1.2 warning, <1.2 danger)',
    },
  },
}

export default meta
type Story = StoryObj<PositionCardArgs>

function toUsdBigint(value: number): bigint {
  return BigInt(Math.floor(value * 1e6))
}

function argsToPosition(args: PositionCardArgs): LeveragePosition {
  return {
    id: '1',
    side: args.side,
    leverage: args.leverage,
    size: toUsdBigint(args.size),
    collateral: toUsdBigint(args.collateral),
    entryPrice: toUsdBigint(103),
    liquidationPrice: toUsdBigint(args.side === 'BEAR' ? 115 : 95),
    healthFactor: args.healthFactor,
    pnl: toUsdBigint(args.pnl),
    pnlPercentage: args.pnlPercentage,
  }
}

export const BearPosition: Story = {
  args: {
    side: 'BEAR',
    leverage: 3,
    size: 15000,
    collateral: 5000,
    pnl: 1250,
    pnlPercentage: 25,
    healthFactor: 2.1,
  },
  render: (args) => <PositionCard position={argsToPosition(args)} onAdjust={() => {}} onClose={() => {}} />,
}

export const BullPosition: Story = {
  args: {
    side: 'BULL',
    leverage: 2,
    size: 10000,
    collateral: 5000,
    pnl: -750,
    pnlPercentage: -7.5,
    healthFactor: 1.8,
  },
  render: (args) => <PositionCard position={argsToPosition(args)} onAdjust={() => {}} onClose={() => {}} />,
}

export const LowHealthPosition: Story = {
  args: {
    side: 'BEAR',
    leverage: 5,
    size: 25000,
    collateral: 5000,
    pnl: -2000,
    pnlPercentage: -40,
    healthFactor: 1.15,
  },
  render: (args) => <PositionCard position={argsToPosition(args)} onAdjust={() => {}} onClose={() => {}} />,
}

export const AllPositions: Story = {
  render: () => (
    <div className="space-y-4">
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Bear Position (Profitable):</p>
        <PositionCard
          position={argsToPosition({ side: 'BEAR', leverage: 3, size: 15000, collateral: 5000, pnl: 1250, pnlPercentage: 25, healthFactor: 2.1 })}
          onAdjust={() => {}}
          onClose={() => {}}
        />
      </div>
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Bull Position (Loss):</p>
        <PositionCard
          position={argsToPosition({ side: 'BULL', leverage: 2, size: 10000, collateral: 5000, pnl: -750, pnlPercentage: -7.5, healthFactor: 1.8 })}
          onAdjust={() => {}}
          onClose={() => {}}
        />
      </div>
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Low Health (Danger):</p>
        <PositionCard
          position={argsToPosition({ side: 'BEAR', leverage: 5, size: 25000, collateral: 5000, pnl: -2000, pnlPercentage: -40, healthFactor: 1.15 })}
          onAdjust={() => {}}
          onClose={() => {}}
        />
      </div>
    </div>
  ),
}
