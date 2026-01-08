import type { Meta, StoryObj } from '@storybook/react-vite'
import { TokenIcon } from '../components/ui'

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

const HEALTH_FACTOR_WARNING = 1.5
const HEALTH_FACTOR_DANGER = 1.2

function formatUsd(value: number): string {
  return new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' }).format(value)
}

function formatPercent(value: number): string {
  return `${value.toFixed(2)}%`
}

interface PositionCardProps {
  side: 'BEAR' | 'BULL'
  leverage: number
  size: number
  collateral: number
  pnl: number
  pnlPercentage: number
  healthFactor: number
  onAdjust?: () => void
}

function PositionCard({ side, leverage, size, collateral, pnl, pnlPercentage, healthFactor, onAdjust }: PositionCardProps) {
  const sideColor = side === 'BEAR' ? 'text-cyber-electric-fuchsia' : 'text-cyber-neon-green'
  const pnlColor = pnl >= 0 ? 'text-cyber-neon-green' : 'text-cyber-electric-fuchsia'
  const healthColor = healthFactor >= HEALTH_FACTOR_WARNING
    ? 'text-cyber-neon-green'
    : healthFactor >= HEALTH_FACTOR_DANGER
      ? 'text-cyber-warning-text'
      : 'text-cyber-electric-fuchsia'

  return (
    <div className="bg-cyber-surface-dark p-4 border border-cyber-border-glow/30 hover:border-cyber-bright-blue/50 transition-all shadow-md">
      <div className="flex flex-col md:flex-row md:items-center justify-between gap-4">
        <div className="flex items-center gap-4">
          <TokenIcon side={side} />
          <div>
            <div className="flex items-center gap-2">
              <span className={`font-semibold ${sideColor}`}>DXY-{side}</span>
              <span className="px-1.5 py-0.5 bg-cyber-surface-light text-xs text-cyber-text-secondary font-medium border border-cyber-border-glow/30">
                {leverage}x
              </span>
            </div>
            <div className="text-xs text-cyber-text-secondary mt-1">
              Size: {formatUsd(size)} | Collateral: {formatUsd(collateral)}
            </div>
          </div>
        </div>

        <div className="flex flex-wrap items-center gap-6 lg:gap-12 flex-1 md:justify-end">
          <div className="flex flex-col">
            <span className="text-xs text-cyber-text-secondary mb-1">PnL</span>
            <span className={`text-sm font-semibold ${pnlColor}`}>
              {formatUsd(pnl)} ({pnlPercentage > 0 ? '+' : ''}{formatPercent(pnlPercentage)})
            </span>
          </div>
          <div className="flex flex-col">
            <span className="text-xs text-cyber-text-secondary mb-1">Liq. Price</span>
            <span className="text-sm font-semibold text-cyber-text-primary">
              ${(side === 'BEAR' ? 115 : 95).toFixed(2)}
            </span>
          </div>
          <div className="flex flex-col">
            <div className="flex items-center gap-1 text-xs text-cyber-text-secondary mb-1">
              Health
              <span className="material-symbols-outlined text-[10px] text-cyber-text-secondary">help</span>
            </div>
            <span className={`text-sm font-semibold ${healthColor}`}>
              {healthFactor.toFixed(2)}
            </span>
          </div>

          <div className="flex items-center gap-2 mt-2 md:mt-0">
            <button
              onClick={onAdjust}
              className="px-3 py-1.5 text-sm border border-cyber-border-glow/30 text-cyber-text-secondary hover:bg-cyber-surface-light hover:text-cyber-bright-blue transition-colors"
            >
              Adjust
            </button>
            <button className="px-3 py-1.5 text-sm bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/80 text-cyber-text-primary transition-colors shadow-md shadow-cyber-electric-fuchsia/20">
              Close
            </button>
          </div>
        </div>
      </div>
    </div>
  )
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
  render: (args) => <PositionCard {...args} />,
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
  render: (args) => <PositionCard {...args} />,
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
  render: (args) => <PositionCard {...args} />,
}

export const AllPositions: Story = {
  render: () => (
    <div className="space-y-4">
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Bear Position (Profitable):</p>
        <PositionCard side="BEAR" leverage={3} size={15000} collateral={5000} pnl={1250} pnlPercentage={25} healthFactor={2.1} />
      </div>
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Bull Position (Loss):</p>
        <PositionCard side="BULL" leverage={2} size={10000} collateral={5000} pnl={-750} pnlPercentage={-7.5} healthFactor={1.8} />
      </div>
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Low Health (Danger):</p>
        <PositionCard side="BEAR" leverage={5} size={25000} collateral={5000} pnl={-2000} pnlPercentage={-40} healthFactor={1.15} />
      </div>
    </div>
  ),
}
