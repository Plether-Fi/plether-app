import type { Meta, StoryObj } from '@storybook/react-vite'
import { MemoryRouter } from 'react-router-dom'
import { PortfolioCard } from '../components/PortfolioCard'

interface PortfolioCardArgs {
  title: string
  value: number
  description: string
  colorClass: string
  isLoading?: boolean
}

const meta: Meta<PortfolioCardArgs> = {
  title: 'Components/PortfolioCard',
  tags: ['autodocs'],
  argTypes: {
    title: {
      control: 'text',
      description: 'Card title',
    },
    value: {
      control: { type: 'number', min: 0 },
      description: 'Value in USD',
    },
    description: {
      control: 'text',
      description: 'Description text below value',
    },
    colorClass: {
      control: 'select',
      options: ['text-cyber-bright-blue', 'text-cyber-electric-fuchsia', 'text-cyber-neon-green'],
      description: 'Text color class for value',
    },
    isLoading: {
      control: 'boolean',
      description: 'Show loading skeleton',
    },
  },
}

export default meta
type Story = StoryObj<PortfolioCardArgs>

function toUsdBigint(value: number): bigint {
  return BigInt(Math.floor(value * 1e6))
}

export const SpotHoldings: Story = {
  args: {
    title: 'Spot Holdings',
    value: 12500,
    description: '5,000 plDXY-BEAR • 7,500 plDXY-BULL',
    colorClass: 'text-cyber-bright-blue',
    isLoading: false,
  },
  render: (args) => (
    <MemoryRouter>
      <div className="w-64">
        <PortfolioCard
          title={args.title}
          value={toUsdBigint(args.value)}
          description={args.description}
          link="/"
          isLoading={args.isLoading ?? false}
          colorClass={args.colorClass}
        />
      </div>
    </MemoryRouter>
  ),
}

export const StakedTokens: Story = {
  args: {
    title: 'Staked',
    value: 8000,
    description: '3,000 splDXY-BEAR • 5,000 splDXY-BULL',
    colorClass: 'text-cyber-bright-blue',
    isLoading: false,
  },
  render: (args) => (
    <MemoryRouter>
      <div className="w-64">
        <PortfolioCard
          title={args.title}
          value={toUsdBigint(args.value)}
          description={args.description}
          link="/stake"
          isLoading={args.isLoading ?? false}
          colorClass={args.colorClass}
        />
      </div>
    </MemoryRouter>
  ),
}

export const LeveragePositions: Story = {
  args: {
    title: 'Leverage',
    value: 25000,
    description: '2 active positions',
    colorClass: 'text-cyber-electric-fuchsia',
    isLoading: false,
  },
  render: (args) => (
    <MemoryRouter>
      <div className="w-64">
        <PortfolioCard
          title={args.title}
          value={toUsdBigint(args.value)}
          description={args.description}
          link="/leverage"
          isLoading={args.isLoading ?? false}
          colorClass={args.colorClass}
        />
      </div>
    </MemoryRouter>
  ),
}

export const Loading: Story = {
  args: {
    title: 'Spot Holdings',
    value: 0,
    description: '',
    colorClass: 'text-cyber-bright-blue',
    isLoading: true,
  },
  render: (args) => (
    <MemoryRouter>
      <div className="w-64">
        <PortfolioCard
          title={args.title}
          value={toUsdBigint(args.value)}
          description={args.description}
          link="/"
          isLoading={args.isLoading ?? false}
          colorClass={args.colorClass}
        />
      </div>
    </MemoryRouter>
  ),
}

export const AllCards: Story = {
  render: () => (
    <MemoryRouter>
      <div className="grid grid-cols-3 gap-4">
        <PortfolioCard
          title="Spot Holdings"
          value={toUsdBigint(12500)}
          description="5,000 plDXY-BEAR • 7,500 plDXY-BULL"
          link="/"
          isLoading={false}
          colorClass="text-cyber-bright-blue"
        />
        <PortfolioCard
          title="Staked"
          value={toUsdBigint(8000)}
          description="3,000 splDXY-BEAR • 5,000 splDXY-BULL"
          link="/stake"
          isLoading={false}
          colorClass="text-cyber-bright-blue"
        />
        <PortfolioCard
          title="Leverage"
          value={toUsdBigint(25000)}
          description="2 active positions"
          link="/leverage"
          isLoading={false}
          colorClass="text-cyber-electric-fuchsia"
        />
      </div>
    </MemoryRouter>
  ),
}
