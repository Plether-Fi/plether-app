import type { Meta, StoryObj } from '@storybook/react-vite'
import { MemoryRouter, Link } from 'react-router-dom'
import { Skeleton } from '../components/ui'

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

function formatUsd(value: number): string {
  return new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' }).format(value)
}

function SkeletonCard() {
  return (
    <div className="bg-cyber-surface-dark p-5 border border-cyber-border-glow/30 shadow-md h-full">
      <Skeleton className="h-3 w-24 mb-3" />
      <Skeleton className="h-8 w-32 mb-2" />
      <Skeleton className="h-3 w-40" />
    </div>
  )
}

interface PortfolioCardProps {
  title: string
  value: number
  description: string
  link: string
  isLoading?: boolean
  colorClass: string
}

function PortfolioCard({ title, value, description, link, isLoading, colorClass }: PortfolioCardProps) {
  if (isLoading) {
    return <SkeletonCard />
  }

  return (
    <Link to={link}>
      <div className="bg-cyber-surface-dark p-5 border border-cyber-border-glow/30 shadow-md hover:border-cyber-bright-blue/50 transition-colors cursor-pointer h-full">
        <p className="text-xs text-cyber-text-secondary uppercase tracking-wider font-medium mb-2">{title}</p>
        <div className={`text-2xl font-bold mb-1 ${colorClass}`}>{formatUsd(value)}</div>
        <p className="text-xs text-cyber-text-secondary truncate">{description}</p>
      </div>
    </Link>
  )
}

export const SpotHoldings: Story = {
  args: {
    title: 'Spot Holdings',
    value: 12500,
    description: '5,000 DXY-BEAR • 7,500 DXY-BULL',
    colorClass: 'text-cyber-bright-blue',
    isLoading: false,
  },
  render: (args) => (
    <MemoryRouter>
      <div className="w-64">
        <PortfolioCard {...args} link="/" />
      </div>
    </MemoryRouter>
  ),
}

export const StakedTokens: Story = {
  args: {
    title: 'Staked',
    value: 8000,
    description: '3,000 sDXY-BEAR • 5,000 sDXY-BULL',
    colorClass: 'text-cyber-bright-blue',
    isLoading: false,
  },
  render: (args) => (
    <MemoryRouter>
      <div className="w-64">
        <PortfolioCard {...args} link="/stake" />
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
        <PortfolioCard {...args} link="/leverage" />
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
        <PortfolioCard {...args} link="/" />
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
          value={12500}
          description="5,000 DXY-BEAR • 7,500 DXY-BULL"
          link="/"
          colorClass="text-cyber-bright-blue"
        />
        <PortfolioCard
          title="Staked"
          value={8000}
          description="3,000 sDXY-BEAR • 5,000 sDXY-BULL"
          link="/stake"
          colorClass="text-cyber-bright-blue"
        />
        <PortfolioCard
          title="Leverage"
          value={25000}
          description="2 active positions"
          link="/leverage"
          colorClass="text-cyber-electric-fuchsia"
        />
      </div>
    </MemoryRouter>
  ),
}
