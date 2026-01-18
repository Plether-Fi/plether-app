import type { Meta, StoryObj } from '@storybook/react-vite'
import { Skeleton, SkeletonCard } from '../components/ui'

const meta: Meta<typeof Skeleton> = {
  title: 'UI/Skeleton',
  component: Skeleton,
  tags: ['autodocs'],
  argTypes: {
    variant: {
      control: 'select',
      options: ['text', 'rectangular', 'circular'],
    },
    width: { control: 'text' },
    height: { control: 'text' },
  },
}

export default meta
type Story = StoryObj<typeof meta>

export const Text: Story = {
  args: {
    variant: 'text',
    width: '200px',
  },
}

export const Rectangular: Story = {
  args: {
    variant: 'rectangular',
    width: '200px',
    height: '100px',
  },
}

export const Circular: Story = {
  args: {
    variant: 'circular',
    width: '48px',
    height: '48px',
  },
}

export const TextLines: Story = {
  render: () => (
    <div className="space-y-2 w-64">
      <Skeleton variant="text" width="100%" />
      <Skeleton variant="text" width="80%" />
      <Skeleton variant="text" width="60%" />
    </div>
  ),
}

export const ProfileCard: Story = {
  render: () => (
    <div className="flex items-center gap-4 p-4 bg-cyber-surface-dark border border-cyber-border-glow/30">
      <Skeleton variant="circular" width={48} height={48} />
      <div className="space-y-2 flex-1">
        <Skeleton variant="text" width="60%" />
        <Skeleton variant="text" width="40%" />
      </div>
    </div>
  ),
}

export const Card: Story = {
  render: () => <SkeletonCard />,
}

export const GridOfCards: Story = {
  render: () => (
    <div className="grid grid-cols-2 gap-4">
      <SkeletonCard />
      <SkeletonCard />
      <SkeletonCard />
      <SkeletonCard />
    </div>
  ),
}

export const TableRow: Story = {
  render: () => (
    <div className="flex items-center gap-4 p-4 bg-cyber-surface-dark border border-cyber-border-glow/30">
      <Skeleton variant="rectangular" width={40} height={40} />
      <Skeleton variant="text" className="flex-1" />
      <Skeleton variant="text" width={80} />
      <Skeleton variant="text" width={60} />
    </div>
  ),
}
