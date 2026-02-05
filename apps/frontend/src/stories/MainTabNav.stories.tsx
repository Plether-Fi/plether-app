import type { Meta, StoryObj } from '@storybook/react-vite'
import { useState } from 'react'
import { MainTabNav } from '../components/MainTabNav'

const meta: Meta<typeof MainTabNav> = {
  title: 'Components/MainTabNav',
  component: MainTabNav,
  tags: ['autodocs'],
}

export default meta
type Story = StoryObj<typeof MainTabNav>

function InteractiveTabNav() {
  const [activeTab, setActiveTab] = useState<'trade' | 'leverage' | 'lending'>('trade')
  return <MainTabNav activeTab={activeTab} onTabChange={setActiveTab} />
}

export const Interactive: Story = {
  render: () => <InteractiveTabNav />,
}

export const TradeActive: Story = {
  args: {
    activeTab: 'trade',
    onTabChange: () => {},
  },
}

export const LeverageActive: Story = {
  args: {
    activeTab: 'leverage',
    onTabChange: () => {},
  },
}

export const LendingActive: Story = {
  args: {
    activeTab: 'lending',
    onTabChange: () => {},
  },
}
