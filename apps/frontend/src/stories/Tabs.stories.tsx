import { useState } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { Tabs, TabPanel } from '../components/ui'

const meta: Meta<typeof Tabs> = {
  title: 'UI/Tabs',
  component: Tabs,
  tags: ['autodocs'],
}

export default meta
type Story = StoryObj<typeof meta>

const defaultTabs = [
  { id: 'tab1', label: 'Tab 1' },
  { id: 'tab2', label: 'Tab 2' },
  { id: 'tab3', label: 'Tab 3' },
]

function TabsWithState({ tabs = defaultTabs }: { tabs?: { id: string; label: string }[] }) {
  const [activeTab, setActiveTab] = useState(tabs[0].id)
  return (
    <div className="space-y-4">
      <Tabs tabs={tabs} activeTab={activeTab} onChange={setActiveTab} />
      {tabs.map((tab) => (
        <TabPanel key={tab.id} isActive={activeTab === tab.id}>
          <div className="p-4 bg-cyber-surface-dark border border-cyber-border-glow/30">
            <p className="text-cyber-text-primary">Content for {tab.label}</p>
          </div>
        </TabPanel>
      ))}
    </div>
  )
}

export const Default: Story = {
  render: () => <TabsWithState />,
}

export const TwoTabs: Story = {
  render: () => (
    <TabsWithState
      tabs={[
        { id: 'buy', label: 'Buy' },
        { id: 'sell', label: 'Sell' },
      ]}
    />
  ),
}

export const ManyTabs: Story = {
  render: () => (
    <TabsWithState
      tabs={[
        { id: 'overview', label: 'Overview' },
        { id: 'positions', label: 'Positions' },
        { id: 'history', label: 'History' },
        { id: 'settings', label: 'Settings' },
      ]}
    />
  ),
}

function TradeExampleTabs() {
  const [activeTab, setActiveTab] = useState('stake')
  const tabs = [
    { id: 'stake', label: 'Stake' },
    { id: 'unstake', label: 'Unstake' },
  ]
  return (
    <div className="max-w-md space-y-4">
      <Tabs tabs={tabs} activeTab={activeTab} onChange={setActiveTab} />
      <TabPanel isActive={activeTab === 'stake'}>
        <div className="p-4 bg-cyber-surface-dark border border-cyber-border-glow/30 space-y-4">
          <p className="text-cyber-text-secondary text-sm">Enter amount to stake</p>
          <input
            type="text"
            placeholder="0.00"
            className="w-full px-4 py-3 bg-cyber-surface-light border border-cyber-border-glow/30 text-cyber-text-primary"
          />
          <button className="w-full py-3 bg-cyber-neon-green text-cyber-bg font-medium">
            Stake
          </button>
        </div>
      </TabPanel>
      <TabPanel isActive={activeTab === 'unstake'}>
        <div className="p-4 bg-cyber-surface-dark border border-cyber-border-glow/30 space-y-4">
          <p className="text-cyber-text-secondary text-sm">Enter amount to unstake</p>
          <input
            type="text"
            placeholder="0.00"
            className="w-full px-4 py-3 bg-cyber-surface-light border border-cyber-border-glow/30 text-cyber-text-primary"
          />
          <button className="w-full py-3 bg-cyber-electric-fuchsia text-cyber-text-primary font-medium">
            Unstake
          </button>
        </div>
      </TabPanel>
    </div>
  )
}

export const TradeExample: Story = {
  render: () => <TradeExampleTabs />,
}
