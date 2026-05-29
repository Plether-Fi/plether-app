import type { Meta, StoryObj } from '@storybook/react-vite'
import { PerpsInstrumentPanel } from '../components/PerpsInstrumentPanel'

const meta: Meta<typeof PerpsInstrumentPanel> = {
  title: 'Perps/Instrument Panel',
  component: PerpsInstrumentPanel,
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',
  },
  args: {
    icon: 'token',
    name: 'DXY Perp',
    description: 'DXY Basket Perpetual',
    stats: [
      { label: 'Oracle price', value: '0.9909' },
      { label: '24h change', value: '-0.16%', tone: 'negative' },
      { label: '24h volume', value: '2.4M USDC' },
      {
        label: 'Open interest',
        values: [
          { label: 'Long', value: '10.8M USDC', tone: 'positive' },
          { label: 'Short', value: '7.9M USDC', tone: 'negative' },
        ],
      },
      { label: 'Available liquidity', value: '6.3M USDC' },
      { label: 'Cost of carry', value: '5.24%' },
    ],
  },
}

export default meta
type Story = StoryObj<typeof meta>

export const Default: Story = {
  render: (args) => (
    <div className="min-h-screen bg-cyber-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl">
        <PerpsInstrumentPanel {...args} />
      </div>
    </div>
  ),
}

export const PositiveSession: Story = {
  args: {
    stats: [
      { label: 'Oracle price', value: '0.9934' },
      { label: '24h change', value: '+0.21%', tone: 'positive' },
      { label: '24h volume', value: '3.1M USDC' },
      {
        label: 'Open interest',
        values: [
          { label: 'Long', value: '13.2M USDC', tone: 'positive' },
          { label: 'Short', value: '8.2M USDC', tone: 'negative' },
        ],
      },
      { label: 'Available liquidity', value: '8.7M USDC' },
      { label: 'Cost of carry', value: '4.87%' },
    ],
  },
  render: Default.render,
}
