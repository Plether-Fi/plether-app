import type { Meta, StoryObj } from '@storybook/react-vite'
import { PerpsInstrumentPanel } from '../components/PerpsInstrumentPanel'
import { TokenAmount } from '../components/ui'

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
    description: 'DXY Perpetual',
    stats: [
      { label: 'DXY price', value: '1.0091', freshness: 'fresh', freshnessTooltip: 'updated 24s ago' },
      { label: '24h change', value: '-0.16%', tone: 'negative' },
      { label: '24h volume', value: <TokenAmount amount="2.4M" /> },
      { label: 'Long open interest', value: <TokenAmount amount="10.8M" />, tone: 'positive' },
      { label: 'Short open interest', value: <TokenAmount amount="7.9M" />, tone: 'negative' },
      { label: 'Available liquidity', value: <TokenAmount amount="6.3M" /> },
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
      { label: 'DXY price', value: '1.0066', freshness: 'fresh', freshnessTooltip: 'updated 1m 12s ago' },
      { label: '24h change', value: '+0.21%', tone: 'positive' },
      { label: '24h volume', value: <TokenAmount amount="3.1M" /> },
      { label: 'Long open interest', value: <TokenAmount amount="13.2M" />, tone: 'positive' },
      { label: 'Short open interest', value: <TokenAmount amount="8.2M" />, tone: 'negative' },
      { label: 'Available liquidity', value: <TokenAmount amount="8.7M" /> },
      { label: 'Cost of carry', value: '4.87%' },
    ],
  },
  render: Default.render,
}
