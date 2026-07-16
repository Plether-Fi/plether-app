import type { Meta, StoryObj } from '@storybook/react-vite'
import { PerpsInstrumentPanel } from '../components/PerpsInstrumentPanel'
import { TokenAmount } from '../components/ui'
import { DOCS_LINKS } from '../config/docs'

function PoolLiquidityTooltip() {
  return (
    <div className="w-full space-y-2 text-left">
      <div className="grid grid-cols-[minmax(0,1fr)_auto] items-center gap-4">
        <span className="min-w-0 text-content-secondary">Long capacity</span>
        <span className="whitespace-nowrap font-semibold text-content-primary">953.33 USDC</span>
      </div>
      <div className="grid grid-cols-[minmax(0,1fr)_auto] items-center gap-4">
        <span className="min-w-0 text-content-secondary">Short capacity</span>
        <span className="whitespace-nowrap font-semibold text-content-primary">4 810.22 USDC</span>
      </div>
      <div className="grid grid-cols-[minmax(0,1fr)_auto] items-center gap-4">
        <span className="min-w-0 text-content-secondary">Minimum order size</span>
        <span className="whitespace-nowrap font-semibold text-content-primary">103.18 USDC</span>
      </div>
      <div className="grid grid-cols-[minmax(0,1fr)_auto] items-center gap-4">
        <span className="min-w-0 text-content-secondary">Minimum new position</span>
        <span className="whitespace-nowrap font-semibold text-content-primary">1 031.8 USDC</span>
      </div>
    </div>
  )
}

function CostOfCarryTooltip() {
  return (
    <div className="w-full space-y-3 text-left leading-5">
      <p>
        Annualized max carry paid by traders to LPs for the part of a position&apos;s worst-case
        payout backed by pool capital.
      </p>
      <p>
        This is not a funding rate; both sides can pay carry at the same time. The actual
        USDC amount depends on borrow base, side utilization, and elapsed time.
      </p>
    </div>
  )
}

const meta: Meta<typeof PerpsInstrumentPanel> = {
  title: 'Perps/Instrument Panel',
  component: PerpsInstrumentPanel,
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',
  },
  args: {
    icon: 'token',
    name: 'plDXY Perp',
    description: 'plDXY Perpetual',
    stats: [
      { label: 'plDXY Perp price', value: '1.0091', freshness: 'fresh', freshnessTooltip: 'updated 24s ago' },
      { label: '24h change', value: '-0.16%', tone: 'negative' },
      { label: '24h volume', value: <TokenAmount amount="2.4M" /> },
      { label: 'Long open interest', value: <TokenAmount amount="10.8M" />, tone: 'positive' },
      { label: 'Short open interest', value: <TokenAmount amount="7.9M" />, tone: 'negative' },
      {
        label: 'Pool liquidity',
        value: <TokenAmount amount="6.3M" />,
        tooltip: <PoolLiquidityTooltip />,
        tooltipDocsLink: DOCS_LINKS.poolLiquidity,
        tooltipClassName: 'w-[400px] whitespace-normal p-4',
        tooltipPosition: 'left',
      },
      {
        label: 'Cost of carry',
        value: '5.24%',
        tooltip: <CostOfCarryTooltip />,
        tooltipDocsLink: DOCS_LINKS.marketCostOfCarry,
        tooltipClassName: 'w-[520px] whitespace-normal p-4',
        tooltipPosition: 'left',
      },
    ],
  },
}

export default meta
type Story = StoryObj<typeof meta>

export const Default: Story = {
  render: (args) => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl">
        <PerpsInstrumentPanel {...args} />
      </div>
    </div>
  ),
}

export const PositiveSession: Story = {
  args: {
    stats: [
      { label: 'plDXY Perp price', value: '1.0066', freshness: 'fresh', freshnessTooltip: 'updated 1m 12s ago' },
      { label: '24h change', value: '+0.21%', tone: 'positive' },
      { label: '24h volume', value: <TokenAmount amount="3.1M" /> },
      { label: 'Long open interest', value: <TokenAmount amount="13.2M" />, tone: 'positive' },
      { label: 'Short open interest', value: <TokenAmount amount="8.2M" />, tone: 'negative' },
      {
        label: 'Pool liquidity',
        value: <TokenAmount amount="8.7M" />,
        tooltip: <PoolLiquidityTooltip />,
        tooltipDocsLink: DOCS_LINKS.poolLiquidity,
        tooltipClassName: 'w-[400px] whitespace-normal p-4',
        tooltipPosition: 'left',
      },
      {
        label: 'Cost of carry',
        value: '4.87%',
        tooltip: <CostOfCarryTooltip />,
        tooltipDocsLink: DOCS_LINKS.marketCostOfCarry,
        tooltipClassName: 'w-[520px] whitespace-normal p-4',
        tooltipPosition: 'left',
      },
    ],
  },
  render: Default.render,
}
