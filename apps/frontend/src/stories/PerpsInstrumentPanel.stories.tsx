import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, userEvent, within } from 'storybook/test'
import {
  PerpsInstrumentPanel,
  type PerpsDirectionalLimitDetails,
  type PerpsInstrumentStat,
} from '../components/PerpsInstrumentPanel'
import { INFO_TOOLTIP_PANEL_CLASS_NAME, TokenAmount } from '../components/ui'
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

const LONG_HEAVY_87: PerpsDirectionalLimitDetails = {
  usagePercent: 87,
  side: 'long',
  netExposure: <TokenAmount amount="307.2M" />,
  limit: <TokenAmount amount="353.1M" />,
}

function directionalLimitStat(details: PerpsDirectionalLimitDetails): PerpsInstrumentStat {
  return {
    label: 'Directional limit used',
    directionalLimit: details,
  }
}

function instrumentStats({
  directionalLimit = LONG_HEAVY_87,
  price = '1.0091',
  priceChange = '-0.16%',
  priceChangeTone = 'negative',
  freshness = 'fresh',
  freshnessTooltip = 'updated 24s ago',
  volume = '2.4M',
  liquidity = '6.3M',
  costOfCarry = '5.24%',
}: {
  directionalLimit?: PerpsDirectionalLimitDetails
  price?: string
  priceChange?: string
  priceChangeTone?: 'default' | 'positive' | 'negative'
  freshness?: 'fresh' | 'market-closed' | 'stale' | 'checking'
  freshnessTooltip?: string
  volume?: string
  liquidity?: string
  costOfCarry?: string
} = {}): PerpsInstrumentStat[] {
  return [
    { label: 'plDXY Perp price', value: price, freshness, freshnessTooltip },
    { label: '24h change', value: priceChange, tone: priceChangeTone },
    { label: '24h volume', value: <TokenAmount amount={volume} /> },
    directionalLimitStat(directionalLimit),
    {
      label: 'Pool liquidity',
      value: <TokenAmount amount={liquidity} />,
      tooltip: <PoolLiquidityTooltip />,
      tooltipDocsLink: DOCS_LINKS.poolLiquidity,
      tooltipClassName: INFO_TOOLTIP_PANEL_CLASS_NAME,
      tooltipPosition: 'left',
    },
    {
      label: 'Cost of carry',
      value: costOfCarry,
      tooltip: <CostOfCarryTooltip />,
      tooltipDocsLink: DOCS_LINKS.marketCostOfCarry,
      tooltipClassName: INFO_TOOLTIP_PANEL_CLASS_NAME,
      tooltipPosition: 'left',
    },
  ]
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
    stats: instrumentStats(),
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

export const WideDesktop: Story = {
  args: {
    description: 'Dollar Index Perpetual',
    stats: instrumentStats({
      directionalLimit: {
        usagePercent: 98,
        side: 'long',
        netExposure: <TokenAmount amount="346M" />,
        limit: <TokenAmount amount="353.1M" />,
      },
      price: '1.0153',
      priceChange: '-0.17%',
      volume: '136.4M',
      liquidity: '25.3M',
      costOfCarry: '5%',
    }),
  },
  render: (args) => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <PerpsInstrumentPanel {...args} />
    </div>
  ),
}

export const HoverOverlayOnChart: Story = {
  args: WideDesktop.args,
  render: (args) => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <PerpsInstrumentPanel {...args} />
      <div
        data-testid="chart-surface"
        className="mt-6 h-64 overflow-hidden border border-brand-border/30 bg-surface-panel p-4"
      >
        <div className="text-xs font-medium text-content-secondary">DXY basket chart</div>
        <div className="mt-4 grid h-44 grid-rows-4 border-x border-brand-border/15">
          {[0, 1, 2, 3].map((line) => (
            <div key={line} className="border-t border-brand-border/15" />
          ))}
        </div>
      </div>
    </div>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const trigger = canvas.getByRole('button', { name: 'Directional limit used details' })
    const chart = canvas.getByTestId('chart-surface')
    const panel = trigger.closest('section')
    const detailsId = trigger.getAttribute('aria-controls')
    const details = detailsId ? canvasElement.ownerDocument.getElementById(detailsId) : null
    const overlay = details?.parentElement

    if (!panel || !overlay) throw new Error('Instrument panel overlay not found')

    const panelHeight = panel.getBoundingClientRect().height
    const chartTop = chart.getBoundingClientRect().top

    await userEvent.hover(trigger)

    await expect(trigger).toHaveAttribute('aria-expanded', 'true')
    expect(panel.getBoundingClientRect().height).toBe(panelHeight)
    expect(chart.getBoundingClientRect().top).toBe(chartTop)

    const panelRect = panel.getBoundingClientRect()
    const overlayRect = overlay.getBoundingClientRect()
    const panelStyle = getComputedStyle(panel)
    const overlayStyle = getComputedStyle(overlay)

    expect(overlayRect.left).toBe(panelRect.left)
    expect(overlayRect.right).toBe(panelRect.right)
    expect(overlayRect.width).toBe(panelRect.width)
    expect(overlayRect.top).toBe(panelRect.bottom - 1)
    expect(overlayStyle.backgroundColor).toBe(panelStyle.backgroundColor)
    expect(overlayStyle.borderLeftWidth).toBe(panelStyle.borderLeftWidth)
    expect(overlayStyle.borderRightWidth).toBe(panelStyle.borderRightWidth)
    expect(overlayStyle.borderBottomWidth).toBe(panelStyle.borderBottomWidth)
  },
}

export const PositiveSession: Story = {
  args: {
    stats: instrumentStats({
      directionalLimit: {
        usagePercent: 58,
        side: 'long',
        netExposure: <TokenAmount amount="204.8M" />,
        limit: <TokenAmount amount="353.1M" />,
      },
      price: '1.0066',
      priceChange: '+0.21%',
      priceChangeTone: 'positive',
      freshnessTooltip: 'updated 1m 12s ago',
      volume: '3.1M',
      liquidity: '8.7M',
      costOfCarry: '4.87%',
    }),
  },
  render: Default.render,
}

export const StaleOracle: Story = {
  args: {
    stats: instrumentStats({
      freshness: 'stale',
      freshnessTooltip: 'last validated update 4m 38s ago',
    }),
  },
  render: Default.render,
}

export const DirectionalLimitDetailsVisible: Story = {
  args: {
    directionalLimitDetailsExpanded: true,
  },
  render: Default.render,
}

export const ShortHeavy: Story = {
  args: {
    stats: instrumentStats({
      directionalLimit: {
        usagePercent: 62,
        side: 'short',
        netExposure: <TokenAmount amount="218.9M" />,
        limit: <TokenAmount amount="353.1M" />,
      },
    }),
  },
  render: Default.render,
}

export const Balanced: Story = {
  args: {
    stats: instrumentStats({
      directionalLimit: {
        usagePercent: 0,
        side: 'balanced',
        netExposure: <TokenAmount amount="0" />,
        limit: <TokenAmount amount="353.1M" />,
      },
    }),
  },
  render: Default.render,
}

export const NearLimit: Story = {
  args: {
    stats: instrumentStats({
      directionalLimit: {
        usagePercent: 94,
        side: 'long',
        netExposure: <TokenAmount amount="331.9M" />,
        limit: <TokenAmount amount="353.1M" />,
      },
    }),
  },
  render: Default.render,
}

export const LimitReached: Story = {
  args: {
    stats: instrumentStats({
      directionalLimit: {
        usagePercent: 100,
        side: 'long',
        netExposure: <TokenAmount amount="353.1M" />,
        limit: <TokenAmount amount="353.1M" />,
      },
    }),
  },
  render: Default.render,
}

export const LimitReachedDetailsVisible: Story = {
  args: {
    directionalLimitDetailsExpanded: true,
    stats: instrumentStats({
      directionalLimit: {
        usagePercent: 100,
        side: 'long',
        netExposure: <TokenAmount amount="353.1M" />,
        limit: <TokenAmount amount="353.1M" />,
      },
    }),
  },
  render: Default.render,
}

export const LimitExceeded: Story = {
  args: {
    stats: instrumentStats({
      directionalLimit: {
        usagePercent: 108,
        side: 'long',
        netExposure: <TokenAmount amount="381.3M" />,
        limit: <TokenAmount amount="353.1M" />,
      },
    }),
  },
  render: Default.render,
}

export const LimitExceededDetailsVisible: Story = {
  args: {
    directionalLimitDetailsExpanded: true,
    stats: instrumentStats({
      directionalLimit: {
        usagePercent: 108,
        side: 'long',
        netExposure: <TokenAmount amount="381.3M" />,
        limit: <TokenAmount amount="353.1M" />,
      },
    }),
  },
  render: Default.render,
}

export const DirectionalLimitLoading: Story = {
  args: {
    stats: instrumentStats({
      directionalLimit: {
        isLoading: true,
      },
    }),
  },
  render: Default.render,
}

export const PoolLiquidityTooltipVisible: Story = {
  render: Default.render,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    await userEvent.hover(canvas.getByLabelText('Pool liquidity details'))
  },
}

export const CostOfCarryTooltipVisible: Story = {
  render: Default.render,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    await userEvent.hover(canvas.getByLabelText('Cost of carry details'))
  },
}
