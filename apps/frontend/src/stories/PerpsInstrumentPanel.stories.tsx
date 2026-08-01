import type { ReactNode } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, userEvent, waitFor, within } from 'storybook/test'
import type { BasketComponentPrice } from '../api'
import { DxyBasketComponentsRail } from '../components/DxyBasketComponentsRail'
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
  totalExposure: <TokenAmount amount="882.9M" />,
  netExposure: <TokenAmount amount="307.2M" />,
  limit: <TokenAmount amount="353.1M" />,
}

const BASKET_DETAILS_NOW = Date.UTC(2026, 7, 1, 10, 0, 0) / 1000

const BASKET_COMPONENTS: BasketComponentPrice[] = [
  {
    symbol: 'EUR/USD',
    feedSymbol: 'EUR/USD',
    feedId: 'storybook-eur-usd',
    price: '115300000',
    rawPrice: '115300000',
    confidence: '40355',
    exponent: -8,
    publishTime: BASKET_DETAILS_NOW - 24,
    inverted: false,
    weightBps: 5760,
    basePrice: '117500000',
  },
  {
    symbol: 'JPY/USD',
    feedSymbol: 'USD/JPY',
    feedId: 'storybook-usd-jpy',
    price: '638000',
    rawPrice: '15674000000',
    confidence: '223',
    exponent: -8,
    publishTime: BASKET_DETAILS_NOW - 21,
    inverted: true,
    weightBps: 1360,
    basePrice: '638000',
  },
  {
    symbol: 'GBP/USD',
    feedSymbol: 'GBP/USD',
    feedId: 'storybook-gbp-usd',
    price: '134800000',
    rawPrice: '134800000',
    confidence: '47180',
    exponent: -8,
    publishTime: BASKET_DETAILS_NOW - 18,
    inverted: false,
    weightBps: 1190,
    basePrice: '134480000',
  },
  {
    symbol: 'CAD/USD',
    feedSymbol: 'USD/CAD',
    feedId: 'storybook-usd-cad',
    price: '71300000',
    rawPrice: '140250000',
    confidence: '24955',
    exponent: -8,
    publishTime: BASKET_DETAILS_NOW - 19,
    inverted: true,
    weightBps: 910,
    basePrice: '72880000',
  },
  {
    symbol: 'SEK/USD',
    feedSymbol: 'USD/SEK',
    feedId: 'storybook-usd-sek',
    price: '10500000',
    rawPrice: '952380000',
    confidence: '3675',
    exponent: -8,
    publishTime: BASKET_DETAILS_NOW - 16,
    inverted: true,
    weightBps: 420,
    basePrice: '10860000',
  },
  {
    symbol: 'CHF/USD',
    feedSymbol: 'USD/CHF',
    feedId: 'storybook-usd-chf',
    price: '123800000',
    rawPrice: '80775000',
    confidence: '43330',
    exponent: -8,
    publishTime: BASKET_DETAILS_NOW - 22,
    inverted: true,
    weightBps: 360,
    basePrice: '126100000',
  },
]

const BASKET_PRICE_CHANGES: Partial<Record<string, number>> = {
  'storybook-eur-usd': 0.0018,
  'storybook-usd-jpy': 0.0161,
  'storybook-gbp-usd': 0.0023,
  'storybook-usd-cad': -0.0003,
  'storybook-usd-sek': 0.0014,
  'storybook-usd-chf': 0.0009,
}

function BasketDetailsRail() {
  return (
    <DxyBasketComponentsRail
      components={BASKET_COMPONENTS}
      priceChanges={BASKET_PRICE_CHANGES}
      nowSeconds={BASKET_DETAILS_NOW}
    />
  )
}

function directionalLimitStat(details: PerpsDirectionalLimitDetails): PerpsInstrumentStat {
  return {
    label: 'Directional limit used',
    directionalLimit: details,
  }
}

function instrumentStats({
  directionalLimit = LONG_HEAVY_87,
  priceDetails = <BasketDetailsRail />,
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
  priceDetails?: ReactNode
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
    {
      label: 'plDXY Perp price',
      value: price,
      freshness,
      freshnessTooltip,
      hoverDetails: priceDetails,
    },
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
        totalExposure: <TokenAmount amount="875.8M" />,
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

export const PriceDetailsVisible: Story = {
  args: {
    priceDetailsExpanded: true,
  },
  render: Default.render,
}

export const PriceDetailsLoading: Story = {
  args: {
    priceDetailsExpanded: true,
    stats: instrumentStats({
      priceDetails: (
        <DxyBasketComponentsRail
          isLoading
          nowSeconds={BASKET_DETAILS_NOW}
        />
      ),
    }),
  },
  render: Default.render,
}

export const PriceDetailsUnavailable: Story = {
  args: {
    priceDetailsExpanded: true,
    stats: instrumentStats({
      priceDetails: (
        <DxyBasketComponentsRail
          isError
          nowSeconds={BASKET_DETAILS_NOW}
        />
      ),
    }),
  },
  render: Default.render,
}

export const PriceDetailsNarrowOverflow: Story = {
  args: {
    priceDetailsExpanded: true,
  },
  render: (args) => (
    <div className="min-h-screen bg-app-bg p-3">
      <div className="w-[360px] max-w-full">
        <PerpsInstrumentPanel {...args} />
      </div>
    </div>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const rail = canvas.getByRole('list', { name: 'Basket components' })

    await expect(rail).toHaveAttribute('tabindex', '0')
    await expect(within(rail).getAllByRole('listitem')).toHaveLength(6)
    expect(rail.scrollWidth).toBeGreaterThan(rail.clientWidth)

    const nextButton = await canvas.findByRole('button', { name: 'Next basket components' })
    const initialScrollLeft = rail.scrollLeft
    await userEvent.click(nextButton)
    await waitFor(() => {
      expect(rail.scrollLeft).toBeGreaterThan(initialScrollLeft)
    })
    const previousButton = await canvas.findByRole('button', { name: 'Previous basket components' })
    await expect(previousButton).toBeVisible()
  },
}

export const PriceHoverOverlayOnChart: Story = {
  args: WideDesktop.args,
  render: (args) => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <PerpsInstrumentPanel {...args} />
      <div
        data-testid="price-chart-surface"
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
    const trigger = canvas.getByRole('button', { name: 'plDXY Perp price basket components' })
    const chart = canvas.getByTestId('price-chart-surface')
    const panel = trigger.closest('section')
    const detailsId = trigger.getAttribute('aria-controls')
    const details = detailsId ? canvasElement.ownerDocument.getElementById(detailsId) : null
    const overlay = details?.parentElement

    if (!panel || !overlay) throw new Error('Price details overlay not found')

    const panelHeight = panel.getBoundingClientRect().height
    const chartTop = chart.getBoundingClientRect().top

    await userEvent.hover(trigger)

    await expect(trigger).toHaveAttribute('aria-expanded', 'true')
    const rail = await canvas.findByRole('list', { name: 'Basket components' })
    await expect(within(rail).getAllByRole('listitem')).toHaveLength(6)
    await waitFor(() => {
      expect(overlay.getBoundingClientRect().height).toBeGreaterThan(70)
    })

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
    expect(overlayRect.bottom).toBeGreaterThan(chartTop)
    expect(overlayStyle.backgroundColor).toBe(panelStyle.backgroundColor)
    expect(overlayStyle.borderLeftWidth).toBe(panelStyle.borderLeftWidth)
    expect(overlayStyle.borderRightWidth).toBe(panelStyle.borderRightWidth)
    expect(overlayStyle.borderBottomWidth).toBe(panelStyle.borderBottomWidth)
    expect(overlayStyle.boxShadow).not.toBe('none')
  },
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
    await expect(canvas.getByText('Total LONG exposure')).toBeVisible()
    await expect(canvas.getByText('Net LONG exposure')).toBeVisible()
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

    const netMetric = canvas.getByText('Net LONG exposure').parentElement
    const metricsRow = netMetric?.parentElement
    if (!netMetric || !metricsRow) throw new Error('Directional limit metrics not found')

    const netMetricRect = netMetric.getBoundingClientRect()
    const metricsRowRect = metricsRow.getBoundingClientRect()
    const netMetricCenter = netMetricRect.left + netMetricRect.width / 2
    const metricsRowCenter = metricsRowRect.left + metricsRowRect.width / 2
    expect(Math.abs(netMetricCenter - metricsRowCenter)).toBeLessThan(1)
  },
}

export const PositiveSession: Story = {
  args: {
    stats: instrumentStats({
      directionalLimit: {
        usagePercent: 58,
        side: 'long',
        totalExposure: <TokenAmount amount="734.6M" />,
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
        totalExposure: <TokenAmount amount="580.8M" />,
        netExposure: <TokenAmount amount="225.4M" />,
        limit: <TokenAmount amount="363.3M" />,
      },
    }),
  },
  render: Default.render,
}

export const ShortHeavyDetailsVisible: Story = {
  args: {
    ...ShortHeavy.args,
    directionalLimitDetailsExpanded: true,
  },
  render: Default.render,
}

export const Balanced: Story = {
  args: {
    stats: instrumentStats({
      directionalLimit: {
        usagePercent: 0,
        side: 'balanced',
        totalExposure: <TokenAmount amount="529.8M" />,
        netExposure: <TokenAmount amount="0" />,
        limit: <TokenAmount amount="353.1M" />,
      },
    }),
  },
  render: Default.render,
}

export const BalancedDetailsVisible: Story = {
  args: {
    ...Balanced.args,
    directionalLimitDetailsExpanded: true,
  },
  render: Default.render,
}

export const NearLimit: Story = {
  args: {
    stats: instrumentStats({
      directionalLimit: {
        usagePercent: 94,
        side: 'long',
        totalExposure: <TokenAmount amount="861.7M" />,
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
        totalExposure: <TokenAmount amount="882.9M" />,
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
        totalExposure: <TokenAmount amount="882.9M" />,
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
        totalExposure: <TokenAmount amount="911.1M" />,
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
        totalExposure: <TokenAmount amount="911.1M" />,
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
