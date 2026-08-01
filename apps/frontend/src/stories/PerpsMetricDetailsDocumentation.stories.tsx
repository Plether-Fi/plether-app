import type { Meta, StoryObj } from '@storybook/react-vite'
import type { ReactNode } from 'react'
import { PerpsAccountPanel } from '../components/PerpsAccountPanel'
import { PerpsInstrumentPanel } from '../components/PerpsInstrumentPanel'
import { TokenAmount } from '../components/ui'
import type { PerpsPosition } from '../hooks'

const USDC = 1_000_000n

const position = {
  exists: true,
  side: 0,
  direction: 'long',
  size: 2_000n * 10n ** 18n,
  entryPrice: 98_300_000n,
  marginUsdc: 400n * USDC,
  unrealizedPnlUsdc: 48_250_000n,
  maintenanceMarginUsdc: 20n * USDC,
  liquidatable: false,
  estimatedNotionalUsdc: 1_999_920_000n,
  entryNotionalUsdc: 2_000_000_000n,
  dxyExposureUsdc: 2_069_380_000n,
  displayDxyPrice: 101_700_000n,
  liquidationPrice: 110_000_000n,
  pendingCarryUsdc: 1_250_000n,
} satisfies PerpsPosition

function MetricCard({
  label,
  value,
  detail,
  tone = 'default',
}: {
  label: string
  value: ReactNode
  detail?: string
  tone?: 'default' | 'positive'
}) {
  return (
    <div className="border border-brand-border/20 bg-app-bg p-4">
      <div className="text-xs font-medium uppercase tracking-wide text-content-secondary">
        {label}
      </div>
      <div className={`mt-2 text-xl font-semibold ${
        tone === 'positive' ? 'text-positive' : 'text-content-primary'
      }`}>
        {value}
      </div>
      {detail ? <p className="mt-2 text-xs leading-5 text-content-secondary">{detail}</p> : null}
    </div>
  )
}

function DetailsPanel({
  eyebrow,
  title,
  description,
  children,
  footer,
}: {
  eyebrow: string
  title: string
  description: string
  children: ReactNode
  footer: string
}) {
  return (
    <section className="border border-brand-border/30 bg-surface-panel">
      <header className="border-b border-brand-border/20 px-5 py-4">
        <div className="text-xs font-medium uppercase tracking-wide text-brand-peach">
          {eyebrow}
        </div>
        <h2 className="mt-1 text-xl font-semibold text-content-primary">{title}</h2>
        <p className="mt-2 max-w-4xl text-sm leading-6 text-content-secondary">{description}</p>
      </header>
      <div className="p-5">{children}</div>
      <footer className="border-t border-brand-border/20 bg-app-bg/50 px-5 py-3 text-sm leading-5 text-content-secondary">
        {footer}
      </footer>
    </section>
  )
}

function MetricDetailsDocumentation() {
  return <div />
}

const meta: Meta<typeof MetricDetailsDocumentation> = {
  title: 'Documentation/Metric Details',
  component: MetricDetailsDocumentation,
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta
type Story = StoryObj<typeof meta>

export const UnrealizedPnl: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl space-y-5">
        <PerpsAccountPanel
          isConnected
          equityUsdc={1_248_250_000n}
          freeBuyingPowerUsdc={848_250_000n}
          position={position}
        />
        <DetailsPanel
          eyebrow="Current Position · metric guide"
          title="Reading Unrealized PnL"
          description="This is the mark-to-market price movement from the position’s entry price to the current mark. It is an estimate, not settled USDC."
          footer="Settlement turns the final trading result into Margin Account credit, a trader claim, or a debit."
        >
          <div className="grid gap-3 md:grid-cols-3">
            <MetricCard
              label="Current value"
              value={<TokenAmount amount="+48.25" />}
              detail="Displayed in USDC"
              tone="positive"
            />
            <MetricCard
              label="Included"
              value="Price movement"
              detail="Entry price compared with the current mark"
            />
            <MetricCard
              label="Not included here"
              value="Final adjustments"
              detail="Execution fee, VPI or price impact, and pending carry"
            />
          </div>
        </DetailsPanel>
      </div>
    </div>
  ),
}

export const PoolLiquidity: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl space-y-5">
        <PerpsInstrumentPanel
          icon="token"
          name="plDXY Perp"
          description="plDXY Perpetual"
          stats={[
            {
              label: 'plDXY Perp price',
              value: '1.0091',
              freshness: 'fresh',
              freshnessTooltip: 'updated 24s ago',
            },
            { label: '24h change', value: '-0.16%', tone: 'negative' },
            { label: '24h volume', value: <TokenAmount amount="2.4M" /> },
            {
              label: 'Directional limit used',
              directionalLimit: {
                usagePercent: 87,
                side: 'long',
                totalExposure: <TokenAmount amount="8.36M" />,
                netExposure: <TokenAmount amount="3.07M" />,
                limit: <TokenAmount amount="3.53M" />,
              },
            },
            { label: 'Pool liquidity', value: <TokenAmount amount="6.3M" /> },
            { label: 'Cost of carry', value: '5.24%' },
          ]}
        />
        <DetailsPanel
          eyebrow="Market header · metric guide"
          title="What Pool liquidity represents"
          description="Pool liquidity is free HousePool USDC after protected reserves. It is not total HousePool assets, total tranche NAV, or a promise that every LP can withdraw."
          footer="The capacities and minimums move with HousePool liquidity, open interest, risk limits, and the current market state."
        >
          <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
            <MetricCard label="Long capacity" value={<TokenAmount amount="953.33" />} />
            <MetricCard label="Short capacity" value={<TokenAmount amount="4 810.22" />} />
            <MetricCard label="Minimum order size" value={<TokenAmount amount="103.18" />} />
            <MetricCard label="Minimum new position" value={<TokenAmount amount="1 031.8" />} />
          </div>
        </DetailsPanel>
      </div>
    </div>
  ),
}
