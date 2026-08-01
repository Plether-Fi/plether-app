import type { ComponentProps, ReactNode } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { PerpsAccountPanel } from '../components/PerpsAccountPanel'
import { PerpsInstrumentPanel } from '../components/PerpsInstrumentPanel'
import { PerpsMarketStatePanel } from '../components/PerpsMarketStatePanel'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'
import { OperationStateCard } from '../components/PerpsTradingAccountPanel'
import { Button, TokenAmount } from '../components/ui'
import type {
  PerpsOrderHistoryRow,
  PerpsPendingOrder,
  PerpsPosition,
} from '../hooks'

type TicketProps = ComponentProps<typeof PerpsTradeTicket>
type OpenPreviewFixture = NonNullable<TicketProps['openPreviewFixture']>

const USDC = 1_000_000n
const POSITION_UNIT = 10n ** 18n
const ORACLE_PRICE = 98_300_000n
const CURRENT_EXPOSURE = 8_200n
const ADDED_EXPOSURE = 5_000n
const RESULTING_EXPOSURE = CURRENT_EXPOSURE + ADDED_EXPOSURE

const latestBasket = {
  timestamp: 1_700_000_000,
  basketPrice: ORACLE_PRICE.toString(),
  components: [{
    symbol: 'EUR/USD',
    feedSymbol: 'EUR/USD',
    feedId: 'storybook-eur-usd',
    price: ORACLE_PRICE.toString(),
    rawPrice: ORACLE_PRICE.toString(),
    confidence: '100000',
    exponent: -8,
    publishTime: 1_700_000_000,
    inverted: false,
    weightBps: 10_000,
    basePrice: ORACLE_PRICE.toString(),
  }],
  generatedAt: 1_700_000_001,
  source: 'database' as const,
}

const currentLongPosition = {
  exists: true,
  side: 0,
  direction: 'long',
  size: CURRENT_EXPOSURE * POSITION_UNIT,
  entryPrice: 98_750_000n,
  marginUsdc: 1_640n * USDC,
  unrealizedPnlUsdc: 54_250_000n,
  maintenanceMarginUsdc: 80_950_000n,
  liquidatable: false,
  estimatedNotionalUsdc: 8_060n * USDC,
  entryNotionalUsdc: 8_095n * USDC,
  dxyExposureUsdc: CURRENT_EXPOSURE * USDC,
  displayDxyPrice: 101_700_000n,
  liquidationPrice: 112_000_000n,
  pendingCarryUsdc: 4_200_000n,
} satisfies PerpsPosition

const executedLongPosition = {
  ...currentLongPosition,
  size: RESULTING_EXPOSURE * POSITION_UNIT,
  entryPrice: 98_571_970n,
  marginUsdc: 2_619_600_000n,
  unrealizedPnlUsdc: 36_400_000n,
  maintenanceMarginUsdc: 130_120_000n,
  estimatedNotionalUsdc: 12_972_960_000n,
  entryNotionalUsdc: 13_011_500_000n,
  dxyExposureUsdc: RESULTING_EXPOSURE * USDC,
  liquidationPrice: 104_000_000n,
  pendingCarryUsdc: 0n,
} satisfies PerpsPosition

const executedIncreaseHistory = [
  {
    orderId: 72n,
    time: '14:42',
    market: 'plDXY Perp',
    side: 'Long',
    type: 'Increase',
    price: '1.0172',
    size: '5 000',
    status: 'Executed',
    commitTxHash: '0x7200000000000000000000000000000000000000000000000000000000000001',
    revealTxHash: '0x7200000000000000000000000000000000000000000000000000000000000002',
  },
] satisfies PerpsOrderHistoryRow[]

function openingPreviewFixture(): OpenPreviewFixture {
  return {
    valid: true,
    invalidReason: 0,
    failureCategory: 0,
    executionPrice: 98_280_000n,
    sizeDelta: 2_000n * POSITION_UNIT,
    notionalUsdc: 1_966n * USDC,
    marginDeltaUsdc: 393_200_000n,
    vpiUsdc: 1_400_000n,
    executionFeeUsdc: 786_400n,
    tradeCostUsdc: 2_186_400n,
    poolRebatePayoutUsdc: 0n,
    pendingCarryUsdc: 0n,
    initialMarginRequirementUsdc: 393_200_000n,
    maintenanceMarginUsdc: 19_660_000n,
    postSize: 2_000n * POSITION_UNIT,
    postMarginUsdc: 393_200_000n,
    postEntryPrice: 98_280_000n,
    postVpiAccrued: 1_400_000n,
    postUnrealizedPnlUsdc: 0n,
    postEquityUsdc: 391_800_000n,
    postHealthBps: 9_400n,
    postLiquidatable: false,
    hasLiquidationPrice: true,
    liquidationPrice: 104_000_000n,
  }
}

const marketArgs = {
  oraclePriceRaw: ORACLE_PRICE,
  oraclePriceDisplay: '1.0170',
  latestBasket,
  adverseConfidenceMultiplierBps: '2000',
  oracleFreshness: 'fresh' as const,
  oracleFreshnessTooltip: 'validated oracle basket updated 18s ago',
  availableToTradeRaw: 18_420n * USDC,
  availableToTradeAmount: '18 420',
  portfolioValueRaw: 18_420n * USDC,
  withdrawableUsdcRaw: 18_420n * USDC,
  walletUsdcRaw: 25_000n * USDC,
  longOpenCapacityUsdc: 250_000n * USDC,
  shortOpenCapacityUsdc: 250_000n * USDC,
  maintenanceMarginBps: 100n,
  executionFeeBps: 4n,
}

function DocumentationFrame({
  children,
  maxWidth = 'max-w-7xl',
}: {
  children: ReactNode
  maxWidth?: string
}) {
  return (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className={`mx-auto ${maxWidth}`}>{children}</div>
    </div>
  )
}

function OpenIncreaseDocumentation() {
  return null
}

const meta: Meta<typeof OpenIncreaseDocumentation> = {
  title: 'Documentation/Open or Increase Position',
  component: OpenIncreaseDocumentation,
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta
type Story = StoryObj<typeof meta>

function MarketHeader() {
  return (
    <PerpsInstrumentPanel
      stats={[
        {
          label: 'plDXY Perp price',
          value: '1.0170',
          freshness: 'fresh',
          freshnessTooltip: 'validated oracle basket updated 18s ago',
        },
        { label: 'Market state', value: 'Open', tone: 'positive' },
        { label: 'Oracle state', value: 'Fresh', tone: 'positive' },
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
      ]}
    />
  )
}

export const MarketAndAccountReadiness: Story = {
  render: () => (
    <DocumentationFrame>
      <div className="space-y-5">
        <MarketHeader />
        <PerpsMarketStatePanel
          currentPhase="open"
          currentDuration="1d 12h"
          nextPhase="close-only"
          nextDuration="3h"
        />
        <div className="grid gap-5 lg:grid-cols-[minmax(0,1fr)_420px]">
          <PerpsAccountPanel
            isConnected
            equityUsdc={18_420n * USDC}
            freeBuyingPowerUsdc={18_420n * USDC}
          />
          <PerpsTradeTicket
            {...marketArgs}
            initialDirection="long"
            initialSize="2 000"
            currentPositionAmount="0"
            openPreviewFixture={openingPreviewFixture()}
          />
        </div>
      </div>
    </DocumentationFrame>
  ),
}

export const OpeningPreview: Story = {
  render: () => (
    <DocumentationFrame maxWidth="max-w-md">
      <PerpsTradeTicket
        {...marketArgs}
        initialLifecycleState="preview"
        initialReviewOpen
        initialDirection="long"
        initialSize="2 000"
        currentPositionAmount="0"
        openPreviewFixture={openingPreviewFixture()}
      />
    </DocumentationFrame>
  ),
}

function ComparisonMetric({
  label,
  value,
  tone = 'default',
}: {
  label: string
  value: ReactNode
  tone?: 'default' | 'positive' | 'accent'
}) {
  const toneClass = tone === 'positive'
    ? 'text-positive'
    : tone === 'accent'
      ? 'text-[#FFAB96]'
      : 'text-content-primary'

  return (
    <div className="border-t border-brand-border/15 pt-4 first:border-t-0 first:pt-0">
      <dt className="text-xs font-medium uppercase tracking-wide text-content-secondary">{label}</dt>
      <dd className={`mt-2 text-xl font-semibold ${toneClass}`}>{value}</dd>
    </div>
  )
}

function PositionSnapshot({
  label,
  badge,
  badgeTone,
  exposure,
  averageEntry,
  margin,
  leverage,
  liquidationPrice,
}: {
  label: string
  badge: string
  badgeTone: 'current' | 'projected'
  exposure: string
  averageEntry: string
  margin: string
  leverage: string
  liquidationPrice: string
}) {
  const badgeClass = badgeTone === 'projected'
    ? 'border-positive/40 bg-positive/10 text-positive'
    : 'border-brand-border/30 bg-app-bg text-content-secondary'

  return (
    <section className="border border-brand-border/30 bg-surface-panel">
      <header className="flex items-center justify-between gap-4 border-b border-brand-border/20 px-5 py-4">
        <div>
          <div className="text-xs font-medium uppercase tracking-wide text-content-secondary">{label}</div>
          <h2 className="mt-1 text-xl font-semibold text-content-primary">Long plDXY Perp</h2>
        </div>
        <span className={`border px-3 py-1 text-xs font-semibold uppercase tracking-wide ${badgeClass}`}>
          {badge}
        </span>
      </header>
      <dl className="grid grid-cols-2 gap-x-6 gap-y-4 px-5 py-5">
        <ComparisonMetric label="Total exposure" value={<TokenAmount amount={exposure} />} tone="positive" />
        <ComparisonMetric label="Average entry price" value={averageEntry} />
        <ComparisonMetric label="Position margin" value={<TokenAmount amount={margin} />} />
        <ComparisonMetric label="Leverage" value={leverage} />
        <ComparisonMetric label="Liquidation price" value={liquidationPrice} tone="accent" />
      </dl>
    </section>
  )
}

function PreviewSummaryRow({
  label,
  value,
  tone = 'default',
}: {
  label: string
  value: ReactNode
  tone?: 'default' | 'positive' | 'accent'
}) {
  const toneClass = tone === 'positive'
    ? 'text-positive'
    : tone === 'accent'
      ? 'text-[#FFAB96]'
      : 'text-content-primary'

  return (
    <div className="flex min-h-7 items-center justify-between gap-5 text-sm">
      <dt className="text-content-secondary">{label}</dt>
      <dd className={`text-right font-semibold ${toneClass}`}>{value}</dd>
    </div>
  )
}

export const IncreaseProjectionComparison: Story = {
  render: () => (
    <DocumentationFrame>
      <section className="overflow-hidden border border-brand-border/30 bg-app-bg">
        <header className="flex flex-col gap-4 border-b border-brand-border/20 bg-surface-panel px-6 py-5 md:flex-row md:items-center md:justify-between">
          <div>
            <div className="text-xs font-medium uppercase tracking-wide text-[#FFAB96]">Increase preview</div>
            <h1 className="mt-1 text-2xl font-semibold text-content-primary">
              Add <TokenAmount amount="5 000" /> of Long exposure
            </h1>
            <p className="mt-2 text-sm text-content-secondary">
              The current position remains unchanged until the committed order executes.
            </p>
          </div>
          <div className="flex items-center gap-3">
            <span className="border border-positive/40 bg-positive/10 px-3 py-1.5 text-xs font-semibold uppercase tracking-wide text-positive">
              Valid preview
            </span>
            <span className="text-sm font-semibold text-content-secondary">Market state: Open</span>
          </div>
        </header>

        <div className="grid gap-5 p-5 lg:grid-cols-2">
          <PositionSnapshot
            label="Before execution"
            badge="Current position"
            badgeTone="current"
            exposure="8 200"
            averageEntry="1.0125"
            margin="1 640"
            leverage="4.94x"
            liquidationPrice="0.8800"
          />
          <PositionSnapshot
            label="If this order executes"
            badge="Projected result"
            badgeTone="projected"
            exposure="13 200"
            averageEntry="1.0143"
            margin="2 619.6"
            leverage="4.95x"
            liquidationPrice="0.9600"
          />
        </div>

        <div className="grid gap-5 border-t border-brand-border/20 p-5 lg:grid-cols-2">
          <section className="border border-brand-border/30 bg-surface-panel p-5">
            <div className="mb-4 text-xs font-medium uppercase tracking-wide text-content-secondary">
              Order and price limit
            </div>
            <dl className="space-y-2">
              <PreviewSummaryRow label="Direction" value="Long" tone="positive" />
              <PreviewSummaryRow label="Exposure being added" value={<TokenAmount amount="5 000" />} />
              <PreviewSummaryRow label="Estimated execution price" value="1.0172" />
              <PreviewSummaryRow label="Max slippage" value="0.1%" />
              <PreviewSummaryRow label="Execution limit" value="1.0180" tone="accent" />
              <PreviewSummaryRow label="Submitted margin" value={<TokenAmount amount="983" />} />
            </dl>
          </section>

          <section className="border border-brand-border/30 bg-surface-panel p-5">
            <div className="mb-4 text-xs font-medium uppercase tracking-wide text-content-secondary">
              Estimated costs and reservation
            </div>
            <dl className="space-y-2">
              <PreviewSummaryRow label="Protocol execution fee" value={<TokenAmount amount="2.0" />} />
              <PreviewSummaryRow label="VPI / Price impact" value={<TokenAmount amount="1.4" />} />
              <PreviewSummaryRow label="Adverse confidence adjustment" value="~0.0207%" />
              <PreviewSummaryRow label="Pending carry checkpoint" value={<TokenAmount amount="4.2" />} />
              <PreviewSummaryRow label="Execution reward" value={<TokenAmount amount="0.2" />} />
              <PreviewSummaryRow label="Reserved from Available to Trade" value={<TokenAmount amount="983.2" />} tone="accent" />
            </dl>
          </section>
        </div>
      </section>
    </DocumentationFrame>
  ),
}

export const PendingRevealWithManualFinalization: Story = {
  render: () => {
    const nowSeconds = Math.floor(Date.now() / 1_000)
    const pendingOrder = {
      orderId: 72n,
      side: 0,
      direction: 'long',
      sizeDelta: ADDED_EXPOSURE * POSITION_UNIT,
      marginDeltaUsdc: 983n * USDC,
      acceptablePrice: 98_200_000n,
      isReduceOnly: false,
      status: 1,
      estimatedNotionalUsdc: 4_915n * USDC,
      commitTime: BigInt(nowSeconds - 42),
      expiryTime: BigInt(nowSeconds + 2_958),
    } satisfies PerpsPendingOrder

    return (
      <DocumentationFrame>
        <div className="grid gap-5 lg:grid-cols-[380px_minmax(0,1fr)]">
          <OperationStateCard
            title="Finalize Trade"
            stage="Pending reveal"
            message="Automatic keeper processing has not arrived. You can submit the oracle data needed to process this same binding order under its original FIFO and price-limit rules."
            tone="pending"
            identifierLabel="Order ID"
            identifier="72 · Expires in 49m 18s"
            action={<Button className="w-full">Finalize Trade</Button>}
          />
          <PerpsAccountPanel
            initialTab="openOrders"
            isConnected
            pendingOrders={[pendingOrder]}
          />
        </div>
      </DocumentationFrame>
    )
  },
}

export const ExecutedPositionAndOrderHistory: Story = {
  render: () => (
    <DocumentationFrame>
      <div className="space-y-5">
        <OperationStateCard
          title="Increase executed"
          stage="Delayed order result"
          message="Order 72 added 5 000 USDC of Long exposure. The combined position and matching terminal order record now show the same execution result."
          tone="success"
          identifierLabel="Execution"
          identifier="1.0172 · 5 000 USDC · Order 72"
        />
        <PerpsAccountPanel
          isConnected
          equityUsdc={2_659_400_000n}
          freeBuyingPowerUsdc={36_400_000n}
          position={executedLongPosition}
        />
        <PerpsAccountPanel
          initialTab="orderHistory"
          isConnected
          orderHistory={executedIncreaseHistory}
        />
      </div>
    </DocumentationFrame>
  ),
}
