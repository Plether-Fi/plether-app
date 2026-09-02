import type { ComponentProps } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, within } from 'storybook/test'
import { PerpsMarketStatePanel } from '../components/PerpsMarketStatePanel'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'
import {
  PerpsIdentityContext,
  type PerpsIdentityContextValue,
} from '../perps-aa'

type TradingRegime = 'fad-only' | 'live' | 'oracle-frozen'
type TicketProps = ComponentProps<typeof PerpsTradeTicket>
type ClosePreviewFixture = NonNullable<TicketProps['closePreviewFixture']>

interface TradingRegimeStoryProps {
  regime: TradingRegime
}

const USDC = 1_000_000n
const POSITION_SIZE = 1_000n * 10n ** 18n
const REDUCE_SIZE = 500n * 10n ** 18n
const ORACLE_PRICE = 100_000_000n
const STORY_ADDRESS = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'

const STORY_IDENTITY: PerpsIdentityContextValue = {
  status: 'ready',
  ownerAddress: STORY_ADDRESS,
  accountAddress: STORY_ADDRESS,
  chainId: 421614,
  isAaManifestConfigured: false,
  sponsorshipEnabled: false,
  manifest: null,
  identity: null,
  proposedIdentity: null,
  changedIdentityFields: [],
  error: null,
  confirmIdentityAfterContinuityCheck: () => false,
  reloadIdentity: () => undefined,
}

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

const currentPosition = {
  exists: true,
  side: 0,
  direction: 'long' as const,
  size: POSITION_SIZE,
  entryPrice: 99_500_000n,
  marginUsdc: 250n * USDC,
  unrealizedPnlUsdc: 5n * USDC,
  maintenanceMarginUsdc: 10n * USDC,
  liquidatable: false,
  estimatedNotionalUsdc: 1_000n * USDC,
  entryNotionalUsdc: 995n * USDC,
  dxyExposureUsdc: 1_000n * USDC,
}

function closePreviewFixture({
  executionPrice,
  frozenSpreadUsdc = 0n,
  frozenSpreadPaidUsdc = 0n,
  frozenSpreadWaivedUsdc = 0n,
}: {
  executionPrice: bigint
  frozenSpreadUsdc?: bigint
  frozenSpreadPaidUsdc?: bigint
  frozenSpreadWaivedUsdc?: bigint
}): ClosePreviewFixture {
  return {
    valid: true,
    invalidReason: 0,
    executionPrice,
    sizeDelta: REDUCE_SIZE,
    realizedPnlUsdc: 2_500_000n,
    vpiDeltaUsdc: -750_000n,
    vpiUsdc: 0n,
    executionFeeUsdc: 200_000n,
    remainingSize: POSITION_SIZE - REDUCE_SIZE,
    remainingMargin: 125n * USDC,
    frozenSpreadUsdc,
    frozenSpreadPaidUsdc,
    frozenSpreadWaivedUsdc,
  }
}

const liveClosePreview = closePreviewFixture({
  executionPrice: 100_020_000n,
})

const frozenClosePreview = closePreviewFixture({
  executionPrice: ORACLE_PRICE,
  frozenSpreadUsdc: 2_500_000n,
  frozenSpreadPaidUsdc: 2_500_000n,
})

const REGIME_CONFIG = {
  live: {
    title: 'Live market close',
    description: 'Oracle is live. The close keeps adverse confidence pricing and has no frozen spread.',
    marketPhase: 'open' as const,
    marketCurrentDuration: '1d 12h',
    nextPhase: 'close-only' as const,
    nextDuration: '3h',
    oracleFrozen: false,
    oracleFreshness: 'fresh' as const,
    oracleFreshnessTooltip: 'live oracle basket updated 18s ago',
    closePreviewFixture: liveClosePreview,
  },
  'fad-only': {
    title: 'FAD-only close',
    description: 'Market is close-only, but the oracle is not frozen. Adverse confidence remains applied.',
    marketPhase: 'close-only' as const,
    marketCurrentDuration: '3h',
    nextPhase: 'open' as const,
    nextDuration: '1d',
    oracleFrozen: false,
    oracleFreshness: 'fresh' as const,
    oracleFreshnessTooltip: 'live oracle basket updated 18s ago',
    closePreviewFixture: liveClosePreview,
  },
  'oracle-frozen': {
    title: 'Oracle-frozen close',
    description: 'Market is close-only and the oracle is frozen. Adverse confidence is waived and the fixed spread is shown.',
    marketPhase: 'close-only' as const,
    marketCurrentDuration: '1d 8h',
    nextPhase: 'open' as const,
    nextDuration: '1d',
    oracleFrozen: true,
    oracleFreshness: 'market-closed' as const,
    oracleFreshnessTooltip: 'using the validated stored basket while the oracle is frozen',
    closePreviewFixture: frozenClosePreview,
  },
} satisfies Record<TradingRegime, {
  title: string
  description: string
  marketPhase: 'close-only' | 'open'
  marketCurrentDuration: string
  nextPhase: 'close-only' | 'open'
  nextDuration: string
  oracleFrozen: boolean
  oracleFreshness: NonNullable<TicketProps['oracleFreshness']>
  oracleFreshnessTooltip: string
  closePreviewFixture: ClosePreviewFixture
}>

function RegimePanel({ regime }: TradingRegimeStoryProps) {
  const config = REGIME_CONFIG[regime]

  return (
    <PerpsIdentityContext.Provider value={STORY_IDENTITY}>
      <section className="min-w-0">
        <div className="mb-3 border border-brand-border/30 bg-surface-panel p-4">
          <h2 className="text-lg font-semibold text-content-primary">{config.title}</h2>
          <p className="mt-1 text-sm leading-5 text-content-secondary">{config.description}</p>
        </div>
        <PerpsMarketStatePanel
          currentPhase={config.marketPhase}
          currentDuration={config.marketCurrentDuration}
          nextPhase={config.nextPhase}
          nextDuration={config.nextDuration}
        />
        <PerpsTradeTicket
          initialDirection="short"
          initialOrderQuantity="500"
          oraclePriceRaw={ORACLE_PRICE}
          oraclePriceDisplay="1.0000"
          latestBasket={latestBasket}
          adverseConfidenceMultiplierBps="2000"
          oracleFrozen={config.oracleFrozen}
          closePreviewFixture={config.closePreviewFixture}
          oracleFreshness={config.oracleFreshness}
          oracleFreshnessTooltip={config.oracleFreshnessTooltip}
          availableToTradeRaw={1_500n * USDC}
          availableToTradeAmount="1 500"
          portfolioValueRaw={1_255n * USDC}
          withdrawableUsdcRaw={1_000n * USDC}
          walletUsdcRaw={2_000n * USDC}
          currentPosition={currentPosition}
          longOpenCapacityUsdc={250_000n * USDC}
          shortOpenCapacityUsdc={250_000n * USDC}
          maintenanceMarginBps={100n}
          initialMarginBps={200n}
          executionFeeBps={4n}
          marketPhase={config.marketPhase}
          marketCurrentDuration={config.marketCurrentDuration}
        />
      </section>
    </PerpsIdentityContext.Provider>
  )
}

function TradingRegimeStory({ regime }: TradingRegimeStoryProps) {
  return (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-md">
        <RegimePanel regime={regime} />
      </div>
    </div>
  )
}

const meta: Meta<typeof TradingRegimeStory> = {
  title: 'Perps/Trading Regime Comparison',
  component: TradingRegimeStory,
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',
  },
  argTypes: {
    regime: {
      control: 'select',
      options: ['live', 'fad-only', 'oracle-frozen'],
    },
  },
}

export default meta
type Story = StoryObj<typeof meta>

export const LiveMarketClose: Story = {
  args: {
    regime: 'live',
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    await expect(canvas.getByText('~0.0200%')).toBeInTheDocument()
    await expect(canvas.getByText('0.1%')).toBeInTheDocument()
    await expect(canvas.queryByText('Waived')).not.toBeInTheDocument()
    await expect(canvas.queryByText('Estimated frozen close spread')).not.toBeInTheDocument()
  },
}

export const FadOnlyClose: Story = {
  name: 'FAD-Only Close',
  args: {
    regime: 'fad-only',
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    await expect(canvas.getByText('~0.0200%')).toBeInTheDocument()
    await expect(canvas.getByText('0.1%')).toBeInTheDocument()
    await expect(canvas.queryByText('Waived')).not.toBeInTheDocument()
    await expect(canvas.queryByText('Estimated frozen close spread')).not.toBeInTheDocument()
  },
}

export const OracleFrozenClose: Story = {
  args: {
    regime: 'oracle-frozen',
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    await expect(canvas.queryByText('Adverse oracle confidence spread')).not.toBeInTheDocument()
    await expect(canvas.queryByText('Waived')).not.toBeInTheDocument()
    await expect(canvas.getByText('Estimated frozen close spread')).toBeInTheDocument()
    await expect(canvas.getByText('2.5')).toBeInTheDocument()
    await expect(canvas.getByText('Exact')).toBeInTheDocument()
  },
}

export const SideBySide: Story = {
  args: {
    regime: 'live',
  },
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto grid max-w-[1500px] gap-6 xl:grid-cols-3">
        <RegimePanel regime="live" />
        <RegimePanel regime="fad-only" />
        <RegimePanel regime="oracle-frozen" />
      </div>
    </div>
  ),
}
