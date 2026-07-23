import type { ComponentProps } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'
import {
  PerpsIdentityContext,
  type PerpsIdentityContextValue,
} from '../perps-aa'

type TicketProps = ComponentProps<typeof PerpsTradeTicket>
type OpenPreviewFixture = NonNullable<TicketProps['openPreviewFixture']>
type ClosePreviewFixture = NonNullable<TicketProps['closePreviewFixture']>

const USDC = 1_000_000n
const POSITION_SIZE = 8_200n * 10n ** 18n
const ORACLE_PRICE = 98_300_000n
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

const currentLongPosition = {
  exists: true,
  side: 0,
  direction: 'long' as const,
  size: POSITION_SIZE,
  entryPrice: 98_750_000n,
  marginUsdc: 1_640n * USDC,
  unrealizedPnlUsdc: 54_250_000n,
  maintenanceMarginUsdc: 80_600_000n,
  liquidatable: false,
  estimatedNotionalUsdc: 8_060n * USDC,
  entryNotionalUsdc: 8_095n * USDC,
  dxyExposureUsdc: 8_200n * USDC,
  displayDxyPrice: 101_700_000n,
  liquidationPrice: 112_000_000n,
  pendingCarryUsdc: 4_200_000n,
}

function openPreviewFixture({
  size,
  notionalUsdc,
  marginDeltaUsdc,
  postSize,
  postMarginUsdc,
}: {
  size: bigint
  notionalUsdc: bigint
  marginDeltaUsdc: bigint
  postSize: bigint
  postMarginUsdc: bigint
}): OpenPreviewFixture {
  return {
    valid: true,
    invalidReason: 0,
    failureCategory: 0,
    executionPrice: 98_280_000n,
    sizeDelta: size,
    notionalUsdc,
    marginDeltaUsdc,
    vpiUsdc: 1_400_000n,
    executionFeeUsdc: (notionalUsdc * 4n) / 10_000n,
    tradeCostUsdc: 2_186_400n,
    poolRebatePayoutUsdc: 0n,
    pendingCarryUsdc: 0n,
    initialMarginRequirementUsdc: marginDeltaUsdc,
    maintenanceMarginUsdc: notionalUsdc / 100n,
    postSize,
    postMarginUsdc,
    postEntryPrice: 98_280_000n,
    postVpiAccrued: 1_400_000n,
    postUnrealizedPnlUsdc: 0n,
    postEquityUsdc: postMarginUsdc - 1_400_000n,
    postHealthBps: 9_400n,
    postLiquidatable: false,
    hasLiquidationPrice: true,
    liquidationPrice: 104_000_000n,
  }
}

function closePreviewFixture({
  sizeDelta,
  remainingSize,
  remainingMargin,
}: {
  sizeDelta: bigint
  remainingSize: bigint
  remainingMargin: bigint
}): ClosePreviewFixture {
  return {
    valid: true,
    invalidReason: 0,
    executionPrice: 98_320_000n,
    sizeDelta,
    realizedPnlUsdc: 32_500_000n,
    vpiDeltaUsdc: -2_750_000n,
    vpiUsdc: 0n,
    executionFeeUsdc: 1_966_400n,
    remainingSize,
    remainingMargin,
    frozenSpreadUsdc: 0n,
    frozenSpreadPaidUsdc: 0n,
    frozenSpreadWaivedUsdc: 0n,
  }
}

const documentationMarketArgs = {
  oraclePriceRaw: ORACLE_PRICE,
  oraclePriceDisplay: '1.0170',
  latestBasket,
  adverseConfidenceMultiplierBps: '2000',
  oracleFreshness: 'fresh' as const,
  oracleFreshnessTooltip: 'validated oracle basket updated 18s ago',
  availableToTradeRaw: 18_420n * USDC,
  availableToTradeAmount: '18 420',
  portfolioValueRaw: 10_254_250_000n,
  withdrawableUsdcRaw: 8_420n * USDC,
  walletUsdcRaw: 25_000n * USDC,
  longOpenCapacityUsdc: 250_000n * USDC,
  shortOpenCapacityUsdc: 250_000n * USDC,
  executionFeeBps: 4n,
}

const meta: Meta<typeof PerpsTradeTicket> = {
  title: 'Perps/Trade Ticket',
  component: PerpsTradeTicket,
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',
  },
  decorators: [
    (Story) => (
      <PerpsIdentityContext.Provider value={STORY_IDENTITY}>
        <Story />
      </PerpsIdentityContext.Provider>
    ),
  ],
}

export default meta
type Story = StoryObj<typeof meta>

function TicketFrame(args: React.ComponentProps<typeof PerpsTradeTicket>) {
  return (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="ml-auto max-w-md">
        <PerpsTradeTicket maintenanceMarginBps={100n} {...args} />
      </div>
    </div>
  )
}

export const Compose: Story = {
  args: {
    initialLifecycleState: 'preview',
    oracleFreshness: 'fresh',
    oracleFreshnessTooltip: 'live oracle price',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const PreviewModal: Story = {
  args: {
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const PreparingWalletRequest: Story = {
  args: {
    initialLifecycleState: 'commitPreparing',
    initialReviewOpen: true,
    initialDirection: 'long',
    initialSize: '2 000',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const OpenLongPreview: Story = {
  args: {
    ...documentationMarketArgs,
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'long',
    initialSize: '2 000',
    currentPositionAmount: '0',
    openPreviewFixture: openPreviewFixture({
      size: 2_000n * 10n ** 18n,
      notionalUsdc: 1_966n * USDC,
      marginDeltaUsdc: 393_200_000n,
      postSize: 2_000n * 10n ** 18n,
      postMarginUsdc: 393_200_000n,
    }),
  },
  render: (args) => <TicketFrame {...args} />,
}

export const IncreaseLongPreview: Story = {
  args: {
    ...documentationMarketArgs,
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'long',
    initialSize: '5 000',
    currentPosition: currentLongPosition,
    openPreviewFixture: openPreviewFixture({
      size: 5_000n * 10n ** 18n,
      notionalUsdc: 4_915n * USDC,
      marginDeltaUsdc: 983n * USDC,
      postSize: 13_200n * 10n ** 18n,
      postMarginUsdc: 2_623n * USDC,
    }),
  },
  render: (args) => <TicketFrame {...args} />,
}

export const ReduceLongPreview: Story = {
  args: {
    ...documentationMarketArgs,
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'short',
    initialSize: '5 000',
    currentPosition: currentLongPosition,
    closePreviewFixture: closePreviewFixture({
      sizeDelta: 5_000n * 10n ** 18n,
      remainingSize: 3_200n * 10n ** 18n,
      remainingMargin: 640n * USDC,
    }),
  },
  render: (args) => <TicketFrame {...args} />,
}

export const CloseLongPreview: Story = {
  args: {
    ...documentationMarketArgs,
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'short',
    initialSize: '8 200',
    currentPosition: currentLongPosition,
    closePreviewFixture: closePreviewFixture({
      sizeDelta: POSITION_SIZE,
      remainingSize: 0n,
      remainingMargin: 0n,
    }),
  },
  render: (args) => <TicketFrame {...args} />,
}

export const FlipLongToShortPreview: Story = {
  args: {
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'short',
    initialSize: '10 000',
    currentPositionSide: 'long',
    currentPositionAmount: '8 200',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const FlipShortToLongPreview: Story = {
  args: {
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'long',
    initialSize: '10 000',
    currentPositionSide: 'short',
    currentPositionAmount: '8 200',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const ReduceOnlyPreventsFlipPreview: Story = {
  args: {
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'short',
    initialSize: '10 000',
    initialReduceOnly: true,
    currentPositionSide: 'long',
    currentPositionAmount: '8 200',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const CommitPending: Story = {
  args: {
    initialLifecycleState: 'commitPending',
    initialReviewOpen: true,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const RevealPending: Story = {
  name: 'Finalizing Price',
  args: {
    initialLifecycleState: 'revealPending',
    initialReviewOpen: true,
    showFinalizationProgress: true,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const SelfExecuteAvailable: Story = {
  name: 'Manual Finalization Ready',
  args: {
    initialLifecycleState: 'selfExecuteAvailable',
    initialReviewOpen: true,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const SelfExecutePending: Story = {
  name: 'Finalizing Trade',
  args: {
    initialLifecycleState: 'selfExecutePending',
    initialReviewOpen: true,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const SelfExecuteFailed: Story = {
  name: 'Finalization Failed',
  args: {
    initialLifecycleState: 'selfExecuteFailed',
    initialReviewOpen: true,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const Executed: Story = {
  args: {
    initialLifecycleState: 'executed',
    initialReviewOpen: true,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const Failed: Story = {
  args: {
    initialLifecycleState: 'failed',
    initialReviewOpen: true,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const DepositMargin: Story = {
  args: {
    initialMarginAction: 'deposit',
    initialMarginActionAmount: '10 000',
    walletUsdcRaw: 100_000_000_000n,
    portfolioValueRaw: 5_000_000_000n,
    withdrawableUsdcRaw: 5_000_000_000n,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const WithdrawMargin: Story = {
  args: {
    initialMarginAction: 'withdraw',
    initialMarginActionAmount: '1 500',
    walletUsdcRaw: 25_000_000_000n,
    portfolioValueRaw: 8_750_000_000n,
    withdrawableUsdcRaw: 3_200_000_000n,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const WithdrawExceedsAvailable: Story = {
  args: {
    initialMarginAction: 'withdraw',
    initialMarginActionAmount: '4 000',
    walletUsdcRaw: 25_000_000_000n,
    portfolioValueRaw: 8_750_000_000n,
    withdrawableUsdcRaw: 3_200_000_000n,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const MarginCallSimulatorConfirmation: Story = {
  args: {
    initialMarginCallSimulatorConfirmationOpen: true,
    initialDirection: 'long',
    initialSize: '10 000',
    initialLeverage: 33,
    maintenanceMarginBps: 25n,
    marketPhase: 'open',
    marketCurrentDuration: '2h 18m',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const MarginAccountSummary: Story = {
  args: {
    initialDirection: 'long',
    initialSize: '2 000',
    portfolioValueRaw: 1_248_250_000n,
    withdrawableUsdcRaw: 648_250_000n,
    availableToTradeRaw: 848_250_000n,
    availableToTradeAmount: '848.25',
    currentPosition: {
      exists: true,
      side: 0,
      direction: 'long',
      size: 2_000n * 10n ** 18n,
      entryPrice: 98_300_000n,
      marginUsdc: 400_000_000n,
      unrealizedPnlUsdc: 48_250_000n,
      maintenanceMarginUsdc: 20_000_000n,
      liquidatable: false,
      estimatedNotionalUsdc: 1_999_920_000n,
      entryNotionalUsdc: 2_000_000_000n,
      dxyExposureUsdc: 2_069_380_000n,
      displayDxyPrice: 101_700_000n,
      liquidationPrice: 110_000_000n,
      pendingCarryUsdc: 1_250_000n,
    },
  },
  render: (args) => <TicketFrame {...args} />,
}
