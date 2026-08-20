import type { ComponentProps } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'
import { getOpenCapacityUnavailableMessage } from '../utils/perpsTradeTicketMessages'
import {
  PerpsIdentityContext,
  type PerpsAaDeploymentManifest,
  type PerpsIdentityContextValue,
} from '../perps-aa'

type TicketProps = ComponentProps<typeof PerpsTradeTicket>
type OpenPreviewFixture = NonNullable<TicketProps['openPreviewFixture']>
type ClosePreviewFixture = NonNullable<TicketProps['closePreviewFixture']>

const USDC = 1_000_000n
const POSITION_SIZE = 8_200n * 10n ** 18n
const ORACLE_PRICE = 98_300_000n
const STORY_ADDRESS = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
const STORY_TRADING_ACCOUNT = '0x9314586D4068C73B23a64d7406Ca8FfEeCc2cBFc'

const SPONSORED_STORY_MANIFEST: PerpsAaDeploymentManifest = {
  version: 'perps-aa-arbitrum-sepolia-v1',
  chainId: 421614,
  entryPoint: '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108',
  entryPointVersion: '0.8',
  pimlicoRpcUrl: '/api/perps/v1/aa/pimlico',
  smartAccountMode: 'simple',
  smartAccountVersion: 'permissionless-simple-v0.8',
  smartAccountIndex: '0',
  smartAccountFactory: '0x13E9ed32155810FDbd067D4522C492D6f68E5944',
  usdc: '0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3',
  usdcSupportsEip3009: false,
  usdcEip712Name: null,
  usdcEip712Version: null,
  marginClearinghouse: '0x19c2f60f6312EAF9acDE4C2b04551a05cA9bE76e',
  cfdEngine: '0x6A25eA1015b5f032d8a2D95d57AEfcB99219bF0a',
  orderRouter: '0x04E3103752f623fBcDcD01f588590Af4c53E4c1E',
  userOperationExplorerUrlTemplate:
    'https://arbitrum-sepolia.blockscout.com/op/{userOperationHash}',
  transactionExplorerUrlTemplate:
    'https://arbitrum-sepolia.blockscout.com/tx/{transactionHash}',
  testnetFaucet: null,
  sponsorshipEnabled: true,
}

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

const SPONSORED_STORY_IDENTITY: PerpsIdentityContextValue = {
  ...STORY_IDENTITY,
  accountAddress: STORY_TRADING_ACCOUNT,
  isAaManifestConfigured: true,
  sponsorshipEnabled: true,
  manifest: SPONSORED_STORY_MANIFEST,
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
  vpiAccrued: 60n * USDC,
}

function openPreviewFixture({
  size,
  notionalUsdc,
  marginDeltaUsdc,
  postSize,
  postMarginUsdc,
  postVpiAccrued = 1_400_000n,
}: {
  size: bigint
  notionalUsdc: bigint
  marginDeltaUsdc: bigint
  postSize: bigint
  postMarginUsdc: bigint
  postVpiAccrued?: bigint
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
    postVpiAccrued,
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
  vpiDeltaUsdc = -2_750_000n,
}: {
  sizeDelta: bigint
  remainingSize: bigint
  remainingMargin: bigint
  vpiDeltaUsdc?: bigint
}): ClosePreviewFixture {
  return {
    valid: true,
    invalidReason: 0,
    executionPrice: 98_320_000n,
    sizeDelta,
    realizedPnlUsdc: 32_500_000n,
    vpiDeltaUsdc,
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

export const LongUnavailableDueToMarketSkew: Story = {
  name: 'Long unavailable due to market skew',
  args: {
    ...documentationMarketArgs,
    initialLifecycleState: 'preview',
    initialDirection: 'long',
    initialSize: '1 058.97',
    currentPositionAmount: '0',
    validationErrorFixture: getOpenCapacityUnavailableMessage({
      direction: 'long',
      isOpeningFromZero: true,
      minimumDxyExposureUsdc: 1_058_970_000n,
    }),
  },
  render: (args) => <TicketFrame {...args} />,
}

export const PreviewModal: Story = {
  name: 'Open Long · Confirmation Modal',
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

export const PreparingWalletRequest: Story = {
  name: 'Open Long · Preparing Wallet Request',
  args: {
    ...documentationMarketArgs,
    initialLifecycleState: 'commitPreparing',
    initialReviewOpen: true,
    initialDirection: 'long',
    initialSize: '2 000',
    initialCommittedVpiUsdc: 1_400_000n,
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

export const IncreaseLongPreparingWalletRequest: Story = {
  name: 'Increase Long · Preparing Wallet Request',
  args: {
    ...documentationMarketArgs,
    initialLifecycleState: 'commitPreparing',
    initialReviewOpen: true,
    initialDirection: 'long',
    initialSize: '5 000',
    initialCommittedVpiUsdc: 1_400_000n,
    initialCommittedPositionVpiAccrued: 61_400_000n,
    currentPosition: currentLongPosition,
    openPreviewFixture: openPreviewFixture({
      size: 5_000n * 10n ** 18n,
      notionalUsdc: 4_915n * USDC,
      marginDeltaUsdc: 983n * USDC,
      postSize: 13_200n * 10n ** 18n,
      postMarginUsdc: 2_623n * USDC,
      postVpiAccrued: 61_400_000n,
    }),
  },
  render: (args) => <TicketFrame {...args} />,
}

export const CloseLongPreparingWalletRequest: Story = {
  name: 'Close Long · Preparing VPI Credit',
  args: {
    ...documentationMarketArgs,
    initialLifecycleState: 'commitPreparing',
    initialReviewOpen: true,
    initialDirection: 'short',
    initialSize: '8 200',
    initialReduceOnly: true,
    initialCommittedVpiUsdc: -12_300_000n,
    initialCommittedPositionVpiAccrued: 60n * USDC,
    currentPosition: currentLongPosition,
    closePreviewFixture: closePreviewFixture({
      sizeDelta: POSITION_SIZE,
      remainingSize: 0n,
      remainingMargin: 0n,
      vpiDeltaUsdc: -12_300_000n,
    }),
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
      postVpiAccrued: 61_400_000n,
    }),
  },
  render: (args) => <TicketFrame {...args} />,
}

export const ReduceLongPreview: Story = {
  name: 'Reduce Long · VPI Credit',
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

export const ReduceLongWithProvisionalVpiCredit: Story = {
  name: 'Reduce Long · Existing Provisional VPI Credit',
  args: {
    ...documentationMarketArgs,
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'short',
    initialSize: '4 100',
    currentPosition: { ...currentLongPosition, vpiAccrued: -40n * USDC },
    closePreviewFixture: closePreviewFixture({
      sizeDelta: 4_100n * 10n ** 18n,
      remainingSize: 4_100n * 10n ** 18n,
      remainingMargin: 820n * USDC,
      vpiDeltaUsdc: 6n * USDC,
    }),
  },
  render: (args) => <TicketFrame {...args} />,
}

export const CloseLongPreview: Story = {
  name: 'Close Long · VPI Charge',
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
      vpiDeltaUsdc: 4_250_000n,
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
    ...documentationMarketArgs,
    initialLifecycleState: 'commitPending',
    initialReviewOpen: true,
    initialDirection: 'long',
    initialSize: '2 000',
    initialCommittedVpiUsdc: 1_400_000n,
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
  name: 'Close Long · Finalized VPI Credit',
  args: {
    ...documentationMarketArgs,
    initialLifecycleState: 'executed',
    initialReviewOpen: true,
    initialDirection: 'short',
    initialSize: '8 200',
    initialReduceOnly: true,
    initialCommittedIsFullClose: true,
    initialCommittedPositionVpiAccrued: 60n * USDC,
    initialOrderId: 62n,
    initialCommittedSizeDelta: POSITION_SIZE,
    initialFinalExecutionPrice: 98_320_000n,
    initialFinalExecutionOraclePrice: ORACLE_PRICE,
    initialFinalExecutionOracleFrozen: false,
    initialFinalExecutionEconomicsVersion: 1,
    initialFinalVpiUsdc: -12_300_000n,
    currentPositionAmount: '0',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const ExecutedWithVpiCharge: Story = {
  name: 'Close Long · Finalized VPI Charge',
  args: {
    ...Executed.args,
    initialFinalVpiUsdc: 4_250_000n,
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

export const OwnerWalletTransferDeposit: Story = {
  args: {
    initialMarginAction: 'deposit',
    initialMarginActionAmount: '100 000',
    availableToTradeRaw: 880_000n,
    availableToTradeAmount: '0.88',
    ownerWalletUsdcRaw: 100_049n * USDC,
    tradingAccountUsdcRaw: 0n,
    portfolioValueRaw: 880_000n,
    withdrawableUsdcRaw: 880_000n,
  },
  render: (args) => (
    <PerpsIdentityContext.Provider value={SPONSORED_STORY_IDENTITY}>
      <TicketFrame {...args} />
    </PerpsIdentityContext.Provider>
  ),
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
