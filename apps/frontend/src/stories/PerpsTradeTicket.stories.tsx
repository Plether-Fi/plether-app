import { relaxedProtectionCloseLimits } from '../contracts/positionProtection'
import { PERPS_SETTLEMENT_TOKEN_LABEL } from '../contracts/perpsAddresses'
import type { ComponentProps } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, userEvent, within } from 'storybook/test'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'
import {
  PERPS_EXECUTION_MODE,
  PERPS_EXECUTION_MODE_MASK,
  type PreparedPerpsOrderV3,
} from '../contracts/perpsOrderV3'
import { getOpenCapacityUnavailableMessage } from '../utils/perpsTradeTicketMessages'
import { getPerpsErrorMessage } from '../utils/perpsErrors'
import {
  PerpsIdentityContext,
  type PerpsAaDeploymentManifest,
  type PerpsIdentityContextValue,
} from '../perps-aa'

type TicketProps = ComponentProps<typeof PerpsTradeTicket>
type OpenPreviewFixture = NonNullable<TicketProps['openPreviewFixture']>
type ClosePreviewFixture = NonNullable<TicketProps['closePreviewFixture']>

const ASSETS = 1_000_000n
const POSITION_SIZE = 8_200n * 10n ** 18n
const ORACLE_PRICE = 98_300_000n
const STORY_ADDRESS = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
const STORY_TRADING_ACCOUNT = '0x9314586D4068C73B23a64d7406Ca8FfEeCc2cBFc'

const SPONSORED_STORY_MANIFEST: PerpsAaDeploymentManifest = {
  version: 'perps-aa-arbitrum-sepolia-v2',
    orderInterfaceVersion: 3,
  chainId: 421614,
  entryPoint: '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108',
  entryPointVersion: '0.8',
  bundlerRpcUrl: '/api/perps/v1/aa/rpc',
  paymasterRpcUrl: '/api/perps/v1/aa/rpc',
  paymasterAddress: '0x1234567890123456789012345678901234567890',
  paymasterVersion: 'plether-verifying-v1',
  smartAccountMode: 'simple',
  smartAccountVersion: 'permissionless-simple-v0.8',
  smartAccountIndex: '0',
  smartAccountFactory: '0x13E9ed32155810FDbd067D4522C492D6f68E5944',
  settlementToken: '0x4b44312d1f94d3d79a36a875691d768dd6c0575f',
  settlementTokenSupportsEip3009: false,
  settlementTokenEip712Name: null,
  settlementTokenEip712Version: null,
  marginClearinghouse: '0xe91f82bd0e1a3ebc78ee0d7c9c745029c2d324c7',
  cfdEngine: '0x55e5d20ebc41234584fe0ba3c6ddcf71b3cbb2cb',
  orderRouter: '0x9b9ddc783780a14e4930dbbcd94d6356a82725e8',
  orderLifecycleBook: '0x519f57a34fb440c42bfd2d8328d2f80349af3872',
  policyEvaluator: '0xf550cada6b7d477b23069c8b4f254d19088f28c8',
  positionProtectionBook: '0xa7e2132ff262ac97cdeaeeb158f0ee5c8c30e312',
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
  marginAssets: 1_640n * ASSETS,
  unrealizedPnlAssets: 54_250_000n,
  maintenanceMarginAssets: 80_600_000n,
  liquidatable: false,
  estimatedNotionalAssets: 8_060n * ASSETS,
  entryNotionalAssets: 8_095n * ASSETS,
  dxyExposureAssets: 8_200n * ASSETS,
  displayDxyPrice: 101_700_000n,
  liquidationPrice: 112_000_000n,
  pendingCarryAssets: 4_200_000n,
  vpiAccrued: 60n * ASSETS,
}

const executionProtectionsFixture = {
  account: STORY_TRADING_ACCOUNT,
  manifestVersion: 'perps-aa-arbitrum-sepolia-v2',
  orderRouter: '0x9b9ddc783780a14e4930dbbcd94d6356a82725e8',
  orderLifecycleBook: '0x1111111111111111111111111111111111111111',
  request: {
    clientOrderId: `0x${'12'.repeat(32)}`,
    side: 0,
    sizeDelta: 2_000n * 10n ** 18n,
    marginDelta: 393_200_000n,
    targetPrice: 98_250_000n,
    isClose: false,
    bounds: {
      submitBy: 1_788_000_300n, executionWindowSeconds: 60,
      allowedExecutionModes: PERPS_EXECUTION_MODE_MASK.LIVE,
      expectedConfigHash: `0x${'34'.repeat(32)}`,
      maxExecutionBountyAssets: 250_000n,
      maxExecutionNotionalAssets: 1_966n * ASSETS,
      maxGrossAccountDebitAssets: 396_236_400n,
      maxActionChargeAssets: 1_400_000n,
      maxExplicitFeesAssets: 786_400n,
      maxPostPositionSize: 2_000n * 10n ** 18n,
      minPostSettlementBalanceAssets: 9_857_013_600n,
      minPostPositionEquityAssets: 391_013_600n,
      maxPostLeverageBps: 50_000,
    },
  },
  executionBountyAssets: 250_000n,
  reviewedBlockNumber: 302_257_125n,
  reviewedBlockHash: `0x${'56'.repeat(32)}`,
  reviewedPrice: ORACLE_PRICE,
  protection: {
    submitBy: 1_788_000_300n, executionWindowSeconds: 60,
    executionMode: PERPS_EXECUTION_MODE.LIVE,
    executionBountyAssets: 250_000n,
  },
} satisfies PreparedPerpsOrderV3

function openPreviewFixture({
  size,
  notionalAssets,
  marginDeltaAssets,
  postSize,
  postMarginAssets,
  postVpiAccrued = 1_400_000n,
}: {
  size: bigint
  notionalAssets: bigint
  marginDeltaAssets: bigint
  postSize: bigint
  postMarginAssets: bigint
  postVpiAccrued?: bigint
}): OpenPreviewFixture {
  return {
    valid: true,
    invalidReason: 0,
    failureCategory: 0,
    executionPrice: 98_280_000n,
    sizeDelta: size,
    notionalAssets,
    marginDeltaAssets,
    vpiAssets: 1_400_000n,
    executionFeeAssets: (notionalAssets * 4n) / 10_000n,
    tradeCostAssets: 2_186_400n,
    poolRebatePayoutAssets: 0n,
    pendingCarryAssets: 0n,
    initialMarginRequirementAssets: marginDeltaAssets,
    maintenanceMarginAssets: notionalAssets / 100n,
    postSize,
    postMarginAssets,
    postEntryPrice: 98_280_000n,
    postVpiAccrued,
    postUnrealizedPnlAssets: 0n,
    postEquityAssets: postMarginAssets - 1_400_000n,
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
  vpiDeltaAssets = -2_750_000n,
}: {
  sizeDelta: bigint
  remainingSize: bigint
  remainingMargin: bigint
  vpiDeltaAssets?: bigint
}): ClosePreviewFixture {
  return {
    valid: true,
    invalidReason: 0,
    executionPrice: 98_320_000n,
    sizeDelta,
    realizedPnlAssets: 32_500_000n,
    vpiDeltaAssets,
    vpiAssets: 0n,
    executionFeeAssets: 1_966_400n,
    remainingSize,
    remainingMargin,
    frozenSpreadAssets: 0n,
    frozenSpreadPaidAssets: 0n,
    frozenSpreadWaivedAssets: 0n,
  }
}

const documentationMarketArgs = {
  oraclePriceRaw: ORACLE_PRICE,
  oraclePriceDisplay: '1.0170',
  latestBasket,
  adverseConfidenceMultiplierBps: '2000',
  oracleFreshness: 'fresh' as const,
  oracleFreshnessTooltip: 'validated oracle basket updated 18s ago',
  availableToTradeRaw: 18_420n * ASSETS,
  availableToTradeAmount: '18 420',
  settlementBalanceAssetsRaw: 10_254_250_000n,
  withdrawableAssetsRaw: 8_420n * ASSETS,
  walletAssetsRaw: 25_000n * ASSETS,
  longOpenCapacityAssets: 250_000n * ASSETS,
  shortOpenCapacityAssets: 250_000n * ASSETS,
  executionFeeBps: 4n,
}

const executedFrozenCloseReceipt = {
  executionNotionalAssets: (8_062_240_000n).toString(),
  executionBountyAssets: '0',
  realizedPnlAssets: (390n * ASSETS).toString(),
  vpiAssets: (12n * ASSETS).toString(),
  carryAssets: (6n * ASSETS).toString(),
  executionFeeAssets: (4n * ASSETS).toString(),
  frozenSpreadAssets: (24n * ASSETS).toString(),
  actionChargeAssessedAssets: (42n * ASSETS).toString(),
  actionChargeCollectedAssets: (36n * ASSETS).toString(),
  grossAccountDebitAssets: (40n * ASSETS).toString(),
  preSettlementBalanceAssets: (2_450n * ASSETS).toString(),
  postSettlementBalanceAssets: (2_450n * ASSETS).toString(),
  preTraderClaimBalanceAssets: '0',
  postTraderClaimBalanceAssets: (350n * ASSETS).toString(),
  postPositionSize: '0',
  postPositionMarginAssets: '0',
  postPositionEquityAssets: '0',
  postLeverageBps: '0',
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
        <PerpsTradeTicket maintenanceMarginBps={10n} initialMarginBps={20n} executionFeeBps={4n} {...args} />
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
    initialOrderQuantity: '1 000',
    currentPositionAmount: '0',
    validationErrorFixture: getOpenCapacityUnavailableMessage({
      direction: 'long',
      isOpeningFromZero: true,
      minimumDxyExposureAssets: 1_058_970_000n,
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
    initialOrderQuantity: '2 000',
    currentPositionAmount: '0',
    openPreviewFixture: openPreviewFixture({
      size: 2_000n * 10n ** 18n,
      notionalAssets: 1_966n * ASSETS,
      marginDeltaAssets: 393_200_000n,
      postSize: 2_000n * 10n ** 18n,
      postMarginAssets: 393_200_000n,
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
    initialOrderQuantity: '2 000',
    initialCommittedVpiAssets: 1_400_000n,
    currentPositionAmount: '0',
    openPreviewFixture: openPreviewFixture({
      size: 2_000n * 10n ** 18n,
      notionalAssets: 1_966n * ASSETS,
      marginDeltaAssets: 393_200_000n,
      postSize: 2_000n * 10n ** 18n,
      postMarginAssets: 393_200_000n,
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
    initialOrderQuantity: '5 000',
    initialCommittedVpiAssets: 1_400_000n,
    initialCommittedPositionVpiAccrued: 61_400_000n,
    currentPosition: currentLongPosition,
    openPreviewFixture: openPreviewFixture({
      size: 5_000n * 10n ** 18n,
      notionalAssets: 4_915n * ASSETS,
      marginDeltaAssets: 983n * ASSETS,
      postSize: 13_200n * 10n ** 18n,
      postMarginAssets: 2_623n * ASSETS,
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
    initialOrderQuantity: '8 200',
    initialReduceOnly: true,
    initialCommittedVpiAssets: -12_300_000n,
    initialCommittedPositionVpiAccrued: 60n * ASSETS,
    currentPosition: currentLongPosition,
    closePreviewFixture: closePreviewFixture({
      sizeDelta: POSITION_SIZE,
      remainingSize: 0n,
      remainingMargin: 0n,
      vpiDeltaAssets: -12_300_000n,
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
    initialOrderQuantity: '2 000',
    currentPositionAmount: '0',
    openPreviewFixture: openPreviewFixture({
      size: 2_000n * 10n ** 18n,
      notionalAssets: 1_966n * ASSETS,
      marginDeltaAssets: 393_200_000n,
      postSize: 2_000n * 10n ** 18n,
      postMarginAssets: 393_200_000n,
    }),
  },
  render: (args) => <TicketFrame {...args} />,
}

export const ExecutionProtections: Story = {
  name: 'Open Long · Execution Protections',
  args: {
    ...OpenLongPreview.args,
    executionProtectionsFixture,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const AddTakeProfitStopLoss: Story = {
  name: 'New order · Optional TP/SL',
  parameters: { controls: { disable: true } },
  render: () => <TicketFrame {...documentationMarketArgs} currentPositionAmount="0" initialOrderQuantity="2 000"
    protectionCapPrice={200_000_000n} protectionConfiguration={{ enabled: true, triggerBountyAssets: 200_000n, executionBountyAssets: 200_000n }} />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    await userEvent.click(canvas.getByRole('checkbox', { name: 'Take profit / stop loss' }))
    await userEvent.type(canvas.getByLabelText(`Take profit (${PERPS_SETTLEMENT_TOKEN_LABEL})`), '1.1')
    await userEvent.type(canvas.getByLabelText(`Stop loss (${PERPS_SETTLEMENT_TOKEN_LABEL})`), '0.98')
    await expect(canvas.getByLabelText(`Take profit (${PERPS_SETTLEMENT_TOKEN_LABEL})`)).toHaveValue('1.1')
  },
}

export const DisabledAttachedProtection: Story = {
  name: 'Existing position · TP/SL disabled',
  render: () => <TicketFrame {...documentationMarketArgs} currentPosition={currentLongPosition}
    protectionConfiguration={{ enabled: true, triggerBountyAssets: 200_000n, executionBountyAssets: 200_000n }} />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    await expect(canvas.getByRole('checkbox', { name: 'Take profit / stop loss' })).toBeDisabled()
  },
}

export const ReduceOnlyHidesProtection: Story = {
  name: 'Reduce only · TP/SL hidden',
  render: () => <TicketFrame {...documentationMarketArgs} currentPosition={currentLongPosition} initialReduceOnly
    protectionConfiguration={{ enabled: true, triggerBountyAssets: 200_000n, executionBountyAssets: 200_000n }} />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    await expect(canvas.queryByRole('checkbox', { name: 'Take profit / stop loss' })).not.toBeInTheDocument()
    await userEvent.click(canvas.getByRole('checkbox', { name: 'Reduce only' }))
    await expect(canvas.getByRole('checkbox', { name: 'Take profit / stop loss' })).toBeDisabled()
  },
}

export const ProtectedOpenReview: Story = {
  name: 'New order · TP/SL confirmation',
  parameters: { controls: { disable: true } },
  render: () => <TicketFrame {...OpenLongPreview.args} protectionCapPrice={200_000_000n}
    executionProtectionsFixture={{ ...executionProtectionsFixture, positionProtection: {
      book: '0xa7e2132ff262ac97cdeaeeb158f0ee5c8c30e312',
      params: { takeProfitTriggerPrice: 90_000_000n, stopLossTriggerPrice: 102_000_000n, closeLimits: relaxedProtectionCloseLimits() },
      triggerBountyAssets: 200_000n, executionBountyAssets: 200_000n,
    } }} />,
}

export const IncreaseLongPreview: Story = {
  args: {
    ...documentationMarketArgs,
    initialLifecycleState: 'preview',
    initialReviewOpen: false,
    initialDirection: 'long',
    initialOrderQuantity: '4 900',
    currentPosition: currentLongPosition,
    openPreviewFixture: openPreviewFixture({
      size: 4_900n * 10n ** 18n,
      notionalAssets: 4_816_700_000n,
      marginDeltaAssets: 963_340_000n,
      postSize: 13_100n * 10n ** 18n,
      postMarginAssets: 2_603_340_000n,
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
    initialOrderQuantity: '5 000',
    currentPosition: currentLongPosition,
    closePreviewFixture: closePreviewFixture({
      sizeDelta: 5_000n * 10n ** 18n,
      remainingSize: 3_200n * 10n ** 18n,
      remainingMargin: 640n * ASSETS,
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
    initialOrderQuantity: '4 100',
    currentPosition: { ...currentLongPosition, vpiAccrued: -40n * ASSETS },
    closePreviewFixture: closePreviewFixture({
      sizeDelta: 4_100n * 10n ** 18n,
      remainingSize: 4_100n * 10n ** 18n,
      remainingMargin: 820n * ASSETS,
      vpiDeltaAssets: 6n * ASSETS,
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
    initialOrderQuantity: '8 200',
    currentPosition: currentLongPosition,
    closePreviewFixture: closePreviewFixture({
      sizeDelta: POSITION_SIZE,
      remainingSize: 0n,
      remainingMargin: 0n,
      vpiDeltaAssets: 4_250_000n,
    }),
  },
  render: (args) => <TicketFrame {...args} />,
}

export const FlipLongToShortPreview: Story = {
  args: {
    initialLifecycleState: 'preview',
    initialReviewOpen: true,
    initialDirection: 'short',
    initialOrderQuantity: '10 000',
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
    initialOrderQuantity: '10 000',
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
    initialOrderQuantity: '10 000',
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
    initialOrderQuantity: '2 000',
    initialCommittedVpiAssets: 1_400_000n,
    currentPositionAmount: '0',
    openPreviewFixture: openPreviewFixture({
      size: 2_000n * 10n ** 18n,
      notionalAssets: 1_966n * ASSETS,
      marginDeltaAssets: 393_200_000n,
      postSize: 2_000n * 10n ** 18n,
      postMarginAssets: 393_200_000n,
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

const slippageFailedOrder: NonNullable<TicketProps['initialFailedOrder']> = {
  orderId: 10725n,
  time: '21 Sep, 09:16',
  market: 'plDXY Perp',
  side: 'Long',
  type: 'Open',
  price: '1.0206',
  size: '--',
  status: 'Failed',
  account: STORY_ADDRESS,
  clientOrderId: `0x${'12'.repeat(32)}`,
  terminalReason: 'Slippage',
  executionPriceRaw: 97_936_552n,
  commitTxHash: '0xa25928530a9f2ec9dfd419cf5392c511571ecc256d4a991f1661bdf6d4897aee',
  revealTxHash: '0x9cff4682bc1c9c478977e799c3e360a4e173f1b3371b4bcbb9d598f4f0010a45',
  receiptEconomics: { executionBountyAssets: '200000', executionFeeAssets: '0' },
}

export const SlippageFailed: Story = {
  name: 'Order Failed · Price Limit',
  args: {
    initialLifecycleState: 'selfExecuteFailed',
    initialReviewOpen: true,
    initialDirection: 'long',
    initialOrderId: slippageFailedOrder.orderId,
    initialCommitTxHash: slippageFailedOrder.commitTxHash,
    initialFailedOrder: slippageFailedOrder,
    // Illustrative exact limit: both prices would round to 1.0206 in the old UI.
    initialCommittedTargetPrice: 1.02063,
  },
  render: (args) => <TicketFrame {...args} />,
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body)
    await expect(page.getByText('Order failed')).toBeVisible()
    await expect(page.getByText(/execution price of 1\.02063448 was above your maximum acceptable price of 1\.02063000 by 0\.00000448/)).toBeVisible()
    await expect(page.getByText('1.02063000')).toBeVisible()
    await expect(page.getByText('1.02063448')).toBeVisible()
    await expect(page.getByText(`0.2 ${PERPS_SETTLEMENT_TOKEN_LABEL}`)).toBeVisible()
    await expect(page.getByText(/cannot be retried or finalized manually/)).toBeVisible()
  },
}

export const CloseSlippageFailed: Story = {
  name: 'Order Failed · Close Price Limit',
  args: {
    ...SlippageFailed.args,
    initialDirection: 'short',
    initialCommittedDirection: 'long',
    initialReduceOnly: true,
    initialCommittedTargetPrice: 1.02064,
    initialFailedOrder: { ...slippageFailedOrder, type: 'Close' },
  },
  render: (args) => <TicketFrame {...args} />,
}

export const ExpiredOrder: Story = {
  name: 'Order Failed · Expired',
  args: {
    ...SlippageFailed.args,
    initialFailedOrder: {
      ...slippageFailedOrder,
      terminalReason: 'Expired',
      executionPriceRaw: undefined,
    },
  },
  render: (args) => <TicketFrame {...args} />,
}

export const Executed: Story = {
  name: 'Close Long · Reconciled Frozen Close',
  args: {
    ...documentationMarketArgs,
    initialLifecycleState: 'executed',
    initialReviewOpen: true,
    initialDirection: 'short',
    initialOrderQuantity: '8 200',
    initialReduceOnly: true,
    initialCommittedIsFullClose: true,
    initialCommittedPositionVpiAccrued: 60n * ASSETS,
    initialOrderId: 62n,
    initialCommittedSizeDelta: POSITION_SIZE,
    initialFinalExecutionPrice: 98_320_000n,
    initialFinalExecutionOraclePrice: ORACLE_PRICE,
    initialFinalExecutionOracleFrozen: true,
    initialFinalExecutionEconomicsVersion: 2,
    initialFinalReceiptEconomics: executedFrozenCloseReceipt,
    initialFinalVpiAssets: 12n * ASSETS,
    initialCommittedPrePositionMarginAssets: currentLongPosition.marginAssets,
    currentPositionAmount: '0',
    oracleFrozen: true,
    marketPhase: 'close-only',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const ExecutedWithVpiCharge: Story = {
  name: 'Close Long · Finalized VPI Charge',
  args: {
    ...Executed.args,
    initialFinalVpiAssets: 4_250_000n,
    initialFinalReceiptEconomics: {
      ...executedFrozenCloseReceipt,
      vpiAssets: '4250000',
      actionChargeAssessedAssets: '34250000',
      actionChargeCollectedAssets: '28250000',
      postTraderClaimBalanceAssets: '357750000',
    },
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

export const ApprovalDeadlineFailed: Story = {
  name: 'Commit Failed · Approval Deadline',
  args: {
    ...Failed.args,
    initialFlowError: getPerpsErrorMessage({
      details: JSON.stringify({ code: -32001, data: { reason: 'DEADLINE_TOO_CLOSE', retryable: false } }),
    }, 'commit') + '\n\nSupport reference: 7b2d5d87-5ddf-4eca-8c03-cd68c4af7281',
  },
  render: (args) => <TicketFrame {...args} />,
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body)
    await expect(page.getByText('Commit transaction failed')).toBeVisible()
    await expect(page.getByText(/Approval finished too close/)).toBeVisible()
    await expect(page.getByText('7b2d5d87-5ddf-4eca-8c03-cd68c4af7281')).toBeVisible()
    await expect(page.getByRole('button', { name: 'Review again' })).toBeVisible()
  },
}

export const DepositMargin: Story = {
  args: {
    initialMarginAction: 'deposit',
    initialMarginActionAmount: '10 000',
    walletAssetsRaw: 100_000_000_000n,
    settlementBalanceAssetsRaw: 5_000_000_000n,
    withdrawableAssetsRaw: 5_000_000_000n,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const OwnerWalletTransferDeposit: Story = {
  args: {
    initialMarginAction: 'deposit',
    initialMarginActionAmount: '100 000',
    availableToTradeRaw: 880_000n,
    availableToTradeAmount: '0.88',
    ownerWalletAssetsRaw: 100_049n * ASSETS,
    tradingAccountAssetsRaw: 0n,
    settlementBalanceAssetsRaw: 880_000n,
    withdrawableAssetsRaw: 880_000n,
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
    walletAssetsRaw: 25_000_000_000n,
    settlementBalanceAssetsRaw: 8_750_000_000n,
    withdrawableAssetsRaw: 3_200_000_000n,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const WithdrawExceedsAvailable: Story = {
  args: {
    initialMarginAction: 'withdraw',
    initialMarginActionAmount: '4 000',
    walletAssetsRaw: 25_000_000_000n,
    settlementBalanceAssetsRaw: 8_750_000_000n,
    withdrawableAssetsRaw: 3_200_000_000n,
  },
  render: (args) => <TicketFrame {...args} />,
}

export const MarginCallSimulatorConfirmation: Story = {
  args: {
    initialMarginCallSimulatorConfirmationOpen: true,
    initialDirection: 'long',
    initialOrderQuantity: '10 000',
    initialLeverage: 33,
    maintenanceMarginBps: 10n,
    initialMarginBps: 20n,
    executionFeeBps: 4n,
    marketPhase: 'open',
    marketCurrentDuration: '2h 18m',
  },
  render: (args) => <TicketFrame {...args} />,
}

export const MarginAccountSummary: Story = {
  args: {
    initialDirection: 'long',
    initialOrderQuantity: '2 000',
    settlementBalanceAssetsRaw: 1_248_250_000n,
    withdrawableAssetsRaw: 648_250_000n,
    availableToTradeRaw: 848_250_000n,
    availableToTradeAmount: '848.25',
    currentPosition: {
      exists: true,
      side: 0,
      direction: 'long',
      size: 2_000n * 10n ** 18n,
      entryPrice: 98_300_000n,
      marginAssets: 400_000_000n,
      unrealizedPnlAssets: 48_250_000n,
      maintenanceMarginAssets: 20_000_000n,
      liquidatable: false,
      estimatedNotionalAssets: 1_999_920_000n,
      entryNotionalAssets: 2_000_000_000n,
      dxyExposureAssets: 2_069_380_000n,
      displayDxyPrice: 101_700_000n,
      liquidationPrice: 110_000_000n,
      pendingCarryAssets: 1_250_000n,
    },
  },
  render: (args) => <TicketFrame {...args} />,
}
