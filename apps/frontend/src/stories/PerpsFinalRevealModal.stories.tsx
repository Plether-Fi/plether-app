import type { ComponentProps } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'
import type { PerpsOrderHistoryRow, PerpsPosition } from '../hooks'
import {
  PerpsIdentityContext,
  type PerpsAaDeploymentManifest,
  type PerpsIdentityContextValue,
} from '../perps-aa'

const STORY_OWNER_ADDRESS = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
const STORY_TRADING_ACCOUNT = '0x9314586D4068C73B23a64d7406Ca8FfEeCc2cBFc'
const USER_OPERATION_HASH =
  '0x677700000000000000000000000000000000000000000000000000000000e12f'

const STORY_MANIFEST: PerpsAaDeploymentManifest = {
  version: 'perps-aa-arbitrum-sepolia-v2',
  chainId: 421614,
  entryPoint: '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108',
  entryPointVersion: '0.8',
  pimlicoRpcUrl: '/api/perps/v1/aa/pimlico',
  smartAccountMode: 'simple',
  smartAccountVersion: 'permissionless-simple-v0.8',
  smartAccountIndex: '0',
  smartAccountFactory: '0x13E9ed32155810FDbd067D4522C492D6f68E5944',
  usdc: '0xAbEe441b564DC084857468fA244AEE0A444B07DF',
  usdcSupportsEip3009: false,
  usdcEip712Name: null,
  usdcEip712Version: null,
  marginClearinghouse: '0x91c85540A1f64C9AEC2C801fcc927F037d619f17',
  cfdEngine: '0x2CEDc3f0059f0E9C1099bE96974f459E58c428d6',
  orderRouter: '0x2b9790AD11cE5fB1B91aC3415B08cD1Ec7D0cE0B',
  orderLifecycleBook: '0xca57215a3859462eb380ea40969762Ac89D99522',
  positionProtectionBook: '0x63973Eb0B5a862dfc95348D4d575FC55C9546F04',
  policyEvaluator: '0x611b34a98261D60f0aE8584F4Dd1fF09CF663466',
  userOperationExplorerUrlTemplate:
    'https://arbitrum-sepolia.blockscout.com/op/{userOperationHash}',
  transactionExplorerUrlTemplate:
    'https://arbitrum-sepolia.blockscout.com/tx/{transactionHash}',
  testnetFaucet: null,
  sponsorshipEnabled: true,
}

const STORY_IDENTITY: PerpsIdentityContextValue = {
  status: 'ready',
  ownerAddress: STORY_OWNER_ADDRESS,
  accountAddress: STORY_TRADING_ACCOUNT,
  chainId: 421614,
  isAaManifestConfigured: true,
  sponsorshipEnabled: true,
  manifest: STORY_MANIFEST,
  identity: null,
  proposedIdentity: null,
  changedIdentityFields: [],
  error: null,
  confirmIdentityAfterContinuityCheck: () => false,
  reloadIdentity: () => undefined,
}

const OWNER_WALLET_IDENTITY: PerpsIdentityContextValue = {
  ...STORY_IDENTITY,
  accountAddress: STORY_OWNER_ADDRESS,
  isAaManifestConfigured: false,
  sponsorshipEnabled: false,
  manifest: null,
}

const meta: Meta<typeof PerpsTradeTicket> = {
  title: 'Perps/Final Reveal Modal',
  component: PerpsTradeTicket,
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',
    docs: {
      description: {
        component:
          'Every user-visible state of the commit and finalization confirmation window, followed by its meaningful retry and terminal-result variants.',
      },
    },
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
type TicketProps = ComponentProps<typeof PerpsTradeTicket>

const USDC = 1_000_000n
const POSITION_SIZE_TO_USDC_SCALE = 100_000_000_000_000_000_000n
const ORDER_ID = 62n
const FINAL_PRICE_RAW = 102_590_000n
const TARGET_EXPOSURE_USDC = 2_116n * USDC
const EXECUTED_SIZE_DELTA = ((TARGET_EXPOSURE_USDC * POSITION_SIZE_TO_USDC_SCALE) + FINAL_PRICE_RAW - 1n) / FINAL_PRICE_RAW
const COMMIT_TX = '0x46cb00000000000000000000000000000000000000000000000000000000001cbb'
const AUTO_FINALIZE_TX = '0x6c0d00000000000000000000000000000000000000000000000000000000b7d3'
const SELF_REVEAL_TX = '0x9e1f00000000000000000000000000000000000000000000000000000000cafe'

const executedPosition = {
  exists: true,
  side: 0,
  direction: 'long',
  size: EXECUTED_SIZE_DELTA,
  entryPrice: FINAL_PRICE_RAW,
  marginUsdc: 401_980_000n,
  unrealizedPnlUsdc: 0n,
  maintenanceMarginUsdc: 20_100_000n,
  liquidatable: false,
  estimatedNotionalUsdc: 2_009_940_000n,
  entryNotionalUsdc: 2_009_940_000n,
  dxyExposureUsdc: 2_116_700_000n,
} satisfies PerpsPosition

const automaticallyFinalizedOrderHistory = [
  {
    orderId: ORDER_ID,
    time: '12:02',
    market: 'plDXY Perp',
    side: 'Long',
    type: 'Open',
    price: '1.0259',
    size: '2 116',
    status: 'Executed',
    account: STORY_TRADING_ACCOUNT,
    clientOrderId: `0x${'46'.repeat(32)}`,
    commitTxHash: COMMIT_TX,
    revealTxHash: AUTO_FINALIZE_TX,
  },
] satisfies PerpsOrderHistoryRow[]

const baseModalArgs = {
  initialReviewOpen: true,
  initialDirection: 'long',
  initialOrderQuantity: '2116',
  initialOrderId: ORDER_ID,
  initialCommitTxHash: COMMIT_TX,
  initialUserOperationHash: USER_OPERATION_HASH,
  initialFinalExecutionPrice: FINAL_PRICE_RAW,
  initialFinalExecutionOraclePrice: FINAL_PRICE_RAW,
  initialFinalExecutionOracleFrozen: false,
  initialFinalExecutionEconomicsVersion: 1,
  initialCommittedSizeDelta: EXECUTED_SIZE_DELTA,
  currentPositionSide: 'long',
  currentPositionAmount: '2 116',
  oraclePriceRaw: FINAL_PRICE_RAW,
  oraclePriceDisplay: '1.0259',
  oracleFreshness: 'fresh',
  oracleFreshnessTooltip: 'backend updated just now',
  oraclePublishTime: Math.floor(Date.now() / 1_000),
  availableToTradeRaw: 18_420n * USDC,
  portfolioValueRaw: 12_400n * USDC,
  withdrawableUsdcRaw: 8_100n * USDC,
  walletUsdcRaw: 5_000n * USDC,
  marginAllowanceUsdc: 5_000n * USDC,
  longOpenCapacityUsdc: 250_000n * USDC,
  shortOpenCapacityUsdc: 250_000n * USDC,
  minOpenNotionalUsdc: 10n * USDC,
  minNewPositionNotionalUsdc: 10n * USDC,
  maintenanceMarginBps: 100n,
  executionFeeBps: 4n,
} satisfies Partial<TicketProps>

function TicketFrame(args: TicketProps) {
  return (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="ml-auto max-w-md">
        <PerpsTradeTicket {...args} />
      </div>
    </div>
  )
}

function modalProps(overrides: Partial<TicketProps> = {}): TicketProps {
  return {
    ...baseModalArgs,
    ...overrides,
  }
}

function renderModal(overrides: Partial<TicketProps> = {}) {
  return () => <TicketFrame {...modalProps(overrides)} />
}

function renderModalWithIdentity(
  identity: PerpsIdentityContextValue,
  overrides: Partial<TicketProps> = {}
) {
  return () => (
    <PerpsIdentityContext.Provider value={identity}>
      <TicketFrame {...modalProps(overrides)} />
    </PerpsIdentityContext.Provider>
  )
}

export const CommitPreview: Story = {
  name: '01 · Commit Preview',
  render: renderModal({
    initialLifecycleState: 'preview',
  }),
}

export const PreparingSponsoredTransaction: Story = {
  name: '02 · Preparing Sponsored Transaction',
  render: renderModal({
    initialLifecycleState: 'commitPreparing',
    initialCommitTxHash: undefined,
  }),
}

export const PreparingOwnerWalletRequest: Story = {
  name: '02a · Preparing Owner-Wallet Request',
  render: renderModalWithIdentity(OWNER_WALLET_IDENTITY, {
    initialLifecycleState: 'commitPreparing',
    initialCommitTxHash: undefined,
    initialUserOperationHash: undefined,
  }),
}

export const WaitingForWalletConfirmation: Story = {
  name: '03a · Waiting for Wallet Confirmation',
  render: renderModal({
    initialLifecycleState: 'commitPending',
    initialCommitTxHash: undefined,
  }),
}

export const WalletResponseDelayed: Story = {
  name: '03b · Wallet Response Delayed',
  render: renderModal({
    initialLifecycleState: 'commitPending',
    initialCommitTxHash: undefined,
    initialWalletRequestWarning:
      'No wallet response yet. Open your wallet app or extension and check for a pending confirmation. If there is no pending request, reject any stuck request, reconnect the wallet, and retry.',
  }),
}

export const SubmittingSponsoredTransaction: Story = {
  name: '03c · Submitting Sponsored Transaction',
  render: renderModal({
    initialLifecycleState: 'commitPending',
    initialCommitTxHash: undefined,
    initialCommitExecutionStatus: 'submitting',
  }),
}

export const WaitingForOnChainConfirmation: Story = {
  name: '03d · Waiting for On-Chain Confirmation',
  render: renderModal({
    initialLifecycleState: 'commitPending',
    initialCommitTxHash: undefined,
    initialCommitExecutionStatus: 'confirming',
  }),
}

export const LoadingCommittedOrder: Story = {
  name: '03e · Confirmed, Loading Committed Order',
  render: renderModal({
    initialLifecycleState: 'commitPending',
    initialCommitExecutionStatus: 'confirmed',
  }),
}

export const WaitingForOwnerWalletConfirmation: Story = {
  name: '03f · Waiting for Owner-Wallet Confirmation',
  render: renderModalWithIdentity(OWNER_WALLET_IDENTITY, {
    initialLifecycleState: 'commitPending',
    initialCommitTxHash: undefined,
    initialUserOperationHash: undefined,
  }),
}

export const CommitConfirmed: Story = {
  name: '04 · Commit Confirmed (Legacy)',
  render: renderModal({
    initialLifecycleState: 'commitConfirmed',
  }),
  parameters: {
    docs: {
      description: {
        story:
          'This screen remains in the lifecycle type but is not entered by the current production flow.',
      },
    },
  },
}

export const FinalizingExecutionPrice: Story = {
  name: '05a · Finalizing Execution Price (Mock)',
  render: renderModal({
    initialLifecycleState: 'revealPending',
  }),
  parameters: {
    docs: {
      description: {
        story:
          'Static non-live fallback retained for component review; the current live flow uses keeper progress.',
      },
    },
  },
}

export const FinalizingPrice: Story = {
  name: '05b · Keeper Finalization Progress',
  render: renderModal({
    initialLifecycleState: 'revealPending',
    showFinalizationProgress: true,
  }),
}

export const OwnerWalletFinalizationCountdown: Story = {
  name: '05c · Owner-Wallet Finalization Countdown',
  render: renderModalWithIdentity(OWNER_WALLET_IDENTITY, {
    initialLifecycleState: 'revealPending',
    initialUserOperationHash: undefined,
    showFinalizationProgress: true,
  }),
}

export const ManualFinalizationReady: Story = {
  name: '06 · Manual Finalization Ready',
  render: renderModalWithIdentity(OWNER_WALLET_IDENTITY, {
    initialLifecycleState: 'selfExecuteAvailable',
  }),
}

export const FinalPriceNotReady: Story = {
  name: '06a · Final Price Not Ready',
  render: renderModalWithIdentity(OWNER_WALLET_IDENTITY, {
    initialLifecycleState: 'selfExecuteAvailable',
    initialFlowError: 'Final price is not ready yet. Execution must happen after the commit block.',
  }),
}

export const PriceDataRateLimited: Story = {
  name: '06b · Price Data Rate Limited',
  render: renderModalWithIdentity(OWNER_WALLET_IDENTITY, {
    initialLifecycleState: 'selfExecuteAvailable',
    initialFlowError: 'Price data service rate limit reached while fetching historical market data. Retry shortly.',
  }),
}

export const HistoricalPriceDataRequired: Story = {
  name: '06c · Historical Price Data Required',
  render: renderModalWithIdentity(OWNER_WALLET_IDENTITY, {
    initialLifecycleState: 'selfExecuteAvailable',
    initialFlowError: 'Historical price data was unavailable for the first post-commit tick. Wait for the cache to backfill, then retry finalizing.',
  }),
}

export const HistoricalPriceDataRejected: Story = {
  name: '06d · Historical Price Data Rejected',
  render: renderModalWithIdentity(OWNER_WALLET_IDENTITY, {
    initialLifecycleState: 'selfExecuteAvailable',
    initialFlowError: 'Historical price data was rejected with a stale-price error. The payload did not contain the exact first post-commit tick.',
  }),
}

export const ManualFinalizationPending: Story = {
  name: '07 · Manual Finalization Pending',
  render: renderModalWithIdentity(OWNER_WALLET_IDENTITY, {
    initialLifecycleState: 'selfExecutePending',
  }),
}

export const ManualFinalizationFailed: Story = {
  name: '08 · Manual Finalization Failed',
  render: renderModalWithIdentity(OWNER_WALLET_IDENTITY, {
    initialLifecycleState: 'selfExecuteFailed',
  }),
}

export const OrderNoLongerPending: Story = {
  name: '08a · Order No Longer Pending',
  render: renderModalWithIdentity(OWNER_WALLET_IDENTITY, {
    initialLifecycleState: 'selfExecuteFailed',
    initialFlowError: 'Order 62 is no longer pending, but your position did not change. It likely failed or expired before execution. Refresh order history for the terminal event.',
  }),
}

export const TerminalOrderFailed: Story = {
  name: '08b · Terminal Order Failed',
  render: renderModalWithIdentity(OWNER_WALLET_IDENTITY, {
    initialLifecycleState: 'selfExecuteFailed',
    initialFlowError:
      'Order failed: the committed execution price exceeded the accepted slippage limit.',
  }),
}

export const ManualFinalizationWalletRejected: Story = {
  name: '08c · Wallet Rejected Finalization',
  render: renderModalWithIdentity(OWNER_WALLET_IDENTITY, {
    initialLifecycleState: 'selfExecuteFailed',
    initialFlowError: 'User rejected the finalization transaction request in the wallet.',
  }),
}

export const AutomaticallyFinalizedSuccess: Story = {
  name: '09a · Automatic Success, VPI Pending',
  render: renderModal({
    initialLifecycleState: 'executed',
    initialFinalExecutionEconomicsVersion: undefined,
    currentPosition: executedPosition,
    orderHistory: automaticallyFinalizedOrderHistory,
  }),
}

export const LiveOracleExactEvidence: Story = {
  name: '09b · Live Oracle, Exact Evidence',
  render: renderModal({
    initialLifecycleState: 'executed',
    initialFinalExecutionOraclePrice: 102_570_000n,
    initialFinalVpiUsdc: 182_822_887n,
    currentPosition: executedPosition,
    orderHistory: automaticallyFinalizedOrderHistory,
  }),
}

export const NegativeVpiExactEvidence: Story = {
  name: '09c · Live Oracle, Negative VPI',
  render: renderModal({
    initialLifecycleState: 'executed',
    initialFinalExecutionOraclePrice: 102_570_000n,
    initialFinalVpiUsdc: -44_872_071n,
    currentPosition: executedPosition,
    orderHistory: automaticallyFinalizedOrderHistory,
  }),
}

export const FrozenOracleClose: Story = {
  name: '09d · Frozen Oracle Close, VPI Credited',
  render: renderModal({
    initialLifecycleState: 'executed',
    initialDirection: 'short',
    initialReduceOnly: true,
    initialCommittedIsFullClose: true,
    initialCommittedPositionVpiAccrued: 60n * USDC,
    initialFinalExecutionOracleFrozen: true,
    initialFinalVpiUsdc: -2_875_000n,
    currentPosition: executedPosition,
    currentPositionSide: 'long',
    currentPositionAmount: '2 116',
  }),
}

export const SelfExecutedSuccess: Story = {
  name: '09e · Manually Finalized Success',
  render: renderModalWithIdentity(OWNER_WALLET_IDENTITY, {
    initialLifecycleState: 'executed',
    initialExecuteTxHash: SELF_REVEAL_TX,
    initialFinalVpiUsdc: 182_822_887n,
    currentPosition: executedPosition,
  }),
}

export const AutomaticCloseVpiCredited: Story = {
  name: '09f · Automatic Close, VPI Credited',
  render: renderModal({
    initialLifecycleState: 'executed',
    initialDirection: 'short',
    initialReduceOnly: true,
    initialCommittedIsFullClose: true,
    initialCommittedPositionVpiAccrued: 60n * USDC,
    initialFinalVpiUsdc: -12_300_000n,
    currentPosition: undefined,
    currentPositionAmount: '0',
  }),
}

export const AutomaticCloseVpiPaid: Story = {
  name: '09g · Automatic Close, VPI Paid',
  render: renderModal({
    initialLifecycleState: 'executed',
    initialDirection: 'short',
    initialReduceOnly: true,
    initialCommittedIsFullClose: true,
    initialCommittedPositionVpiAccrued: 60n * USDC,
    initialFinalVpiUsdc: 4_250_000n,
    currentPosition: undefined,
    currentPositionAmount: '0',
  }),
}

export const CommitRevertedWithDiagnostics: Story = {
  name: '10a · Commit Failed with Diagnostics',
  render: renderModal({
    initialLifecycleState: 'failed',
    initialFlowError: 'Commit reverted before creating an order, but the RPC did not return a contract error. Tx: 0x46cb...1cbb. Diagnostics: no pending order was emitted; refresh account state and check free margin, market state, and slippage.',
  }),
}

export const CommitWalletRejected: Story = {
  name: '10b · Wallet Rejected Commit',
  render: renderModal({
    initialLifecycleState: 'failed',
    initialFlowError: 'User rejected the commit transaction request in the wallet.',
  }),
}
