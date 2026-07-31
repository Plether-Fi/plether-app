import type { ComponentProps } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'
import type { PerpsOrderHistoryRow, PerpsPosition } from '../hooks'

const meta: Meta<typeof PerpsTradeTicket> = {
  title: 'Perps/Final Reveal Modal',
  component: PerpsTradeTicket,
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',
  },
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
    commitTxHash: COMMIT_TX,
    revealTxHash: AUTO_FINALIZE_TX,
  },
] satisfies PerpsOrderHistoryRow[]

const baseModalArgs = {
  initialReviewOpen: true,
  initialDirection: 'long',
  initialSize: '2116',
  initialOrderId: ORDER_ID,
  initialCommitTxHash: COMMIT_TX,
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

export const AutomaticallyFinalizedSuccess: Story = {
  name: 'Automatically Finalized Success',
  render: renderModal({
    initialLifecycleState: 'executed',
    currentPosition: executedPosition,
    orderHistory: automaticallyFinalizedOrderHistory,
  }),
}

export const SelfExecutedSuccess: Story = {
  name: 'Manually Finalized Success',
  render: renderModal({
    initialLifecycleState: 'executed',
    initialExecuteTxHash: SELF_REVEAL_TX,
    currentPosition: executedPosition,
  }),
}

export const FinalizingPrice: Story = {
  name: 'Finalizing Price',
  render: renderModal({
    initialLifecycleState: 'revealPending',
    showFinalizationProgress: true,
  }),
}

export const ManualFinalizationReady: Story = {
  name: 'Manual Finalization Ready',
  render: renderModal({
    initialLifecycleState: 'selfExecuteAvailable',
  }),
}

export const FinalPriceNotReady: Story = {
  name: 'Final Price Not Ready',
  render: renderModal({
    initialLifecycleState: 'selfExecuteAvailable',
    initialFlowError: 'Final price is not ready yet. Execution must happen after the commit block.',
  }),
}

export const PriceDataRateLimited: Story = {
  name: 'Price Data Rate Limited',
  render: renderModal({
    initialLifecycleState: 'selfExecuteAvailable',
    initialFlowError: 'Price data service rate limit reached while fetching historical market data. Retry shortly.',
  }),
}

export const HistoricalPriceDataRequired: Story = {
  name: 'Historical Price Data Required',
  render: renderModal({
    initialLifecycleState: 'selfExecuteAvailable',
    initialFlowError: 'Historical price data was unavailable for the first post-commit tick. Wait for the cache to backfill, then retry finalizing.',
  }),
}

export const HistoricalPriceDataRejected: Story = {
  name: 'Historical Price Data Rejected',
  render: renderModal({
    initialLifecycleState: 'selfExecuteFailed',
    initialFlowError: 'Historical price data was rejected with a stale-price error. The payload did not contain the exact first post-commit tick.',
  }),
}

export const OrderNoLongerPending: Story = {
  render: renderModal({
    initialLifecycleState: 'selfExecuteFailed',
    initialFlowError: 'Order 62 is no longer pending, but your position did not change. It likely failed or expired before execution. Refresh order history for the terminal event.',
  }),
}

export const ManualFinalizationWalletRejected: Story = {
  name: 'Manual Finalization Wallet Rejected',
  render: renderModal({
    initialLifecycleState: 'selfExecuteFailed',
    initialFlowError: 'User rejected the finalization transaction request in the wallet.',
  }),
}

export const CommitRevertedWithDiagnostics: Story = {
  render: renderModal({
    initialLifecycleState: 'failed',
    initialFlowError: 'Commit reverted before creating an order, but the RPC did not return a contract error. Tx: 0x46cb...1cbb. Diagnostics: no pending order was emitted; refresh account state and check free margin, market state, and slippage.',
  }),
}
