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
const KEEPER_REVEAL_TX = '0x6c0d00000000000000000000000000000000000000000000000000000000b7d3'
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

const keeperExecutedOrderHistory = [
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
    revealTxHash: KEEPER_REVEAL_TX,
  },
] satisfies PerpsOrderHistoryRow[]

const baseModalArgs = {
  initialReviewOpen: true,
  initialDirection: 'long',
  initialSize: '2116',
  initialOrderId: ORDER_ID,
  initialPositionSnapshotAtCommit: { exists: false, size: 0n },
  initialCommitTxHash: COMMIT_TX,
  initialFinalExecutionPrice: FINAL_PRICE_RAW,
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
    <div className="min-h-screen bg-cyber-bg p-4 md:p-8">
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

export const KeeperExecutedSuccess: Story = {
  render: renderModal({
    initialLifecycleState: 'executed',
    currentPosition: executedPosition,
    orderHistory: keeperExecutedOrderHistory,
  }),
}

export const SelfExecutedSuccess: Story = {
  render: renderModal({
    initialLifecycleState: 'executed',
    initialExecuteTxHash: SELF_REVEAL_TX,
    currentPosition: executedPosition,
  }),
}

export const KeeperWaiting: Story = {
  render: renderModal({
    initialLifecycleState: 'revealPending',
  }),
}

export const KeeperOverdue: Story = {
  render: renderModal({
    initialLifecycleState: 'selfExecuteAvailable',
  }),
}

export const RevealNotReady: Story = {
  render: renderModal({
    initialLifecycleState: 'selfExecuteAvailable',
    initialFlowError: 'Reveal is not ready yet. Execution must happen after the commit block.',
  }),
}

export const HermesRateLimited: Story = {
  render: renderModal({
    initialLifecycleState: 'selfExecuteAvailable',
    initialFlowError: 'Hermes rate limit reached while fetching historical Pyth data. Retry shortly.',
  }),
}

export const HistoricalPythDataRequired: Story = {
  render: renderModal({
    initialLifecycleState: 'selfExecuteAvailable',
    initialFlowError: 'Historical Pyth update was unavailable for the first post-commit tick. Wait for the cache to backfill, then retry self execute.',
  }),
}

export const HistoricalPythRejected: Story = {
  render: renderModal({
    initialLifecycleState: 'selfExecuteFailed',
    initialFlowError: 'Historical Pyth update was rejected by the router with stale-price error. The payload did not contain the exact first post-commit tick.',
  }),
}

export const OrderNoLongerPending: Story = {
  render: renderModal({
    initialLifecycleState: 'selfExecuteFailed',
    initialFlowError: 'Order 62 is no longer pending, but your position did not change. It likely failed or expired before execution. Refresh order history for the terminal event.',
  }),
}

export const SelfExecuteWalletRejected: Story = {
  render: renderModal({
    initialLifecycleState: 'selfExecuteFailed',
    initialFlowError: 'User rejected the reveal transaction request in the wallet.',
  }),
}

export const CommitRevertedWithDiagnostics: Story = {
  render: renderModal({
    initialLifecycleState: 'failed',
    initialFlowError: 'Commit reverted before creating an order, but the RPC did not return a contract error. Tx: 0x46cb...1cbb. Diagnostics: no pending order was emitted; refresh account state and check free margin, market state, and slippage.',
  }),
}
