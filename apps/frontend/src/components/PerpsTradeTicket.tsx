import { type CSSProperties, type ReactNode, useCallback, useEffect, useMemo, useRef, useState } from 'react'
import type { SponsoredExecutionStatus } from '@plether/perps-aa-client'
import { useChainId, useReadContracts } from 'wagmi'
import { zeroAddress } from 'viem'
import { openAppKit } from '../config/wagmi'
import { PERPS_CFD_ENGINE_LENS_ABI } from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import {
  PERPS_EXECUTION_MODE_LABELS,
  type PreparedPerpsOrderV2,
} from '../contracts/perpsOrderV2'
import type { BasketLatest } from '../api'
import type { PerpsMarketPhase } from '../utils/perpsMarketSchedule'
import type { PerpsOrderHistoryRow, PerpsPendingOrder, PerpsPosition } from '../hooks'
import { usePerpsTrading, useSwitchToArbitrumSepolia, waitForPerpsOrderTerminal } from '../hooks'
import { getExplorerTxUrl } from '../utils/explorer'
import { usePerpsUiStore } from '../stores/perpsUiStore'
import {
  findBundlerRequestError,
  usePerpsIdentity,
  useSponsoredOperationStore,
} from '../perps-aa'
import {
  directionToPerpsSide,
  dxyExposureFromContractNotional,
  formatDisplayDxyPrice,
  formatPerpsNumber,
  formatSignedPerpsUsdc,
  formatPerpsUsdc,
  getPerpsTargetPrice,
  notionalUsdcToSizeDelta,
  oraclePriceToDisplayDxyPrice,
  parsePerpsUsdc,
  sizeDeltaToNotionalUsdc,
  type PerpsBasketComponentPrice,
  type PerpsDirection,
  type PerpsOracleFreshness,
} from '../utils/perps'
import {
  calculateRawBasketOracleConfidenceSpreadPercent,
  formatAdverseConfidenceMultiplier,
  formatAdverseOracleConfidenceSpread,
  formatOracleConfidenceSpreadPercent,
} from '../utils/perpsOracleConfidence'
import {
  perpsChainState,
  perpsConnectedState,
  perpsErrorCategory,
  perpsSizeBucket,
  trackPerpsButtonClicked,
  trackPerpsMarginLifecycle,
  trackPerpsOrderLifecycle,
  trackPerpsValidationBlocked,
  type PerpsAnalyticsProperties,
} from '../analytics/perps'
import {
  getPerpsCloseInvalidReasonMessage,
  getPerpsOpenRevertMessage,
  getPerpsOrderFailureMessage,
} from '../utils/perpsErrors'
import { getOpenCapacityUnavailableMessage } from '../utils/perpsTradeTicketMessages'
import { DOCS_LINKS } from '../config/docs'
import { PerpsFinalizationConfetti } from './PerpsFinalizationConfetti'
import { Button, INFO_TOOLTIP_PANEL_CLASS_NAME, InfoTooltip, Input, Modal, SuccessIcon, TokenAmount, TokenLabel, Tooltip, type TooltipDocsLink } from './ui'

type Direction = PerpsDirection
export type TradeLifecycleState =
  | 'preview'
  | 'commitPreparing'
  | 'commitPending'
  | 'commitConfirmed'
  | 'revealPending'
  | 'selfExecuteAvailable'
  | 'selfExecutePending'
  | 'selfExecuteFailed'
  | 'executed'
  | 'failed'
type OrderLifecycleStep = 'preview' | 'commit' | 'reveal'
type MarginAction = 'deposit' | 'withdraw'
type MarginActionStatus = 'idle' | 'pending' | 'funding' | 'depositing' | 'failed'
type CleanupStatus = 'idle' | 'pending' | 'failed'

function readMutableRef<T>(ref: { current: T }): T {
  return ref.current
}

interface PreviewRowBase {
  label: string
  value: ReactNode
  tone?: 'default' | 'positive' | 'warning' | 'muted'
}

type PreviewRow = PreviewRowBase & (
  | {
      tooltip?: undefined
      tooltipDocsLink?: never
    }
  | {
      tooltip: ReactNode
      tooltipDocsLink: TooltipDocsLink
    }
)

interface ContractResult {
  status: 'failure' | 'success'
  result?: unknown
  error?: unknown
}

type TradePreviewRequest =
  | {
      kind: 'close'
      account: `0x${string}`
      sizeDelta: bigint
      oraclePrice: bigint
    }
  | {
      kind: 'open'
      account: `0x${string}`
      side: ReturnType<typeof directionToPerpsSide>
      sizeDelta: bigint
      marginDelta: bigint
      oraclePrice: bigint
      publishTime: bigint
    }

interface OpenPreviewView {
  valid: boolean
  invalidReason: number
  failureCategory: number
  executionPrice: bigint
  sizeDelta: bigint
  notionalUsdc: bigint
  marginDeltaUsdc: bigint
  vpiUsdc: bigint
  executionFeeUsdc: bigint
  tradeCostUsdc: bigint
  poolRebatePayoutUsdc: bigint
  pendingCarryUsdc: bigint
  initialMarginRequirementUsdc: bigint
  maintenanceMarginUsdc: bigint
  postSize: bigint
  postMarginUsdc: bigint
  postEntryPrice: bigint
  postVpiAccrued: bigint
  postUnrealizedPnlUsdc: bigint
  postEquityUsdc: bigint
  postHealthBps: bigint
  postLiquidatable: boolean
  hasLiquidationPrice: boolean
  liquidationPrice: bigint
}

interface ClosePreviewView {
  valid: boolean
  invalidReason: number
  executionPrice: bigint
  sizeDelta: bigint
  realizedPnlUsdc: bigint
  vpiDeltaUsdc: bigint
  vpiUsdc: bigint
  executionFeeUsdc: bigint
  remainingSize: bigint
  remainingMargin: bigint
  frozenSpreadUsdc: bigint
  frozenSpreadPaidUsdc: bigint
  frozenSpreadWaivedUsdc: bigint
}

interface PerpsTradeTicketProps {
  initialLifecycleState?: TradeLifecycleState
  initialReviewOpen?: boolean
  initialDirection?: Direction
  initialSize?: string
  initialReduceOnly?: boolean
  initialLeverage?: number
  initialMarginAction?: MarginAction
  initialMarginActionAmount?: string
  initialMarginCallSimulatorConfirmationOpen?: boolean
  initialOrderId?: bigint
  initialCommitTxHash?: string
  initialExecuteTxHash?: string
  /** Static UserOperation hash for deterministic stories and tests. */
  initialUserOperationHash?: string
  /** Static sponsored-operation status for deterministic stories and tests. */
  initialCommitExecutionStatus?: SponsoredExecutionStatus
  /** Static delayed-wallet warning for deterministic stories and tests. */
  initialWalletRequestWarning?: string
  initialFinalExecutionPrice?: bigint
  initialFinalExecutionOraclePrice?: bigint
  initialFinalExecutionOracleFrozen?: boolean
  initialFinalFrozenCloseSpreadUsdc?: bigint
  initialFinalExecutionEconomicsVersion?: number
  /** Static exact VPI evidence for deterministic stories and tests. */
  initialFinalVpiUsdc?: bigint
  /** Static committed VPI estimate for deterministic stories and tests. */
  initialCommittedVpiUsdc?: bigint
  /** Static committed position VPI balance for deterministic stories and tests. */
  initialCommittedPositionVpiAccrued?: bigint
  /** Static full-close intent for deterministic finalized stories and tests. */
  initialCommittedIsFullClose?: boolean
  initialCommittedSizeDelta?: bigint
  initialFlowError?: string
  closePositionRequestId?: number
  currentPositionSide?: Direction
  currentPositionAmount?: string
  enableLiveTrading?: boolean
  showFinalizationProgress?: boolean
  oraclePriceRaw?: bigint
  oraclePublishTime?: number
  oraclePriceDisplay?: string
  latestBasket?: BasketLatest
  adverseConfidenceMultiplierBps?: string
  oracleFrozen?: boolean
  /** Static preview data for non-live stories and design review. Ignored when live trading is enabled. */
  openPreviewFixture?: OpenPreviewView
  /** Static preview data for non-live stories and design review. Ignored when live trading is enabled. */
  closePreviewFixture?: ClosePreviewView
  /** Static validation message for non-live stories and design review. Ignored when live trading is enabled. */
  validationErrorFixture?: string
  /** Static V2 protections for deterministic stories and design review. Ignored when live trading is enabled. */
  executionProtectionsFixture?: PreparedPerpsOrderV2
  oracleFreshness?: PerpsOracleFreshness
  oracleFreshnessTooltip?: string
  oracleBasketComponents?: readonly PerpsBasketComponentPrice[]
  availableToTradeRaw?: bigint
  availableToTradeAmount?: string
  portfolioValueRaw?: bigint
  withdrawableUsdcRaw?: bigint
  walletUsdcRaw?: bigint
  ownerWalletUsdcRaw?: bigint
  tradingAccountUsdcRaw?: bigint
  marginAllowanceUsdc?: bigint
  currentPosition?: PerpsPosition
  pendingOrders?: PerpsPendingOrder[]
  orderHistory?: PerpsOrderHistoryRow[]
  ordersIndexedThroughBlockRaw?: bigint
  pendingOrderCount?: number
  activePositionProtectionId?: bigint
  maxPendingOrders?: bigint
  firstPendingOrderId?: bigint
  firstPendingOrderExpiryTime?: bigint
  longOpenCapacityUsdc?: bigint
  shortOpenCapacityUsdc?: bigint
  minOpenNotionalUsdc?: bigint
  minNewPositionNotionalUsdc?: bigint
  maintenanceMarginBps?: bigint
  executionFeeBps?: bigint
  marketPhase?: PerpsMarketPhase
  marketCurrentDuration?: string
  onAccountRefresh?: () => Promise<unknown>
}

const MOCK_PREVIEW_PRICE = 0.9909
const TRADE_PREVIEW_DEBOUNCE_MS = 300
const AVAILABLE_TO_TRADE_AMOUNT = '18 420'
const CURRENT_POSITION_AMOUNT = '8 200'
const ORDER_ID = '0x7f21...9c04'
const COMMIT_TX = '0x4a6b9f1e7c2d8a5b3c9012f4e6d7c8b9a0f123456789abcdef0123456788e2'
const EXECUTE_TX = '0xa91d6c4f83b27e10d55a4c0e29f8b6a73219d4e5c8b70af11223344556634bf'

function useDebouncedValue<T>(value: T, delayMs: number): T {
  const [debouncedValue, setDebouncedValue] = useState(value)

  useEffect(() => {
    if (Object.is(value, debouncedValue)) return undefined

    const timeout = globalThis.setTimeout(() => {
      setDebouncedValue(value)
    }, delayMs)

    return () => {
      globalThis.clearTimeout(timeout)
    }
  }, [debouncedValue, delayMs, value])

  return debouncedValue
}
const SLIPPAGE_OPTIONS = [0, 0.05, 0.1, 0.25, Infinity]
const DEFAULT_LIVE_SLIPPAGE = 0.1
const DEFAULT_ORACLE_FROZEN_SLIPPAGE = 0
const LIGHT_ORANGE_ACTION_BUTTON_CLASS = '!border-[#FFAB96] !bg-[#FFAB96] !text-[#250917] enabled:hover:!border-[#FF572D] enabled:hover:!bg-[#FF572D] enabled:hover:!text-[#FFF5F9] enabled:hover:underline enabled:hover:underline-offset-4'
const DARK_CANCEL_BUTTON_CLASS = '!border-[#FFAB96]/40 !bg-[#250917] !text-[#FFF5F9] enabled:hover:!border-[#FFAB96] enabled:hover:!bg-[#3B212D] enabled:hover:underline enabled:hover:underline-offset-4'
const CONNECT_WALLET_ACTION_BUTTON_CLASS = '!border-[#FF572D] !bg-[#FF572D] !text-[#FFF5F9] enabled:hover:!border-[#FFF5F9] enabled:hover:!bg-[#FFF5F9] enabled:hover:!text-[#250917] enabled:hover:underline enabled:hover:underline-offset-4'
const EXECUTION_FEE_BPS = 4
const USDC_UNIT = 1_000_000n
const PREVIEW_USDC_DECIMALS = 1
const OPEN_BOUNTY_BPS_RAW = 1n
const MIN_OPEN_BOUNTY_USDC_RAW = 10_000n
const MAX_OPEN_BOUNTY_USDC_RAW = 200_000n
const CLOSE_BOUNTY_USDC_RAW = 200_000n
const SUMMARY_CLOSE_DUST_USDC_RAW = 10_000n
const ORACLE_PRICE_FRESH_SECONDS = 60
const DEFAULT_MAX_LEVERAGE = 33
const PREVIEW_LOADING_VALUE = 'Loading'
const PREVIEW_UNAVAILABLE_VALUE = 'Unavailable'
const COMPACT_PREVIEW_ROW_LABELS = new Set([
  'plDXY Perp price',
  'Required margin',
  'Max slippage',
  'Execution limit',
  'Liquidation price',
  'Estimated fee',
  'Position VPI balance',
  'VPI',
])
const VPI_PRICE_IMPACT_TOOLTIP =
  'Virtual Price Impact (VPI) is the protocol skew adjustment for a trade. It is calculated from trade size, direction, current long/short skew, available pool depth, and the protocol VPI factor. Positive values are a cost; negative values are a rebate.'
const CLOSE_VPI_TOOLTIP =
  'For a close or reduction, positive VPI is paid from the Margin Account and negative VPI is credited to the Margin Account settlement after the lifetime VPI clamp. A credit is not sent directly to the owner wallet. The preview can change before execution.'
const FINAL_CLOSE_VPI_TOOLTIP =
  'This is the VPI settled for the close or reduction. Paid VPI was charged to the Margin Account; credited VPI was added to the Margin Account settlement after the lifetime VPI clamp, not sent directly to the owner wallet.'
const FINAL_POSITION_VPI_BALANCE_TOOLTIP =
  'This is the signed aggregate VPI balance on the position immediately before this close or reduction. It is shown with the transaction VPI so you can compare the lifetime position balance with the amount settled by this transaction. A full close leaves no remaining position VPI balance.'
const POSITION_VPI_BALANCE_TOOLTIP =
  'The position\'s signed net VPI over its lifecycle. Net paid VPI can support a future closing credit. A provisional credit has already been added to settlement, remains excluded from risk equity, and may be reconciled on close. Partial-reduction limits are applied automatically to the VPI estimate.'
const ORACLE_CONFIDENCE_SPREAD_TOOLTIP =
  'Execution uses the adverse side of the Pyth confidence range for opens and for live or FAD-only closes. Oracle-frozen voluntary closes/reductions waive that price shift and use the separate frozen close spread instead; confidence-width validation still applies.'
const FROZEN_CLOSE_SPREAD_TOOLTIP =
  'Oracle-frozen closes/reductions use this fixed LP-owned spread instead of the adverse-confidence price shift to protect LPs from price uncertainty. Wait until the market reopens to avoid this spread.'
const DIRECTION_TOOLTIP =
  'LONG USD benefits when the displayed price rises; SHORT USD benefits when it falls. The underlying FX basket moves in the opposite direction.'
const CONTRACT_NOTIONAL_TOOLTIP =
  'The protocol\'s accounting size, calculated using the raw basket price. It is different from your displayed plDXY Perp exposure and determines margin and fees.'
const EXECUTION_LIMIT_TOOLTIP =
  'The worst oracle execution price you accept. It does not limit VPI, fees, carry, execution rewards, or a frozen-close spread.'
const MAINTENANCE_MARGIN_TOOLTIP =
  'The minimum account equity required to avoid liquidation. At or below this amount, the entire position can be liquidated.'
const EXECUTION_REWARD_TOOLTIP =
  'USDC reserved for whoever finalizes or clears the order. It can still be paid if the order fails or expires.'
const MANUAL_FINALIZATION_TOOLTIP =
  'Unless marked Sponsored, manual finalization requires ETH for network gas and the Pyth update fee.'
const KEEPER_REVEAL_GRACE_MS = 20_000
const KEEPER_REVEAL_PROGRESS_MS = 250
const FINALIZATION_MESSAGE_ROTATE_MS = 4_000
const ORDER_TERMINAL_WAIT_SECONDS = 60
const ORDER_TERMINAL_RETRY_DELAY_MS = 2_000
const ORDER_EXECUTION_EVIDENCE_POLL_MS = 60_000
const FINALIZATION_LOADING_MESSAGES = [
  {
    title: 'Waiting for verified market data',
    subtitle: 'Using signed oracle data for the order window before settling the trade.',
  },
  {
    title: 'Reducing MEV exposure',
    subtitle: 'Your order was committed before the final settlement price is used.',
  },
  {
    title: 'Limiting value extraction',
    subtitle: 'Settling from committed order parameters instead of a last-second click race.',
  },
  {
    title: 'Checking price limits',
    subtitle: 'Comparing the final market price with your acceptable price.',
  },
  {
    title: 'Confirming settlement conditions',
    subtitle: 'Checking the order is ready, unexpired, and eligible to finalize.',
  },
  {
    title: 'Preparing onchain finalization',
    subtitle: 'Submitting the transaction that settles the committed order.',
  },
  {
    title: 'Verifying the final price',
    subtitle: 'Reading the price that will be recorded for this order.',
  },
  {
    title: 'Checking margin accounting',
    subtitle: 'Calculating margin, fees, and resulting position size together.',
  },
  {
    title: 'Keeping collateral accounting consistent',
    subtitle: 'Matching collateral changes to the new position state.',
  },
  {
    title: 'Verifying solvency after execution',
    subtitle: 'Checking the account remains properly collateralized after settlement.',
  },
  {
    title: 'Checking protocol solvency',
    subtitle: 'Verifying system accounting remains collateral-backed.',
  },
  {
    title: 'Reconciling exposure against collateral',
    subtitle: 'Comparing position exposure against the margin backing it.',
  },
  {
    title: 'Making the button race irrelevant',
    subtitle: 'Automatic finalization gets the first chance before manual action appears.',
  },
] as const
type FinalizationLoadingMessage = (typeof FINALIZATION_LOADING_MESSAGES)[number]

function isPerpsCommitDebugEnabled(): boolean {
  if (import.meta.env.MODE === 'test') return false
  if (import.meta.env.DEV) return true

  try {
    return globalThis.localStorage.getItem('PLETHER_PERPS_DEBUG') === '1'
  } catch {
    return false
  }
}

function debugPerpsCommit(stage: string, details?: Record<string, unknown>): void {
  if (!isPerpsCommitDebugEnabled()) return
  if (details === undefined) {
    console.info(`[perps:commit] ${stage}`)
    return
  }
  console.info(`[perps:commit] ${stage}`, details)
}

function randomFinalizationMessage(currentTitle?: string, shownTitles?: ReadonlySet<string>): FinalizationLoadingMessage {
  const unseenMessages = FINALIZATION_LOADING_MESSAGES.filter((message) => (
    message.title !== currentTitle && !shownTitles?.has(message.title)
  ))
  const fallbackMessages = FINALIZATION_LOADING_MESSAGES.filter((message) => message.title !== currentTitle)
  const messages = unseenMessages.length > 0
    ? unseenMessages
    : fallbackMessages.length > 0
      ? fallbackMessages
      : FINALIZATION_LOADING_MESSAGES

  return messages[Math.floor(Math.random() * messages.length)]
}

function isPythExpiryMessage(message: string): boolean {
  const lowerMessage = message.toLowerCase()
  return (
    lowerMessage.includes('pyth price data expired') ||
    lowerMessage.includes('stale-price error') ||
    lowerMessage.includes('historical pyth update was unavailable') ||
    lowerMessage.includes('historical price data was unavailable') ||
    lowerMessage.includes('router could not use the historical pyth update') ||
    lowerMessage.includes('historical pyth update was rejected') ||
    lowerMessage.includes('historical price data was rejected') ||
    lowerMessage.includes('hermes rate limit reached') ||
    lowerMessage.includes('price data service rate limit reached')
  )
}

function isHermesRateLimitMessage(message: string): boolean {
  const lowerMessage = message.toLowerCase()
  return lowerMessage.includes('hermes rate limit reached') ||
    lowerMessage.includes('price data service rate limit reached')
}

function isHistoricalPythRejectedMessage(message: string): boolean {
  const lowerMessage = message.toLowerCase()
  return lowerMessage.includes('historical pyth update was rejected') ||
    lowerMessage.includes('historical price data was rejected')
}

function isRevealNotReadyMessage(message: string): boolean {
  const lowerMessage = message.toLowerCase()
  return lowerMessage.includes('execution must happen after the commit block') ||
    lowerMessage.includes('reveal is not ready yet') ||
    lowerMessage.includes('order reveal is not ready yet')
}

function isRetryableSelfExecuteMessage(message: string): boolean {
  return isPythExpiryMessage(message) || isRevealNotReadyMessage(message)
}

function isOrderNoLongerPendingMessage(message: string): boolean {
  return message.toLowerCase().includes('no longer pending')
}

function isTerminalOrderFailureMessage(message: string): boolean {
  return message.toLowerCase().startsWith('order failed:')
}

function failureReasonMessage(reason: string | undefined): string | undefined {
  if (!reason) return undefined
  const messages: Record<string, string> = {
    Expired: 'The order expired before execution. Review and create a fresh order.',
    Slippage: 'Execution exceeded the reviewed target price.',
    SlippageExceeded: 'Execution exceeded the reviewed target price.',
    ConfigMismatch: 'Protocol configuration changed after review.',
    'Config mismatch': 'Protocol configuration changed after review.',
    ExecutionModeDisallowed: 'The market regime changed after review.',
    'Mode disallowed': 'The market regime changed after review.',
    RiskOff: 'The order was invalidated by protocol risk-off policy.',
    'Risk off': 'The order was invalidated by protocol risk-off policy.',
    PlannerRejected: 'The execution planner rejected the order.',
    'Planner rejected': 'The execution planner rejected the order.',
    ConstraintViolation: 'Execution would violate the reviewed financial bounds.',
    'Constraint violation': 'Execution would violate the reviewed financial bounds.',
    AccountLiquidated: 'The account was liquidated before this order executed.',
    'Account liquidated': 'The account was liquidated before this order executed.',
    // Legacy history rows remain readable after the V2 cutover.
    CloseOnly: getPerpsOrderFailureMessage(1),
    EnginePanic: getPerpsOrderFailureMessage(3),
    EngineRevert: getPerpsOrderFailureMessage(5),
  }
  return messages[reason]
}

function terminalOrderFailureMessage(order: PerpsOrderHistoryRow): string {
  const detail = failureReasonMessage(order.failureReason)
    ?? `Terminal status: ${order.status}. Refresh order history for details.`
  return `Order failed: ${detail}`
}

function hasCompleteExecutionEvidence(order: PerpsOrderHistoryRow): boolean {
  return order.status !== 'Executed'
    || (order.receiptHash !== undefined && order.executionEconomicsVersion === 2)
    || (
      order.oracleDerivationVersion !== undefined
      && order.executionEconomicsVersion !== undefined
      && order.executionOracleFrozen !== undefined
    )
}

function terminalOrderKey(order: PerpsOrderHistoryRow): string {
  return [
    order.orderId.toString(),
    order.status,
    order.clientOrderId?.toLowerCase() ?? '',
    order.commitTxHash?.toLowerCase() ?? '',
    order.revealTxHash?.toLowerCase() ?? '',
    order.terminalBlockNumberRaw?.toString() ?? '',
  ].join(':')
}

function orderMatchesCommittedIdentity(
  order: PerpsOrderHistoryRow,
  identity: {
    account: string
    clientOrderId: string
    hash?: string
    orderId: bigint
  }
): boolean {
  if (order.orderId !== identity.orderId) return false
  if (
    order.account &&
    order.account.toLowerCase() !== identity.account.toLowerCase()
  ) return false
  if (order.clientOrderId) {
    return order.clientOrderId.toLowerCase() ===
      identity.clientOrderId.toLowerCase()
  }
  if (order.commitTxHash && identity.hash) {
    return order.commitTxHash.toLowerCase() === identity.hash.toLowerCase()
  }
  // Legacy API rows may not yet carry the V2 identity. The account-scoped
  // order id remains sufficient until those rows are backfilled.
  return true
}

const ORDER_LIFECYCLE_STEPS: { id: OrderLifecycleStep; label: string }[] = [
  { id: 'preview', label: 'Preview' },
  { id: 'commit', label: 'Commit' },
  { id: 'reveal', label: 'Finalize' },
]

function parseAmount(value: string): number {
  const parsed = Number(value.replaceAll(',', '').replaceAll(' ', ''))
  return Number.isFinite(parsed) ? parsed : 0
}

function formatUsdcAmount(value: number): string {
  return value.toLocaleString('en-US', {
    minimumFractionDigits: PREVIEW_USDC_DECIMALS,
    maximumFractionDigits: PREVIEW_USDC_DECIMALS,
  }).replaceAll(',', ' ')
}

function formatUsdc(value: number): ReactNode {
  return <TokenAmount amount={formatUsdcAmount(value)} />
}

function formatPreviewUsdcRaw(value: bigint | undefined): string {
  if (value === undefined) return '--'
  return formatPerpsNumber(Number(value) / Number(USDC_UNIT), PREVIEW_USDC_DECIMALS, PREVIEW_USDC_DECIMALS)
}

function formatUsdcRaw(value: bigint | undefined): ReactNode {
  return <TokenAmount amount={formatPreviewUsdcRaw(value)} />
}

function formatSignedUsdcNoPlus(value: bigint | undefined): ReactNode {
  if (value === undefined) return 'Unavailable'
  const sign = value < 0n ? '-' : ''
  const absolute = value < 0n ? -value : value
  return <TokenAmount amount={`${sign}${formatPreviewUsdcRaw(absolute)}`} />
}

function formatTradeVpi(
  value: bigint | undefined,
  phase: 'estimate' | 'final',
  fallback: ReactNode = 'Unavailable',
  fallbackTone?: PreviewRow['tone']
): Pick<PreviewRow, 'value' | 'tone'> {
  if (value === undefined) return { value: fallback, tone: fallbackTone }
  if (value === 0n) return { value: 'No VPI' }

  const isCredit = value < 0n
  const absolute = isCredit ? -value : value
  const action = isCredit
    ? phase === 'estimate' ? 'Credit' : 'Credited'
    : phase === 'estimate' ? 'Pay' : 'Paid'
  const formattedAmount = formatPreviewUsdcRaw(absolute)

  return {
    value: (
      <span
        aria-label={`${action} ${formattedAmount} USDC`}
        className="inline-flex items-baseline justify-end gap-1.5 whitespace-nowrap"
      >
        <span>{action}</span>
        <TokenAmount amount={formattedAmount} />
      </span>
    ),
    tone: isCredit ? 'positive' : 'warning',
  }
}

function formatVpiBalance(
  value: bigint | undefined,
  fallback: ReactNode = 'Unavailable',
  fallbackTone?: PreviewRow['tone']
): Pick<PreviewRow, 'value' | 'tone'> {
  if (value === undefined) return { value: fallback, tone: fallbackTone }
  if (value === 0n) return { value: 'No VPI balance' }

  const isProvisionalCredit = value < 0n
  const absolute = isProvisionalCredit ? -value : value
  const status = isProvisionalCredit ? 'Provisional credit' : 'Net paid'
  const compactStatus = isProvisionalCredit ? 'Credit' : 'Paid'
  const formattedAmount = formatPreviewUsdcRaw(absolute)

  return {
    value: (
      <span
        aria-label={`${status} ${formattedAmount} USDC`}
        className="inline-flex items-baseline justify-end gap-1.5 whitespace-nowrap"
      >
        <span>{compactStatus}</span>
        <TokenAmount amount={formattedAmount} />
      </span>
    ),
    tone: isProvisionalCredit ? 'positive' : 'warning',
  }
}

function historyOrderIsClose(order: PerpsOrderHistoryRow | undefined): boolean | undefined {
  if (order === undefined) return undefined
  if (order.type === 'Close') return true
  if (order.type === 'Open') return false
  return undefined
}

function usdcRawToNumber(value: bigint | undefined): number {
  if (value === undefined) return 0
  return Number(value) / Number(USDC_UNIT)
}

function readResult(data: readonly ContractResult[] | undefined, index: number): unknown {
  const item = data?.[index]
  if (item?.status !== 'success') return undefined
  return item.result
}

function readFailure(data: readonly ContractResult[] | undefined, index: number): unknown {
  const item = data?.[index]
  if (item?.status !== 'failure') return undefined
  return item.error ?? true
}

function tupleValue(value: unknown, index: number, key: string): unknown {
  if (value && typeof value === 'object' && key in value) {
    return (value as Record<string, unknown>)[key]
  }

  if (Array.isArray(value)) return value[index]
  return undefined
}

function tupleBigInt(value: unknown, index: number, key: string): bigint {
  const raw = tupleValue(value, index, key)
  if (typeof raw === 'bigint') return raw
  if (typeof raw === 'number') return BigInt(raw)
  if (typeof raw === 'string') return BigInt(raw)
  return 0n
}

function parseOpenPreview(value: unknown): OpenPreviewView | undefined {
  if (!value) return undefined

  return {
    valid: Boolean(tupleValue(value, 0, 'valid')),
    invalidReason: Number(tupleValue(value, 1, 'invalidReason') ?? 0),
    failureCategory: Number(tupleValue(value, 2, 'failureCategory') ?? 0),
    executionPrice: tupleBigInt(value, 3, 'executionPrice'),
    sizeDelta: tupleBigInt(value, 4, 'sizeDelta'),
    notionalUsdc: tupleBigInt(value, 5, 'notionalUsdc'),
    marginDeltaUsdc: tupleBigInt(value, 6, 'marginDeltaUsdc'),
    vpiUsdc: tupleBigInt(value, 7, 'vpiUsdc'),
    executionFeeUsdc: tupleBigInt(value, 8, 'executionFeeUsdc'),
    tradeCostUsdc: tupleBigInt(value, 9, 'tradeCostUsdc'),
    poolRebatePayoutUsdc: tupleBigInt(value, 10, 'poolRebatePayoutUsdc'),
    pendingCarryUsdc: tupleBigInt(value, 11, 'pendingCarryUsdc'),
    initialMarginRequirementUsdc: tupleBigInt(value, 12, 'initialMarginRequirementUsdc'),
    maintenanceMarginUsdc: tupleBigInt(value, 13, 'maintenanceMarginUsdc'),
    postSize: tupleBigInt(value, 14, 'postSize'),
    postMarginUsdc: tupleBigInt(value, 15, 'postMarginUsdc'),
    postEntryPrice: tupleBigInt(value, 16, 'postEntryPrice'),
    postVpiAccrued: tupleBigInt(value, 17, 'postVpiAccrued'),
    postUnrealizedPnlUsdc: tupleBigInt(value, 18, 'postUnrealizedPnlUsdc'),
    postEquityUsdc: tupleBigInt(value, 19, 'postEquityUsdc'),
    postHealthBps: tupleBigInt(value, 20, 'postHealthBps'),
    postLiquidatable: Boolean(tupleValue(value, 21, 'postLiquidatable')),
    hasLiquidationPrice: Boolean(tupleValue(value, 22, 'hasLiquidationPrice')),
    liquidationPrice: tupleBigInt(value, 23, 'liquidationPrice'),
  }
}

function parseClosePreview(value: unknown): ClosePreviewView | undefined {
  if (!value) return undefined

  return {
    valid: Boolean(tupleValue(value, 0, 'valid')),
    invalidReason: Number(tupleValue(value, 1, 'invalidReason') ?? 0),
    executionPrice: tupleBigInt(value, 2, 'executionPrice'),
    sizeDelta: tupleBigInt(value, 3, 'sizeDelta'),
    realizedPnlUsdc: tupleBigInt(value, 4, 'realizedPnlUsdc'),
    vpiDeltaUsdc: tupleBigInt(value, 5, 'vpiDeltaUsdc'),
    vpiUsdc: tupleBigInt(value, 6, 'vpiUsdc'),
    executionFeeUsdc: tupleBigInt(value, 7, 'executionFeeUsdc'),
    remainingSize: tupleBigInt(value, 15, 'remainingSize'),
    remainingMargin: tupleBigInt(value, 16, 'remainingMargin'),
    frozenSpreadUsdc: tupleBigInt(value, 21, 'frozenSpreadUsdc'),
    frozenSpreadPaidUsdc: tupleBigInt(value, 22, 'frozenSpreadPaidUsdc'),
    frozenSpreadWaivedUsdc: tupleBigInt(value, 23, 'frozenSpreadWaivedUsdc'),
  }
}

function formatPercent(value: number): string {
  if (value === 0) return 'Exact'
  if (!Number.isFinite(value)) return 'Infinity'

  return `${value.toLocaleString('en-US', {
    maximumFractionDigits: 3,
    minimumFractionDigits: 0,
  })}%`
}

function formatLeverage(value: number): string {
  return `${value.toString()}x`
}

function formatLeverageRaw(notionalUsdc: bigint | undefined, marginUsdc: bigint | undefined): string {
  if (notionalUsdc === undefined || marginUsdc === undefined || marginUsdc <= 0n) return '--'
  return `${formatPerpsNumber(Number(notionalUsdc) / Number(marginUsdc), 2)}x`
}

function formatBpsPercent(value: bigint | undefined): string {
  if (value === undefined) return 'Unavailable'
  return formatPercent(Number(value) / 100)
}

function formatDuration(seconds: number): string {
  if (!Number.isFinite(seconds) || seconds <= 0) return 'now'

  const days = Math.floor(seconds / 86_400)
  const hours = Math.floor((seconds % 86_400) / 3_600)
  const minutes = Math.floor((seconds % 3_600) / 60)
  const remainingSeconds = seconds % 60
  const parts = [
    days > 0 ? `${days.toString()}d` : '',
    hours > 0 ? `${hours.toString()}h` : '',
    minutes > 0 ? `${minutes.toString()}m` : '',
    days === 0 && hours === 0 ? `${remainingSeconds.toString()}s` : '',
  ].filter(Boolean)

  return parts.join(' ')
}

function clampBigInt(value: bigint, min: bigint, max: bigint): bigint {
  if (value < min) return min
  if (value > max) return max
  return value
}

function minBigInt(a: bigint, b: bigint): bigint {
  return a < b ? a : b
}

function maxBigInt(a: bigint, b: bigint): bigint {
  return a > b ? a : b
}

function estimateOpenBountyUsdcRaw(notionalUsdc: bigint): bigint {
  if (notionalUsdc <= 0n) return 0n
  const rawBounty = (notionalUsdc * OPEN_BOUNTY_BPS_RAW) / 10_000n
  return clampBigInt(rawBounty, MIN_OPEN_BOUNTY_USDC_RAW, MAX_OPEN_BOUNTY_USDC_RAW)
}

function executionFeeUsdcRaw(notionalUsdc: bigint, executionFeeBps: bigint): bigint {
  if (notionalUsdc <= 0n || executionFeeBps <= 0n) return 0n
  return (notionalUsdc * executionFeeBps) / 10_000n
}

function maxOpenNotionalForMargin(availableUsdc: bigint, leverage: number): bigint {
  if (availableUsdc <= 0n || leverage <= 0) return 0n

  const leverageRaw = BigInt(leverage)
  let low = 0n
  let high = availableUsdc * leverageRaw

  while (low < high) {
    const midpoint = (low + high + 1n) / 2n
    const requiredUsdc = (midpoint / leverageRaw) + estimateOpenBountyUsdcRaw(midpoint)
    if (requiredUsdc <= availableUsdc) {
      low = midpoint
    } else {
      high = midpoint - 1n
    }
  }

  return (low / USDC_UNIT) * USDC_UNIT
}

function maxLeverageFromMaintenanceMargin(maintenanceMarginBps: bigint | undefined): number {
  if (maintenanceMarginBps === undefined || maintenanceMarginBps <= 0n) return DEFAULT_MAX_LEVERAGE

  const cap = Number(10_000n / maintenanceMarginBps)
  if (!Number.isFinite(cap) || cap <= 0) return DEFAULT_MAX_LEVERAGE

  return Math.max(DEFAULT_MAX_LEVERAGE, cap)
}

function directionLabel(direction: Direction): string {
  return direction === 'long' ? 'Long plDXY Perp' : 'Short plDXY Perp'
}

function OrderSummaryRawAmount({ value }: { value: bigint }) {
  return <span className="break-words sm:whitespace-nowrap">{formatPerpsUsdc(value)} USDC</span>
}

function truncateHash(hash: string): string {
  return `${hash.slice(0, 6)}...${hash.slice(-4)}`
}

function formatOptionalPrice(value: number | null | undefined): string {
  if (value === null) return 'Market'
  if (value === undefined || !Number.isFinite(value)) return '--'
  return value.toFixed(4)
}

function formatOracleAge(ageSeconds: number): string {
  if (!Number.isFinite(ageSeconds) || ageSeconds < 0) return 'unknown age'
  if (ageSeconds < 60) return `${ageSeconds.toString()}s ago`

  const minutes = Math.floor(ageSeconds / 60)
  const seconds = ageSeconds % 60
  if (minutes < 60) return seconds > 0 ? `${minutes.toString()}m ${seconds.toString()}s ago` : `${minutes.toString()}m ago`

  const hours = Math.floor(minutes / 60)
  const remainingMinutes = minutes % 60
  if (hours < 24) return remainingMinutes > 0 ? `${hours.toString()}h ${remainingMinutes.toString()}m ago` : `${hours.toString()}h ago`

  const days = Math.floor(hours / 24)
  const remainingHours = hours % 24
  return remainingHours > 0 ? `${days.toString()}d ${remainingHours.toString()}h ago` : `${days.toString()}d ago`
}

function DxyPricePreviewValue({
  value,
  publishTime,
  nowSeconds,
  freshness: freshnessOverride,
  freshnessTooltip: freshnessTooltipOverride,
}: {
  value: ReactNode
  publishTime?: number
  nowSeconds: number
  freshness?: PerpsOracleFreshness
  freshnessTooltip?: string
}) {
  const ageSeconds = publishTime === undefined ? undefined : Math.max(0, nowSeconds - publishTime)
  const inferredFreshness = ageSeconds === undefined ? undefined : ageSeconds <= ORACLE_PRICE_FRESH_SECONDS ? 'fresh' : 'stale'
  const freshness = freshnessOverride ?? inferredFreshness
  const dotClass = freshness === 'fresh'
    ? 'bg-positive'
    : freshness === 'market-closed'
      ? 'bg-warning'
      : freshness === 'stale'
        ? 'bg-brand-orange'
        : 'bg-[#FFAB96]'
  const freshnessTooltip = freshnessTooltipOverride ?? (ageSeconds === undefined ? undefined : `updated ${formatOracleAge(ageSeconds)}`)

  return (
    <span className="inline-flex min-h-6 max-w-full flex-wrap items-center justify-end gap-2">
      {freshness && freshnessTooltip ? (
        <Tooltip content={freshnessTooltip} position="top" className={INFO_TOOLTIP_PANEL_CLASS_NAME}>
          <span
            aria-label={`plDXY Perp price ${freshness}`}
            className={`h-2 w-2 shrink-0 rounded-full ${dotClass}`}
            tabIndex={0}
          />
        </Tooltip>
      ) : null}
      <span>{value}</span>
    </span>
  )
}

function displayDxyPriceNumber(rawOraclePrice: bigint | undefined): number | undefined {
  const formatted = formatDisplayDxyPrice(rawOraclePrice)
  if (formatted === '--') return undefined
  const value = Number(formatted.replaceAll(' ', ''))
  return Number.isFinite(value) ? value : undefined
}

function absBigInt(value: bigint): bigint {
  return value < 0n ? -value : value
}

function formatConfidenceSpread(value: bigint | undefined, midpointPrice: bigint | undefined): string {
  if (value === undefined || midpointPrice === undefined || midpointPrice <= 0n) return 'Unavailable'
  if (value === 0n) return '0.0000%'
  const percent = (Number(value) / Number(midpointPrice)) * 100
  if (!Number.isFinite(percent)) return 'Unavailable'
  return `~${formatPerpsNumber(percent, 4, 4)}%`
}

function dxyExposureToSizeDelta(dxyExposureUsdc: bigint, rawOraclePrice: bigint | undefined): bigint | undefined {
  const displayDxyPrice = oraclePriceToDisplayDxyPrice(rawOraclePrice)
  if (displayDxyPrice === undefined || displayDxyPrice <= 0n) return undefined
  return notionalUsdcToSizeDelta(dxyExposureUsdc, displayDxyPrice)
}

function TxHashActions({ hash }: { hash: string }) {
  return (
    <span className="inline-flex items-center justify-end gap-1 whitespace-nowrap">
      <span>{truncateHash(hash)}</span>
      <button
        type="button"
        aria-label="Copy tx hash"
        title="Copy tx hash"
        className="inline-flex h-4 w-4 items-center justify-center text-content-secondary/70 transition-colors hover:text-[#FFAB96]"
        onClick={() => {
          void navigator.clipboard.writeText(hash)
        }}
      >
        <span className="material-symbols-outlined !text-[14px] !leading-none">content_copy</span>
      </button>
      <a
        aria-label="Open tx in block explorer"
        title="Open in block explorer"
        href={getExplorerTxUrl(PERPS_ARBITRUM_SEPOLIA_CHAIN_ID, hash)}
        target="_blank"
        rel="noopener noreferrer"
        className="inline-flex h-4 w-4 items-center justify-center text-content-secondary/70 transition-colors hover:text-[#FFAB96]"
      >
        <span className="material-symbols-outlined !text-[14px] !leading-none">open_in_new</span>
      </a>
    </span>
  )
}

function CopyableValue({
  ariaLabel,
  value,
}: {
  ariaLabel: string
  value: string
}) {
  return (
    <span className="inline-flex min-w-0 max-w-full items-center justify-end gap-1">
      <span className="min-w-0 truncate" title={value}>{value}</span>
      <button
        type="button"
        aria-label={ariaLabel}
        title={ariaLabel}
        className="inline-flex h-4 w-4 shrink-0 items-center justify-center text-content-secondary/70 transition-colors hover:text-[#FFAB96]"
        onClick={() => {
          void navigator.clipboard.writeText(value)
        }}
      >
        <span className="material-symbols-outlined !text-[14px] !leading-none">content_copy</span>
      </button>
    </span>
  )
}

function UserOperationHashActions({
  hash,
  explorerUrlTemplate,
}: {
  hash: string
  explorerUrlTemplate?: string
}) {
  const explorerUrl = explorerUrlTemplate?.replace(
    '{userOperationHash}',
    hash
  )

  return (
    <span className="inline-flex min-w-0 max-w-full items-center justify-end gap-1">
      <span className="min-w-0 truncate" title={hash}>{truncateHash(hash)}</span>
      <button
        type="button"
        aria-label="Copy UserOperation hash"
        title="Copy UserOperation hash"
        className="inline-flex h-4 w-4 shrink-0 items-center justify-center text-content-secondary/70 transition-colors hover:text-[#FFAB96]"
        onClick={() => {
          void navigator.clipboard.writeText(hash)
        }}
      >
        <span className="material-symbols-outlined !text-[14px] !leading-none">content_copy</span>
      </button>
      {explorerUrl ? (
        <a
          aria-label="Open UserOperation in block explorer"
          title="Open UserOperation in block explorer"
          href={explorerUrl}
          target="_blank"
          rel="noopener noreferrer"
          className="inline-flex h-4 w-4 shrink-0 items-center justify-center text-content-secondary/70 transition-colors hover:text-[#FFAB96]"
        >
          <span className="material-symbols-outlined !text-[14px] !leading-none">open_in_new</span>
        </a>
      ) : null}
    </span>
  )
}

function previewToneClass(tone: PreviewRow['tone']): string {
  if (tone === 'positive') return 'text-positive'
  if (tone === 'warning') return 'text-brand-peach'
  if (tone === 'muted') return 'text-content-secondary'
  return 'text-content-primary'
}

function PreviewRows({
  rows,
  onSlippageClick,
  slippageConfig,
}: {
  rows: PreviewRow[]
  onSlippageClick?: () => void
  slippageConfig?: ReactNode
}) {
  return (
    <dl className="space-y-2">
      {rows.map((row) => {
        if (row.label === 'Max slippage' && onSlippageClick) {
          return (
            <div key={row.label}>
              <button
                type="button"
                className="group flex min-h-6 w-full flex-nowrap items-start justify-between gap-x-3 text-left text-sm text-[#FFAB96] transition-colors hover:text-content-primary"
                onClick={onSlippageClick}
              >
                <span className="group-hover:underline group-focus-visible:underline">{row.label}</span>
                <span className="ml-auto flex min-h-6 shrink-0 items-center justify-end whitespace-nowrap text-right font-normal group-hover:underline group-focus-visible:underline">
                  {row.value}
                </span>
              </button>
              {slippageConfig}
            </div>
          )
        }

        return (
          <div key={row.label} className="flex min-h-6 min-w-0 flex-nowrap items-start justify-between gap-x-3 text-sm">
            <dt className="inline-flex min-w-0 items-center gap-1.5 text-content-secondary">
              <span className="min-w-0 overflow-hidden text-ellipsis whitespace-nowrap" title={row.label}>
                {row.label}
              </span>
              {row.tooltip ? (
                <Tooltip
                  content={row.tooltip}
                  position="bottom-end"
                  className={INFO_TOOLTIP_PANEL_CLASS_NAME}
                  docsLink={row.tooltipDocsLink}
                >
                  <span
                    aria-label={`${row.label} info`}
                    className="inline-flex h-3.5 w-3.5 shrink-0 cursor-help items-center justify-center rounded-full border border-current text-[9px] font-semibold leading-none text-content-secondary/80 transition-colors hover:text-[#FFAB96]"
                    tabIndex={0}
                  >
                    i
                  </span>
                </Tooltip>
              ) : null}
            </dt>
            <dd className={`ml-auto flex min-h-6 shrink-0 items-center justify-end whitespace-nowrap text-right font-normal ${previewToneClass(row.tone)}`}>{row.value}</dd>
          </div>
        )
      })}
    </dl>
  )
}

function lifecycleStep(state: TradeLifecycleState): OrderLifecycleStep {
  if (state === 'preview') return 'preview'
  if (state === 'commitPreparing' || state === 'commitPending' || state === 'commitConfirmed' || state === 'failed') return 'commit'
  return 'reveal'
}

function oppositeDirection(direction: Direction): Direction {
  return direction === 'long' ? 'short' : 'long'
}

function buildOrderSummary({
  currentPositionSide,
  currentPositionDxyExposureUsdc,
  direction,
  isReduceOnly,
  leverage,
  dxyExposureUsdc,
}: {
  currentPositionSide: Direction
  currentPositionDxyExposureUsdc: bigint
  direction: Direction
  isReduceOnly: boolean
  leverage: number
  dxyExposureUsdc: bigint
}): ReactNode {
  const orderAmount = <OrderSummaryRawAmount value={dxyExposureUsdc} />
  const selectedDirection = directionLabel(direction)
  const currentDirection = directionLabel(currentPositionSide)
  const remainingPositionDxyExposureUsdc = currentPositionDxyExposureUsdc > dxyExposureUsdc
    ? currentPositionDxyExposureUsdc - dxyExposureUsdc
    : 0n
  const isFullClose = currentPositionDxyExposureUsdc > 0n && remainingPositionDxyExposureUsdc <= SUMMARY_CLOSE_DUST_USDC_RAW

  if (currentPositionDxyExposureUsdc <= 0n) {
    if (isReduceOnly) {
      return <>You are submitting a reduce-only {selectedDirection} order with {orderAmount} plDXY Perp exposure.</>
    }
    return <>You are opening a {selectedDirection} position with {orderAmount} plDXY Perp exposure at up to {formatLeverage(leverage)} leverage.</>
  }

  if (isReduceOnly) {
    if (isFullClose) return <>You are closing your {currentDirection} position.</>
    return <>You are reducing your {currentDirection} exposure by {orderAmount} to <OrderSummaryRawAmount value={remainingPositionDxyExposureUsdc} />.</>
  }

  if (direction === currentPositionSide) {
    return <>You are increasing your {selectedDirection} exposure by {orderAmount} to <OrderSummaryRawAmount value={currentPositionDxyExposureUsdc + dxyExposureUsdc} />.</>
  }

  if (!isFullClose && dxyExposureUsdc < currentPositionDxyExposureUsdc) {
    return <>You are reducing your {currentDirection} exposure by {orderAmount} to <OrderSummaryRawAmount value={remainingPositionDxyExposureUsdc} />.</>
  }

  if (isFullClose) {
    return <>You are closing your {currentDirection} position.</>
  }

  return <>You are closing your {currentDirection} position and opening a {directionLabel(oppositeDirection(currentPositionSide))} position with <OrderSummaryRawAmount value={dxyExposureUsdc - currentPositionDxyExposureUsdc} /> plDXY Perp exposure.</>
}

function OrderLifecycleSteps({
  currentStep,
}: {
  currentStep: OrderLifecycleStep
}) {
  const currentIndex = ORDER_LIFECYCLE_STEPS.findIndex((step) => step.id === currentStep)

  return (
    <div className="relative">
      <div
        className="absolute top-[7px] h-px bg-brand-border/35"
        style={{ left: 'calc(16.666667% + 0.5rem)', width: 'calc(33.333333% - 1rem)' }}
      />
      <div
        className="absolute top-[7px] h-px bg-brand-border/35"
        style={{ left: 'calc(50% + 0.5rem)', width: 'calc(33.333333% - 1rem)' }}
      />
      <ol className="relative grid grid-cols-3 gap-2">
        {ORDER_LIFECYCLE_STEPS.map((step, index) => {
          const isCurrent = step.id === currentStep
          const isFuture = index > currentIndex
          const dotClass = isCurrent
            ? 'border-brand-peach bg-brand-peach'
            : isFuture
              ? 'border-brand-border/30 bg-surface-panel'
              : 'border-content-secondary/50 bg-content-secondary/50'
          const labelClass = isCurrent
            ? 'text-brand-peach'
            : isFuture
              ? 'text-content-secondary/50'
              : 'text-content-secondary'

          return (
            <li key={step.id} className="relative min-w-0 text-center" aria-current={isCurrent ? 'step' : undefined}>
              <div className="flex justify-center">
                <span className={`relative z-10 h-3.5 w-3.5 rounded-full border-2 ${dotClass}`} />
              </div>
              <div className="mt-3 min-w-0">
                <div className={`text-base font-semibold ${labelClass}`}>{step.label}</div>
              </div>
            </li>
          )
        })}
      </ol>
    </div>
  )
}

function PendingStateCard({
  title,
  description,
  progressPercent,
  showAnimatedDots = false,
}: {
  title: string
  description: ReactNode
  progressPercent?: number
  showAnimatedDots?: boolean
}) {
  const descriptionKey = typeof description === 'string' || typeof description === 'number'
    ? String(description)
    : title

  return (
    <div className="flex min-h-52 flex-col items-center justify-center border border-brand-border/20 bg-app-bg px-6 py-8 text-center">
      {progressPercent === undefined ? <PendingSpinner /> : <PendingProgressCircle progressPercent={progressPercent} />}
      <div className="mt-5 flex min-h-[5.25rem] max-w-full items-center justify-center text-xl font-semibold leading-7 text-content-primary sm:min-h-14">
        <AnimatedLineSwap
          contentKey={title}
          suffix={showAnimatedDots ? <AnimatedTitleDots /> : null}
          className="min-w-0 max-w-full text-center"
        >
          {title}
        </AnimatedLineSwap>
      </div>
      <div className="mt-2 flex min-h-[4.5rem] max-w-md items-start justify-center text-sm leading-6 text-content-secondary sm:min-h-12">
        <AnimatedLineSwap contentKey={descriptionKey} delayMs={180} className="max-w-full text-center">
          {description}
        </AnimatedLineSwap>
      </div>
    </div>
  )
}

type LineSwapPhase = 'idle' | 'running'

const LINE_SWAP_TRANSITION_MS = 1_200

function AnimatedLineSwap({
  contentKey,
  children,
  delayMs = 0,
  suffix,
  className = '',
}: {
  contentKey: string
  children: ReactNode
  delayMs?: number
  suffix?: ReactNode
  className?: string
}) {
  const [displayedContent, setDisplayedContent] = useState<{
    key: string
    targetKey?: string
    current: ReactNode
    outgoing?: ReactNode
    phase: LineSwapPhase
  }>(() => ({
    key: contentKey,
    current: children,
    phase: 'idle',
  }))

  useEffect(() => {
    if (displayedContent.key === contentKey) return undefined

    const prefersReducedMotion = typeof window.matchMedia === 'function' &&
      window.matchMedia('(prefers-reduced-motion: reduce)').matches

    let startTimeout: number | undefined
    let endTimeout: number | undefined

    if (prefersReducedMotion) {
      startTimeout = window.setTimeout(() => {
        setDisplayedContent({
          key: contentKey,
          current: children,
          phase: 'idle',
        })
      }, 0)
    } else {
      startTimeout = window.setTimeout(() => {
        setDisplayedContent((current) => ({
          key: current.key,
          targetKey: contentKey,
          current: children,
          outgoing: current.current,
          phase: 'running',
        }))

        endTimeout = window.setTimeout(() => {
          setDisplayedContent((current) => (
            current.targetKey === contentKey
              ? { key: contentKey, current: current.current, phase: 'idle' }
              : current
          ))
        }, delayMs + LINE_SWAP_TRANSITION_MS)
      }, 0)
    }

    return () => {
      window.clearTimeout(startTimeout)
      if (endTimeout !== undefined) {
        window.clearTimeout(endTimeout)
      }
    }
  }, [children, contentKey, delayMs, displayedContent.key])

  const animationDelayStyle: CSSProperties = { animationDelay: `${delayMs.toString()}ms` }

  return (
    <span className={`relative block overflow-hidden ${className}`}>
      {displayedContent.outgoing === undefined ? null : (
        <span
          className="perps-line-swap-out absolute inset-x-0 top-0 block transform-gpu motion-reduce:hidden"
          style={animationDelayStyle}
          aria-hidden="true"
        >
          {displayedContent.outgoing}
        </span>
      )}
      <span
        className={displayedContent.phase === 'running' ? 'perps-line-swap-in relative block transform-gpu' : 'relative block transform-gpu'}
        style={displayedContent.phase === 'running' ? animationDelayStyle : undefined}
      >
        <LineSwapContentWithSuffix content={displayedContent.current} suffix={suffix} />
      </span>
    </span>
  )
}

function LineSwapContentWithSuffix({ content, suffix }: { content: ReactNode; suffix?: ReactNode }) {
  if (suffix === undefined || suffix === null) return <>{content}</>
  if (typeof content !== 'string') {
    return (
      <>
        {content}
        {'\u2060'}
        {suffix}
      </>
    )
  }

  return (
    <>
      <span>{content}</span>
      {'\u2060'}
      {suffix}
    </>
  )
}

function AnimatedTitleDots() {
  const [dotCount, setDotCount] = useState(0)

  useEffect(() => {
    const interval = window.setInterval(() => {
      setDotCount((current) => current === 3 ? 0 : current + 1)
    }, 1_000)

    return () => {
      window.clearInterval(interval)
    }
  }, [])

  return (
    <span className="ml-0.5 inline-block w-4 text-left" aria-hidden="true">
      {'.'.repeat(dotCount)}
    </span>
  )
}

function PendingSpinner() {
  return (
    <div className="relative h-14 w-14 shrink-0">
      <div className="absolute inset-0 rounded-full border-4 border-brand-peach/20 border-t-brand-peach animate-spin" />
    </div>
  )
}

function PendingProgressCircle({ progressPercent }: { progressPercent: number }) {
  const radius = 22
  const circumference = 2 * Math.PI * radius
  const normalizedProgress = Math.max(0, Math.min(100, progressPercent))
  const strokeDashoffset = circumference * (1 - normalizedProgress / 100)

  return (
    <div
      className="relative h-14 w-14 shrink-0"
      role="progressbar"
      aria-label="Price finalization progress"
      aria-valuemin={0}
      aria-valuemax={100}
      aria-valuenow={Math.round(normalizedProgress)}
    >
      <svg className="h-14 w-14 -rotate-90" viewBox="0 0 56 56" aria-hidden="true">
        <circle
          className="fill-none stroke-brand-peach/20"
          cx="28"
          cy="28"
          r={radius}
          strokeWidth="4"
        />
        <circle
          className="fill-none stroke-brand-peach"
          cx="28"
          cy="28"
          r={radius}
          strokeWidth="4"
          strokeLinecap="round"
          style={{
            strokeDasharray: circumference,
            strokeDashoffset,
            transition: 'stroke-dashoffset 200ms linear',
          }}
        />
      </svg>
    </div>
  )
}

function SuccessStateCard({
  title,
  description,
  celebrate = false,
}: {
  title: string
  description: string
  celebrate?: boolean
}) {
  const [celebrationKey, setCelebrationKey] = useState(0)
  const [celebrationOrigin, setCelebrationOrigin] = useState<{
    x: number
    y: number
    stageWidth: number
    stageHeight: number
    direction: 'up' | 'down'
  }>()
  const className = 'relative isolate flex min-h-52 flex-col items-center justify-center overflow-hidden border border-brand-border/20 bg-app-bg px-6 py-8 text-center'
  const content = (
    <>
      {celebrate ? <PerpsFinalizationConfetti key={celebrationKey} origin={celebrationOrigin} /> : null}
      <SuccessIcon className="relative z-10" />
      <div className="relative z-10 mt-5 text-xl font-semibold text-content-primary">{title}</div>
      <div className="relative z-10 mt-2 max-w-md text-sm leading-6 text-content-secondary">{description}</div>
    </>
  )

  if (celebrate) {
    return (
      <button
        type="button"
        className={`${className} w-full cursor-pointer focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-positive`}
        aria-label="Replay celebration confetti"
        onClick={(event) => {
          const bounds = event.currentTarget.getBoundingClientRect()
          const x = event.detail === 0
            ? bounds.width / 2
            : Math.min(Math.max(event.clientX - bounds.left, 0), bounds.width)
          const y = event.detail === 0
            ? bounds.height / 2
            : Math.min(Math.max(event.clientY - bounds.top, 0), bounds.height)
          const direction = y > bounds.height / 2 ? 'up' : 'down'

          setCelebrationOrigin({
            x,
            y,
            stageWidth: Math.max(bounds.width, 1),
            stageHeight: Math.max(direction === 'up' ? y : bounds.height - y, 1),
            direction,
          })
          setCelebrationKey((currentKey) => currentKey + 1)
        }}
      >
        {content}
      </button>
    )
  }

  return (
    <div className={className}>
      {content}
    </div>
  )
}

function FailedStateCard({ title, description }: { title: string; description: string }) {
  return (
    <div className="flex min-h-52 flex-col items-center justify-center border border-brand-orange/40 bg-brand-orange/10 px-6 py-8 text-center">
      <div className="flex h-14 w-14 items-center justify-center border border-brand-orange/40 bg-brand-orange/15 text-brand-orange">
        <span className="material-symbols-outlined text-4xl">close</span>
      </div>
      <div className="mt-5 text-xl font-semibold text-brand-orange">{title}</div>
      <div className="mt-2 max-w-xl whitespace-pre-line text-left text-sm leading-6 text-content-secondary">{description}</div>
    </div>
  )
}

function AccountContextRow({
  label,
  value,
  valueTone = 'default',
  onClick,
  disabled = false,
}: {
  label: string
  value: ReactNode
  valueTone?: 'default' | 'positive'
  onClick: () => void
  disabled?: boolean
}) {
  const valueColor = valueTone === 'positive' ? 'text-positive' : 'text-content-primary'

  return (
    <button
      type="button"
      disabled={disabled}
      className="group flex w-full min-w-0 cursor-pointer flex-wrap items-start justify-between gap-x-3 gap-y-1 text-left text-sm transition-colors hover:text-content-primary disabled:cursor-default disabled:hover:text-inherit focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-[#FFAB96]"
      onClick={onClick}
    >
      <span className="min-w-0 flex-1 text-content-secondary">{label}</span>
      <span className={`ml-auto max-w-full min-w-0 break-words text-right font-semibold group-hover:underline group-focus-visible:underline ${valueColor}`}>{value}</span>
    </button>
  )
}

interface AccountSummaryRowBaseProps {
  label: string
  value: ReactNode
  tone?: 'default' | 'positive' | 'negative'
}

type AccountSummaryRowProps = AccountSummaryRowBaseProps & (
  | {
      tooltip?: undefined
      tooltipDocsLink?: never
    }
  | {
      tooltip: ReactNode
      tooltipDocsLink: TooltipDocsLink
    }
)

function AccountSummaryRow({
  label,
  value,
  tone = 'default',
  tooltip,
  tooltipDocsLink,
}: AccountSummaryRowProps) {
  const valueClass = tone === 'positive'
    ? 'text-positive'
    : tone === 'negative'
      ? 'text-brand-orange'
      : 'text-content-primary'

  return (
    <div className="flex min-w-0 flex-wrap items-start justify-between gap-x-3 gap-y-1 text-sm">
      <span className="inline-flex min-w-0 flex-1 flex-wrap items-center gap-1.5 text-content-secondary">
        {label}
        {tooltip ? (
          <Tooltip
            content={tooltip}
            position="bottom-end"
            className={INFO_TOOLTIP_PANEL_CLASS_NAME}
            docsLink={tooltipDocsLink}
          >
            <span
              aria-label={`${label} info`}
              className="inline-flex h-3.5 w-3.5 shrink-0 cursor-help items-center justify-center rounded-full border border-current text-[9px] font-semibold leading-none text-content-secondary/80 transition-colors hover:text-[#FFAB96]"
              tabIndex={0}
            >
              i
            </span>
          </Tooltip>
        ) : null}
      </span>
      <span className={`ml-auto max-w-full min-w-0 break-words text-right font-semibold ${valueClass}`}>{value}</span>
    </div>
  )
}

function isNumericInput(value: string): boolean {
  return /^[0-9., ]*$/.test(value)
}

function validationReasonCategory(message: string): string {
  const normalized = message.toLowerCase()
  if (normalized.includes('connect wallet')) return 'connect_wallet'
  if (normalized.includes('switch to')) return 'wrong_chain'
  if (normalized.includes('price') || normalized.includes('oracle')) return 'oracle_unavailable'
  if (normalized.includes('order size') || normalized.includes('minimum') || normalized.includes('max')) return 'size_or_capacity'
  if (normalized.includes('pending order') || normalized.includes('expired')) return 'pending_order_limit'
  if (normalized.includes('deposit')) return 'margin_shortfall'
  if (normalized.includes('no current position')) return 'no_position'
  if (normalized.includes('one-step flips')) return 'one_step_flip'
  if (normalized.includes('preview')) return 'preview_unavailable'
  return 'unknown'
}

export function PerpsTradeTicket({
  initialLifecycleState = 'preview',
  initialReviewOpen = false,
  initialDirection = 'long',
  initialSize = '0',
  initialReduceOnly = false,
  initialLeverage = 5,
  initialMarginAction,
  initialMarginActionAmount = '',
  initialMarginCallSimulatorConfirmationOpen = false,
  initialOrderId,
  initialCommitTxHash,
  initialExecuteTxHash,
  initialUserOperationHash,
  initialCommitExecutionStatus,
  initialWalletRequestWarning,
  initialFinalExecutionPrice,
  initialFinalExecutionOraclePrice,
  initialFinalExecutionOracleFrozen,
  initialFinalFrozenCloseSpreadUsdc,
  initialFinalExecutionEconomicsVersion,
  initialFinalVpiUsdc,
  initialCommittedVpiUsdc,
  initialCommittedPositionVpiAccrued,
  initialCommittedIsFullClose,
  initialCommittedSizeDelta,
  initialFlowError,
  closePositionRequestId,
  currentPositionSide = 'long',
  currentPositionAmount,
  enableLiveTrading = false,
  showFinalizationProgress = false,
  oraclePriceRaw,
  oraclePublishTime,
  oraclePriceDisplay,
  latestBasket,
  adverseConfidenceMultiplierBps,
  oracleFrozen = false,
  openPreviewFixture,
  closePreviewFixture,
  validationErrorFixture,
  executionProtectionsFixture,
  oracleFreshness,
  oracleFreshnessTooltip,
  availableToTradeRaw,
  availableToTradeAmount,
  portfolioValueRaw,
  withdrawableUsdcRaw,
  walletUsdcRaw,
  ownerWalletUsdcRaw,
  tradingAccountUsdcRaw,
  marginAllowanceUsdc,
  currentPosition,
  pendingOrders = [],
  orderHistory = [],
  ordersIndexedThroughBlockRaw,
  pendingOrderCount,
  activePositionProtectionId = 0n,
  maxPendingOrders,
  firstPendingOrderId,
  firstPendingOrderExpiryTime,
  longOpenCapacityUsdc,
  shortOpenCapacityUsdc,
  minOpenNotionalUsdc,
  minNewPositionNotionalUsdc,
  maintenanceMarginBps,
  executionFeeBps,
  marketPhase = 'open',
  marketCurrentDuration,
  onAccountRefresh,
}: PerpsTradeTicketProps) {
  const identity = usePerpsIdentity()
  const address = identity.accountAddress
  const isConnected = identity.ownerAddress !== undefined
  const isSponsoredAccountConfigured = identity.isAaManifestConfigured
  const chainId = useChainId()
  const { switchToArbitrumSepolia, switchError: networkSwitchError } = useSwitchToArbitrumSepolia()
  const {
    abandonDepositAuthorization,
    fundTradingAccount,
    depositMargin,
    withdrawMargin,
    prepareOrder,
    commitOrder,
    executeOrder,
    cleanupExpiredOrder,
  } = usePerpsTrading()
  const sponsoredOperations = useSponsoredOperationStore((state) => state.operations)
  const marginActionRequest = usePerpsUiStore((s) => s.marginActionRequest)
  const clearMarginActionRequest = usePerpsUiStore((s) => s.clearMarginActionRequest)
  const slippageOptions = SLIPPAGE_OPTIONS
  const [direction, setDirection] = useState<Direction>(initialDirection)
  const [isReduceOnly, setIsReduceOnly] = useState(initialReduceOnly)
  const [isMarginCallSimulatorEnabled, setIsMarginCallSimulatorEnabled] = useState(false)
  const [isMarginCallSimulatorConfirmationOpen, setIsMarginCallSimulatorConfirmationOpen] = useState(
    initialMarginCallSimulatorConfirmationOpen
  )
  const [size, setSize] = useState(initialSize)
  const [isFullCloseIntent, setIsFullCloseIntent] = useState(false)
  const [leverage, setLeverage] = useState(initialLeverage)
  const [leverageInputValue, setLeverageInputValue] = useState(initialLeverage.toString())
  const [slippage, setSlippage] = useState(
    oracleFrozen ? DEFAULT_ORACLE_FROZEN_SLIPPAGE : DEFAULT_LIVE_SLIPPAGE
  )
  const [lifecycleState, setLifecycleState] = useState<TradeLifecycleState>(initialLifecycleState)
  const [isReviewOpen, setIsReviewOpen] = useState(initialReviewOpen)
  const [isSlippageConfigOpen, setIsSlippageConfigOpen] = useState(false)
  const [isPreviewExpanded, setIsPreviewExpanded] = useState(false)
  const [preparedOrder, setPreparedOrder] = useState<PreparedPerpsOrderV2 | undefined>()
  const [isExecutionProtectionsLoading, setIsExecutionProtectionsLoading] = useState(false)
  const [executionProtectionsError, setExecutionProtectionsError] = useState<string | undefined>()
  const [orderId, setOrderId] = useState<bigint | undefined>(initialOrderId)
  const [commitTxHash, setCommitTxHash] = useState<string | undefined>(initialCommitTxHash)
  const [executeTxHash, setExecuteTxHash] = useState<string | undefined>(initialExecuteTxHash)
  const [finalExecutionPrice, setFinalExecutionPrice] = useState<bigint | undefined>(initialFinalExecutionPrice)
  const [finalExecutionOraclePrice, setFinalExecutionOraclePrice] = useState<bigint | undefined>(
    initialFinalExecutionOraclePrice
  )
  const [finalExecutionOracleFrozen, setFinalExecutionOracleFrozen] = useState<boolean | undefined>(
    initialFinalExecutionOracleFrozen
  )
  const [finalExecutionFrozenCloseSpreadUsdc, setFinalExecutionFrozenCloseSpreadUsdc] =
    useState<bigint | undefined>(initialFinalFrozenCloseSpreadUsdc)
  const [finalExecutionEconomicsVersion, setFinalExecutionEconomicsVersion] =
    useState<number | undefined>(initialFinalExecutionEconomicsVersion)
  const [finalVpiUsdc, setFinalVpiUsdc] = useState<bigint | undefined>(initialFinalVpiUsdc)
  const [committedVpiUsdc, setCommittedVpiUsdc] = useState<bigint | undefined>(initialCommittedVpiUsdc)
  const [committedPositionVpiAccrued, setCommittedPositionVpiAccrued] = useState<bigint | undefined>(
    initialCommittedPositionVpiAccrued
  )
  const [committedShowsPositionVpiBalance, setCommittedShowsPositionVpiBalance] = useState(
    initialCommittedPositionVpiAccrued !== undefined
  )
  const [committedSizeDelta, setCommittedSizeDelta] = useState<bigint | undefined>(initialCommittedSizeDelta)
  const [committedSlippage, setCommittedSlippage] = useState<number | undefined>()
  const [committedTargetPrice, setCommittedTargetPrice] = useState<number | null | undefined>()
  const [committedIsClose, setCommittedIsClose] = useState<boolean | undefined>(
    initialReduceOnly ? true : undefined
  )
  const [committedIsFullClose, setCommittedIsFullClose] = useState<boolean | undefined>(
    initialCommittedIsFullClose
  )
  const [flowError, setFlowError] = useState<string | undefined>(initialFlowError)
  const [marginAction, setMarginAction] = useState<MarginAction | null>(initialMarginAction ?? null)
  const [marginActionAmount, setMarginActionAmount] = useState(initialMarginActionAmount)
  const [marginActionStatus, setMarginActionStatus] = useState<MarginActionStatus>('idle')
  const [marginActionError, setMarginActionError] = useState<string | undefined>()
  const [locallyConfirmedFundingBalances, setLocallyConfirmedFundingBalances] = useState<{
    ownerWallet: bigint
    tradingAccount: bigint
  } | null>(null)
  const [cleanupStatus, setCleanupStatus] = useState<CleanupStatus>('idle')
  const [cleanupError, setCleanupError] = useState<string | undefined>()
  const [nowSeconds, setNowSeconds] = useState(() => Math.floor(Date.now() / 1000))
  const [keeperRevealDeadlineMs, setKeeperRevealDeadlineMs] = useState<number | undefined>()
  const [keeperRevealNowMs, setKeeperRevealNowMs] = useState(() => Date.now())
  const [finalizationLoadingMessage, setFinalizationLoadingMessage] = useState<FinalizationLoadingMessage>(FINALIZATION_LOADING_MESSAGES[0])
  const [walletRequestWarning, setWalletRequestWarning] = useState<string | undefined>(
    initialWalletRequestWarning
  )
  const [commitExecutionStatus, setCommitExecutionStatus] = useState<
    SponsoredExecutionStatus | undefined
  >(
    initialCommitExecutionStatus ??
      (initialLifecycleState === 'commitPending' ? 'awaiting-signature' : undefined)
  )
  const onAccountRefreshRef = useRef(onAccountRefresh)
  const orderWaitStartedForRef = useRef<bigint | undefined>(undefined)
  const handledTerminalOrderKeyRef = useRef<string | undefined>(undefined)
  const handledTerminalBlockNumberRef = useRef<bigint | undefined>(undefined)
  const handledTerminalBlockHashRef = useRef<string | undefined>(undefined)
  const rejectedTerminalRef = useRef<{
    terminalKey: string
    blockHash?: string
  } | undefined>(undefined)
  const executionEvidencePollRef = useRef<{
    terminalKey: string
    deadlineMs: number
    exhausted: boolean
  } | undefined>(undefined)
  const commitAttemptIdRef = useRef(0)
  const includedCommitAttemptRef = useRef<number | undefined>(undefined)
  const includedCommitIdentityRef = useRef<{
    account: string
    clientOrderId: string
    hash?: string
    orderId: bigint
  } | undefined>(undefined)
  const deferredSafeConfirmationErrorRef = useRef<{
    account: string
    clientOrderId: string
    hash?: string
    message: string
    orderId: bigint
  } | undefined>(undefined)
  const orderHistoryRef = useRef(orderHistory)
  orderHistoryRef.current = orderHistory
  const handledMarginActionRequestRef = useRef<number | undefined>(undefined)
  const handledClosePositionRequestRef = useRef<number | undefined>(undefined)
  const terminalLifecycleTrackedRef = useRef<TradeLifecycleState | undefined>(undefined)
  const finalizationShownTitlesRef = useRef<Set<string>>(new Set([FINALIZATION_LOADING_MESSAGES[0].title]))
  const simulatorMaxLeverage = maxLeverageFromMaintenanceMargin(maintenanceMarginBps)
  const canEnableMarginCallSimulator = simulatorMaxLeverage > DEFAULT_MAX_LEVERAGE
  const maxLeverage = isMarginCallSimulatorEnabled ? simulatorMaxLeverage : DEFAULT_MAX_LEVERAGE
  const activeLeverage = Math.min(leverage, maxLeverage)
  const normalizedAccountAddress = address?.toLowerCase()
  const latestSponsoredOperation = useMemo(
    () => sponsoredOperations
      .filter((operation) =>
        operation.accountAddress.toLowerCase() === normalizedAccountAddress &&
        operation.action === 'place-order'
      )
      .sort((a, b) => b.updatedAt - a.updatedAt)
      .at(0),
    [normalizedAccountAddress, sponsoredOperations]
  )
  const commitPendingTitle =
    commitExecutionStatus === 'submitting'
      ? 'Submitting sponsored transaction'
      : commitExecutionStatus === 'confirming'
        ? 'Waiting for on-chain confirmation'
        : commitExecutionStatus === 'confirmed'
          ? 'Transaction confirmed'
          : 'Waiting for wallet confirmation'
  const commitPendingDescription =
    commitExecutionStatus === 'submitting'
      ? 'Your wallet approved the sponsored UserOperation. Plether is submitting it to the network.'
      : commitExecutionStatus === 'confirming'
        ? 'Your wallet approved the sponsored UserOperation and it was submitted. Plether is waiting for on-chain confirmation.'
        : commitExecutionStatus === 'confirmed'
          ? 'The sponsored UserOperation is confirmed on-chain. Plether is loading the committed order.'
          : isSponsoredAccountConfigured
            ? 'Confirm the final sponsored UserOperation in your owner wallet. Plether has already installed the gas sponsorship.'
            : 'Confirm the commit transaction in your wallet, then wait for it to be included onchain.'

  useEffect(() => {
    onAccountRefreshRef.current = onAccountRefresh
  }, [onAccountRefresh])

  useEffect(() => {
    if (firstPendingOrderExpiryTime === undefined && oraclePublishTime === undefined) return undefined
    const interval = window.setInterval(() => {
      setNowSeconds(Math.floor(Date.now() / 1000))
    }, 1_000)

    return () => {
      window.clearInterval(interval)
    }
  }, [firstPendingOrderExpiryTime, oraclePublishTime])

  useEffect(() => {
    if ((!enableLiveTrading && !showFinalizationProgress) || lifecycleState !== 'revealPending') return

    setKeeperRevealDeadlineMs((currentDeadline) => currentDeadline ?? Date.now() + KEEPER_REVEAL_GRACE_MS)
    setKeeperRevealNowMs(Date.now())
    finalizationShownTitlesRef.current = new Set([FINALIZATION_LOADING_MESSAGES[0].title])
    setFinalizationLoadingMessage(FINALIZATION_LOADING_MESSAGES[0])
  }, [enableLiveTrading, lifecycleState, orderId, showFinalizationProgress])

  useEffect(() => {
    if ((!enableLiveTrading && !showFinalizationProgress) || lifecycleState !== 'revealPending' || keeperRevealDeadlineMs === undefined) return undefined

    const progressInterval = window.setInterval(() => {
      setKeeperRevealNowMs(Date.now())
    }, KEEPER_REVEAL_PROGRESS_MS)
    const messageInterval = window.setInterval(() => {
      setFinalizationLoadingMessage((currentMessage) => {
        const nextMessage = randomFinalizationMessage(currentMessage.title, finalizationShownTitlesRef.current)
        finalizationShownTitlesRef.current.add(nextMessage.title)
        return nextMessage
      })
    }, FINALIZATION_MESSAGE_ROTATE_MS)

    const timeout = window.setTimeout(() => {
      setKeeperRevealNowMs(Date.now())
      if (!isSponsoredAccountConfigured) {
        setLifecycleState((currentState) => (
          currentState === 'revealPending' ? 'selfExecuteAvailable' : currentState
        ))
      }
    }, Math.max(0, keeperRevealDeadlineMs - Date.now()))

    return () => {
      window.clearInterval(progressInterval)
      window.clearInterval(messageInterval)
      window.clearTimeout(timeout)
    }
  }, [enableLiveTrading, isSponsoredAccountConfigured, keeperRevealDeadlineMs, lifecycleState, showFinalizationProgress])

  useEffect(() => {
    if (
      isSponsoredAccountConfigured &&
      (
        lifecycleState === 'selfExecuteAvailable' ||
        lifecycleState === 'selfExecutePending'
      )
    ) {
      setLifecycleState('revealPending')
    }
  }, [isSponsoredAccountConfigured, lifecycleState])

  const applyTerminalOrder = useCallback((
    order: PerpsOrderHistoryRow,
    isCanonicalHistory = false
  ) => {
    if (order.status === 'Committed') return false
    const includedIdentity = includedCommitIdentityRef.current
    if (
      includedIdentity !== undefined &&
      !orderMatchesCommittedIdentity(order, includedIdentity)
    ) {
      return false
    }

    const nextTerminalOrderKey = terminalOrderKey(order)
    const nextTerminalBlockHash = order.terminalBlockHash?.toLowerCase()
    const rejectedTerminal = rejectedTerminalRef.current
    if (rejectedTerminal?.terminalKey === nextTerminalOrderKey) {
      const provesDifferentCanonicalBlock =
        rejectedTerminal.blockHash !== undefined
        && nextTerminalBlockHash !== undefined
        && rejectedTerminal.blockHash !== nextTerminalBlockHash
      if (!isCanonicalHistory && !provesDifferentCanonicalBlock) return false
    }
    if (rejectedTerminal !== undefined) {
      rejectedTerminalRef.current = undefined
    }
    const hasSameTerminalBase =
      handledTerminalOrderKeyRef.current === nextTerminalOrderKey
    const hasKnownBlockHashReplacement =
      hasSameTerminalBase
      && handledTerminalBlockHashRef.current !== undefined
      && nextTerminalBlockHash !== undefined
      && handledTerminalBlockHashRef.current !== nextTerminalBlockHash
    const isSameTerminalOrder =
      hasSameTerminalBase && !hasKnownBlockHashReplacement
    if (hasKnownBlockHashReplacement) {
      executionEvidencePollRef.current = undefined
    }
    if (hasCompleteExecutionEvidence(order)) {
      executionEvidencePollRef.current = undefined
    } else if (executionEvidencePollRef.current?.terminalKey !== nextTerminalOrderKey) {
      executionEvidencePollRef.current = {
        terminalKey: nextTerminalOrderKey,
        deadlineMs: Date.now() + ORDER_EXECUTION_EVIDENCE_POLL_MS,
        exhausted: false,
      }
    }

    const terminalCloseIntent = historyOrderIsClose(order)
    if (terminalCloseIntent !== undefined) {
      setCommittedIsClose(terminalCloseIntent)
    }

    if (order.status === 'Executed') {
      const indexedExecutionPrice = order.executionPriceRaw ?? order.activityPriceRaw
      setFinalExecutionPrice((current) => (
        isSameTerminalOrder && indexedExecutionPrice === undefined
          ? current
          : indexedExecutionPrice
      ))
      setFinalExecutionOraclePrice((current) => (
        isSameTerminalOrder && order.executionOraclePriceRaw === undefined
          ? current
          : order.executionOraclePriceRaw
      ))
      setFinalExecutionOracleFrozen((current) => (
        isSameTerminalOrder && order.executionOracleFrozen === undefined
          ? current
          : order.executionOracleFrozen
      ))
      setFinalVpiUsdc((current) => (
        isSameTerminalOrder && order.vpiUsdcRaw === undefined
          ? current
          : order.vpiUsdcRaw
      ))
      setFinalExecutionFrozenCloseSpreadUsdc((current) => (
        isSameTerminalOrder && order.frozenCloseSpreadUsdcRaw === undefined
          ? current
          : order.frozenCloseSpreadUsdcRaw
      ))
      setFinalExecutionEconomicsVersion((current) => (
        isSameTerminalOrder && order.executionEconomicsVersion === undefined
          ? current
          : order.executionEconomicsVersion
      ))
    } else if (!isSameTerminalOrder) {
      setFinalExecutionPrice(undefined)
      setFinalExecutionOraclePrice(undefined)
      setFinalExecutionOracleFrozen(undefined)
      setFinalExecutionFrozenCloseSpreadUsdc(undefined)
      setFinalExecutionEconomicsVersion(undefined)
      setFinalVpiUsdc(undefined)
    }

    if (isSameTerminalOrder) {
      if (
        handledTerminalBlockHashRef.current === undefined
        && nextTerminalBlockHash !== undefined
      ) {
        handledTerminalBlockHashRef.current = nextTerminalBlockHash
      }
      return true
    }
    handledTerminalOrderKeyRef.current = nextTerminalOrderKey
    handledTerminalBlockNumberRef.current = order.terminalBlockNumberRaw
    handledTerminalBlockHashRef.current = nextTerminalBlockHash

    setCommitTxHash((current) => current ?? order.commitTxHash)
    setExecuteTxHash(order.revealTxHash)

    if (order.status === 'Executed') {
      setFlowError(undefined)
      const refreshAccount = onAccountRefreshRef.current
      if (refreshAccount === undefined) {
        setLifecycleState('executed')
      } else {
        void (async () => {
          try {
            await refreshAccount()
          } catch {
            // The order is already terminal. Show its result even if the
            // follow-up account read fails; the normal polling can retry it.
          } finally {
            const terminalIsStillCurrent =
              handledTerminalOrderKeyRef.current === nextTerminalOrderKey &&
              (
                nextTerminalBlockHash === undefined ||
                handledTerminalBlockHashRef.current === nextTerminalBlockHash
              )
            if (terminalIsStillCurrent) {
              setLifecycleState('executed')
            }
          }
        })()
      }
    } else {
      setFlowError(terminalOrderFailureMessage(order))
      setLifecycleState('selfExecuteFailed')
      void onAccountRefreshRef.current?.()
    }
    return true
  }, [])

  const rewindHandledTerminalOrder = useCallback(() => {
    const rewindStartedAt = Date.now()
    const rejectedTerminalKey = handledTerminalOrderKeyRef.current
    if (rejectedTerminalKey !== undefined) {
      rejectedTerminalRef.current = {
        terminalKey: rejectedTerminalKey,
        blockHash: handledTerminalBlockHashRef.current,
      }
    }
    handledTerminalOrderKeyRef.current = undefined
    handledTerminalBlockNumberRef.current = undefined
    handledTerminalBlockHashRef.current = undefined
    executionEvidencePollRef.current = undefined
    orderWaitStartedForRef.current = undefined
    setExecuteTxHash(undefined)
    setFinalExecutionPrice(undefined)
    setFinalExecutionOraclePrice(undefined)
    setFinalExecutionOracleFrozen(undefined)
    setFinalExecutionFrozenCloseSpreadUsdc(undefined)
    setFinalExecutionEconomicsVersion(undefined)
    setFinalVpiUsdc(undefined)
    setFlowError(undefined)
    setKeeperRevealDeadlineMs(rewindStartedAt + KEEPER_REVEAL_GRACE_MS)
    setKeeperRevealNowMs(rewindStartedAt)
    setLifecycleState('revealPending')
  }, [])

  useEffect(() => {
    if (!enableLiveTrading || orderId === undefined) return

    const deferredSafeConfirmationError =
      deferredSafeConfirmationErrorRef.current
    if (
      deferredSafeConfirmationError?.orderId === orderId &&
      !orderHistory.some((row) =>
        orderMatchesCommittedIdentity(row, deferredSafeConfirmationError)
      )
    ) {
      deferredSafeConfirmationErrorRef.current = undefined
      includedCommitAttemptRef.current = undefined
      includedCommitIdentityRef.current = undefined
      handledTerminalOrderKeyRef.current = undefined
      handledTerminalBlockNumberRef.current = undefined
      handledTerminalBlockHashRef.current = undefined
      rejectedTerminalRef.current = undefined
      executionEvidencePollRef.current = undefined
      setOrderId(undefined)
      setCommitTxHash(undefined)
      setExecuteTxHash(undefined)
      setFinalExecutionPrice(undefined)
      setFinalExecutionOraclePrice(undefined)
      setFinalExecutionOracleFrozen(undefined)
      setFinalExecutionFrozenCloseSpreadUsdc(undefined)
      setFinalExecutionEconomicsVersion(undefined)
      setFinalVpiUsdc(undefined)
      setFlowError(deferredSafeConfirmationError.message)
      setLifecycleState('failed')
      return
    }

    const includedIdentity = includedCommitIdentityRef.current
    const indexedOrder = orderHistory.find((row) =>
      row.orderId === orderId &&
      (
        includedIdentity === undefined ||
        orderMatchesCommittedIdentity(row, includedIdentity)
      )
    )
    if (indexedOrder && indexedOrder.status !== 'Committed') {
      applyTerminalOrder(indexedOrder, true)
      return
    }

    const handledTerminalBlockNumber = handledTerminalBlockNumberRef.current
    if (
      handledTerminalOrderKeyRef.current !== undefined
      && handledTerminalBlockNumber !== undefined
      && ordersIndexedThroughBlockRaw !== undefined
      && ordersIndexedThroughBlockRaw >= handledTerminalBlockNumber
      && indexedOrder?.status === 'Committed'
    ) {
      rewindHandledTerminalOrder()
    }
  }, [
    applyTerminalOrder,
    enableLiveTrading,
    orderHistory,
    orderId,
    ordersIndexedThroughBlockRaw,
    rewindHandledTerminalOrder,
  ])

  useEffect(() => {
    if (!enableLiveTrading || orderId === undefined) return undefined
    const handledTerminalKey = handledTerminalOrderKeyRef.current
    const indexedOrder = orderHistory.find((row) => row.orderId === orderId)
    const indexedTerminalKey =
      indexedOrder !== undefined && indexedOrder.status !== 'Committed'
        ? terminalOrderKey(indexedOrder)
        : undefined
    if (
      handledTerminalKey !== undefined
      && indexedTerminalKey === handledTerminalKey
      && indexedOrder !== undefined
      && hasCompleteExecutionEvidence(indexedOrder)
    ) {
      return undefined
    }
    const activeEvidencePoll = executionEvidencePollRef.current
    if (
      handledTerminalKey !== undefined
      && activeEvidencePoll?.terminalKey === handledTerminalKey
    ) {
      if (activeEvidencePoll.exhausted) return undefined
      if (Date.now() >= activeEvidencePoll.deadlineMs) {
        activeEvidencePoll.exhausted = true
        void onAccountRefreshRef.current?.()
        return undefined
      }
    }
    if (orderWaitStartedForRef.current === orderId) return undefined

    const activeOrderId = orderId
    orderWaitStartedForRef.current = orderId
    const controller = new AbortController()
    const isCancelled = () => controller.signal.aborted

    function stopIfEvidencePollExpired(): boolean {
      const currentTerminalKey = handledTerminalOrderKeyRef.current
      const pollState = executionEvidencePollRef.current
      if (
        currentTerminalKey === undefined
        || pollState?.terminalKey !== currentTerminalKey
      ) {
        return false
      }
      if (pollState.exhausted) return true
      if (Date.now() < pollState.deadlineMs) return false
      pollState.exhausted = true
      void onAccountRefreshRef.current?.()
      return true
    }

    async function waitForTerminalOrderLoop() {
      while (!isCancelled()) {
        try {
          const result = await waitForPerpsOrderTerminal({
            accountAddress: address,
            orderId: activeOrderId,
            timeoutSeconds: ORDER_TERMINAL_WAIT_SECONDS,
            signal: controller.signal,
          })

          if (isCancelled()) return
          if (result.order !== undefined && applyTerminalOrder(result.order)) {
            if (hasCompleteExecutionEvidence(result.order)) return
            if (stopIfEvidencePollExpired()) return
          } else {
            void onAccountRefreshRef.current?.()
          }
        } catch (error: unknown) {
          if (isCancelled() || (error instanceof DOMException && error.name === 'AbortError')) return
        }

        if (stopIfEvidencePollExpired()) return
        await new Promise<void>((resolve) => {
          window.setTimeout(resolve, ORDER_TERMINAL_RETRY_DELAY_MS)
        })
      }
    }

    void waitForTerminalOrderLoop().finally(() => {
      if (orderWaitStartedForRef.current === activeOrderId) {
        orderWaitStartedForRef.current = undefined
      }
    })

    return () => {
      controller.abort()
      if (orderWaitStartedForRef.current === orderId) {
        orderWaitStartedForRef.current = undefined
      }
    }
  }, [
    address,
    applyTerminalOrder,
    enableLiveTrading,
    orderHistory,
    orderId,
    ordersIndexedThroughBlockRaw,
  ])

  useEffect(() => {
    setLeverage((currentLeverage) => Math.min(currentLeverage, maxLeverage))
  }, [maxLeverage])

  useEffect(() => {
    setLeverageInputValue(activeLeverage.toString())
  }, [activeLeverage])

  useEffect(() => {
    if (!canEnableMarginCallSimulator) {
      setIsMarginCallSimulatorEnabled(false)
    }
  }, [canEnableMarginCallSimulator])

  useEffect(() => {
    if (
      lifecycleState !== 'commitPending' ||
      commitExecutionStatus !== 'awaiting-signature' ||
      commitTxHash ||
      flowError
    ) {
      setWalletRequestWarning(undefined)
      return undefined
    }

    const timeout = globalThis.setTimeout(() => {
      const warning = 'No wallet response yet. Open your wallet app or extension and check for a pending confirmation. If there is no pending request, reject any stuck request, reconnect the wallet, and retry.'
      debugPerpsCommit('ticket:wallet-request:still-pending', {
        seconds: 15,
        address,
        chainId,
      })
      setWalletRequestWarning(warning)
    }, 15_000)

    return () => {
      globalThis.clearTimeout(timeout)
    }
  }, [
    address,
    chainId,
    commitExecutionStatus,
    commitTxHash,
    flowError,
    lifecycleState,
  ])

  const dxyExposureNumber = parseAmount(size)
  const currentPositionSideValue = currentPosition?.exists ? currentPosition.direction : currentPositionSide
  const currentPositionRawNotional = currentPosition?.estimatedNotionalUsdc
    ?? parsePerpsUsdc(currentPositionAmount ?? (enableLiveTrading ? '0' : CURRENT_POSITION_AMOUNT))
  const currentPositionDxyExposureRaw = currentPosition?.dxyExposureUsdc ?? currentPositionRawNotional
  const currentPositionDisplayAmount = currentPosition?.exists
    ? formatPerpsUsdc(currentPositionDxyExposureRaw)
    : currentPositionAmount ?? (enableLiveTrading ? '0' : CURRENT_POSITION_AMOUNT)
  const currentPositionInputAmount = currentPosition?.exists
    ? formatPerpsUsdc(currentPositionDxyExposureRaw, 6)
    : currentPositionDisplayAmount
  const unrealizedPnlRaw = currentPosition?.exists ? currentPosition.unrealizedPnlUsdc : undefined
  const accountSummaryPnlTone = unrealizedPnlRaw === undefined || unrealizedPnlRaw === 0n
    ? 'default'
    : unrealizedPnlRaw > 0n ? 'positive' : 'negative'
  const availableToTradeDisplayAmount = availableToTradeAmount ?? (enableLiveTrading ? '0' : AVAILABLE_TO_TRADE_AMOUNT)
  const canUseAvailableToTrade = parseAmount(availableToTradeDisplayAmount) > 0
  const hasCurrentPositionDisplayAmount = parseAmount(currentPositionInputAmount) > 0
  const dxyExposureUsdc = parsePerpsUsdc(size)
  const hasCurrentPosition = Boolean(currentPosition?.exists && currentPositionDxyExposureRaw > 0n)
  const isOppositePositionDirection = hasCurrentPosition && currentPosition !== undefined && direction !== currentPosition.direction
  const isReducingCurrentPosition = hasCurrentPosition && (isReduceOnly || isOppositePositionDirection)
  const isOracleFrozenClose = oracleFrozen && isReducingCurrentPosition
  const effectiveOrderDirection = isReducingCurrentPosition && currentPosition?.direction
    ? currentPosition.direction
    : direction
  const pendingCloseOrders = currentPosition?.exists
    ? pendingOrders.filter((order) => (
        order.isReduceOnly &&
        order.direction === currentPosition.direction &&
        order.sizeDelta > 0n
      ))
    : []
  const pendingCloseSizeRaw = pendingCloseOrders.reduce((total, order) => total + order.sizeDelta, 0n)
  const reservedCloseSizeRaw = currentPosition?.size === undefined
    ? 0n
    : minBigInt(pendingCloseSizeRaw, currentPosition.size)
  const availableCloseSizeRaw = currentPosition?.size === undefined || currentPosition.size <= reservedCloseSizeRaw
    ? 0n
    : currentPosition.size - reservedCloseSizeRaw
  const availableCloseDxyExposureRaw = currentPosition?.size === undefined || currentPosition.size <= 0n
    ? currentPositionDxyExposureRaw
    : sizeDeltaToNotionalUsdc(
        availableCloseSizeRaw,
        oraclePriceToDisplayDxyPrice(oraclePriceRaw)
      ) ?? 0n
  const pendingCloseDxyExposureRaw = sizeDeltaToNotionalUsdc(
    reservedCloseSizeRaw,
    oraclePriceToDisplayDxyPrice(oraclePriceRaw)
  ) ?? 0n
  const firstPendingCloseOrder = pendingCloseOrders
    .filter((order) => order.expiryTime !== undefined)
    .sort((a, b) => {
      const aExpiry = a.expiryTime ?? 0n
      const bExpiry = b.expiryTime ?? 0n
      return aExpiry < bExpiry ? -1 : aExpiry > bExpiry ? 1 : 0
    })
    .at(0) ?? pendingCloseOrders.at(0)
  const firstPendingCloseSecondsToExpiry = firstPendingCloseOrder?.expiryTime === undefined
    ? undefined
    : Number(firstPendingCloseOrder.expiryTime) - nowSeconds
  const pendingCloseContext = firstPendingCloseOrder === undefined
    ? 'An existing close order'
    : `Order #${firstPendingCloseOrder.orderId.toString()}`
  const pendingCloseExpiryContext = firstPendingCloseSecondsToExpiry === undefined
    ? ''
    : firstPendingCloseSecondsToExpiry <= 0
      ? isSponsoredAccountConfigured
        ? ` It is expired and awaiting keeper cleanup.`
        : ` It is expired and can be cleaned up.`
      : ` It expires in ${formatDuration(firstPendingCloseSecondsToExpiry)}.`
  const availableToTradeForMaxRaw = availableToTradeRaw ?? (enableLiveTrading ? 0n : parsePerpsUsdc(availableToTradeDisplayAmount))
  const selectedOpenCapacityUsdc = direction === 'long' ? longOpenCapacityUsdc : shortOpenCapacityUsdc
  const maxNotionalFromFundingRaw = canUseAvailableToTrade
    ? maxOpenNotionalForMargin(availableToTradeForMaxRaw, activeLeverage)
    : 0n
  const maxOpenNotionalRaw = selectedOpenCapacityUsdc === undefined
    ? maxNotionalFromFundingRaw
    : minBigInt(maxNotionalFromFundingRaw, selectedOpenCapacityUsdc)
  const maxOpenDxyExposureRaw = dxyExposureFromContractNotional(maxOpenNotionalRaw, oraclePriceRaw) ?? maxOpenNotionalRaw
  const maxDxyExposureForSizeInputRaw = isReducingCurrentPosition
    ? availableCloseDxyExposureRaw
    : maxOpenDxyExposureRaw
  const maxDxyExposureDisplayAmount = formatPerpsUsdc(maxDxyExposureForSizeInputRaw)
  const maxDxyExposureInputAmount = formatPerpsUsdc(maxDxyExposureForSizeInputRaw, 6)
  const maxDxyExposureRaw = maxDxyExposureForSizeInputRaw
  const canUseMaxNotional = maxDxyExposureRaw > 0n
  const currentPositionFillAmount = isReducingCurrentPosition ? maxDxyExposureInputAmount : currentPositionInputAmount
  const canUseCurrentPosition = isReducingCurrentPosition ? canUseMaxNotional : hasCurrentPositionDisplayAmount
  const effectiveDxyExposureUsdc = isFullCloseIntent && isReducingCurrentPosition
    ? availableCloseDxyExposureRaw
    : dxyExposureUsdc
  const sizeInputValue = isFullCloseIntent && isReducingCurrentPosition
    ? maxDxyExposureInputAmount
    : size

  const orderSizeDelta = (() => {
    if (effectiveDxyExposureUsdc <= 0n) return 0n
    if (
      isReducingCurrentPosition &&
      currentPosition?.size !== undefined &&
      currentPosition.size > 0n &&
      availableCloseSizeRaw > 0n &&
      maxDxyExposureRaw > 0n &&
      (isFullCloseIntent || effectiveDxyExposureUsdc >= maxDxyExposureRaw)
    ) {
      return availableCloseSizeRaw
    }

    return dxyExposureToSizeDelta(effectiveDxyExposureUsdc, oraclePriceRaw) ?? 0n
  })()
  const contractNotionalUsdc = orderSizeDelta > 0n
    ? sizeDeltaToNotionalUsdc(orderSizeDelta, oraclePriceRaw) ?? effectiveDxyExposureUsdc
    : effectiveDxyExposureUsdc
  const contractNotionalNumber = usdcRawToNumber(contractNotionalUsdc)
  const marginNumber = isReducingCurrentPosition ? 0 : activeLeverage > 0 ? contractNotionalNumber / activeLeverage : 0
  const marginUsdc = isReducingCurrentPosition ? 0n : activeLeverage > 0 ? contractNotionalUsdc / BigInt(activeLeverage) : 0n
  const defaultMaxLeverageMarginUsdc = contractNotionalUsdc > 0n
    ? contractNotionalUsdc / BigInt(DEFAULT_MAX_LEVERAGE)
    : 0n
  const simulatorMaxLeverageMarginUsdc = contractNotionalUsdc > 0n && simulatorMaxLeverage > 0
    ? contractNotionalUsdc / BigInt(simulatorMaxLeverage)
    : 0n
  const estimatedMaintenanceMarginUsdc = maintenanceMarginBps !== undefined && contractNotionalUsdc > 0n
    ? (contractNotionalUsdc * maintenanceMarginBps) / 10_000n
    : undefined
  const estimatedKeeperBountyUsdc = isReducingCurrentPosition ? CLOSE_BOUNTY_USDC_RAW : estimateOpenBountyUsdcRaw(contractNotionalUsdc)
  const keeperBounty = usdcRawToNumber(estimatedKeeperBountyUsdc)
  const executionFeeBpsRaw = executionFeeBps ?? BigInt(EXECUTION_FEE_BPS)
  const protocolExecutionFeeRaw = executionFeeUsdcRaw(contractNotionalUsdc, executionFeeBpsRaw)
  const slippageNumber = isOracleFrozenClose
    ? DEFAULT_ORACLE_FROZEN_SLIPPAGE
    : Math.max(slippage, 0)
  const previewPrice = oraclePriceRaw
    ? displayDxyPriceNumber(oraclePriceRaw)
    : enableLiveTrading
      ? undefined
      : 2 - MOCK_PREVIEW_PRICE
  const rawExecutionLimit = oraclePriceRaw
    ? getPerpsTargetPrice({ direction: effectiveOrderDirection, isClose: isReducingCurrentPosition, oraclePrice: oraclePriceRaw, slippagePercent: slippageNumber })
    : undefined
  const executionLimit = rawExecutionLimit === 0n
    ? null
    : rawExecutionLimit ? displayDxyPriceNumber(rawExecutionLimit) : !enableLiveTrading && Number.isFinite(slippageNumber)
      ? (2 - MOCK_PREVIEW_PRICE) * (direction === 'long' ? 1 + slippageNumber / 100 : 1 - slippageNumber / 100)
      : undefined
  const committedExecutionLimit = committedTargetPrice === undefined
    ? executionLimit
    : committedTargetPrice
  const committedSlippageNumber = committedSlippage ?? slippageNumber
  const liquidationPrice = previewPrice === undefined
    ? undefined
    : direction === 'long'
      ? previewPrice * 0.945
      : previewPrice * 1.055
  const summaryDxyExposureUsdc = isReducingCurrentPosition &&
    maxDxyExposureRaw > 0n &&
    (isFullCloseIntent || effectiveDxyExposureUsdc >= maxDxyExposureRaw)
    ? availableCloseDxyExposureRaw
    : effectiveDxyExposureUsdc
  const orderSummary = buildOrderSummary({
    currentPositionSide: currentPositionSideValue,
    currentPositionDxyExposureUsdc: currentPositionDxyExposureRaw,
    direction,
    isReduceOnly,
    leverage: activeLeverage,
    dxyExposureUsdc: summaryDxyExposureUsdc,
  })
  // Close bounties can be backed by collateral already attached to the position,
  // even when the account has no free buying power. The close preview and the
  // commit simulation are the authoritative checks for that backing.
  const openFundingRequirementUsdc = marginUsdc + estimatedKeeperBountyUsdc
  const marginShortfall = !isReducingCurrentPosition &&
    availableToTradeRaw !== undefined &&
    openFundingRequirementUsdc > availableToTradeRaw
    ? openFundingRequirementUsdc - availableToTradeRaw
    : 0n
  const isCorrectChain = chainId === PERPS_ARBITRUM_SEPOLIA_CHAIN_ID
  const isZeroSize = effectiveDxyExposureUsdc <= 0n
  const previewPublishTime = BigInt(oraclePublishTime ?? 0)
  const hasTradePreviewInputs = enableLiveTrading &&
    isConnected &&
    isCorrectChain &&
    orderSizeDelta > 0n &&
    oraclePriceRaw !== undefined &&
    oraclePriceRaw > 0n
  const shouldReadTradePreview = enableLiveTrading &&
    isConnected &&
    isCorrectChain &&
    orderSizeDelta > 0n &&
    oraclePriceRaw !== undefined &&
    oraclePriceRaw > 0n &&
    (isReducingCurrentPosition || previewPublishTime > 0n)
  const previewRequestSide = isReducingCurrentPosition
    ? 0
    : directionToPerpsSide(effectiveOrderDirection)
  const previewRequestMargin = isReducingCurrentPosition ? 0n : marginUsdc
  const previewRequestPublishTime = isReducingCurrentPosition ? 0n : previewPublishTime
  const tradePreviewRequest = useMemo<TradePreviewRequest | undefined>(() => {
    if (!shouldReadTradePreview) return undefined

    if (isReducingCurrentPosition) {
      return {
        kind: 'close',
        account: address ?? zeroAddress,
        sizeDelta: orderSizeDelta,
        oraclePrice: oraclePriceRaw,
      }
    }

    return {
      kind: 'open',
      account: address ?? zeroAddress,
      side: previewRequestSide,
      sizeDelta: orderSizeDelta,
      marginDelta: previewRequestMargin,
      oraclePrice: oraclePriceRaw,
      publishTime: previewRequestPublishTime,
    }
  }, [
    address,
    isReducingCurrentPosition,
    oraclePriceRaw,
    orderSizeDelta,
    previewRequestMargin,
    previewRequestPublishTime,
    previewRequestSide,
    shouldReadTradePreview,
  ])
  const debouncedTradePreviewRequest = useDebouncedValue(
    tradePreviewRequest,
    TRADE_PREVIEW_DEBOUNCE_MS
  )
  // Opening review is an explicit preflight boundary: read its exact current
  // inputs immediately, while free-form ticket edits remain debounced.
  const isTradePreviewDebouncing = !isReviewOpen && tradePreviewRequest !== undefined &&
    debouncedTradePreviewRequest !== tradePreviewRequest
  const isTradePreviewQueryEnabled = tradePreviewRequest !== undefined && !isTradePreviewDebouncing
  const {
    data: tradePreviewData,
    isLoading: isTradePreviewLoading,
    isFetching: isTradePreviewFetching,
  } = useReadContracts({
    contracts: isTradePreviewQueryEnabled
      ? [
          tradePreviewRequest.kind === 'close'
            ? {
                chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
                address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens,
                abi: PERPS_CFD_ENGINE_LENS_ABI,
                functionName: 'previewClose',
                args: [
                  tradePreviewRequest.account,
                  tradePreviewRequest.sizeDelta,
                  tradePreviewRequest.oraclePrice,
                ],
              } as const
            : {
                chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
                address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens,
                abi: PERPS_CFD_ENGINE_LENS_ABI,
                functionName: 'previewOpen',
                args: [
                  tradePreviewRequest.account,
                  tradePreviewRequest.side,
                  tradePreviewRequest.sizeDelta,
                  tradePreviewRequest.marginDelta,
                  tradePreviewRequest.oraclePrice,
                  tradePreviewRequest.publishTime,
                ],
              } as const,
        ]
      : [],
    query: {
      enabled: isTradePreviewQueryEnabled,
      refetchInterval: 15_000,
    },
  })
  const activeTradePreviewData = isTradePreviewQueryEnabled ? tradePreviewData : undefined
  const openPreview = !isReducingCurrentPosition
    ? parseOpenPreview(readResult(activeTradePreviewData as readonly ContractResult[] | undefined, 0))
      ?? (!enableLiveTrading ? openPreviewFixture : undefined)
    : undefined
  const closePreview = isReducingCurrentPosition
    ? parseClosePreview(readResult(activeTradePreviewData as readonly ContractResult[] | undefined, 0))
      ?? (!enableLiveTrading ? closePreviewFixture : undefined)
    : undefined
  const tradePreviewFailure = readFailure(activeTradePreviewData as readonly ContractResult[] | undefined, 0)
  const currentTradePreview = isReducingCurrentPosition ? closePreview : openPreview
  const isTradePreviewPending = shouldReadTradePreview && (
    isTradePreviewDebouncing ||
    (isTradePreviewQueryEnabled && isTradePreviewLoading) ||
    (isTradePreviewQueryEnabled && isTradePreviewFetching && currentTradePreview === undefined)
  )
  const minOpenDxyExposureUsdc = minOpenNotionalUsdc === undefined
    ? undefined
    : dxyExposureFromContractNotional(minOpenNotionalUsdc, oraclePriceRaw) ?? minOpenNotionalUsdc
  const minNewPositionDxyExposureUsdc = minNewPositionNotionalUsdc === undefined
    ? undefined
    : dxyExposureFromContractNotional(minNewPositionNotionalUsdc, oraclePriceRaw) ?? minNewPositionNotionalUsdc
  const isOpeningFromZero = !currentPosition?.exists && !isReducingCurrentPosition
  const effectiveMinOpenDxyExposureUsdc = isOpeningFromZero
    ? maxBigInt(minOpenDxyExposureUsdc ?? 0n, minNewPositionDxyExposureUsdc ?? 0n)
    : minOpenDxyExposureUsdc
  const selectedOpenDxyCapacityUsdc = selectedOpenCapacityUsdc === undefined
    ? undefined
    : dxyExposureFromContractNotional(selectedOpenCapacityUsdc, oraclePriceRaw) ?? selectedOpenCapacityUsdc
  const oldestPendingOrderSecondsToExpiry = firstPendingOrderExpiryTime === undefined
    ? undefined
    : Number(firstPendingOrderExpiryTime) - nowSeconds
  const canCleanupOldestPendingOrder = enableLiveTrading &&
    !isSponsoredAccountConfigured &&
    firstPendingOrderId !== undefined &&
    oldestPendingOrderSecondsToExpiry !== undefined &&
    oldestPendingOrderSecondsToExpiry <= 0
  const liveValidationError = (() => {
    if (!enableLiveTrading) return undefined
    if (!isConnected) return 'Connect wallet to trade.'
    if (isSponsoredAccountConfigured && identity.status !== 'ready') {
      return identity.error?.message ??
        'Confirm the Plether Trading Account before trading.'
    }
    if (!isCorrectChain) return 'Switch to Arbitrum Sepolia.'
    if (activePositionProtectionId > 0n) {
      return `Position protection #${activePositionProtectionId.toString()} is active. Cancel or finalize it before placing a discretionary order.`
    }
    if (!oraclePriceRaw || oraclePriceRaw <= 0n) return 'plDXY Perp price is not available.'
    if (isZeroSize) return 'Enter an order size.'
    if (
      isOppositePositionDirection &&
      currentPositionDxyExposureRaw > 0n &&
      effectiveDxyExposureUsdc > currentPositionDxyExposureRaw + SUMMARY_CLOSE_DUST_USDC_RAW
    ) {
      return 'One-step flips are not supported yet. Reduce or close the current position first, then open the other side.'
    }
    if (
      !isReducingCurrentPosition &&
      !isReduceOnly &&
      selectedOpenDxyCapacityUsdc !== undefined &&
      effectiveMinOpenDxyExposureUsdc !== undefined &&
      selectedOpenDxyCapacityUsdc < effectiveMinOpenDxyExposureUsdc
    ) {
      return getOpenCapacityUnavailableMessage({
        direction,
        isOpeningFromZero,
        minimumDxyExposureUsdc: effectiveMinOpenDxyExposureUsdc,
      })
    }
    if (
      !isReducingCurrentPosition &&
      !isReduceOnly &&
      effectiveMinOpenDxyExposureUsdc !== undefined &&
      effectiveDxyExposureUsdc < effectiveMinOpenDxyExposureUsdc
    ) {
      const minimumLabel = isOpeningFromZero ? 'Minimum new position' : 'Minimum order size'
      return `${minimumLabel} is ${formatPerpsUsdc(effectiveMinOpenDxyExposureUsdc)} USDC.`
    }
    if (!isReducingCurrentPosition && !isReduceOnly && selectedOpenDxyCapacityUsdc !== undefined && effectiveDxyExposureUsdc > selectedOpenDxyCapacityUsdc) {
      return `Max ${directionLabel(direction)} plDXY Perp exposure is ${formatPerpsUsdc(selectedOpenDxyCapacityUsdc)} USDC before hitting the market skew cap.`
    }
    if (isReduceOnly && !currentPosition?.exists) return 'No current position to reduce.'
    if (
      isReducingCurrentPosition &&
      currentPosition?.size !== undefined &&
      currentPosition.size > 0n &&
      availableCloseSizeRaw <= 0n &&
      pendingCloseSizeRaw >= currentPosition.size
    ) {
      return isSponsoredAccountConfigured
        ? `${pendingCloseContext} is already closing the full current position.${pendingCloseExpiryContext} Wait for keeper finalization or cleanup before submitting another reduce order.`
        : `${pendingCloseContext} is already closing the full current position.${pendingCloseExpiryContext} Execute it or clean it up before submitting another reduce order.`
    }
    if (
      isReducingCurrentPosition &&
      currentPositionDxyExposureRaw > 0n &&
      availableCloseDxyExposureRaw > 0n &&
      effectiveDxyExposureUsdc > availableCloseDxyExposureRaw + SUMMARY_CLOSE_DUST_USDC_RAW
    ) {
      return pendingCloseDxyExposureRaw > 0n
        ? `Only ${formatPerpsUsdc(availableCloseDxyExposureRaw)} USDC plDXY Perp exposure is available to reduce because ${formatPerpsUsdc(pendingCloseDxyExposureRaw)} USDC is already reserved by pending close orders.`
        : `Only ${formatPerpsUsdc(availableCloseDxyExposureRaw)} USDC plDXY Perp exposure is available to reduce at the latest plDXY Perp price.`
    }
    if (
      pendingOrderCount !== undefined &&
      maxPendingOrders !== undefined &&
      BigInt(pendingOrderCount) >= maxPendingOrders
    ) {
      const expiryContext = oldestPendingOrderSecondsToExpiry === undefined
        ? 'Waiting for expiry data.'
        : oldestPendingOrderSecondsToExpiry <= 0
          ? Math.abs(oldestPendingOrderSecondsToExpiry) <= 1
            ? 'Oldest pending order expires now.'
            : `Oldest pending order expired ${formatDuration(Math.abs(oldestPendingOrderSecondsToExpiry))} ago.`
          : `Oldest pending order expires in ${formatDuration(oldestPendingOrderSecondsToExpiry)}.`

      return isSponsoredAccountConfigured
        ? `You already have ${pendingOrderCount.toString()} pending orders, which is the current account limit. ${expiryContext} Wait for the keeper to finalize or clean up an order before committing a new one.`
        : `You already have ${pendingOrderCount.toString()} pending orders, which is the current account limit. ${expiryContext} Execute or clean up an expired order before committing a new one.`
    }
    if (marginShortfall > 0n) return `Deposit ${formatPerpsUsdc(marginShortfall)} USDC more before committing this order.`
    if (hasTradePreviewInputs && !isReducingCurrentPosition && previewPublishTime <= 0n) {
      return 'Waiting for fresh oracle publish time before previewing this order.'
    }
    if (shouldReadTradePreview) {
      if (tradePreviewFailure) {
        return 'Trade preview failed. Refresh market data and retry before reviewing this order.'
      }
      if (isTradePreviewPending) return undefined
      if (isReducingCurrentPosition) {
        if (closePreview === undefined) return 'Trade preview is unavailable. Refresh market data and retry.'
        if (!closePreview.valid) return getPerpsCloseInvalidReasonMessage(closePreview.invalidReason)
      } else {
        if (openPreview === undefined) return 'Trade preview is unavailable. Refresh market data and retry.'
        if (!openPreview.valid) return getPerpsOpenRevertMessage(openPreview.invalidReason)
      }
    }
    return undefined
  })()
  useEffect(() => {
    if (
      !enableLiveTrading ||
      !isReviewOpen ||
      lifecycleState !== 'preview' ||
      liveValidationError ||
      typeof prepareOrder !== 'function'
    ) {
      setPreparedOrder(undefined)
      setIsExecutionProtectionsLoading(false)
      setExecutionProtectionsError(undefined)
      return
    }

    let cancelled = false
    setPreparedOrder(undefined)
    setIsExecutionProtectionsLoading(true)
    setExecutionProtectionsError(undefined)
    void prepareOrder({
      direction: effectiveOrderDirection,
      notionalUsdc: contractNotionalUsdc,
      sizeDelta: orderSizeDelta,
      marginUsdc,
      oraclePrice: oraclePriceRaw ?? 0n,
      slippagePercent: slippageNumber,
      isClose: isReducingCurrentPosition,
      selectedMaxLeverageBps: Math.round(activeLeverage * 10_000),
    }).then((prepared) => {
      if (!cancelled) setPreparedOrder(prepared)
    }).catch((error: unknown) => {
      if (!cancelled) {
        setExecutionProtectionsError(
          error instanceof Error
            ? error.message
            : 'Execution protections could not be prepared'
        )
      }
    }).finally(() => {
      if (!cancelled) setIsExecutionProtectionsLoading(false)
    })

    return () => {
      cancelled = true
    }
  }, [
    activeLeverage,
    contractNotionalUsdc,
    effectiveOrderDirection,
    enableLiveTrading,
    isReducingCurrentPosition,
    isReviewOpen,
    lifecycleState,
    liveValidationError,
    marginUsdc,
    oraclePriceRaw,
    orderSizeDelta,
    prepareOrder,
    slippageNumber,
  ])
  const displayedValidationError = enableLiveTrading
    ? liveValidationError
    : validationErrorFixture
  const displayedExecutionProtections = enableLiveTrading
    ? preparedOrder
    : executionProtectionsFixture
  const shouldShowExecutionProtections = enableLiveTrading ||
    displayedExecutionProtections !== undefined
  const previewContractNotionalUsdc = openPreview?.notionalUsdc ?? contractNotionalUsdc
  const previewInitialMarginUsdc = openPreview?.marginDeltaUsdc ?? marginUsdc
  const previewMaintenanceMarginUsdc = openPreview?.maintenanceMarginUsdc
  const previewExecutionFeeUsdc = isReducingCurrentPosition
    ? closePreview?.executionFeeUsdc ?? protocolExecutionFeeRaw
    : openPreview?.executionFeeUsdc ?? protocolExecutionFeeRaw
  const previewVpiUsdc = isReducingCurrentPosition ? closePreview?.vpiDeltaUsdc : openPreview?.vpiUsdc
  const previewLensFallbackValue = isTradePreviewPending ? PREVIEW_LOADING_VALUE : PREVIEW_UNAVAILABLE_VALUE
  const previewLensFallbackTone = isTradePreviewPending ? 'muted' : undefined
  const previewVpiAction = formatTradeVpi(
    previewVpiUsdc,
    'estimate',
    previewLensFallbackValue,
    previewLensFallbackTone
  )
  const previewPositionVpiAccrued = isReducingCurrentPosition
    ? currentPosition?.vpiAccrued
    : openPreview?.postVpiAccrued
  const positionVpiBalanceRow = useMemo<PreviewRow>(() => {
    const fallback = isReducingCurrentPosition
      ? formatVpiBalance(undefined)
      : formatVpiBalance(undefined, previewLensFallbackValue, previewLensFallbackTone)

    return {
      label: 'Position VPI balance',
      ...formatVpiBalance(
        previewPositionVpiAccrued,
        fallback.value,
        fallback.tone
      ),
      tooltip: POSITION_VPI_BALANCE_TOOLTIP,
      tooltipDocsLink: DOCS_LINKS.virtualPriceImpact,
    }
  }, [
    isReducingCurrentPosition,
    previewPositionVpiAccrued,
    previewLensFallbackTone,
    previewLensFallbackValue,
  ])
  const previewFrozenCloseSpreadValue = closePreview === undefined
    ? previewLensFallbackValue
    : formatUsdcRaw(closePreview.frozenSpreadUsdc)
  const previewMaintenanceMarginValue = previewMaintenanceMarginUsdc === undefined
    ? previewLensFallbackValue
    : formatUsdcRaw(previewMaintenanceMarginUsdc)
  const previewVpiValue = previewVpiAction.value
  const previewLiquidationPrice = (() => {
    if (!enableLiveTrading) return formatOptionalPrice(liquidationPrice)
    if (openPreview === undefined) return shouldReadTradePreview ? previewLensFallbackValue : PREVIEW_UNAVAILABLE_VALUE
    if (!openPreview.hasLiquidationPrice) return PREVIEW_UNAVAILABLE_VALUE
    return formatDisplayDxyPrice(openPreview.liquidationPrice)
  })()
  const previewResultingLeverage = (() => {
    if (!enableLiveTrading) return formatLeverage(activeLeverage)
    if (isTradePreviewPending) return PREVIEW_LOADING_VALUE

    if (isReducingCurrentPosition) {
      if (closePreview === undefined) return shouldReadTradePreview ? PREVIEW_UNAVAILABLE_VALUE : formatLeverage(activeLeverage)
      if (closePreview.remainingSize <= 0n) return 'Closed'

      return formatLeverageRaw(
        sizeDeltaToNotionalUsdc(closePreview.remainingSize, closePreview.executionPrice),
        closePreview.remainingMargin
      )
    }

    if (openPreview === undefined) return shouldReadTradePreview ? PREVIEW_UNAVAILABLE_VALUE : formatLeverage(activeLeverage)

    return formatLeverageRaw(
      sizeDeltaToNotionalUsdc(openPreview.postSize, openPreview.executionPrice),
      openPreview.postMarginUsdc
    )
  })()
  const adverseOracleConfidenceSpreadValue = useMemo(
    () => formatAdverseOracleConfidenceSpread(latestBasket, adverseConfidenceMultiplierBps) ?? PREVIEW_UNAVAILABLE_VALUE,
    [adverseConfidenceMultiplierBps, latestBasket]
  )
  const rawOracleConfidenceSpreadValue = useMemo(
    () => formatOracleConfidenceSpreadPercent(
      calculateRawBasketOracleConfidenceSpreadPercent(latestBasket)
    ) ?? PREVIEW_UNAVAILABLE_VALUE,
    [latestBasket]
  )
  const adverseOracleConfidenceMultiplierValue = useMemo(
    () => formatAdverseConfidenceMultiplier(adverseConfidenceMultiplierBps) ?? PREVIEW_UNAVAILABLE_VALUE,
    [adverseConfidenceMultiplierBps]
  )
  const adverseOracleConfidenceSpreadTooltip = useMemo(
    () => (
      <div className="space-y-2">
        <p>
          Oracle confidence spread is the uncertainty range around the latest basket price. The adverse spread is that range after the protocol applies its safety multiplier.
        </p>
        <p>
          It applies to opens and to close/reduce execution in live and FAD-only regimes. An oracle-frozen close/reduce replaces this row with the fixed frozen close spread.
        </p>
        <p>
          Calculation: <span className="font-semibold text-content-primary">{rawOracleConfidenceSpreadValue}</span> raw spread * <span className="font-semibold text-content-primary">{adverseOracleConfidenceMultiplierValue}</span> = <span className="font-semibold text-content-primary">{adverseOracleConfidenceSpreadValue}</span>.
        </p>
      </div>
    ),
    [
      adverseOracleConfidenceMultiplierValue,
      adverseOracleConfidenceSpreadValue,
      rawOracleConfidenceSpreadValue,
    ]
  )

  const previewRows = useMemo<PreviewRow[]>(
    () => [
      {
        label: 'plDXY Perp price',
        value: (
          <DxyPricePreviewValue
            value={oraclePriceDisplay ?? formatOptionalPrice(previewPrice)}
            publishTime={oraclePublishTime}
            nowSeconds={nowSeconds}
            freshness={oracleFreshness}
            freshnessTooltip={oracleFreshnessTooltip}
          />
        ),
      },
      { label: 'plDXY Perp exposure', value: formatUsdc(dxyExposureNumber) },
      {
        label: 'Contract notional',
        value: formatUsdcRaw(previewContractNotionalUsdc),
        tooltip: CONTRACT_NOTIONAL_TOOLTIP,
        tooltipDocsLink: DOCS_LINKS.contractNotional,
      },
      { label: 'Required margin', value: formatUsdcRaw(previewInitialMarginUsdc) },
      {
        label: 'Maintenance margin',
        value: previewMaintenanceMarginValue,
        tone: previewMaintenanceMarginUsdc === undefined ? previewLensFallbackTone : undefined,
        tooltip: MAINTENANCE_MARGIN_TOOLTIP,
        tooltipDocsLink: DOCS_LINKS.maintenanceMargin,
      },
      { label: 'Resulting leverage', value: previewResultingLeverage, tone: previewResultingLeverage === PREVIEW_LOADING_VALUE ? 'muted' : undefined },
      { label: 'Max slippage', value: formatPercent(slippageNumber) },
      {
        label: 'Execution limit',
        value: formatOptionalPrice(executionLimit),
        tooltip: EXECUTION_LIMIT_TOOLTIP,
        tooltipDocsLink: DOCS_LINKS.executionLimit,
      },
      isOracleFrozenClose
        ? {
            label: 'Estimated frozen close spread',
            value: previewFrozenCloseSpreadValue,
            tone: previewFrozenCloseSpreadValue === PREVIEW_LOADING_VALUE ? 'muted' as const : undefined,
            tooltip: FROZEN_CLOSE_SPREAD_TOOLTIP,
            tooltipDocsLink: DOCS_LINKS.frozenCloseSpread,
          }
        : {
            label: 'Adverse oracle confidence spread',
            value: adverseOracleConfidenceSpreadValue,
            tooltip: adverseOracleConfidenceSpreadTooltip,
            tooltipDocsLink: DOCS_LINKS.oracleConfidence,
          },
      { label: 'Liquidation price', value: previewLiquidationPrice, tone: previewLiquidationPrice === PREVIEW_LOADING_VALUE ? 'muted' : undefined },
      { label: 'Estimated fee', value: formatUsdcRaw(previewExecutionFeeUsdc) },
      ...(isOpeningFromZero ? [] : [positionVpiBalanceRow]),
      {
        label: 'VPI',
        value: previewVpiValue,
        tone: previewVpiAction.tone,
        tooltip: isReducingCurrentPosition ? CLOSE_VPI_TOOLTIP : VPI_PRICE_IMPACT_TOOLTIP,
        tooltipDocsLink: DOCS_LINKS.virtualPriceImpact,
      },
      {
        label: 'Estimated execution reward',
        value: formatUsdc(keeperBounty),
        tooltip: EXECUTION_REWARD_TOOLTIP,
        tooltipDocsLink: DOCS_LINKS.executionReward,
      },
      {
        label: 'Contract side capacity',
        value: selectedOpenCapacityUsdc === undefined
          ? 'Unavailable'
          : formatUsdcRaw(selectedOpenCapacityUsdc),
        tone: selectedOpenCapacityUsdc === undefined ? undefined : 'positive',
      },
    ],
    [
      executionLimit,
      keeperBounty,
      oraclePriceDisplay,
      oracleFreshness,
      oracleFreshnessTooltip,
      oraclePublishTime,
      nowSeconds,
      previewPrice,
      previewContractNotionalUsdc,
      previewExecutionFeeUsdc,
      previewInitialMarginUsdc,
      previewResultingLeverage,
      previewLiquidationPrice,
      previewMaintenanceMarginValue,
      previewVpiValue,
      previewVpiAction.tone,
      previewLensFallbackTone,
      previewMaintenanceMarginUsdc,
      isOpeningFromZero,
      isReducingCurrentPosition,
      positionVpiBalanceRow,
      selectedOpenCapacityUsdc,
      dxyExposureNumber,
      slippageNumber,
      adverseOracleConfidenceSpreadValue,
      adverseOracleConfidenceSpreadTooltip,
      isOracleFrozenClose,
      previewFrozenCloseSpreadValue,
    ]
  )
  const sidePanelPreviewRows = useMemo(
    () => previewRows.filter((row) =>
      row.label !== 'Resulting leverage' &&
      row.label !== 'plDXY Perp exposure' &&
      row.label !== 'Contract side capacity'
    ),
    [previewRows]
  )
  const visibleSidePanelPreviewRows = useMemo(
    () => isPreviewExpanded
      ? sidePanelPreviewRows
      : sidePanelPreviewRows.filter((row) => COMPACT_PREVIEW_ROW_LABELS.has(row.label)),
    [isPreviewExpanded, sidePanelPreviewRows]
  )

  const currentLifecycleStep = lifecycleStep(lifecycleState)
  const displayOrderId = orderId === undefined ? (enableLiveTrading ? '--' : ORDER_ID) : orderId.toString()
  const executedOrderHistoryRow = orderId === undefined
    ? undefined
    : orderHistory.find((row) => row.orderId === orderId && row.status === 'Executed')
  const finalIsClose = committedIsClose
    ?? historyOrderIsClose(executedOrderHistoryRow)
    ?? isReducingCurrentPosition
  const finalIsFullClose = finalIsClose && committedIsFullClose === true
  const displayCommitTx = commitTxHash ?? executedOrderHistoryRow?.commitTxHash ?? (enableLiveTrading ? undefined : COMMIT_TX)
  const displayExecuteTx = executeTxHash ?? executedOrderHistoryRow?.revealTxHash ?? (enableLiveTrading ? undefined : EXECUTE_TX)
  const displayCommitTxValue = displayCommitTx ? <TxHashActions hash={displayCommitTx} /> : '--'
  const displayExecuteTxValue = displayExecuteTx ? <TxHashActions hash={displayExecuteTx} /> : '--'
  const displayUserOperationHash =
    initialUserOperationHash ?? latestSponsoredOperation?.userOperationHash
  const displayUserOperationHashValue = displayUserOperationHash
    ? (
        <UserOperationHashActions
          hash={displayUserOperationHash}
          explorerUrlTemplate={identity.manifest?.userOperationExplorerUrlTemplate}
        />
      )
    : '--'
  const isTerminalRevealError = flowError !== undefined &&
    (isOrderNoLongerPendingMessage(flowError) || isTerminalOrderFailureMessage(flowError))
  const shouldShowFinalizationProgress = enableLiveTrading || showFinalizationProgress
  const isKeeperRevealGraceActive = shouldShowFinalizationProgress &&
    lifecycleState === 'revealPending' &&
    (
      isSponsoredAccountConfigured ||
      keeperRevealDeadlineMs === undefined ||
      keeperRevealNowMs < keeperRevealDeadlineMs
    )
  const keeperRevealRemainingSeconds = keeperRevealDeadlineMs === undefined
    ? Math.ceil(KEEPER_REVEAL_GRACE_MS / 1_000)
    : Math.max(0, Math.ceil((keeperRevealDeadlineMs - keeperRevealNowMs) / 1_000))
  const keeperRevealProgressPercent = keeperRevealDeadlineMs === undefined
    ? 0
    : Math.max(
      0,
      Math.min(
        100,
        ((KEEPER_REVEAL_GRACE_MS - Math.max(0, keeperRevealDeadlineMs - keeperRevealNowMs)) / KEEPER_REVEAL_GRACE_MS) * 100
      )
    )
  const finalizationLoadingDescription = finalizationLoadingMessage.subtitle
  const finalExecutedNotionalUsdc = finalExecutionPrice
    ? sizeDeltaToNotionalUsdc(committedSizeDelta, finalExecutionPrice)
    : undefined
  const finalExecutedDxyExposureUsdc = finalExecutionPrice
    ? sizeDeltaToNotionalUsdc(committedSizeDelta, oraclePriceToDisplayDxyPrice(finalExecutionPrice))
    : undefined
  const finalProtocolExecutionFee = executionFeeUsdcRaw(finalExecutedNotionalUsdc ?? contractNotionalUsdc, executionFeeBpsRaw)
  const finalExecutionEconomicsComplete = finalExecutionEconomicsVersion !== undefined
  const finalVpiAction = formatTradeVpi(finalVpiUsdc, 'final')
  const finalVpiValue = !finalExecutionEconomicsComplete || finalVpiUsdc === undefined
    ? 'Unavailable'
    : finalIsClose ? finalVpiAction.value : formatSignedUsdcNoPlus(finalVpiUsdc)
  const finalUsesFrozenCloseSpread =
    finalExecutionEconomicsComplete && finalExecutionOracleFrozen === true
  const finalFrozenCloseSpreadValue = finalExecutionFrozenCloseSpreadUsdc === undefined
    ? PREVIEW_UNAVAILABLE_VALUE
    : formatUsdcRaw(finalExecutionFrozenCloseSpreadUsdc)
  const committedVpiAction = formatTradeVpi(committedVpiUsdc, 'estimate')
  const committedPositionVpiBalance = formatVpiBalance(committedPositionVpiAccrued)
  const committedVpiRows: PreviewRow[] = [
    ...(committedShowsPositionVpiBalance
      ? [{
          label: 'Position VPI balance',
          ...committedPositionVpiBalance,
          tooltip: POSITION_VPI_BALANCE_TOOLTIP,
          tooltipDocsLink: DOCS_LINKS.virtualPriceImpact,
        }]
      : []),
    {
      label: 'VPI',
      ...committedVpiAction,
      tooltip: committedIsClose ? CLOSE_VPI_TOOLTIP : VPI_PRICE_IMPACT_TOOLTIP,
      tooltipDocsLink: DOCS_LINKS.virtualPriceImpact,
    },
  ]
  const finalOracleConfidenceSpreadRaw =
    finalExecutionEconomicsComplete
      && finalExecutionOracleFrozen === false
      && finalExecutionPrice !== undefined
      && finalExecutionOraclePrice !== undefined
      ? absBigInt(finalExecutionPrice - finalExecutionOraclePrice)
      : undefined
  const finalOracleConfidenceSpreadValue = formatConfidenceSpread(
    finalOracleConfidenceSpreadRaw,
    finalExecutionOraclePrice
  )
  const finalPriceDisplay = finalExecutionPrice
    ? formatDisplayDxyPrice(finalExecutionPrice)
    : enableLiveTrading
      ? '--'
      : formatDisplayDxyPrice(99_110_000n)
  const executedTitle = finalPriceDisplay === '--'
    ? finalIsFullClose ? 'Position closed' : finalIsClose ? 'Position reduced' : 'Trade executed'
    : finalIsFullClose
      ? `${directionLabel(oppositeDirection(direction))} position closed at ${finalPriceDisplay} USDC`
      : finalIsClose
        ? `${directionLabel(oppositeDirection(direction))} position reduced at ${finalPriceDisplay} USDC`
        : `Trade executed at ${finalPriceDisplay} USDC`
  const isReviewingFullClose = isReducingCurrentPosition &&
    availableCloseDxyExposureRaw > 0n &&
    (isFullCloseIntent || availableCloseDxyExposureRaw <= effectiveDxyExposureUsdc + SUMMARY_CLOSE_DUST_USDC_RAW)
  const reviewCtaLabel = enableLiveTrading && !isConnected
    ? 'Connect Wallet'
    : enableLiveTrading && !isCorrectChain
      ? 'Switch Network'
      : isReducingCurrentPosition
        ? isReviewingFullClose ? 'Review Close' : 'Review Reduce'
      : direction === 'long' ? 'Review Long' : 'Review Short'
  const isConnectWalletCta = enableLiveTrading && !isConnected
  const isSwitchNetworkCta = enableLiveTrading && isConnected && !isCorrectChain
  const isReviewButtonDisabled = (
    enableLiveTrading &&
    isConnected &&
    isCorrectChain &&
    (Boolean(liveValidationError) || isTradePreviewPending)
  ) || (!enableLiveTrading && Boolean(validationErrorFixture))
  const marginActionAmountRaw = parsePerpsUsdc(marginActionAmount)
  const marginActionLabel = marginAction === 'withdraw' ? 'Withdraw' : 'Deposit'
  const ownerWalletBalance = ownerWalletUsdcRaw ?? walletUsdcRaw
  const usesOwnerDepositAuthorization = isSponsoredAccountConfigured &&
    identity.manifest?.smartAccountMode === 'simple' &&
    identity.manifest.usdcSupportsEip3009
  const usesTradingAccountDepositBalance = isSponsoredAccountConfigured &&
    identity.manifest?.smartAccountMode === 'simple' &&
    !identity.manifest.usdcSupportsEip3009
  const effectiveOwnerWalletBalance = locallyConfirmedFundingBalances &&
      ownerWalletBalance !== undefined
    ? ownerWalletBalance < locallyConfirmedFundingBalances.ownerWallet
      ? ownerWalletBalance
      : locallyConfirmedFundingBalances.ownerWallet
    : ownerWalletBalance
  const effectiveTradingAccountBalance = locallyConfirmedFundingBalances &&
      tradingAccountUsdcRaw !== undefined
    ? tradingAccountUsdcRaw > locallyConfirmedFundingBalances.tradingAccount
      ? tradingAccountUsdcRaw
      : locallyConfirmedFundingBalances.tradingAccount
    : tradingAccountUsdcRaw
  const manualTransferDepositBalance = effectiveOwnerWalletBalance !== undefined &&
      effectiveTradingAccountBalance !== undefined
    ? effectiveOwnerWalletBalance + effectiveTradingAccountBalance
    : undefined
  const depositSourceBalance = usesTradingAccountDepositBalance
    ? manualTransferDepositBalance
    : ownerWalletBalance
  const marginActionLimit = marginAction === 'withdraw'
    ? withdrawableUsdcRaw
    : depositSourceBalance
  const marginActionLimitLabel = marginAction === 'withdraw'
    ? 'Withdrawable'
    : 'Available to deposit'
  const marginActionLimitDisplay = formatPerpsUsdc(marginActionLimit)
  const canUseMarginActionMax = marginActionLimit !== undefined && marginActionLimit > 0n
  const isMarginActionInsufficient = marginActionLimit !== undefined && marginActionAmountRaw > marginActionLimit
  const isDepositBalanceUnavailable = marginAction === 'deposit' && marginActionLimit === undefined
  const isMarginActionInvalid = marginActionAmountRaw <= 0n ||
    isMarginActionInsufficient ||
    isDepositBalanceUnavailable
  const ownerWalletTransferAmountRaw = marginAction === 'deposit' &&
      usesTradingAccountDepositBalance &&
      effectiveTradingAccountBalance !== undefined &&
      marginActionAmountRaw > effectiveTradingAccountBalance
    ? marginActionAmountRaw - effectiveTradingAccountBalance
    : 0n
  const requiresOwnerWalletTransfer = ownerWalletTransferAmountRaw > 0n
  const isMarginActionPending = marginActionStatus === 'pending' ||
    marginActionStatus === 'funding' ||
    marginActionStatus === 'depositing'
  const marginActionCtaLabel = enableLiveTrading && !isConnected
    ? 'Connect Wallet'
    : enableLiveTrading && !isCorrectChain
      ? 'Switch Network'
      : marginActionStatus === 'funding'
        ? 'Transferring USDC'
        : marginActionStatus === 'depositing'
          ? 'Depositing'
          : requiresOwnerWalletTransfer
            ? 'Transfer & Deposit'
            : marginActionLabel
  const marginActionCurrentCollateral = currentPosition?.exists
    ? currentPosition.marginUsdc
    : undefined
  const shouldShowMarginActionPositionContext = currentPosition?.exists && marginActionCurrentCollateral !== undefined
  const areMarginActionsDisabled = enableLiveTrading && !isConnected
  const isMarginActionSubmitDisabled = isMarginActionPending
    || (enableLiveTrading && isConnected && isCorrectChain && isMarginActionInvalid)
  const commonAnalyticsProperties = useMemo<PerpsAnalyticsProperties>(() => ({
    market_phase: marketPhase,
    lifecycle_state: lifecycleState,
    direction,
    reduce_only: isReduceOnly,
    connected_state: perpsConnectedState(isConnected),
    chain_state: perpsChainState(isConnected, isCorrectChain),
    size_bucket: perpsSizeBucket(dxyExposureNumber),
  }), [
    direction,
    dxyExposureNumber,
    isConnected,
    isCorrectChain,
    isReduceOnly,
    lifecycleState,
    marketPhase,
  ])

  useEffect(() => {
    if (!liveValidationError || isZeroSize) return

    trackPerpsValidationBlocked(validationReasonCategory(liveValidationError), commonAnalyticsProperties)
  }, [
    commonAnalyticsProperties,
    isZeroSize,
    liveValidationError,
  ])

  useEffect(() => {
    if (lifecycleState === 'preview') {
      terminalLifecycleTrackedRef.current = undefined
      return
    }
    if (lifecycleState !== 'executed' || terminalLifecycleTrackedRef.current === 'executed') return

    terminalLifecycleTrackedRef.current = 'executed'
    trackPerpsOrderLifecycle('executed', commonAnalyticsProperties)
  }, [commonAnalyticsProperties, lifecycleState])

  const openMarginAction = useCallback((action: MarginAction) => {
    trackPerpsMarginLifecycle(`${action}_opened`, commonAnalyticsProperties)
    setMarginAction(action)
    setMarginActionAmount('')
    setMarginActionStatus('idle')
    setMarginActionError(undefined)
    setLocallyConfirmedFundingBalances(null)
  }, [commonAnalyticsProperties])

  useEffect(() => {
    if (!marginActionRequest) return
    if (handledMarginActionRequestRef.current === marginActionRequest.id) return

    handledMarginActionRequestRef.current = marginActionRequest.id
    openMarginAction(marginActionRequest.action)
    clearMarginActionRequest(marginActionRequest.id)
  }, [clearMarginActionRequest, marginActionRequest, openMarginAction])

  async function handleMarginActionSubmit() {
    if (!marginAction) return
    if (enableLiveTrading && !isConnected) {
      void openAppKit()
      return
    }
    if (enableLiveTrading && !isCorrectChain) {
      void switchToArbitrumSepolia()
      return
    }
    if (isMarginActionInvalid) {
      trackPerpsValidationBlocked('margin_amount_invalid', commonAnalyticsProperties)
      return
    }

    let ownerWalletTransferConfirmed = false
    try {
      trackPerpsMarginLifecycle(`${marginAction}_submitted`, commonAnalyticsProperties)
      setMarginActionError(undefined)
      if (marginAction === 'deposit') {
        if (requiresOwnerWalletTransfer) {
          if (
            effectiveOwnerWalletBalance === undefined ||
            effectiveTradingAccountBalance === undefined
          ) {
            throw new Error('Wallet balances are still loading. Refresh and retry.')
          }
          setMarginActionStatus('funding')
          await fundTradingAccount(ownerWalletTransferAmountRaw)
          ownerWalletTransferConfirmed = true
          setLocallyConfirmedFundingBalances({
            ownerWallet: effectiveOwnerWalletBalance - ownerWalletTransferAmountRaw,
            tradingAccount: effectiveTradingAccountBalance + ownerWalletTransferAmountRaw,
          })
          void onAccountRefresh?.()
        }
        setMarginActionStatus('depositing')
        const depositSource = isSponsoredAccountConfigured &&
          identity.manifest?.smartAccountMode === 'simple' &&
          identity.manifest.usdcSupportsEip3009
          ? 'owner'
          : 'account'
        await depositMargin(
          marginActionAmountRaw,
          marginAllowanceUsdc,
          depositSource
        )
      } else {
        setMarginActionStatus('pending')
        await withdrawMargin(marginActionAmountRaw)
      }
      setMarginActionStatus('idle')
      setMarginAction(null)
      setMarginActionAmount('')
      setLocallyConfirmedFundingBalances(null)
      trackPerpsMarginLifecycle(`${marginAction}_succeeded`, commonAnalyticsProperties)
      void onAccountRefresh?.()
    } catch (error) {
      setMarginActionStatus('failed')
      const errorMessage = error instanceof Error
        ? error.message
        : `${marginActionLabel} failed. Check wallet and retry.`
      setMarginActionError(ownerWalletTransferConfirmed
        ? `The transfer succeeded, but the Margin Account deposit failed. The USDC remains in your Trading Account and will not be transferred again when you retry. ${errorMessage}`
        : errorMessage)
      trackPerpsMarginLifecycle(`${marginAction}_failed`, {
        ...commonAnalyticsProperties,
        error_category: perpsErrorCategory(error),
      })
    }
  }

  async function handleConfirmCommit() {
    const commitAttemptId = commitAttemptIdRef.current + 1
    commitAttemptIdRef.current = commitAttemptId
    deferredSafeConfirmationErrorRef.current = undefined
    rejectedTerminalRef.current = undefined
    executionEvidencePollRef.current = undefined
    setFlowError(undefined)
    setWalletRequestWarning(undefined)
    setCommitExecutionStatus(undefined)
    setCommittedIsClose(isReducingCurrentPosition)
    setCommittedIsFullClose(isReviewingFullClose)
    setCommittedVpiUsdc(previewVpiUsdc)
    setCommittedPositionVpiAccrued(previewPositionVpiAccrued)
    setCommittedShowsPositionVpiBalance(!isOpeningFromZero)
    debugPerpsCommit('ticket:confirm-click', {
      enableLiveTrading,
      isConnected,
      isCorrectChain,
      chainId,
      address,
      lifecycleState,
      liveValidationError,
      direction,
      effectiveOrderDirection,
      isReducingCurrentPosition,
      dxyExposureUsdc: effectiveDxyExposureUsdc,
      contractNotionalUsdc,
      marginUsdc,
      oraclePriceRaw,
      slippageNumber,
    })
    if (!enableLiveTrading) {
      debugPerpsCommit('ticket:mock-flow')
      trackPerpsOrderLifecycle('commit_started', commonAnalyticsProperties)
      setCommittedSizeDelta(orderSizeDelta)
      setCommittedSlippage(slippageNumber)
      setCommittedTargetPrice(executionLimit)
      setCommitExecutionStatus('awaiting-signature')
      setLifecycleState('commitPending')
      return
    }
    if (liveValidationError) {
      debugPerpsCommit('ticket:blocked-by-validation', {
        liveValidationError,
      })
      trackPerpsValidationBlocked(validationReasonCategory(liveValidationError), commonAnalyticsProperties)
      setFlowError(liveValidationError)
      return
    }
    if (!preparedOrder) {
      setFlowError(
        executionProtectionsError ??
          'Wait for execution protections to finish before confirming.'
      )
      return
    }

    try {
      debugPerpsCommit('ticket:lifecycle:commitPreparing')
      trackPerpsOrderLifecycle('commit_started', commonAnalyticsProperties)
      setLifecycleState('commitPreparing')
      const sizeDelta = orderSizeDelta
      setCommittedSizeDelta(sizeDelta)
      setCommittedSlippage(slippageNumber)
      setCommittedTargetPrice(executionLimit)
      setFinalExecutionPrice(undefined)
      setFinalExecutionOraclePrice(undefined)
      setFinalExecutionOracleFrozen(undefined)
      setFinalExecutionFrozenCloseSpreadUsdc(undefined)
      setFinalExecutionEconomicsVersion(undefined)
      setFinalVpiUsdc(undefined)
      const applyIncludedCommit = (result: {
        account: string
        clientOrderId: string
        hash?: string
        orderId: bigint
      }) => {
        if (commitAttemptIdRef.current !== commitAttemptId) return

        const previousIdentity = includedCommitIdentityRef.current
        const inclusionChanged = previousIdentity !== undefined &&
          (
            previousIdentity.orderId !== result.orderId ||
            previousIdentity.clientOrderId.toLowerCase() !==
              result.clientOrderId.toLowerCase() ||
            previousIdentity.account.toLowerCase() !== result.account.toLowerCase()
          )
        includedCommitIdentityRef.current = result
        deferredSafeConfirmationErrorRef.current = undefined
        if (result.hash) setCommitTxHash(result.hash)
        setOrderId(result.orderId)
        const isFirstInclusion =
          includedCommitAttemptRef.current !== commitAttemptId
        if (!isFirstInclusion) {
          if (inclusionChanged) {
            handledTerminalOrderKeyRef.current = undefined
            handledTerminalBlockNumberRef.current = undefined
            handledTerminalBlockHashRef.current = undefined
            rejectedTerminalRef.current = undefined
            executionEvidencePollRef.current = undefined
            setExecuteTxHash(undefined)
            setFinalExecutionPrice(undefined)
            setFinalExecutionOraclePrice(undefined)
            setFinalExecutionOracleFrozen(undefined)
            setFinalExecutionFrozenCloseSpreadUsdc(undefined)
            setFinalExecutionEconomicsVersion(undefined)
            setFinalVpiUsdc(undefined)
            setKeeperRevealDeadlineMs(Date.now() + KEEPER_REVEAL_GRACE_MS)
            setKeeperRevealNowMs(Date.now())
            setLifecycleState('revealPending')
            void onAccountRefresh?.()
          }
          return
        }

        includedCommitAttemptRef.current = commitAttemptId
        setKeeperRevealDeadlineMs(Date.now() + KEEPER_REVEAL_GRACE_MS)
        setKeeperRevealNowMs(Date.now())
        setLifecycleState((currentState) => (
          currentState === 'commitPreparing' ||
          currentState === 'commitPending' ||
          currentState === 'commitConfirmed'
            ? 'revealPending'
            : currentState
        ))
        trackPerpsOrderLifecycle('commit_succeeded', commonAnalyticsProperties)
        void onAccountRefresh?.()
      }
      const result = await commitOrder({
        direction: effectiveOrderDirection,
        notionalUsdc: contractNotionalUsdc,
        sizeDelta,
        marginUsdc,
        oraclePrice: oraclePriceRaw ?? 0n,
        slippagePercent: slippageNumber,
        isClose: isReducingCurrentPosition,
        selectedMaxLeverageBps: Math.round(activeLeverage * 10_000),
        preparedOrder,
        onIncluded: (includedResult) => {
          debugPerpsCommit('ticket:commit-included', {
            hash: includedResult.hash,
            orderId: includedResult.orderId,
          })
          applyIncludedCommit(includedResult)
        },
        onStatus: (status) => {
          if (commitAttemptIdRef.current !== commitAttemptId) return
          debugPerpsCommit(`ticket:execution:${status}`)
          setCommitExecutionStatus(status)
          if (status === 'awaiting-signature') {
            trackPerpsOrderLifecycle('commit_pending', commonAnalyticsProperties)
            setLifecycleState('commitPending')
          } else {
            setWalletRequestWarning(undefined)
          }
        },
      })
      debugPerpsCommit('ticket:commit-result', {
        hash: result.hash,
        orderId: result.orderId,
      })
      applyIncludedCommit(result)
    } catch (error) {
      const inclusionWasReported = () =>
        includedCommitAttemptRef.current === commitAttemptId
      if (commitAttemptIdRef.current !== commitAttemptId) {
        debugPerpsCommit('ticket:commit-error-after-inclusion', {
          message: error instanceof Error ? error.message : String(error),
        })
        return
      }
      const includedIdentity = readMutableRef(includedCommitIdentityRef)
      const currentOrderHistory = readMutableRef(orderHistoryRef)
      const bundlerError = findBundlerRequestError(error)
      const hasIndexedOrderEvidence =
        includedIdentity !== undefined &&
        currentOrderHistory.some((row) =>
          orderMatchesCommittedIdentity(row, includedIdentity)
        )
      if (
        inclusionWasReported() &&
        bundlerError?.terminalStatus === 'receipt-timeout' &&
        hasIndexedOrderEvidence
      ) {
        deferredSafeConfirmationErrorRef.current = {
          ...includedIdentity,
          message: bundlerError.message,
        }
        debugPerpsCommit('ticket:safe-confirmation-timeout-after-indexing', {
          orderId: includedIdentity.orderId,
          message: error instanceof Error ? error.message : String(error),
        })
        return
      }
      if (inclusionWasReported()) {
        includedCommitAttemptRef.current = undefined
        includedCommitIdentityRef.current = undefined
        deferredSafeConfirmationErrorRef.current = undefined
        handledTerminalOrderKeyRef.current = undefined
        handledTerminalBlockNumberRef.current = undefined
        handledTerminalBlockHashRef.current = undefined
        rejectedTerminalRef.current = undefined
        executionEvidencePollRef.current = undefined
        setOrderId(undefined)
        setCommitTxHash(undefined)
        setExecuteTxHash(undefined)
        setFinalExecutionPrice(undefined)
        setFinalExecutionOraclePrice(undefined)
        setFinalExecutionOracleFrozen(undefined)
        setFinalExecutionFrozenCloseSpreadUsdc(undefined)
        setFinalExecutionEconomicsVersion(undefined)
        setFinalVpiUsdc(undefined)
      }
      debugPerpsCommit('ticket:commit-error', {
        message: error instanceof Error ? error.message : String(error),
      })
      setFlowError(error instanceof Error ? error.message : 'Commit transaction failed')
      setLifecycleState('failed')
      trackPerpsOrderLifecycle('commit_failed', {
        ...commonAnalyticsProperties,
        error_category: perpsErrorCategory(error),
      })
    }
  }

  async function handleCleanupOldestOrder() {
    if (firstPendingOrderId === undefined) {
      setCleanupError('Missing pending order ID. Refresh account state and retry.')
      setCleanupStatus('failed')
      return
    }

    try {
      setCleanupError(undefined)
      setCleanupStatus('pending')
      await cleanupExpiredOrder(firstPendingOrderId)
      setCleanupStatus('idle')
      void onAccountRefresh?.()
    } catch (error) {
      setCleanupStatus('failed')
      setCleanupError(error instanceof Error ? error.message : 'Expired-order cleanup failed')
      void onAccountRefresh?.()
    }
  }

  async function handleSelfExecute() {
    if (!enableLiveTrading) {
      trackPerpsOrderLifecycle('reveal_started', commonAnalyticsProperties)
      setLifecycleState('selfExecutePending')
      return
    }
    if (orderId === undefined) {
      setFlowError('Missing order ID from commit transaction.')
      setLifecycleState('selfExecuteFailed')
      trackPerpsOrderLifecycle('reveal_failed', {
        ...commonAnalyticsProperties,
        error_category: 'missing_order',
      })
      return
    }

    try {
      setFlowError(undefined)
      trackPerpsOrderLifecycle('reveal_started', commonAnalyticsProperties)
      setLifecycleState('selfExecutePending')
      const result = await executeOrder(orderId)
      setExecuteTxHash(result.hash)
      trackPerpsOrderLifecycle('reveal_succeeded', commonAnalyticsProperties)
    } catch (error) {
      const message = error instanceof Error ? error.message : 'Self-execute transaction failed'
      setFlowError(message)
      setLifecycleState(isRetryableSelfExecuteMessage(message) ? 'selfExecuteAvailable' : 'selfExecuteFailed')
      trackPerpsOrderLifecycle('reveal_failed', {
        ...commonAnalyticsProperties,
        error_category: perpsErrorCategory(error),
      })
      void onAccountRefresh?.()
    }
  }

  const resetReviewLifecycle = useCallback(() => {
    commitAttemptIdRef.current += 1
    includedCommitAttemptRef.current = undefined
    includedCommitIdentityRef.current = undefined
    deferredSafeConfirmationErrorRef.current = undefined
    handledTerminalOrderKeyRef.current = undefined
    handledTerminalBlockNumberRef.current = undefined
    handledTerminalBlockHashRef.current = undefined
    rejectedTerminalRef.current = undefined
    executionEvidencePollRef.current = undefined
    setLifecycleState('preview')
    setOrderId(undefined)
    setCommitTxHash(undefined)
    setExecuteTxHash(undefined)
    setFinalExecutionPrice(undefined)
    setFinalExecutionOraclePrice(undefined)
    setFinalExecutionOracleFrozen(undefined)
    setFinalExecutionFrozenCloseSpreadUsdc(undefined)
    setFinalExecutionEconomicsVersion(undefined)
    setFinalVpiUsdc(undefined)
    setCommittedSizeDelta(undefined)
    setCommittedSlippage(undefined)
    setCommittedTargetPrice(undefined)
    setCommittedIsClose(undefined)
    setCommittedIsFullClose(undefined)
    setCommittedVpiUsdc(undefined)
    setCommittedPositionVpiAccrued(undefined)
    setCommittedShowsPositionVpiBalance(false)
    setPreparedOrder(undefined)
    setIsExecutionProtectionsLoading(false)
    setExecutionProtectionsError(undefined)
    setFlowError(undefined)
    setCommitExecutionStatus(undefined)
    setWalletRequestWarning(undefined)
    setKeeperRevealDeadlineMs(undefined)
    setKeeperRevealNowMs(Date.now())
  }, [])

  function closeReviewModal() {
    const shouldResetSize = lifecycleState === 'executed'
    resetReviewLifecycle()
    if (shouldResetSize) {
      setSize('0')
      setIsFullCloseIntent(false)
    }
    setIsReviewOpen(false)
  }

  useEffect(() => {
    if (
      closePositionRequestId === undefined ||
      closePositionRequestId <= 0 ||
      handledClosePositionRequestRef.current === closePositionRequestId
    ) {
      return
    }

    handledClosePositionRequestRef.current = closePositionRequestId
    resetReviewLifecycle()
    setDirection(currentPosition?.direction ?? currentPositionSide)
    setIsReduceOnly(true)
    setIsFullCloseIntent(true)
    setSize(formatPerpsUsdc(availableCloseDxyExposureRaw, 6))
    setIsReviewOpen(true)
  }, [
    availableCloseDxyExposureRaw,
    closePositionRequestId,
    currentPosition?.direction,
    currentPositionSide,
    resetReviewLifecycle,
  ])

  function commitLeverageInput() {
    const parsedLeverage = Number(leverageInputValue)
    const nextLeverage = Number.isFinite(parsedLeverage)
      ? Math.min(Math.max(Math.round(parsedLeverage), 1), maxLeverage)
      : activeLeverage

    setLeverage(nextLeverage)
    setLeverageInputValue(nextLeverage.toString())
    trackPerpsButtonClicked('leverage_input_changed', commonAnalyticsProperties)
  }

  return (
    <section className="bg-surface-panel border border-brand-border/30 overflow-visible">
      <div className="space-y-5 px-3 py-3 sm:px-5 sm:py-4">
        <div>
          <div className="mb-2 flex items-center gap-1.5 text-xs font-medium uppercase text-content-secondary">
            <span>Direction</span>
            <InfoTooltip
              ariaLabel="Direction info"
              content={DIRECTION_TOOLTIP}
              docsLink={DOCS_LINKS.direction}
            />
          </div>
          <div className="grid grid-cols-2 border border-brand-border/30 bg-app-bg">
            {(['long', 'short'] as Direction[]).map((item) => (
              <button
                key={item}
                type="button"
                className={`border px-3 py-3 text-sm font-semibold transition-colors hover:underline hover:underline-offset-4 focus-visible:underline focus-visible:underline-offset-4 ${
                  direction === item
                    ? item === 'long'
                      ? 'border-positive bg-positive text-app-bg'
                      : 'border-brand-orange bg-brand-orange text-app-bg'
                    : 'border-transparent text-content-primary hover:bg-[#3B212D]'
                }`}
                onClick={() => {
                  trackPerpsButtonClicked(`direction_${item}`, commonAnalyticsProperties)
                  setIsFullCloseIntent(false)
                  setDirection(item)
                }}
              >
                {directionLabel(item)}
              </button>
            ))}
          </div>
        </div>

        <div className="grid gap-2">
          <AccountContextRow
            label="Available to Trade"
            value={<TokenAmount amount={availableToTradeDisplayAmount} />}
            disabled={!canUseAvailableToTrade}
            onClick={() => {
              if (canUseAvailableToTrade) {
                trackPerpsButtonClicked('fill_available_to_trade', commonAnalyticsProperties)
                setIsFullCloseIntent(false)
                setSize(availableToTradeDisplayAmount)
              }
            }}
          />
          <AccountContextRow
            label="Current Position"
            value={<TokenAmount amount={currentPositionDisplayAmount} />}
            disabled={!canUseCurrentPosition}
            onClick={() => {
              if (canUseCurrentPosition) {
                trackPerpsButtonClicked('fill_current_position', commonAnalyticsProperties)
                setIsFullCloseIntent(isReducingCurrentPosition)
                setSize(currentPositionFillAmount)
              }
            }}
          />
        </div>

        <div>
          <Input
            label="plDXY Perp exposure"
            value={sizeInputValue}
            onChange={(event) => {
              if (isNumericInput(event.target.value)) {
                setIsFullCloseIntent(false)
                setSize(event.target.value)
              }
            }}
            rightElement={<TokenLabel token="USDC" />}
          />
          <div className="mt-1.5 flex justify-end">
            <button
              type="button"
              className="group cursor-pointer text-right text-xs font-semibold text-content-secondary transition-colors hover:text-content-primary disabled:cursor-not-allowed disabled:opacity-50 disabled:hover:text-content-secondary focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-[#FFAB96]"
              disabled={!canUseMaxNotional}
              onClick={() => {
                if (canUseMaxNotional) {
                  trackPerpsButtonClicked('fill_max_exposure', commonAnalyticsProperties)
                  setIsFullCloseIntent(isReducingCurrentPosition)
                  setSize(maxDxyExposureInputAmount)
                }
              }}
            >
              <span>Max: </span>
              <span className="group-hover:underline group-focus-visible:underline">
                <TokenAmount amount={maxDxyExposureDisplayAmount} />
              </span>
            </button>
          </div>
        </div>

        <div className="space-y-2">
          <div className="flex items-center gap-3 py-0.5 text-content-primary">
            <input
              id="perps-reduce-only"
              type="checkbox"
              checked={isReduceOnly}
              onChange={(event) => {
                trackPerpsButtonClicked('toggle_reduce_only', {
                  ...commonAnalyticsProperties,
                  reduce_only: event.target.checked,
                })
                setIsFullCloseIntent(false)
                setIsReduceOnly(event.target.checked)
              }}
              className="h-4 w-4 accent-[#FFAB96]"
            />
            <span className="inline-flex items-center gap-1.5">
              <label
                className="cursor-pointer text-sm font-semibold transition-colors hover:text-[#FFAB96]"
                htmlFor="perps-reduce-only"
              >
                Reduce only
              </label>
              <Tooltip
                content="Only reduces or closes your current position. It will not open a new position or increase exposure."
                position="top"
                className={INFO_TOOLTIP_PANEL_CLASS_NAME}
                docsLink={DOCS_LINKS.reduceOnly}
              >
                <span
                  aria-label="Reduce only info"
                  className="inline-flex h-3.5 w-3.5 shrink-0 cursor-help items-center justify-center rounded-full border border-current text-[9px] font-semibold leading-none text-content-secondary/80 transition-colors hover:text-[#FFAB96]"
                  tabIndex={0}
                >
                  i
                </span>
              </Tooltip>
            </span>
          </div>

          <div className="flex items-center gap-3 py-0.5 text-content-primary">
            <input
              id="perps-margin-call-simulator"
              type="checkbox"
              checked={isMarginCallSimulatorEnabled}
              onChange={(event) => {
                trackPerpsButtonClicked('toggle_margin_call_simulator', commonAnalyticsProperties)
                if (event.target.checked) {
                  setIsMarginCallSimulatorConfirmationOpen(true)
                } else {
                  setIsMarginCallSimulatorEnabled(false)
                  setIsMarginCallSimulatorConfirmationOpen(false)
                }
              }}
              className="h-4 w-4 accent-[#FFAB96]"
            />
            <span className="inline-flex items-center gap-1.5">
              <label
                className="cursor-pointer text-sm font-semibold transition-colors hover:text-[#FFAB96]"
                htmlFor="perps-margin-call-simulator"
              >
                Margin Call Simulator
              </label>
              <Tooltip
                content="Maximum leverage mode"
                position="top"
                className={INFO_TOOLTIP_PANEL_CLASS_NAME}
                docsLink={DOCS_LINKS.marginCallSimulator}
              >
                <span
                  aria-label="Margin Call Simulator info"
                  className="inline-flex h-3.5 w-3.5 shrink-0 cursor-help items-center justify-center rounded-full border border-current text-[9px] font-semibold leading-none text-content-secondary/80 transition-colors hover:text-[#FFAB96]"
                  tabIndex={0}
                >
                  i
                </span>
              </Tooltip>
            </span>
          </div>
        </div>

        <div>
          <div className="mb-2 flex items-center justify-between gap-3">
            <label className="text-sm font-medium text-content-secondary" htmlFor="perps-leverage-input">
              Leverage
            </label>
            <div className="relative">
              <input
                id="perps-leverage-input"
                type="number"
                inputMode="numeric"
                min="1"
                max={maxLeverage}
                step="1"
                value={leverageInputValue}
                onChange={(event) => {
                  const nextValue = event.target.value
                  setLeverageInputValue(nextValue)

                  const nextLeverage = Number(nextValue)
                  if (Number.isInteger(nextLeverage) && nextLeverage >= 1 && nextLeverage <= maxLeverage) {
                    setLeverage(nextLeverage)
                  }
                }}
                onBlur={commitLeverageInput}
                onKeyDown={(event) => {
                  if (event.key === 'Enter') {
                    event.currentTarget.blur()
                  }
                }}
                className="w-20 border border-brand-border/30 bg-app-bg py-1 pl-2 pr-7 text-right text-lg font-semibold text-[#FFAB96] [appearance:textfield] outline-none transition-colors focus:border-[#FFAB96] [&::-webkit-inner-spin-button]:appearance-none [&::-webkit-outer-spin-button]:appearance-none"
              />
              <span
                aria-hidden="true"
                className="pointer-events-none absolute right-2 top-1/2 -translate-y-1/2 text-lg font-semibold text-[#FFAB96]"
              >
                x
              </span>
            </div>
          </div>
          <input
            id="perps-leverage-slider"
            aria-label="Leverage slider"
            type="range"
            min="1"
            max={maxLeverage}
            step="1"
            value={activeLeverage}
            onChange={(event) => {
              const nextLeverage = Math.min(Number(event.target.value), maxLeverage)
              setLeverage(nextLeverage)
              setLeverageInputValue(nextLeverage.toString())
            }}
            onPointerUp={() => {
              trackPerpsButtonClicked('leverage_slider_changed', commonAnalyticsProperties)
            }}
            onKeyUp={() => {
              trackPerpsButtonClicked('leverage_slider_changed', commonAnalyticsProperties)
            }}
            className="perps-leverage-slider h-2 w-full cursor-pointer appearance-none accent-[#FFAB96]"
          />
          <div className="mt-2 flex items-center justify-between text-xs font-semibold text-content-secondary">
            <span>1x</span>
            <span>{formatLeverage(maxLeverage)}</span>
          </div>
        </div>

        <div className="border border-brand-border/20 bg-app-bg p-3 sm:p-4">
          <div className="mb-3 text-xs font-medium uppercase text-content-secondary">Preview</div>
          <div id="perps-preview-details">
            <PreviewRows
              rows={visibleSidePanelPreviewRows}
              onSlippageClick={isOracleFrozenClose
                ? undefined
                : () => {
                    trackPerpsButtonClicked('toggle_slippage_config', commonAnalyticsProperties)
                    setIsSlippageConfigOpen((isOpen) => !isOpen)
                  }}
              slippageConfig={
                isSlippageConfigOpen && !isOracleFrozenClose ? (
                  <div className="mt-3 py-3">
                    <div className="grid grid-cols-[repeat(auto-fit,minmax(4.5rem,1fr))] gap-2">
                      {slippageOptions.map((option) => (
                        <button
                          key={option.toString()}
                          type="button"
                          className={`border px-2 py-2 text-sm font-semibold transition-colors hover:underline hover:underline-offset-4 focus-visible:underline focus-visible:underline-offset-4 ${
                            slippage === option
                              ? 'border-[#FFAB96] bg-[#FFAB96] text-app-bg'
                              : 'border-brand-border/30 text-content-secondary hover:bg-[#3B212D] hover:text-content-primary'
                          }`}
                          onClick={() => {
                            trackPerpsButtonClicked('select_slippage_preset', commonAnalyticsProperties)
                            setSlippage(option)
                          }}
                        >
                          {formatPercent(option)}
                        </button>
                      ))}
                    </div>
                  </div>
                ) : null
              }
            />
          </div>
          <button
            type="button"
            aria-expanded={isPreviewExpanded}
            aria-controls="perps-preview-details"
            className="mx-auto mt-3 block cursor-pointer text-sm font-semibold text-[#FFAB96] transition-colors hover:text-content-primary hover:underline hover:underline-offset-4 focus-visible:underline focus-visible:underline-offset-4"
            onClick={() => {
              trackPerpsButtonClicked(
                isPreviewExpanded ? 'hide_trade_preview_details' : 'show_trade_preview_details',
                commonAnalyticsProperties
              )
              setIsPreviewExpanded((isExpanded) => !isExpanded)
            }}
          >
            {isPreviewExpanded ? 'Show less' : 'Show more...'}
          </button>
        </div>

        {displayedValidationError &&
        !isZeroSize &&
        (!enableLiveTrading || (isConnected && isCorrectChain)) ? (
          <div className="border border-brand-orange/30 bg-brand-orange/10 p-3 text-sm text-brand-orange">
            {displayedValidationError}
          </div>
        ) : null}

        <Button
          className={`w-full ${
            isConnectWalletCta
              ? CONNECT_WALLET_ACTION_BUTTON_CLASS
              : isSwitchNetworkCta
                ? LIGHT_ORANGE_ACTION_BUTTON_CLASS
                : ''
          }`}
          size="lg"
          variant={isConnectWalletCta || isSwitchNetworkCta ? 'secondary' : direction === 'short' ? 'danger' : 'primary'}
          disabled={isReviewButtonDisabled}
          title={isReviewButtonDisabled ? displayedValidationError : undefined}
          analyticsId={isConnectWalletCta ? 'connect_wallet_cta' : isSwitchNetworkCta ? 'switch_network_cta' : 'review_trade'}
          analyticsProperties={commonAnalyticsProperties}
          onClick={() => {
            if (enableLiveTrading && !isConnected) {
              void openAppKit()
              return
            }
            if (enableLiveTrading && !isCorrectChain) {
              void switchToArbitrumSepolia()
              return
            }
            if (liveValidationError) {
              trackPerpsValidationBlocked(validationReasonCategory(liveValidationError), commonAnalyticsProperties)
              setFlowError(liveValidationError)
              return
            }
            setIsReviewOpen(true)
          }}
        >
          {isConnectWalletCta ? <span className="material-symbols-outlined text-xl">account_balance_wallet</span> : null}
          {reviewCtaLabel}
        </Button>
        {isSwitchNetworkCta && networkSwitchError ? (
          <div className="text-xs leading-4 text-[#FFAB96]">
            {networkSwitchError}
          </div>
        ) : null}

        <div className="border border-brand-border/20 bg-app-bg p-3 sm:p-4">
          <div className="mb-3 text-xs font-medium uppercase text-content-secondary">Margin Account</div>
          <div className="space-y-2">
            <AccountSummaryRow label="Portfolio value" value={<TokenAmount amount={formatPerpsUsdc(portfolioValueRaw)} />} />
            <AccountSummaryRow
              label="Unrealized PnL"
              value={<TokenAmount amount={formatSignedPerpsUsdc(unrealizedPnlRaw)} />}
              tone={accountSummaryPnlTone}
            />
            <AccountSummaryRow
              label="Maintenance margin"
              value={<TokenAmount amount={formatPerpsUsdc(currentPosition?.maintenanceMarginUsdc)} />}
            />
            <AccountSummaryRow
              label="Withdrawable"
              value={<TokenAmount amount={formatPerpsUsdc(withdrawableUsdcRaw)} />}
              tooltip={
                <span>
                  Amount that can leave the protocol right now. It can be lower than available to trade because withdrawals
                  require a fresh mark and must pass protocol state, pending carry, and post-withdraw margin checks.
                </span>
              }
              tooltipDocsLink={DOCS_LINKS.withdrawable}
            />
          </div>
        </div>

        <div className="grid grid-cols-1 gap-3 sm:grid-cols-2">
          <Button
            type="button"
            variant="secondary"
            className={`w-full ${LIGHT_ORANGE_ACTION_BUTTON_CLASS}`}
            disabled={areMarginActionsDisabled}
            title={areMarginActionsDisabled ? 'Connect wallet to deposit margin' : undefined}
            analyticsId="open_deposit_margin"
            analyticsProperties={commonAnalyticsProperties}
            onClick={() => {
              if (areMarginActionsDisabled) return
              openMarginAction('deposit')
            }}
          >
            Deposit
          </Button>
          <Button
            type="button"
            variant="secondary"
            className={`w-full ${LIGHT_ORANGE_ACTION_BUTTON_CLASS}`}
            disabled={areMarginActionsDisabled}
            title={areMarginActionsDisabled ? 'Connect wallet to withdraw margin' : undefined}
            analyticsId="open_withdraw_margin"
            analyticsProperties={commonAnalyticsProperties}
            onClick={() => {
              if (areMarginActionsDisabled) return
              openMarginAction('withdraw')
            }}
          >
            Withdraw
          </Button>
        </div>
      </div>

      <Modal
        isOpen={isReviewOpen}
        onClose={closeReviewModal}
        headerContent={<OrderLifecycleSteps currentStep={currentLifecycleStep} />}
        showCloseButton={false}
        size="lg"
        analyticsId="trade_review"
        analyticsProperties={commonAnalyticsProperties}
      >
        <div className="space-y-5">
          {lifecycleState === 'preview' ? (
            <>
              <p className="px-1 py-2 text-xl font-semibold leading-7 text-content-primary">
                {orderSummary}
              </p>

              <div className="border border-brand-border/20 bg-app-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-content-secondary">Commit Preview</div>
                <PreviewRows rows={previewRows} />
                <p className="mt-4 border-t border-brand-border/20 pt-3 text-sm leading-5 text-content-secondary">
                  plDXY Perp exposure is the size you choose. Contract notional is derived from the raw basket price for protocol accounting.
                </p>
              </div>

              {shouldShowExecutionProtections ? (
                <details className="border border-brand-border/20 bg-app-bg p-4">
                  <summary className="cursor-pointer text-sm font-semibold text-content-primary">
                    Execution protections
                  </summary>
                  {enableLiveTrading && isExecutionProtectionsLoading ? (
                    <p className="mt-3 text-sm text-content-secondary">
                      Deriving protections from one coherent block…
                    </p>
                  ) : enableLiveTrading && executionProtectionsError ? (
                    <p className="mt-3 text-sm text-brand-orange">
                      {executionProtectionsError}
                    </p>
                  ) : displayedExecutionProtections ? (
                    <div className="mt-3">
                      <PreviewRows rows={[
                        {
                          label: 'Client order ID',
                          value: `${displayedExecutionProtections.request.clientOrderId.slice(0, 10)}…${displayedExecutionProtections.request.clientOrderId.slice(-8)}`,
                        },
                        {
                          label: 'Deadline',
                          value: new Date(
                            Number(displayedExecutionProtections.protection.validUntil) * 1_000
                          ).toLocaleString(),
                        },
                        {
                          label: 'Pinned regime',
                          value: PERPS_EXECUTION_MODE_LABELS[
                            displayedExecutionProtections.protection.executionMode
                          ],
                        },
                        {
                          label: 'Maximum account debit',
                          value: `${formatPerpsUsdc(
                            displayedExecutionProtections.protection.maxGrossAccountDebitUsdc
                          )} USDC`,
                        },
                        {
                          label: 'Maximum action charge',
                          value: `${formatPerpsUsdc(
                            displayedExecutionProtections.protection.maxActionChargeUsdc
                          )} USDC`,
                        },
                        {
                          label: 'Maximum explicit fees',
                          value: `${formatPerpsUsdc(
                            displayedExecutionProtections.protection.maxExplicitFeesUsdc
                          )} USDC`,
                        },
                        {
                          label: 'Maximum leverage',
                          value: `${(
                            displayedExecutionProtections.protection.maxPostLeverageBps / 10_000
                          ).toFixed(2)}x`,
                        },
                        {
                          label: 'Minimum settlement balance',
                          value: `${formatPerpsUsdc(
                            displayedExecutionProtections.protection.minPostSettlementBalanceUsdc
                          )} USDC`,
                        },
                        {
                          label: 'Minimum position equity',
                          value: `${formatPerpsUsdc(
                            displayedExecutionProtections.protection.minPostPositionEquityUsdc
                          )} USDC`,
                        },
                      ]} />
                    </div>
                  ) : (
                    <p className="mt-3 text-sm text-content-secondary">
                      Protections will appear after the final review is ready.
                    </p>
                  )}
                </details>
              ) : null}

              <div className="border border-brand-border/20 bg-app-bg p-4">
                <div className="text-sm font-semibold text-content-primary">Delayed execution</div>
                <div className="mt-2 text-sm text-content-secondary">
                  This submits your order. Final execution settles shortly after with your accepted price constraints.
                </div>
              </div>

              {enableLiveTrading && liveValidationError ? (
                <div className="border border-brand-orange/30 bg-brand-orange/10 p-4 text-sm text-brand-orange">
                  {liveValidationError}
                  {!isCorrectChain ? (
                    <>
                      <Button
                        className={`mt-3 w-full ${LIGHT_ORANGE_ACTION_BUTTON_CLASS}`}
                        size="sm"
                        variant="secondary"
                        analyticsId="review_switch_network"
                        analyticsProperties={commonAnalyticsProperties}
                        onClick={() => {
                          void switchToArbitrumSepolia()
                        }}
                      >
                        Switch Network
                      </Button>
                      {networkSwitchError ? (
                        <div className="mt-3 text-xs leading-4 text-content-primary">
                          {networkSwitchError}
                        </div>
                      ) : null}
                    </>
                  ) : null}
                  {canCleanupOldestPendingOrder ? (
                    <Button
                      className={`mt-3 w-full ${LIGHT_ORANGE_ACTION_BUTTON_CLASS}`}
                      size="sm"
                      variant="secondary"
                      disabled={cleanupStatus === 'pending'}
                      analyticsId="cleanup_oldest_order"
                      analyticsProperties={commonAnalyticsProperties}
                      onClick={() => {
                        void handleCleanupOldestOrder()
                      }}
                    >
                      {cleanupStatus === 'pending' ? 'Cleaning Up...' : 'Clean Up Oldest Order'}
                    </Button>
                  ) : null}
                  {cleanupError ? (
                    <div className="mt-3 text-xs text-content-primary">
                      {cleanupError}
                    </div>
                  ) : null}
                </div>
              ) : null}
              {flowError ? (
                <div className="border border-brand-orange/30 bg-brand-orange/10 p-4 text-sm text-brand-orange">
                  {flowError}
                </div>
              ) : null}

              <div className="flex gap-3">
                <Button
                  className={`flex-1 ${DARK_CANCEL_BUTTON_CLASS}`}
                  variant="secondary"
                  analyticsId="cancel_review"
                  analyticsProperties={commonAnalyticsProperties}
                  onClick={closeReviewModal}
                >
                  Cancel
                </Button>
                <Button
                  className="flex-1"
                  variant={direction === 'short' ? 'danger' : 'primary'}
                  disabled={enableLiveTrading && (
                    Boolean(liveValidationError) ||
                    isExecutionProtectionsLoading ||
                    Boolean(executionProtectionsError) ||
                    preparedOrder === undefined
                  )}
                  analyticsId="confirm_commit"
                  analyticsProperties={commonAnalyticsProperties}
                  onClick={() => {
                    void handleConfirmCommit()
                  }}
                >
                  Confirm Commit
                </Button>
              </div>
            </>
          ) : null}

          {lifecycleState === 'commitPreparing' ? (
            <>
              <PendingStateCard
                title={isSponsoredAccountConfigured ? 'Preparing sponsored transaction' : 'Preparing wallet request'}
                description={
                  isSponsoredAccountConfigured
                    ? 'Plether is requesting sponsorship and estimating the final UserOperation before asking for your signature.'
                    : 'Checking gas and wallet network before opening your wallet. If this takes more than a few seconds, switch to Arbitrum Sepolia manually and try again.'
                }
              />

              <div className="border border-brand-border/20 bg-app-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-content-secondary">Commit Transaction</div>
                <PreviewRows
                  rows={[
                    { label: 'Direction', value: directionLabel(direction) },
                    { label: 'plDXY Perp exposure', value: formatUsdc(dxyExposureNumber) },
                    { label: 'Contract notional', value: formatUsdcRaw(contractNotionalUsdc) },
                    { label: 'Max slippage', value: formatPercent(committedSlippageNumber) },
                    { label: 'Execution limit', value: formatOptionalPrice(committedExecutionLimit) },
                    { label: 'Estimated protocol execution fee', value: formatUsdcRaw(protocolExecutionFeeRaw) },
                    ...committedVpiRows,
                    { label: 'Estimated execution reward', value: formatUsdc(keeperBounty) },
                  ]}
                />
              </div>
            </>
          ) : null}

          {lifecycleState === 'commitPending' ? (
            <>
              <PendingStateCard
                title={commitPendingTitle}
                description={commitPendingDescription}
              />

              {walletRequestWarning ? (
                <div className="border border-[#FFAB96]/40 bg-[#FF572D]/10 p-4 text-sm leading-5 text-[#FFAB96]">
                  {walletRequestWarning}
                </div>
              ) : null}

              <div className="border border-brand-border/20 bg-app-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-content-secondary">Commit Transaction</div>
                <PreviewRows
                  rows={[
                    { label: 'Direction', value: directionLabel(direction) },
                    { label: 'plDXY Perp exposure', value: formatUsdc(dxyExposureNumber) },
                    { label: 'Contract notional', value: formatUsdcRaw(contractNotionalUsdc) },
                    { label: 'Max slippage', value: formatPercent(committedSlippageNumber) },
                    { label: 'Execution limit', value: formatOptionalPrice(committedExecutionLimit) },
                    { label: 'Estimated protocol execution fee', value: formatUsdcRaw(protocolExecutionFeeRaw) },
                    ...committedVpiRows,
                    { label: 'Estimated execution reward', value: formatUsdc(keeperBounty) },
                    ...(isSponsoredAccountConfigured
                      ? [{ label: 'UserOperation', value: displayUserOperationHashValue }]
                      : []),
                  ]}
                />
              </div>

              {!enableLiveTrading ? (
                <div className="grid grid-cols-1 gap-3 sm:grid-cols-2">
                  <Button
                    className={`w-full ${DARK_CANCEL_BUTTON_CLASS}`}
                    variant="secondary"
                    analyticsId="mock_commit_failed"
                    analyticsProperties={commonAnalyticsProperties}
                    onClick={() => {
                      setLifecycleState('failed')
                    }}
                  >
                    Transaction Failed
                  </Button>
                  <Button
                    className={`w-full ${LIGHT_ORANGE_ACTION_BUTTON_CLASS}`}
                    analyticsId="mock_commit_confirmed"
                    analyticsProperties={commonAnalyticsProperties}
                    onClick={() => {
                      setLifecycleState('revealPending')
                    }}
                  >
                    Transaction Confirmed
                  </Button>
                </div>
              ) : null}
            </>
          ) : null}

          {lifecycleState === 'commitConfirmed' ? (
            <>
              <SuccessStateCard title="Commit confirmed" description="The order is waiting for final price confirmation." />
              <PreviewRows
                rows={[
                  { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                  { label: 'Commit tx', value: displayCommitTxValue },
                ]}
              />
              <Button
                className={`w-full ${LIGHT_ORANGE_ACTION_BUTTON_CLASS}`}
                analyticsId="continue_to_finalize"
                analyticsProperties={commonAnalyticsProperties}
                onClick={() => {
                  setLifecycleState('revealPending')
                }}
              >
                Continue to Finalize
              </Button>
            </>
          ) : null}

          {lifecycleState === 'revealPending' ? (
            <>
              <PendingStateCard
                title={shouldShowFinalizationProgress ? finalizationLoadingMessage.title : 'Finalizing execution price'}
                progressPercent={shouldShowFinalizationProgress ? keeperRevealProgressPercent : undefined}
                showAnimatedDots={shouldShowFinalizationProgress}
                description={
                  shouldShowFinalizationProgress
                    ? finalizationLoadingDescription
                    : 'Your order is committed. The next step settles it onchain with the market price for the order window.'
                }
              />

              <div className="border border-brand-border/20 bg-app-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-content-secondary">Settlement Details</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                    { label: 'Commit tx', value: displayCommitTxValue },
                    ...(isSponsoredAccountConfigured
                      ? [{ label: 'UserOperation', value: displayUserOperationHashValue }]
                      : []),
                    { label: 'Acceptable price', value: formatOptionalPrice(committedExecutionLimit) },
                    { label: 'Estimated execution reward', value: formatUsdc(keeperBounty) },
                    {
                      label: isSponsoredAccountConfigured ? 'Keeper processing' : 'Manual finalization',
                      value: isSponsoredAccountConfigured
                        ? 'In progress'
                        : shouldShowFinalizationProgress
                          ? `Available in ${keeperRevealRemainingSeconds.toString()}s`
                          : 'Available after 04:38',
                      tone: shouldShowFinalizationProgress ? 'muted' : undefined,
                      tooltip: MANUAL_FINALIZATION_TOOLTIP,
                      tooltipDocsLink: DOCS_LINKS.manualFinalization,
                    },
                  ]}
                />
              </div>

              {isKeeperRevealGraceActive ? null : !enableLiveTrading ? (
                <div className="grid grid-cols-1 gap-3 sm:grid-cols-2">
                  <>
                    <Button
                      className={`w-full ${DARK_CANCEL_BUTTON_CLASS}`}
                      variant="secondary"
                      analyticsId="show_manual_finalize_option"
                      analyticsProperties={commonAnalyticsProperties}
                      onClick={() => {
                        setLifecycleState('selfExecuteAvailable')
                      }}
                    >
                      Show Manual Option
                    </Button>
                    <Button
                      className={`w-full ${LIGHT_ORANGE_ACTION_BUTTON_CLASS}`}
                      analyticsId="mock_auto_finalized"
                      analyticsProperties={commonAnalyticsProperties}
                      onClick={() => {
                        setLifecycleState('executed')
                      }}
                    >
                      Auto Finalized
                    </Button>
                  </>
                </div>
              ) : (
                <Button
                  className={`w-full ${LIGHT_ORANGE_ACTION_BUTTON_CLASS}`}
                  analyticsId="finalize_trade"
                  analyticsProperties={commonAnalyticsProperties}
                  onClick={() => {
                    void handleSelfExecute()
                  }}
                >
                  Finalize Trade
                </Button>
              )}
            </>
          ) : null}

          {lifecycleState === 'selfExecuteAvailable' ? (
            <>
              <PendingStateCard
                title={
                  flowError && isHermesRateLimitMessage(flowError)
                    ? 'Price data rate limited'
                    : flowError && isHistoricalPythRejectedMessage(flowError)
                      ? 'Historical price data rejected'
                    : flowError && isRevealNotReadyMessage(flowError)
                      ? 'Final price not ready yet'
                    : flowError && isPythExpiryMessage(flowError)
                      ? 'Historical price data required'
                      : 'Ready to finalize manually'
                }
                description={
                  flowError && isPythExpiryMessage(flowError)
                    ? flowError
                    : 'Automatic finalization has not completed yet. You can submit the finalization transaction now; the order status check will confirm the result.'
                }
              />

              <div className="border border-brand-border/20 bg-app-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-content-secondary">Settlement Details</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                    { label: 'Commit tx', value: displayCommitTxValue },
                    { label: 'Acceptable price', value: formatOptionalPrice(committedExecutionLimit) },
                    { label: 'Estimated execution reward', value: formatUsdc(keeperBounty) },
                    {
                      label: 'Manual finalization',
                      value: flowError && isRevealNotReadyMessage(flowError)
                        ? 'Retry shortly'
                        : flowError && isPythExpiryMessage(flowError)
                          ? 'Retry with price data'
                          : 'Available now',
                      tone: flowError && isRetryableSelfExecuteMessage(flowError) ? 'warning' : 'positive',
                      tooltip: MANUAL_FINALIZATION_TOOLTIP,
                      tooltipDocsLink: DOCS_LINKS.manualFinalization,
                    },
                  ]}
                />
              </div>

              <Button
                className={`w-full ${LIGHT_ORANGE_ACTION_BUTTON_CLASS}`}
                size="lg"
                analyticsId={flowError && isRetryableSelfExecuteMessage(flowError) ? 'retry_finalize_trade' : 'finalize_trade'}
                analyticsProperties={commonAnalyticsProperties}
                onClick={() => {
                  void handleSelfExecute()
                }}
              >
                {flowError && isRetryableSelfExecuteMessage(flowError) ? 'Retry Finalizing' : 'Finalize Trade'}
              </Button>
            </>
          ) : null}

          {lifecycleState === 'selfExecutePending' ? (
            <>
              <PendingStateCard
                title="Finalizing trade"
                description="Confirm the transaction in your wallet. We will show the result after the final price is confirmed onchain."
              />

              <div className="border border-brand-border/20 bg-app-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-content-secondary">Finalization Transaction</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                    { label: 'Commit tx', value: displayCommitTxValue },
                    { label: 'Acceptable price', value: formatOptionalPrice(committedExecutionLimit) },
                    { label: 'Estimated execution reward', value: formatUsdc(keeperBounty) },
                    { label: 'Transaction', value: 'Awaiting confirmation' },
                  ]}
                />
              </div>

              {!enableLiveTrading ? (
                <div className="grid grid-cols-1 gap-3 sm:grid-cols-2">
                  <Button
                    className={`w-full ${DARK_CANCEL_BUTTON_CLASS}`}
                    variant="secondary"
                    analyticsId="mock_finalize_failed"
                    analyticsProperties={commonAnalyticsProperties}
                    onClick={() => {
                      setLifecycleState('selfExecuteFailed')
                    }}
                  >
                    Transaction Failed
                  </Button>
                  <Button
                    className={`w-full ${LIGHT_ORANGE_ACTION_BUTTON_CLASS}`}
                    analyticsId="mock_finalize_confirmed"
                    analyticsProperties={commonAnalyticsProperties}
                    onClick={() => {
                      setLifecycleState('executed')
                    }}
                  >
                    Transaction Confirmed
                  </Button>
                </div>
              ) : null}
            </>
          ) : null}

          {lifecycleState === 'selfExecuteFailed' ? (
            <>
              <FailedStateCard
                title={
                  flowError && isOrderNoLongerPendingMessage(flowError)
                    ? 'Order no longer pending'
                    : flowError && isTerminalOrderFailureMessage(flowError)
                      ? 'Order failed'
                    : flowError && isHistoricalPythRejectedMessage(flowError)
                      ? 'Historical price data rejected'
                      : 'Finalization transaction failed'
                }
                description={flowError ?? 'The wallet rejected the transaction or the finalization transaction did not settle the order.'}
              />

              <div className="border border-brand-border/20 bg-app-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-content-secondary">Settlement Details</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                    { label: 'Commit tx', value: displayCommitTxValue },
                    { label: 'Acceptable price', value: formatOptionalPrice(committedExecutionLimit) },
                    {
                      label: 'Manual finalization',
                      value: isTerminalRevealError ? 'Unavailable' : 'Retry available',
                      tone: 'warning',
                      tooltip: MANUAL_FINALIZATION_TOOLTIP,
                      tooltipDocsLink: DOCS_LINKS.manualFinalization,
                    },
                  ]}
                />
              </div>

              {isTerminalRevealError ? (
                <Button
                  className={`w-full ${DARK_CANCEL_BUTTON_CLASS}`}
                  variant="secondary"
                  analyticsId="back_to_preview"
                  analyticsProperties={commonAnalyticsProperties}
                  onClick={() => {
                    setLifecycleState('preview')
                  }}
                >
                  Back to Preview
                </Button>
              ) : (
                <div className="flex gap-3">
                  <Button
                    className={`flex-1 ${DARK_CANCEL_BUTTON_CLASS}`}
                    variant="secondary"
                    analyticsId="back_to_finalize"
                    analyticsProperties={commonAnalyticsProperties}
                    onClick={() => {
                      setLifecycleState('selfExecuteAvailable')
                    }}
                  >
                    Back
                  </Button>
                  <Button
                    className={`flex-1 ${LIGHT_ORANGE_ACTION_BUTTON_CLASS}`}
                    analyticsId="retry_finalize_trade"
                    analyticsProperties={commonAnalyticsProperties}
                    onClick={() => {
                      void handleSelfExecute()
                    }}
                  >
                    Retry Finalizing
                  </Button>
                </div>
              )}
            </>
          ) : null}

          {lifecycleState === 'executed' ? (
            <>
              <SuccessStateCard
                title={executedTitle}
                description="Execution settled onchain and the final price is confirmed."
                celebrate
              />
              <div className="border border-brand-border/20 bg-app-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-content-secondary">Final Result</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                    {
                      label: finalIsClose ? 'Position side' : 'Direction',
                      value: finalIsClose
                        ? directionLabel(oppositeDirection(direction))
                        : directionLabel(direction),
                    },
                    { label: 'Final price', value: finalPriceDisplay },
                    {
                      label: finalIsFullClose
                        ? 'Requested close exposure'
                        : finalIsClose
                          ? 'Requested reduction exposure'
                          : 'Target plDXY Perp exposure',
                      value: formatUsdc(dxyExposureNumber),
                    },
                    {
                      label: finalIsFullClose
                        ? 'Executed close exposure'
                        : finalIsClose
                          ? 'Executed reduction exposure'
                          : 'Execution plDXY Perp exposure',
                      value: finalExecutedDxyExposureUsdc === undefined
                        ? formatUsdc(dxyExposureNumber)
                        : formatUsdcRaw(finalExecutedDxyExposureUsdc),
                    },
                    { label: 'Contract notional', value: finalExecutedNotionalUsdc === undefined ? formatUsdcRaw(contractNotionalUsdc) : formatUsdcRaw(finalExecutedNotionalUsdc) },
                    ...(!finalIsClose
                      ? [{ label: 'Margin posted', value: formatUsdc(marginNumber) }]
                      : []),
                    { label: 'Protocol execution fee', value: formatUsdcRaw(finalProtocolExecutionFee) },
                    finalUsesFrozenCloseSpread
                      ? {
                          label: 'Frozen close spread',
                          value: finalFrozenCloseSpreadValue,
                          tooltip: FROZEN_CLOSE_SPREAD_TOOLTIP,
                          tooltipDocsLink: DOCS_LINKS.frozenCloseSpread,
                        }
                      : {
                          label: 'Oracle confidence spread',
                          value: finalOracleConfidenceSpreadValue,
                          tooltip: ORACLE_CONFIDENCE_SPREAD_TOOLTIP,
                          tooltipDocsLink: DOCS_LINKS.oracleConfidence,
                        },
                    ...(finalIsClose && committedShowsPositionVpiBalance
                      ? [{
                          label: finalIsFullClose
                            ? 'Position VPI before close'
                            : 'Position VPI before reduction',
                          ...committedPositionVpiBalance,
                          tooltip: FINAL_POSITION_VPI_BALANCE_TOOLTIP,
                          tooltipDocsLink: DOCS_LINKS.virtualPriceImpact,
                        }]
                      : []),
                    {
                      label: 'VPI',
                      value: finalVpiValue,
                      tone: finalIsClose && finalExecutionEconomicsComplete
                        ? finalVpiAction.tone
                        : undefined,
                      tooltip: finalIsClose ? FINAL_CLOSE_VPI_TOOLTIP : VPI_PRICE_IMPACT_TOOLTIP,
                      tooltipDocsLink: DOCS_LINKS.virtualPriceImpact,
                    },
                    { label: 'Execution reward', value: formatUsdc(keeperBounty) },
                    { label: 'Commit tx', value: displayCommitTxValue },
                    { label: 'Reveal tx', value: displayExecuteTxValue },
                  ]}
                />
                <p className="mt-4 border-t border-brand-border/20 pt-3 text-sm leading-5 text-content-secondary">
                  {finalIsFullClose
                    ? 'Requested close exposure is the position exposure submitted for closure. Executed close exposure is the committed size valued with the displayed plDXY Perp price at finalization.'
                    : finalIsClose
                      ? 'Requested reduction exposure is what you submitted. Executed reduction exposure is the committed size valued with the displayed plDXY Perp price at finalization.'
                      : 'Target plDXY Perp exposure is what you submitted. Execution plDXY Perp exposure is the committed size valued with the displayed plDXY Perp price at finalization.'}
                </p>
              </div>
              <Button
                className={`w-full ${LIGHT_ORANGE_ACTION_BUTTON_CLASS}`}
                variant="secondary"
                analyticsId="done_trade"
                analyticsProperties={commonAnalyticsProperties}
                onClick={closeReviewModal}
              >
                Done
              </Button>
            </>
          ) : null}

          {lifecycleState === 'failed' ? (
            <>
              <FailedStateCard
                title="Commit transaction failed"
                description={flowError ?? 'The wallet rejected the transaction or the commit failed before the order could wait for finalization.'}
              />
              <div className="flex gap-3">
                <Button
                  className={`flex-1 ${DARK_CANCEL_BUTTON_CLASS}`}
                  variant="secondary"
                  analyticsId="back_to_preview"
                  analyticsProperties={commonAnalyticsProperties}
                  onClick={() => {
                    setLifecycleState('preview')
                  }}
                >
                  Back to Preview
                </Button>
                <Button
                  className={`flex-1 ${LIGHT_ORANGE_ACTION_BUTTON_CLASS}`}
                  analyticsId="retry_commit"
                  analyticsProperties={commonAnalyticsProperties}
                  onClick={() => {
                    void handleConfirmCommit()
                  }}
                >
                  Retry Commit
                </Button>
              </div>
            </>
          ) : null}
        </div>
      </Modal>

      <Modal
        isOpen={isMarginCallSimulatorConfirmationOpen}
        onClose={() => {
          setIsMarginCallSimulatorConfirmationOpen(false)
        }}
        title="Enable Margin Call Simulator?"
        size="lg"
        analyticsId="margin_call_simulator"
        analyticsProperties={commonAnalyticsProperties}
      >
        <div className="space-y-5">
          <div className="border border-[#FFAB96]/40 bg-[#250917] p-4">
            <p className="text-sm leading-6 text-content-secondary">
              This mode removes the normal {formatLeverage(DEFAULT_MAX_LEVERAGE)} UI cap and lets the leverage control
              reach the protocol maintenance-margin boundary. It is useful for testing margin-call behavior, but a position
              opened near this cap can become invalid or liquidatable from a tiny adverse move, VPI, execution fees,
              execution rewards, or carry.
            </p>
            <p className="mt-3 text-sm leading-6 text-[#FFAB96]">
              The current maintenance margin can be temporary.
              {marketPhase === 'open' && marketCurrentDuration ? (
                <> Market is open for another <span className="font-semibold text-content-primary">{marketCurrentDuration}</span>.</>
              ) : null}
              {' '}
              When the market closes, this setting may expire or become stricter, so add margin or reduce the position
              before that time if you keep a simulator-level position open.
            </p>
          </div>

          <div className="border border-brand-border/20 bg-app-bg p-4">
            <div className="mb-3 text-xs font-medium uppercase text-content-secondary">Leverage rule</div>
            <div className="space-y-2">
              <AccountSummaryRow label="Normal max leverage" value={formatLeverage(DEFAULT_MAX_LEVERAGE)} />
              <AccountSummaryRow label="Simulator max leverage" value={formatLeverage(simulatorMaxLeverage)} />
              <AccountSummaryRow label="Maintenance margin" value={formatBpsPercent(maintenanceMarginBps)} />
              <AccountSummaryRow
                label="Simulator max formula"
                value={maintenanceMarginBps === undefined ? 'Unavailable' : `floor(10 000 / ${maintenanceMarginBps.toString()})`}
              />
            </div>
          </div>

          <div className="border border-brand-border/20 bg-app-bg p-4">
            <div className="mb-3 text-xs font-medium uppercase text-content-secondary">Current order math</div>
            <div className="space-y-2">
              <AccountSummaryRow label="Selected leverage" value={formatLeverage(activeLeverage)} />
              <AccountSummaryRow label="plDXY Perp exposure" value={formatUsdcRaw(effectiveDxyExposureUsdc)} />
              <AccountSummaryRow label="Contract notional" value={formatUsdcRaw(contractNotionalUsdc)} />
              <AccountSummaryRow label="Position margin at selected leverage" value={formatUsdcRaw(marginUsdc)} />
              <AccountSummaryRow label={`Position margin at ${formatLeverage(DEFAULT_MAX_LEVERAGE)}`} value={formatUsdcRaw(defaultMaxLeverageMarginUsdc)} />
              <AccountSummaryRow label={`Position margin at ${formatLeverage(simulatorMaxLeverage)}`} value={formatUsdcRaw(simulatorMaxLeverageMarginUsdc)} />
              <AccountSummaryRow
                label="Estimated maintenance margin"
                value={estimatedMaintenanceMarginUsdc === undefined ? PREVIEW_UNAVAILABLE_VALUE : formatUsdcRaw(estimatedMaintenanceMarginUsdc)}
              />
            </div>
          </div>

          {!canEnableMarginCallSimulator ? (
            <div className="border border-[#FFAB96]/40 bg-[#FF572D]/10 p-3 text-sm leading-5 text-[#FFAB96]">
              The simulator cannot unlock additional leverage because the maintenance-margin setting is unavailable or already
              implies a cap at or below {formatLeverage(DEFAULT_MAX_LEVERAGE)}.
            </div>
          ) : null}

          <div className="grid grid-cols-1 gap-3 sm:grid-cols-2">
            <Button
              type="button"
              variant="secondary"
              className={DARK_CANCEL_BUTTON_CLASS}
              analyticsId="cancel_margin_call_simulator"
              analyticsProperties={commonAnalyticsProperties}
              onClick={() => {
                setIsMarginCallSimulatorConfirmationOpen(false)
              }}
            >
              Cancel
            </Button>
            <Button
              type="button"
              className={LIGHT_ORANGE_ACTION_BUTTON_CLASS}
              disabled={!canEnableMarginCallSimulator}
              analyticsId="enable_margin_call_simulator"
              analyticsProperties={commonAnalyticsProperties}
              onClick={() => {
                setIsMarginCallSimulatorEnabled(true)
                setIsMarginCallSimulatorConfirmationOpen(false)
              }}
            >
              Enable Simulator
            </Button>
          </div>
        </div>
      </Modal>

      <Modal
        isOpen={marginAction !== null}
        onClose={() => {
          if (!isMarginActionPending) {
            setMarginAction(null)
            setLocallyConfirmedFundingBalances(null)
          }
        }}
        title={`${marginActionLabel} Margin`}
        size="md"
        analyticsId={marginAction === 'withdraw' ? 'withdraw_margin' : 'deposit_margin'}
        analyticsProperties={commonAnalyticsProperties}
      >
        <div className="space-y-5">
          <p className="text-sm leading-6 text-content-secondary">
            {marginAction === 'withdraw'
              ? isSponsoredAccountConfigured
                ? 'Withdraw free USDC from the Margin Account. Separate Trading Accounts return the exact withdrawal to the connected owner wallet in the same sponsored action.'
                : 'Withdraw free USDC from your margin account. Locked margin, pending orders, and maintenance requirements remain reserved.'
              : isSponsoredAccountConfigured
                ? usesOwnerDepositAuthorization
                  ? 'Authorize USDC from the Owner Wallet, then deposit it atomically into the Plether Trading Account Margin Account. Plether sponsors network gas; USDC protocol costs still apply.'
                  : 'Deposit USDC into the Plether Trading Account Margin Account. If the Trading Account needs funds, Plether first transfers the exact shortfall from the Owner Wallet.'
                : 'Deposit USDC into your margin account. Deposited margin increases available buying power and can be used for committed orders.'}
          </p>

          {marginAction === 'deposit' && requiresOwnerWalletTransfer ? (
            <div className="border border-brand-orange/30 bg-brand-orange/10 p-3 text-sm leading-5 text-content-primary">
              <p className="font-semibold">
                Transfer <TokenAmount amount={formatPerpsUsdc(ownerWalletTransferAmountRaw)} /> from Owner Wallet
              </p>
              <p className="mt-1 text-content-secondary">
                This first confirmation is a regular Arbitrum Sepolia USDC transfer to your Trading Account and requires ETH for network gas. The following Margin Account deposit is gas-sponsored.
              </p>
            </div>
          ) : null}

          <Input
            label="Amount"
            value={marginActionAmount}
            onChange={(event) => {
              if (isNumericInput(event.target.value)) {
                setMarginActionAmount(event.target.value)
                setMarginActionStatus('idle')
                setMarginActionError(undefined)
              }
            }}
            rightElement={<TokenLabel token="USDC" />}
            autoFocus
          />
          <div className="-mt-3 flex justify-end">
            <button
              type="button"
              disabled={!canUseMarginActionMax || isMarginActionPending}
              className="group inline-flex items-center gap-1 text-xs font-semibold text-content-secondary transition-colors enabled:hover:text-content-primary disabled:cursor-not-allowed disabled:opacity-50"
              onClick={() => {
                if (!canUseMarginActionMax) return
                trackPerpsButtonClicked(`${marginAction ?? 'margin'}_max`, commonAnalyticsProperties)
                setMarginActionAmount(marginActionLimitDisplay)
                setMarginActionStatus('idle')
                setMarginActionError(undefined)
              }}
            >
              <span>Max: </span>
              <span className="group-enabled:group-hover:underline">
                <TokenAmount amount={marginActionLimitDisplay} />
              </span>
            </button>
          </div>

          <div className="border border-brand-border/20 bg-app-bg p-4">
            <div className="space-y-2">
              {marginAction === 'deposit' ? (
                <AccountSummaryRow
                  label={marginActionLimitLabel}
                  value={<TokenAmount amount={marginActionLimitDisplay} />}
                  tooltip={usesTradingAccountDepositBalance
                    ? 'Combined Trading Account and Owner Wallet USDC available for this flow. Any required Owner Wallet transfer confirms before the sponsored deposit.'
                    : 'Wallet-held USDC available to move into the Margin Account. It cannot fund orders until the deposit confirms.'}
                  tooltipDocsLink={DOCS_LINKS.withdrawable}
                />
              ) : (
                <AccountSummaryRow
                  label={marginActionLimitLabel}
                  value={<TokenAmount amount={marginActionLimitDisplay} />}
                />
              )}
              {isSponsoredAccountConfigured ? (
                <>
                  {marginAction === 'deposit' ? (
                    <AccountSummaryRow
                      label="Available to trade"
                      value={<TokenAmount amount={availableToTradeDisplayAmount} />}
                      tooltip="Free margin already deposited in the Margin Account and available for orders."
                      tooltipDocsLink={DOCS_LINKS.withdrawable}
                    />
                  ) : null}
                  {marginAction !== 'deposit' || usesTradingAccountDepositBalance ? (
                    <AccountSummaryRow
                      label="Owner Wallet USDC"
                      value={<TokenAmount amount={formatPerpsUsdc(effectiveOwnerWalletBalance)} />}
                    />
                  ) : null}
                  {marginAction === 'deposit' && usesTradingAccountDepositBalance ? (
                    <AccountSummaryRow
                      label="Trading Account USDC"
                      value={<TokenAmount amount={formatPerpsUsdc(effectiveTradingAccountBalance)} />}
                    />
                  ) : null}
                  {marginAction !== 'deposit' ? (
                    <AccountSummaryRow
                      label="Trading Account USDC"
                      value={<TokenAmount amount={formatPerpsUsdc(tradingAccountUsdcRaw)} />}
                    />
                  ) : null}
                </>
              ) : null}
              {shouldShowMarginActionPositionContext ? (
                <>
                  <AccountSummaryRow label="Position margin" value={<TokenAmount amount={formatPerpsUsdc(marginActionCurrentCollateral)} />} />
                  <p className="pt-2 text-xs leading-5 text-content-secondary">
                    Deposit and withdraw change free margin only. Position leverage changes when you open, increase, reduce, close, or add isolated position margin.
                  </p>
                </>
              ) : null}
            </div>
          </div>

          {isMarginActionInsufficient ? (
            <div className="border border-brand-orange/30 bg-brand-orange/10 p-3 text-sm text-brand-orange">
              Amount exceeds {marginActionLimitLabel.toLowerCase()}.
            </div>
          ) : null}

          {marginActionError ? (
            <div className="border border-brand-orange/30 bg-brand-orange/10 p-3 text-sm text-brand-orange">
              <p>{marginActionError}</p>
              {marginAction === 'deposit' && usesOwnerDepositAuthorization ? (
                <button
                  type="button"
                  className="mt-2 font-semibold underline underline-offset-2"
                  onClick={() => {
                    abandonDepositAuthorization()
                    setMarginActionStatus('idle')
                    setMarginActionError(undefined)
                  }}
                >
                  Start with a new USDC authorization
                </button>
              ) : null}
            </div>
          ) : null}

          <div className="grid grid-cols-1 gap-3 sm:grid-cols-2">
            <Button
              type="button"
              variant="secondary"
              className={DARK_CANCEL_BUTTON_CLASS}
              disabled={isMarginActionPending}
              analyticsId="cancel_margin_action"
              analyticsProperties={commonAnalyticsProperties}
              onClick={() => {
                setMarginAction(null)
                setLocallyConfirmedFundingBalances(null)
              }}
            >
              Cancel
            </Button>
            <Button
              type="button"
              className={LIGHT_ORANGE_ACTION_BUTTON_CLASS}
              isLoading={isMarginActionPending}
              disabled={isMarginActionSubmitDisabled}
              analyticsId={marginAction === 'withdraw' ? 'submit_withdraw_margin' : 'submit_deposit_margin'}
              analyticsProperties={commonAnalyticsProperties}
              onClick={() => {
                void handleMarginActionSubmit()
              }}
            >
              {marginActionCtaLabel}
            </Button>
          </div>
          {enableLiveTrading && isConnected && !isCorrectChain && networkSwitchError ? (
            <div className="text-xs leading-4 text-[#FFAB96]">
              {networkSwitchError}
            </div>
          ) : null}
        </div>
      </Modal>
    </section>
  )
}
