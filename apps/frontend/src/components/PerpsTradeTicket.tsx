import { type CSSProperties, type ReactNode, useEffect, useMemo, useRef, useState } from 'react'
import { useAppKit } from '@reown/appkit/react'
import { useAccount, useChainId, useReadContracts } from 'wagmi'
import { zeroAddress } from 'viem'
import { PERPS_CFD_ENGINE_LENS_ABI } from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import type { PerpsMarketPhase } from '../utils/perpsMarketSchedule'
import type { PerpsOrderHistoryRow, PerpsPendingOrder, PerpsPosition } from '../hooks'
import { usePerpsTrading, useSwitchToArbitrumSepolia, waitForPerpsOrderTerminal } from '../hooks'
import { getExplorerTxUrl } from '../utils/explorer'
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
  type PerpsDirection,
  type PerpsOracleFreshness,
} from '../utils/perps'
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
import { Button, Input, Modal, TokenAmount, TokenLabel, Tooltip } from './ui'

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
type MarginActionStatus = 'idle' | 'pending' | 'failed'
type CleanupStatus = 'idle' | 'pending' | 'failed'

interface PreviewRow {
  label: string
  value: ReactNode
  tone?: 'default' | 'positive' | 'warning' | 'muted'
}

interface ContractResult {
  status: 'failure' | 'success'
  result?: unknown
  error?: unknown
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
}

interface PerpsTradeTicketProps {
  initialLifecycleState?: TradeLifecycleState
  initialReviewOpen?: boolean
  initialDirection?: Direction
  initialSize?: string
  initialReduceOnly?: boolean
  initialOrderId?: bigint
  initialCommitTxHash?: string
  initialExecuteTxHash?: string
  initialFinalExecutionPrice?: bigint
  initialCommittedSizeDelta?: bigint
  initialFlowError?: string
  currentPositionSide?: Direction
  currentPositionAmount?: string
  enableLiveTrading?: boolean
  showFinalizationProgress?: boolean
  oraclePriceRaw?: bigint
  oraclePublishTime?: number
  oraclePriceDisplay?: string
  oracleFreshness?: PerpsOracleFreshness
  oracleFreshnessTooltip?: string
  availableToTradeRaw?: bigint
  availableToTradeAmount?: string
  portfolioValueRaw?: bigint
  withdrawableUsdcRaw?: bigint
  walletUsdcRaw?: bigint
  marginAllowanceUsdc?: bigint
  currentPosition?: PerpsPosition
  pendingOrders?: PerpsPendingOrder[]
  orderHistory?: PerpsOrderHistoryRow[]
  pendingOrderCount?: number
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
  onAccountRefresh?: () => void
}

const MOCK_PREVIEW_PRICE = 0.9909
const AVAILABLE_TO_TRADE_AMOUNT = '18 420'
const CURRENT_POSITION_AMOUNT = '8 200'
const ORDER_ID = '0x7f21...9c04'
const COMMIT_TX = '0x4a6b9f1e7c2d8a5b3c9012f4e6d7c8b9a0f123456789abcdef0123456788e2'
const EXECUTE_TX = '0xa91d6c4f83b27e10d55a4c0e29f8b6a73219d4e5c8b70af11223344556634bf'
const SLIPPAGE_OPTIONS = [0, 0.05, 0.1, 0.25, Infinity]
const LIGHT_ORANGE_ACTION_BUTTON_CLASS = '!border-[#FFAB96] !bg-[#FFAB96] !text-[#250917] enabled:hover:!border-[#FF572D] enabled:hover:!bg-[#FF572D] enabled:hover:!text-[#FFF5F9] enabled:hover:underline enabled:hover:underline-offset-4'
const DARK_CANCEL_BUTTON_CLASS = '!border-[#FFAB96]/40 !bg-[#250917] !text-[#FFF5F9] enabled:hover:!border-[#FFAB96] enabled:hover:!bg-[#3B212D] enabled:hover:underline enabled:hover:underline-offset-4'
const CONNECT_WALLET_ACTION_BUTTON_CLASS = '!border-[#FF572D] !bg-[#FF572D] !text-[#FFF5F9] enabled:hover:!border-[#FFF5F9] enabled:hover:!bg-[#FFF5F9] enabled:hover:!text-[#250917] enabled:hover:underline enabled:hover:underline-offset-4'
const EXECUTION_FEE_BPS = 4
const USDC_UNIT = 1_000_000n
const OPEN_BOUNTY_BPS_RAW = 1n
const MIN_OPEN_BOUNTY_USDC_RAW = 10_000n
const MAX_OPEN_BOUNTY_USDC_RAW = 200_000n
const CLOSE_BOUNTY_USDC_RAW = 200_000n
const SUMMARY_CLOSE_DUST_USDC_RAW = 10_000n
const ORACLE_PRICE_FRESH_SECONDS = 60
const DEFAULT_MAX_LEVERAGE = 33
const PREVIEW_LOADING_VALUE = 'Loading'
const PREVIEW_UNAVAILABLE_VALUE = 'Unavailable'
const KEEPER_REVEAL_GRACE_MS = 20_000
const KEEPER_REVEAL_PROGRESS_MS = 250
const FINALIZATION_MESSAGE_ROTATE_MS = 4_000
const ORDER_TERMINAL_WAIT_SECONDS = 60
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
  const code = {
    Expired: 0,
    CloseOnly: 1,
    SlippageExceeded: 2,
    EnginePanic: 3,
    AccountLiquidated: 4,
    EngineRevert: 5,
  }[reason]

  return code === undefined ? undefined : getPerpsOrderFailureMessage(code)
}

function terminalOrderFailureMessage(order: PerpsOrderHistoryRow): string {
  const detail = failureReasonMessage(order.failureReason)
    ?? `Terminal status: ${order.status}. Refresh order history for details.`
  return `Order failed: ${detail}`
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
    maximumFractionDigits: 2,
  }).replaceAll(',', ' ')
}

function formatUsdc(value: number): ReactNode {
  return <TokenAmount amount={formatUsdcAmount(value)} />
}

function formatUsdcRaw(value: bigint | undefined): ReactNode {
  return <TokenAmount amount={formatPerpsUsdc(value)} />
}

function formatSignedUsdcNoPlus(value: bigint | undefined): ReactNode {
  if (value === undefined) return 'Unavailable'
  const sign = value < 0n ? '-' : ''
  const absolute = value < 0n ? -value : value
  return <TokenAmount amount={`${sign}${formatPerpsUsdc(absolute)}`} />
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
  return <span className="whitespace-nowrap">{formatPerpsUsdc(value)} USDC</span>
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
    ? 'bg-cyber-neon-green'
    : freshness === 'market-closed'
      ? 'bg-cyber-warning-text'
      : freshness === 'stale'
        ? 'bg-cyber-electric-fuchsia'
        : 'bg-[#FFAB96]'
  const freshnessTooltip = freshnessTooltipOverride ?? (ageSeconds === undefined ? undefined : `updated ${formatOracleAge(ageSeconds)}`)

  return (
    <span className="inline-flex min-h-6 items-center justify-end gap-2 whitespace-nowrap">
      {freshness && freshnessTooltip ? (
        <Tooltip content={freshnessTooltip} position="top">
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
        className="inline-flex h-4 w-4 items-center justify-center text-cyber-text-secondary/70 transition-colors hover:text-[#FFAB96]"
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
        className="inline-flex h-4 w-4 items-center justify-center text-cyber-text-secondary/70 transition-colors hover:text-[#FFAB96]"
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
    <span className="inline-flex items-center justify-end gap-1 whitespace-nowrap">
      <span>{value}</span>
      <button
        type="button"
        aria-label={ariaLabel}
        title={ariaLabel}
        className="inline-flex h-4 w-4 items-center justify-center text-cyber-text-secondary/70 transition-colors hover:text-[#FFAB96]"
        onClick={() => {
          void navigator.clipboard.writeText(value)
        }}
      >
        <span className="material-symbols-outlined !text-[14px] !leading-none">content_copy</span>
      </button>
    </span>
  )
}

function previewToneClass(tone: PreviewRow['tone']): string {
  if (tone === 'positive') return 'text-cyber-neon-green'
  if (tone === 'warning') return 'text-yellow-300'
  if (tone === 'muted') return 'text-cyber-text-secondary'
  return 'text-cyber-text-primary'
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
                className="group flex min-h-6 w-full items-center justify-between gap-3 text-left text-sm text-[#FFAB96] transition-colors hover:text-cyber-text-primary"
                onClick={onSlippageClick}
              >
                <span className="group-hover:underline group-focus-visible:underline">{row.label}</span>
                <span className="flex min-h-6 items-center justify-end text-right font-semibold group-hover:underline group-focus-visible:underline">
                  {row.value}
                </span>
              </button>
              {slippageConfig}
            </div>
          )
        }

        return (
          <div key={row.label} className="flex min-h-6 items-center justify-between gap-3 text-sm">
            <dt className="text-cyber-text-secondary">{row.label}</dt>
            <dd className={`flex min-h-6 items-center justify-end text-right font-semibold ${previewToneClass(row.tone)}`}>{row.value}</dd>
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
        className="absolute top-[7px] h-px bg-cyber-border-glow/35"
        style={{ left: 'calc(16.666667% + 0.5rem)', width: 'calc(33.333333% - 1rem)' }}
      />
      <div
        className="absolute top-[7px] h-px bg-cyber-border-glow/35"
        style={{ left: 'calc(50% + 0.5rem)', width: 'calc(33.333333% - 1rem)' }}
      />
      <ol className="relative grid grid-cols-3 gap-2">
        {ORDER_LIFECYCLE_STEPS.map((step, index) => {
          const isCurrent = step.id === currentStep
          const isFuture = index > currentIndex
          const dotClass = isCurrent
            ? 'border-cyber-bright-blue bg-cyber-bright-blue'
            : isFuture
              ? 'border-cyber-border-glow/30 bg-cyber-surface-dark'
              : 'border-cyber-text-secondary/50 bg-cyber-text-secondary/50'
          const labelClass = isCurrent
            ? 'text-cyber-bright-blue'
            : isFuture
              ? 'text-cyber-text-secondary/50'
              : 'text-cyber-text-secondary'

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
    <div className="flex min-h-52 flex-col items-center justify-center border border-cyber-border-glow/20 bg-cyber-bg px-6 py-8 text-center">
      {progressPercent === undefined ? <PendingSpinner /> : <PendingProgressCircle progressPercent={progressPercent} />}
      <div className="mt-5 flex min-h-[5.25rem] max-w-full items-center justify-center text-xl font-semibold leading-7 text-cyber-text-primary sm:min-h-14">
        <AnimatedLineSwap
          contentKey={title}
          suffix={showAnimatedDots ? <AnimatedTitleDots /> : null}
          className="min-w-0 max-w-full text-center"
        >
          {title}
        </AnimatedLineSwap>
      </div>
      <div className="mt-2 flex min-h-[4.5rem] max-w-md items-start justify-center text-sm leading-6 text-cyber-text-secondary sm:min-h-12">
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
      <div className="absolute inset-0 rounded-full border-4 border-cyber-bright-blue/20 border-t-cyber-bright-blue animate-spin" />
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
          className="fill-none stroke-cyber-bright-blue/20"
          cx="28"
          cy="28"
          r={radius}
          strokeWidth="4"
        />
        <circle
          className="fill-none stroke-cyber-bright-blue"
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

function SuccessStateCard({ title, description }: { title: string; description: string }) {
  return (
    <div className="flex min-h-52 flex-col items-center justify-center border border-cyber-border-glow/20 bg-cyber-bg px-6 py-8 text-center">
      <div className="flex h-14 w-14 items-center justify-center border border-cyber-neon-green/40 bg-cyber-bg text-cyber-neon-green">
        <span className="material-symbols-outlined text-4xl">check</span>
      </div>
      <div className="mt-5 text-xl font-semibold text-cyber-text-primary">{title}</div>
      <div className="mt-2 max-w-md text-sm leading-6 text-cyber-text-secondary">{description}</div>
    </div>
  )
}

function FailedStateCard({ title, description }: { title: string; description: string }) {
  return (
    <div className="flex min-h-52 flex-col items-center justify-center border border-cyber-electric-fuchsia/40 bg-cyber-electric-fuchsia/10 px-6 py-8 text-center">
      <div className="flex h-14 w-14 items-center justify-center border border-cyber-electric-fuchsia/40 bg-cyber-electric-fuchsia/15 text-cyber-electric-fuchsia">
        <span className="material-symbols-outlined text-4xl">close</span>
      </div>
      <div className="mt-5 text-xl font-semibold text-cyber-electric-fuchsia">{title}</div>
      <div className="mt-2 max-w-xl whitespace-pre-line text-left text-sm leading-6 text-cyber-text-secondary">{description}</div>
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
  const valueColor = valueTone === 'positive' ? 'text-cyber-neon-green' : 'text-cyber-text-primary'

  return (
    <button
      type="button"
      disabled={disabled}
      className="group flex w-full cursor-pointer items-center justify-between gap-3 text-left text-sm transition-colors hover:text-cyber-text-primary disabled:cursor-default disabled:hover:text-inherit focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-[#FFAB96]"
      onClick={onClick}
    >
      <span className="text-cyber-text-secondary">{label}</span>
      <span className={`text-right font-semibold group-hover:underline group-focus-visible:underline ${valueColor}`}>{value}</span>
    </button>
  )
}

function AccountSummaryRow({
  label,
  value,
  tone = 'default',
  tooltip,
}: {
  label: string
  value: ReactNode
  tone?: 'default' | 'positive' | 'negative'
  tooltip?: ReactNode
}) {
  const valueClass = tone === 'positive'
    ? 'text-cyber-neon-green'
    : tone === 'negative'
      ? 'text-cyber-electric-fuchsia'
      : 'text-cyber-text-primary'

  return (
    <div className="flex items-center justify-between gap-3 text-sm">
      <span className="inline-flex items-center gap-1.5 text-cyber-text-secondary">
        {label}
        {tooltip ? (
          <Tooltip
            content={tooltip}
            position="bottom-end"
            className="w-[320px] max-w-[calc(100vw-2rem)] whitespace-normal p-3 text-left leading-5"
          >
            <span
              aria-label={`${label} info`}
              className="inline-flex h-3.5 w-3.5 shrink-0 cursor-help items-center justify-center rounded-full border border-current text-[9px] font-semibold leading-none text-cyber-text-secondary/80 transition-colors hover:text-[#FFAB96]"
              tabIndex={0}
            >
              i
            </span>
          </Tooltip>
        ) : null}
      </span>
      <span className={`text-right font-semibold ${valueClass}`}>{value}</span>
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
  initialOrderId,
  initialCommitTxHash,
  initialExecuteTxHash,
  initialFinalExecutionPrice,
  initialCommittedSizeDelta,
  initialFlowError,
  currentPositionSide = 'long',
  currentPositionAmount,
  enableLiveTrading = false,
  showFinalizationProgress = false,
  oraclePriceRaw,
  oraclePublishTime,
  oraclePriceDisplay,
  oracleFreshness,
  oracleFreshnessTooltip,
  availableToTradeRaw,
  availableToTradeAmount,
  portfolioValueRaw,
  withdrawableUsdcRaw,
  walletUsdcRaw,
  marginAllowanceUsdc,
  currentPosition,
  pendingOrders = [],
  orderHistory = [],
  pendingOrderCount,
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
  const { address, isConnected } = useAccount()
  const chainId = useChainId()
  const { open } = useAppKit()
  const { switchToArbitrumSepolia, switchError: networkSwitchError } = useSwitchToArbitrumSepolia()
  const { depositMargin, withdrawMargin, commitOrder, executeOrder, cleanupExpiredOrder } = usePerpsTrading()
  const [direction, setDirection] = useState<Direction>(initialDirection)
  const [isReduceOnly, setIsReduceOnly] = useState(initialReduceOnly)
  const [isMarginCallSimulatorEnabled, setIsMarginCallSimulatorEnabled] = useState(false)
  const [isMarginCallSimulatorConfirmationOpen, setIsMarginCallSimulatorConfirmationOpen] = useState(false)
  const [size, setSize] = useState(initialSize)
  const [leverage, setLeverage] = useState(5)
  const [slippage, setSlippage] = useState(0.1)
  const [lifecycleState, setLifecycleState] = useState<TradeLifecycleState>(initialLifecycleState)
  const [isReviewOpen, setIsReviewOpen] = useState(initialReviewOpen)
  const [isSlippageConfigOpen, setIsSlippageConfigOpen] = useState(false)
  const [orderId, setOrderId] = useState<bigint | undefined>(initialOrderId)
  const [commitTxHash, setCommitTxHash] = useState<string | undefined>(initialCommitTxHash)
  const [executeTxHash, setExecuteTxHash] = useState<string | undefined>(initialExecuteTxHash)
  const [finalExecutionPrice, setFinalExecutionPrice] = useState<bigint | undefined>(initialFinalExecutionPrice)
  const [committedSizeDelta, setCommittedSizeDelta] = useState<bigint | undefined>(initialCommittedSizeDelta)
  const [flowError, setFlowError] = useState<string | undefined>(initialFlowError)
  const [marginAction, setMarginAction] = useState<MarginAction | null>(null)
  const [marginActionAmount, setMarginActionAmount] = useState('')
  const [marginActionStatus, setMarginActionStatus] = useState<MarginActionStatus>('idle')
  const [marginActionError, setMarginActionError] = useState<string | undefined>()
  const [cleanupStatus, setCleanupStatus] = useState<CleanupStatus>('idle')
  const [cleanupError, setCleanupError] = useState<string | undefined>()
  const [nowSeconds, setNowSeconds] = useState(() => Math.floor(Date.now() / 1000))
  const [keeperRevealDeadlineMs, setKeeperRevealDeadlineMs] = useState<number | undefined>()
  const [keeperRevealNowMs, setKeeperRevealNowMs] = useState(() => Date.now())
  const [finalizationLoadingMessage, setFinalizationLoadingMessage] = useState<FinalizationLoadingMessage>(FINALIZATION_LOADING_MESSAGES[0])
  const [walletRequestWarning, setWalletRequestWarning] = useState<string | undefined>()
  const onAccountRefreshRef = useRef(onAccountRefresh)
  const orderWaitStartedForRef = useRef<bigint | undefined>(undefined)
  const terminalLifecycleTrackedRef = useRef<TradeLifecycleState | undefined>(undefined)
  const finalizationShownTitlesRef = useRef<Set<string>>(new Set([FINALIZATION_LOADING_MESSAGES[0].title]))
  const simulatorMaxLeverage = maxLeverageFromMaintenanceMargin(maintenanceMarginBps)
  const canEnableMarginCallSimulator = simulatorMaxLeverage > DEFAULT_MAX_LEVERAGE
  const maxLeverage = isMarginCallSimulatorEnabled ? simulatorMaxLeverage : DEFAULT_MAX_LEVERAGE
  const activeLeverage = Math.min(leverage, maxLeverage)

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
      setLifecycleState((currentState) => (
        currentState === 'revealPending' ? 'selfExecuteAvailable' : currentState
      ))
    }, Math.max(0, keeperRevealDeadlineMs - Date.now()))

    return () => {
      window.clearInterval(progressInterval)
      window.clearInterval(messageInterval)
      window.clearTimeout(timeout)
    }
  }, [enableLiveTrading, keeperRevealDeadlineMs, lifecycleState, showFinalizationProgress])

  useEffect(() => {
    if (!enableLiveTrading || orderId === undefined) return undefined
    if (orderWaitStartedForRef.current === orderId) return undefined

    orderWaitStartedForRef.current = orderId
    const controller = new AbortController()
    let cancelled = false

    void waitForPerpsOrderTerminal({
      accountAddress: address,
      orderId,
      timeoutSeconds: ORDER_TERMINAL_WAIT_SECONDS,
      signal: controller.signal,
    })
      .then((result) => {
        if (cancelled || result.timedOut || result.order === undefined || result.order.status === 'Committed') return

        setCommitTxHash((current) => current ?? result.order?.commitTxHash)
        setExecuteTxHash(result.order.revealTxHash)

        if (result.order.status === 'Executed') {
          setFlowError(undefined)
          setFinalExecutionPrice(result.order.executionPriceRaw ?? result.order.activityPriceRaw)
          setLifecycleState('executed')
        } else {
          setFlowError(terminalOrderFailureMessage(result.order))
          setLifecycleState('selfExecuteFailed')
        }

        onAccountRefreshRef.current?.()
      })
      .catch((error: unknown) => {
        if (cancelled || (error instanceof DOMException && error.name === 'AbortError')) return
      })

    return () => {
      cancelled = true
      controller.abort()
      if (orderWaitStartedForRef.current === orderId) {
        orderWaitStartedForRef.current = undefined
      }
    }
  }, [address, enableLiveTrading, orderId])

  useEffect(() => {
    setLeverage((currentLeverage) => Math.min(currentLeverage, maxLeverage))
  }, [maxLeverage])

  useEffect(() => {
    if (!canEnableMarginCallSimulator) {
      setIsMarginCallSimulatorEnabled(false)
    }
  }, [canEnableMarginCallSimulator])

  useEffect(() => {
    if (lifecycleState !== 'commitPending' || commitTxHash || flowError) {
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
  }, [address, chainId, commitTxHash, flowError, lifecycleState])

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
      ? ` It is expired and can be cleaned up.`
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
  const orderSizeDelta = (() => {
    if (dxyExposureUsdc <= 0n) return 0n
    if (
      isReducingCurrentPosition &&
      currentPosition?.size !== undefined &&
      currentPosition.size > 0n &&
      availableCloseSizeRaw > 0n &&
      maxDxyExposureRaw > 0n &&
      dxyExposureUsdc >= maxDxyExposureRaw
    ) {
      return availableCloseSizeRaw
    }

    return dxyExposureToSizeDelta(dxyExposureUsdc, oraclePriceRaw) ?? 0n
  })()
  const contractNotionalUsdc = orderSizeDelta > 0n
    ? sizeDeltaToNotionalUsdc(orderSizeDelta, oraclePriceRaw) ?? dxyExposureUsdc
    : dxyExposureUsdc
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
  const slippageNumber = Math.max(slippage, 0)
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
  const liquidationPrice = previewPrice === undefined
    ? undefined
    : direction === 'long'
      ? previewPrice * 0.945
      : previewPrice * 1.055
  const summaryDxyExposureUsdc = isReducingCurrentPosition &&
    maxDxyExposureRaw > 0n &&
    dxyExposureUsdc >= maxDxyExposureRaw
    ? availableCloseDxyExposureRaw
    : dxyExposureUsdc
  const orderSummary = buildOrderSummary({
    currentPositionSide: currentPositionSideValue,
    currentPositionDxyExposureUsdc: currentPositionDxyExposureRaw,
    direction,
    isReduceOnly,
    leverage: activeLeverage,
    dxyExposureUsdc: summaryDxyExposureUsdc,
  })
  const orderFundingRequirementUsdc = !isReducingCurrentPosition ? marginUsdc + estimatedKeeperBountyUsdc : estimatedKeeperBountyUsdc
  const marginShortfall = availableToTradeRaw !== undefined && orderFundingRequirementUsdc > availableToTradeRaw
    ? orderFundingRequirementUsdc - availableToTradeRaw
    : 0n
  const isCorrectChain = chainId === PERPS_ARBITRUM_SEPOLIA_CHAIN_ID
  const isZeroSize = dxyExposureUsdc <= 0n
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
  const {
    data: tradePreviewData,
    isLoading: isTradePreviewLoading,
    isFetching: isTradePreviewFetching,
  } = useReadContracts({
    contracts: shouldReadTradePreview
      ? [
          isReducingCurrentPosition
            ? {
                chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
                address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens,
                abi: PERPS_CFD_ENGINE_LENS_ABI,
                functionName: 'previewClose',
                args: [address ?? zeroAddress, orderSizeDelta, oraclePriceRaw],
              } as const
            : {
                chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
                address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens,
                abi: PERPS_CFD_ENGINE_LENS_ABI,
                functionName: 'previewOpen',
                args: [
                  address ?? zeroAddress,
                  directionToPerpsSide(effectiveOrderDirection),
                  orderSizeDelta,
                  marginUsdc,
                  oraclePriceRaw,
                  previewPublishTime,
                ],
              } as const,
        ]
      : [],
    query: {
      enabled: shouldReadTradePreview,
      refetchInterval: 15_000,
    },
  })
  const openPreview = !isReducingCurrentPosition
    ? parseOpenPreview(readResult(tradePreviewData as readonly ContractResult[] | undefined, 0))
    : undefined
  const closePreview = isReducingCurrentPosition
    ? parseClosePreview(readResult(tradePreviewData as readonly ContractResult[] | undefined, 0))
    : undefined
  const tradePreviewFailure = readFailure(tradePreviewData as readonly ContractResult[] | undefined, 0)
  const currentTradePreview = isReducingCurrentPosition ? closePreview : openPreview
  const isTradePreviewPending = shouldReadTradePreview && (
    isTradePreviewLoading ||
    (isTradePreviewFetching && currentTradePreview === undefined)
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
    firstPendingOrderId !== undefined &&
    oldestPendingOrderSecondsToExpiry !== undefined &&
    oldestPendingOrderSecondsToExpiry <= 0
  const liveValidationError = (() => {
    if (!enableLiveTrading) return undefined
    if (!isConnected) return 'Connect wallet to trade.'
    if (!isCorrectChain) return 'Switch to Arbitrum Sepolia.'
    if (!oraclePriceRaw || oraclePriceRaw <= 0n) return 'plDXY Perp price is not available.'
    if (isZeroSize) return 'Enter an order size.'
    if (
      isOppositePositionDirection &&
      currentPositionDxyExposureRaw > 0n &&
      dxyExposureUsdc > currentPositionDxyExposureRaw + SUMMARY_CLOSE_DUST_USDC_RAW
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
      const minimumLabel = isOpeningFromZero ? 'minimum new position' : 'minimum order size'
      return `New ${directionLabel(direction)} opens are unavailable right now. Max plDXY Perp exposure is ${formatPerpsUsdc(selectedOpenDxyCapacityUsdc)} USDC, below the ${formatPerpsUsdc(effectiveMinOpenDxyExposureUsdc)} USDC ${minimumLabel}. Add LP liquidity or loosen the skew cap before opening this side.`
    }
    if (
      !isReducingCurrentPosition &&
      !isReduceOnly &&
      effectiveMinOpenDxyExposureUsdc !== undefined &&
      dxyExposureUsdc < effectiveMinOpenDxyExposureUsdc
    ) {
      const minimumLabel = isOpeningFromZero ? 'Minimum new position' : 'Minimum order size'
      return `${minimumLabel} is ${formatPerpsUsdc(effectiveMinOpenDxyExposureUsdc)} USDC.`
    }
    if (!isReducingCurrentPosition && !isReduceOnly && selectedOpenDxyCapacityUsdc !== undefined && dxyExposureUsdc > selectedOpenDxyCapacityUsdc) {
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
      return `${pendingCloseContext} is already closing the full current position.${pendingCloseExpiryContext} Execute it or clean it up before submitting another reduce order.`
    }
    if (
      isReducingCurrentPosition &&
      currentPositionDxyExposureRaw > 0n &&
      availableCloseDxyExposureRaw > 0n &&
      dxyExposureUsdc > availableCloseDxyExposureRaw + SUMMARY_CLOSE_DUST_USDC_RAW
    ) {
      return `Only ${formatPerpsUsdc(availableCloseDxyExposureRaw)} USDC plDXY Perp exposure is available to reduce because ${formatPerpsUsdc(pendingCloseDxyExposureRaw)} USDC is already reserved by pending close orders.`
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

      return `You already have ${pendingOrderCount.toString()} pending orders, which is the current account limit. ${expiryContext} Execute or clean up an expired order before committing a new one.`
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
  const previewContractNotionalUsdc = openPreview?.notionalUsdc ?? contractNotionalUsdc
  const previewInitialMarginUsdc = openPreview?.marginDeltaUsdc ?? marginUsdc
  const previewMaintenanceMarginUsdc = openPreview?.maintenanceMarginUsdc
  const previewExecutionFeeUsdc = isReducingCurrentPosition
    ? closePreview?.executionFeeUsdc ?? protocolExecutionFeeRaw
    : openPreview?.executionFeeUsdc ?? protocolExecutionFeeRaw
  const previewVpiUsdc = isReducingCurrentPosition ? closePreview?.vpiDeltaUsdc : openPreview?.vpiUsdc
  const previewLensFallbackValue = isTradePreviewPending ? PREVIEW_LOADING_VALUE : PREVIEW_UNAVAILABLE_VALUE
  const previewLensFallbackTone = isTradePreviewPending ? 'muted' : undefined
  const previewMaintenanceMarginValue = previewMaintenanceMarginUsdc === undefined
    ? previewLensFallbackValue
    : formatUsdcRaw(previewMaintenanceMarginUsdc)
  const previewVpiValue = previewVpiUsdc === undefined
    ? previewLensFallbackValue
    : formatSignedUsdcNoPlus(previewVpiUsdc)
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
      { label: 'Contract notional', value: formatUsdcRaw(previewContractNotionalUsdc) },
      { label: 'Initial margin', value: formatUsdcRaw(previewInitialMarginUsdc) },
      { label: 'Maintenance margin', value: previewMaintenanceMarginValue, tone: previewMaintenanceMarginUsdc === undefined ? previewLensFallbackTone : undefined },
      { label: 'Resulting leverage', value: previewResultingLeverage, tone: previewResultingLeverage === PREVIEW_LOADING_VALUE ? 'muted' : undefined },
      { label: 'Max slippage', value: formatPercent(slippageNumber) },
      { label: 'Execution limit', value: formatOptionalPrice(executionLimit) },
      { label: 'Liquidation price', value: previewLiquidationPrice, tone: previewLiquidationPrice === PREVIEW_LOADING_VALUE ? 'muted' : undefined },
      { label: 'Estimated protocol execution fee', value: formatUsdcRaw(previewExecutionFeeUsdc) },
      { label: 'VPI / Price impact', value: previewVpiValue, tone: previewVpiUsdc === undefined ? previewLensFallbackTone : undefined },
      { label: 'Estimated execution reward', value: formatUsdc(keeperBounty) },
      {
        label: 'Contract side capacity',
        value: selectedOpenCapacityUsdc === undefined
          ? 'Unavailable'
          : <TokenAmount amount={formatPerpsUsdc(selectedOpenCapacityUsdc)} />,
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
      previewLensFallbackTone,
      previewMaintenanceMarginUsdc,
      previewVpiUsdc,
      selectedOpenCapacityUsdc,
      dxyExposureNumber,
      slippageNumber,
    ]
  )
  const sidePanelPreviewRows = useMemo(
    () => previewRows.filter((row) => row.label !== 'Resulting leverage'),
    [previewRows]
  )

  const currentLifecycleStep = lifecycleStep(lifecycleState)
  const displayOrderId = orderId === undefined ? (enableLiveTrading ? '--' : ORDER_ID) : orderId.toString()
  const executedOrderHistoryRow = orderId === undefined
    ? undefined
    : orderHistory.find((row) => row.orderId === orderId && row.status === 'Executed')
  const displayCommitTx = commitTxHash ?? executedOrderHistoryRow?.commitTxHash ?? (enableLiveTrading ? undefined : COMMIT_TX)
  const displayExecuteTx = executeTxHash ?? executedOrderHistoryRow?.revealTxHash ?? (enableLiveTrading ? undefined : EXECUTE_TX)
  const displayCommitTxValue = displayCommitTx ? <TxHashActions hash={displayCommitTx} /> : '--'
  const displayExecuteTxValue = displayExecuteTx ? <TxHashActions hash={displayExecuteTx} /> : '--'
  const isTerminalRevealError = flowError !== undefined &&
    (isOrderNoLongerPendingMessage(flowError) || isTerminalOrderFailureMessage(flowError))
  const shouldShowFinalizationProgress = enableLiveTrading || showFinalizationProgress
  const isKeeperRevealGraceActive = shouldShowFinalizationProgress &&
    lifecycleState === 'revealPending' &&
    (keeperRevealDeadlineMs === undefined || keeperRevealNowMs < keeperRevealDeadlineMs)
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
  const finalPriceDisplay = finalExecutionPrice
    ? formatDisplayDxyPrice(finalExecutionPrice)
    : enableLiveTrading
      ? '--'
      : formatDisplayDxyPrice(99_110_000n)
  const executedTitle = finalPriceDisplay === '--'
    ? 'Trade executed'
    : `Trade executed at ${finalPriceDisplay} USDC`
  const isReviewingFullClose = isReducingCurrentPosition &&
    availableCloseDxyExposureRaw > 0n &&
    availableCloseDxyExposureRaw <= dxyExposureUsdc + SUMMARY_CLOSE_DUST_USDC_RAW
  const reviewCtaLabel = enableLiveTrading && !isConnected
    ? 'Connect Wallet'
    : enableLiveTrading && !isCorrectChain
      ? 'Switch Network'
      : isReducingCurrentPosition
        ? isReviewingFullClose ? 'Review Close' : 'Review Reduce'
      : direction === 'long' ? 'Review Long' : 'Review Short'
  const isConnectWalletCta = enableLiveTrading && !isConnected
  const isSwitchNetworkCta = enableLiveTrading && isConnected && !isCorrectChain
  const isReviewButtonDisabled = enableLiveTrading &&
    isConnected &&
    isCorrectChain &&
    (Boolean(liveValidationError) || isTradePreviewPending)
  const marginActionAmountRaw = parsePerpsUsdc(marginActionAmount)
  const isMarginActionPending = marginActionStatus === 'pending'
  const marginActionLabel = marginAction === 'withdraw' ? 'Withdraw' : 'Deposit'
  const marginActionCtaLabel = enableLiveTrading && !isConnected
    ? 'Connect Wallet'
    : enableLiveTrading && !isCorrectChain
      ? 'Switch Network'
      : marginActionLabel
  const marginActionLimit = marginAction === 'withdraw' ? withdrawableUsdcRaw : walletUsdcRaw
  const marginActionLimitLabel = marginAction === 'withdraw' ? 'Withdrawable' : 'Wallet balance'
  const marginActionLimitDisplay = formatPerpsUsdc(marginActionLimit)
  const canUseMarginActionMax = marginActionLimit !== undefined && marginActionLimit > 0n
  const isMarginActionInsufficient = marginActionLimit !== undefined && marginActionAmountRaw > marginActionLimit
  const isMarginActionInvalid = marginActionAmountRaw <= 0n || isMarginActionInsufficient
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

  function openMarginAction(action: MarginAction) {
    trackPerpsMarginLifecycle(`${action}_opened`, commonAnalyticsProperties)
    setMarginAction(action)
    setMarginActionAmount('')
    setMarginActionStatus('idle')
    setMarginActionError(undefined)
  }

  async function handleMarginActionSubmit() {
    if (!marginAction) return
    if (enableLiveTrading && !isConnected) {
      void open()
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

    try {
      trackPerpsMarginLifecycle(`${marginAction}_submitted`, commonAnalyticsProperties)
      setMarginActionStatus('pending')
      setMarginActionError(undefined)
      if (marginAction === 'deposit') {
        await depositMargin(marginActionAmountRaw, marginAllowanceUsdc)
      } else {
        await withdrawMargin(marginActionAmountRaw)
      }
      setMarginActionStatus('idle')
      setMarginAction(null)
      setMarginActionAmount('')
      trackPerpsMarginLifecycle(`${marginAction}_succeeded`, commonAnalyticsProperties)
      onAccountRefresh?.()
    } catch (error) {
      setMarginActionStatus('failed')
      setMarginActionError(error instanceof Error ? error.message : `${marginActionLabel} failed. Check wallet and retry.`)
      trackPerpsMarginLifecycle(`${marginAction}_failed`, {
        ...commonAnalyticsProperties,
        error_category: perpsErrorCategory(error),
      })
    }
  }

  async function handleConfirmCommit() {
    setFlowError(undefined)
    setWalletRequestWarning(undefined)
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
      dxyExposureUsdc,
      contractNotionalUsdc,
      marginUsdc,
      oraclePriceRaw,
      slippageNumber,
    })
    if (!enableLiveTrading) {
      debugPerpsCommit('ticket:mock-flow')
      trackPerpsOrderLifecycle('commit_started', commonAnalyticsProperties)
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

    try {
      debugPerpsCommit('ticket:lifecycle:commitPreparing')
      trackPerpsOrderLifecycle('commit_started', commonAnalyticsProperties)
      setLifecycleState('commitPreparing')
      const sizeDelta = orderSizeDelta
      setCommittedSizeDelta(sizeDelta)
      const result = await commitOrder({
        direction: effectiveOrderDirection,
        notionalUsdc: contractNotionalUsdc,
        sizeDelta,
        marginUsdc,
        oraclePrice: oraclePriceRaw ?? 0n,
        slippagePercent: slippageNumber,
        isClose: isReducingCurrentPosition,
        onWalletRequestStart: () => {
          debugPerpsCommit('ticket:lifecycle:commitPending')
          trackPerpsOrderLifecycle('commit_pending', commonAnalyticsProperties)
          setLifecycleState('commitPending')
        },
      })
      debugPerpsCommit('ticket:commit-result', {
        hash: result.hash,
        orderId: result.orderId,
      })
      setCommitTxHash(result.hash)
      setOrderId(result.orderId)
      setKeeperRevealDeadlineMs(Date.now() + KEEPER_REVEAL_GRACE_MS)
      setKeeperRevealNowMs(Date.now())
      setLifecycleState('revealPending')
      trackPerpsOrderLifecycle('commit_succeeded', commonAnalyticsProperties)
      onAccountRefresh?.()
    } catch (error) {
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
      onAccountRefresh?.()
    } catch (error) {
      setCleanupStatus('failed')
      setCleanupError(error instanceof Error ? error.message : 'Expired-order cleanup failed')
      onAccountRefresh?.()
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
      onAccountRefresh?.()
    }
  }

  function resetReviewLifecycle() {
    setLifecycleState('preview')
    setOrderId(undefined)
    setCommitTxHash(undefined)
    setExecuteTxHash(undefined)
    setFinalExecutionPrice(undefined)
    setCommittedSizeDelta(undefined)
    setFlowError(undefined)
    setKeeperRevealDeadlineMs(undefined)
    setKeeperRevealNowMs(Date.now())
  }

  function closeReviewModal() {
    const shouldResetSize = lifecycleState === 'executed'
    resetReviewLifecycle()
    if (shouldResetSize) {
      setSize('0')
    }
    setIsReviewOpen(false)
  }

  return (
    <section className="bg-cyber-surface-dark border border-cyber-border-glow/30 overflow-visible">
      <div className="space-y-5 px-5 py-4">
        <div>
          <div className="mb-2 text-xs font-medium uppercase text-cyber-text-secondary">Direction</div>
          <div className="grid grid-cols-2 border border-cyber-border-glow/30 bg-cyber-bg">
            {(['long', 'short'] as Direction[]).map((item) => (
              <button
                key={item}
                type="button"
                className={`border px-3 py-3 text-sm font-semibold transition-colors hover:underline hover:underline-offset-4 focus-visible:underline focus-visible:underline-offset-4 ${
                  direction === item
                    ? item === 'long'
                      ? 'border-cyber-neon-green bg-cyber-neon-green text-cyber-bg'
                      : 'border-cyber-electric-fuchsia bg-cyber-electric-fuchsia text-cyber-bg'
                    : 'border-transparent text-cyber-text-primary hover:bg-[#3B212D]'
                }`}
                onClick={() => {
                  trackPerpsButtonClicked(`direction_${item}`, commonAnalyticsProperties)
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
                setSize(currentPositionFillAmount)
              }
            }}
          />
        </div>

        <div>
          <Input
            label="plDXY Perp exposure"
            value={size}
            onChange={(event) => {
              if (isNumericInput(event.target.value)) {
                setSize(event.target.value)
              }
            }}
            rightElement={<TokenLabel token="USDC" />}
          />
          <div className="mt-1.5 flex justify-end">
            <button
              type="button"
              className="group cursor-pointer text-right text-xs font-semibold text-cyber-text-secondary transition-colors hover:text-cyber-text-primary disabled:cursor-not-allowed disabled:opacity-50 disabled:hover:text-cyber-text-secondary focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-[#FFAB96]"
              disabled={!canUseMaxNotional}
              onClick={() => {
                if (canUseMaxNotional) {
                  trackPerpsButtonClicked('fill_max_exposure', commonAnalyticsProperties)
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

        <label className="flex cursor-pointer items-center gap-3 py-1 text-cyber-text-primary transition-colors hover:text-[#FFAB96]">
          <input
            type="checkbox"
            checked={isReduceOnly}
            onChange={(event) => {
              trackPerpsButtonClicked('toggle_reduce_only', {
                ...commonAnalyticsProperties,
                reduce_only: event.target.checked,
              })
              setIsReduceOnly(event.target.checked)
            }}
            className="h-4 w-4 accent-[#FFAB96]"
          />
          <span className="text-sm font-semibold">Reduce only</span>
        </label>

        <label className="flex cursor-pointer items-start gap-3 py-1 text-cyber-text-primary transition-colors hover:text-[#FFAB96]">
          <input
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
            className="mt-0.5 h-4 w-4 accent-[#FFAB96]"
          />
          <span className="text-sm font-semibold">Margin Call Simulator</span>
        </label>

        <div>
          <div className="mb-2 flex items-center justify-between gap-3">
            <label className="text-sm font-medium text-cyber-text-secondary" htmlFor="perps-leverage">
              Leverage
            </label>
            <span className="text-lg font-semibold text-[#FFAB96]">{formatLeverage(activeLeverage)}</span>
          </div>
          <input
            id="perps-leverage"
            type="range"
            min="1"
            max={maxLeverage}
            step="1"
            value={activeLeverage}
            onChange={(event) => {
              setLeverage(Math.min(Number(event.target.value), maxLeverage))
            }}
            onPointerUp={() => {
              trackPerpsButtonClicked('leverage_slider_changed', commonAnalyticsProperties)
            }}
            onKeyUp={() => {
              trackPerpsButtonClicked('leverage_slider_changed', commonAnalyticsProperties)
            }}
            className="perps-leverage-slider h-2 w-full cursor-pointer appearance-none accent-[#FFAB96]"
          />
          <div className="mt-2 flex items-center justify-between text-xs font-semibold text-cyber-text-secondary">
            <span>1x</span>
            <span>{formatLeverage(maxLeverage)}</span>
          </div>
        </div>

        <div className="border border-cyber-border-glow/20 bg-cyber-bg p-4">
          <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Preview</div>
          <PreviewRows
            rows={sidePanelPreviewRows.slice(0, 11)}
            onSlippageClick={() => {
              trackPerpsButtonClicked('toggle_slippage_config', commonAnalyticsProperties)
              setIsSlippageConfigOpen((isOpen) => !isOpen)
            }}
            slippageConfig={
              isSlippageConfigOpen ? (
                <div className="mt-3 py-3">
                  <div className="grid grid-cols-5 gap-2">
                    {SLIPPAGE_OPTIONS.map((option) => (
                      <button
                        key={option.toString()}
                        type="button"
                        className={`border px-2 py-2 text-sm font-semibold transition-colors hover:underline hover:underline-offset-4 focus-visible:underline focus-visible:underline-offset-4 ${
                          slippage === option
                            ? 'border-[#FFAB96] bg-[#FFAB96] text-cyber-bg'
                            : 'border-cyber-border-glow/30 text-cyber-text-secondary hover:bg-[#3B212D] hover:text-cyber-text-primary'
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

        {enableLiveTrading && isConnected && isCorrectChain && liveValidationError && !isZeroSize ? (
          <div className="border border-cyber-electric-fuchsia/30 bg-cyber-electric-fuchsia/10 p-3 text-sm text-cyber-electric-fuchsia">
            {liveValidationError}
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
          title={isReviewButtonDisabled ? liveValidationError : undefined}
          analyticsId={isConnectWalletCta ? 'connect_wallet_cta' : isSwitchNetworkCta ? 'switch_network_cta' : 'review_trade'}
          analyticsProperties={commonAnalyticsProperties}
          onClick={() => {
            if (enableLiveTrading && !isConnected) {
              void open()
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

        <div className="border border-cyber-border-glow/20 bg-cyber-bg p-4">
          <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Margin Account</div>
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
            />
          </div>
        </div>

        <div className="grid grid-cols-2 gap-3">
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
              <p className="px-1 py-2 text-xl font-semibold leading-7 text-cyber-text-primary">
                {orderSummary}
              </p>

              <div className="border border-cyber-border-glow/20 bg-cyber-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Commit Preview</div>
                <PreviewRows rows={previewRows} />
                <p className="mt-4 border-t border-cyber-border-glow/20 pt-3 text-sm leading-5 text-cyber-text-secondary">
                  plDXY Perp exposure is the size you choose. Contract notional is derived from the raw basket price for protocol accounting.
                </p>
              </div>

              <div className="border border-cyber-border-glow/20 bg-cyber-bg p-4">
                <div className="text-sm font-semibold text-cyber-text-primary">Delayed execution</div>
                <div className="mt-2 text-sm text-cyber-text-secondary">
                  This submits your order. Final execution settles shortly after with your accepted price constraints.
                </div>
              </div>

              {enableLiveTrading && liveValidationError ? (
                <div className="border border-cyber-electric-fuchsia/30 bg-cyber-electric-fuchsia/10 p-4 text-sm text-cyber-electric-fuchsia">
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
                        <div className="mt-3 text-xs leading-4 text-cyber-text-primary">
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
                    <div className="mt-3 text-xs text-cyber-text-primary">
                      {cleanupError}
                    </div>
                  ) : null}
                </div>
              ) : null}
              {flowError ? (
                <div className="border border-cyber-electric-fuchsia/30 bg-cyber-electric-fuchsia/10 p-4 text-sm text-cyber-electric-fuchsia">
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
                  disabled={enableLiveTrading && Boolean(liveValidationError)}
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
                title="Preparing wallet request"
                description="Checking gas and wallet network before opening your wallet. If this takes more than a few seconds, switch to Arbitrum Sepolia manually and try again."
              />

              <div className="border border-cyber-border-glow/20 bg-cyber-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Commit Transaction</div>
                <PreviewRows
                  rows={[
                    { label: 'Direction', value: directionLabel(direction) },
                    { label: 'plDXY Perp exposure', value: formatUsdc(dxyExposureNumber) },
                    { label: 'Contract notional', value: formatUsdcRaw(contractNotionalUsdc) },
                    { label: 'Max slippage', value: formatPercent(slippageNumber) },
                    { label: 'Execution limit', value: formatOptionalPrice(executionLimit) },
                    { label: 'Estimated protocol execution fee', value: formatUsdcRaw(protocolExecutionFeeRaw) },
                    { label: 'Estimated execution reward', value: formatUsdc(keeperBounty) },
                  ]}
                />
              </div>
            </>
          ) : null}

          {lifecycleState === 'commitPending' ? (
            <>
              <PendingStateCard
                title="Waiting for wallet confirmation"
                description="Confirm the commit transaction in your wallet, then wait for it to be included onchain."
              />

              {walletRequestWarning ? (
                <div className="border border-[#FFAB96]/40 bg-[#FF572D]/10 p-4 text-sm leading-5 text-[#FFAB96]">
                  {walletRequestWarning}
                </div>
              ) : null}

              <div className="border border-cyber-border-glow/20 bg-cyber-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Commit Transaction</div>
                <PreviewRows
                  rows={[
                    { label: 'Direction', value: directionLabel(direction) },
                    { label: 'plDXY Perp exposure', value: formatUsdc(dxyExposureNumber) },
                    { label: 'Contract notional', value: formatUsdcRaw(contractNotionalUsdc) },
                    { label: 'Max slippage', value: formatPercent(slippageNumber) },
                    { label: 'Execution limit', value: formatOptionalPrice(executionLimit) },
                    { label: 'Estimated protocol execution fee', value: formatUsdcRaw(protocolExecutionFeeRaw) },
                    { label: 'Estimated execution reward', value: formatUsdc(keeperBounty) },
                  ]}
                />
              </div>

              {!enableLiveTrading ? (
                <div className="grid grid-cols-2 gap-3">
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

              <div className="border border-cyber-border-glow/20 bg-cyber-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Settlement Details</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                    { label: 'Commit tx', value: displayCommitTxValue },
                    { label: 'Acceptable price', value: formatOptionalPrice(executionLimit) },
                    { label: 'Estimated execution reward', value: formatUsdc(keeperBounty) },
                    {
                      label: 'Manual finalization',
                      value: shouldShowFinalizationProgress
                        ? `Available in ${keeperRevealRemainingSeconds.toString()}s`
                        : 'Available after 04:38',
                      tone: shouldShowFinalizationProgress ? 'muted' : undefined,
                    },
                  ]}
                />
              </div>

              {isKeeperRevealGraceActive ? null : !enableLiveTrading ? (
                <div className="grid grid-cols-2 gap-3">
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

              <div className="border border-cyber-border-glow/20 bg-cyber-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Settlement Details</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                    { label: 'Commit tx', value: displayCommitTxValue },
                    { label: 'Acceptable price', value: formatOptionalPrice(executionLimit) },
                    { label: 'Estimated execution reward', value: formatUsdc(keeperBounty) },
                    {
                      label: 'Manual finalization',
                      value: flowError && isRevealNotReadyMessage(flowError)
                        ? 'Retry shortly'
                        : flowError && isPythExpiryMessage(flowError)
                          ? 'Retry with price data'
                          : 'Available now',
                      tone: flowError && isRetryableSelfExecuteMessage(flowError) ? 'warning' : 'positive',
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

              <div className="border border-cyber-border-glow/20 bg-cyber-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Finalization Transaction</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                    { label: 'Commit tx', value: displayCommitTxValue },
                    { label: 'Acceptable price', value: formatOptionalPrice(executionLimit) },
                    { label: 'Estimated execution reward', value: formatUsdc(keeperBounty) },
                    { label: 'Transaction', value: 'Awaiting confirmation' },
                  ]}
                />
              </div>

              {!enableLiveTrading ? (
                <div className="grid grid-cols-2 gap-3">
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

              <div className="border border-cyber-border-glow/20 bg-cyber-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Settlement Details</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                    { label: 'Commit tx', value: displayCommitTxValue },
                    { label: 'Acceptable price', value: formatOptionalPrice(executionLimit) },
                    {
                      label: 'Manual finalization',
                      value: isTerminalRevealError ? 'Unavailable' : 'Retry available',
                      tone: 'warning',
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
              <SuccessStateCard title={executedTitle} description="Execution settled onchain and the final price is confirmed." />
              <div className="border border-cyber-border-glow/20 bg-cyber-bg p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Final Result</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                    { label: 'Direction', value: directionLabel(direction) },
                    { label: 'Final price', value: finalPriceDisplay },
                    { label: 'Target plDXY Perp exposure', value: formatUsdc(dxyExposureNumber) },
                    { label: 'Execution plDXY Perp exposure', value: finalExecutedDxyExposureUsdc === undefined ? formatUsdc(dxyExposureNumber) : formatUsdcRaw(finalExecutedDxyExposureUsdc) },
                    { label: 'Contract notional', value: finalExecutedNotionalUsdc === undefined ? formatUsdcRaw(contractNotionalUsdc) : formatUsdcRaw(finalExecutedNotionalUsdc) },
                    { label: 'Margin posted', value: formatUsdc(marginNumber) },
                    { label: 'Protocol execution fee', value: formatUsdcRaw(finalProtocolExecutionFee) },
                    { label: 'VPI / Price impact', value: 'Unavailable' },
                    { label: 'Execution reward', value: formatUsdc(keeperBounty) },
                    { label: 'Commit tx', value: displayCommitTxValue },
                    { label: 'Reveal tx', value: displayExecuteTxValue },
                  ]}
                />
                <p className="mt-4 border-t border-cyber-border-glow/20 pt-3 text-sm leading-5 text-cyber-text-secondary">
                  Target plDXY Perp exposure is what you submitted. Execution plDXY Perp exposure is the committed size valued with the displayed plDXY Perp price at finalization.
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
            <p className="text-sm leading-6 text-cyber-text-secondary">
              This mode removes the normal {formatLeverage(DEFAULT_MAX_LEVERAGE)} UI cap and lets the leverage control
              reach the protocol maintenance-margin boundary. It is useful for testing margin-call behavior, but a position
              opened near this cap can become invalid or liquidatable from a tiny adverse move, VPI, execution fees,
              execution rewards, or carry.
            </p>
            <p className="mt-3 text-sm leading-6 text-[#FFAB96]">
              The current maintenance margin can be temporary.
              {marketPhase === 'open' && marketCurrentDuration ? (
                <> Market is open for another <span className="font-semibold text-cyber-text-primary">{marketCurrentDuration}</span>.</>
              ) : null}
              {' '}
              When the market closes, this setting may expire or become stricter, so add margin or reduce the position
              before that time if you keep a simulator-level position open.
            </p>
          </div>

          <div className="border border-cyber-border-glow/20 bg-cyber-bg p-4">
            <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Leverage rule</div>
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

          <div className="border border-cyber-border-glow/20 bg-cyber-bg p-4">
            <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Current order math</div>
            <div className="space-y-2">
              <AccountSummaryRow label="Selected leverage" value={formatLeverage(activeLeverage)} />
              <AccountSummaryRow label="plDXY Perp exposure" value={formatUsdcRaw(dxyExposureUsdc)} />
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

          <div className="grid grid-cols-2 gap-3">
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
          }
        }}
        title={`${marginActionLabel} Margin`}
        size="md"
        analyticsId={marginAction === 'withdraw' ? 'withdraw_margin' : 'deposit_margin'}
        analyticsProperties={commonAnalyticsProperties}
      >
        <div className="space-y-5">
          <p className="text-sm leading-6 text-cyber-text-secondary">
            {marginAction === 'withdraw'
              ? 'Withdraw free USDC from your margin account. Locked margin, pending orders, and maintenance requirements remain reserved.'
              : 'Deposit USDC into your margin account. Deposited margin increases available buying power and can be used for committed orders.'}
          </p>

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
              className="group inline-flex items-center gap-1 text-xs font-semibold text-cyber-text-secondary transition-colors enabled:hover:text-cyber-text-primary disabled:cursor-not-allowed disabled:opacity-50"
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

          <div className="border border-cyber-border-glow/20 bg-cyber-bg p-4">
            <div className="space-y-2">
              <AccountSummaryRow label={marginActionLimitLabel} value={<TokenAmount amount={marginActionLimitDisplay} />} />
              <AccountSummaryRow label="Amount" value={<TokenAmount amount={formatPerpsUsdc(marginActionAmountRaw)} />} />
              {shouldShowMarginActionPositionContext ? (
                <>
                  <AccountSummaryRow label="Position margin" value={<TokenAmount amount={formatPerpsUsdc(marginActionCurrentCollateral)} />} />
                  <p className="pt-2 text-xs leading-5 text-cyber-text-secondary">
                    Deposit and withdraw change free margin only. Position leverage changes when you open, increase, reduce, close, or add isolated position margin.
                  </p>
                </>
              ) : null}
            </div>
          </div>

          {isMarginActionInsufficient ? (
            <div className="border border-cyber-electric-fuchsia/30 bg-cyber-electric-fuchsia/10 p-3 text-sm text-cyber-electric-fuchsia">
              Amount exceeds {marginActionLimitLabel.toLowerCase()}.
            </div>
          ) : null}

          {marginActionError ? (
            <div className="border border-cyber-electric-fuchsia/30 bg-cyber-electric-fuchsia/10 p-3 text-sm text-cyber-electric-fuchsia">
              {marginActionError}
            </div>
          ) : null}

          <div className="grid grid-cols-2 gap-3">
            <Button
              type="button"
              variant="secondary"
              className={DARK_CANCEL_BUTTON_CLASS}
              disabled={isMarginActionPending}
              analyticsId="cancel_margin_action"
              analyticsProperties={commonAnalyticsProperties}
              onClick={() => {
                setMarginAction(null)
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
