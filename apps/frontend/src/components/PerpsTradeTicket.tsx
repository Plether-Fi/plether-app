import { type ReactNode, useEffect, useMemo, useState } from 'react'
import { useAppKit } from '@reown/appkit/react'
import { useAccount, useChainId, useSwitchChain } from 'wagmi'
import { PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import type { PerpsPosition } from '../hooks'
import { usePerpsTrading } from '../hooks'
import { getExplorerTxUrl } from '../utils/explorer'
import {
  formatPerpsPrice,
  formatSignedPerpsUsdc,
  formatPerpsUsdc,
  formatPerpsUsdcFloor,
  getPerpsTargetPrice,
  parsePerpsUsdc,
  sizeDeltaToNotionalUsdc,
  type PerpsDirection,
} from '../utils/perps'
import { getPerpsOrderFailureMessage } from '../utils/perpsErrors'
import { resolvePerpsSizeDelta } from '../utils/perpsOrder'
import { Button, Input, Modal, TokenAmount, TokenLabel } from './ui'

type Direction = PerpsDirection
export type TradeLifecycleState =
  | 'preview'
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
interface PositionSnapshot {
  exists: boolean
  side?: Direction
  size: bigint
}

interface PreviewRow {
  label: string
  value: ReactNode
  tone?: 'default' | 'positive' | 'warning'
}

interface PerpsTradeTicketProps {
  initialLifecycleState?: TradeLifecycleState
  initialReviewOpen?: boolean
  initialDirection?: Direction
  initialSize?: string
  initialReduceOnly?: boolean
  currentPositionSide?: Direction
  currentPositionAmount?: string
  enableLiveTrading?: boolean
  oraclePriceRaw?: bigint
  oraclePriceDisplay?: string
  availableToTradeRaw?: bigint
  availableToTradeAmount?: string
  portfolioValueRaw?: bigint
  withdrawableUsdcRaw?: bigint
  walletUsdcRaw?: bigint
  marginAllowanceUsdc?: bigint
  currentPosition?: PerpsPosition
  pendingOrderCount?: number
  pendingOrderIds?: bigint[]
  maxPendingOrders?: bigint
  firstPendingOrderId?: bigint
  firstPendingOrderExpiryTime?: bigint
  longOpenCapacityUsdc?: bigint
  shortOpenCapacityUsdc?: bigint
  minOpenNotionalUsdc?: bigint
  executionFeeBps?: bigint
  onAccountRefresh?: () => void
}

const MOCK_PREVIEW_PRICE = 0.9909
const AVAILABLE_TO_TRADE_AMOUNT = '18 420'
const CURRENT_POSITION_AMOUNT = '8 200'
const ORDER_ID = '0x7f21...9c04'
const COMMIT_TX = '0x4a6b9f1e7c2d8a5b3c9012f4e6d7c8b9a0f123456789abcdef0123456788e2'
const EXECUTE_TX = '0xa91d6c4f83b27e10d55a4c0e29f8b6a73219d4e5c8b70af11223344556634bf'
const SLIPPAGE_OPTIONS = [0.05, 0.1, 0.25, Infinity]
const EXECUTION_FEE_BPS = 4
const OPEN_BOUNTY_BPS = 1
const MIN_OPEN_BOUNTY_USDC = 0.01
const MAX_OPEN_BOUNTY_USDC = 0.2
const USDC_UNIT = 1_000_000n
const OPEN_BOUNTY_BPS_RAW = 1n
const MIN_OPEN_BOUNTY_USDC_RAW = 10_000n
const MAX_OPEN_BOUNTY_USDC_RAW = 200_000n
const CLOSE_BOUNTY_USDC = 0.2
const CLOSE_BOUNTY_USDC_RAW = 200_000n
const SUMMARY_CLOSE_DUST_USDC_RAW = 10_000n

function isPythExpiryMessage(message: string): boolean {
  const lowerMessage = message.toLowerCase()
  return (
    lowerMessage.includes('pyth price data expired') ||
    lowerMessage.includes('stale-price error') ||
    lowerMessage.includes('historical pyth update was unavailable') ||
    lowerMessage.includes('router could not use the historical pyth update') ||
    lowerMessage.includes('historical pyth update was rejected') ||
    lowerMessage.includes('hermes rate limit reached')
  )
}

function isHermesRateLimitMessage(message: string): boolean {
  return message.toLowerCase().includes('hermes rate limit reached')
}

function isHistoricalPythRejectedMessage(message: string): boolean {
  return message.toLowerCase().includes('historical pyth update was rejected')
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

function didPositionMoveAsExpected({
  before,
  after,
  direction,
  isReduceOnly,
}: {
  before: PositionSnapshot | undefined
  after: PerpsPosition | undefined
  direction: Direction
  isReduceOnly: boolean
}): boolean {
  if (!before) return false
  const afterExists = Boolean(after?.exists)
  const afterSize = after?.size ?? 0n
  const afterSide = after?.direction

  if (!before.exists) {
    return afterExists && afterSide === direction && afterSize > 0n
  }

  if (isReduceOnly || direction !== before.side) {
    if (!afterExists) return true
    return afterSide === before.side && afterSize < before.size
  }

  return afterExists && afterSide === direction && afterSize > before.size
}

const ORDER_LIFECYCLE_STEPS: { id: OrderLifecycleStep; label: string }[] = [
  { id: 'preview', label: 'Preview' },
  { id: 'commit', label: 'Commit' },
  { id: 'reveal', label: 'Reveal' },
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

function formatPercent(value: number): string {
  if (!Number.isFinite(value)) return 'Infinity'

  return `${value.toLocaleString('en-US', {
    maximumFractionDigits: 3,
    minimumFractionDigits: 0,
  })}%`
}

function formatLeverage(value: number): string {
  return `${value.toString()}x`
}

function formatDuration(seconds: number): string {
  if (!Number.isFinite(seconds) || seconds <= 0) return 'now'

  const days = Math.floor(seconds / 86_400)
  const hours = Math.floor((seconds % 86_400) / 3_600)
  const minutes = Math.floor((seconds % 3_600) / 60)
  const remainingSeconds = seconds % 60
  const parts = [
    days > 0 ? `${days}d` : '',
    hours > 0 ? `${hours}h` : '',
    minutes > 0 ? `${minutes}m` : '',
    days === 0 && hours === 0 ? `${remainingSeconds}s` : '',
  ].filter(Boolean)

  return parts.join(' ')
}

function clamp(value: number, min: number, max: number): number {
  return Math.min(Math.max(value, min), max)
}

function clampBigInt(value: bigint, min: bigint, max: bigint): bigint {
  if (value < min) return min
  if (value > max) return max
  return value
}

function minBigInt(a: bigint, b: bigint): bigint {
  return a < b ? a : b
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

function directionLabel(direction: Direction): string {
  return direction === 'long' ? 'Long DXY' : 'Short DXY'
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

function TxHashActions({ hash }: { hash: string }) {
  return (
    <span className="inline-flex items-center justify-end gap-1 whitespace-nowrap">
      <span>{truncateHash(hash)}</span>
      <button
        type="button"
        aria-label="Copy tx hash"
        title="Copy tx hash"
        className="inline-flex h-4 w-4 items-center justify-center text-cyber-text-secondary/70 transition-colors hover:text-cyber-bright-blue"
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
        className="inline-flex h-4 w-4 items-center justify-center text-cyber-text-secondary/70 transition-colors hover:text-cyber-bright-blue"
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
        className="inline-flex h-4 w-4 items-center justify-center text-cyber-text-secondary/70 transition-colors hover:text-cyber-bright-blue"
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
        if (row.label === 'Slippage' && onSlippageClick) {
          return (
            <div key={row.label}>
              <button
                type="button"
                className="flex w-full items-center justify-between gap-3 text-left text-sm text-cyber-bright-blue transition-colors hover:text-cyber-neon-green"
                onClick={onSlippageClick}
              >
                <span>{row.label}</span>
                <span className="text-right font-semibold">{row.value}</span>
              </button>
              {slippageConfig}
            </div>
          )
        }

        return (
          <div key={row.label} className="flex items-center justify-between gap-3 text-sm">
            <dt className="text-cyber-text-secondary">{row.label}</dt>
            <dd className={`text-right font-semibold ${previewToneClass(row.tone)}`}>{row.value}</dd>
          </div>
        )
      })}
    </dl>
  )
}

function lifecycleStep(state: TradeLifecycleState): OrderLifecycleStep {
  if (state === 'preview') return 'preview'
  if (state === 'commitPending' || state === 'commitConfirmed' || state === 'failed') return 'commit'
  return 'reveal'
}

function oppositeDirection(direction: Direction): Direction {
  return direction === 'long' ? 'short' : 'long'
}

function buildOrderSummary({
  currentPositionSide,
  currentPositionNotionalUsdc,
  direction,
  isReduceOnly,
  leverage,
  notionalUsdc,
}: {
  currentPositionSide: Direction
  currentPositionNotionalUsdc: bigint
  direction: Direction
  isReduceOnly: boolean
  leverage: number
  notionalUsdc: bigint
}): ReactNode {
  const orderAmount = <OrderSummaryRawAmount value={notionalUsdc} />
  const selectedDirection = directionLabel(direction)
  const currentDirection = directionLabel(currentPositionSide)
  const remainingPositionNotionalUsdc = currentPositionNotionalUsdc > notionalUsdc
    ? currentPositionNotionalUsdc - notionalUsdc
    : 0n
  const isFullClose = currentPositionNotionalUsdc > 0n && remainingPositionNotionalUsdc <= SUMMARY_CLOSE_DUST_USDC_RAW

  if (currentPositionNotionalUsdc <= 0n) {
    if (isReduceOnly) {
      return <>You are submitting a reduce-only {selectedDirection} order with {orderAmount} target notional.</>
    }
    return <>You are opening a {selectedDirection} position with {orderAmount} target notional at up to {formatLeverage(leverage)} leverage.</>
  }

  if (isReduceOnly) {
    if (isFullClose) return <>You are closing your {currentDirection} position.</>
    return <>You are reducing your {currentDirection} position by {orderAmount} target notional to <OrderSummaryRawAmount value={remainingPositionNotionalUsdc} />.</>
  }

  if (direction === currentPositionSide) {
    return <>You are increasing your {selectedDirection} position by {orderAmount} target notional to <OrderSummaryRawAmount value={currentPositionNotionalUsdc + notionalUsdc} />.</>
  }

  if (!isFullClose && notionalUsdc < currentPositionNotionalUsdc) {
    return <>You are reducing your {currentDirection} position by {orderAmount} target notional to <OrderSummaryRawAmount value={remainingPositionNotionalUsdc} />.</>
  }

  if (isFullClose) {
    return <>You are closing your {currentDirection} position.</>
  }

  return <>You are closing your {currentDirection} position and opening a {directionLabel(oppositeDirection(currentPositionSide))} position with <OrderSummaryRawAmount value={notionalUsdc - currentPositionNotionalUsdc} /> target notional.</>
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
            ? 'border-cyber-bright-blue bg-cyber-bright-blue shadow-[0_0_0_5px_rgba(56,189,248,0.16)]'
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
}: {
  title: string
  description: string
}) {
  return (
    <div className="flex min-h-52 flex-col items-center justify-center border border-cyber-border-glow/20 bg-cyber-bg/35 px-6 py-8 text-center">
      <div className="relative h-14 w-14 shrink-0">
        <div className="absolute inset-0 rounded-full border-4 border-cyber-bright-blue/20 border-t-cyber-bright-blue animate-spin" />
      </div>
      <div className="mt-5 text-xl font-semibold text-cyber-text-primary">{title}</div>
      <div className="mt-2 max-w-md text-sm leading-6 text-cyber-text-secondary">{description}</div>
    </div>
  )
}

function SuccessStateCard({ title, description }: { title: string; description: string }) {
  return (
    <div className="flex min-h-52 flex-col items-center justify-center border border-cyber-border-glow/20 bg-cyber-bg/35 px-6 py-8 text-center">
      <div className="flex h-14 w-14 items-center justify-center border border-cyber-neon-green/40 bg-cyber-bg/50 text-cyber-neon-green">
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
      <div className="mt-2 max-w-md text-sm leading-6 text-cyber-text-secondary">{description}</div>
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
      className="group flex w-full cursor-pointer items-center justify-between gap-3 text-left text-sm transition-colors hover:text-cyber-text-primary disabled:cursor-default disabled:hover:text-inherit focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-cyber-bright-blue"
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
}: {
  label: string
  value: ReactNode
  tone?: 'default' | 'positive' | 'negative'
}) {
  const valueClass = tone === 'positive'
    ? 'text-cyber-neon-green'
    : tone === 'negative'
      ? 'text-cyber-electric-fuchsia'
      : 'text-cyber-text-primary'

  return (
    <div className="flex items-center justify-between gap-3 text-sm">
      <span className="text-cyber-text-secondary">{label}</span>
      <span className={`text-right font-semibold ${valueClass}`}>{value}</span>
    </div>
  )
}

function isNumericInput(value: string): boolean {
  return /^[0-9., ]*$/.test(value)
}

export function PerpsTradeTicket({
  initialLifecycleState = 'preview',
  initialReviewOpen = false,
  initialDirection = 'long',
  initialSize = '0',
  initialReduceOnly = false,
  currentPositionSide = 'long',
  currentPositionAmount,
  enableLiveTrading = false,
  oraclePriceRaw,
  oraclePriceDisplay,
  availableToTradeRaw,
  availableToTradeAmount,
  portfolioValueRaw,
  withdrawableUsdcRaw,
  walletUsdcRaw,
  marginAllowanceUsdc,
  currentPosition,
  pendingOrderCount,
  pendingOrderIds = [],
  maxPendingOrders,
  firstPendingOrderId,
  firstPendingOrderExpiryTime,
  longOpenCapacityUsdc,
  shortOpenCapacityUsdc,
  minOpenNotionalUsdc,
  executionFeeBps,
  onAccountRefresh,
}: PerpsTradeTicketProps) {
  const { isConnected } = useAccount()
  const chainId = useChainId()
  const { open } = useAppKit()
  const { switchChain } = useSwitchChain()
  const { depositMargin, withdrawMargin, commitOrder, executeOrder, cleanupExpiredOrder } = usePerpsTrading()
  const [direction, setDirection] = useState<Direction>(initialDirection)
  const [isReduceOnly, setIsReduceOnly] = useState(initialReduceOnly)
  const [size, setSize] = useState(initialSize)
  const [leverage, setLeverage] = useState(5)
  const [slippage, setSlippage] = useState(0.1)
  const [lifecycleState, setLifecycleState] = useState<TradeLifecycleState>(initialLifecycleState)
  const [isReviewOpen, setIsReviewOpen] = useState(initialReviewOpen)
  const [isSlippageConfigOpen, setIsSlippageConfigOpen] = useState(false)
  const [orderId, setOrderId] = useState<bigint | undefined>()
  const [commitTxHash, setCommitTxHash] = useState<string | undefined>()
  const [executeTxHash, setExecuteTxHash] = useState<string | undefined>()
  const [finalExecutionPrice, setFinalExecutionPrice] = useState<bigint | undefined>()
  const [committedSizeDelta, setCommittedSizeDelta] = useState<bigint | undefined>()
  const [flowError, setFlowError] = useState<string | undefined>()
  const [marginAction, setMarginAction] = useState<MarginAction | null>(null)
  const [marginActionAmount, setMarginActionAmount] = useState('')
  const [marginActionStatus, setMarginActionStatus] = useState<MarginActionStatus>('idle')
  const [marginActionError, setMarginActionError] = useState<string | undefined>()
  const [cleanupStatus, setCleanupStatus] = useState<CleanupStatus>('idle')
  const [cleanupError, setCleanupError] = useState<string | undefined>()
  const [nowSeconds, setNowSeconds] = useState(() => Math.floor(Date.now() / 1000))
  const [positionSnapshotAtCommit, setPositionSnapshotAtCommit] = useState<PositionSnapshot | undefined>()

  useEffect(() => {
    if (firstPendingOrderExpiryTime === undefined) return undefined
    const interval = window.setInterval(() => {
      setNowSeconds(Math.floor(Date.now() / 1000))
    }, 1_000)

    return () => {
      window.clearInterval(interval)
    }
  }, [firstPendingOrderExpiryTime])

  useEffect(() => {
    if (!enableLiveTrading || orderId === undefined) return
    if (!['revealPending', 'selfExecuteAvailable', 'selfExecutePending', 'selfExecuteFailed'].includes(lifecycleState)) return
    if (pendingOrderIds.some((pendingOrderId) => pendingOrderId === orderId)) return
    if (!didPositionMoveAsExpected({
      before: positionSnapshotAtCommit,
      after: currentPosition,
      direction,
      isReduceOnly,
    })) {
      return
    }

    setFlowError(undefined)
    setFinalExecutionPrice(currentPosition?.entryPrice)
    setLifecycleState('executed')
  }, [
    currentPosition,
    direction,
    enableLiveTrading,
    isReduceOnly,
    lifecycleState,
    orderId,
    pendingOrderIds,
    positionSnapshotAtCommit,
  ])

  const sizeNumber = parseAmount(size)
  const currentPositionSideValue = currentPosition?.exists ? currentPosition.direction : currentPositionSide
  const currentPositionDisplayAmount = currentPosition?.exists
    ? formatPerpsUsdc(currentPosition.estimatedNotionalUsdc)
    : currentPositionAmount ?? (enableLiveTrading ? '0' : CURRENT_POSITION_AMOUNT)
  const unrealizedPnlRaw = currentPosition?.exists ? currentPosition.unrealizedPnlUsdc : undefined
  const accountSummaryPnlTone = unrealizedPnlRaw === undefined || unrealizedPnlRaw === 0n
    ? 'default'
    : unrealizedPnlRaw > 0n ? 'positive' : 'negative'
  const availableToTradeDisplayAmount = availableToTradeAmount ?? (enableLiveTrading ? '0' : AVAILABLE_TO_TRADE_AMOUNT)
  const canUseAvailableToTrade = parseAmount(availableToTradeDisplayAmount) > 0
  const canUseCurrentPosition = parseAmount(currentPositionDisplayAmount) > 0
  const currentPositionRawNotional = currentPosition?.estimatedNotionalUsdc ?? parsePerpsUsdc(currentPositionDisplayAmount)
  const notionalUsdc = parsePerpsUsdc(size)
  const hasCurrentPosition = Boolean(currentPosition?.exists && currentPositionRawNotional > 0n)
  const isOppositePositionDirection = Boolean(hasCurrentPosition && currentPosition && direction !== currentPosition.direction)
  const isReducingCurrentPosition = Boolean(hasCurrentPosition && (isReduceOnly || isOppositePositionDirection))
  const effectiveOrderDirection = isReducingCurrentPosition && currentPosition?.direction
    ? currentPosition.direction
    : direction
  const availableToTradeForMaxRaw = availableToTradeRaw ?? (enableLiveTrading ? 0n : parsePerpsUsdc(availableToTradeDisplayAmount))
  const selectedOpenCapacityUsdc = direction === 'long' ? longOpenCapacityUsdc : shortOpenCapacityUsdc
  const maxNotionalFromFundingRaw = canUseAvailableToTrade
    ? maxOpenNotionalForMargin(availableToTradeForMaxRaw, leverage)
    : 0n
  const maxOpenNotionalRaw = selectedOpenCapacityUsdc === undefined
    ? maxNotionalFromFundingRaw
    : minBigInt(maxNotionalFromFundingRaw, selectedOpenCapacityUsdc)
  const maxNotionalForSizeInputRaw = isReducingCurrentPosition
    ? currentPositionRawNotional
    : maxOpenNotionalRaw
  const maxNotionalFromLeverageAmount = formatPerpsUsdcFloor(maxNotionalForSizeInputRaw)
  const maxNotionalFromLeverageRaw = parsePerpsUsdc(maxNotionalFromLeverageAmount)
  const canUseMaxNotional = parseAmount(maxNotionalFromLeverageAmount) > 0
  const marginNumber = isReducingCurrentPosition ? 0 : leverage > 0 ? sizeNumber / leverage : 0
  const executionFeeBpsRaw = executionFeeBps ?? BigInt(EXECUTION_FEE_BPS)
  const protocolExecutionFeeRaw = executionFeeUsdcRaw(notionalUsdc, executionFeeBpsRaw)
  const keeperBounty = isReducingCurrentPosition
    ? CLOSE_BOUNTY_USDC
    : clamp((sizeNumber * OPEN_BOUNTY_BPS) / 10_000, MIN_OPEN_BOUNTY_USDC, MAX_OPEN_BOUNTY_USDC)
  const slippageNumber = Math.max(slippage, 0)
  const previewPrice = oraclePriceRaw
    ? Number(formatPerpsPrice(oraclePriceRaw))
    : enableLiveTrading
      ? undefined
      : MOCK_PREVIEW_PRICE
  const rawExecutionLimit = oraclePriceRaw
    ? getPerpsTargetPrice({ direction: effectiveOrderDirection, isClose: isReducingCurrentPosition, oraclePrice: oraclePriceRaw, slippagePercent: slippageNumber })
    : undefined
  const executionLimit = rawExecutionLimit === 0n
    ? null
    : rawExecutionLimit ? Number(formatPerpsPrice(rawExecutionLimit)) : !enableLiveTrading && Number.isFinite(slippageNumber)
      ? MOCK_PREVIEW_PRICE * (direction === 'long' ? 1 - slippageNumber / 100 : 1 + slippageNumber / 100)
      : undefined
  const liquidationPrice = previewPrice === undefined
    ? undefined
    : direction === 'long'
      ? previewPrice * 0.945
      : previewPrice * 1.055
  const sideCapacityValue = selectedOpenCapacityUsdc === undefined
    ? 'Unavailable'
    : <TokenAmount amount={formatPerpsUsdc(selectedOpenCapacityUsdc)} />
  const sideCapacityTone = selectedOpenCapacityUsdc === undefined ? undefined : 'positive'
  const summaryNotionalUsdc = isReducingCurrentPosition &&
    maxNotionalFromLeverageRaw > 0n &&
    notionalUsdc >= maxNotionalFromLeverageRaw
    ? currentPositionRawNotional
    : notionalUsdc
  const orderSummary = buildOrderSummary({
    currentPositionSide: currentPositionSideValue,
    currentPositionNotionalUsdc: currentPositionRawNotional,
    direction,
    isReduceOnly,
    leverage,
    notionalUsdc: summaryNotionalUsdc,
  })
  const marginUsdc = isReducingCurrentPosition ? 0n : leverage > 0 ? notionalUsdc / BigInt(leverage) : 0n
  const estimatedKeeperBountyUsdc = isReducingCurrentPosition ? CLOSE_BOUNTY_USDC_RAW : estimateOpenBountyUsdcRaw(notionalUsdc)
  const orderFundingRequirementUsdc = !isReducingCurrentPosition ? marginUsdc + estimatedKeeperBountyUsdc : estimatedKeeperBountyUsdc
  const marginShortfall = availableToTradeRaw !== undefined && orderFundingRequirementUsdc > availableToTradeRaw
    ? orderFundingRequirementUsdc - availableToTradeRaw
    : 0n
  const isCorrectChain = chainId === PERPS_ARBITRUM_SEPOLIA_CHAIN_ID
  const isZeroSize = notionalUsdc <= 0n
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
    if (!oraclePriceRaw || oraclePriceRaw <= 0n) return 'Oracle price is not available.'
    if (isZeroSize) return 'Enter an order size.'
    if (
      isOppositePositionDirection &&
      currentPositionRawNotional > 0n &&
      notionalUsdc > currentPositionRawNotional
    ) {
      return 'One-step flips are not supported yet. Reduce or close the current position first, then open the other side.'
    }
    if (
      !isReducingCurrentPosition &&
      !isReduceOnly &&
      selectedOpenCapacityUsdc !== undefined &&
      minOpenNotionalUsdc !== undefined &&
      selectedOpenCapacityUsdc < minOpenNotionalUsdc
    ) {
      return `New ${directionLabel(direction)} opens are unavailable right now. Max open size is ${formatPerpsUsdc(selectedOpenCapacityUsdc)} USDC, below the ${formatPerpsUsdc(minOpenNotionalUsdc)} USDC minimum. Add LP liquidity or loosen the skew cap before opening this side.`
    }
    if (!isReducingCurrentPosition && !isReduceOnly && minOpenNotionalUsdc !== undefined && notionalUsdc < minOpenNotionalUsdc) {
      return `Minimum open size is ${formatPerpsUsdc(minOpenNotionalUsdc)} USDC.`
    }
    if (!isReducingCurrentPosition && !isReduceOnly && selectedOpenCapacityUsdc !== undefined && notionalUsdc > selectedOpenCapacityUsdc) {
      return `Max ${directionLabel(direction)} open size is ${formatPerpsUsdc(selectedOpenCapacityUsdc)} USDC before hitting the market skew cap.`
    }
    if (isReduceOnly && !currentPosition?.exists) return 'No current position to reduce.'
    if (isReducingCurrentPosition && currentPositionRawNotional > 0n && notionalUsdc > currentPositionRawNotional) {
      return 'Reduce size exceeds the current position.'
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
    return undefined
  })()

  const previewRows = useMemo<PreviewRow[]>(
    () => [
      { label: 'Oracle price', value: oraclePriceDisplay ?? formatOptionalPrice(previewPrice) },
      { label: 'Target notional', value: formatUsdc(sizeNumber) },
      { label: 'Initial margin', value: formatUsdc(marginNumber) },
      { label: 'Leverage', value: formatLeverage(leverage) },
      { label: 'Slippage', value: formatPercent(slippageNumber) },
      { label: 'Execution limit', value: formatOptionalPrice(executionLimit) },
      { label: 'Liquidation price', value: enableLiveTrading ? 'Unavailable' : formatOptionalPrice(liquidationPrice) },
      { label: 'Estimated protocol execution fee', value: formatUsdcRaw(protocolExecutionFeeRaw) },
      { label: 'VPI / Price impact', value: 'Unavailable' },
      { label: 'Estimated keeper bounty', value: formatUsdc(keeperBounty) },
      { label: 'Side capacity', value: sideCapacityValue, tone: sideCapacityTone },
    ],
    [
      enableLiveTrading,
      executionLimit,
      keeperBounty,
      leverage,
      liquidationPrice,
      marginNumber,
      oraclePriceDisplay,
      previewPrice,
      protocolExecutionFeeRaw,
      sideCapacityTone,
      sideCapacityValue,
      sizeNumber,
      slippageNumber,
    ]
  )

  const currentLifecycleStep = lifecycleStep(lifecycleState)
  const displayOrderId = orderId === undefined ? (enableLiveTrading ? '--' : ORDER_ID) : orderId.toString()
  const displayCommitTx = commitTxHash ?? (enableLiveTrading ? undefined : COMMIT_TX)
  const displayExecuteTx = executeTxHash ?? (enableLiveTrading ? undefined : EXECUTE_TX)
  const displayCommitTxValue = displayCommitTx ? <TxHashActions hash={displayCommitTx} /> : '--'
  const displayExecuteTxValue = displayExecuteTx ? <TxHashActions hash={displayExecuteTx} /> : '--'
  const finalExecutedNotionalUsdc = finalExecutionPrice
    ? sizeDeltaToNotionalUsdc(committedSizeDelta, finalExecutionPrice)
    : undefined
  const finalProtocolExecutionFee = executionFeeUsdcRaw(finalExecutedNotionalUsdc ?? notionalUsdc, executionFeeBpsRaw)
  const finalPriceDisplay = finalExecutionPrice
    ? formatPerpsPrice(finalExecutionPrice)
    : enableLiveTrading
      ? '--'
      : '0.9911'
  const reviewCtaLabel = enableLiveTrading && !isConnected
    ? 'Connect Wallet'
    : enableLiveTrading && !isCorrectChain
      ? 'Switch Network'
      : direction === 'long' ? 'Review Long' : 'Review Short'
  const isConnectWalletCta = enableLiveTrading && !isConnected
  const isSwitchNetworkCta = enableLiveTrading && isConnected && !isCorrectChain
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
  const isMarginActionInsufficient = marginActionLimit !== undefined && marginActionAmountRaw > marginActionLimit
  const isMarginActionInvalid = marginActionAmountRaw <= 0n || isMarginActionInsufficient
  const isMarginActionSubmitDisabled = isMarginActionPending
    || (enableLiveTrading && isConnected && isCorrectChain && isMarginActionInvalid)

  function openMarginAction(action: MarginAction) {
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
      switchChain({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID })
      return
    }
    if (isMarginActionInvalid) return

    try {
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
      onAccountRefresh?.()
    } catch (error) {
      setMarginActionStatus('failed')
      setMarginActionError(error instanceof Error ? error.message : `${marginActionLabel} failed. Check wallet and retry.`)
    }
  }

  async function handleConfirmCommit() {
    setFlowError(undefined)
    if (!enableLiveTrading) {
      setLifecycleState('commitPending')
      return
    }
    if (liveValidationError) {
      setFlowError(liveValidationError)
      return
    }

    try {
      setLifecycleState('commitPending')
      setPositionSnapshotAtCommit({
        exists: Boolean(currentPosition?.exists),
        side: currentPosition?.direction,
        size: currentPosition?.size ?? 0n,
      })
      const sizeDelta = resolvePerpsSizeDelta({
        isReducingCurrentPosition,
        currentPositionSize: currentPosition?.size,
        notionalUsdc,
        maxNotionalUsdc: maxNotionalFromLeverageRaw,
        oraclePrice: oraclePriceRaw ?? 0n,
      })
      setCommittedSizeDelta(sizeDelta)
      const result = await commitOrder({
        direction: effectiveOrderDirection,
        notionalUsdc,
        sizeDelta,
        marginUsdc,
        oraclePrice: oraclePriceRaw ?? 0n,
        slippagePercent: slippageNumber,
        isClose: isReducingCurrentPosition,
      })
      setCommitTxHash(result.hash)
      setOrderId(result.orderId)
      setLifecycleState('revealPending')
      onAccountRefresh?.()
    } catch (error) {
      setFlowError(error instanceof Error ? error.message : 'Commit transaction failed')
      setLifecycleState('failed')
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
      setLifecycleState('selfExecutePending')
      return
    }
    if (orderId === undefined) {
      setFlowError('Missing order ID from commit transaction.')
      setLifecycleState('selfExecuteFailed')
      return
    }

    try {
      setFlowError(undefined)
      setLifecycleState('selfExecutePending')
      const result = await executeOrder(orderId)
      setExecuteTxHash(result.hash)
      if (result.failedReason !== undefined) {
        setFlowError(getPerpsOrderFailureMessage(result.failedReason))
        setLifecycleState('selfExecuteFailed')
        return
      }
      setFinalExecutionPrice(result.executionPrice)
      setLifecycleState('executed')
      onAccountRefresh?.()
    } catch (error) {
      const message = error instanceof Error ? error.message : 'Self-execute transaction failed'
      setFlowError(message)
      setLifecycleState(isRetryableSelfExecuteMessage(message) ? 'selfExecuteAvailable' : 'selfExecuteFailed')
      onAccountRefresh?.()
    }
  }

  return (
    <section className="bg-cyber-surface-dark border border-cyber-border-glow/30 shadow-lg shadow-cyber-border-glow/10 overflow-hidden">
      <div className="space-y-5 px-5 py-4">
        <div>
          <div className="mb-2 text-xs font-medium uppercase text-cyber-text-secondary">Direction</div>
          <div className="grid grid-cols-2 border border-cyber-border-glow/30 bg-cyber-bg/50">
            {(['long', 'short'] as Direction[]).map((item) => (
              <button
                key={item}
                type="button"
                className={`px-3 py-3 text-sm font-semibold transition-colors ${
                  direction === item
                    ? item === 'long'
                      ? 'bg-cyber-neon-green text-cyber-bg'
                      : 'bg-cyber-electric-fuchsia text-cyber-bg'
                    : 'text-cyber-text-primary hover:bg-cyber-surface-light/60'
                }`}
                onClick={() => {
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
              if (canUseAvailableToTrade) setSize(availableToTradeDisplayAmount)
            }}
          />
          <AccountContextRow
            label="Current Position"
            value={<TokenAmount amount={currentPositionDisplayAmount} />}
            disabled={!canUseCurrentPosition}
            onClick={() => {
              if (canUseCurrentPosition) setSize(currentPositionDisplayAmount)
            }}
          />
        </div>

        <div>
          <Input
            label="Target notional"
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
              className="group cursor-pointer text-right text-xs font-semibold text-cyber-text-secondary transition-colors hover:text-cyber-text-primary focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-cyber-bright-blue"
              disabled={!canUseMaxNotional}
              onClick={() => {
                if (canUseMaxNotional) setSize(maxNotionalFromLeverageAmount)
              }}
            >
              <span>Max: </span>
              <span className="group-hover:underline group-focus-visible:underline">
                <TokenAmount amount={maxNotionalFromLeverageAmount} />
              </span>
            </button>
          </div>
        </div>

        <label className="flex cursor-pointer items-center gap-3 py-1">
          <input
            type="checkbox"
            checked={isReduceOnly}
            onChange={(event) => {
              setIsReduceOnly(event.target.checked)
            }}
            className="h-4 w-4 accent-cyber-bright-blue"
          />
          <span className="text-sm font-semibold text-cyber-text-primary">Reduce only</span>
        </label>

        <div>
          <div className="mb-2 flex items-center justify-between gap-3">
            <label className="text-sm font-medium text-cyber-text-secondary" htmlFor="perps-leverage">
              Leverage
            </label>
            <span className="text-lg font-semibold text-cyber-bright-blue">{formatLeverage(leverage)}</span>
          </div>
          <input
            id="perps-leverage"
            type="range"
            min="1"
            max="100"
            step="1"
            value={leverage}
            onChange={(event) => {
              setLeverage(Number(event.target.value))
            }}
            className="h-2 w-full cursor-pointer appearance-none bg-cyber-surface-light accent-cyber-bright-blue"
          />
          <div className="mt-2 flex items-center justify-between text-xs font-semibold text-cyber-text-secondary">
            <span>1x</span>
            <span>100x</span>
          </div>
        </div>

        <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
          <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Preview</div>
          <PreviewRows
            rows={previewRows.slice(0, 10)}
            onSlippageClick={() => {
              setIsSlippageConfigOpen((isOpen) => !isOpen)
            }}
            slippageConfig={
              isSlippageConfigOpen ? (
                <div className="mt-3 border-y border-cyber-border-glow/20 py-3">
                  <div className="grid grid-cols-4 gap-2">
                    {SLIPPAGE_OPTIONS.map((option) => (
                      <button
                        key={option.toString()}
                        type="button"
                        className={`border px-2 py-2 text-sm font-semibold transition-colors ${
                          slippage === option
                            ? 'border-cyber-bright-blue bg-cyber-bright-blue text-cyber-bg'
                            : 'border-cyber-border-glow/30 text-cyber-text-secondary hover:text-cyber-text-primary'
                        }`}
                        onClick={() => {
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

        <Button
          className={`w-full ${
            isConnectWalletCta
              ? '!bg-cyber-text-primary !text-cyber-bg hover:!bg-cyber-text-primary/90 !shadow-lg !shadow-cyber-text-primary/20'
              : isSwitchNetworkCta
                ? '!bg-cyber-bright-blue !text-cyber-bg hover:!bg-cyber-bright-blue/85 !shadow-lg !shadow-cyber-bright-blue/20'
                : ''
          }`}
          size="lg"
          variant={isConnectWalletCta || isSwitchNetworkCta ? 'secondary' : direction === 'short' ? 'danger' : 'primary'}
          disabled={enableLiveTrading && isConnected && isCorrectChain && isZeroSize}
          onClick={() => {
            if (enableLiveTrading && !isConnected) {
              void open()
              return
            }
            if (enableLiveTrading && !isCorrectChain) {
              switchChain({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID })
              return
            }
            setIsReviewOpen(true)
          }}
        >
          {isConnectWalletCta ? <span className="material-symbols-outlined text-xl">account_balance_wallet</span> : null}
          {reviewCtaLabel}
        </Button>

        <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
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
            <AccountSummaryRow label="Withdrawable" value={<TokenAmount amount={formatPerpsUsdc(withdrawableUsdcRaw)} />} />
          </div>
        </div>

        <div className="grid grid-cols-2 gap-3">
          <Button
            type="button"
            variant="secondary"
            className="w-full"
            onClick={() => {
              openMarginAction('deposit')
            }}
          >
            Deposit
          </Button>
          <Button
            type="button"
            variant="secondary"
            className="w-full"
            onClick={() => {
              openMarginAction('withdraw')
            }}
          >
            Withdraw
          </Button>
        </div>
      </div>

      <Modal
        isOpen={isReviewOpen}
        onClose={() => {
          setIsReviewOpen(false)
        }}
        headerContent={<OrderLifecycleSteps currentStep={currentLifecycleStep} />}
        showCloseButton={false}
        size="lg"
      >
        <div className="space-y-5">
          {lifecycleState === 'preview' ? (
            <>
              <p className="px-1 py-2 text-xl font-semibold leading-7 text-cyber-text-primary">
                {orderSummary}
              </p>

              <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Commit Preview</div>
                <PreviewRows rows={previewRows} />
              </div>

              <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
                <div className="text-sm font-semibold text-cyber-text-primary">Delayed execution</div>
                <div className="mt-2 text-sm text-cyber-text-secondary">
                  This submits a committed order. Execution settles after the reveal window using the accepted price constraints.
                </div>
              </div>

              {enableLiveTrading && liveValidationError ? (
                <div className="border border-cyber-electric-fuchsia/30 bg-cyber-electric-fuchsia/10 p-4 text-sm text-cyber-electric-fuchsia">
                  {liveValidationError}
                  {!isCorrectChain ? (
                    <Button
                      className="mt-3 w-full"
                      size="sm"
                      variant="secondary"
                      onClick={() => {
                        switchChain({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID })
                      }}
                    >
                      Switch Network
                    </Button>
                  ) : null}
                  {canCleanupOldestPendingOrder ? (
                    <Button
                      className="mt-3 w-full"
                      size="sm"
                      variant="secondary"
                      disabled={cleanupStatus === 'pending'}
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
              {flowError && lifecycleState === 'preview' ? (
                <div className="border border-cyber-electric-fuchsia/30 bg-cyber-electric-fuchsia/10 p-4 text-sm text-cyber-electric-fuchsia">
                  {flowError}
                </div>
              ) : null}

              <div className="flex gap-3">
                <Button
                  className="flex-1"
                  variant="secondary"
                  onClick={() => {
                    setIsReviewOpen(false)
                  }}
                >
                  Cancel
                </Button>
                <Button
                  className="flex-1"
                  variant={direction === 'short' ? 'danger' : 'primary'}
                  disabled={enableLiveTrading && Boolean(liveValidationError)}
                  onClick={() => {
                    void handleConfirmCommit()
                  }}
                >
                  Confirm Commit
                </Button>
              </div>
            </>
          ) : null}

          {lifecycleState === 'commitPending' ? (
            <>
              <PendingStateCard
                title="Waiting for wallet confirmation"
                description="Confirm the commit transaction in your wallet, then wait for it to be included onchain."
              />

              <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Commit Transaction</div>
                <PreviewRows
                  rows={[
                    { label: 'Direction', value: directionLabel(direction) },
                    { label: 'Target notional', value: formatUsdc(sizeNumber) },
                    { label: 'Slippage', value: formatPercent(slippageNumber) },
                    { label: 'Execution limit', value: formatOptionalPrice(executionLimit) },
                    { label: 'Estimated protocol execution fee', value: formatUsdcRaw(protocolExecutionFeeRaw) },
                    { label: 'Estimated keeper bounty', value: formatUsdc(keeperBounty) },
                  ]}
                />
              </div>

              {!enableLiveTrading ? (
                <div className="grid grid-cols-2 gap-3">
                  <Button
                    className="w-full"
                    variant="secondary"
                    onClick={() => {
                      setLifecycleState('failed')
                    }}
                  >
                    Transaction Failed
                  </Button>
                  <Button
                    className="w-full"
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
              <SuccessStateCard title="Commit confirmed" description="The order has entered the reveal queue." />
              <PreviewRows
                rows={[
                  { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                  { label: 'Commit tx', value: displayCommitTxValue },
                ]}
              />
              <Button
                className="w-full"
                onClick={() => {
                  setLifecycleState('revealPending')
                }}
              >
                Continue to Reveal
              </Button>
            </>
          ) : null}

          {lifecycleState === 'revealPending' ? (
            <>
              <PendingStateCard
                title="Waiting for keeper reveal"
                description="The keeper can now execute the committed order and settle the final contract price. Self-execute fetches historical Pyth data for the order window."
              />

              <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Reveal Queue</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                    { label: 'Commit tx', value: displayCommitTxValue },
                    { label: 'Acceptable price', value: formatOptionalPrice(executionLimit) },
                    { label: 'Estimated keeper bounty', value: formatUsdc(keeperBounty) },
                    { label: 'Self execute', value: enableLiveTrading ? 'Fetches historical Pyth data' : 'Available after 04:38' },
                  ]}
                />
              </div>

              <div className="grid grid-cols-2 gap-3">
                {!enableLiveTrading ? (
                  <>
                    <Button
                      className="w-full"
                      variant="secondary"
                      onClick={() => {
                        setLifecycleState('selfExecuteAvailable')
                      }}
                    >
                      Timeout Reached
                    </Button>
                    <Button
                      className="w-full"
                      onClick={() => {
                        setLifecycleState('executed')
                      }}
                    >
                      Keeper Executed
                    </Button>
                  </>
                ) : (
                  <Button
                    className="col-span-2 w-full"
                    onClick={() => {
                      void handleSelfExecute()
                    }}
                  >
                    Self Execute
                  </Button>
                )}
              </div>
            </>
          ) : null}

          {lifecycleState === 'selfExecuteAvailable' ? (
            <>
              <PendingStateCard
                title={
                  flowError && isHermesRateLimitMessage(flowError)
                    ? 'Hermes rate limit reached'
                    : flowError && isHistoricalPythRejectedMessage(flowError)
                      ? 'Historical Pyth data rejected'
                    : flowError && isRevealNotReadyMessage(flowError)
                      ? 'Reveal not ready yet'
                    : flowError && isPythExpiryMessage(flowError)
                      ? 'Historical Pyth data required'
                      : 'Keeper reveal overdue'
                }
                description={
                  flowError && isPythExpiryMessage(flowError)
                    ? flowError
                    : 'The keeper has not executed within the timeout. You can self execute the reveal transaction now.'
                }
              />

              <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Reveal Queue</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                    { label: 'Commit tx', value: displayCommitTxValue },
                    { label: 'Acceptable price', value: formatOptionalPrice(executionLimit) },
                    { label: 'Estimated keeper bounty', value: formatUsdc(keeperBounty) },
                    {
                      label: 'Self execute',
                      value: flowError && isRevealNotReadyMessage(flowError)
                        ? 'Retry shortly'
                        : flowError && isPythExpiryMessage(flowError)
                          ? 'Retry with historical Pyth data'
                          : 'Available now',
                      tone: flowError && isRetryableSelfExecuteMessage(flowError) ? 'warning' : 'positive',
                    },
                  ]}
                />
              </div>

              <Button
                className="w-full"
                size="lg"
                onClick={() => {
                  void handleSelfExecute()
                }}
              >
                {flowError && isRetryableSelfExecuteMessage(flowError) ? 'Retry Self Execute' : 'Self Execute'}
              </Button>
            </>
          ) : null}

          {lifecycleState === 'selfExecutePending' ? (
            <>
              <PendingStateCard
                title="Waiting for self-execute confirmation"
                description="Confirm promptly in your wallet. The order can expire if the reveal transaction takes too long to submit."
              />

              <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Self Execute Transaction</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                    { label: 'Commit tx', value: displayCommitTxValue },
                    { label: 'Acceptable price', value: formatOptionalPrice(executionLimit) },
                    { label: 'Estimated keeper bounty', value: formatUsdc(keeperBounty) },
                    { label: 'Transaction', value: 'Awaiting confirmation' },
                  ]}
                />
              </div>

              {!enableLiveTrading ? (
                <div className="grid grid-cols-2 gap-3">
                  <Button
                    className="w-full"
                    variant="secondary"
                    onClick={() => {
                      setLifecycleState('selfExecuteFailed')
                    }}
                  >
                    Transaction Failed
                  </Button>
                  <Button
                    className="w-full"
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
                    : flowError && isHistoricalPythRejectedMessage(flowError)
                      ? 'Historical Pyth data rejected'
                      : 'Self-execute transaction failed'
                }
                description={flowError ?? 'The wallet rejected the transaction or the reveal transaction failed before settling the order.'}
              />

              <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Reveal Queue</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                    { label: 'Commit tx', value: displayCommitTxValue },
                    { label: 'Acceptable price', value: formatOptionalPrice(executionLimit) },
                    { label: 'Self execute', value: 'Retry available', tone: 'warning' },
                  ]}
                />
              </div>

              <div className="flex gap-3">
                <Button
                  className="flex-1"
                  variant="secondary"
                  onClick={() => {
                    setLifecycleState('selfExecuteAvailable')
                  }}
                >
                  Back
                </Button>
                <Button
                  className="flex-1"
                  onClick={() => {
                    void handleSelfExecute()
                  }}
                >
                  Retry Self Execute
                </Button>
              </div>
            </>
          ) : null}

          {lifecycleState === 'executed' ? (
            <>
              <SuccessStateCard title="Trade executed" description="Execution settled onchain and the final price is confirmed." />
              <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Final Result</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={displayOrderId} /> },
                    { label: 'Direction', value: directionLabel(direction) },
                    { label: 'Final price', value: finalPriceDisplay },
                    { label: 'Target notional', value: formatUsdc(sizeNumber) },
                    { label: 'Execution notional', value: finalExecutedNotionalUsdc === undefined ? formatUsdc(sizeNumber) : formatUsdcRaw(finalExecutedNotionalUsdc) },
                    { label: 'Margin posted', value: formatUsdc(marginNumber) },
                    { label: 'Protocol execution fee', value: formatUsdcRaw(finalProtocolExecutionFee) },
                    { label: 'VPI / Price impact', value: 'Unavailable' },
                    { label: 'Keeper bounty', value: formatUsdc(keeperBounty) },
                    { label: 'Commit tx', value: displayCommitTxValue },
                    { label: 'Reveal tx', value: displayExecuteTxValue },
                  ]}
                />
              </div>
              <Button
                className="w-full"
                variant="secondary"
                onClick={() => {
                  setLifecycleState('preview')
                  setIsReviewOpen(false)
                }}
              >
                Done
              </Button>
            </>
          ) : null}

          {lifecycleState === 'failed' ? (
            <>
              <FailedStateCard
                title="Commit transaction failed"
                description={flowError ?? 'The wallet rejected the transaction or the commit failed before reaching the reveal queue.'}
              />
              <div className="flex gap-3">
                <Button
                  className="flex-1"
                  variant="secondary"
                  onClick={() => {
                    setLifecycleState('preview')
                  }}
                >
                  Back to Preview
                </Button>
                <Button
                  className="flex-1"
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
        isOpen={marginAction !== null}
        onClose={() => {
          if (!isMarginActionPending) {
            setMarginAction(null)
          }
        }}
        title={`${marginActionLabel} Margin`}
        size="md"
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

          <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
            <div className="space-y-2">
              <AccountSummaryRow label={marginActionLimitLabel} value={<TokenAmount amount={marginActionLimitDisplay} />} />
              <AccountSummaryRow label="Amount" value={<TokenAmount amount={formatPerpsUsdc(marginActionAmountRaw)} />} />
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
              disabled={isMarginActionPending}
              onClick={() => {
                setMarginAction(null)
              }}
            >
              Cancel
            </Button>
            <Button
              type="button"
              isLoading={isMarginActionPending}
              disabled={isMarginActionSubmitDisabled}
              onClick={() => {
                void handleMarginActionSubmit()
              }}
            >
              {marginActionCtaLabel}
            </Button>
          </div>
        </div>
      </Modal>
    </section>
  )
}
