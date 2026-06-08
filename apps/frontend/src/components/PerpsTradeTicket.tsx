import { type ReactNode, useMemo, useState } from 'react'
import { PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { getExplorerTxUrl } from '../utils/explorer'
import { Button, Input, Modal, TokenAmount, TokenLabel } from './ui'

type Direction = 'long' | 'short'
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
}

const PREVIEW_PRICE = 0.9909
const COST_OF_CARRY = '5.24%'
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
const CLOSE_BOUNTY_USDC = 0.2
const VPI_PRICE_IMPACT_USDC = 6.42

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

function formatVpi(value: number): ReactNode {
  if (value < 0) return <TokenAmount amount={`-${formatUsdcAmount(Math.abs(value))}`} />
  return <TokenAmount amount={formatUsdcAmount(value)} />
}

function vpiTone(value: number): PreviewRow['tone'] {
  return value < 0 ? 'positive' : 'default'
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

function clamp(value: number, min: number, max: number): number {
  return Math.min(Math.max(value, min), max)
}

function directionLabel(direction: Direction): string {
  return direction === 'long' ? 'Long DXY' : 'Short DXY'
}

function OrderSummaryAmount({ value }: { value: number }) {
  return <span className="whitespace-nowrap">{formatUsdcAmount(value)} USDC</span>
}

function truncateHash(hash: string): string {
  return `${hash.slice(0, 6)}...${hash.slice(-4)}`
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
  currentPositionSize,
  direction,
  isReduceOnly,
  leverage,
  size,
}: {
  currentPositionSide: Direction
  currentPositionSize: number
  direction: Direction
  isReduceOnly: boolean
  leverage: number
  size: number
}): ReactNode {
  const orderAmount = <OrderSummaryAmount value={size} />
  const selectedDirection = directionLabel(direction)
  const currentDirection = directionLabel(currentPositionSide)

  if (currentPositionSize <= 0) {
    if (isReduceOnly) {
      return <>You are submitting a reduce-only {selectedDirection} order with {orderAmount} notional.</>
    }
    return <>You are opening a {selectedDirection} position with {orderAmount} notional at up to {formatLeverage(leverage)} leverage.</>
  }

  if (isReduceOnly) {
    if (size >= currentPositionSize) return <>You are closing your {currentDirection} position.</>
    return <>You are reducing your {currentDirection} position by {orderAmount} notional to <OrderSummaryAmount value={currentPositionSize - size} />.</>
  }

  if (direction === currentPositionSide) {
    return <>You are increasing your {selectedDirection} position by {orderAmount} notional to <OrderSummaryAmount value={currentPositionSize + size} />.</>
  }

  if (size < currentPositionSize) {
    return <>You are reducing your {currentDirection} position by {orderAmount} notional to <OrderSummaryAmount value={currentPositionSize - size} />.</>
  }

  if (size === currentPositionSize) {
    return <>You are closing your {currentDirection} position.</>
  }

  return <>You are closing your {currentDirection} position and opening a {directionLabel(oppositeDirection(currentPositionSide))} position with <OrderSummaryAmount value={size - currentPositionSize} /> notional.</>
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
      <div className="mt-2 max-w-sm text-sm leading-6 text-cyber-text-secondary">{description}</div>
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
      <div className="mt-2 max-w-sm text-sm leading-6 text-cyber-text-secondary">{description}</div>
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
      <div className="mt-2 max-w-sm text-sm leading-6 text-cyber-text-secondary">{description}</div>
    </div>
  )
}

function AccountContextRow({
  label,
  value,
  valueTone = 'default',
  onClick,
}: {
  label: string
  value: ReactNode
  valueTone?: 'default' | 'positive'
  onClick: () => void
}) {
  const valueColor = valueTone === 'positive' ? 'text-cyber-neon-green' : 'text-cyber-text-primary'

  return (
    <button
      type="button"
      className="group flex w-full cursor-pointer items-center justify-between gap-3 text-left text-sm transition-colors hover:text-cyber-text-primary focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-cyber-bright-blue"
      onClick={onClick}
    >
      <span className="text-cyber-text-secondary">{label}</span>
      <span className={`text-right font-semibold group-hover:underline group-focus-visible:underline ${valueColor}`}>{value}</span>
    </button>
  )
}

export function PerpsTradeTicket({
  initialLifecycleState = 'preview',
  initialReviewOpen = false,
  initialDirection = 'long',
  initialSize = '5000',
  initialReduceOnly = false,
  currentPositionSide = 'long',
  currentPositionAmount = CURRENT_POSITION_AMOUNT,
}: PerpsTradeTicketProps) {
  const [direction, setDirection] = useState<Direction>(initialDirection)
  const [isReduceOnly, setIsReduceOnly] = useState(initialReduceOnly)
  const [size, setSize] = useState(initialSize)
  const [leverage, setLeverage] = useState(5)
  const [slippage, setSlippage] = useState(0.1)
  const [lifecycleState, setLifecycleState] = useState<TradeLifecycleState>(initialLifecycleState)
  const [isReviewOpen, setIsReviewOpen] = useState(initialReviewOpen)
  const [isSlippageConfigOpen, setIsSlippageConfigOpen] = useState(false)

  const sizeNumber = parseAmount(size)
  const currentPositionNumber = parseAmount(currentPositionAmount)
  const marginNumber = leverage > 0 ? sizeNumber / leverage : 0
  const protocolExecutionFee = (sizeNumber * EXECUTION_FEE_BPS) / 10_000
  const keeperBounty = isReduceOnly
    ? CLOSE_BOUNTY_USDC
    : clamp((sizeNumber * OPEN_BOUNTY_BPS) / 10_000, MIN_OPEN_BOUNTY_USDC, MAX_OPEN_BOUNTY_USDC)
  const slippageNumber = Math.max(slippage, 0)
  const executionLimit = Number.isFinite(slippageNumber)
    ? PREVIEW_PRICE * (direction === 'long' ? 1 + slippageNumber / 100 : 1 - slippageNumber / 100)
    : null
  const liquidationPrice = direction === 'long' ? PREVIEW_PRICE * 0.945 : PREVIEW_PRICE * 1.055
  const orderSummary = buildOrderSummary({
    currentPositionSide,
    currentPositionSize: currentPositionNumber,
    direction,
    isReduceOnly,
    leverage,
    size: sizeNumber,
  })

  const previewRows = useMemo<PreviewRow[]>(
    () => [
      { label: 'Oracle price', value: PREVIEW_PRICE.toFixed(4) },
      { label: 'Notional', value: formatUsdc(sizeNumber) },
      { label: 'Initial margin', value: formatUsdc(marginNumber) },
      { label: 'Leverage', value: formatLeverage(leverage) },
      { label: 'Slippage', value: formatPercent(slippageNumber) },
      { label: 'Execution limit', value: executionLimit === null ? 'Market' : executionLimit.toFixed(4) },
      { label: 'Liquidation price', value: liquidationPrice.toFixed(4) },
      { label: 'Protocol execution fee', value: formatUsdc(protocolExecutionFee) },
      {
        label: 'VPI / Price impact',
        value: formatVpi(VPI_PRICE_IMPACT_USDC),
        tone: vpiTone(VPI_PRICE_IMPACT_USDC),
      },
      { label: 'Keeper bounty', value: formatUsdc(keeperBounty) },
      { label: 'Cost of carry', value: COST_OF_CARRY },
      { label: 'Pool capacity', value: <TokenAmount amount="6.3M" />, tone: 'positive' },
      { label: 'Skew', value: '42% used' },
    ],
    [
      executionLimit,
      keeperBounty,
      leverage,
      liquidationPrice,
      marginNumber,
      protocolExecutionFee,
      sizeNumber,
      slippageNumber,
    ]
  )

  const currentLifecycleStep = lifecycleStep(lifecycleState)

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
            value={<TokenAmount amount={AVAILABLE_TO_TRADE_AMOUNT} />}
            onClick={() => {
              setSize(AVAILABLE_TO_TRADE_AMOUNT)
            }}
          />
          <AccountContextRow
            label="Current Position"
            value={<TokenAmount amount={currentPositionAmount} />}
            valueTone="positive"
            onClick={() => {
              setSize(currentPositionAmount)
            }}
          />
        </div>

        <div>
          <Input
            label="Size"
            value={size}
            onChange={(event) => {
              setSize(event.target.value)
            }}
            rightElement={<TokenLabel token="USDC" />}
          />
          <div className="mt-1.5 flex justify-end">
            <button
              type="button"
              className="group cursor-pointer text-right text-xs font-semibold text-cyber-text-secondary transition-colors hover:text-cyber-text-primary focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-cyber-bright-blue"
              onClick={() => {
                setSize(AVAILABLE_TO_TRADE_AMOUNT)
              }}
            >
              <span>Max: </span>
              <span className="group-hover:underline group-focus-visible:underline">
                <TokenAmount amount={AVAILABLE_TO_TRADE_AMOUNT} />
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
          className="w-full"
          size="lg"
          variant={direction === 'short' ? 'danger' : 'primary'}
          onClick={() => {
            setIsReviewOpen(true)
          }}
        >
          {direction === 'long' ? 'Review Long' : 'Review Short'}
        </Button>
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
                  onClick={() => {
                    setLifecycleState('commitPending')
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
                    { label: 'Size', value: formatUsdc(sizeNumber) },
                    { label: 'Slippage', value: formatPercent(slippageNumber) },
                    { label: 'Execution limit', value: executionLimit === null ? 'Market' : executionLimit.toFixed(4) },
                    { label: 'Protocol execution fee', value: formatUsdc(protocolExecutionFee) },
                    { label: 'Keeper bounty', value: formatUsdc(keeperBounty) },
                  ]}
                />
              </div>

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
            </>
          ) : null}

          {lifecycleState === 'commitConfirmed' ? (
            <>
              <SuccessStateCard title="Commit confirmed" description="The order has entered the reveal queue." />
              <PreviewRows
                rows={[
                  { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={ORDER_ID} /> },
                  { label: 'Commit tx', value: <TxHashActions hash={COMMIT_TX} /> },
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
                description="The keeper can now execute the committed order and settle the final contract price."
              />

              <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Reveal Queue</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={ORDER_ID} /> },
                    { label: 'Commit tx', value: <TxHashActions hash={COMMIT_TX} /> },
                    { label: 'Acceptable price', value: executionLimit === null ? 'Market' : executionLimit.toFixed(4) },
                    { label: 'Keeper bounty', value: formatUsdc(keeperBounty) },
                    { label: 'Self execute', value: 'Available after 04:38' },
                  ]}
                />
              </div>

              <div className="grid grid-cols-2 gap-3">
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
              </div>
            </>
          ) : null}

          {lifecycleState === 'selfExecuteAvailable' ? (
            <>
              <PendingStateCard
                title="Keeper reveal overdue"
                description="The keeper has not executed within the timeout. You can self execute the reveal transaction now."
              />

              <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Reveal Queue</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={ORDER_ID} /> },
                    { label: 'Commit tx', value: <TxHashActions hash={COMMIT_TX} /> },
                    { label: 'Acceptable price', value: executionLimit === null ? 'Market' : executionLimit.toFixed(4) },
                    { label: 'Keeper bounty', value: formatUsdc(keeperBounty) },
                    { label: 'Self execute', value: 'Available now', tone: 'positive' },
                  ]}
                />
              </div>

              <Button
                className="w-full"
                size="lg"
                onClick={() => {
                  setLifecycleState('selfExecutePending')
                }}
              >
                Self Execute
              </Button>
            </>
          ) : null}

          {lifecycleState === 'selfExecutePending' ? (
            <>
              <PendingStateCard
                title="Waiting for self-execute confirmation"
                description="Confirm the reveal transaction in your wallet, then wait for it to settle the order onchain."
              />

              <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Self Execute Transaction</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={ORDER_ID} /> },
                    { label: 'Commit tx', value: <TxHashActions hash={COMMIT_TX} /> },
                    { label: 'Acceptable price', value: executionLimit === null ? 'Market' : executionLimit.toFixed(4) },
                    { label: 'Keeper bounty', value: formatUsdc(keeperBounty) },
                    { label: 'Transaction', value: 'Awaiting confirmation' },
                  ]}
                />
              </div>

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
            </>
          ) : null}

          {lifecycleState === 'selfExecuteFailed' ? (
            <>
              <FailedStateCard
                title="Self-execute transaction failed"
                description="The wallet rejected the transaction or the reveal transaction failed before settling the order."
              />

              <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
                <div className="mb-3 text-xs font-medium uppercase text-cyber-text-secondary">Reveal Queue</div>
                <PreviewRows
                  rows={[
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={ORDER_ID} /> },
                    { label: 'Commit tx', value: <TxHashActions hash={COMMIT_TX} /> },
                    { label: 'Acceptable price', value: executionLimit === null ? 'Market' : executionLimit.toFixed(4) },
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
                    setLifecycleState('selfExecutePending')
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
                    { label: 'Order ID', value: <CopyableValue ariaLabel="Copy order ID" value={ORDER_ID} /> },
                    { label: 'Direction', value: directionLabel(direction) },
                    { label: 'Final price', value: '0.9911' },
                    { label: 'Position size', value: formatUsdc(sizeNumber) },
                    { label: 'Margin used', value: formatUsdc(marginNumber) },
                    { label: 'Protocol execution fee', value: formatUsdc(protocolExecutionFee) },
                    {
                      label: 'VPI / Price impact',
                      value: formatVpi(VPI_PRICE_IMPACT_USDC),
                      tone: vpiTone(VPI_PRICE_IMPACT_USDC),
                    },
                    { label: 'Keeper bounty', value: formatUsdc(keeperBounty) },
                    { label: 'Commit tx', value: <TxHashActions hash={COMMIT_TX} /> },
                    { label: 'Reveal tx', value: <TxHashActions hash={EXECUTE_TX} /> },
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
                description="The wallet rejected the transaction or the commit failed before reaching the reveal queue."
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
                    setLifecycleState('commitPending')
                  }}
                >
                  Retry Commit
                </Button>
              </div>
            </>
          ) : null}
        </div>
      </Modal>
    </section>
  )
}
