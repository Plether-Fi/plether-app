import { type ReactNode, useMemo, useState } from 'react'
import { Button, Input, Modal, TokenAmount, TokenLabel } from './ui'

type Direction = 'long' | 'short'
export type TradeTicketStatus = 'compose' | 'queued' | 'executed'

interface PreviewRow {
  label: string
  value: ReactNode
  tone?: 'default' | 'positive' | 'warning'
}

interface PerpsTradeTicketProps {
  initialStatus?: TradeTicketStatus
}

const PREVIEW_PRICE = 0.9909
const COST_OF_CARRY = '5.24%'
const AVAILABLE_TO_TRADE_AMOUNT = '18 420'
const CURRENT_POSITION_AMOUNT = '8 200'
const ORDER_ID = '0x7f21...9c04'
const COMMIT_TX = '0x4a6b...88e2'
const EXECUTE_TX = '0xa91d...34bf'
const SLIPPAGE_OPTIONS = [0.05, 0.1, 0.25, Infinity]
const EXECUTION_FEE_BPS = 4
const OPEN_BOUNTY_BPS = 1
const MIN_OPEN_BOUNTY_USDC = 0.01
const MAX_OPEN_BOUNTY_USDC = 0.2
const CLOSE_BOUNTY_USDC = 0.2
const VPI_PRICE_IMPACT_USDC = 6.42

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

function actionLabel(isReduceOnly: boolean): string {
  return isReduceOnly ? 'Close / Reduce' : 'Open / Increase'
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

function StatusChip({ label, value, tone = 'positive' }: { label: string; value: string; tone?: 'positive' | 'warning' }) {
  return (
    <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 px-3 py-2">
      <div className="text-xs text-cyber-text-secondary">{label}</div>
      <div className={`mt-1 text-sm font-semibold ${tone === 'positive' ? 'text-cyber-neon-green' : 'text-yellow-300'}`}>
        {value}
      </div>
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

export function PerpsTradeTicket({ initialStatus = 'compose' }: PerpsTradeTicketProps) {
  const [direction, setDirection] = useState<Direction>('long')
  const [isReduceOnly, setIsReduceOnly] = useState(false)
  const [size, setSize] = useState('5000')
  const [leverage, setLeverage] = useState(5)
  const [slippage, setSlippage] = useState(0.1)
  const [status, setStatus] = useState<TradeTicketStatus>(initialStatus)
  const [isReviewOpen, setIsReviewOpen] = useState(false)
  const [isSlippageConfigOpen, setIsSlippageConfigOpen] = useState(false)

  const sizeNumber = parseAmount(size)
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
      { label: 'Oracle freshness', value: 'Fresh', tone: 'positive' },
      { label: 'Protocol status', value: 'Active', tone: 'positive' },
    ],
    [executionLimit, keeperBounty, leverage, liquidationPrice, marginNumber, protocolExecutionFee, sizeNumber, slippageNumber]
  )

  const reviewTitle = `Review ${direction === 'long' ? 'Long' : 'Short'}`

  if (status === 'queued') {
    return (
      <section className="bg-cyber-surface-dark border border-cyber-border-glow/30 shadow-lg shadow-cyber-border-glow/10 overflow-hidden">
        <div className="border-b border-cyber-border-glow/20 px-5 py-4">
          <h2 className="text-lg font-semibold text-cyber-text-primary">Order Queued</h2>
          <p className="mt-1 text-sm text-cyber-text-secondary">{ORDER_ID}</p>
        </div>
        <div className="space-y-4 px-5 py-4">
          <div className="flex items-center gap-3 border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
            <div className="relative h-9 w-9 shrink-0">
              <div className="absolute inset-0 rounded-full border-2 border-cyber-bright-blue/25 border-t-cyber-bright-blue animate-spin" />
            </div>
            <div>
              <div className="text-sm font-semibold text-cyber-text-primary">Waiting for execution</div>
              <div className="mt-1 text-xs text-cyber-text-secondary">Reveal window active · expires in 04:38</div>
            </div>
          </div>
          <PreviewRows
            rows={[
              { label: 'Direction', value: directionLabel(direction) },
              { label: 'Size', value: formatUsdc(sizeNumber) },
              { label: 'Slippage', value: formatPercent(slippageNumber) },
              { label: 'Execution limit', value: executionLimit === null ? 'Market' : executionLimit.toFixed(4) },
              { label: 'Protocol execution fee', value: formatUsdc(protocolExecutionFee) },
              {
                label: 'VPI / Price impact',
                value: formatVpi(VPI_PRICE_IMPACT_USDC),
                tone: vpiTone(VPI_PRICE_IMPACT_USDC),
              },
              { label: 'Keeper bounty', value: formatUsdc(keeperBounty) },
              { label: 'Commit tx', value: COMMIT_TX },
            ]}
          />
          <Button
            className="w-full"
            size="lg"
            onClick={() => {
              setStatus('executed')
            }}
          >
            Execute Trade
          </Button>
        </div>
      </section>
    )
  }

  if (status === 'executed') {
    return (
      <section className="bg-cyber-surface-dark border border-cyber-border-glow/30 shadow-lg shadow-cyber-border-glow/10 overflow-hidden">
        <div className="border-b border-cyber-border-glow/20 px-5 py-4">
          <h2 className="text-lg font-semibold text-cyber-text-primary">Trade Executed</h2>
          <p className="mt-1 text-sm text-cyber-text-secondary">{ORDER_ID}</p>
        </div>
        <div className="space-y-4 px-5 py-4">
          <div className="border border-cyber-neon-green/30 bg-cyber-neon-green/10 p-4">
            <div className="text-sm font-semibold text-cyber-neon-green">Position opened</div>
            <div className="mt-1 text-xs text-cyber-text-secondary">Execution settled onchain</div>
          </div>
          <PreviewRows
            rows={[
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
              { label: 'Execute tx', value: EXECUTE_TX },
            ]}
          />
          <Button
            className="w-full"
            variant="secondary"
            onClick={() => {
              setStatus('compose')
            }}
          >
            New Trade
          </Button>
        </div>
      </section>
    )
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
                    : item === 'long'
                      ? 'text-cyber-neon-green hover:bg-cyber-neon-green/10'
                      : 'text-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/10'
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
            value={<TokenAmount amount={CURRENT_POSITION_AMOUNT} />}
            valueTone="positive"
            onClick={() => {
              setSize(CURRENT_POSITION_AMOUNT)
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

        <label className="flex cursor-pointer items-center gap-3 border border-cyber-border-glow/30 bg-cyber-bg/50 px-4 py-3 transition-colors hover:border-cyber-bright-blue/50">
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

        <div className="grid grid-cols-2 gap-2">
          <StatusChip label="Oracle" value="Fresh" />
          <StatusChip label="Protocol" value="Active" />
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
        title={reviewTitle}
        size="lg"
      >
        <div className="space-y-5">
          <div className="grid grid-cols-2 gap-2">
            <StatusChip label="Direction" value={directionLabel(direction)} />
            <StatusChip label="Action" value={actionLabel(isReduceOnly)} />
          </div>

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
              onClick={() => {
                setIsReviewOpen(false)
                setStatus('queued')
              }}
            >
              Commit Trade
            </Button>
          </div>
        </div>
      </Modal>
    </section>
  )
}
