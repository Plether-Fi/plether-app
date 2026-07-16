import { type ReactNode, useEffect, useState } from 'react'
import type { PerpsOrderHistoryRow, PerpsPendingOrder, PerpsPosition, PerpsTradeHistoryRow } from '../hooks'
import { usePerpsTrading } from '../hooks'
import { PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { getExplorerTxUrl } from '../utils/explorer'
import { formatDisplayDxyPrice, formatPerpsNumber, formatPerpsUsdc, formatSignedPerpsUsdc, oraclePriceToDisplayDxyPrice, parsePerpsUsdc, perpsSideLabel } from '../utils/perps'
import { Button, Input, Modal, TokenAmount, TokenLabel, Tooltip } from './ui'

type PerpsAccountTab = 'position' | 'openOrders' | 'orderHistory' | 'tradeHistory'

interface AccountTab {
  id: PerpsAccountTab
  label: string
}

interface PositionRow {
  market: string
  side: string
  size: ReactNode
  entryNotional: ReactNode
  entry: string
  leverage: string
  liquidationPrice: ReactNode
  pnl: ReactNode
  costOfCarryUsdc: ReactNode
  tone?: 'positive' | 'negative'
}

interface OrderRow {
  orderId?: bigint
  time?: string
  market: string
  side: string
  type: string
  price: string
  size: ReactNode
  status?: string
  expiryTime?: bigint
  commitTxHash?: string
  revealTxHash?: string
}

interface TradeRow {
  time: string
  market: string
  side: string
  price: string
  size: ReactNode
  pnl?: ReactNode
  txHash?: string
}

interface PerpsAccountPanelProps {
  position?: PerpsPosition
  pendingOrders?: PerpsPendingOrder[]
  orderHistory?: PerpsOrderHistoryRow[]
  tradeHistory?: PerpsTradeHistoryRow[]
  equityUsdc?: bigint
  freeBuyingPowerUsdc?: bigint
  isConnected?: boolean
  isLoading?: boolean
  isHistoryLoading?: boolean
  historyError?: Error
  onAccountRefresh?: () => void
}

const ACCOUNT_TABS: AccountTab[] = [
  { id: 'position', label: 'Position' },
  { id: 'openOrders', label: 'Open Orders' },
  { id: 'orderHistory', label: 'Order History' },
  { id: 'tradeHistory', label: 'Transaction History' },
]

const LIGHT_ORANGE_ACTION_BUTTON_CLASS = '!border-[#FFAB96] !bg-[#FFAB96] !text-[#250917] enabled:hover:!border-[#FF572D] enabled:hover:!bg-[#FF572D] enabled:hover:!text-[#FFF5F9] enabled:hover:underline enabled:hover:underline-offset-4'
const DARK_CANCEL_BUTTON_CLASS = '!border-[#FFAB96]/40 !bg-[#250917] !text-[#FFF5F9] enabled:hover:!border-[#FFAB96] enabled:hover:!bg-[#3B212D] enabled:hover:underline enabled:hover:underline-offset-4'

const OPEN_ORDERS: OrderRow[] = [
  { market: 'plDXY Perp', side: 'Buy', type: 'Limit', price: '0.9880', size: <TokenAmount amount="1 500" /> },
  { market: 'plDXY Perp', side: 'Sell', type: 'Take profit', price: '1.0040', size: <TokenAmount amount="3 000" /> },
]

const ORDER_HISTORY: OrderRow[] = [
  {
    orderId: 101n,
    time: '12:42',
    market: 'plDXY Perp',
    side: 'Long',
    type: 'Open',
    price: '0.9850',
    size: <TokenAmount amount="2 500" />,
    status: 'Executed',
    commitTxHash: '0x0000000000000000000000000000000000000000000000000000000000000101',
    revealTxHash: '0x0000000000000000000000000000000000000000000000000000000000000201',
  },
  {
    orderId: 100n,
    time: '11:08',
    market: 'plDXY Perp',
    side: 'Short',
    type: 'Close',
    price: '0.9790',
    size: <TokenAmount amount="1 200" />,
    status: 'Expired',
    commitTxHash: '0x0000000000000000000000000000000000000000000000000000000000000100',
  },
]

const TRADE_HISTORY: TradeRow[] = [
  { time: '12:42', market: 'plDXY Perp', side: 'Open Long', price: '0.9912', size: <TokenAmount amount="1 000" /> },
  { time: '11:08', market: 'plDXY Perp', side: 'Close Long', price: '0.9931', size: <TokenAmount amount="650" />, pnl: <TokenAmount amount="+12.2" /> },
]

function pnlToneClass(tone: PositionRow['tone']): string {
  if (tone === 'positive') return 'text-positive'
  if (tone === 'negative') return 'text-brand-orange'
  return 'text-content-primary'
}

function positionSideBadgeClass(direction: PerpsPosition['direction']): string {
  return direction === 'long'
    ? 'border-positive/40 text-positive'
    : 'border-brand-orange/40 text-brand-orange'
}

function formatLiquidationDistance(currentPrice?: bigint, liquidationPrice?: bigint): string | undefined {
  const displayLiquidationPrice = oraclePriceToDisplayDxyPrice(liquidationPrice)
  if (
    currentPrice === undefined ||
    displayLiquidationPrice === undefined ||
    currentPrice <= 0n
  ) {
    return undefined
  }

  const distance = currentPrice > displayLiquidationPrice
    ? currentPrice - displayLiquidationPrice
    : displayLiquidationPrice - currentPrice
  const sign = displayLiquidationPrice >= currentPrice ? '+' : '-'
  const distanceBps = (distance * 10_000n) / currentPrice
  const whole = distanceBps / 100n
  const decimals = distanceBps % 100n

  return `${sign}${whole.toString()}.${decimals.toString().padStart(2, '0')}% away`
}

function formatPositionLeverage(position: PerpsPosition): string {
  return formatPositionLeverageForMargin(position, position.marginUsdc)
}

function formatPositionLeverageForMargin(position: PerpsPosition, marginUsdc: bigint): string {
  if (marginUsdc <= 0n) return '--'

  const notionalUsdc = position.estimatedNotionalUsdc ?? position.entryNotionalUsdc
  if (notionalUsdc === undefined) return '--'

  return `${formatPerpsNumber(Number(notionalUsdc) / Number(marginUsdc), 2)}x`
}

function formatEffectiveAccountLeverage(position: PerpsPosition, equityUsdc?: bigint): string {
  if (equityUsdc === undefined || equityUsdc <= 0n) return '--'

  const notionalUsdc = position.estimatedNotionalUsdc ?? position.entryNotionalUsdc
  if (notionalUsdc === undefined) return '--'

  return `${formatPerpsNumber(Number(notionalUsdc) / Number(equityUsdc), 2)}x`
}

function LiquidationPriceValue({
  currentPrice,
  liquidationPrice,
}: {
  currentPrice?: bigint
  liquidationPrice?: bigint
}) {
  if (liquidationPrice === undefined) {
    return (
      <span className="text-base font-medium text-content-secondary">
        Not in range
      </span>
    )
  }

  const distance = formatLiquidationDistance(currentPrice, liquidationPrice)

  return (
    <span className="inline-flex flex-col items-start gap-1">
      <span>{formatDisplayDxyPrice(liquidationPrice)}</span>
      {distance ? (
        <span className="text-xs font-medium text-content-secondary">{distance}</span>
      ) : null}
    </span>
  )
}

function EmptyState({ label }: { label: string }) {
  return (
    <div className="flex min-h-[150px] items-center justify-center border border-brand-border/20 bg-app-bg">
      <span className="text-sm text-content-secondary">No {label.toLowerCase()}</span>
    </div>
  )
}

function LoadingState({ label }: { label: string }) {
  return (
    <div className="flex min-h-[150px] items-center justify-center border border-brand-border/20 bg-app-bg">
      <span className="text-sm text-content-secondary">Loading {label.toLowerCase()}...</span>
    </div>
  )
}

function ErrorState({ message }: { message: string }) {
  return (
    <div className="flex min-h-[150px] items-center justify-center border border-brand-orange/30 bg-brand-orange/10 p-4">
      <span className="text-sm text-brand-orange">{message}</span>
    </div>
  )
}

function isPositionMarginInput(value: string): boolean {
  return /^\d*(?:[.,]\d{0,6})?$/.test(value.replaceAll(' ', ''))
}

function AccountSummaryRow({ label, value }: { label: string; value: ReactNode }) {
  return (
    <div className="flex items-center justify-between gap-4 text-sm">
      <dt className="text-content-secondary">{label}</dt>
      <dd className="text-right font-semibold text-content-primary">{value}</dd>
    </div>
  )
}

function AccountMetric({
  label,
  value,
  tone,
  tooltip,
  action,
}: {
  label: string
  value: ReactNode
  tone?: PositionRow['tone']
  tooltip?: ReactNode
  action?: ReactNode
}) {
  return (
    <div className="min-w-0">
      <div className="flex min-h-5 items-center gap-1.5 text-xs font-medium uppercase text-content-secondary">
        <span>{label}</span>
        {tooltip ? (
          <Tooltip
            content={tooltip}
            position="left"
            className="w-[420px] max-w-[calc(100vw-2rem)] whitespace-normal p-4 text-left leading-5"
          >
            <span
              className="inline-flex h-3.5 w-3.5 shrink-0 items-center justify-center rounded-full border border-current text-[9px] font-semibold leading-none text-content-secondary/80 transition-colors hover:text-[#FFAB96]"
              aria-label={`${label} details`}
              tabIndex={0}
            >
              i
            </span>
          </Tooltip>
        ) : null}
      </div>
      <div className="mt-2 flex items-center gap-2">
        <div className={`text-xl font-semibold ${pnlToneClass(tone)}`}>{value}</div>
        {action}
      </div>
    </div>
  )
}

function formatDuration(seconds: number): string {
  if (!Number.isFinite(seconds) || seconds <= 0) return 'now'

  const minutes = Math.floor(seconds / 60)
  const remainingSeconds = seconds % 60
  if (minutes <= 0) return `${remainingSeconds.toString()}s`

  const hours = Math.floor(minutes / 60)
  const remainingMinutes = minutes % 60
  if (hours <= 0) return `${minutes.toString()}m ${remainingSeconds.toString()}s`

  return `${hours.toString()}h ${remainingMinutes.toString()}m`
}

function OpenOrderStatus({ secondsToExpiry }: { secondsToExpiry?: number }) {
  if (secondsToExpiry === undefined) {
    return (
      <div>
        <div className="font-semibold text-content-primary">Pending</div>
        <div className="mt-1 text-xs text-content-secondary">Waiting for reveal</div>
      </div>
    )
  }

  if (secondsToExpiry <= 0) {
    return (
      <div>
        <div className="font-semibold text-brand-orange">Expired</div>
        <div className="mt-1 text-xs text-content-secondary">Clean up to release reserved margin</div>
      </div>
    )
  }

  return (
    <div>
      <div className="font-semibold text-content-primary">Pending reveal</div>
      <div className="mt-1 text-xs text-content-secondary">
        Expires in {formatDuration(secondsToExpiry)}
      </div>
    </div>
  )
}

function TxLink({ hash }: { hash?: string }) {
  if (!hash) return <span className="text-content-secondary">--</span>

  return (
    <a
      aria-label="Open transaction in block explorer"
      title="Open in block explorer"
      href={getExplorerTxUrl(PERPS_ARBITRUM_SEPOLIA_CHAIN_ID, hash)}
      target="_blank"
      rel="noopener noreferrer"
      className="inline-flex h-5 w-5 items-center justify-center text-content-secondary transition-colors hover:text-[#FFAB96]"
    >
      <span className="material-symbols-outlined !text-[16px] !leading-none">open_in_new</span>
    </a>
  )
}

function PositionView({
  position,
  equityUsdc,
  freeBuyingPowerUsdc,
  isConnected,
  isLoading,
  onAccountRefresh,
}: {
  position?: PerpsPosition
  equityUsdc?: bigint
  freeBuyingPowerUsdc?: bigint
  isConnected?: boolean
  isLoading?: boolean
  onAccountRefresh?: () => void
}) {
  const { addPositionMargin } = usePerpsTrading()
  const [isPositionMarginModalOpen, setIsPositionMarginModalOpen] = useState(false)
  const [positionMarginAmount, setPositionMarginAmount] = useState('')
  const [positionMarginStatus, setPositionMarginStatus] = useState<'idle' | 'pending' | 'failed'>('idle')
  const [positionMarginError, setPositionMarginError] = useState<string | undefined>()

  if (isConnected === false) return <EmptyState label="connected wallet" />
  if (isLoading) return <LoadingState label="position data" />
  if (!position?.exists) return <EmptyState label="current position" />

  const positionMarginAmountRaw = parsePerpsUsdc(positionMarginAmount)
  const positionMarginLimitRaw = freeBuyingPowerUsdc ?? 0n
  const isPositionMarginTooHigh = positionMarginAmountRaw > positionMarginLimitRaw
  const resultingPositionMargin = position.marginUsdc + positionMarginAmountRaw
  const canSubmitPositionMargin =
    positionMarginAmountRaw > 0n &&
    !isPositionMarginTooHigh &&
    positionMarginStatus !== 'pending'

  function handleClosePositionMarginModal() {
    if (positionMarginStatus === 'pending') return
    setIsPositionMarginModalOpen(false)
    setPositionMarginAmount('')
    setPositionMarginError(undefined)
    setPositionMarginStatus('idle')
  }

  async function handleAddPositionMargin() {
    if (!canSubmitPositionMargin) return

    setPositionMarginStatus('pending')
    setPositionMarginError(undefined)
    try {
      await addPositionMargin(positionMarginAmountRaw)
      onAccountRefresh?.()
      setIsPositionMarginModalOpen(false)
      setPositionMarginAmount('')
      setPositionMarginStatus('idle')
    } catch (error) {
      setPositionMarginStatus('failed')
      setPositionMarginError(error instanceof Error ? error.message : 'Add position margin failed')
    }
  }

  const currentPnl = position.unrealizedPnlUsdc
  const pendingCarryTooltip = (
    <span>
      Pending carry is unpaid carry accrued since the last position checkpoint. It is a position liability:
      it reduces equity, can consume free balance or margin, reduces close payout, and can push the
      position toward liquidation.
    </span>
  )
  const liquidationTooltip = (
    <span>
      Liquidation is based on account equity versus maintenance margin, not isolated position margin alone.
      <br />
      <br />
      <strong>Not in range</strong> means this account is not liquidatable anywhere inside the protocol&apos;s
      bounded oracle price range, so there is no single liquidation threshold to show right now.
    </span>
  )
  const effectiveAccountLeverage = formatEffectiveAccountLeverage(position, equityUsdc)
  const leverageTooltip = (
    <span>
      Position leverage is current contract notional divided by the margin assigned to this position.
      <br />
      <br />
      Effective account leverage includes free USDC through account equity: <strong>{effectiveAccountLeverage}</strong>.
    </span>
  )
  const entryNotionalTooltip = (
    <span>
      Entry notional is the executed order size recorded at entry. It does not move with price; current plDXY Perp
      exposure does.
    </span>
  )
  const unrealizedPnlTooltip = (
    <span>
      Price PnL from entry to current mark. It is before execution fees, VPI / price impact, and pending carry.
    </span>
  )
  const currentPosition: PositionRow = {
    market: 'plDXY Perp',
    side: perpsSideLabel(position.side),
    size: <TokenAmount amount={formatPerpsUsdc(position.dxyExposureUsdc ?? position.estimatedNotionalUsdc)} />,
    entryNotional: <TokenAmount amount={formatPerpsUsdc(position.entryNotionalUsdc)} />,
    entry: formatDisplayDxyPrice(position.entryPrice),
    leverage: formatPositionLeverage(position),
    liquidationPrice: (
      <LiquidationPriceValue
        currentPrice={position.displayDxyPrice}
        liquidationPrice={position.liquidationPrice}
      />
    ),
    pnl: <TokenAmount amount={formatSignedPerpsUsdc(currentPnl)} />,
    costOfCarryUsdc: <TokenAmount amount={formatPerpsUsdc(position.pendingCarryUsdc)} />,
    tone: currentPnl < 0n ? 'negative' : currentPnl > 0n ? 'positive' : undefined,
  }
  const editPositionMarginAction = (
    <button
      type="button"
      aria-label="Edit position margin"
      title="Edit position margin"
      className="inline-flex h-7 w-7 shrink-0 cursor-pointer items-center justify-center border border-brand-border/30 bg-app-bg text-content-secondary transition-colors hover:border-[#FFAB96] hover:text-[#FFAB96]"
      onClick={() => {
        setPositionMarginError(undefined)
        setPositionMarginStatus('idle')
        setIsPositionMarginModalOpen(true)
      }}
    >
      <span className="material-symbols-outlined !text-[16px] !leading-none">edit</span>
    </button>
  )

  return (
    <div className="border border-brand-border/20 bg-app-bg p-4">
      <div className="mb-4">
        <div className="text-xs font-medium uppercase text-content-secondary">Current Position</div>
        <div className="mt-2 flex items-center gap-3">
          <span className={`border px-3 py-1 text-sm font-semibold ${positionSideBadgeClass(position.direction)}`}>
            {currentPosition.side}
          </span>
          <div className="mt-1 text-lg font-semibold text-content-primary">{currentPosition.market}</div>
        </div>
      </div>
      <div className="grid grid-cols-2 gap-4 md:grid-cols-3 xl:grid-cols-7">
        <AccountMetric label="plDXY Perp exposure" value={currentPosition.size} />
        <AccountMetric label="Entry notional" value={currentPosition.entryNotional} tooltip={entryNotionalTooltip} />
        <AccountMetric label="Entry price" value={currentPosition.entry} />
        <AccountMetric
          label="Leverage"
          value={currentPosition.leverage}
          tooltip={leverageTooltip}
          action={editPositionMarginAction}
        />
        <AccountMetric
          label="Liquidation price"
          value={currentPosition.liquidationPrice}
          tooltip={liquidationTooltip}
        />
        <AccountMetric
          label="Unrealized PnL"
          value={currentPosition.pnl}
          tone={currentPosition.tone}
          tooltip={unrealizedPnlTooltip}
        />
        <AccountMetric
          label="Cost of carry"
          value={currentPosition.costOfCarryUsdc}
          tooltip={pendingCarryTooltip}
        />
      </div>
      <p className="mt-4 border-t border-brand-border/20 pt-3 text-sm leading-5 text-content-secondary">
        <span>Entry notional is the executed order size. plDXY Perp exposure is current displayed exposure.</span>
        {' '}
        <span>This is a shared-collateral account, so free margin outside the position can still protect it from liquidation.</span>
      </p>
      <Modal
        isOpen={isPositionMarginModalOpen}
        onClose={handleClosePositionMarginModal}
        title="Edit Position Margin"
        size="md"
      >
        <div className="space-y-5">
          <p className="text-sm leading-5 text-content-secondary">
            This locks free USDC into the current position margin bucket. It does not change position size.
          </p>

          <div className="border border-brand-border/20 bg-app-bg p-4 text-sm leading-5 text-content-secondary">
            Direct margin removal is not supported. Reducing or closing the position
            releases position margin proportionally.
          </div>

          <Input
            label="Add margin"
            inputMode="decimal"
            value={positionMarginAmount}
            placeholder="0"
            rightElement={<TokenLabel token="USDC" />}
            error={isPositionMarginTooHigh ? 'Amount exceeds available free margin.' : undefined}
            onChange={(event) => {
              const nextValue = event.target.value
              if (!isPositionMarginInput(nextValue)) return
              setPositionMarginAmount(nextValue)
              setPositionMarginError(undefined)
              if (positionMarginStatus === 'failed') setPositionMarginStatus('idle')
            }}
          />

          <div className="-mt-3 flex justify-end">
            <button
              type="button"
              className="cursor-pointer text-xs font-semibold text-content-secondary transition-colors hover:text-[#FFAB96] hover:underline hover:underline-offset-4 disabled:cursor-not-allowed disabled:opacity-50 disabled:hover:no-underline"
              disabled={positionMarginLimitRaw <= 0n || positionMarginStatus === 'pending'}
              onClick={() => {
                setPositionMarginAmount(formatPerpsUsdc(positionMarginLimitRaw, 6))
                setPositionMarginError(undefined)
              }}
            >
              Max: {formatPerpsUsdc(positionMarginLimitRaw, 2)} USDC
            </button>
          </div>

          <dl className="space-y-2 border border-brand-border/20 bg-app-bg p-4">
            <AccountSummaryRow label="Free margin" value={<TokenAmount amount={formatPerpsUsdc(positionMarginLimitRaw)} />} />
            <AccountSummaryRow label="Current position margin" value={<TokenAmount amount={formatPerpsUsdc(position.marginUsdc)} />} />
            <AccountSummaryRow label="Resulting position margin" value={<TokenAmount amount={formatPerpsUsdc(resultingPositionMargin)} />} />
            <AccountSummaryRow label="Current leverage" value={formatPositionLeverageForMargin(position, position.marginUsdc)} />
            <AccountSummaryRow label="Resulting leverage" value={formatPositionLeverageForMargin(position, resultingPositionMargin)} />
          </dl>

          {positionMarginError ? (
            <div className="border border-[#FF572D]/40 bg-[#FF572D]/10 p-3 text-sm text-[#FFAB96]">
              {positionMarginError}
            </div>
          ) : null}

          <div className="grid grid-cols-2 gap-3">
            <Button
              type="button"
              variant="secondary"
              className={DARK_CANCEL_BUTTON_CLASS}
              disabled={positionMarginStatus === 'pending'}
              onClick={handleClosePositionMarginModal}
            >
              Cancel
            </Button>
            <Button
              type="button"
              className={LIGHT_ORANGE_ACTION_BUTTON_CLASS}
              isLoading={positionMarginStatus === 'pending'}
              disabled={!canSubmitPositionMargin}
              onClick={() => {
                void handleAddPositionMargin()
              }}
            >
              Add Margin
            </Button>
          </div>
        </div>
      </Modal>
    </div>
  )
}

function OrdersView({
  rows,
  includeStatus,
  nowSeconds,
  cleanupOrderId,
  cleanupError,
  onCleanupExpiredOrder,
}: {
  rows: OrderRow[]
  includeStatus?: boolean
  nowSeconds?: number
  cleanupOrderId?: bigint
  cleanupError?: string
  onCleanupExpiredOrder?: (orderId: bigint) => void
}) {
  if (rows.length === 0) return <EmptyState label={includeStatus ? 'order history' : 'open orders'} />

  return (
    <div className="space-y-3">
      <div className="overflow-x-auto">
        <table className="w-full min-w-[760px] text-left">
          <thead className="text-xs uppercase text-content-secondary">
            <tr className="border-b border-brand-border/20">
              {includeStatus ? <th className="py-3 font-medium">Order ID</th> : null}
              {includeStatus ? <th className="py-3 font-medium">Time</th> : null}
              <th className="py-3 font-medium">Market</th>
              <th className="py-3 font-medium">Side</th>
              <th className="py-3 font-medium">Type</th>
              <th className="py-3 font-medium">Price</th>
              <th className="py-3 font-medium">Size</th>
              {includeStatus ? <th className="py-3 font-medium">Status</th> : null}
              {includeStatus ? <th className="py-3 text-right font-medium">Commit</th> : null}
              {includeStatus ? <th className="py-3 text-right font-medium">Reveal</th> : null}
              {!includeStatus ? <th className="py-3 font-medium">Status</th> : null}
              {!includeStatus ? <th className="py-3 text-right font-medium">Action</th> : null}
            </tr>
          </thead>
          <tbody className="divide-y divide-brand-border/10 text-sm text-content-primary">
            {rows.map((row) => {
              const secondsToExpiry = row.expiryTime === undefined || nowSeconds === undefined
                ? undefined
                : Number(row.expiryTime) - nowSeconds
              const isExpired = secondsToExpiry !== undefined && secondsToExpiry <= 0
              const canCleanup = Boolean(row.orderId && isExpired && onCleanupExpiredOrder)

              return (
                <tr key={`${row.market}-${row.side}-${row.type}-${row.price}-${row.orderId?.toString() ?? 'mock'}`}>
                  {includeStatus ? <td className="py-4 font-mono text-xs text-content-secondary">{row.orderId?.toString() ?? '--'}</td> : null}
                  {includeStatus ? <td className="py-4">{row.time ?? '--'}</td> : null}
                  <td className="py-4 font-semibold">{row.market}</td>
                  <td className="py-4">{row.side}</td>
                  <td className="py-4">{row.type}</td>
                  <td className="py-4">{row.price}</td>
                  <td className="py-4">{row.size}</td>
                  {includeStatus ? <td className="py-4">{row.status}</td> : null}
                  {includeStatus ? <td className="py-3 text-right"><TxLink hash={row.commitTxHash} /></td> : null}
                  {includeStatus ? <td className="py-3 text-right"><TxLink hash={row.revealTxHash} /></td> : null}
                  {!includeStatus ? (
                    <td className="py-4">
                      <OpenOrderStatus secondsToExpiry={secondsToExpiry} />
                    </td>
                  ) : null}
                  {!includeStatus ? (
                    <td className="py-3 text-right">
                      {canCleanup ? (
                        <Button
                          size="sm"
                          variant="secondary"
                          isLoading={cleanupOrderId === row.orderId}
                          onClick={() => {
                            if (row.orderId) onCleanupExpiredOrder?.(row.orderId)
                          }}
                        >
                          Clean Up
                        </Button>
                      ) : (
                        <span className="text-xs text-content-secondary">
                          Cancel unavailable
                        </span>
                      )}
                    </td>
                  ) : null}
                </tr>
              )
            })}
          </tbody>
        </table>
      </div>
      {cleanupError ? (
        <div className="border border-brand-orange/30 bg-brand-orange/10 p-3 text-sm text-brand-orange">
          {cleanupError}
        </div>
      ) : null}
    </div>
  )
}

function TradeHistoryView({ rows }: { rows: TradeRow[] }) {
  if (rows.length === 0) return <EmptyState label="transaction history" />

  return (
    <div className="overflow-x-auto">
      <table className="w-full min-w-[720px] text-left">
        <thead className="text-xs uppercase text-content-secondary">
          <tr className="border-b border-brand-border/20">
            <th className="py-3 font-medium">Time</th>
            <th className="py-3 font-medium">Market</th>
            <th className="py-3 font-medium">Action</th>
            <th className="py-3 font-medium">Price</th>
            <th className="py-3 font-medium">Size</th>
            <th className="py-3 font-medium">Result</th>
            <th className="py-3 text-right font-medium">Tx</th>
          </tr>
        </thead>
        <tbody className="divide-y divide-brand-border/10 text-sm text-content-primary">
          {rows.map((row) => (
            <tr key={`${row.time}-${row.side}-${row.txHash ?? 'no-tx'}`}>
              <td className="py-4">{row.time}</td>
              <td className="py-4 font-semibold">{row.market}</td>
              <td className="py-4">{row.side}</td>
              <td className="py-4">{row.price}</td>
              <td className="py-4">{row.size}</td>
              <td className="py-4">{row.pnl ?? '--'}</td>
              <td className="py-3 text-right"><TxLink hash={row.txHash} /></td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

function AccountTabContent({
  activeTab,
  position,
  equityUsdc,
  freeBuyingPowerUsdc,
  pendingOrders,
  orderHistory,
  tradeHistory,
  isConnected,
  isLoading,
  isHistoryLoading,
  historyError,
  onAccountRefresh,
  nowSeconds,
  cleanupOrderId,
  cleanupError,
  onCleanupExpiredOrder,
}: PerpsAccountPanelProps & {
  activeTab: PerpsAccountTab
  nowSeconds: number
  cleanupOrderId?: bigint
  cleanupError?: string
  onCleanupExpiredOrder?: (orderId: bigint) => void
}) {
  const mockPosition: PerpsPosition = {
    exists: true,
    side: 0,
    direction: 'long',
    size: 0n,
    entryPrice: 98740000n,
    marginUsdc: 0n,
    unrealizedPnlUsdc: 284120000n,
    maintenanceMarginUsdc: 0n,
    liquidatable: false,
    estimatedNotionalUsdc: 8200000000n,
    liquidationPrice: 110000000n,
    pendingCarryUsdc: 0n,
  }
  const liveOpenOrders = pendingOrders?.map((order) => ({
    orderId: order.orderId,
    market: 'plDXY Perp',
    side: perpsSideLabel(order.side),
    type: order.isReduceOnly ? 'Reduce' : 'Open',
    price: order.acceptablePrice === 0n ? 'Market' : formatDisplayDxyPrice(order.acceptablePrice),
    size: <TokenAmount amount={formatPerpsUsdc(order.estimatedNotionalUsdc)} />,
    status: `Status ${order.status.toString()}`,
    expiryTime: order.expiryTime,
  }))
  const liveOrderHistory = orderHistory?.map((order) => ({
    orderId: order.orderId,
    time: order.time,
    market: order.market,
    side: order.side,
    type: order.type,
    price: order.price,
    size: order.size === '--' || order.size === 'Not executed' ? order.size : <TokenAmount amount={order.size} />,
    status: order.status,
    commitTxHash: order.commitTxHash,
    revealTxHash: order.revealTxHash,
  }))
  const liveTradeHistory = tradeHistory?.map((trade) => ({
    time: trade.time,
    market: trade.market,
    side: trade.side,
    price: trade.price,
    size: trade.size === '--' ? '--' : <TokenAmount amount={trade.size} />,
    pnl: trade.pnl === undefined ? undefined : <TokenAmount amount={trade.pnl} />,
    txHash: trade.txHash,
  }))

  if (activeTab === 'position') {
    return (
      <PositionView
        position={position ?? (isConnected === undefined ? mockPosition : undefined)}
        equityUsdc={equityUsdc}
        freeBuyingPowerUsdc={freeBuyingPowerUsdc}
        isConnected={isConnected}
        isLoading={isLoading}
        onAccountRefresh={onAccountRefresh}
      />
    )
  }
  if (activeTab === 'openOrders') {
    return (
      <OrdersView
        rows={liveOpenOrders ?? OPEN_ORDERS}
        nowSeconds={nowSeconds}
        cleanupOrderId={cleanupOrderId}
        cleanupError={cleanupError}
        onCleanupExpiredOrder={onCleanupExpiredOrder}
      />
    )
  }
  if (activeTab === 'orderHistory') {
    if (historyError) return <ErrorState message="Could not load order history. Check the backend history API and perps indexer." />
    if (isHistoryLoading) return <LoadingState label="order history" />
    return <OrdersView rows={liveOrderHistory ?? ORDER_HISTORY} includeStatus />
  }
  if (historyError) return <ErrorState message="Could not load transaction history. Check the backend history API and perps indexer." />
  if (isHistoryLoading) return <LoadingState label="transaction history" />
  return <TradeHistoryView rows={liveTradeHistory ?? TRADE_HISTORY} />
}

export function PerpsAccountPanel(props: PerpsAccountPanelProps) {
  const [activeTab, setActiveTab] = useState<PerpsAccountTab>('position')
  const [nowSeconds, setNowSeconds] = useState(() => Math.floor(Date.now() / 1000))
  const [cleanupOrderId, setCleanupOrderId] = useState<bigint | undefined>()
  const [cleanupError, setCleanupError] = useState<string | undefined>()
  const { cleanupExpiredOrder } = usePerpsTrading()

  useEffect(() => {
    if (!props.pendingOrders?.length) return undefined
    const interval = window.setInterval(() => {
      setNowSeconds(Math.floor(Date.now() / 1000))
    }, 1_000)

    return () => {
      window.clearInterval(interval)
    }
  }, [props.pendingOrders?.length])

  async function handleCleanupExpiredOrder(orderId: bigint) {
    setCleanupOrderId(orderId)
    setCleanupError(undefined)
    try {
      await cleanupExpiredOrder(orderId)
      props.onAccountRefresh?.()
    } catch (error) {
      setCleanupError(error instanceof Error ? error.message : 'Expired-order cleanup failed')
    } finally {
      setCleanupOrderId(undefined)
    }
  }

  return (
    <section className="bg-surface-panel border border-brand-border/30 overflow-visible">
      <div className="border-b border-brand-border/20 px-4 pt-4">
        <div className="flex gap-1 overflow-x-auto">
          {ACCOUNT_TABS.map((tab) => (
            <button
              key={tab.id}
              type="button"
              aria-pressed={activeTab === tab.id}
              className={`shrink-0 px-4 py-3 text-sm font-semibold transition-colors hover:underline hover:underline-offset-4 focus-visible:underline focus-visible:underline-offset-4 ${
                activeTab === tab.id
                  ? 'border-b-2 border-[#FFAB96] text-[#FFAB96]'
                  : 'text-content-secondary hover:text-content-primary'
              }`}
              onClick={() => {
                setActiveTab(tab.id)
              }}
            >
              {tab.label}
            </button>
          ))}
        </div>
      </div>

      <div className="px-5 py-4">
        <AccountTabContent
          activeTab={activeTab}
          nowSeconds={nowSeconds}
          cleanupOrderId={cleanupOrderId}
          cleanupError={cleanupError}
          onCleanupExpiredOrder={(orderId) => {
            void handleCleanupExpiredOrder(orderId)
          }}
          {...props}
        />
      </div>
    </section>
  )
}
