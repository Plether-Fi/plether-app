import { type ReactNode, useEffect, useState } from 'react'
import type { PerpsOrderHistoryRow, PerpsPendingOrder, PerpsPosition, PerpsTradeHistoryRow } from '../hooks'
import { usePerpsTrading } from '../hooks'
import { PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { getExplorerTxUrl } from '../utils/explorer'
import { formatDisplayDxyPrice, formatPerpsUsdc, formatSignedPerpsUsdc, oraclePriceToDisplayDxyPrice, perpsSideLabel } from '../utils/perps'
import { Button, TokenAmount } from './ui'

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
  liquidationPrice: ReactNode
  pnl: ReactNode
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
  { id: 'tradeHistory', label: 'Trade History' },
]

const OPEN_ORDERS: OrderRow[] = [
  { market: 'DXY Perp', side: 'Buy', type: 'Limit', price: '0.9880', size: <TokenAmount amount="1 500" /> },
  { market: 'DXY Perp', side: 'Sell', type: 'Take profit', price: '1.0040', size: <TokenAmount amount="3 000" /> },
]

const ORDER_HISTORY: OrderRow[] = [
  {
    orderId: 101n,
    time: '12:42',
    market: 'DXY Perp',
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
    market: 'DXY Perp',
    side: 'Short',
    type: 'Close',
    price: '0.9790',
    size: <TokenAmount amount="1 200" />,
    status: 'Expired',
    commitTxHash: '0x0000000000000000000000000000000000000000000000000000000000000100',
  },
]

const TRADE_HISTORY: TradeRow[] = [
  { time: '12:42', market: 'DXY Perp', side: 'Open Long', price: '0.9912', size: <TokenAmount amount="1 000" /> },
  { time: '11:08', market: 'DXY Perp', side: 'Close Long', price: '0.9931', size: <TokenAmount amount="650" />, pnl: <TokenAmount amount="+12.2" /> },
]

function pnlToneClass(tone: PositionRow['tone']): string {
  if (tone === 'positive') return 'text-cyber-neon-green'
  if (tone === 'negative') return 'text-cyber-electric-fuchsia'
  return 'text-cyber-text-primary'
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

function LiquidationPriceValue({
  currentPrice,
  liquidationPrice,
}: {
  currentPrice?: bigint
  liquidationPrice?: bigint
}) {
  const distance = formatLiquidationDistance(currentPrice, liquidationPrice)

  return (
    <span className="inline-flex flex-col items-start gap-1">
      <span>{formatDisplayDxyPrice(liquidationPrice)}</span>
      {distance ? (
        <span className="text-xs font-medium text-cyber-text-secondary">{distance}</span>
      ) : null}
    </span>
  )
}

function EmptyState({ label }: { label: string }) {
  return (
    <div className="flex min-h-[150px] items-center justify-center border border-cyber-border-glow/20 bg-cyber-bg/35">
      <span className="text-sm text-cyber-text-secondary">No {label.toLowerCase()}</span>
    </div>
  )
}

function LoadingState({ label }: { label: string }) {
  return (
    <div className="flex min-h-[150px] items-center justify-center border border-cyber-border-glow/20 bg-cyber-bg/35">
      <span className="text-sm text-cyber-text-secondary">Loading {label.toLowerCase()}...</span>
    </div>
  )
}

function ErrorState({ message }: { message: string }) {
  return (
    <div className="flex min-h-[150px] items-center justify-center border border-cyber-electric-fuchsia/30 bg-cyber-electric-fuchsia/10 p-4">
      <span className="text-sm text-cyber-electric-fuchsia">{message}</span>
    </div>
  )
}

function AccountMetric({ label, value, tone }: { label: string; value: ReactNode; tone?: PositionRow['tone'] }) {
  return (
    <div className="min-w-0">
      <div className="text-xs font-medium uppercase text-cyber-text-secondary">{label}</div>
      <div className={`mt-2 text-xl font-semibold ${pnlToneClass(tone)}`}>{value}</div>
    </div>
  )
}

function formatDuration(seconds: number): string {
  if (!Number.isFinite(seconds) || seconds <= 0) return 'now'

  const minutes = Math.floor(seconds / 60)
  const remainingSeconds = seconds % 60
  if (minutes <= 0) return `${remainingSeconds}s`

  const hours = Math.floor(minutes / 60)
  const remainingMinutes = minutes % 60
  if (hours <= 0) return `${minutes}m ${remainingSeconds}s`

  return `${hours}h ${remainingMinutes}m`
}

function OpenOrderStatus({ secondsToExpiry }: { secondsToExpiry?: number }) {
  if (secondsToExpiry === undefined) {
    return (
      <div>
        <div className="font-semibold text-cyber-text-primary">Pending</div>
        <div className="mt-1 text-xs text-cyber-text-secondary">Waiting for reveal</div>
      </div>
    )
  }

  if (secondsToExpiry <= 0) {
    return (
      <div>
        <div className="font-semibold text-cyber-electric-fuchsia">Expired</div>
        <div className="mt-1 text-xs text-cyber-text-secondary">Clean up to release reserved margin</div>
      </div>
    )
  }

  return (
    <div>
      <div className="font-semibold text-cyber-text-primary">Pending reveal</div>
      <div className="mt-1 text-xs text-cyber-text-secondary">
        Expires in {formatDuration(secondsToExpiry)}
      </div>
    </div>
  )
}

function TxLink({ hash }: { hash?: string }) {
  if (!hash) return <span className="text-cyber-text-secondary">--</span>

  return (
    <a
      aria-label="Open transaction in block explorer"
      title="Open in block explorer"
      href={getExplorerTxUrl(PERPS_ARBITRUM_SEPOLIA_CHAIN_ID, hash)}
      target="_blank"
      rel="noopener noreferrer"
      className="inline-flex h-5 w-5 items-center justify-center text-cyber-text-secondary transition-colors hover:text-cyber-bright-blue"
    >
      <span className="material-symbols-outlined !text-[16px] !leading-none">open_in_new</span>
    </a>
  )
}

function PositionView({
  position,
  isConnected,
  isLoading,
}: {
  position?: PerpsPosition
  isConnected?: boolean
  isLoading?: boolean
}) {
  if (isConnected === false) return <EmptyState label="connected wallet" />
  if (isLoading) return <EmptyState label="position data" />
  if (!position?.exists) return <EmptyState label="current position" />

  const currentPnl = position.unrealizedPnlUsdc
  const currentPosition: PositionRow = {
    market: 'DXY Perp',
    side: perpsSideLabel(position.side),
    size: <TokenAmount amount={formatPerpsUsdc(position.dxyExposureUsdc ?? position.estimatedNotionalUsdc)} />,
    entryNotional: <TokenAmount amount={formatPerpsUsdc(position.entryNotionalUsdc)} />,
    entry: formatDisplayDxyPrice(position.entryPrice),
    liquidationPrice: (
      <LiquidationPriceValue
        currentPrice={position.displayDxyPrice}
        liquidationPrice={position.liquidationPrice}
      />
    ),
    pnl: <TokenAmount amount={formatSignedPerpsUsdc(currentPnl)} />,
    tone: currentPnl < 0n ? 'negative' : currentPnl > 0n ? 'positive' : undefined,
  }

  return (
    <div className="border border-cyber-border-glow/20 bg-cyber-bg/35 p-4">
      <div className="mb-4">
        <div className="text-xs font-medium uppercase text-cyber-text-secondary">Current Position</div>
        <div className="mt-2 flex items-center gap-3">
          <span className="border border-cyber-neon-green/40 px-3 py-1 text-sm font-semibold text-cyber-neon-green">
            {currentPosition.side}
          </span>
          <div className="mt-1 text-lg font-semibold text-cyber-text-primary">{currentPosition.market}</div>
        </div>
      </div>
      <div className="grid grid-cols-2 gap-4 md:grid-cols-5">
        <AccountMetric label="DXY exposure" value={currentPosition.size} />
        <AccountMetric label="Entry notional" value={currentPosition.entryNotional} />
        <AccountMetric label="Entry price" value={currentPosition.entry} />
        <AccountMetric label="Liquidation price" value={currentPosition.liquidationPrice} />
        <AccountMetric label="Unrealized PnL" value={currentPosition.pnl} tone={currentPosition.tone} />
      </div>
      <p className="mt-4 border-t border-cyber-border-glow/20 pt-3 text-sm leading-5 text-cyber-text-secondary">
        Entry notional is the executed order size. DXY exposure is current displayed exposure.
      </p>
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
          <thead className="text-xs uppercase text-cyber-text-secondary">
            <tr className="border-b border-cyber-border-glow/20">
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
          <tbody className="divide-y divide-cyber-border-glow/10 text-sm text-cyber-text-primary">
            {rows.map((row) => {
              const secondsToExpiry = row.expiryTime === undefined || nowSeconds === undefined
                ? undefined
                : Number(row.expiryTime) - nowSeconds
              const isExpired = secondsToExpiry !== undefined && secondsToExpiry <= 0
              const canCleanup = Boolean(row.orderId && isExpired && onCleanupExpiredOrder)

              return (
                <tr key={`${row.market}-${row.side}-${row.type}-${row.price}-${row.orderId?.toString() ?? 'mock'}`}>
                  {includeStatus ? <td className="py-4 font-mono text-xs text-cyber-text-secondary">{row.orderId?.toString() ?? '--'}</td> : null}
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
                        <span className="text-xs text-cyber-text-secondary">
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
        <div className="border border-cyber-electric-fuchsia/30 bg-cyber-electric-fuchsia/10 p-3 text-sm text-cyber-electric-fuchsia">
          {cleanupError}
        </div>
      ) : null}
    </div>
  )
}

function TradeHistoryView({ rows }: { rows: TradeRow[] }) {
  if (rows.length === 0) return <EmptyState label="trade history" />

  return (
    <div className="overflow-x-auto">
      <table className="w-full min-w-[720px] text-left">
        <thead className="text-xs uppercase text-cyber-text-secondary">
          <tr className="border-b border-cyber-border-glow/20">
            <th className="py-3 font-medium">Time</th>
            <th className="py-3 font-medium">Market</th>
            <th className="py-3 font-medium">Side</th>
            <th className="py-3 font-medium">Price</th>
            <th className="py-3 font-medium">Size</th>
            <th className="py-3 font-medium">PnL</th>
            <th className="py-3 text-right font-medium">Tx</th>
          </tr>
        </thead>
        <tbody className="divide-y divide-cyber-border-glow/10 text-sm text-cyber-text-primary">
          {rows.map((row) => (
            <tr key={`${row.time}-${row.side}-${row.price}`}>
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
  pendingOrders,
  orderHistory,
  tradeHistory,
  isConnected,
  isLoading,
  isHistoryLoading,
  historyError,
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
  }
  const liveOpenOrders = pendingOrders?.map((order) => ({
    orderId: order.orderId,
    market: 'DXY Perp',
    side: perpsSideLabel(order.side),
    type: order.isReduceOnly ? 'Reduce' : 'Open',
    price: order.acceptablePrice === 0n ? 'Market' : formatDisplayDxyPrice(order.acceptablePrice),
    size: <TokenAmount amount={formatPerpsUsdc(order.estimatedNotionalUsdc)} />,
    status: `Status ${order.status}`,
    expiryTime: order.expiryTime,
  }))
  const liveOrderHistory = orderHistory?.map((order) => ({
    orderId: order.orderId,
    time: order.time,
    market: order.market,
    side: order.side,
    type: order.type,
    price: order.price,
    size: order.size === '--' ? '--' : <TokenAmount amount={order.size} />,
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
        isConnected={isConnected}
        isLoading={isLoading}
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
    if (historyError) return <ErrorState message="Could not load order history. Check RPC access and retry." />
    if (isHistoryLoading) return <LoadingState label="order history" />
    return <OrdersView rows={liveOrderHistory ?? ORDER_HISTORY} includeStatus />
  }
  if (historyError) return <ErrorState message="Could not load trade history. Check RPC access and retry." />
  if (isHistoryLoading) return <LoadingState label="trade history" />
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
    <section className="bg-cyber-surface-dark border border-cyber-border-glow/30 shadow-lg shadow-cyber-border-glow/10 overflow-hidden">
      <div className="border-b border-cyber-border-glow/20 px-4 pt-4">
        <div className="flex gap-1 overflow-x-auto">
          {ACCOUNT_TABS.map((tab) => (
            <button
              key={tab.id}
              type="button"
              aria-pressed={activeTab === tab.id}
              className={`shrink-0 px-4 py-3 text-sm font-semibold transition-colors ${
                activeTab === tab.id
                  ? 'border-b-2 border-cyber-bright-blue text-cyber-bright-blue'
                  : 'text-cyber-text-secondary hover:text-cyber-text-primary'
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
