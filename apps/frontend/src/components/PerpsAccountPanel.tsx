import { type ReactNode, useState } from 'react'
import type { PerpsPendingOrder, PerpsPosition } from '../hooks'
import { formatPerpsPrice, formatPerpsUsdc, formatSignedPerpsUsdc, perpsSideLabel } from '../utils/perps'
import { TokenAmount } from './ui'

type PerpsAccountTab = 'position' | 'openOrders' | 'orderHistory' | 'tradeHistory'

interface AccountTab {
  id: PerpsAccountTab
  label: string
}

interface PositionRow {
  market: string
  side: string
  size: ReactNode
  entry: string
  pnl: ReactNode
  tone?: 'positive' | 'negative'
}

interface OrderRow {
  market: string
  side: string
  type: string
  price: string
  size: ReactNode
  status?: string
}

interface TradeRow {
  time: string
  market: string
  side: string
  price: string
  size: ReactNode
  fee: ReactNode
}

interface PerpsAccountPanelProps {
  position?: PerpsPosition
  pendingOrders?: PerpsPendingOrder[]
  isConnected?: boolean
  isLoading?: boolean
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
  { market: 'DXY Perp', side: 'Buy', type: 'Limit', price: '0.9850', size: <TokenAmount amount="2 500" />, status: 'Filled' },
  { market: 'DXY Perp', side: 'Sell', type: 'Stop', price: '0.9790', size: <TokenAmount amount="1 200" />, status: 'Cancelled' },
]

const TRADE_HISTORY: TradeRow[] = [
  { time: '12:42', market: 'DXY Perp', side: 'Buy', price: '0.9912', size: <TokenAmount amount="1 000" />, fee: <TokenAmount amount="0.42" /> },
  { time: '11:08', market: 'DXY Perp', side: 'Sell', price: '0.9931', size: <TokenAmount amount="650" />, fee: <TokenAmount amount="0.27" /> },
]

function pnlToneClass(tone: PositionRow['tone']): string {
  if (tone === 'positive') return 'text-cyber-neon-green'
  if (tone === 'negative') return 'text-cyber-electric-fuchsia'
  return 'text-cyber-text-primary'
}

function EmptyState({ label }: { label: string }) {
  return (
    <div className="flex min-h-[150px] items-center justify-center border border-cyber-border-glow/20 bg-cyber-bg/35">
      <span className="text-sm text-cyber-text-secondary">No {label.toLowerCase()}</span>
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
    size: <TokenAmount amount={formatPerpsUsdc(position.estimatedNotionalUsdc)} />,
    entry: formatPerpsPrice(position.entryPrice),
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
      <div className="grid grid-cols-3 gap-4">
        <AccountMetric label="Size" value={currentPosition.size} />
        <AccountMetric label="Entry" value={currentPosition.entry} />
        <AccountMetric label="PnL" value={currentPosition.pnl} tone={currentPosition.tone} />
      </div>
    </div>
  )
}

function OrdersView({ rows, includeStatus }: { rows: OrderRow[]; includeStatus?: boolean }) {
  if (rows.length === 0) return <EmptyState label={includeStatus ? 'order history' : 'open orders'} />

  return (
    <div className="overflow-x-auto">
      <table className="w-full min-w-[680px] text-left">
        <thead className="text-xs uppercase text-cyber-text-secondary">
          <tr className="border-b border-cyber-border-glow/20">
            <th className="py-3 font-medium">Market</th>
            <th className="py-3 font-medium">Side</th>
            <th className="py-3 font-medium">Type</th>
            <th className="py-3 font-medium">Price</th>
            <th className="py-3 font-medium">Size</th>
            {includeStatus ? <th className="py-3 font-medium">Status</th> : null}
          </tr>
        </thead>
        <tbody className="divide-y divide-cyber-border-glow/10 text-sm text-cyber-text-primary">
          {rows.map((row) => (
            <tr key={`${row.market}-${row.side}-${row.type}-${row.price}`}>
              <td className="py-4 font-semibold">{row.market}</td>
              <td className="py-4">{row.side}</td>
              <td className="py-4">{row.type}</td>
              <td className="py-4">{row.price}</td>
              <td className="py-4">{row.size}</td>
              {includeStatus ? <td className="py-4">{row.status}</td> : null}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

function TradeHistoryView() {
  if (TRADE_HISTORY.length === 0) return <EmptyState label="trade history" />

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
            <th className="py-3 font-medium">Fee</th>
          </tr>
        </thead>
        <tbody className="divide-y divide-cyber-border-glow/10 text-sm text-cyber-text-primary">
          {TRADE_HISTORY.map((row) => (
            <tr key={`${row.time}-${row.side}-${row.price}`}>
              <td className="py-4">{row.time}</td>
              <td className="py-4 font-semibold">{row.market}</td>
              <td className="py-4">{row.side}</td>
              <td className="py-4">{row.price}</td>
              <td className="py-4">{row.size}</td>
              <td className="py-4">{row.fee}</td>
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
  isConnected,
  isLoading,
}: PerpsAccountPanelProps & { activeTab: PerpsAccountTab }) {
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
  }
  const liveOpenOrders = pendingOrders?.map((order) => ({
    market: 'DXY Perp',
    side: perpsSideLabel(order.side),
    type: order.isReduceOnly ? 'Reduce' : 'Open',
    price: order.acceptablePrice === 0n ? 'Market' : formatPerpsPrice(order.acceptablePrice),
    size: <TokenAmount amount={formatPerpsUsdc(order.estimatedNotionalUsdc)} />,
    status: `Status ${order.status}`,
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
  if (activeTab === 'openOrders') return <OrdersView rows={liveOpenOrders ?? OPEN_ORDERS} />
  if (activeTab === 'orderHistory') return <OrdersView rows={ORDER_HISTORY} includeStatus />
  return <TradeHistoryView />
}

export function PerpsAccountPanel(props: PerpsAccountPanelProps) {
  const [activeTab, setActiveTab] = useState<PerpsAccountTab>('position')

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
        <AccountTabContent activeTab={activeTab} {...props} />
      </div>
    </section>
  )
}
