import type { ReactNode } from 'react'
import { Link, useParams } from 'react-router-dom'
import { DEFAULT_COMPETITION_SLUG, InsightsApiError, useWallet, type WalletActivity, type WalletDetails, type WalletPosition } from '../api'
import { EmptyState, ErrorState, LoadingState, Panel, Pnl, StatusBadge } from '../components/ui'
import { formatCompactUsdc, formatPrice, formatRoi, formatSignedUsdc, formatUsdc, formatUtc, isWalletAddress, shortAddress, xProfileUrl } from '../utils/format'
import { calculatePnlBreakdown } from '../utils/pnl'

const ARBITRUM_SEPOLIA_EXPLORER = 'https://sepolia.arbiscan.io'

function Metric({ label, children }: { label: string; children: ReactNode }) {
  return (
    <div className="border-l border-brand-border/20 px-4 py-4 first:border-l-0 sm:px-5">
      <dt className="text-[11px] font-semibold uppercase tracking-[0.14em] text-content-tertiary">{label}</dt>
      <dd className="mt-1 text-base font-semibold tabular-nums sm:text-lg">{children}</dd>
    </div>
  )
}

function PositionPanel({ position }: { position: WalletPosition | null }) {
  return (
    <Panel>
      <div className="border-b border-brand-border/20 px-5 py-4">
        <div className="flex items-center justify-between gap-3">
          <h2 className="text-lg font-semibold">Open position</h2>
          {position?.liquidatable === true ? <span className="text-xs font-semibold uppercase tracking-wide text-brand-orange">Liquidatable</span> : null}
        </div>
      </div>
      {position ? (
        <dl className="grid grid-cols-2 divide-x divide-y divide-brand-border/15 sm:grid-cols-3 lg:grid-cols-6 lg:divide-y-0">
          <Metric label="Market">{position.market}</Metric>
          <Metric label="Side">{position.side ? <span className={position.side === 'long' ? 'text-positive' : 'text-brand-orange'}>{position.side.toUpperCase()}</span> : '—'}</Metric>
          <Metric label="Entry notional">{formatCompactUsdc(position.size)}</Metric>
          <Metric label="Margin">{formatUsdc(position.margin)}</Metric>
          <Metric label="plDXY entry">{formatPrice(position.entryPrice)}</Metric>
          <Metric label="Directional unrealized P&L"><Pnl value={position.unrealizedPnl} /></Metric>
        </dl>
      ) : <EmptyState title="No open position" message="This trader is currently flat." />}
    </Panel>
  )
}

function activityLabel(type: string): string {
  return type.replaceAll('_', ' ').replace(/\b\w/g, (letter) => letter.toUpperCase())
}

function isTradeCostActivity(type: string): boolean {
  return type.toLowerCase() === 'open' || type.toLowerCase() === 'close'
}

function Vpi({ value }: { value: string | null }) {
  const charge = value != null && /^\+?[1-9]\d*$/.test(value)
  const rebate = value?.startsWith('-') === true
  return (
    <span className={`tabular-nums ${charge ? 'text-brand-orange' : rebate ? 'text-positive' : 'text-content-primary'}`}>
      {formatSignedUsdc(value)}
    </span>
  )
}

function ActivityTable({ activity }: { activity: WalletActivity[] | null }) {
  if (!activity?.length) return <EmptyState title="No activity yet" message="Finalized competition activity will appear here." />

  return (
    <>
      <div className="activity-table-desktop overflow-x-auto">
        <table className="w-full min-w-[980px] border-collapse text-left">
          <thead><tr className="border-b border-brand-border/20 text-[11px] font-semibold uppercase tracking-[0.14em] text-content-tertiary"><th className="px-5 py-3">Time</th><th className="px-3 py-3">Activity</th><th className="px-3 py-3">Market</th><th className="px-3 py-3 text-right">Size</th><th className="px-3 py-3 text-right">plDXY price</th><th className="px-3 py-3 text-right">Protocol fee</th><th className="px-3 py-3 text-right" title="Positive VPI is a charge; negative VPI is a rebate.">VPI</th><th className="px-5 py-3 text-right">Directional realized P&amp;L</th></tr></thead>
          <tbody className="divide-y divide-brand-border/15">
            {activity.map((item) => {
              const showTradeCosts = isTradeCostActivity(item.type)
              return (
                <tr key={item.id} className="hover:bg-brand-peach/5">
                  <td className="whitespace-nowrap px-5 py-4 text-xs text-content-secondary">{formatUtc(item.occurredAt)}</td>
                  <td className="px-3 py-4"><div className="text-sm font-semibold">{activityLabel(item.type)}</div>{item.txHash ? <a href={`https://sepolia.arbiscan.io/tx/${item.txHash}`} target="_blank" rel="noreferrer" className="font-mono text-xs text-brand-peach hover:underline">{shortAddress(item.txHash)} ↗</a> : null}</td>
                  <td className="px-3 py-4 text-sm">{item.market ?? '—'}{item.side ? <span className={`ml-2 text-xs uppercase ${item.side === 'long' ? 'text-positive' : 'text-brand-orange'}`}>{item.side}</span> : null}</td>
                  <td className="px-3 py-4 text-right text-sm tabular-nums text-content-secondary">{item.size ? formatCompactUsdc(item.size) : '—'}</td>
                  <td className="px-3 py-4 text-right text-sm tabular-nums">{formatPrice(item.price)}</td>
                  <td className="px-3 py-4 text-right text-sm tabular-nums text-brand-orange">{showTradeCosts ? formatUsdc(item.executionFee) : '—'}</td>
                  <td className="px-3 py-4 text-right text-sm font-semibold">{showTradeCosts ? <Vpi value={item.vpi} /> : '—'}</td>
                  <td className="px-5 py-4 text-right text-sm font-semibold"><Pnl value={item.pnl} /></td>
                </tr>
              )
            })}
          </tbody>
        </table>
      </div>
      <div className="activity-list-mobile divide-y divide-brand-border/15">
        {activity.map((item) => {
          const showTradeCosts = isTradeCostActivity(item.type)
          return (
            <div key={item.id} className="px-4 py-4">
              <div className="flex items-start justify-between gap-3"><div><p className="text-sm font-semibold">{activityLabel(item.type)}</p><p className="mt-1 text-xs text-content-tertiary">{formatUtc(item.occurredAt)}</p></div><Pnl value={item.pnl} className="text-sm font-semibold" /></div>
              <div className="mt-3 flex items-center justify-between text-xs text-content-secondary"><span>{item.market ?? 'Account'}{item.side ? ` · ${item.side}` : ''}</span><span>{item.size ? formatCompactUsdc(item.size) : ''}</span></div>
              {showTradeCosts ? (
                <dl className="mt-3 grid grid-cols-2 gap-3 border-t border-brand-border/15 pt-3 text-xs">
                  <div><dt className="text-content-tertiary">Protocol fee</dt><dd className="mt-1 font-semibold tabular-nums text-brand-orange">{formatUsdc(item.executionFee)}</dd></div>
                  <div className="text-right"><dt className="text-content-tertiary">VPI</dt><dd className="mt-1 font-semibold"><Vpi value={item.vpi} /></dd></div>
                </dl>
              ) : null}
            </div>
          )
        })}
      </div>
    </>
  )
}

function PnlReconciliation({ wallet }: { wallet: WalletDetails }) {
  const breakdown = calculatePnlBreakdown(wallet)
  if (!breakdown) return null

  const items = [
    { label: 'Directional realized P&L', value: breakdown.realized },
    { label: 'Directional unrealized P&L', value: breakdown.unrealized },
    { label: 'Directional P&L subtotal', value: breakdown.directional },
    { label: 'Costs & adjustments (net)', value: breakdown.costsAndAdjustments },
    { label: 'Net competition P&L', value: breakdown.net, emphasized: true },
  ]

  return (
    <Panel>
      <div className="border-b border-brand-border/20 px-5 py-4">
        <h2 className="text-lg font-semibold">Net P&amp;L reconciliation</h2>
        <p className="mt-1 max-w-3xl text-xs leading-5 text-content-tertiary">
          Directional price P&amp;L is shown separately from the account costs that determine competition ranking.
        </p>
      </div>
      <dl className="grid divide-y divide-brand-border/15 sm:grid-cols-2 sm:divide-x sm:divide-y-0 lg:grid-cols-5">
        {items.map((item) => (
          <div key={item.label} className={`px-5 py-4 ${item.emphasized ? 'bg-brand-peach/5' : ''}`}>
            <dt className="text-[11px] font-semibold uppercase tracking-[0.12em] text-content-tertiary">{item.label}</dt>
            <dd className={`mt-2 font-semibold ${item.emphasized ? 'text-lg' : 'text-base'}`}><Pnl value={item.value} /></dd>
          </div>
        ))}
      </dl>
      <div className="border-t border-brand-border/20 bg-app-bg/45 px-5 py-3 text-xs leading-5 text-content-secondary">
        <strong className="text-content-primary">Net competition P&amp;L</strong> = directional realized P&amp;L + directional unrealized P&amp;L + the net effect of execution fees, VPI, carry, execution rewards, and competition adjustments.
      </div>
    </Panel>
  )
}

export function WalletPage() {
  const { slug = DEFAULT_COMPETITION_SLUG, address = '' } = useParams()
  const validAddress = isWalletAddress(address)
  const query = useWallet(slug, validAddress ? address : '')

  if (!validAddress) {
    return <ErrorState title="Invalid wallet address" message="Enter a complete 0x-prefixed Ethereum wallet address." />
  }
  if (query.isLoading) {
    return <div className="space-y-6"><div className="skeleton h-40" /><Panel><LoadingState rows={5} /></Panel></div>
  }
  if (query.isError) {
    const notFound = query.error instanceof InsightsApiError && query.error.status === 404
    return <ErrorState title={notFound ? 'Trader not found' : 'Unable to load wallet'} message={notFound ? 'This wallet is not registered for the current competition.' : query.error.message} onRetry={notFound ? undefined : () => void query.refetch()} />
  }
  if (!query.data) {
    return <ErrorState title="Unable to load wallet" />
  }

  const { wallet, activity, competition } = query.data
  const profileUrl = xProfileUrl(wallet.displayName)
  const explorerUrl = `${ARBITRUM_SEPOLIA_EXPLORER}/address/${wallet.address}`
  return (
    <div className="space-y-6">
      <div>
        <Link to="/" className="text-sm text-brand-peach hover:underline">← Back to leaderboard</Link>
        <div className="mt-5 flex flex-col items-start justify-between gap-4 sm:flex-row">
          <div className="min-w-0">
            <p className="text-xs font-semibold uppercase tracking-[0.16em] text-content-tertiary">
              {wallet.prizePlace !== null ? `Prize place #${String(wallet.prizePlace)} · ` : ''}
              {wallet.rank === null ? 'Rank pending' : `Overall rank #${String(wallet.rank)}`}
            </p>
            <h1 className="mt-2 text-3xl font-semibold sm:text-4xl">
              {profileUrl && wallet.displayName ? (
                <a
                  href={profileUrl}
                  target="_blank"
                  rel="noreferrer"
                  className="inline-flex max-w-full items-baseline gap-2 hover:text-brand-peach hover:underline"
                >
                  <span className="truncate">{wallet.displayName}</span>
                  <span aria-hidden="true" className="text-lg">↗</span>
                </a>
              ) : wallet.displayName ?? shortAddress(wallet.address)}
            </h1>
            <p className="mt-2 font-mono text-xs text-content-secondary sm:text-sm">
              <a
                href={explorerUrl}
                target="_blank"
                rel="noreferrer"
                className="inline-flex max-w-full items-center gap-1.5 hover:text-brand-peach hover:underline"
              >
                <span className="sm:hidden">{shortAddress(wallet.address)}</span>
                <span className="hidden break-all sm:inline">{wallet.address}</span>
                <span aria-hidden="true">↗</span>
              </a>
            </p>
          </div>
          <StatusBadge eligible={wallet.eligible} label={wallet.eligible ? 'Prize eligible' : wallet.eligibilityStatus === 'pending' ? 'Pending review' : wallet.eligibilityStatus === 'under_review' ? 'Under review' : 'Not eligible'} />
        </div>
      </div>

      {wallet.prizeAmountUsdc !== null ? (
        <div className="border border-brand-yellow/40 bg-brand-yellow/10 px-4 py-3 text-sm text-brand-yellow">
          <strong>{wallet.prizePlaces.length > 1 ? `Tied prize places ${wallet.prizePlaces.join('–')}` : `Prize place #${String(wallet.prizePlace ?? '—')}`}:</strong>{' '}
          {formatUsdc(wallet.prizeAmountUsdc)}
        </div>
      ) : null}

      {!wallet.eligible && wallet.eligibilityReasons.length > 0 ? (
        <div className="border border-brand-orange/35 bg-brand-orange/10 px-4 py-3 text-sm text-brand-peach">
          <strong>Eligibility:</strong> {wallet.eligibilityReasons.join(' · ')}
        </div>
      ) : null}

      <Panel>
        <dl className="grid grid-cols-2 divide-x divide-y divide-brand-border/15 sm:grid-cols-3 lg:grid-cols-6 lg:divide-y-0">
          <Metric label="Net competition P&L"><Pnl value={wallet.pnl} /></Metric>
          <Metric label="Net return"><span className={wallet.roiBps !== null && wallet.roiBps >= 0 ? 'text-positive' : 'text-brand-orange'}>{formatRoi(wallet.roiBps)}</span></Metric>
          <Metric label="Current net equity">{formatUsdc(wallet.equity)}</Metric>
          <Metric label="Volume">{formatCompactUsdc(wallet.volume)}</Metric>
          <Metric label="Trades">{wallet.trades}</Metric>
          <Metric label={`Active days / ${String(competition.minActiveDays)}`}>{wallet.activeDays}</Metric>
        </dl>
      </Panel>

      <PnlReconciliation wallet={wallet} />

      <PositionPanel position={wallet.position ?? null} />

      <Panel>
        <div className="border-b border-brand-border/20 px-5 py-4"><h2 className="text-lg font-semibold">Competition activity</h2><p className="mt-1 text-xs text-content-tertiary">Finalized protocol events, newest first · realized values are directional price P&amp;L before account costs</p></div>
        <ActivityTable activity={activity ?? null} />
      </Panel>
    </div>
  )
}
