import type { ReactNode } from 'react'
import { Link, useParams } from 'react-router-dom'
import { DEFAULT_COMPETITION_SLUG, InsightsApiError, useWallet, type WalletActivity, type WalletPosition } from '../api'
import { EmptyState, ErrorState, LoadingState, Panel, Pnl, StatusBadge } from '../components/ui'
import { formatCompactUsdc, formatPrice, formatRoi, formatUsdc, formatUtc, isWalletAddress, shortAddress, xProfileUrl } from '../utils/format'

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
          <Metric label="Unrealized P&L"><Pnl value={position.unrealizedPnl} /></Metric>
        </dl>
      ) : <EmptyState title="No open position" message="This trader is currently flat." />}
    </Panel>
  )
}

function activityLabel(type: string): string {
  return type.replaceAll('_', ' ').replace(/\b\w/g, (letter) => letter.toUpperCase())
}

function ActivityTable({ activity }: { activity: WalletActivity[] | null }) {
  if (!activity?.length) return <EmptyState title="No activity yet" message="Finalized competition activity will appear here." />

  return (
    <>
      <div className="hidden overflow-x-auto sm:block">
        <table className="w-full min-w-[760px] border-collapse text-left">
          <thead><tr className="border-b border-brand-border/20 text-[11px] font-semibold uppercase tracking-[0.14em] text-content-tertiary"><th className="px-5 py-3">Time</th><th className="px-3 py-3">Activity</th><th className="px-3 py-3">Market</th><th className="px-3 py-3 text-right">Size</th><th className="px-3 py-3 text-right">plDXY price</th><th className="px-5 py-3 text-right">Realized P&amp;L</th></tr></thead>
          <tbody className="divide-y divide-brand-border/15">
            {activity.map((item) => (
              <tr key={item.id} className="hover:bg-brand-peach/5">
                <td className="whitespace-nowrap px-5 py-4 text-xs text-content-secondary">{formatUtc(item.occurredAt)}</td>
                <td className="px-3 py-4"><div className="text-sm font-semibold">{activityLabel(item.type)}</div>{item.txHash ? <a href={`https://sepolia.arbiscan.io/tx/${item.txHash}`} target="_blank" rel="noreferrer" className="font-mono text-xs text-brand-peach hover:underline">{shortAddress(item.txHash)} ↗</a> : null}</td>
                <td className="px-3 py-4 text-sm">{item.market ?? '—'}{item.side ? <span className={`ml-2 text-xs uppercase ${item.side === 'long' ? 'text-positive' : 'text-brand-orange'}`}>{item.side}</span> : null}</td>
                <td className="px-3 py-4 text-right text-sm tabular-nums text-content-secondary">{item.size ? formatCompactUsdc(item.size) : '—'}</td>
                <td className="px-3 py-4 text-right text-sm tabular-nums">{formatPrice(item.price)}</td>
                <td className="px-5 py-4 text-right text-sm font-semibold"><Pnl value={item.pnl} /></td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      <div className="divide-y divide-brand-border/15 sm:hidden">
        {activity.map((item) => (
          <div key={item.id} className="px-4 py-4">
            <div className="flex items-start justify-between gap-3"><div><p className="text-sm font-semibold">{activityLabel(item.type)}</p><p className="mt-1 text-xs text-content-tertiary">{formatUtc(item.occurredAt)}</p></div><Pnl value={item.pnl} className="text-sm font-semibold" /></div>
            <div className="mt-3 flex items-center justify-between text-xs text-content-secondary"><span>{item.market ?? 'Account'}{item.side ? ` · ${item.side}` : ''}</span><span>{item.size ? formatCompactUsdc(item.size) : ''}</span></div>
          </div>
        ))}
      </div>
    </>
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
        <div className="mt-5 flex flex-col gap-4 sm:flex-row sm:items-start sm:justify-between">
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
          {formatUsdc(wallet.prizeAmountUsdc, 0)}
        </div>
      ) : null}

      {!wallet.eligible && wallet.eligibilityReasons.length > 0 ? (
        <div className="border border-brand-orange/35 bg-brand-orange/10 px-4 py-3 text-sm text-brand-peach">
          <strong>Eligibility:</strong> {wallet.eligibilityReasons.join(' · ')}
        </div>
      ) : null}

      <Panel>
        <dl className="grid grid-cols-2 divide-x divide-y divide-brand-border/15 sm:grid-cols-3 lg:grid-cols-6 lg:divide-y-0">
          <Metric label="Final P&L"><Pnl value={wallet.pnl} /></Metric>
          <Metric label="Return"><span className={wallet.roiBps !== null && wallet.roiBps >= 0 ? 'text-positive' : 'text-brand-orange'}>{formatRoi(wallet.roiBps)}</span></Metric>
          <Metric label="Net equity">{formatUsdc(wallet.equity)}</Metric>
          <Metric label="Volume">{formatCompactUsdc(wallet.volume)}</Metric>
          <Metric label="Trades">{wallet.trades}</Metric>
          <Metric label={`Active days / ${String(competition.minActiveDays)}`}>{wallet.activeDays}</Metric>
        </dl>
      </Panel>

      <PositionPanel position={wallet.position ?? null} />

      <Panel>
        <div className="border-b border-brand-border/20 px-5 py-4"><h2 className="text-lg font-semibold">Competition activity</h2><p className="mt-1 text-xs text-content-tertiary">Finalized protocol events, newest first</p></div>
        <ActivityTable activity={activity ?? null} />
      </Panel>
    </div>
  )
}
