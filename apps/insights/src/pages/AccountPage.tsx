import { Link, useParams } from 'react-router-dom'
import { useInsightsStatus } from '../api'
import { StatCard } from '../components/StatCard'
import { StatusPill } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel, Pnl } from '../components/ui'
import { WalletMetric as Metric } from '../components/WalletMetric'
import { CurrentBadge } from '../divisions/CurrentBadge'
import { formatReturn } from '../divisions/engine'
import { useDivisions, useLiveDivisions } from '../divisions/query'
import { explorerAddressUrl, resolvePerpsChain } from '../utils/chain'
import { formatRoi, formatUtc, isWalletAddress, shortAddress } from '../utils/format'

function TradingAccountInspector({ address }: { address: string }) {
  const statusQuery = useInsightsStatus()
  const liveQuery = useLiveDivisions()
  const publicationQuery = useDivisions()

  const chain = resolvePerpsChain(statusQuery.data?.chainId)
  const live = liveQuery.data?.status === 'ready' ? liveQuery.data : null
  const publication = publicationQuery.data
  const current = live?.standings?.find(
    (row) => row.address.toLowerCase() === address.toLowerCase(),
  )
  const awarded = publication?.standings.find(
    (row) => row.address.toLowerCase() === address.toLowerCase(),
  )
  const handle = current?.discordUsername ?? awarded?.displayName

  return (
    <div className="space-y-8">
      <div className="flex flex-wrap items-center justify-between gap-3 text-sm">
        <Link to="/" className="text-brand-peach hover:underline">
          ← Protocol Pulse
        </Link>
        <Link to="/divisions" className="text-content-secondary hover:text-brand-peach">
          Divisions leaderboard →
        </Link>
      </div>

      <header className="flex flex-col items-start justify-between gap-4 border border-brand-border/25 bg-surface-panel p-6 sm:flex-row">
        <div className="min-w-0">
          <div className="flex flex-wrap items-center gap-2">
            <p className="text-xs font-semibold uppercase tracking-[0.16em] text-brand-peach">
              Trading Account Inspector
            </p>
            <span className="border border-brand-border/25 bg-app-bg px-2 py-0.5 font-mono text-xs text-content-secondary">
              {chain.name}
            </span>
          </div>
          <h1 className="mt-3 break-all text-3xl font-semibold sm:text-4xl">
            {handle ? `@${handle.replace(/^@/, '')}` : shortAddress(address)}
          </h1>
          <p className="mt-2 break-all font-mono text-xs text-content-secondary sm:text-sm">
            {address}
          </p>
          <a
            href={explorerAddressUrl(address, statusQuery.data?.chainId)}
            target="_blank"
            rel="noreferrer"
            className="mt-2 inline-block text-xs text-brand-peach hover:underline"
          >
            View Trading Account on explorer ↗
          </a>
        </div>
        <div className="space-y-2 sm:text-right">
          <p className="text-xs text-content-tertiary">Current Divisions badge</p>
          <CurrentBadge
            tier={awarded?.tier}
            loading={publicationQuery.isLoading}
            unavailable={publicationQuery.isError && !publication}
          />
        </div>
      </header>

      <section aria-labelledby="account-divisions-title" className="space-y-4">
        <h2 id="account-divisions-title" className="text-xl font-semibold">
          Divisions standing
        </h2>

        {liveQuery.isLoading ? (
          <Panel>
            <LoadingState rows={2} />
          </Panel>
        ) : liveQuery.isError && !live ? (
          <ErrorState
            title="Live performance is unavailable"
            message={liveQuery.error.message}
            onRetry={() => void liveQuery.refetch()}
          />
        ) : current && live ? (
          <>
            <p className="text-xs text-content-tertiary">
              Provisional · Week starting {formatUtc(live.weekStartsAt)} · Snapshot{' '}
              {formatUtc(live.snapshotAt ?? '')}
            </p>
            <Panel>
              <dl className="grid grid-cols-1 divide-y divide-brand-border/15 sm:grid-cols-3 sm:divide-y-0">
                <Metric label="Live weekly rank">{current.rank ?? '—'}</Metric>
                <Metric label="Live weekly return">{formatRoi(current.weeklyReturnBps)}</Metric>
                <Metric label="Active days">{current.activeDays}</Metric>
              </dl>
            </Panel>
          </>
        ) : (
          <Panel>
            <EmptyState
              title="No active-week Trading Account ranking"
              message="This Trading Account has no live weekly Divisions snapshot on the current release yet."
            />
          </Panel>
        )}

        {awarded && publication ? (
          <Panel>
            <div className="border-b border-brand-border/15 px-5 py-3 text-xs text-content-tertiary">
              Last published week ending {formatUtc(publication.week.closesAt)} · Published{' '}
              {formatUtc(publication.publishedAt)}
            </div>
            <dl className="grid grid-cols-1 divide-y divide-brand-border/15 sm:grid-cols-2 lg:grid-cols-4 lg:divide-y-0">
              <Metric label="Final rank">{awarded.rank ?? '—'}</Metric>
              <Metric label="Weekly return">
                {formatReturn(awarded.weeklyPnlAssets, awarded.adjustedEquityAssets)}
              </Metric>
              <Metric label="Net P&L">
                <Pnl
                  value={awarded.weeklyPnlAssets}
                  assetKind={
                    publication.scoringRelease?.assetSymbol === 'USDC' ? 'mock' : undefined
                  }
                />
              </Metric>
              <Metric label="Active days">{awarded.activeDays}</Metric>
            </dl>
          </Panel>
        ) : null}
      </section>

      <section aria-labelledby="account-deep-title" className="space-y-4">
        <div className="flex flex-wrap items-center justify-between gap-2">
          <h2 id="account-deep-title" className="text-xl font-semibold">
            Positions, receipts &amp; LP holdings
          </h2>
          <StatusPill tone="unknown" label="Stage 3 API pending" />
        </div>
        <div className="grid gap-4 sm:grid-cols-3">
          <StatCard
            label="Open position & margin"
            unavailable
            detail="Live cfdEngineAccountLens position, liquidation buffer & active TP/SL rules"
          />
          <StatCard
            label="Execution receipt history"
            unavailable
            detail="Per-order VPI, protocol fee, waivers, rebates & account change audit"
          />
          <StatCard
            label="Senior / Junior LP balance"
            unavailable
            detail="Attributed vault shares & pending deposit/redemption requests"
          />
        </div>
      </section>
    </div>
  )
}

export function AccountPage() {
  const { address = '' } = useParams()
  if (!isWalletAddress(address)) {
    return (
      <ErrorState
        title="Invalid Trading Account address"
        message="Enter a complete 0x-prefixed 40-hex-character Ethereum Trading Account address."
      />
    )
  }
  return <TradingAccountInspector address={address} />
}
