import { useState } from 'react'
import { Link, useParams, useSearchParams } from 'react-router-dom'
import {
  InsightsApiError,
  accountActivityToProtocolTrade,
  enrichBalanceHistory,
  formatPlDxyQuantity,
  formatVaultShares,
  transactionPath,
  useWalletExposure,
  useWalletSummary,
  type AccountActivityRecord,
  type TransactionRouteState,
} from '../api'
import { ArbiscanButton, CopyButton } from '../components/ActionButtons'
import { AddressAvatar } from '../components/AddressAvatar'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import {
  ActivePositionProtectionStrip,
  OrderProtectionsInlineChecklist,
} from '../components/OrderProtectionsChecklist'
import {
  ActivePositionRiskStrip,
  WalletExposureSection,
} from '../components/WalletExposureSection'
import {
  normalizeAccountProtection,
  protectionLifecycleTone,
} from '../utils/orderProtections'
import { ShareCardModal, type ShareCardOption } from '../components/ShareCardModal'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel, Pnl } from '../components/ui'
import { WalletBalanceChart } from '../components/WalletBalanceChart'
import { WalletMetric as Metric } from '../components/WalletMetric'
import { CurrentBadge } from '../divisions/CurrentBadge'
import { formatReturn } from '../divisions/engine'
import { useDivisions, useLiveDivisions } from '../divisions/query'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { arbiscanLinkKey, resolvePerpsChain, vaultsAppUrl } from '../utils/chain'
import {
  formatAssets,
  formatCompactAssets,
  formatPrice,
  formatUtc,
  isWalletAddress,
  shortAddress,
} from '../utils/format'

type InspectorTab = 'overview' | 'exposure' | 'trades' | 'protections' | 'lp' | 'divisions'
type ActivityFilter = 'all' | 'Open' | 'Close' | 'Liquidated' | 'other'

function isEndpointNotDeployed(error: unknown): boolean {
  return error instanceof InsightsApiError && error.status === 404
}

function activityTone(activityType: string): StatusTone {
  if (activityType === 'Open') return 'live'
  if (activityType === 'Liquidated') return 'blocked'
  return 'unknown'
}

function trancheLabel(tranche: 'senior' | 'junior' | 'unknown'): string {
  if (tranche === 'senior') return 'Senior Vault'
  if (tranche === 'junior') return 'Junior Vault'
  return 'Vault'
}

const VALID_TABS: readonly InspectorTab[] = [
  'overview',
  'exposure',
  'trades',
  'protections',
  'lp',
  'divisions',
]

function parseTabParam(raw: string | null): InspectorTab {
  return VALID_TABS.includes(raw as InspectorTab) ? (raw as InspectorTab) : 'overview'
}

function TradingAccountInspector({ address }: { address: string }) {
  const protocol = useProtocolContext()
  const [searchParams, setSearchParams] = useSearchParams()
  const summaryQuery = useWalletSummary(address, 50)
  const exposureQuery = useWalletExposure(address)
  const liveQuery = useLiveDivisions()
  const publicationQuery = useDivisions()
  const activeTab = parseTabParam(searchParams.get('tab'))
  const [activityFilter, setActivityFilter] = useState<ActivityFilter>('all')

  const summary = summaryQuery.data
  const exposure = exposureQuery.data
  const awaitingDeploy = isEndpointNotDeployed(summaryQuery.error)
  const awaitingExposureDeploy = isEndpointNotDeployed(exposureQuery.error)
  const chainId = summary?.network.chainId ?? protocol.chainId
  const chain = resolvePerpsChain(chainId)
  const releaseVersion = summary?.network.releaseVersion ?? protocol.releaseVersion

  const live = liveQuery.data?.status === 'ready' ? liveQuery.data : null
  const publication = publicationQuery.data
  const current = live?.standings?.find(
    (row) => row.address.toLowerCase() === address.toLowerCase(),
  )
  const awarded = publication?.standings.find(
    (row) => row.address.toLowerCase() === address.toLowerCase(),
  )
  const handle =
    summary?.identity.discordUsername ?? current?.discordUsername ?? awarded?.displayName
  const traderLabel = handle ? `@${handle.replace(/^@/, '')}` : shortAddress(address)

  const ledger = summary?.ledger ?? null
  const position = ledger?.hasPosition ? ledger.position : null
  const perf = summary?.performance ?? null
  const protections = summary?.protections.items ?? []
  const lpBalances = summary?.lp.balances ?? []
  const lpRequestStates = summary?.lp.requestStates ?? []
  const lpRequestEvents = summary?.lp.requestEvents ?? []
  const divisionsHistory = summary?.divisionsHistory ?? []

  const latestClosedTrade = (summary?.activity ?? []).find(
    (row) => row.activityType === 'Close' && row.pnlAssets != null,
  )

  const enrichedBalanceHistory = enrichBalanceHistory(
    summary?.balanceHistory ?? [],
    summary?.activity ?? [],
  )

  const shareCards: ShareCardOption[] = []
  if (summary) {
    const balanceHistory = enrichedBalanceHistory
    const latestHistoryBalance = balanceHistory[balanceHistory.length - 1]?.balanceAssets ?? null
    const accountBalanceAssets =
      ledger?.settlementBalanceAssets ??
      (ledger?.netEquityAssets && ledger.netEquityAssets !== '0' ? ledger.netEquityAssets : null) ??
      latestHistoryBalance
    shareCards.push({
      id: 'account-summary',
      label: 'Account Overview',
      data: {
        kind: 'account',
        traderLabel,
        account: address,
        networkName: chain.name,
        releaseVersion,
        netEquityAssets: ledger?.netEquityAssets ?? null,
        accountBalanceAssets,
        realizedPnlAssets: perf?.realizedPnlAssets ?? null,
        volumeAssets: perf?.volumeAssets ?? null,
        totalTrades: perf?.totalTrades ?? null,
        activeDays: perf?.activeDays ?? null,
        firstActivityAt: perf?.firstActivityAt ?? null,
        tier: awarded?.tier ?? null,
        balancePoints: balanceHistory.map((pt) => ({
          timestamp: pt.timestamp,
          balanceAssets: pt.balanceAssets,
        })),
        sparklineValues: balanceHistory
          .map((pt) => Number(pt.balanceAssets) / 1_000_000)
          .filter((v) => Number.isFinite(v)),
      },
    })
  }
  if (position) {
    shareCards.push({
      id: 'open-position',
      label: 'Open Position',
      data: {
        kind: 'position',
        traderLabel,
        account: address,
        networkName: chain.name,
        releaseVersion,
        market: position.market,
        side: position.side,
        size: position.size,
        entryPrice: position.entryPrice,
        marginAssets: position.marginAssets,
        unrealizedPnlAssets: position.unrealizedPnlAssets,
        liquidatable: position.liquidatable,
      },
    })
  }
  if (latestClosedTrade?.pnlAssets) {
    shareCards.push({
      id: `trade-${latestClosedTrade.id}`,
      label: 'Latest Closed PnL',
      data: {
        kind: 'trade',
        traderLabel,
        account: address,
        networkName: chain.name,
        releaseVersion,
        market: 'plDXY-PERP',
        side:
          latestClosedTrade.side === 'long' || latestClosedTrade.side === 'short'
            ? latestClosedTrade.side
            : null,
        realizedPnlAssets: latestClosedTrade.pnlAssets,
        price: latestClosedTrade.price,
        sizeDelta: latestClosedTrade.sizeDelta,
        notionalAssets: latestClosedTrade.amountAssets,
        occurredAt: latestClosedTrade.occurredAt,
        txHash: latestClosedTrade.txHash,
      },
    })
  }

  const filteredActivity: AccountActivityRecord[] = (summary?.activity ?? []).filter((row) => {
    if (activityFilter === 'all') return true
    if (activityFilter === 'other') {
      return !['Open', 'Close', 'Liquidated'].includes(row.activityType)
    }
    return row.activityType === activityFilter
  })

  const bannerTone: StatusTone = awaitingDeploy
    ? 'unknown'
    : summaryQuery.isError
      ? 'blocked'
      : summary
        ? summary.ledgerAvailable
          ? position?.liquidatable
            ? 'degraded'
            : 'live'
          : 'degraded'
        : summaryQuery.isLoading
          ? 'unknown'
          : 'stale'

  const bannerLabel = awaitingDeploy
    ? 'Awaiting Stage 3 backend deploy'
    : summaryQuery.isError
      ? 'Account summary unavailable'
      : summary
        ? summary.ledgerAvailable
          ? position?.liquidatable
            ? 'Position liquidatable'
            : 'Live account telemetry'
          : 'Live ledger RPC unavailable'
        : 'Loading Trading Account'

  const tabs: { id: InspectorTab; label: string; count?: number }[] = [
    { id: 'overview', label: 'Overview & Margin' },
    {
      id: 'exposure',
      label: 'Market Exposure',
      count: exposure?.summary.totalPositions,
    },
    { id: 'trades', label: 'Trades & Receipts', count: summary?.activity.length },
    { id: 'protections', label: 'TP/SL Protections', count: protections.length },
    {
      id: 'lp',
      label: 'LP Vault Activity',
      count: lpBalances.length + lpRequestStates.filter((r) => r.isActive).length,
    },
    { id: 'divisions', label: 'Divisions History', count: divisionsHistory.length || (awarded ? 1 : 0) },
  ]

  return (
    <div className="space-y-8">
      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={protocol.blockNumber}
        observedAt={summary?.observedAt ?? protocol.observedAt}
        tone={bannerTone}
        statusLabel={bannerLabel}
        note="Direct Trading Account lookup · Read-only public forensics"
      />

      <div className="flex flex-wrap items-center justify-between gap-3 text-sm">
        <Link to="/" className="text-brand-peach hover:underline">
          ← Protocol Pulse
        </Link>
        <div className="flex flex-wrap gap-4">
          <Link to="/execution" className="text-content-secondary hover:text-brand-peach">
            Execution tape →
          </Link>
          <Link to="/divisions" className="text-content-secondary hover:text-brand-peach">
            Divisions leaderboard →
          </Link>
        </div>
      </div>

      <header className="flex flex-col items-start justify-between gap-4 border border-brand-border/25 bg-surface-panel p-4 sm:flex-row sm:p-6">
        <div className="min-w-0 max-w-full">
          <div className="flex flex-wrap items-center gap-2">
            <p className="text-xs font-semibold uppercase tracking-[0.16em] text-brand-peach">
              Public Trading Account Inspector
            </p>
            <span className="border border-brand-border/25 bg-app-bg px-2 py-0.5 font-mono text-xs text-content-secondary">
              {chain.name}
            </span>
            {summary?.identity.isDivisionsLinked ? (
              <StatusPill tone="live" label="Discord linked" />
            ) : null}
          </div>
          <div className="mt-3 flex items-center gap-3">
            <AddressAvatar address={address} size={40} className="shrink-0" />
            <h1 className="break-all text-2xl font-semibold sm:text-4xl">
              {handle ? `@${handle.replace(/^@/, '')}` : shortAddress(address)}
            </h1>
          </div>
          <div className="mt-2 flex flex-wrap items-center gap-1.5">
            <p className="break-all font-mono text-xs text-content-secondary sm:text-sm">
              {address}
            </p>
            <CopyButton value={address} />
            <ArbiscanButton kind="address" value={address} chainId={chainId} />
          </div>
          <div className="mt-3 flex flex-wrap gap-4 text-xs">
            <Link
              to={`/trading-accounts/${address}`}
              className="text-brand-peach hover:underline"
            >
              Kernel v4 Smart Account →
            </Link>
            <Link
              to={`/divisions/wallets/${address}`}
              className="text-content-secondary hover:text-brand-peach"
            >
              Divisions card view →
            </Link>
          </div>
        </div>
        <div className="flex flex-col items-start gap-3 sm:items-end">
          <div className="space-y-1 sm:text-right">
            <p className="text-xs text-content-tertiary">Current Divisions badge</p>
            <CurrentBadge
              tier={awarded?.tier}
              loading={publicationQuery.isLoading}
              unavailable={publicationQuery.isError && !publication}
            />
          </div>
          {shareCards.length > 0 ? (
            <ShareCardModal cards={shareCards} triggerLabel="Share card" />
          ) : null}
        </div>
      </header>

      {awaitingDeploy ? (
        <div
          role="status"
          className="border border-brand-yellow/40 bg-brand-yellow/10 px-4 py-3 text-sm text-brand-yellow"
        >
          <strong>Connected backend does not serve Stage 3 Account Summary yet (HTTP 404).</strong>{' '}
          Visit the{' '}
          <Link to={`/divisions/wallets/${address}`} className="text-brand-peach underline">
            Divisions Scorecard
          </Link>{' '}
          for weekly competition standings, or deploy the updated backend containing{' '}
          <code className="font-mono text-xs">/api/insights/v1/wallets/:address/summary</code> to
          hydrate live on-chain margin health, trade receipts, TP/SL protections, and attributed LP
          vault activity.
        </div>
      ) : null}

      <section aria-labelledby="account-kpis-title">
        <h2 id="account-kpis-title" className="sr-only">
          Trading Account headline metrics
        </h2>
        <Panel className="overflow-hidden">
          {summaryQuery.isLoading ? (
            <div className="p-5">
              <LoadingState rows={3} />
            </div>
          ) : (
            <>
              <div className="flex flex-wrap items-end justify-between gap-4 p-4 sm:p-6">
                <div>
                  <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                    Net Account Equity
                  </p>
                  <p className="mt-1 font-mono text-3xl font-bold tracking-tight tabular-nums text-content-primary sm:text-4xl">
                    {ledger ? formatAssets(ledger.netEquityAssets) : 'Unavailable'}
                  </p>
                  <p className="mt-1 font-mono text-xs text-content-tertiary">
                    {ledger
                      ? `Free collateral ${formatCompactAssets(ledger.freeSettlementAssets)} · Pending orders: ${String(ledger.pendingOrderCount)}`
                      : 'Live CfdEngineAccountLens read'}
                  </p>
                </div>

                <div className="text-left font-mono sm:text-right">
                  <p className="text-xs text-content-tertiary">Open Position</p>
                  <p
                    className={`mt-1 text-xl font-bold tabular-nums ${
                      position?.liquidatable
                        ? 'text-brand-orange'
                        : position
                          ? 'text-positive'
                          : 'text-content-secondary'
                    }`}
                  >
                    {ledger
                      ? position
                        ? `${position.side.toUpperCase()} · ${formatPlDxyQuantity(position.size)}`
                        : 'Flat'
                      : 'Unavailable'}
                  </p>
                  <p className="mt-0.5 text-xs text-content-tertiary">
                    {position
                      ? `Entry ${formatPrice(position.entryPrice)} · Margin ${formatCompactAssets(position.marginAssets)}${position.liquidatable ? ' · LIQUIDATABLE' : ''}`
                      : ledger
                        ? 'No active perpetual position'
                        : 'Position size, entry & liquidation status'}
                  </p>
                </div>
              </div>

              <dl className="grid grid-cols-1 divide-y divide-brand-border/15 border-t border-brand-border/20 bg-app-bg/35 sm:grid-cols-2 sm:divide-x sm:divide-y-0">
                <div className="px-4 py-3.5 sm:px-6">
                  <dt className="text-xs text-content-tertiary">
                    Realized PnL ({releaseVersion ?? 'current'})
                  </dt>
                  <dd className="mt-1 font-mono text-lg font-bold tabular-nums">
                    {perf ? <Pnl value={perf.realizedPnlAssets} /> : <span className="text-sm font-normal text-content-tertiary">Unavailable</span>}
                  </dd>
                  <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
                    {perf
                      ? `${String(perf.totalTrades)} fills (${String(perf.opensCount)} open / ${String(perf.closesCount)} close / ${String(perf.liquidationsCount)} liq)`
                      : 'Release-scoped realized PnL'}
                  </p>
                </div>
                <div className="px-4 py-3.5 sm:px-6">
                  <dt className="text-xs text-content-tertiary">
                    Trading Volume ({releaseVersion ?? 'current'})
                  </dt>
                  <dd className="mt-1 font-mono text-lg font-bold tabular-nums text-content-primary">
                    {perf ? formatCompactAssets(perf.volumeAssets) : <span className="text-sm font-normal text-content-tertiary">Unavailable</span>}
                  </dd>
                  <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
                    {perf
                      ? `${String(perf.activeDays)} active days · ${String(lpBalances.length)} attributed LP vault${lpBalances.length === 1 ? '' : 's'}`
                      : 'Notional volume & LP vault holdings'}
                  </p>
                </div>
              </dl>
            </>
          )}
        </Panel>
      </section>

      <div
        role="tablist"
        aria-label="Trading Account sections"
        className="flex flex-wrap gap-1 border-b border-brand-border/25 pb-2"
      >
        {tabs.map((tab) => {
          const isSelected = activeTab === tab.id
          return (
            <button
              key={tab.id}
              type="button"
              role="tab"
              aria-selected={isSelected}
              onClick={() => {
                const next = new URLSearchParams(searchParams)
                if (tab.id === 'overview') {
                  next.delete('tab')
                } else {
                  next.set('tab', tab.id)
                }
                setSearchParams(next, { replace: true })
              }}
              className={`border px-3.5 py-2 text-xs font-semibold transition-colors ${
                isSelected
                  ? 'border-brand-orange bg-brand-orange text-content-primary'
                  : 'border-brand-border/25 bg-surface-panel text-content-secondary hover:border-brand-orange/50 hover:text-content-primary'
              }`}
            >
              {tab.label}
              {tab.count !== undefined ? (
                <span className="ml-1.5 font-mono text-[11px] opacity-80">({tab.count})</span>
              ) : null}
            </button>
          )
        })}
      </div>

      {activeTab === 'overview' ? (
        <div role="tabpanel" className="space-y-8">
          <WalletBalanceChart
            points={enrichedBalanceHistory}
            activity={summary?.activity ?? []}
            observedAt={summary?.observedAt ?? protocol.observedAt}
            releaseVersion={releaseVersion}
            loading={summaryQuery.isLoading}
            awaitingDeploy={awaitingDeploy}
          />

          <section aria-labelledby="account-ledger-title" className="space-y-4">
            <div className="flex flex-wrap items-baseline justify-between gap-2">
              <h2 id="account-ledger-title" className="text-xl font-semibold">
                On-chain margin &amp; position health
              </h2>
              <span className="font-mono text-xs text-content-tertiary">
                CfdEngineAccountLens.getAccountLedgerSnapshot
              </span>
            </div>

            {summaryQuery.isLoading ? (
              <Panel>
                <LoadingState rows={3} />
              </Panel>
            ) : summaryQuery.isError && !awaitingDeploy ? (
              <ErrorState
                title="Account telemetry unavailable"
                message={summaryQuery.error.message}
                onRetry={() => void summaryQuery.refetch()}
              />
            ) : ledger ? (
              <div className="grid gap-4 lg:grid-cols-2">
                <Panel>
                  <div className="border-b border-brand-border/15 px-5 py-3 text-xs font-semibold uppercase tracking-wider text-content-tertiary">
                    Collateral &amp; margin buckets
                  </div>
                  <CollateralAllocationBar
                    freeAssets={ledger.freeSettlementAssets}
                    positionMarginAssets={ledger.activePositionMarginAssets}
                    committedOrderAssets={ledger.committedMarginAssets}
                    otherLockedAssets={
                      parseUnsignedBigInt(ledger.executionBountyReserveAssets) +
                      parseUnsignedBigInt(ledger.otherLockedMarginAssets)
                    }
                  />
                  <dl className="grid grid-cols-1 divide-y divide-brand-border/15 sm:grid-cols-2 sm:divide-y-0">
                    <Metric label="Settlement balance">
                      {formatAssets(ledger.settlementBalanceAssets)}
                    </Metric>
                    <Metric label="Free collateral">
                      {formatAssets(ledger.freeSettlementAssets)}
                    </Metric>
                    <Metric label="Active position margin">
                      {formatAssets(ledger.activePositionMarginAssets)}
                    </Metric>
                    <Metric label="Committed order margin">
                      {formatAssets(ledger.committedMarginAssets)}
                    </Metric>
                    <Metric label="Free buying power">
                      {formatAssets(ledger.freeBuyingPowerAssets)}
                    </Metric>
                    <Metric label="Trader claim balance">
                      {formatAssets(ledger.traderClaimBalanceAssets)}
                    </Metric>
                  </dl>
                </Panel>

                <Panel>
                  <div className="flex flex-wrap items-center justify-between gap-2 border-b border-brand-border/15 px-5 py-3">
                    <span className="text-xs font-semibold uppercase tracking-wider text-content-tertiary">
                      Open perpetual position
                    </span>
                    <div className="flex items-center gap-2">
                      {position ? (
                        <>
                          <StatusPill
                            tone={position.liquidatable ? 'blocked' : 'live'}
                            label={position.liquidatable ? 'Liquidatable' : `${position.side.toUpperCase()} active`}
                          />
                          <ShareCardModal
                            compact
                            triggerLabel="Share position"
                            cards={shareCards}
                            initialCardId="open-position"
                          />
                        </>
                      ) : (
                        <StatusPill tone="unknown" label="Flat" />
                      )}
                    </div>
                  </div>
                  {position ? (
                    <>
                      <dl className="grid grid-cols-1 divide-y divide-brand-border/15 sm:grid-cols-2 sm:divide-y-0">
                        <Metric label="Market">{position.market}</Metric>
                        <Metric label="Size">{formatPlDxyQuantity(position.size)}</Metric>
                        <Metric label="Entry price">{formatPrice(position.entryPrice)}</Metric>
                        <Metric label="Position margin">
                          {formatAssets(position.marginAssets)}
                        </Metric>
                        <Metric label="Unrealized PnL">
                          <Pnl value={position.unrealizedPnlAssets} />
                        </Metric>
                        <Metric label="Execution bounty reserve">
                          {formatAssets(ledger.executionBountyReserveAssets)}
                        </Metric>
                      </dl>
                      <ActivePositionProtectionStrip protections={protections} />
                      {exposure?.activeExposure ? (
                        <ActivePositionRiskStrip
                          address={address}
                          exposure={exposure.activeExposure}
                        />
                      ) : null}
                    </>
                  ) : (
                    <EmptyState
                      title="No open position"
                      message="This Trading Account currently holds zero open plDXY-PERP exposure."
                    />
                  )}
                </Panel>
              </div>
            ) : (
              <Panel>
                <EmptyState
                  title={
                    awaitingDeploy
                      ? 'Live margin snapshot awaiting Stage 3 backend'
                      : 'On-chain ledger snapshot unavailable'
                  }
                  message="We fail closed rather than manufacturing zeros when the on-chain CfdEngineAccountLens call is unavailable."
                />
              </Panel>
            )}
          </section>
        </div>
      ) : null}

      {activeTab === 'exposure' ? (
        <div role="tabpanel">
          <WalletExposureSection
            address={address}
            data={exposure}
            isLoading={exposureQuery.isLoading}
            isError={exposureQuery.isError}
            awaitingDeploy={awaitingExposureDeploy}
            errorMessage={
              exposureQuery.error instanceof Error ? exposureQuery.error.message : undefined
            }
            releaseVersion={releaseVersion}
            onRetry={() => void exposureQuery.refetch()}
          />
        </div>
      ) : null}

      {activeTab === 'trades' ? (
        <section role="tabpanel" aria-labelledby="account-trades-title" className="space-y-4">
          <div className="flex flex-col justify-between gap-3 sm:flex-row sm:items-end">
            <div>
              <h2 id="account-trades-title" className="text-xl font-semibold">
                Account activity &amp; trade receipts
              </h2>
              <p className="mt-1 text-xs text-content-tertiary">
                Canonical {releaseVersion} fills, liquidations, and transfers with full execution
                receipt reconciliation
              </p>
            </div>
            <div className="flex flex-wrap gap-1.5" role="group" aria-label="Filter account activity">
              {([
                ['all', 'All'],
                ['Open', 'Opens'],
                ['Close', 'Closes'],
                ['Liquidated', 'Liquidations'],
                ['other', 'Transfers'],
              ] as const).map(([option, label]) => (
                <button
                  key={option}
                  type="button"
                  aria-pressed={activityFilter === option}
                  onClick={() => {
                    setActivityFilter(option)
                  }}
                  className={`border px-3 py-1.5 text-xs font-semibold transition-colors ${
                    activityFilter === option
                      ? 'border-brand-orange bg-brand-orange text-content-primary'
                      : 'border-brand-border/30 bg-surface-panel text-content-secondary hover:border-brand-orange/50 hover:text-content-primary'
                  }`}
                >
                  {label}
                </button>
              ))}
            </div>
          </div>

          <Panel>
            {summaryQuery.isLoading ? (
              <LoadingState rows={4} />
            ) : filteredActivity.length > 0 ? (
              <div className="overflow-x-auto">
                <table className="w-full min-w-[880px] text-left text-sm">
                  <caption className="sr-only">Trading Account activity and receipts</caption>
                  <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                    <tr>
                      <th scope="col" className="px-4 py-3.5">Time (UTC)</th>
                      <th scope="col" className="px-4 py-3.5">Type</th>
                      <th scope="col" className="px-4 py-3.5">Side</th>
                      <th scope="col" className="px-4 py-3.5">Price</th>
                      <th scope="col" className="px-4 py-3.5">Amount</th>
                      <th scope="col" className="px-4 py-3.5">Realized PnL</th>
                      <th scope="col" className="px-4 py-3.5">VPI / Spread</th>
                      <th scope="col" className="px-4 py-3.5">Protections</th>
                      <th scope="col" className="px-4 py-3.5">Receipt</th>
                      <th scope="col" className="px-4 py-3.5">Tx</th>
                    </tr>
                  </thead>
                  <tbody>
                    {(() => {
                      const seenTradeTxKeys = new Set<string>()
                      return filteredActivity.map((row) => {
                        const hasReceipt =
                          row.execution != null &&
                          (row.activityType === 'Open' || row.activityType === 'Close')
                        const txKey = arbiscanLinkKey('tx', row.txHash, chainId)
                        const showArbiscan = !seenTradeTxKeys.has(txKey)
                        if (showArbiscan) seenTradeTxKeys.add(txKey)
                        return (
                        <tr key={row.id} className="border-b border-brand-border/15 last:border-0">
                          <td className="px-4 py-3.5 text-xs text-content-secondary">
                            {formatUtc(row.occurredAt)}
                          </td>
                          <td className="px-4 py-3.5">
                            <StatusPill tone={activityTone(row.activityType)} label={row.activityType} />
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs uppercase">
                            {row.side ? (
                              <span
                                className={
                                  row.side === 'long'
                                    ? 'text-positive'
                                    : row.side === 'short'
                                      ? 'text-brand-orange'
                                      : 'text-content-tertiary'
                                }
                              >
                                {row.side}
                              </span>
                            ) : (
                              '—'
                            )}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            {row.price ? formatPrice(row.price) : '—'}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            {formatCompactAssets(row.amountAssets)}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs">
                            {row.pnlAssets != null ? <Pnl value={row.pnlAssets} /> : '—'}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            {row.vpiAssets != null
                              ? formatCompactAssets(row.vpiAssets)
                              : row.frozenCloseSpreadAssets != null
                                ? `Spread ${formatCompactAssets(row.frozenCloseSpreadAssets)}`
                                : '—'}
                          </td>
                          <td className="px-4 py-3.5">
                            {row.activityType === 'Open' ? (
                              <OrderProtectionsInlineChecklist protections={row.orderProtections} />
                            ) : (
                              <span className="font-mono text-xs text-content-tertiary">—</span>
                            )}
                          </td>
                          <td className="px-4 py-3.5">
                            <div className="flex flex-wrap items-center gap-1.5">
                              {hasReceipt ? (
                                <Link
                                  to={transactionPath(row.txHash, row.id)}
                                  state={
                                    {
                                      trade: accountActivityToProtocolTrade(row, address),
                                      chainId,
                                      releaseVersion,
                                      traderLabel,
                                      from: `/wallets/${address}`,
                                      fromLabel: `Back to ${traderLabel}`,
                                    } satisfies TransactionRouteState
                                  }
                                  className="inline-block border border-brand-border/40 px-2.5 py-1 text-xs font-semibold text-brand-peach transition-colors hover:bg-brand-peach/10"
                                >
                                  View breakdown
                                </Link>
                              ) : null}
                              {row.activityType === 'Close' && row.pnlAssets != null ? (
                                <ShareCardModal
                                  compact
                                  triggerLabel="Share PnL"
                                  cards={[
                                    {
                                      id: `trade-${row.id}`,
                                      label: 'Closed Position PnL',
                                      data: {
                                        kind: 'trade',
                                        traderLabel,
                                        account: address,
                                        networkName: chain.name,
                                        releaseVersion,
                                        market: 'plDXY-PERP',
                                        side:
                                          row.side === 'long' || row.side === 'short'
                                            ? row.side
                                            : null,
                                        realizedPnlAssets: row.pnlAssets,
                                        price: row.price,
                                        sizeDelta: row.sizeDelta,
                                        notionalAssets: row.amountAssets,
                                        occurredAt: row.occurredAt,
                                        txHash: row.txHash,
                                      },
                                    },
                                  ]}
                                />
                              ) : null}
                              {row.activityType === 'Open' ? (
                                <ShareCardModal
                                  compact
                                  triggerLabel="Share Open"
                                  cards={[
                                    {
                                      id: `open-${row.id}`,
                                      label: 'Opened Position Receipt',
                                      data: {
                                        kind: 'open_trade',
                                        traderLabel,
                                        account: address,
                                        networkName: chain.name,
                                        releaseVersion,
                                        market: 'plDXY-PERP',
                                        side:
                                          row.side === 'long' || row.side === 'short'
                                            ? row.side
                                            : null,
                                        price: row.price,
                                        sizeDelta: row.sizeDelta,
                                        notionalAssets: row.amountAssets,
                                        occurredAt: row.occurredAt,
                                        txHash: row.txHash,
                                      },
                                    },
                                  ]}
                                />
                              ) : null}
                              {!hasReceipt &&
                              !(row.activityType === 'Close' && row.pnlAssets != null) &&
                              row.activityType !== 'Open' ? (
                                <span className="font-mono text-xs text-content-tertiary">—</span>
                              ) : null}
                            </div>
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs">
                            <div className="inline-flex items-center gap-1.5 text-content-secondary">
                              <span title={row.txHash}>{shortAddress(row.txHash)}</span>
                              {showArbiscan ? (
                                <ArbiscanButton kind="tx" value={row.txHash} chainId={chainId} />
                              ) : null}
                            </div>
                          </td>
                        </tr>
                      )
                    })})()}
                  </tbody>
                </table>
              </div>
            ) : (
              <EmptyState
                title={summary ? 'No matching activity' : 'Trade history awaiting Stage 3 backend'}
                message={
                  summary
                    ? 'No indexed fills or transfers match the active filter for this Trading Account.'
                    : 'Deploy /api/insights/v1/wallets/:address/summary to inspect paginated trade receipts.'
                }
              />
            )}
          </Panel>
        </section>
      ) : null}

      {activeTab === 'protections' ? (
        <section role="tabpanel" aria-labelledby="account-protections-title" className="space-y-4">
          <div className="flex flex-wrap items-baseline justify-between gap-2">
            <h2 id="account-protections-title" className="text-xl font-semibold">
              TP/SL Position Protections
            </h2>
            {summary && summary.protections.indexedThroughBlock !== '0' ? (
              <span className="font-mono text-xs text-content-tertiary">
                Indexed through block{' '}
                {Number(summary.protections.indexedThroughBlock).toLocaleString('en-US')}
              </span>
            ) : null}
          </div>

          <Panel>
            {summaryQuery.isLoading ? (
              <LoadingState rows={3} />
            ) : protections.length > 0 ? (
              <div className="overflow-x-auto">
                <table className="w-full min-w-[780px] text-left text-sm">
                  <caption className="sr-only">TP/SL protections for this Trading Account</caption>
                  <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                    <tr>
                      <th scope="col" className="px-4 py-3.5">Protection ID</th>
                      <th scope="col" className="px-4 py-3.5">Status</th>
                      <th scope="col" className="px-4 py-3.5">Open Order</th>
                      <th scope="col" className="px-4 py-3.5">Take Profit (TP)</th>
                      <th scope="col" className="px-4 py-3.5">Stop Loss (SL)</th>
                      <th scope="col" className="px-4 py-3.5">Triggered leg</th>
                      <th scope="col" className="px-4 py-3.5">Updated block</th>
                    </tr>
                  </thead>
                  <tbody>
                    {protections.map((item, idx) => {
                      const p = normalizeAccountProtection(item, String(idx + 1))
                      return (
                        <tr
                          key={`${p.protectionId}-${String(idx)}`}
                          className="border-b border-brand-border/15 last:border-0"
                        >
                          <td className="px-4 py-3.5 font-mono text-xs">#{p.protectionId}</td>
                          <td className="px-4 py-3.5">
                            <StatusPill tone={protectionLifecycleTone(p.status)} label={p.status} />
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs text-content-secondary">
                            {p.parentOrderId ? `#${p.parentOrderId}` : 'Standalone'}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            {p.takeProfitTriggerPrice ? (
                              <span className="text-positive">
                                ✓ {formatPrice(p.takeProfitTriggerPrice)}
                              </span>
                            ) : (
                              <span className="text-content-tertiary">○ Not set</span>
                            )}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            {p.stopLossTriggerPrice ? (
                              <span className="text-brand-peach">
                                ✓ {formatPrice(p.stopLossTriggerPrice)}
                              </span>
                            ) : (
                              <span className="text-content-tertiary">○ Not set</span>
                            )}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs">
                            {p.triggeredLeg ? (
                              <span className="text-content-primary">
                                {p.triggeredLeg}
                                {p.linkedOrderId ? ` (#${p.linkedOrderId})` : ''}
                              </span>
                            ) : (
                              <span className="text-content-tertiary">—</span>
                            )}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            {p.updatedBlock ? Number(p.updatedBlock).toLocaleString('en-US') : '—'}
                          </td>
                        </tr>
                      )
                    })}
                  </tbody>
                </table>
              </div>
            ) : (
              <EmptyState
                title={summary ? 'No TP/SL protections recorded' : 'Protections awaiting Stage 3 backend'}
                message={
                  summary
                    ? 'This Trading Account has no indexed PositionProtectionBook orders on the current release.'
                    : 'Deploy /api/insights/v1/wallets/:address/summary to inspect TP/SL protection states.'
                }
              />
            )}
          </Panel>
        </section>
      ) : null}

      {activeTab === 'lp' ? (
        <section role="tabpanel" aria-labelledby="account-lp-title" className="space-y-6">
          <div className="flex flex-wrap items-baseline justify-between gap-2">
            <div>
              <h2 id="account-lp-title" className="text-xl font-semibold">
                Attributed LP Vault Shares &amp; Async Requests
              </h2>
              <p className="mt-1 text-xs text-content-tertiary">
                Canonical Senior &amp; Junior ERC-7540 share balances, unclaimed deposit shares, and
                withdrawal escrow
              </p>
            </div>
            <a
              href={vaultsAppUrl(chainId)}
              className="text-xs font-semibold text-brand-peach hover:underline"
            >
              Manage LP deposits on Main App ↗
            </a>
          </div>

          <Panel>
            <div className="border-b border-brand-border/15 px-5 py-3 text-xs font-semibold uppercase tracking-wider text-content-tertiary">
              Attributed share balances
            </div>
            {lpBalances.length > 0 ? (
              <div className="overflow-x-auto">
                <table className="w-full min-w-[720px] text-left text-sm">
                  <caption className="sr-only">Attributed Senior and Junior vault share balances</caption>
                  <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                    <tr>
                      <th scope="col" className="px-4 py-3.5">Tranche</th>
                      <th scope="col" className="px-4 py-3.5">Held shares</th>
                      <th scope="col" className="px-4 py-3.5">Unclaimed deposit</th>
                      <th scope="col" className="px-4 py-3.5">Withdrawal escrow</th>
                      <th scope="col" className="px-4 py-3.5">Total attributed</th>
                      <th scope="col" className="px-4 py-3.5">Observed block</th>
                    </tr>
                  </thead>
                  <tbody>
                    {lpBalances.map((bal) => (
                      <tr
                        key={bal.vaultAddress}
                        className="border-b border-brand-border/15 last:border-0"
                      >
                        <td className="px-4 py-3.5 font-semibold">{trancheLabel(bal.tranche)}</td>
                        <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                          {formatVaultShares(bal.shareBalance)}
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                          {formatVaultShares(bal.unclaimedDepositShares)}
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                          {formatVaultShares(bal.withdrawalEscrowShares)}
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs font-semibold text-brand-peach tabular-nums">
                          {formatVaultShares(bal.totalAttributedShares)}
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                          {Number(bal.observedBlock).toLocaleString('en-US')}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            ) : (
              <EmptyState
                title={summary ? 'No attributed LP vault shares' : 'LP balances awaiting Stage 3 backend'}
                message="This address does not currently hold attributed Senior or Junior vault shares on the active release."
              />
            )}
          </Panel>

          <Panel>
            <div className="border-b border-brand-border/15 px-5 py-3 text-xs font-semibold uppercase tracking-wider text-content-tertiary">
              Async deposit &amp; redeem requests ({lpRequestStates.length} states · {lpRequestEvents.length} events)
            </div>
            {lpRequestStates.length > 0 ? (
              <div className="overflow-x-auto">
                <table className="w-full min-w-[760px] text-left text-sm">
                  <caption className="sr-only">Async LP deposit and redeem request states</caption>
                  <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                    <tr>
                      <th scope="col" className="px-4 py-3.5">Tranche</th>
                      <th scope="col" className="px-4 py-3.5">Request ID</th>
                      <th scope="col" className="px-4 py-3.5">Status</th>
                      <th scope="col" className="px-4 py-3.5">Pending deposit</th>
                      <th scope="col" className="px-4 py-3.5">Claimable shares</th>
                      <th scope="col" className="px-4 py-3.5">Pending redeem</th>
                    </tr>
                  </thead>
                  <tbody>
                    {lpRequestStates.map((req) => (
                      <tr
                        key={`${req.vaultAddress}-${req.requestId}`}
                        className="border-b border-brand-border/15 last:border-0"
                      >
                        <td className="px-4 py-3.5 font-semibold">{trancheLabel(req.tranche)}</td>
                        <td className="px-4 py-3.5 font-mono text-xs">#{req.requestId}</td>
                        <td className="px-4 py-3.5">
                          <StatusPill
                            tone={req.isActive ? 'live' : 'unknown'}
                            label={req.isActive ? 'Active' : 'Settled'}
                          />
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                          {formatCompactAssets(req.pendingDepositAssets)}
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                          {formatVaultShares(req.claimableDepositShares)}
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                          {formatVaultShares(req.pendingRedeemShares)}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            ) : lpRequestEvents.length > 0 ? (
              <div className="overflow-x-auto">
                <table className="w-full min-w-[700px] text-left text-sm">
                  <caption className="sr-only">Recent LP vault request events</caption>
                  <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                    <tr>
                      <th scope="col" className="px-4 py-3.5">Time (UTC)</th>
                      <th scope="col" className="px-4 py-3.5">Tranche</th>
                      <th scope="col" className="px-4 py-3.5">Event</th>
                      <th scope="col" className="px-4 py-3.5">Request ID</th>
                      <th scope="col" className="px-4 py-3.5">Tx</th>
                    </tr>
                  </thead>
                  <tbody>
                    {(() => {
                      const seenLpTxKeys = new Set<string>()
                      return lpRequestEvents.map((ev) => {
                        const txKey = arbiscanLinkKey('tx', ev.txHash, chainId)
                        const showArbiscan = !seenLpTxKeys.has(txKey)
                        if (showArbiscan) seenLpTxKeys.add(txKey)
                        return (
                          <tr
                            key={`${ev.txHash}-${ev.requestId}-${ev.eventName}`}
                            className="border-b border-brand-border/15 last:border-0"
                          >
                            <td className="px-4 py-3.5 text-xs text-content-secondary">
                              {formatUtc(ev.occurredAt)}
                            </td>
                            <td className="px-4 py-3.5 font-semibold">{trancheLabel(ev.tranche)}</td>
                            <td className="px-4 py-3.5 font-mono text-xs">{ev.eventName}</td>
                            <td className="px-4 py-3.5 font-mono text-xs">#{ev.requestId}</td>
                            <td className="px-4 py-3.5 font-mono text-xs">
                              <div className="inline-flex items-center gap-1.5 text-content-secondary">
                                <span title={ev.txHash}>{shortAddress(ev.txHash)}</span>
                                {showArbiscan ? (
                                  <ArbiscanButton kind="tx" value={ev.txHash} chainId={chainId} />
                                ) : null}
                              </div>
                            </td>
                          </tr>
                        )
                      })
                    })()}
                  </tbody>
                </table>
              </div>
            ) : (
              <EmptyState
                title="No LP request history"
                message="No Senior or Junior async deposit/redeem requests have been indexed for this address."
              />
            )}
          </Panel>
        </section>
      ) : null}

      {activeTab === 'divisions' ? (
        <section role="tabpanel" aria-labelledby="account-divisions-history-title" className="space-y-4">
          <h2 id="account-divisions-history-title" className="text-xl font-semibold">
            Published Divisions history
          </h2>

          {divisionsHistory.length > 0 ? (
            <Panel>
              <div className="overflow-x-auto">
                <table className="w-full min-w-[760px] text-left text-sm">
                  <caption className="sr-only">Historical published Divisions weeks</caption>
                  <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                    <tr>
                      <th scope="col" className="px-4 py-3.5">Week ending</th>
                      <th scope="col" className="px-4 py-3.5">Rank</th>
                      <th scope="col" className="px-4 py-3.5">Tier</th>
                      <th scope="col" className="px-4 py-3.5">Movement</th>
                      <th scope="col" className="px-4 py-3.5">Weekly PnL</th>
                      <th scope="col" className="px-4 py-3.5">Active days</th>
                      <th scope="col" className="px-4 py-3.5">Qualifying</th>
                    </tr>
                  </thead>
                  <tbody>
                    {divisionsHistory.map((week) => (
                      <tr
                        key={week.weekStart}
                        className="border-b border-brand-border/15 last:border-0"
                      >
                        <td className="px-4 py-3.5 text-xs text-content-secondary">
                          {week.weekClosesAt ? formatUtc(week.weekClosesAt) : String(week.weekStart)}
                        </td>
                        <td className="px-4 py-3.5 font-mono tabular-nums">
                          {week.rank !== null ? `#${String(week.rank)}` : '—'}
                        </td>
                        <td className="px-4 py-3.5 font-semibold capitalize">
                          {week.tier ?? '—'}
                        </td>
                        <td className="px-4 py-3.5 text-xs capitalize text-content-secondary">
                          {week.movement ?? '—'}
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs">
                          {week.weeklyPnlAssets ? <Pnl value={week.weeklyPnlAssets} /> : '—'}
                        </td>
                        <td className="px-4 py-3.5 font-mono tabular-nums">
                          {week.activeDays ?? '—'}
                        </td>
                        <td className="px-4 py-3.5">
                          <StatusPill
                            tone={week.qualifying ? 'ready' : 'unknown'}
                            label={week.qualifying ? 'Qualifying' : 'Non-qualifying'}
                          />
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </Panel>
          ) : awarded && publication ? (
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
                <Metric label="Weekly PnL">
                  <Pnl value={awarded.weeklyPnlAssets} />
                </Metric>
                <Metric label="Active days">{awarded.activeDays}</Metric>
              </dl>
            </Panel>
          ) : (
            <Panel>
              <EmptyState
                title="No published Divisions history yet"
                message="Completed weekly Divisions standings for this Trading Account will appear here after weekly publication."
              />
            </Panel>
          )}
        </section>
      ) : null}
    </div>
  )
}

function parseUnsignedBigInt(raw: string | null | undefined): bigint {
  return raw != null && /^\d+$/.test(raw) ? BigInt(raw) : 0n
}

function CollateralAllocationBar({
  freeAssets,
  positionMarginAssets,
  committedOrderAssets,
  otherLockedAssets,
}: {
  freeAssets: string
  positionMarginAssets: string
  committedOrderAssets: string
  otherLockedAssets: bigint
}) {
  const free = parseUnsignedBigInt(freeAssets)
  const posMargin = parseUnsignedBigInt(positionMarginAssets)
  const orderMargin = parseUnsignedBigInt(committedOrderAssets)
  const other = otherLockedAssets > 0n ? otherLockedAssets : 0n
  const total = free + posMargin + orderMargin + other

  const buckets = [
    { id: 'free', label: 'Free collateral', value: free, color: 'bg-positive' },
    { id: 'position', label: 'Position margin', value: posMargin, color: 'bg-brand-orange' },
    { id: 'orders', label: 'Committed orders', value: orderMargin, color: 'bg-brand-yellow' },
    { id: 'locked', label: 'Bounty & locked', value: other, color: 'bg-brand-peach' },
  ].filter((b) => b.value > 0n)

  const usedPct =
    total > 0n ? ((Number(posMargin + orderMargin + other) / Number(total)) * 100).toFixed(1) : null

  return (
    <div
      aria-label="Collateral allocation"
      className="border-b border-brand-border/15 bg-app-bg/45 px-5 py-3.5"
    >
      <div className="flex items-center justify-between gap-2 text-xs">
        <span className="text-content-secondary">Collateral utilization</span>
        <span className="font-mono text-content-primary">
          {usedPct !== null ? `${usedPct}% locked` : 'No collateral'}
        </span>
      </div>
      <div className="mt-2 flex h-2 w-full overflow-hidden bg-surface-raised">
        {total > 0n
          ? buckets.map((bucket) => {
              const pct = (Number(bucket.value) / Number(total)) * 100
              return (
                <div
                  key={bucket.id}
                  title={`${bucket.label}: ${formatAssets(bucket.value.toString())} (${pct.toFixed(1)}%)`}
                  className={`h-full ${bucket.color}`}
                  style={{ width: `${Math.max(pct, 1).toFixed(2)}%` }}
                />
              )
            })
          : null}
      </div>
      {buckets.length > 0 ? (
        <div className="mt-2.5 flex flex-wrap gap-x-4 gap-y-1 text-[11px] text-content-secondary">
          {buckets.map((bucket) => {
            const pct = total > 0n ? ((Number(bucket.value) / Number(total)) * 100).toFixed(1) : '0.0'
            return (
              <span key={bucket.id} className="inline-flex items-center gap-1.5">
                <span aria-hidden="true" className={`inline-block h-2 w-2 ${bucket.color}`} />
                <span>{bucket.label}</span>
                <span className="font-mono text-content-tertiary">{pct}%</span>
              </span>
            )
          })}
        </div>
      ) : null}
    </div>
  )
}

export function AccountPage() {
  const { address } = useParams()
  if (!address || !isWalletAddress(address)) {
    return (
      <div className="space-y-6">
        <Panel>
          <EmptyState
            title="Invalid Trading Account address"
            message="Enter a 42-character 0x Trading Account address in the header search bar."
          />
        </Panel>
        <Link to="/" className="inline-block text-sm text-brand-peach hover:underline">
          ← Back to Protocol Pulse
        </Link>
      </div>
    )
  }
  return <TradingAccountInspector address={address} />
}
