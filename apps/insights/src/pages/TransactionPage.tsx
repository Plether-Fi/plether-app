import { useRef, type ReactNode } from 'react'
import { Link, useLocation, useParams, useSearchParams } from 'react-router-dom'
import {
  InsightsApiError,
  extractLogIndex,
  formatDurationCompact,
  formatPlDxyQuantity,
  matchPositionForTrade,
  positionPath,
  tradeToWalletActivity,
  useTransaction,
  useWalletExposure,
  type ProtocolTradeRecord,
  type TransactionRouteState,
} from '../api'
import { ArbiscanButton, CopyButton } from '../components/ActionButtons'
import { AddressAvatar } from '../components/AddressAvatar'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { OraclePriceLink } from '../components/OraclePriceLink'
import { TraderIdentity } from '../components/TraderIdentity'
import { OrderProtectionsChecklistPanel } from '../components/OrderProtectionsChecklist'
import { OrderTimingTimelinePanel } from '../components/OrderTimingTimeline'
import { ShareCardModal } from '../components/ShareCardModal'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { ExactUsdgAmount, TradeBreakdownDetails } from '../components/TradeBreakdown'
import {
  BatchFinalizationPanel,
  ProtocolEventTracePanel,
  TxLpSettlementPanel,
} from '../components/TransactionBatchInspector'
import { EmptyState, ErrorState, LoadingState, Panel, Pnl } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { arbiscanLinkKey, resolvePerpsChain } from '../utils/chain'
import {
  formatAssets,
  formatCompactAssets,
  formatPrice,
  formatSignedAssets,
  formatUtc,
  isTxHash,
  shortAddress,
} from '../utils/format'
import { deriveTradeBreakdown } from '../utils/tradeBreakdown'

function unsigned(value: string | null | undefined): bigint | null {
  return value != null && /^\d+$/.test(value) ? BigInt(value) : null
}

function ExactPlDxy({ value }: { value: bigint | null }) {
  if (value === null) return <>Unavailable</>
  const scale = 10n ** 18n
  const whole = new Intl.NumberFormat('en-US').format(value / scale)
  const frac = (value % scale).toString().padStart(18, '0').replace(/0{1,12}$/, '')
  const head = frac.slice(0, 2)
  const tail = frac.slice(2)
  return (
    <span className="whitespace-nowrap font-mono tabular-nums text-content-primary">
      <span>{whole}.{head}</span>
      {tail ? <span className="text-[11px] opacity-40">{tail}</span> : null}
      <span className="text-xs font-normal text-content-tertiary"> plDXY</span>
    </span>
  )
}

function activityTone(activityType: string): StatusTone {
  const lower = activityType.toLowerCase()
  if (lower === 'open') return 'live'
  if (lower === 'close') return 'unknown'
  if (lower === 'liquidated') return 'blocked'
  if (lower === 'deposit') return 'ready'
  return 'unknown'
}

function DetailRow({ label, children }: { label: string; children: ReactNode }) {
  return (
    <div className="flex items-start justify-between gap-4 border-b border-brand-border/15 py-2.5 last:border-0">
      <dt className="text-content-secondary">{label}</dt>
      <dd className="min-w-0 text-right tabular-nums [overflow-wrap:anywhere]">{children}</dd>
    </div>
  )
}

function resolveEventOrderId(ev: ProtocolTradeRecord): string | null {
  return ev.orderId ?? ev.execution?.orderId ?? null
}

function pickInitialEvent(
  events: ProtocolTradeRecord[],
  logParam: string | null,
): ProtocolTradeRecord | null {
  if (events.length === 0) return null
  if (logParam) {
    const matched = events.find((ev) => extractLogIndex(ev.id) === logParam)
    if (matched) return matched
  }
  const withReceipt = events.find(
    (ev) => ev.execution != null && (ev.activityType === 'Open' || ev.activityType === 'Close'),
  )
  return withReceipt ?? events[0]
}

export function TransactionPage() {
  const { txHash = '' } = useParams()
  const location = useLocation()
  const [searchParams, setSearchParams] = useSearchParams()
  const protocol = useProtocolContext()
  const receiptTopRef = useRef<HTMLDivElement>(null)

  const normalizedHash = txHash.trim().toLowerCase()
  const validHash = isTxHash(normalizedHash)
  const logParam = searchParams.get('log')

  const rawState = location.state as TransactionRouteState | null
  const routeState =
    rawState?.trade?.txHash.toLowerCase() === normalizedHash ? rawState : null

  const txQuery = useTransaction(validHash ? normalizedHash : undefined)
  const txData = txQuery.data
  const events: ProtocolTradeRecord[] = txData?.events.length
    ? txData.events
    : routeState?.trade
      ? [routeState.trade]
      : []
  const selectedEvent = pickInitialEvent(events, logParam)
  const exposureQuery = useWalletExposure(validHash ? selectedEvent?.account : undefined)

  if (!validHash) {
    return (
      <div className="space-y-4">
        <Link to="/execution" className="text-sm font-semibold text-brand-peach hover:underline">
          ← Back to Execution
        </Link>
        <ErrorState
          title="Invalid transaction hash"
          message="Enter a complete 0x-prefixed 66-character Arbitrum transaction hash."
        />
      </div>
    )
  }

  const finalizedOrders = txData?.finalizedOrders ?? []
  const protocolEvents = txData?.protocolEvents ?? []
  const batchSummary = txData?.batchSummary ?? null
  const latencyBenchmark = txData?.latencyBenchmark ?? null
  const lpSettlement = txData?.lpSettlement ?? null
  const hasAnyProtocolData =
    events.length > 0 || finalizedOrders.length > 0 || protocolEvents.length > 0 || lpSettlement != null

  const chainId = txData?.network.chainId ?? routeState?.chainId ?? protocol.chainId
  const chain = resolvePerpsChain(chainId)
  const releaseVersion =
    txData?.network.releaseVersion ?? routeState?.releaseVersion ?? protocol.releaseVersion

  const blockNumber = txData?.blockNumber ?? selectedEvent?.blockNumber ?? null
  const occurredAt = txData?.occurredAt ?? selectedEvent?.occurredAt ?? null
  const executorAddress = txData?.executorAddress ?? null

  const backTarget = routeState?.from ?? '/execution'
  const backLabel = routeState?.fromLabel ?? 'Back to Execution'
  const pageUrl = typeof window !== 'undefined' ? window.location.href : `/tx/${normalizedHash}`
  const claimedExternalKeys = new Set<string>([
    arbiscanLinkKey('tx', normalizedHash, chainId),
    ...(executorAddress ? [arbiscanLinkKey('address', executorAddress, chainId)] : []),
  ])

  if (!hasAnyProtocolData && txQuery.isLoading) {
    return (
      <div className="space-y-4">
        <Link to={backTarget} className="text-sm font-semibold text-brand-peach hover:underline">
          ← {backLabel}
        </Link>
        <LoadingState rows={4} />
      </div>
    )
  }

  if (!hasAnyProtocolData && txQuery.isError) {
    const isNotFound =
      txQuery.error instanceof InsightsApiError && txQuery.error.status === 404
    return (
      <div className="space-y-5">
        <div className="flex flex-wrap items-center justify-between gap-3">
          <Link to={backTarget} className="text-sm font-semibold text-brand-peach hover:underline">
            ← {backLabel}
          </Link>
          <ArbiscanButton kind="tx" value={normalizedHash} chainId={chainId} />
        </div>
        {isNotFound ? (
          <Panel className="p-6">
            <EmptyState
              title="No Plether Perps execution indexed for this transaction"
              message={`Transaction ${shortAddress(normalizedHash)} was not found in the indexed Plether Perps history on ${chain.name}. It may belong to another contract or still be indexing.`}
            />
          </Panel>
        ) : (
          <ErrorState
            title="Unable to load transaction receipt"
            message={txQuery.error.message}
          />
        )}
      </div>
    )
  }

  if (!selectedEvent) {
    if (!hasAnyProtocolData) {
      return (
        <div className="space-y-4">
          <Link to={backTarget} className="text-sm font-semibold text-brand-peach hover:underline">
            ← {backLabel}
          </Link>
          <EmptyState
            title="No execution events"
            message="This transaction does not contain any decoded Perps trade events."
          />
        </div>
      )
    }

    const firstProtectedOrder = finalizedOrders.find((order) => order.orderProtections != null)
    const kindLabel =
      txData?.txKind === 'batch_finalization'
        ? `Batch Finalization (${String(finalizedOrders.length)} orders)`
        : txData?.txKind === 'order_finalization'
          ? 'Order Finalization'
          : txData?.txKind === 'lp_settlement'
            ? 'LP Epoch Settlement'
            : 'Protocol Transaction'

    return (
      <div className="space-y-6">
        <div className="flex flex-wrap items-center justify-between gap-3">
          <Link to={backTarget} className="text-sm font-semibold text-brand-peach hover:underline">
            ← {backLabel}
          </Link>
          <div className="flex flex-wrap items-center gap-2">
            <CopyButton
              value={pageUrl}
              hoverLabel="Copy link"
              copiedLabel="Link copied"
              ariaLabel="Copy link"
            />
            <ArbiscanButton kind="tx" value={normalizedHash} chainId={chainId} />
          </div>
        </div>

        <DataFreshnessBanner
          tone={lpSettlement ? 'ready' : finalizedOrders.length > 0 ? 'live' : 'unknown'}
          statusLabel={kindLabel}
          networkName={chain.name}
          isTestnet={chain.isTestnet}
          releaseVersion={releaseVersion}
          blockNumber={blockNumber ? Number(blockNumber) : null}
          observedAt={occurredAt}
          note={`Transaction ${shortAddress(normalizedHash)}`}
        />

        <Panel className="space-y-4 p-5 sm:p-6">
          <div className="flex flex-wrap items-center gap-2.5">
            <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
              {kindLabel}
            </p>
            <CopyButton value={normalizedHash} ariaLabel={`Copy ${normalizedHash}`} />
          </div>
          <h1 className="break-all font-mono text-base font-semibold text-content-primary sm:text-lg">
            {normalizedHash}
          </h1>
          <div className="grid gap-4 border-t border-brand-border/20 pt-4 text-xs sm:grid-cols-3">
            <div>
              <span className="text-content-tertiary">Executor / Keeper</span>
              <div className="mt-1 font-mono text-sm">
                {executorAddress ? (
                  <div className="inline-flex items-center gap-2">
                    <Link
                      to={`/keepers/${executorAddress.toLowerCase()}`}
                      className="inline-flex items-center gap-2 text-brand-peach hover:underline"
                    >
                      <AddressAvatar address={executorAddress} size={18} />
                      {shortAddress(executorAddress)}
                    </Link>
                    <ArbiscanButton kind="address" value={executorAddress} chainId={chainId} />
                  </div>
                ) : (
                  <span className="text-content-tertiary">—</span>
                )}
              </div>
            </div>
            <div>
              <span className="text-content-tertiary">Block</span>
              <div className="mt-1 font-mono text-sm text-content-primary">
                {blockNumber ? `#${Number(blockNumber).toLocaleString('en-US')}` : '—'}
              </div>
            </div>
            <div>
              <span className="text-content-tertiary">Timestamp (UTC)</span>
              <div className="mt-1 font-mono text-sm text-content-primary">
                {occurredAt ? formatUtc(occurredAt) : '—'}
              </div>
            </div>
          </div>
        </Panel>

        {lpSettlement ? <TxLpSettlementPanel settlement={lpSettlement} /> : null}
        {finalizedOrders.length > 0 ? (
          <OrderTimingTimelinePanel
            finalizedOrders={finalizedOrders}
            activeOrderId={null}
            batchSummary={batchSummary}
            latencyBenchmark={latencyBenchmark}
            fallbackFinalizedAt={occurredAt}
            fallbackFinalizedBlock={blockNumber}
            txHash={normalizedHash}
            chainId={chainId}
            excludeExternalKeys={claimedExternalKeys}
          />
        ) : null}
        {firstProtectedOrder?.orderProtections ? (
          <OrderProtectionsChecklistPanel
            protections={firstProtectedOrder.orderProtections}
            side={firstProtectedOrder.side}
          />
        ) : null}
        <BatchFinalizationPanel
          batchSummary={batchSummary}
          finalizedOrders={finalizedOrders}
          chainId={chainId}
          activeOrderId={null}
          excludeExternalKeys={claimedExternalKeys}
        />
        <ProtocolEventTracePanel
          protocolEvents={protocolEvents}
          chainId={chainId}
          excludeExternalKeys={claimedExternalKeys}
        />
      </div>
    )
  }

  const walletActivity = tradeToWalletActivity(selectedEvent)
  const isClose = selectedEvent.activityType.toLowerCase() === 'close'
  const isOpen = selectedEvent.activityType.toLowerCase() === 'open'
  const isLiquidation = selectedEvent.activityType.toLowerCase() === 'liquidated'
  const validReceipt = deriveTradeBreakdown(walletActivity.execution, walletActivity.type) !== null
  const receipt = validReceipt ? walletActivity.execution?.receipt : undefined

  const remaining = unsigned(receipt?.postPositionSize)
  const margin = unsigned(receipt?.postPositionMarginAssets)
  const notional = unsigned(receipt?.executionNotionalAssets)
  const quantityDelta = unsigned(selectedEvent.sizeDelta)
  const positionStatus =
    remaining === null
      ? 'Unavailable'
      : isClose
        ? remaining === 0n
          ? 'Fully closed'
          : 'Partially closed'
        : remaining > 0n
          ? 'Open'
          : 'Unavailable'

  const traderLabel = routeState?.traderLabel ?? shortAddress(selectedEvent.account)
  const effectiveOrderId =
    resolveEventOrderId(selectedEvent) ??
    (events.length === 1 &&
    finalizedOrders.length === 1 &&
    finalizedOrders[0].terminalStatus === 'Executed'
      ? finalizedOrders[0].orderId
      : null)
  const matchedFinalizedOrder =
    effectiveOrderId != null
      ? finalizedOrders.find((order) => order.orderId === effectiveOrderId) ?? null
      : null
  const selectedOrderProtections =
    selectedEvent.orderProtections ??
    (isOpen ? matchedFinalizedOrder?.orderProtections ?? null : null)

  const bannerTone: StatusTone = validReceipt ? 'ready' : isLiquidation ? 'blocked' : 'live'
  const bannerLabel = validReceipt
    ? 'Verified execution receipt'
    : isLiquidation
      ? 'On-chain liquidation'
      : 'Indexed transaction'

  const matchedPosition = matchPositionForTrade(exposureQuery.data, {
    id: selectedEvent.id,
    txHash: selectedEvent.txHash,
    blockNumber: selectedEvent.blockNumber,
    occurredAt: selectedEvent.occurredAt,
    orderId: effectiveOrderId,
    side: selectedEvent.side,
  })
  const positionHref = matchedPosition
    ? positionPath(selectedEvent.account, matchedPosition.positionId)
    : `/wallets/${selectedEvent.account.toLowerCase()}?tab=exposure`

  return (
    <div ref={receiptTopRef} className="space-y-6">
      <div className="flex flex-wrap items-center justify-between gap-3">
        <div className="flex flex-wrap items-center gap-4 text-sm font-semibold">
          <Link to={backTarget} className="text-brand-peach hover:underline">
            ← {backLabel}
          </Link>
          {backTarget !== `/wallets/${selectedEvent.account}` ? (
            <Link
              to={`/wallets/${selectedEvent.account}`}
              className="text-content-secondary hover:text-content-primary hover:underline"
            >
              Account {shortAddress(selectedEvent.account)} →
            </Link>
          ) : null}
          <Link
            to={positionHref}
            className="text-content-secondary hover:text-brand-peach hover:underline"
          >
            {matchedPosition ? 'Position lifecycle →' : 'Market exposure →'}
          </Link>
        </div>
        <div className="flex flex-wrap items-center gap-2">
          <CopyButton
            value={pageUrl}
            hoverLabel="Copy link"
            copiedLabel="Link copied"
            ariaLabel="Copy link"
          />
          {isClose && selectedEvent.pnlAssets != null ? (
            <ShareCardModal
              compact
              triggerLabel="Share PnL"
              cards={[
                {
                  id: `trade-${selectedEvent.id}`,
                  label: 'Closed Position PnL',
                  data: {
                    kind: 'trade',
                    traderLabel,
                    account: selectedEvent.account,
                    networkName: chain.name,
                    releaseVersion,
                    market: 'plDXY-PERP',
                    side:
                      selectedEvent.side === 'long' || selectedEvent.side === 'short'
                        ? selectedEvent.side
                        : null,
                    realizedPnlAssets: selectedEvent.pnlAssets,
                    price: selectedEvent.price ?? null,
                    sizeDelta: selectedEvent.sizeDelta ?? null,
                    notionalAssets: receipt?.executionNotionalAssets ?? selectedEvent.amountAssets ?? null,
                    occurredAt: selectedEvent.occurredAt,
                    txHash: selectedEvent.txHash,
                  },
                },
              ]}
            />
          ) : selectedEvent.activityType === 'Open' ? (
            <ShareCardModal
              compact
              triggerLabel="Share Open"
              cards={[
                {
                  id: `open-${selectedEvent.id}`,
                  label: 'Opened Position Receipt',
                  data: {
                    kind: 'open_trade',
                    traderLabel,
                    account: selectedEvent.account,
                    networkName: chain.name,
                    releaseVersion,
                    market: 'plDXY-PERP',
                    side:
                      selectedEvent.side === 'long' || selectedEvent.side === 'short'
                        ? selectedEvent.side
                        : null,
                    price: selectedEvent.price ?? null,
                    sizeDelta: selectedEvent.sizeDelta ?? null,
                    notionalAssets: receipt?.executionNotionalAssets ?? selectedEvent.amountAssets ?? null,
                    occurredAt: selectedEvent.occurredAt,
                    txHash: selectedEvent.txHash,
                  },
                },
              ]}
            />
          ) : null}
          <ArbiscanButton kind="tx" value={normalizedHash} chainId={chainId} />
        </div>
      </div>

      <DataFreshnessBanner
        tone={bannerTone}
        statusLabel={bannerLabel}
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={blockNumber}
        observedAt={occurredAt}
        note={`Transaction ${shortAddress(normalizedHash)}`}
      />

      <Panel className="p-5 sm:p-6">
        <div className="flex flex-wrap items-start justify-between gap-4">
          <div className="min-w-0 space-y-2">
            <div className="flex flex-wrap items-center gap-2.5">
              <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                Transaction Breakdown
              </p>
              <StatusPill
                tone={activityTone(selectedEvent.activityType)}
                label={selectedEvent.activityType}
              />
              {selectedEvent.side ? (
                <span
                  className={`font-mono text-xs font-semibold uppercase ${
                    selectedEvent.side === 'long'
                      ? 'text-positive'
                      : selectedEvent.side === 'short'
                        ? 'text-brand-orange'
                        : 'text-content-tertiary'
                  }`}
                >
                  {selectedEvent.side} · plDXY-PERP
                </span>
              ) : null}
            </div>
            <div className="flex flex-wrap items-center gap-2.5">
              <h1 className="break-all font-mono text-base font-semibold text-content-primary sm:text-lg">
                {normalizedHash}
              </h1>
              <CopyButton value={normalizedHash} ariaLabel={`Copy ${normalizedHash}`} />
            </div>
          </div>
          <div className="text-left sm:text-right">
            <p className="text-xs text-content-tertiary">Executed at</p>
            <p className="mt-0.5 font-mono text-sm text-content-primary">
              {formatUtc(selectedEvent.occurredAt)}
            </p>
          </div>
        </div>

        <div className="mt-5 grid gap-3 border-t border-brand-border/20 pt-4 text-xs sm:grid-cols-2 lg:grid-cols-4">
          <div>
            <span className="text-content-tertiary">Trading Account</span>
            <div className="mt-1">
              <TraderIdentity
                address={selectedEvent.account}
                discordUsername={
                  routeState?.traderLabel?.startsWith('@') ? routeState.traderLabel : undefined
                }
                avatarSize={18}
                showCopy
              />
            </div>
          </div>
          <div>
            <span className="text-content-tertiary">Block</span>
            <div className="mt-1 font-mono text-sm text-content-primary">
              {Number(selectedEvent.blockNumber).toLocaleString('en-US')}
            </div>
          </div>
          <div>
            <span className="text-content-tertiary">Order ID</span>
            <div className="mt-1 font-mono text-sm text-content-primary">
              {selectedEvent.orderId ?? (validReceipt ? walletActivity.execution?.orderId : '—')}
            </div>
          </div>
          <div>
            <span className="text-content-tertiary">Executor / Keeper</span>
            <div className="mt-1 font-mono text-sm">
              {executorAddress ? (
                <div className="inline-flex items-center gap-2">
                  <Link
                    to={`/keepers/${executorAddress.toLowerCase()}`}
                    className="inline-flex items-center gap-2 text-brand-peach hover:underline"
                  >
                    <AddressAvatar address={executorAddress} size={18} />
                    {shortAddress(executorAddress)}
                  </Link>
                  <ArbiscanButton kind="address" value={executorAddress} chainId={chainId} />
                </div>
              ) : (
                <span className="text-content-tertiary">—</span>
              )}
            </div>
          </div>
        </div>
      </Panel>

      {events.length > 1 ? (
        <section aria-label="Events in transaction" className="space-y-2">
          <p className="text-xs font-medium text-content-secondary">
            Events in this transaction ({events.length})
          </p>
          <div className="flex flex-wrap gap-2">
            {events.map((ev, index) => {
              const evLog = extractLogIndex(ev.id)
              const active = ev.id === selectedEvent.id
              return (
                <button
                  key={ev.id}
                  type="button"
                  onClick={() => {
                    if (evLog) {
                      setSearchParams({ log: evLog }, { replace: true })
                    }
                  }}
                  className={`cursor-pointer border px-3 py-1.5 text-xs font-semibold transition-colors ${
                    active
                      ? 'border-brand-orange bg-brand-orange text-content-primary'
                      : 'border-brand-border/35 bg-surface-panel text-content-secondary hover:border-brand-peach/50 hover:text-content-primary'
                  }`}
                >
                  #{index + 1} {ev.activityType}
                  {ev.side ? ` · ${ev.side.toUpperCase()}` : ''}
                  {evLog ? ` (log ${evLog})` : ''}
                </button>
              )
            })}
          </div>
        </section>
      ) : null}

      {/* Unified Execution Fill Summary Panel */}
      <Panel className="overflow-hidden">
        <div className="flex flex-wrap items-end justify-between gap-4 p-4 sm:p-6">
          <div>
            <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
              Executed Exposure
            </p>
            <p className="mt-1 font-mono text-3xl font-bold tracking-tight tabular-nums text-content-primary sm:text-4xl">
              {notional !== null
                ? formatAssets(notional.toString())
                : selectedEvent.amountAssets
                  ? formatCompactAssets(selectedEvent.amountAssets)
                  : 'Unavailable'}
            </p>
            <p className="mt-1 font-mono text-xs text-content-tertiary">
              {selectedEvent.latencySeconds != null
                ? `Commit → fill: ${String(selectedEvent.latencySeconds)}s${
                    matchedFinalizedOrder?.latencyPercentile != null
                      ? ` (P${String(matchedFinalizedOrder.latencyPercentile)})`
                      : ''
                  }`
                : 'Settled in USDG'}
            </p>
          </div>

          <div className="text-left font-mono sm:text-right">
            <p className="text-xs text-content-tertiary">Execution Price</p>
            <p className="mt-1 text-2xl font-bold tabular-nums text-content-primary">
              {selectedEvent.price ? (
                <OraclePriceLink
                  updateId={matchedFinalizedOrder?.oracleUpdateId ?? batchSummary?.oracleUpdateId}
                >
                  {formatPrice(selectedEvent.price)}
                </OraclePriceLink>
              ) : (
                'Unavailable'
              )}
            </p>
            <p className="mt-0.5 text-xs text-content-tertiary">
              {selectedEvent.oracleFrozen
                ? 'Executed with frozen oracle mark'
                : 'Final verified plDXY mark'}
            </p>
          </div>
        </div>

        <dl className="grid grid-cols-1 divide-y divide-brand-border/15 border-t border-brand-border/20 bg-app-bg/35 sm:grid-cols-2 sm:divide-x sm:divide-y-0">
          <div className="px-4 py-3.5 sm:px-6">
            <dt className="text-xs text-content-tertiary">
              {isClose ? 'Quantity closed' : isOpen ? 'Quantity added' : 'Quantity'}
            </dt>
            <dd className="mt-1 font-mono text-lg font-bold tabular-nums text-content-primary">
              {quantityDelta !== null ? formatPlDxyQuantity(selectedEvent.sizeDelta) : 'Unavailable'}
            </dd>
            <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
              {selectedEvent.side ? `${selectedEvent.side.toUpperCase()} exposure` : 'Position size delta'}
            </p>
          </div>

          <div className="px-4 py-3.5 sm:px-6">
            {selectedEvent.pnlAssets != null ? (
              <>
                <dt className="text-xs text-content-tertiary">Directional realized PnL</dt>
                <dd
                  className={`mt-1 font-mono text-lg font-bold tabular-nums ${
                    selectedEvent.pnlAssets.startsWith('-')
                      ? 'text-brand-orange'
                      : selectedEvent.pnlAssets === '0'
                        ? 'text-content-primary'
                        : 'text-positive'
                  }`}
                >
                  {formatSignedAssets(selectedEvent.pnlAssets)}
                </dd>
                <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
                  {isLiquidation && selectedEvent.keeperBountyAssets
                    ? `Keeper bounty: ${formatCompactAssets(selectedEvent.keeperBountyAssets)}`
                    : 'Before fees, VPI & adjustments'}
                </p>
              </>
            ) : (
              <>
                <dt className="text-xs text-content-tertiary">Position after fill</dt>
                <dd
                  className={`mt-1 font-mono text-lg font-bold ${
                    positionStatus === 'Open' ? 'text-positive' : 'text-content-primary'
                  }`}
                >
                  {positionStatus}
                </dd>
                <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
                  {margin !== null
                    ? `Margin: ${formatAssets(margin.toString())}`
                    : 'Post-execution position state'}
                </p>
              </>
            )}
          </div>
        </dl>
      </Panel>

      {finalizedOrders.length > 0 ? (
        <OrderTimingTimelinePanel
          finalizedOrders={finalizedOrders}
          activeOrderId={effectiveOrderId}
          batchSummary={batchSummary}
          latencyBenchmark={latencyBenchmark}
          fallbackFinalizedAt={occurredAt}
          fallbackFinalizedBlock={blockNumber}
          txHash={normalizedHash}
          chainId={chainId}
          excludeExternalKeys={claimedExternalKeys}
        />
      ) : null}

      {selectedOrderProtections ? (
        <OrderProtectionsChecklistPanel
          protections={selectedOrderProtections}
          side={selectedEvent.side}
        />
      ) : null}

      <div className="grid gap-6 lg:grid-cols-12">
        <Panel className="p-5 sm:p-6 lg:col-span-7">
          <h2 className="text-base font-semibold text-content-primary">
            Execution result &amp; account change
          </h2>
          <p className="mt-1 text-xs leading-5 text-content-secondary">
            Full 6-decimal USDG reconciliation for this single execution. Other opens, increases,
            and closes appear on their own transaction events.
          </p>
          <div className="mt-5">
            {isOpen || isClose ? (
              <TradeBreakdownDetails item={walletActivity} />
            ) : (
              <dl className="text-sm">
                <DetailRow label="Event type">{selectedEvent.activityType}</DetailRow>
                <DetailRow label="Amount">{formatCompactAssets(selectedEvent.amountAssets)}</DetailRow>
                <DetailRow label="Realized PnL">
                  {selectedEvent.pnlAssets != null ? (
                    <Pnl value={selectedEvent.pnlAssets} />
                  ) : (
                    '—'
                  )}
                </DetailRow>
                {selectedEvent.keeperBountyAssets ? (
                  <DetailRow label="Liquidation keeper bounty">
                    {formatCompactAssets(selectedEvent.keeperBountyAssets)}
                  </DetailRow>
                ) : null}
              </dl>
            )}
          </div>
        </Panel>

        <div className="space-y-6 lg:col-span-5">
          <Panel className="p-5 sm:p-6">
            <h2 className="text-base font-semibold text-content-primary">Execution details</h2>
            <dl className="mt-3 text-sm">
              <DetailRow label="Action">
                {isClose ? 'Close' : isOpen ? 'Open / increase' : selectedEvent.activityType}
              </DetailRow>
              <DetailRow label="Market">plDXY-PERP</DetailRow>
              <DetailRow label="Position side">
                {selectedEvent.side?.toUpperCase() ?? 'Unavailable'}
              </DetailRow>
              <DetailRow label="Order ID">
                <span className="font-mono">
                  {selectedEvent.orderId ?? (validReceipt ? walletActivity.execution?.orderId : 'Unavailable')}
                </span>
              </DetailRow>
              <DetailRow label="Final plDXY price">
                {selectedEvent.price ? (
                  <OraclePriceLink
                    updateId={matchedFinalizedOrder?.oracleUpdateId ?? batchSummary?.oracleUpdateId}
                    className="font-mono"
                  >
                    {formatPrice(selectedEvent.price)}
                  </OraclePriceLink>
                ) : (
                  <span className="font-mono">—</span>
                )}
              </DetailRow>
              <DetailRow label={isClose ? 'Quantity closed' : 'Quantity added'}>
                <ExactPlDxy value={quantityDelta} />
              </DetailRow>
              <DetailRow label="Executed exposure">
                {notional === null ? 'Unavailable' : <ExactUsdgAmount value={notional} />}
              </DetailRow>
              {selectedEvent.latencySeconds != null ? (
                <DetailRow label="Execution latency">
                  {`${String(selectedEvent.latencySeconds)}s`}
                </DetailRow>
              ) : null}
            </dl>
          </Panel>

          <Panel className="p-5 sm:p-6">
            <h2 className="text-base font-semibold text-content-primary">
              Position after execution
            </h2>
            <dl className="mt-3 text-sm">
              <DetailRow label="Status">{positionStatus}</DetailRow>
              {remaining !== null && remaining > 0n ? (
                <>
                  <DetailRow label="Position quantity">
                    <ExactPlDxy value={remaining} />
                  </DetailRow>
                  <DetailRow label="Position margin">
                    {margin === null ? 'Unavailable' : <ExactUsdgAmount value={margin} />}
                  </DetailRow>
                </>
              ) : null}
            </dl>
            <div className="mt-4 flex flex-wrap items-center justify-between gap-3 border-t border-brand-border/20 pt-3">
              <div className="space-y-0.5">
                <Link
                  to={positionHref}
                  className="inline-block text-sm font-semibold text-brand-peach hover:underline"
                >
                  {matchedPosition ? 'View full position lifecycle →' : 'View wallet market exposure →'}
                </Link>
                {matchedPosition ? (
                  <p className="font-mono text-xs text-content-tertiary">
                    {matchedPosition.side.toUpperCase()} · {matchedPosition.status} ·{' '}
                    {String(matchedPosition.fillCount)}{' '}
                    {matchedPosition.fillCount === 1 ? 'fill' : 'fills'} · held{' '}
                    {formatDurationCompact(matchedPosition.durationSeconds)}
                  </p>
                ) : null}
              </div>
              <Link
                to={`/wallets/${selectedEvent.account}`}
                className="text-xs font-semibold text-content-secondary hover:text-content-primary hover:underline"
              >
                Inspect full Trading Account →
              </Link>
            </div>
          </Panel>
        </div>
      </div>

      {lpSettlement ? <TxLpSettlementPanel settlement={lpSettlement} /> : null}

      <BatchFinalizationPanel
        batchSummary={batchSummary}
        finalizedOrders={finalizedOrders}
        chainId={chainId}
        activeOrderId={effectiveOrderId}
        excludeExternalKeys={claimedExternalKeys}
        onInspectOrder={(orderId) => {
          const order = finalizedOrders.find((o) => o.orderId === orderId)
          const orderAccount = order?.account?.toLowerCase()
          const match =
            events.find((ev) => resolveEventOrderId(ev) === orderId) ??
            (orderAccount
              ? events.find((ev) => ev.account.toLowerCase() === orderAccount)
              : undefined) ??
            (events.length === 1 ? events[0] : undefined)
          const idx = match ? extractLogIndex(match.id) : null
          if (idx) {
            setSearchParams({ log: idx }, { replace: true })
          }
          if (typeof receiptTopRef.current?.scrollIntoView === 'function') {
            receiptTopRef.current.scrollIntoView({ behavior: 'smooth', block: 'start' })
          }
        }}
      />

      <ProtocolEventTracePanel
        protocolEvents={protocolEvents}
        chainId={chainId}
        excludeExternalKeys={claimedExternalKeys}
      />
    </div>
  )
}
