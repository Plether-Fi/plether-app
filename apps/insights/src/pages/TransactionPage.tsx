import { useState, type ReactNode } from 'react'
import { Link, useLocation, useParams, useSearchParams } from 'react-router-dom'
import {
  InsightsApiError,
  extractLogIndex,
  formatPlDxyQuantity,
  tradeToWalletActivity,
  useTransaction,
  type ProtocolTradeRecord,
  type TransactionRouteState,
} from '../api'
import { AddressAvatar } from '../components/AddressAvatar'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { ShareCardModal } from '../components/ShareCardModal'
import { StatCard } from '../components/StatCard'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { ExactUsdgAmount, TradeBreakdownDetails } from '../components/TradeBreakdown'
import { EmptyState, ErrorState, LoadingState, Panel, Pnl } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { explorerAddressUrl, explorerTxUrl, resolvePerpsChain } from '../utils/chain'
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
  const [copiedHash, setCopiedHash] = useState(false)
  const [copiedLink, setCopiedLink] = useState(false)

  const normalizedHash = txHash.trim().toLowerCase()
  const validHash = isTxHash(normalizedHash)
  const logParam = searchParams.get('log')

  const rawState = location.state as TransactionRouteState | null
  const routeState =
    rawState?.trade?.txHash.toLowerCase() === normalizedHash ? rawState : null

  const txQuery = useTransaction(validHash ? normalizedHash : undefined)

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

  const txData = txQuery.data
  const events: ProtocolTradeRecord[] = txData?.events.length
    ? txData.events
    : routeState?.trade
      ? [routeState.trade]
      : []

  const selectedEvent = pickInitialEvent(events, logParam)

  const chainId = txData?.network.chainId ?? routeState?.chainId ?? protocol.chainId
  const chain = resolvePerpsChain(chainId)
  const releaseVersion =
    txData?.network.releaseVersion ?? routeState?.releaseVersion ?? protocol.releaseVersion

  const blockNumber = txData?.blockNumber ?? selectedEvent?.blockNumber ?? null
  const occurredAt = txData?.occurredAt ?? selectedEvent?.occurredAt ?? null
  const executorAddress = txData?.executorAddress ?? null

  const backTarget = routeState?.from ?? '/execution'
  const backLabel = routeState?.fromLabel ?? 'Back to Execution'

  function handleCopyHash() {
    if (typeof navigator === 'undefined') return
    void navigator.clipboard.writeText(normalizedHash).then(() => {
      setCopiedHash(true)
      window.setTimeout(() => {
        setCopiedHash(false)
      }, 1500)
    })
  }

  function handleCopyLink() {
    if (typeof navigator === 'undefined' || typeof window === 'undefined') {
      return
    }
    void navigator.clipboard.writeText(window.location.href).then(() => {
      setCopiedLink(true)
      window.setTimeout(() => {
        setCopiedLink(false)
      }, 1500)
    })
  }

  if (events.length === 0 && txQuery.isLoading) {
    return (
      <div className="space-y-4">
        <Link to={backTarget} className="text-sm font-semibold text-brand-peach hover:underline">
          ← {backLabel}
        </Link>
        <LoadingState rows={4} />
      </div>
    )
  }

  if (events.length === 0 && txQuery.isError) {
    const isNotFound =
      txQuery.error instanceof InsightsApiError && txQuery.error.status === 404
    return (
      <div className="space-y-5">
        <div className="flex flex-wrap items-center justify-between gap-3">
          <Link to={backTarget} className="text-sm font-semibold text-brand-peach hover:underline">
            ← {backLabel}
          </Link>
          <a
            href={explorerTxUrl(normalizedHash, chainId)}
            target="_blank"
            rel="noreferrer"
            className="border border-brand-border/40 px-3 py-1.5 text-xs font-semibold text-brand-peach transition-colors hover:bg-brand-peach/10"
          >
            View on {chain.explorerName} ↗
          </a>
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

  const bannerTone: StatusTone = validReceipt ? 'ready' : isLiquidation ? 'blocked' : 'live'
  const bannerLabel = validReceipt
    ? 'Verified execution receipt'
    : isLiquidation
      ? 'On-chain liquidation'
      : 'Indexed transaction'

  return (
    <div className="space-y-6">
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
        </div>
        <div className="flex flex-wrap items-center gap-2">
          <button
            type="button"
            onClick={handleCopyLink}
            className="cursor-pointer border border-brand-border/40 bg-surface-raised px-3 py-1.5 text-xs font-semibold text-content-primary transition-colors hover:border-brand-peach/60 hover:text-brand-peach"
          >
            {copiedLink ? 'Link copied' : 'Copy link'}
          </button>
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
          ) : null}
          <a
            href={explorerTxUrl(normalizedHash, chainId)}
            target="_blank"
            rel="noreferrer"
            className="border border-brand-border/40 px-3 py-1.5 text-xs font-semibold text-brand-peach transition-colors hover:bg-brand-peach/10"
          >
            {chain.explorerName} ↗
          </a>
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
              <button
                type="button"
                onClick={handleCopyHash}
                className="shrink-0 cursor-pointer border border-brand-border/30 px-2 py-0.5 text-[11px] font-semibold text-content-secondary transition-colors hover:border-brand-peach/50 hover:text-brand-peach"
              >
                {copiedHash ? 'Copied' : 'Copy'}
              </button>
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
            <div className="mt-1 font-mono text-sm">
              <Link
                to={`/wallets/${selectedEvent.account}`}
                className="inline-flex items-center gap-2 text-brand-peach hover:underline"
              >
                <AddressAvatar address={selectedEvent.account} size={18} />
                {shortAddress(selectedEvent.account)}
              </Link>
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
                <a
                  href={explorerAddressUrl(executorAddress, chainId)}
                  target="_blank"
                  rel="noreferrer"
                  className="inline-flex items-center gap-2 text-brand-peach hover:underline"
                >
                  <AddressAvatar address={executorAddress} size={18} />
                  {shortAddress(executorAddress)} ↗
                </a>
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

      <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
        <StatCard
          label="Execution price"
          value={selectedEvent.price ? formatPrice(selectedEvent.price) : undefined}
          unavailable={!selectedEvent.price}
          detail={
            selectedEvent.oracleFrozen
              ? 'Executed with frozen oracle mark'
              : 'Final verified plDXY mark'
          }
        />
        <StatCard
          label={isClose ? 'Quantity closed' : isOpen ? 'Quantity added' : 'Quantity'}
          value={quantityDelta !== null ? formatPlDxyQuantity(selectedEvent.sizeDelta) : undefined}
          unavailable={quantityDelta === null}
          detail={selectedEvent.side ? `${selectedEvent.side.toUpperCase()} exposure` : 'Position size delta'}
        />
        <StatCard
          label="Executed exposure"
          value={
            notional !== null
              ? formatAssets(notional.toString())
              : selectedEvent.amountAssets
                ? formatCompactAssets(selectedEvent.amountAssets)
                : undefined
          }
          unavailable={notional === null && !selectedEvent.amountAssets}
          detail={
            selectedEvent.latencySeconds != null
              ? `Commit → fill: ${String(selectedEvent.latencySeconds)}s`
              : 'Settled in USDG'
          }
        />
        {selectedEvent.pnlAssets != null ? (
          <StatCard
            label="Directional realized PnL"
            value={formatSignedAssets(selectedEvent.pnlAssets)}
            tone={
              selectedEvent.pnlAssets.startsWith('-')
                ? 'negative'
                : selectedEvent.pnlAssets === '0'
                  ? 'neutral'
                  : 'positive'
            }
            detail={
              isLiquidation && selectedEvent.keeperBountyAssets
                ? `Keeper bounty: ${formatCompactAssets(selectedEvent.keeperBountyAssets)}`
                : 'Before fees, VPI & adjustments'
            }
          />
        ) : (
          <StatCard
            label="Position after fill"
            value={positionStatus}
            tone={positionStatus === 'Open' ? 'positive' : 'neutral'}
            detail={
              margin !== null
                ? `Margin: ${formatAssets(margin.toString())}`
                : 'Post-execution position state'
            }
          />
        )}
      </div>

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
                <span className="font-mono">{formatPrice(selectedEvent.price)}</span>
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
              <DetailRow label="On-chain explorer">
                <a
                  href={explorerTxUrl(normalizedHash, chainId)}
                  target="_blank"
                  rel="noreferrer"
                  className="font-mono text-brand-peach hover:underline"
                >
                  {shortAddress(normalizedHash)} ↗
                </a>
              </DetailRow>
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
            <div className="mt-4 border-t border-brand-border/20 pt-3">
              <Link
                to={`/wallets/${selectedEvent.account}`}
                className="text-sm font-semibold text-brand-peach hover:underline"
              >
                Inspect full Trading Account →
              </Link>
            </div>
          </Panel>
        </div>
      </div>
    </div>
  )
}
