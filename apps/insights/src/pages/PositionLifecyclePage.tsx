import { Link, useParams } from 'react-router-dom'
import {
  InsightsApiError,
  classifyLiquidationBuffer,
  formatPositionBpsPercent,
  formatDurationCompact,
  formatLeverageX100,
  formatPlDxyQuantity,
  transactionPath,
  useWalletPositionDetail,
} from '../api'
import { ArbiscanButton } from '../components/ActionButtons'
import { AddressAvatar } from '../components/AddressAvatar'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { OraclePriceLink } from '../components/OraclePriceLink'
import { PageBreadcrumbBar } from '../components/PageBreadcrumbBar'
import { OrderProtectionsInlineChecklist } from '../components/OrderProtectionsChecklist'
import { PositionLifecycleChart } from '../components/PositionLifecycleChart'
import { ShareCardModal } from '../components/ShareCardModal'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel, Pnl } from '../components/ui'
import { ExcursionMiniBar } from '../components/WalletExposureSection'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { arbiscanLinkKey, resolvePerpsChain } from '../utils/chain'
import {
  formatAssets,
  formatCompactAssets,
  formatPrice,
  formatRoi,
  formatSignedAssets,
  formatUtc,
  isWalletAddress,
  shortAddress,
} from '../utils/format'

function statusTone(status: 'open' | 'closed' | 'liquidated'): StatusTone {
  if (status === 'open') return 'live'
  if (status === 'liquidated') return 'blocked'
  return 'unknown'
}

function actionTone(action: string): StatusTone {
  if (action === 'Open' || action === 'Scale-in') return 'live'
  if (action === 'Liquidated') return 'blocked'
  if (action === 'Add margin') return 'degraded'
  return 'unknown'
}

export function PositionLifecyclePage() {
  const { address = '', positionId = '' } = useParams()
  const validAddress = isWalletAddress(address)
  const protocol = useProtocolContext()
  const query = useWalletPositionDetail(validAddress ? address : undefined, positionId)

  const data = query.data
  const chainId = data?.network.chainId ?? protocol.chainId
  const chain = resolvePerpsChain(chainId)
  const releaseVersion = data?.network.releaseVersion ?? protocol.releaseVersion

  if (!validAddress) {
    return (
      <div className="space-y-4">
        <PageBreadcrumbBar
          items={[
            { label: 'Markets', to: '/execution' },
            { label: 'Position Lifecycle' },
          ]}
        />
        <ErrorState
          title="Invalid Trading Account address"
          message="Expected a 42-character hexadecimal Arbitrum address."
        />
      </div>
    )
  }

  const pos = data?.position
  const handle = data?.identity.discordUsername
  const traderLabel = handle ? `@${handle.replace(/^@/, '')}` : shortAddress(address)
  const isNotFound = query.error instanceof InsightsApiError && query.error.status === 404

  const liqBuffer = classifyLiquidationBuffer(
    pos
      ? pos.status === 'open'
        ? (pos.currentLiquidationBps ?? pos.closestLiquidationBps)
        : pos.closestLiquidationBps
      : null,
  )

  return (
    <div className="space-y-6 sm:space-y-8">
      <PageBreadcrumbBar
        items={[
          { label: 'Markets', to: '/execution' },
          { label: traderLabel, to: `/wallets/${address.toLowerCase()}` },
          { label: `Position ${positionId}` },
        ]}
      />

      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={protocol.blockNumber}
        observedAt={data?.observedAt ?? protocol.observedAt}
        tone={
          query.isError
            ? isNotFound
              ? 'stale'
              : 'blocked'
            : pos?.status === 'liquidated'
              ? 'blocked'
              : pos?.status === 'open'
                ? pos.liquidatable
                  ? 'degraded'
                  : 'live'
                : pos
                  ? 'ready'
                  : 'unknown'
        }
        statusLabel={
          query.isLoading
            ? 'Loading position lifecycle'
            : pos
              ? pos.status === 'open'
                ? 'Active position lifecycle'
                : pos.status === 'liquidated'
                  ? 'Liquidated position'
                  : 'Closed position audit'
              : isNotFound
                ? 'Position not found'
                : 'Position telemetry unavailable'
        }
        note="Position-level open-to-close forensics · MFE/MAE, leverage & liquidation proximity"
      />

      {query.isLoading ? (
        <Panel>
          <LoadingState rows={5} />
        </Panel>
      ) : query.isError || !data || !pos ? (
        isNotFound ? (
          <Panel>
            <EmptyState
              title="Position lifecycle not found"
              message={`No reconstructed position matching "${positionId}" was found for ${shortAddress(address)} on ${releaseVersion ?? 'this release'}.`}
            />
          </Panel>
        ) : (
          <ErrorState
            title="Could not load position lifecycle"
            message={query.error instanceof Error ? query.error.message : 'Unexpected API error'}
            onRetry={() => void query.refetch()}
          />
        )
      ) : (
        <>
          {/* Clean Header */}
          <header className="flex flex-col justify-between gap-4 border border-brand-border/25 bg-surface-panel p-6 sm:flex-row sm:items-center">
            <div className="space-y-2">
              <div className="flex flex-wrap items-center gap-2">
                <span className="text-xs font-semibold uppercase tracking-[0.16em] text-brand-peach">
                  Position Lifecycle
                </span>
                <StatusPill tone={statusTone(pos.status)} label={pos.status} />
                <span className="border border-brand-border/25 bg-app-bg px-2 py-0.5 font-mono text-xs text-content-secondary">
                  {pos.positionId}
                </span>
              </div>
              <div className="flex flex-wrap items-center gap-3">
                <AddressAvatar address={address} size={32} />
                <h1 className="text-2xl font-semibold sm:text-3xl">
                  <span
                    className={
                      pos.side === 'long' ? 'text-positive' : 'text-brand-orange'
                    }
                  >
                    {pos.side.toUpperCase()}
                  </span>{' '}
                  {pos.market}
                </h1>
              </div>
              <p className="font-mono text-xs text-content-secondary">
                {traderLabel} · Opened {formatUtc(pos.openedAt)}
                {pos.closedAt ? ` → Closed ${formatUtc(pos.closedAt)}` : ' → Active'} · Held{' '}
                {formatDurationCompact(pos.durationSeconds)}
              </p>
            </div>

            <div className="flex flex-col items-start gap-2 font-mono text-xs sm:items-end">
              <div className="flex items-center gap-2">
                <ShareCardModal
                  cards={[
                    {
                      id: `lifecycle-${pos.positionId}`,
                      label: 'Position Lifecycle',
                      data: {
                        kind: 'position_lifecycle',
                        traderLabel,
                        account: address,
                        networkName: chain.name,
                        releaseVersion,
                        market: pos.market,
                        side: pos.side,
                        status: pos.status,
                        positionId: pos.positionId,
                        netPnlAssets: pos.netPnlAssets,
                        roiBps: pos.roiBps,
                        entryPrice: pos.entryPrice,
                        exitOrMarkPrice: pos.exitPrice ?? data.currentMarkPrice,
                        peakNotionalAssets: pos.peakNotionalAssets,
                        maxLeverageLabel: formatLeverageX100(pos.maxLeverageX100),
                        mfePnlAssets: pos.mfePnlAssets,
                        maePnlAssets: pos.maePnlAssets,
                        liqBufferLabel:
                          pos.status === 'liquidated' ? '0.00% (Liq)' : liqBuffer.text,
                        durationLabel: formatDurationCompact(pos.durationSeconds),
                      },
                    },
                  ]}
                />
              </div>
              <span className="text-content-tertiary">Weighted Entry → Exit / Mark</span>
              <span className="text-base font-semibold text-content-primary">
                {formatPrice(pos.entryPrice)} →{' '}
                {pos.exitPrice ? (
                  formatPrice(pos.exitPrice)
                ) : data.currentMarkPrice ? (
                  <OraclePriceLink>{formatPrice(data.currentMarkPrice)} mark</OraclePriceLink>
                ) : (
                  'Open'
                )}
              </span>
              <span className="text-content-tertiary">
                {String(pos.fillCount)} fill{pos.fillCount === 1 ? '' : 's'}
                {pos.scaleInCount > 0 ? ` · ${String(pos.scaleInCount)} scale-in` : ''}
                {pos.marginAddCount > 0 ? ` · ${String(pos.marginAddCount)} margin add` : ''}
                {pos.partialCloseCount > 0 ? ` · ${String(pos.partialCloseCount)} partial close` : ''}
              </span>
            </div>
          </header>

          {/* Unified Position Performance & Risk Hero */}
          <Panel className="overflow-hidden">
            <div className="flex flex-wrap items-end justify-between gap-4 p-4 sm:p-6">
              <div>
                <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                  {pos.status === 'open' ? 'Net PnL (incl. uPnL)' : 'Realized Net PnL'}
                </p>
                <p className="mt-1 font-mono text-3xl font-bold tracking-tight tabular-nums sm:text-4xl">
                  <Pnl value={pos.netPnlAssets} />
                </p>
                <p className="mt-1 font-mono text-xs text-content-tertiary">
                  {pos.roiBps != null
                    ? `${formatRoi(pos.roiBps)} ROI on ${formatCompactAssets(pos.peakMarginAssets)} peak margin`
                    : `Peak margin ${formatCompactAssets(pos.peakMarginAssets)}`}
                </p>
              </div>

              <div className="text-left font-mono sm:text-right">
                <p className="text-xs text-content-tertiary">
                  {pos.status === 'open' ? 'Current Liq Buffer' : 'Closest Liq Buffer'}
                </p>
                <p
                  className={`mt-1 text-2xl font-bold tabular-nums ${
                    pos.status === 'liquidated' || liqBuffer.tone === 'negative'
                      ? 'text-brand-orange'
                      : liqBuffer.tone === 'warning'
                        ? 'text-brand-yellow'
                        : 'text-positive'
                  }`}
                >
                  {pos.status === 'liquidated' ? '0.00% (Liq)' : liqBuffer.text}
                </p>
                <p className="mt-0.5 text-xs text-content-tertiary">
                  {pos.liquidationPrice
                    ? `Liq ${formatPrice(pos.liquidationPrice)} · Maint ${formatPositionBpsPercent(data.maintenanceMarginBps)}`
                    : `Liquidation out of range · Maint ${formatPositionBpsPercent(data.maintenanceMarginBps)}`}
                </p>
              </div>
            </div>

            <dl className="grid grid-cols-1 divide-y divide-brand-border/15 border-t border-brand-border/20 bg-app-bg/35 sm:grid-cols-2 sm:divide-x sm:divide-y-0">
              <div className="px-4 py-3.5 sm:px-6">
                <dt className="text-xs text-content-tertiary">Peak Exposure &amp; Leverage</dt>
                <dd className="mt-1 font-mono text-lg font-bold tabular-nums text-brand-peach">
                  {formatCompactAssets(pos.peakNotionalAssets)}
                </dd>
                <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
                  {formatLeverageX100(pos.entryLeverageX100)} entry ·{' '}
                  {formatLeverageX100(pos.maxLeverageX100)} max · {formatPlDxyQuantity(pos.peakSize)}
                </p>
              </div>
              <div className="px-4 py-3.5 sm:px-6">
                <dt className="text-xs text-content-tertiary">Excursion (MFE / MAE)</dt>
                <dd className="mt-1 font-mono text-lg font-bold tabular-nums text-content-primary">
                  +{formatCompactAssets(pos.mfePnlAssets).replace(' USDG', '')} /{' '}
                  {formatCompactAssets(pos.maePnlAssets)}
                </dd>
                <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
                  {pos.captureRatioBps != null
                    ? `Captured ${formatPositionBpsPercent(pos.captureRatioBps)} of peak MFE`
                    : `Peak ${formatPositionBpsPercent(pos.mfeBps, true)} / Worst ${formatPositionBpsPercent(pos.maeBps, true)}`}
                </p>
              </div>
            </dl>
          </Panel>

          {/* Hero Interactive Price / PnL / Leverage Trajectory Chart */}
          <PositionLifecycleChart
            position={pos}
            priceSeries={data.priceSeries}
            events={data.events}
            protections={data.protections}
          />

          {/* Side-by-side Context Panels: Excursion & Risk Anatomy + Economics & Protections */}
          <div className="grid gap-4 lg:grid-cols-2">
            <Panel className="p-5 sm:p-6">
              <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                Excursion &amp; Liquidation Anatomy
              </p>
              <p className="mt-1 text-xs text-content-tertiary">
                How far the market moved against (MAE) and in favor of (MFE) this position before exit
              </p>

              <div className="mt-5">
                <ExcursionMiniBar
                  maeAssets={pos.maePnlAssets}
                  mfeAssets={pos.mfePnlAssets}
                  actualAssets={pos.netPnlAssets}
                  size="lg"
                />
              </div>

              <div className="mt-4 grid grid-cols-1 gap-2.5 text-xs sm:grid-cols-3 sm:gap-3">
                <div className="border border-brand-orange/25 bg-brand-orange/[0.04] p-3">
                  <p className="text-[10px] font-semibold uppercase tracking-wider text-brand-orange">
                    MAE (Worst)
                  </p>
                  <p className="mt-1 font-mono text-sm font-semibold text-brand-orange">
                    {formatCompactAssets(pos.maePnlAssets)}
                  </p>
                  <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
                    {pos.maePrice ? `@ ${formatPrice(pos.maePrice)}` : '—'} ·{' '}
                    {formatPositionBpsPercent(pos.maeBps, true)}
                  </p>
                </div>

                <div className="border border-brand-border/25 bg-app-bg p-3">
                  <p className="text-[10px] font-semibold uppercase tracking-wider text-content-tertiary">
                    {pos.status === 'open' ? 'Current Net' : 'Actual Exit'}
                  </p>
                  <p className="mt-1 font-mono text-sm font-semibold">
                    <Pnl value={pos.netPnlAssets} />
                  </p>
                  <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
                    {pos.captureRatioBps != null
                      ? `${formatPositionBpsPercent(pos.captureRatioBps)} MFE cap`
                      : 'Marker on bar'}
                  </p>
                </div>

                <div className="border border-positive/25 bg-positive/[0.04] p-3">
                  <p className="text-[10px] font-semibold uppercase tracking-wider text-positive">
                    MFE (Peak)
                  </p>
                  <p className="mt-1 font-mono text-sm font-semibold text-positive">
                    +{formatCompactAssets(pos.mfePnlAssets)}
                  </p>
                  <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
                    {pos.mfePrice ? `@ ${formatPrice(pos.mfePrice)}` : '—'} ·{' '}
                    {formatPositionBpsPercent(pos.mfeBps, true)}
                  </p>
                </div>
              </div>

              <div className="mt-5 border-t border-brand-border/20 pt-4 text-xs">
                <div className="flex items-center justify-between">
                  <span className="font-semibold text-content-secondary">
                    Closest liquidation proximity
                  </span>
                  <span className="font-mono font-semibold text-content-primary">
                    {pos.closestLiquidationBps != null
                      ? `${(pos.closestLiquidationBps / 100).toFixed(2)}% from liquidation`
                      : 'Unavailable'}
                  </span>
                </div>
                {pos.closestLiquidationAt ? (
                  <p className="mt-1 font-mono text-[11px] text-content-tertiary">
                    Tightest point observed {formatUtc(pos.closestLiquidationAt)} · Liq price{' '}
                    {pos.liquidationPrice ? formatPrice(pos.liquidationPrice) : 'Out of range'}
                  </p>
                ) : null}
              </div>
            </Panel>

            <Panel className="p-5 sm:p-6">
              <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                Economics &amp; Armed Protections
              </p>
              <dl className="mt-4 divide-y divide-brand-border/15 text-xs">
                <div className="flex items-center justify-between py-2">
                  <dt className="text-content-secondary">Peak position size</dt>
                  <dd className="font-mono font-semibold text-content-primary">
                    {formatPlDxyQuantity(pos.peakSize)}
                  </dd>
                </div>
                <div className="flex items-center justify-between py-2">
                  <dt className="text-content-secondary">Peak locked margin</dt>
                  <dd className="font-mono text-content-primary">
                    {formatAssets(pos.peakMarginAssets)}
                  </dd>
                </div>
                <div className="flex items-center justify-between py-2">
                  <dt className="text-content-secondary">Realized PnL</dt>
                  <dd className="font-mono font-semibold">
                    <Pnl value={pos.realizedPnlAssets} />
                  </dd>
                </div>
                {pos.status === 'open' ? (
                  <div className="flex items-center justify-between py-2">
                    <dt className="text-content-secondary">Unrealized PnL (live)</dt>
                    <dd className="font-mono font-semibold">
                      <Pnl value={pos.unrealizedPnlAssets} />
                    </dd>
                  </div>
                ) : null}
                <div className="flex items-center justify-between py-2">
                  <dt className="text-content-secondary">Execution fees</dt>
                  <dd className="font-mono text-content-primary">
                    {formatAssets(pos.totalFeesAssets)}
                  </dd>
                </div>
                <div className="flex items-center justify-between py-2">
                  <dt className="text-content-secondary">Net VPI impact / rebate</dt>
                  <dd className="font-mono text-content-primary">
                    {formatSignedAssets(pos.totalVpiAssets)}
                  </dd>
                </div>
                {BigInt(pos.totalKeeperBountyAssets) > 0n ? (
                  <div className="flex items-center justify-between py-2">
                    <dt className="text-content-secondary">Liquidation keeper bounty</dt>
                    <dd className="font-mono text-brand-orange">
                      {formatAssets(pos.totalKeeperBountyAssets)}
                    </dd>
                  </div>
                ) : null}
              </dl>

              <div className="mt-4 border-t border-brand-border/20 pt-4">
                <p className="text-[11px] font-semibold uppercase tracking-wider text-content-tertiary">
                  TP / SL Protection Events ({data.protections.length})
                </p>
                {data.protections.length > 0 ? (
                  <ul className="mt-2 space-y-1.5 text-xs">
                    {data.protections.map((pr) => {
                      const trig =
                        typeof pr.snapshot.triggerPrice === 'string'
                          ? pr.snapshot.triggerPrice
                          : typeof pr.snapshot.triggerPrice === 'number'
                            ? String(pr.snapshot.triggerPrice)
                            : null
                      return (
                        <li
                          key={`${pr.protectionId}-${pr.blockNumber}-${String(pr.logIndex)}`}
                          className="flex items-center justify-between gap-2 border border-brand-border/20 bg-app-bg px-3 py-1.5 font-mono text-[11px]"
                        >
                          <span className="text-content-primary">
                            #{pr.protectionId} · {pr.eventName}
                          </span>
                          <span className="text-content-secondary">
                            {trig ? `Trigger ${formatPrice(trig)}` : `Block ${pr.blockNumber}`}
                          </span>
                        </li>
                      )
                    })}
                  </ul>
                ) : (
                  <p className="mt-1.5 text-xs text-content-tertiary">
                    No PositionProtectionBook TP/SL events recorded during this position window.
                  </p>
                )}
              </div>
            </Panel>
          </div>

          {/* Full Chronological Step-by-Step Lifecycle Timeline */}
          <section aria-labelledby="position-timeline-title" className="space-y-3">
            <div>
              <h2 id="position-timeline-title" className="text-xl font-semibold">
                Position lifecycle timeline ({data.events.length})
              </h2>
              <p className="mt-1 text-xs text-content-tertiary">
                Every on-chain fill, scale-in, margin adjustment, partial close, and termination in chronological order
              </p>
            </div>

            <Panel>
              {data.events.length > 0 ? (
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[780px] text-left text-sm">
                    <caption className="sr-only">
                      Chronological position lifecycle events from open to close
                    </caption>
                    <thead className="border-b border-brand-border/25 bg-app-bg/40 text-xs font-medium text-content-secondary">
                      <tr>
                        <th scope="col" className="whitespace-nowrap py-3 pl-4 pr-3">Time (UTC)</th>
                        <th scope="col" className="whitespace-nowrap px-3 py-3">Action</th>
                        <th scope="col" className="whitespace-nowrap px-3 py-3">Price</th>
                        <th scope="col" className="whitespace-nowrap px-3 py-3">Size Δ / Total</th>
                        <th scope="col" className="whitespace-nowrap px-3 py-3">Margin &amp; Lev</th>
                        <th scope="col" className="whitespace-nowrap px-3 py-3">Liq After</th>
                        <th scope="col" className="whitespace-nowrap px-3 py-3">Protections</th>
                        <th scope="col" className="whitespace-nowrap px-3 py-3 text-right">PnL / Fee</th>
                        <th scope="col" className="whitespace-nowrap py-3 pl-3 pr-4 text-right">Tx</th>
                      </tr>
                    </thead>
                    <tbody>
                      {(() => {
                        const seenTxKeys = new Set<string>()
                        return data.events.map((ev) => {
                          const txKey = arbiscanLinkKey('tx', ev.txHash, chainId)
                          const showArbiscan = !seenTxKeys.has(txKey)
                          if (showArbiscan) seenTxKeys.add(txKey)
                          return (
                        <tr
                          key={ev.id}
                          className="border-b border-brand-border/15 last:border-0 hover:bg-app-bg/35"
                        >
                          <td className="whitespace-nowrap py-3 pl-4 pr-3 align-top font-mono text-xs text-content-secondary">
                            {formatUtc(ev.occurredAt)}
                          </td>
                          <td className="whitespace-nowrap px-3 py-3 align-top">
                            <StatusPill tone={actionTone(ev.action)} label={ev.action} />
                          </td>
                          <td className="whitespace-nowrap px-3 py-3 align-top font-mono text-xs tabular-nums text-content-primary">
                            {ev.price ? formatPrice(ev.price) : '—'}
                          </td>
                          <td className="whitespace-nowrap px-3 py-3 align-top font-mono text-xs tabular-nums">
                            {ev.sizeDelta !== '0' ? (
                              <div className="text-content-secondary">
                                {ev.sizeDelta.startsWith('-') ? '−' : '+'}
                                {formatPlDxyQuantity(ev.sizeDelta.replace(/^-/, ''))}
                              </div>
                            ) : (
                              <div className="text-brand-yellow">
                                +{formatCompactAssets(ev.marginDeltaAssets)} margin
                              </div>
                            )}
                            <div className="mt-0.5 text-[11px] font-semibold text-content-primary">
                              →{' '}
                              {ev.resultingSize === '0'
                                ? 'Flat'
                                : formatPlDxyQuantity(ev.resultingSize)}
                            </div>
                          </td>
                          <td className="whitespace-nowrap px-3 py-3 align-top font-mono text-xs tabular-nums">
                            <div className="text-content-primary">
                              {formatCompactAssets(ev.resultingMarginAssets)}
                            </div>
                            <div className="mt-0.5 text-[11px] text-brand-peach">
                              {ev.resultingLeverageX100 > 0
                                ? formatLeverageX100(ev.resultingLeverageX100)
                                : '0×'}
                            </div>
                          </td>
                          <td className="whitespace-nowrap px-3 py-3 align-top font-mono text-xs tabular-nums text-content-secondary">
                            {ev.resultingLiquidationPrice
                              ? formatPrice(ev.resultingLiquidationPrice)
                              : '—'}
                          </td>
                          <td className="px-3 py-3 align-top">
                            {ev.activityType === 'Open' ? (
                              <OrderProtectionsInlineChecklist protections={ev.orderProtections} />
                            ) : (
                              <span className="font-mono text-xs text-content-tertiary">—</span>
                            )}
                          </td>
                          <td className="whitespace-nowrap px-3 py-3 align-top text-right font-mono text-xs tabular-nums">
                            {ev.pnlAssets != null ? (
                              <div>
                                <Pnl value={ev.pnlAssets} />
                              </div>
                            ) : null}
                            {ev.feeAssets != null && ev.feeAssets !== '0' ? (
                              <div className="text-[11px] text-content-tertiary">
                                Fee {formatCompactAssets(ev.feeAssets)}
                              </div>
                            ) : ev.pnlAssets == null ? (
                              <span className="text-content-tertiary">—</span>
                            ) : null}
                          </td>
                          <td className="whitespace-nowrap py-3 pl-3 pr-4 align-top text-right font-mono text-xs">
                            <div className="flex items-center justify-end gap-2">
                              <Link
                                to={transactionPath(ev.txHash, ev.id)}
                                className="text-brand-peach hover:underline"
                              >
                                Receipt
                              </Link>
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
                  title="No indexed fill events"
                  message="This live position has not yet been matched to indexed fill logs."
                />
              )}
            </Panel>
          </section>
        </>
      )}
    </div>
  )
}
