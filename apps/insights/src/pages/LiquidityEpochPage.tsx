import { Link, useParams } from 'react-router-dom'
import {
  formatSharePriceWad,
  InsightsApiError,
  useLiquidityEpoch,
  type DecodedMask,
  type EpochTrancheSnapshot,
  type LpSettlementObservation,
} from '../api'
import { ArbiscanButton, CopyButton } from '../components/ActionButtons'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { ResidualAccuracyMeter } from '../components/LiquidityVisuals'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { arbiscanLinkKey, resolvePerpsChain } from '../utils/chain'
import { formatCompactAssets, formatUtc, shortAddress } from '../utils/format'

const EXECUTION_PATH_LABELS: Record<LpSettlementObservation['executionPath'], string> = {
  no_matured_work: 'No matured work',
  cached_mark: 'Cached mark',
  atomic_oracle_refresh: 'Atomic oracle refresh',
  unknown: 'Unknown',
}

function readinessTone(readiness?: LpSettlementObservation['readiness'] | null): StatusTone {
  switch (readiness) {
    case 'ready':
      return 'ready'
    case 'ready_with_warnings':
    case 'incomplete':
      return 'degraded'
    case 'blocked':
    case 'faulted':
    case 'paused':
      return 'blocked'
    default:
      return 'unknown'
  }
}

function readinessLabel(readiness?: LpSettlementObservation['readiness'] | null): string {
  switch (readiness) {
    case 'ready':
      return 'Settlement ready'
    case 'ready_with_warnings':
      return 'Ready with warnings'
    case 'incomplete':
      return 'Observation incomplete'
    case 'blocked':
      return 'Settlement blocked'
    case 'faulted':
      return 'Critical fault'
    case 'paused':
      return 'Settlement paused'
    default:
      return 'Unobserved'
  }
}

function txStatusTone(status: string): StatusTone {
  if (status === 'confirmed_success') return 'ready'
  if (status === 'pending' || status === 'broadcast' || status === 'confirming' || status === 'replaced') {
    return 'degraded'
  }
  if (status === 'confirmed_revert' || status === 'failed' || status === 'manual_review') {
    return 'blocked'
  }
  return 'unknown'
}

function txStatusLabel(status: string): string {
  switch (status) {
    case 'confirmed_success':
      return 'Confirmed'
    case 'confirmed_revert':
      return 'Reverted'
    case 'manual_review':
      return 'Manual review'
    default:
      return status.replaceAll('_', ' ')
  }
}

function formatDeltaBps(bps: number | null): string | null {
  if (bps == null) return null
  const sign = bps > 0 ? '+' : ''
  return `${sign}${(bps / 100).toFixed(2)}% (1h)`
}

function MaskCard({
  title,
  description,
  mask,
  severity,
}: {
  title: string
  description: string
  mask: DecodedMask
  severity: 'warning' | 'blocked'
}) {
  const active = mask.activeBits.length > 0
  return (
    <div className="border border-brand-border/20 bg-app-bg/55 p-4">
      <div className="flex items-start justify-between gap-2">
        <h3 className="text-sm font-semibold text-content-primary">{title}</h3>
        <StatusPill
          tone={active ? (severity === 'warning' ? 'degraded' : 'blocked') : 'ready'}
          label={active ? `${String(mask.activeBits.length)} active` : 'Clear'}
        />
      </div>
      <p className="mt-1.5 text-xs leading-5 text-content-secondary">{description}</p>
      <div className="mt-3 flex flex-wrap items-center justify-between gap-2 border-t border-brand-border/15 pt-2.5 font-mono text-xs">
        <span className="text-content-tertiary">Raw: {mask.raw}</span>
        {active ? (
          <span className="text-brand-peach">
            Bits: {mask.activeBits.map((bit) => `#${String(bit)}`).join(', ')}
          </span>
        ) : (
          <span className="text-positive">0 active bits</span>
        )}
      </div>
    </div>
  )
}

function TrancheSnapshotCard({
  title,
  subtitle,
  tranche,
  assetSymbol,
}: {
  title: string
  subtitle: string
  tranche: EpochTrancheSnapshot
  assetSymbol: string
}) {
  const delta = formatDeltaBps(tranche.sharePriceDeltaBps)
  const deltaTone =
    tranche.sharePriceDeltaBps != null && tranche.sharePriceDeltaBps > 0
      ? 'text-positive'
      : tranche.sharePriceDeltaBps != null && tranche.sharePriceDeltaBps < 0
        ? 'text-brand-orange'
        : 'text-content-tertiary'

  return (
    <Panel className="space-y-4 p-5">
      <div className="flex items-baseline justify-between gap-2">
        <div>
          <p className="font-mono text-[11px] uppercase tracking-[0.14em] text-brand-peach">
            {subtitle}
          </p>
          <h3 className="mt-1 text-base font-semibold">{title}</h3>
        </div>
        {delta ? <span className={`font-mono text-xs font-semibold ${deltaTone}`}>{delta}</span> : null}
      </div>

      <dl className="grid grid-cols-2 gap-3 border-t border-brand-border/20 pt-3 text-xs">
        <div>
          <dt className="text-content-tertiary">Share price</dt>
          <dd className="mt-1 font-mono text-sm font-semibold tabular-nums text-content-primary">
            {formatSharePriceWad(tranche.sharePriceWad, assetSymbol)}
          </dd>
        </div>
        <div>
          <dt className="text-content-tertiary">Total assets</dt>
          <dd className="mt-1 font-mono text-sm font-semibold tabular-nums text-content-primary">
            {formatCompactAssets(tranche.totalAssets)}
          </dd>
        </div>
        <div>
          <dt className="text-content-tertiary">Locked assets</dt>
          <dd className="mt-1 font-mono text-sm tabular-nums text-content-secondary">
            {tranche.lockedAssets != null ? formatCompactAssets(tranche.lockedAssets) : '—'}
          </dd>
        </div>
        <div>
          <dt className="text-content-tertiary">Total shares</dt>
          <dd className="mt-1 font-mono text-sm tabular-nums text-content-secondary">
            {formatCompactAssets(tranche.totalSupply).replace(/ USDG$/, '')}
          </dd>
        </div>
      </dl>
    </Panel>
  )
}

export function LiquidityEpochPage() {
  const { epoch } = useParams()
  const validEpoch = epoch != null && /^\d+$/.test(epoch)
  const {
    chain: fallbackChain,
    releaseVersion: fallbackRelease,
    blockNumber: fallbackBlock,
    observedAt: fallbackObserved,
  } = useProtocolContext()

  const epochQuery = useLiquidityEpoch(epoch)
  const data = epochQuery.data

  const chainId = data?.network.chainId ?? fallbackChain.chainId
  const chain = resolvePerpsChain(chainId)
  const releaseVersion = data?.network.releaseVersion ?? fallbackRelease
  const assetSymbol = data?.network.settlementAssetSymbol ?? chain.defaultAssetSymbol
  const latestObs = data?.latestObservation ?? null
  const primaryTx = data?.settlements[0] ?? null

  const refBlock =
    (latestObs ? Number(latestObs.observedBlock) : null) ??
    (primaryTx?.receiptBlock ? Number(primaryTx.receiptBlock) : null) ??
    (data?.vaultSnapshot ? Number(data.vaultSnapshot.blockNumber) : null) ??
    fallbackBlock

  const isNotFound = epochQuery.error instanceof InsightsApiError && epochQuery.error.status === 404

  const bannerTone: StatusTone = !validEpoch || isNotFound
    ? 'unknown'
    : epochQuery.isError
      ? 'blocked'
      : primaryTx
        ? txStatusTone(primaryTx.status)
        : latestObs
          ? readinessTone(latestObs.readiness)
          : data?.vaultSnapshot
            ? 'live'
            : 'unknown'

  const bannerLabel = !validEpoch
    ? 'Invalid epoch identifier'
    : isNotFound
      ? `Epoch #${epoch} not found or backend pending`
      : epochQuery.isError
        ? 'Epoch telemetry unavailable'
        : primaryTx
          ? `Epoch #${String(data?.epoch ?? epoch)} · ${primaryTx.status}`
          : latestObs
            ? `Epoch #${String(data?.epoch ?? epoch)} · ${readinessLabel(latestObs.readiness)}`
            : data
              ? `Epoch #${String(data.epoch)} · Snapshot recorded`
              : `Loading Epoch #${epoch}`

  return (
    <div className="space-y-6 sm:space-y-8">
      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={refBlock}
        observedAt={latestObs?.observedAt ?? data?.vaultSnapshot?.blockUtc ?? fallbackObserved}
        tone={bannerTone}
        statusLabel={bannerLabel}
        note={data ? `Hour boundary: ${formatUtc(data.epochUtc)}` : 'LP epoch forensic detail'}
      />

      <div className="flex flex-wrap items-center justify-between gap-3">
        <Link
          to="/liquidity"
          className="text-sm font-semibold text-brand-peach hover:underline hover:underline-offset-4"
        >
          ← Back to LP Settlement Ops
        </Link>

        {data ? (
          <div className="flex items-center gap-2 font-mono text-xs">
            {data.previousEpoch != null ? (
              <Link
                to={`/liquidity/epochs/${String(data.previousEpoch)}`}
                className="border border-brand-border/35 bg-surface-panel px-3 py-1.5 text-content-secondary transition-colors hover:border-brand-peach hover:text-content-primary"
              >
                ← #{data.previousEpoch.toLocaleString('en-US')}
              </Link>
            ) : null}
            {data.nextEpoch != null ? (
              <Link
                to={`/liquidity/epochs/${String(data.nextEpoch)}`}
                className="border border-brand-border/35 bg-surface-panel px-3 py-1.5 text-content-secondary transition-colors hover:border-brand-peach hover:text-content-primary"
              >
                #{data.nextEpoch.toLocaleString('en-US')} →
              </Link>
            ) : null}
          </div>
        ) : null}
      </div>

      <header className="flex flex-col gap-3 sm:flex-row sm:items-end sm:justify-between">
        <div className="space-y-2">
          <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">
            LP Settlement Epoch
          </p>
          <h1 className="font-mono text-3xl font-semibold tracking-tight sm:text-4xl">
            Epoch #{validEpoch ? Number(epoch).toLocaleString('en-US') : (epoch ?? '—')}
          </h1>
          <p className="text-sm text-content-secondary">
            {data
              ? `Hourly settlement window anchored at ${formatUtc(data.epochUtc)} · Monitor ${shortAddress(data.monitorAddress)}`
              : 'Hourly LP epoch settlement attempts, vault share-price snapshots, decoded monitor bitmasks, and attribution audit.'}
          </p>
        </div>

        {data ? (
          <div className="flex flex-wrap items-center gap-2">
            {primaryTx ? (
              <StatusPill tone={txStatusTone(primaryTx.status)} label={primaryTx.status} />
            ) : null}
            {latestObs ? (
              <StatusPill
                tone={readinessTone(latestObs.readiness)}
                label={readinessLabel(latestObs.readiness)}
              />
            ) : null}
          </div>
        ) : null}
      </header>

      {!validEpoch ? (
        <Panel>
          <EmptyState
            title="Invalid LP epoch number"
            message="Epoch identifiers must be non-negative integers (for example, /liquidity/epochs/498776)."
          />
        </Panel>
      ) : epochQuery.isLoading ? (
        <Panel>
          <LoadingState rows={5} />
        </Panel>
      ) : isNotFound ? (
        <Panel className="p-6">
          <h2 className="text-lg font-semibold">No telemetry found for Epoch #{epoch}</h2>
          <p className="mt-2 text-sm leading-6 text-content-secondary">
            Either Epoch #{epoch} has no indexed settlement observations, transactions, or vault
            snapshots yet, or this environment has not deployed the{' '}
            <code className="font-mono text-xs text-content-primary">
              /api/insights/v1/liquidity/epochs/:epoch
            </code>{' '}
            endpoint yet.
          </p>
          <Link
            to="/liquidity"
            className="mt-4 inline-block text-sm font-semibold text-brand-peach hover:underline"
          >
            Return to recent settled epochs →
          </Link>
        </Panel>
      ) : epochQuery.isError ? (
        <ErrorState
          title="Unable to load epoch details"
          message={epochQuery.error.message}
          onRetry={() => void epochQuery.refetch()}
        />
      ) : data ? (
        <>
          {/* Unified Epoch Settlement & Tranche Flows Hero */}
          <section aria-labelledby="epoch-summary-title">
            <h2 id="epoch-summary-title" className="sr-only">
              Epoch summary metrics
            </h2>
            <Panel className="overflow-hidden">
              <div className="flex flex-wrap items-end justify-between gap-4 p-4 sm:p-6">
                <div>
                  <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                    Settlement Status
                  </p>
                  <p
                    className={`mt-1 font-mono text-3xl font-bold tracking-tight sm:text-4xl ${
                      primaryTx?.status === 'confirmed_success'
                        ? 'text-positive'
                        : 'text-content-primary'
                    }`}
                  >
                    {primaryTx
                      ? txStatusLabel(primaryTx.status)
                      : latestObs
                        ? readinessLabel(latestObs.readiness)
                        : 'No tx'}
                  </p>
                  <p className="mt-1 font-mono text-xs text-content-tertiary">
                    {primaryTx?.confirmationLatencySeconds != null
                      ? `Confirmed in ${String(primaryTx.confirmationLatencySeconds)}s · ${EXECUTION_PATH_LABELS[primaryTx.executionPath]}`
                      : latestObs
                        ? `Path: ${EXECUTION_PATH_LABELS[latestObs.executionPath]}`
                        : 'No settlement transaction recorded'}
                  </p>
                </div>

                <div className="text-left font-mono sm:text-right">
                  <p className="text-xs text-content-tertiary">Vault Mark Freshness</p>
                  <p
                    className={`mt-1 text-xl font-bold ${
                      data.vaultSnapshot?.markFresh === true
                        ? 'text-positive'
                        : data.vaultSnapshot?.markFresh === false
                          ? 'text-brand-yellow'
                          : 'text-content-primary'
                    }`}
                  >
                    {data.vaultSnapshot
                      ? data.vaultSnapshot.markFresh === true
                        ? 'Fresh'
                        : data.vaultSnapshot.markFresh === false
                          ? 'Stale'
                          : 'Recorded'
                      : 'Unavailable'}
                  </p>
                  <p className="mt-0.5 text-xs text-content-tertiary">
                    {data.vaultSnapshot
                      ? 'Hourly boundary mark verified'
                      : 'No hourly vault snapshot at boundary'}
                  </p>
                </div>
              </div>

              <dl className="grid grid-cols-1 divide-y divide-brand-border/15 border-t border-brand-border/20 bg-app-bg/35 sm:grid-cols-2 sm:divide-x sm:divide-y-0">
                <div className="px-4 py-3.5 sm:px-6">
                  <dt className="text-xs text-content-tertiary">Senior Flows (Deposit / Redeem)</dt>
                  <dd className="mt-1 font-mono text-lg font-bold tabular-nums text-content-primary">
                    {primaryTx
                      ? `${formatCompactAssets(primaryTx.seniorDepositAssets).replace(/ USDG$/, '')} / ${formatCompactAssets(primaryTx.seniorRedeemAssets)}`
                      : 'Unavailable'}
                  </dd>
                  <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
                    {primaryTx
                      ? primaryTx.seniorBacklog
                        ? 'Senior backlog flagged'
                        : 'No senior backlog'
                      : 'Awaiting settlement receipt'}
                  </p>
                </div>
                <div className="px-4 py-3.5 sm:px-6">
                  <dt className="text-xs text-content-tertiary">Junior Flows (Deposit / Redeem)</dt>
                  <dd className="mt-1 font-mono text-lg font-bold tabular-nums text-content-primary">
                    {primaryTx
                      ? `${formatCompactAssets(primaryTx.juniorDepositAssets).replace(/ USDG$/, '')} / ${formatCompactAssets(primaryTx.juniorRedeemAssets)}`
                      : 'Unavailable'}
                  </dd>
                  <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
                    {primaryTx
                      ? primaryTx.juniorBacklog
                        ? 'Junior backlog flagged'
                        : 'No junior backlog'
                      : 'Awaiting settlement receipt'}
                  </p>
                </div>
              </dl>
            </Panel>
          </section>

          {/* Vault Snapshot */}
          <section aria-labelledby="epoch-vaults-title" className="space-y-3">
            <div className="flex flex-wrap items-baseline justify-between gap-2">
              <h2 id="epoch-vaults-title" className="text-lg font-semibold">
                Hourly vault state at epoch boundary
              </h2>
              {data.vaultSnapshot ? (
                <p className="font-mono text-xs text-content-tertiary">
                  Snapshot block {Number(data.vaultSnapshot.blockNumber).toLocaleString('en-US')} ·{' '}
                  {formatUtc(data.vaultSnapshot.blockUtc)}
                </p>
              ) : null}
            </div>
            {data.vaultSnapshot ? (
              <div className="grid gap-4 md:grid-cols-2">
                <TrancheSnapshotCard
                  title="Senior Vault"
                  subtitle="Protected Yield Tranche"
                  tranche={data.vaultSnapshot.senior}
                  assetSymbol={assetSymbol}
                />
                <TrancheSnapshotCard
                  title="Junior Vault"
                  subtitle="First-Loss Counterparty Tranche"
                  tranche={data.vaultSnapshot.junior}
                  assetSymbol={assetSymbol}
                />
              </div>
            ) : (
              <Panel>
                <EmptyState
                  title="No hourly vault snapshot recorded for this epoch"
                  message="Vault performance snapshots are captured at UTC hour boundaries once the performance indexer covers this epoch."
                />
              </Panel>
            )}
          </section>

          {/* Settlement Attempts Table */}
          <section aria-labelledby="epoch-txs-title" className="space-y-3">
            <h2 id="epoch-txs-title" className="text-lg font-semibold">
              Settlement transactions ({data.settlements.length})
            </h2>
            <Panel>
              {data.settlements.length > 0 ? (
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[780px] text-left text-sm">
                    <thead className="border-b border-brand-border/25 bg-app-bg/40 text-xs font-medium text-content-secondary">
                      <tr>
                        <th scope="col" className="px-4 py-3">Status</th>
                        <th scope="col" className="px-4 py-3">Path</th>
                        <th scope="col" className="px-4 py-3">Cutoff</th>
                        <th scope="col" className="px-4 py-3">Latency</th>
                        <th scope="col" className="px-4 py-3">Senior dep / red</th>
                        <th scope="col" className="px-4 py-3">Junior dep / red</th>
                        <th scope="col" className="px-4 py-3">Flags</th>
                        <th scope="col" className="px-4 py-3">Receipt</th>
                      </tr>
                    </thead>
                    <tbody>
                      {(() => {
                        const seenTxKeys = new Set<string>()
                        return data.settlements.map((row) => {
                          const flags: string[] = []
                          if (row.seniorBacklog) flags.push('Senior backlog')
                          if (row.juniorBacklog) flags.push('Junior backlog')
                          if (row.entriesDeferred) flags.push('Entries deferred')
                          if (row.replacementCount > 0) {
                            flags.push(`Replaced ×${String(row.replacementCount)}`)
                          }
                          let showArbiscan = false
                          if (row.txHash) {
                            const txKey = arbiscanLinkKey('tx', row.txHash, chainId)
                            if (!seenTxKeys.has(txKey)) {
                              seenTxKeys.add(txKey)
                              showArbiscan = true
                            }
                          }
                          return (
                          <tr
                            key={`${String(row.epoch)}-${row.txHash ?? row.createdAt}`}
                            className="border-b border-brand-border/15 last:border-0"
                          >
                            <td className="px-4 py-3.5">
                              <StatusPill tone={txStatusTone(row.status)} label={row.status} />
                            </td>
                            <td className="px-4 py-3.5 text-xs text-content-secondary">
                              {EXECUTION_PATH_LABELS[row.executionPath]}
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs text-content-secondary">
                              {row.cutoffEpoch ? `#${row.cutoffEpoch}` : '—'}
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                              {row.confirmationLatencySeconds != null
                                ? `${String(row.confirmationLatencySeconds)}s`
                                : '—'}
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                              {formatCompactAssets(row.seniorDepositAssets)} /{' '}
                              {formatCompactAssets(row.seniorRedeemAssets)}
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                              {formatCompactAssets(row.juniorDepositAssets)} /{' '}
                              {formatCompactAssets(row.juniorRedeemAssets)}
                            </td>
                            <td className="px-4 py-3.5 text-xs text-content-secondary">
                              {flags.length > 0 ? flags.join(' · ') : 'Clean'}
                            </td>
                            <td className="px-4 py-3.5 font-mono text-xs">
                              {row.txHash ? (
                                <div className="flex items-center gap-2">
                                  <Link
                                    to={`/tx/${row.txHash}`}
                                    className="text-brand-peach hover:underline"
                                  >
                                    {shortAddress(row.txHash)}
                                  </Link>
                                  {showArbiscan ? (
                                    <ArbiscanButton kind="tx" value={row.txHash} chainId={chainId} />
                                  ) : null}
                                </div>
                              ) : (
                                '—'
                              )}
                            </td>
                          </tr>
                        )
                      })})()}
                    </tbody>
                  </table>
                </div>
              ) : (
                <EmptyState
                  title="No settlement transaction broadcast for this epoch"
                  message="This epoch may have had no matured LP work or only monitor observations without a broadcast settlement transaction."
                />
              )}
            </Panel>
          </section>

          {/* Decoded Monitor Bitmasks */}
          {latestObs ? (
            <section aria-labelledby="epoch-masks-title" className="space-y-3">
              <div className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 id="epoch-masks-title" className="text-lg font-semibold">
                  SettlementMonitorLens diagnostics
                </h2>
                <div className="inline-flex items-center gap-1.5 font-mono text-xs text-content-secondary">
                  <span title={data.monitorAddress}>
                    Monitor {shortAddress(data.monitorAddress)}
                  </span>
                  <CopyButton value={data.monitorAddress} />
                  <ArbiscanButton kind="address" value={data.monitorAddress} chainId={chainId} />
                </div>
              </div>
              <div className="grid gap-4 md:grid-cols-2">
                <MaskCard
                  title="Operational blocker mask"
                  description="Blocks keeper submission until cleared (e.g., pending queue preconditions or house pool lock)."
                  mask={latestObs.masks.operationalBlocker}
                  severity="blocked"
                />
                <MaskCard
                  title="Warning mask"
                  description="Non-blocking conditions worth monitoring (e.g., cached mark age or approaching thresholds)."
                  mask={latestObs.masks.warning}
                  severity="warning"
                />
                <MaskCard
                  title="Dependency failure mask"
                  description="Upstream contract or oracle read failures reported by SettlementMonitorLens."
                  mask={latestObs.masks.dependencyFailure}
                  severity="blocked"
                />
                <MaskCard
                  title="Critical fault mask"
                  description="Invariant or accounting faults that immediately halt automated LP epoch settlement."
                  mask={latestObs.masks.criticalFault}
                  severity="blocked"
                />
              </div>
            </section>
          ) : null}

          {/* Covering Attribution Report */}
          {data.coveringReport ? (
            <section aria-labelledby="epoch-attribution-title" className="space-y-3">
              <h2 id="epoch-attribution-title" className="text-lg font-semibold">
                Covering Junior return attribution audit
              </h2>
              <Panel className="flex flex-col gap-4 p-5 sm:flex-row sm:items-center sm:justify-between">
                <div className="space-y-1 text-xs">
                  <p className="font-mono text-sm font-semibold text-content-primary">
                    Blocks {Number(data.coveringReport.startBlockNumber).toLocaleString('en-US')} →{' '}
                    {Number(data.coveringReport.endBlockNumber).toLocaleString('en-US')}
                  </p>
                  <p className="font-mono text-content-tertiary">
                    Schema v{String(data.coveringReport.schemaVersion)} · Net return{' '}
                    {formatCompactAssets(data.coveringReport.netReturnUsdg)} · Updated{' '}
                    {formatUtc(data.coveringReport.updatedAt)}
                  </p>
                </div>
                <ResidualAccuracyMeter report={data.coveringReport} />
              </Panel>
            </section>
          ) : null}
        </>
      ) : null}
    </div>
  )
}
