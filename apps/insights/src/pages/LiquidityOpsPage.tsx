import { Link } from 'react-router-dom'
import {
  InsightsApiError,
  useLiquidityForensics,
  useLiquiditySettlements,
  type DecodedMask,
  type LpSettlementObservation,
  type SnapshotFreshnessWindow,
} from '../api'
import { ArbiscanButton, CopyButton } from '../components/ActionButtons'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import {
  ResidualAccuracyMeter,
  SettlementContinuityStrip,
} from '../components/LiquidityVisuals'
import { StatCard } from '../components/StatCard'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { arbiscanLinkKey, resolvePerpsChain, vaultsAppUrl } from '../utils/chain'
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
      return 'Awaiting observation'
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

function FreshnessBar({ title, windowData }: { title: string; windowData: SnapshotFreshnessWindow }) {
  const observedPct =
    windowData.expectedHours > 0
      ? Math.min(100, (windowData.observedHours / windowData.expectedHours) * 100)
      : 0
  const freshPct =
    windowData.expectedHours > 0
      ? Math.min(100, (windowData.freshHours / windowData.expectedHours) * 100)
      : 0

  return (
    <div className="space-y-2 border border-brand-border/20 bg-app-bg/50 p-4">
      <div className="flex items-baseline justify-between gap-2">
        <h3 className="text-sm font-semibold">{title}</h3>
        <span className="font-mono text-xs text-content-secondary tabular-nums">
          {windowData.freshHours}/{windowData.expectedHours} fresh hrs
        </span>
      </div>
      <div className="h-2.5 w-full overflow-hidden bg-surface-raised">
        <div className="flex h-full">
          <div className="bg-positive" style={{ width: `${freshPct.toFixed(2)}%` }} />
          <div
            className="bg-brand-yellow"
            style={{ width: `${Math.max(0, observedPct - freshPct).toFixed(2)}%` }}
          />
        </div>
      </div>
      <div className="flex flex-wrap justify-between gap-2 text-xs text-content-tertiary">
        <span>Observed: {windowData.observedHours} hrs</span>
        <span>Stale-mark gaps: {windowData.staleHours}</span>
        <span>
          Latest mark:{' '}
          {windowData.latestMarkFresh === true
            ? 'Fresh'
            : windowData.latestMarkFresh === false
              ? 'Stale'
              : 'Unavailable'}
        </span>
      </div>
    </div>
  )
}

function isEndpointNotDeployed(error: unknown): boolean {
  return error instanceof InsightsApiError && error.status === 404
}

export function LiquidityOpsPage() {
  const protocol = useProtocolContext()
  const settlementsQuery = useLiquiditySettlements()
  const forensicsQuery = useLiquidityForensics()

  const settlements = settlementsQuery.data
  const forensics = forensicsQuery.data
  const chainId =
    settlements?.network.chainId ?? forensics?.network.chainId ?? protocol.chainId
  const chain = resolvePerpsChain(chainId)
  const releaseVersion =
    settlements?.network.releaseVersion ??
    forensics?.network.releaseVersion ??
    protocol.releaseVersion

  const latestObs = settlements?.latestObservation ?? null
  const awaitingDeploy =
    isEndpointNotDeployed(settlementsQuery.error) || isEndpointNotDeployed(forensicsQuery.error)

  const bannerTone: StatusTone = awaitingDeploy
    ? 'unknown'
    : settlementsQuery.isError && forensicsQuery.isError
      ? 'blocked'
      : latestObs
        ? readinessTone(latestObs.readiness)
        : settlementsQuery.isLoading || forensicsQuery.isLoading
          ? 'unknown'
          : 'stale'

  const bannerLabel = awaitingDeploy
    ? 'Awaiting Stage 1 backend deploy'
    : settlementsQuery.isError && forensicsQuery.isError
      ? 'LP telemetry unavailable'
      : latestObs
        ? readinessLabel(latestObs.readiness)
        : settlementsQuery.isLoading || forensicsQuery.isLoading
          ? 'Loading LP settlement telemetry'
          : 'No settlement observations yet'

  const blockNumber =
    latestObs?.observedBlock ??
    forensics?.attributionWorker.indexedThroughBlock ??
    protocol.blockNumber
  const observedAt = latestObs?.observedAt ?? forensics?.observedAt ?? protocol.observedAt

  const fresh7d = forensics?.snapshotFreshness.window7d ?? null
  const activityIndexer = forensics?.vaultIndexers.activity ?? null
  const depositAttr = forensics?.vaultIndexers.depositAttribution ?? null

  return (
    <div className="space-y-8">
      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={blockNumber}
        observedAt={observedAt}
        tone={bannerTone}
        statusLabel={bannerLabel}
        note={
          settlements?.monitorAddress
            ? `Monitor ${shortAddress(settlements.monitorAddress)}`
            : 'LP Settlement Engine & Risk Forensics'
        }
      />

      <header className="flex flex-col justify-between gap-4 sm:flex-row sm:items-end">
        <div className="max-w-3xl space-y-3">
          <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">
            LP Settlement Engine &amp; Risk Forensics
          </p>
          <h1 className="text-3xl font-semibold tracking-tight sm:text-4xl">
            Epoch Settlements, Attribution Audit &amp; Pool Forensics
          </h1>
          <p className="text-sm leading-6 text-content-secondary">
            Deep settlement and accounting telemetry complementing the main app’s Vaults page—
            auditing hourly epoch execution, decoded settlement bitmasks, trace-attribution
            residuals, stale-mark snapshot coverage, and vault indexer lag.
          </p>
        </div>
        <a
          href={vaultsAppUrl(chainId)}
          className="shrink-0 border border-brand-border/40 px-4 py-2.5 text-sm font-semibold text-brand-peach hover:bg-brand-peach/10"
        >
          Open Vaults on App ↗
        </a>
      </header>

      {awaitingDeploy ? (
        <div
          role="status"
          className="border border-brand-yellow/40 bg-brand-yellow/10 px-4 py-3 text-sm text-brand-yellow"
        >
          <strong>Connected backend does not serve Stage 1 LP endpoints yet (HTTP 404).</strong>{' '}
          Deploy the updated backend containing{' '}
          <code className="font-mono text-xs">/api/insights/v1/liquidity/settlements</code> and{' '}
          <code className="font-mono text-xs">/api/insights/v1/liquidity/forensics</code> to
          populate live LP settlement and forensics data here.
        </div>
      ) : null}

      <section aria-labelledby="lp-ops-kpis-title" className="space-y-4">
        <h2 id="lp-ops-kpis-title" className="text-xl font-semibold">
          Settlement &amp; accounting health
        </h2>
        <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
          <StatCard
            label="Latest LP epoch"
            loading={settlementsQuery.isLoading}
            unavailable={!latestObs}
            value={latestObs ? `#${latestObs.epoch.toLocaleString('en-US')}` : undefined}
            tone={latestObs?.readiness === 'ready' ? 'positive' : 'accent'}
            detail={
              latestObs
                ? `${EXECUTION_PATH_LABELS[latestObs.executionPath]} · Health: ${latestObs.healthState}`
                : 'Awaiting settlement monitor observation'
            }
          />
          <StatCard
            label="Confirmed settlements (24h)"
            loading={settlementsQuery.isLoading}
            unavailable={!settlements}
            value={
              settlements
                ? settlements.summary.confirmedSettlements24h.toLocaleString('en-US')
                : undefined
            }
            tone={
              settlements && settlements.summary.failedOrReverted24h > 0 ? 'warning' : 'positive'
            }
            detail={
              settlements
                ? `${String(settlements.summary.observedEpochs24h)} epochs observed · ${String(settlements.summary.failedOrReverted24h)} failed · ${String(settlements.summary.replacedAttempts24h)} gas-bumped`
                : '24h LP settlement tx summary'
            }
          />
          <StatCard
            label="7d mark freshness"
            loading={forensicsQuery.isLoading}
            unavailable={!fresh7d}
            value={
              fresh7d
                ? `${String(fresh7d.freshHours)}/${String(fresh7d.expectedHours)} hrs`
                : undefined
            }
            tone={fresh7d && fresh7d.staleHours > 0 ? 'warning' : 'positive'}
            detail={
              fresh7d
                ? `${String(fresh7d.staleHours)} stale-mark gaps across ${String(fresh7d.observedHours)} observed hours`
                : 'Hourly vault_performance_snapshots audit'
            }
          />
          <StatCard
            label="Attribution & indexer head"
            loading={forensicsQuery.isLoading}
            unavailable={!forensics?.attributionWorker.indexedThroughBlock && !activityIndexer}
            value={
              forensics?.attributionWorker.indexedThroughBlock
                ? Number(forensics.attributionWorker.indexedThroughBlock).toLocaleString('en-US')
                : activityIndexer
                  ? Number(activityIndexer.lastIndexedBlock).toLocaleString('en-US')
                  : undefined
            }
            detail={
              activityIndexer
                ? `Vault activity lag: ${activityIndexer.lagBlocks.toLocaleString('en-US')} blocks (${String(activityIndexer.lagSeconds)}s)`
                : 'Trace-proven return attribution block'
            }
          />
        </div>
      </section>

      <section aria-labelledby="settlement-masks-title" className="space-y-4">
        <div className="flex flex-wrap items-baseline justify-between gap-2">
          <div>
            <h2 id="settlement-masks-title" className="text-xl font-semibold">
              Latest epoch settlement masks
            </h2>
            <p className="mt-1 text-xs text-content-tertiary">
              Sanitized on-chain SettlementMonitorLens observation flags — zero raw RPC errors or
              calldata
            </p>
          </div>
          {settlements?.monitorAddress ? (
            <div className="inline-flex items-center gap-1.5 font-mono text-xs text-content-secondary">
              <span title={settlements.monitorAddress}>
                Monitor {shortAddress(settlements.monitorAddress)}
              </span>
              <CopyButton value={settlements.monitorAddress} />
              <ArbiscanButton
                kind="address"
                value={settlements.monitorAddress}
                chainId={chainId}
              />
            </div>
          ) : null}
        </div>

        {settlementsQuery.isLoading ? (
          <Panel>
            <LoadingState rows={3} />
          </Panel>
        ) : settlementsQuery.isError && !awaitingDeploy ? (
          <ErrorState
            title="Settlement monitor telemetry unavailable"
            message={settlementsQuery.error.message}
            onRetry={() => void settlementsQuery.refetch()}
          />
        ) : latestObs ? (
          <Panel className="space-y-4 p-5">
            <div className="flex flex-wrap items-center gap-3 text-xs text-content-secondary">
              <StatusPill
                tone={readinessTone(latestObs.readiness)}
                label={readinessLabel(latestObs.readiness)}
              />
              <span>
                Observation complete:{' '}
                <strong className="text-content-primary">
                  {latestObs.observationComplete === null
                    ? 'Unknown'
                    : latestObs.observationComplete
                      ? 'Yes'
                      : 'No'}
                </strong>
              </span>
              <span>
                Matured work:{' '}
                <strong className="text-content-primary">
                  {latestObs.hasMaturedWork === null
                    ? 'Unknown'
                    : latestObs.hasMaturedWork
                      ? 'Queued'
                      : 'None'}
                </strong>
              </span>
              <span>
                Paused:{' '}
                <strong className="text-content-primary">
                  {latestObs.paused === null ? 'Unknown' : latestObs.paused ? 'Yes' : 'No'}
                </strong>
              </span>
              <Link
                to={`/liquidity/epochs/${String(latestObs.epoch)}`}
                className="ml-auto font-mono font-semibold text-brand-peach hover:underline"
              >
                Inspect Epoch #{latestObs.epoch} →
              </Link>
            </div>

            <div className="grid gap-3 md:grid-cols-2">
              <MaskCard
                title="Operational blocker mask"
                description="Conditions preventing epoch execution (cooldowns, pause state, queue guards)."
                mask={latestObs.masks.operationalBlocker}
                severity="blocked"
              />
              <MaskCard
                title="Warning mask"
                description="Non-blocking advisory flags surfaced by SettlementMonitorLens."
                mask={latestObs.masks.warning}
                severity="warning"
              />
              <MaskCard
                title="Dependency failure mask"
                description="Combined status & health dependency failures (oracle freshness, engine/clearinghouse state)."
                mask={latestObs.masks.dependencyFailure}
                severity="blocked"
              />
              <MaskCard
                title="Critical fault mask"
                description="Terminal NAV or accounting invariant violations halting automated settlement."
                mask={latestObs.masks.criticalFault}
                severity="blocked"
              />
            </div>
          </Panel>
        ) : (
          <Panel>
            <EmptyState
              title="No settlement monitor observations available"
              message="Once SettlementMonitorLens observations are indexed by /api/insights/v1/liquidity/settlements, live bitmask diagnostics will appear here."
            />
          </Panel>
        )}
      </section>

      <section aria-labelledby="recent-settlements-title" className="space-y-4">
        <div className="flex flex-wrap items-baseline justify-between gap-2">
          <h2 id="recent-settlements-title" className="text-xl font-semibold">
            Recent LP epoch settlements
          </h2>
          {settlements ? (
            <span className="text-xs text-content-tertiary">
              24h backlog epochs: {settlements.summary.backlogEpochs24h} · Deferred entries:{' '}
              {settlements.summary.deferredEntryEpochs24h}
            </span>
          ) : null}
        </div>

        {!settlementsQuery.isLoading && (settlements || forensics) ? (
          <SettlementContinuityStrip
            settlements={settlements?.recentSettlements ?? []}
            reports={forensics?.attributionWorker.recentReports}
          />
        ) : null}

        <Panel>
          {settlementsQuery.isLoading ? (
            <LoadingState rows={4} />
          ) : settlements && settlements.recentSettlements.length > 0 ? (
            <div className="overflow-x-auto">
              <table className="w-full min-w-[860px] text-left text-sm">
                <caption className="sr-only">Recent hourly LP epoch settlement transactions</caption>
                <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                  <tr>
                    <th scope="col" className="px-4 py-3.5">Epoch</th>
                    <th scope="col" className="px-4 py-3.5">Status</th>
                    <th scope="col" className="px-4 py-3.5">Path</th>
                    <th scope="col" className="px-4 py-3.5">Latency</th>
                    <th scope="col" className="px-4 py-3.5">Senior (Dep / Red)</th>
                    <th scope="col" className="px-4 py-3.5">Junior (Dep / Red)</th>
                    <th scope="col" className="px-4 py-3.5">Flags</th>
                    <th scope="col" className="px-4 py-3.5">Tx</th>
                  </tr>
                </thead>
                <tbody>
                  {(() => {
                    const seenTxKeys = new Set<string>()
                    return settlements.recentSettlements.map((row) => {
                      const flags: string[] = []
                      if (row.seniorBacklog) flags.push('Senior backlog')
                      if (row.juniorBacklog) flags.push('Junior backlog')
                      if (row.entriesDeferred) flags.push('Entries deferred')
                      if (row.replacementCount > 0) flags.push(`Replaced ×${String(row.replacementCount)}`)
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
                        <td className="px-4 py-3.5 font-mono">
                          <Link
                            to={`/liquidity/epochs/${String(row.epoch)}`}
                            className="font-semibold text-brand-peach hover:underline"
                          >
                            #{row.epoch}
                          </Link>
                        </td>
                        <td className="px-4 py-3.5">
                          <StatusPill tone={txStatusTone(row.status)} label={row.status} />
                        </td>
                        <td className="px-4 py-3.5 text-xs text-content-secondary">
                          {EXECUTION_PATH_LABELS[row.executionPath]}
                        </td>
                        <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                          {row.confirmationLatencySeconds !== null
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
                            <div className="inline-flex items-center gap-1.5 text-content-secondary">
                              <Link
                                to={`/tx/${row.txHash}`}
                                className="text-brand-peach hover:underline"
                                title={row.txHash}
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
              title="No LP epoch settlement transactions recorded"
              message="Settled hourly epoch transactions, capital flows, backlog flags, and confirmation latencies will appear here once indexed."
            />
          )}
        </Panel>
      </section>

      <section aria-labelledby="forensics-title" className="space-y-4">
        <h2 id="forensics-title" className="text-xl font-semibold">
          Attribution residuals &amp; vault indexer forensics
        </h2>

        {forensicsQuery.isLoading ? (
          <Panel>
            <LoadingState rows={3} />
          </Panel>
        ) : forensicsQuery.isError && !awaitingDeploy ? (
          <ErrorState
            title="Vault forensics unavailable"
            message={forensicsQuery.error.message}
            onRetry={() => void forensicsQuery.refetch()}
          />
        ) : forensics ? (
          <div className="space-y-4">
            <div className="grid gap-4 lg:grid-cols-2">
              <FreshnessBar
                title="7-day hourly snapshot coverage"
                windowData={forensics.snapshotFreshness.window7d}
              />
              <FreshnessBar
                title="30-day hourly snapshot coverage"
                windowData={forensics.snapshotFreshness.window30d}
              />
            </div>

            <div className="grid gap-4 md:grid-cols-2">
              <Panel className="p-5">
                <div className="flex items-center justify-between gap-2">
                  <h3 className="text-base font-semibold">Vault Activity Indexer</h3>
                  <StatusPill
                    tone={
                      !activityIndexer
                        ? 'unknown'
                        : activityIndexer.backfillComplete && activityIndexer.lagBlocks <= 120
                          ? 'ready'
                          : 'degraded'
                    }
                    label={
                      !activityIndexer
                        ? 'Unavailable'
                        : activityIndexer.backfillComplete
                          ? 'Backfill complete'
                          : 'Backfilling'
                    }
                  />
                </div>
                {activityIndexer ? (
                  <dl className="mt-4 space-y-2 text-xs">
                    <div className="flex justify-between gap-3">
                      <dt className="text-content-tertiary">Indexed / Safe head</dt>
                      <dd className="font-mono tabular-nums">
                        {Number(activityIndexer.lastIndexedBlock).toLocaleString('en-US')} /{' '}
                        {Number(activityIndexer.safeHeadBlock).toLocaleString('en-US')}
                      </dd>
                    </div>
                    <div className="flex justify-between gap-3">
                      <dt className="text-content-tertiary">Block / Time lag</dt>
                      <dd className="font-mono tabular-nums">
                        {activityIndexer.lagBlocks.toLocaleString('en-US')} blocks ·{' '}
                        {activityIndexer.lagSeconds}s
                      </dd>
                    </div>
                    <div className="flex justify-between gap-3">
                      <dt className="text-content-tertiary">Last successful poll</dt>
                      <dd className="font-mono tabular-nums">{activityIndexer.pollAgeSeconds}s ago</dd>
                    </div>
                  </dl>
                ) : (
                  <p className="mt-3 text-xs text-content-tertiary">No indexer state recorded.</p>
                )}
              </Panel>

              <Panel className="p-5">
                <div className="flex items-center justify-between gap-2">
                  <h3 className="text-base font-semibold">Deposit Attribution Indexer</h3>
                  <StatusPill
                    tone={
                      !depositAttr
                        ? 'unknown'
                        : depositAttr.backfillComplete
                          ? 'ready'
                          : 'degraded'
                    }
                    label={
                      !depositAttr
                        ? 'Unavailable'
                        : depositAttr.backfillComplete
                          ? 'Backfill complete'
                          : 'Backfilling'
                    }
                  />
                </div>
                {depositAttr ? (
                  <dl className="mt-4 space-y-2 text-xs">
                    <div className="flex justify-between gap-3">
                      <dt className="text-content-tertiary">Confirmed through block</dt>
                      <dd className="font-mono tabular-nums">
                        {Number(depositAttr.confirmedThroughBlock).toLocaleString('en-US')}
                      </dd>
                    </div>
                    <div className="flex justify-between gap-3">
                      <dt className="text-content-tertiary">Last successful poll</dt>
                      <dd className="font-mono tabular-nums">{depositAttr.pollAgeSeconds}s ago</dd>
                    </div>
                  </dl>
                ) : (
                  <p className="mt-3 text-xs text-content-tertiary">
                    No deposit-attribution state recorded.
                  </p>
                )}
              </Panel>
            </div>

            <Panel>
              <div className="border-b border-brand-border/20 px-5 py-4">
                <h3 className="text-base font-semibold">
                  Recent Junior return attribution residual audits
                </h3>
                <p className="mt-1 text-xs text-content-tertiary">
                  Verifies exact reconciliation between 7-day diluted share price change and the 8
                  trace-proven attribution components.
                </p>
              </div>
              {forensics.attributionWorker.recentReports.length > 0 ? (
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[820px] text-left text-sm">
                    <caption className="sr-only">
                      Junior vault return attribution residual audit history
                    </caption>
                    <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                      <tr>
                        <th scope="col" className="px-4 py-3">Block range</th>
                        <th scope="col" className="px-4 py-3">Trace blocks</th>
                        <th scope="col" className="px-4 py-3">7d return</th>
                        <th scope="col" className="px-4 py-3">Net return</th>
                        <th scope="col" className="px-4 py-3">Junior liq. fees</th>
                        <th scope="col" className="px-4 py-3">Attribution accuracy</th>
                        <th scope="col" className="px-4 py-3">Updated</th>
                      </tr>
                    </thead>
                    <tbody>
                      {forensics.attributionWorker.recentReports.map((report) => (
                        <tr
                          key={`${report.startBlockNumber}-${report.endBlockNumber}`}
                          className="border-b border-brand-border/15 last:border-0"
                        >
                          <td className="px-4 py-3 font-mono text-xs tabular-nums">
                            <div>
                              {Number(report.startBlockNumber).toLocaleString('en-US')} →{' '}
                              {Number(report.endBlockNumber).toLocaleString('en-US')}
                            </div>
                            <span className="text-[10px] text-content-tertiary">
                              schema v{report.schemaVersion}
                            </span>
                          </td>
                          <td className="px-4 py-3 font-mono text-xs tabular-nums">
                            {report.sourceBlockCount?.toLocaleString('en-US') ?? '—'}
                          </td>
                          <td className="px-4 py-3 font-mono text-xs tabular-nums">
                            {report.reportedReturnPp !== null
                              ? `${report.reportedReturnPp > 0 ? '+' : ''}${report.reportedReturnPp.toFixed(4)}%`
                              : '—'}
                          </td>
                          <td className="px-4 py-3 font-mono text-xs tabular-nums">
                            {formatCompactAssets(report.netReturnUsdg)}
                          </td>
                          <td className="px-4 py-3 font-mono text-xs tabular-nums">
                            {formatCompactAssets(report.liquidationFeesUsdg)}
                          </td>
                          <td className="px-4 py-3">
                            <ResidualAccuracyMeter report={report} />
                          </td>
                          <td className="px-4 py-3 text-xs text-content-secondary">
                            {formatUtc(report.updatedAt)}
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              ) : (
                <EmptyState
                  title="No attribution reports cached yet"
                  message="Trace-proven 7-day Junior attribution reports appear after a complete 169-hour window is indexed."
                />
              )}
            </Panel>
          </div>
        ) : (
          <Panel>
            <EmptyState
              title="Vault forensics awaiting backend endpoint"
              message="Snapshot freshness, attribution residual audits, and vault indexer lag will render here once /api/insights/v1/liquidity/forensics is available."
            />
          </Panel>
        )}
      </section>

      <div className="flex flex-wrap items-center justify-between gap-4 border-t border-brand-border/20 pt-4 text-sm font-semibold">
        <a href={vaultsAppUrl(chainId)} className="text-brand-peach hover:underline">
          Live TVL, share prices, return breakdown &amp; deposits on App ↗
        </a>
        <div className="flex flex-wrap gap-4">
          <Link to="/protocol" className="text-content-secondary hover:text-brand-peach">
            Verify LP risk parameters →
          </Link>
          <Link to="/" className="text-content-secondary hover:text-brand-peach">
            ← Back to Protocol Pulse
          </Link>
        </div>
      </div>
    </div>
  )
}
