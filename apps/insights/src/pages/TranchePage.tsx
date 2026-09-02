import { Link, Navigate, useParams } from 'react-router-dom'
import type { ProtocolAction, TrancheHistoryCheckpoint } from '../api'
import { useCurrentProtocolRelease, useTranche, useTrancheHistory } from '../api'
import { AccessibleLineChart, type ChartPoint } from '../components/Charts'
import { ActionTable, AvailabilityList, Metric, ObjectTable, PageTitle, ProtocolMeta, Section } from '../components/Protocol'
import { ErrorState, LoadingState, Panel } from '../components/ui'
import { formatUsdc } from '../utils/format'
import { dedupeBy, readRecord, readString } from '../utils/protocol'
import { buildTrancheHistoryCsv } from '../utils/trancheHistory'

export function TranchePage() {
  const { tranche = '' } = useParams()
  const normalized = tranche.toLowerCase()
  const release = useCurrentProtocolRelease()
  const releaseId = release.data?.releaseId ?? ''
  const query = useTranche(releaseId, normalized)
  const history = useTrancheHistory(releaseId, normalized)
  const historyPages = history.data?.pages ?? []
  const historyAnchor = historyPages.at(0)
  const historyProgress = historyPages.at(-1)?.history.pagination
  const historyItems = dedupeBy(
    historyPages.flatMap((page) => page.history.items),
    (action) => action.actionId,
  )
  const historyCheckpoints = dedupeBy(
    historyPages.flatMap((page) => page.history.checkpoints),
    (checkpoint) =>
      `${checkpoint.blockNumber}:${checkpoint.blockHash}:${checkpoint.formulaIdentifier}`,
  )
  const availability = dedupeBy(
    [
      ...(query.data?.availability ?? []),
      ...historyPages.flatMap((page) => page.availability),
    ],
    (item) => `${item.field}:${item.reason}`,
  )
  const series = historySeries(historyItems, historyCheckpoints)

  if (normalized !== 'senior' && normalized !== 'junior') return <Navigate to="/house-pool" replace />
  if (release.isLoading || query.isLoading) return <Panel><LoadingState rows={10} /></Panel>
  if (query.isError) return <ErrorState title="Tranche state unavailable" message={query.error.message} onRetry={() => void query.refetch()} />
  const data = query.data
  if (!data) return <ErrorState title="Tranche state unavailable" />
  const detail = readRecord(data.tranche)
  const title = normalized === 'senior' ? 'Senior tranche' : 'Junior tranche'

  return (
    <div className="space-y-7">
      <PageTitle title={title} description={normalized === 'senior' ? 'Protected principal, high-water mark, impairment gap, share economics, deposit epochs, and every indexed lifecycle action.' : 'First-loss capital, current buffer, share economics, deposit epochs, realized flows, and every indexed lifecycle action.'}>
        <Link to="/house-pool" className="text-sm font-semibold text-brand-peach hover:underline">← HousePool</Link>
      </PageTitle>
      <ProtocolMeta data={data} />
      <AvailabilityList items={availability} />
      <section className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
        <Metric label="Principal" value={formatUsdc(readString(detail.principalUsdc))} />
        <Metric label="NAV" value={formatUsdc(readString(detail.navUsdc))} />
        <Metric label="Assets per share" value={readString(detail.assetsPerShare) ?? '—'} hint="USDC:6 per 1.0 share (18 decimals)." />
        <Metric label={normalized === 'senior' ? 'Impairment gap' : 'First-loss buffer'} value={formatUsdc(readString(normalized === 'senior' ? detail.impairmentGapUsdc : detail.firstLossBufferUsdc))} tone={normalized === 'senior' && isPositiveInteger(readString(detail.impairmentGapUsdc)) ? 'critical' : 'default'} />
      </section>
      <Section title="Current tranche state"><ObjectTable value={detail} evidence={data.evidence.currentState} /></Section>
      {historyAnchor ? (
        <div aria-label="Tranche history pagination anchor" className="space-y-2">
          <p className="text-xs text-content-tertiary">Loaded history pages remain anchored to this first page’s confirmed block.</p>
          <ProtocolMeta data={historyAnchor} />
          <div aria-label="Tranche history load status" className="grid gap-2 border border-brand-border/20 bg-brand-panel/40 p-4 text-sm sm:grid-cols-2">
            <p>
              <span className="font-semibold">{String(historyItems.length)} unique actions loaded</span>
              {' · '}
              {paginationStatus('action', historyProgress?.actionsComplete)}
            </p>
            <p>
              <span className="font-semibold">{String(historyCheckpoints.length)} unique sparse checkpoints loaded</span>
              {' · '}
              {paginationStatus('checkpoint', historyProgress?.checkpointsComplete)}
            </p>
            <p className="text-xs text-content-tertiary sm:col-span-2">
              Charts and CSV use only these loaded, confirmed slices. Checkpoint pagination completion does not make sparse checkpoints a continuous history.
            </p>
          </div>
        </div>
      ) : null}
      {history.isLoading ? (
        <Panel><LoadingState rows={8} /></Panel>
      ) : history.isError ? (
        <ErrorState title="Tranche history charts unavailable" message={history.error.message} onRetry={() => void history.refetch()} />
      ) : (
        <HistoryCharts title={title} series={series} />
      )}
      <Section title="Lifecycle history" description={`${String(historyItems.length)} unique loaded actions and ${String(historyCheckpoints.length)} unique loaded sparse checkpoints across ${String(historyPages.length)} anchored API page${historyPages.length === 1 ? '' : 's'}. Deposits, requests, cancellations, epochs, claims, withdrawals, allocations, recapitalizations, pauses, and parameter changes appear when emitted and indexed.`}>
        <div className="flex justify-end border-b border-brand-border/15 px-5 py-3">
          <button type="button" onClick={() => { exportHistoryCsv(normalized, historyItems, historyCheckpoints); }} className="border border-brand-border/35 px-3 py-2 text-xs font-semibold hover:border-brand-peach">Export loaded history CSV</button>
        </div>
        {history.isLoading ? <LoadingState rows={7} /> : history.isError ? <ErrorState message={history.error.message} /> : <ActionTable actions={historyItems} releaseId={data.releaseId} />}
        {history.hasNextPage ? (
          <div className="border-t border-brand-border/15 p-4 text-center">
            <button
              type="button"
              onClick={() => void history.fetchNextPage()}
              disabled={history.isFetchingNextPage}
              className="border border-brand-border/40 px-5 py-2.5 text-sm font-semibold hover:border-brand-orange disabled:cursor-wait disabled:opacity-60"
            >
              {history.isFetchingNextPage ? 'Loading tranche history…' : 'Load more tranche history'}
            </button>
          </div>
        ) : null}
      </Section>
    </div>
  )
}

interface HistorySeries {
  principal: ChartPoint[]
  nav: ChartPoint[]
  sharePrice: ChartPoint[]
  drawdown: {
    points: ChartPoint[]
    valueLabel: string
    sourceFields: string
  }
  coverage: ChartPoint[]
  cumulativeNetFlows: ChartPoint[]
}

function HistoryCharts({ title, series }: { title: string; series: HistorySeries }) {
  return (
    <div className="grid gap-5 lg:grid-cols-2">
      <Section
        title="Principal and NAV history"
        description="Confirmed range-end contract snapshots supply principal and NAV. Each point names its source block, evidence level, and snapshot scope; the current page state is never inserted into history."
        className="lg:col-span-2"
      >
        <div className="grid gap-6 p-5 lg:grid-cols-2">
          <div>
            <h3 className="mb-3 text-sm font-semibold">Principal checkpoints</h3>
            <AccessibleLineChart
              title={`${title} principal`}
              points={series.principal}
              valueLabel="Principal"
              emptyMessage="Fewer than two confirmed history.checkpoints rows expose principalUsdc. No principal value is inferred from deposits, withdrawals, or current state."
            />
          </div>
          <div>
            <h3 className="mb-3 text-sm font-semibold">NAV checkpoints</h3>
            <AccessibleLineChart
              title={`${title} NAV`}
              points={series.nav}
              valueLabel="NAV"
              emptyMessage="Fewer than two confirmed history.checkpoints rows expose navUsdc. An ERC-4626 event's data.assets amount is not treated as tranche NAV."
            />
          </div>
        </div>
      </Section>
      <Section
        title="Cumulative indexed net flows"
        description="Derived exactly with integer arithmetic over the currently loaded action pages: chronological tranche_deposit rows add data.assets and tranche_withdraw rows subtract data.assets. Unrelated assets fields are ignored. Until action pagination completes, the series is a loaded-window total rather than an all-time flow."
      >
        <div className="p-5">
          <AccessibleLineChart
            title={`${title} cumulative indexed net flows`}
            points={series.cumulativeNetFlows}
            valueLabel="Cumulative net flow"
            emptyMessage="Fewer than two confirmed ERC-4626 tranche deposit or withdrawal rows expose an unsigned integer data.assets amount. No missing flow is estimated."
          />
        </div>
      </Section>
      <Section
        title="Share-price history"
        description="Assets per share is derived only from NAV and share supply captured at the same confirmed snapshot block. The formula identifier and evidence are exposed with every point."
      >
        <div className="p-5">
          <AccessibleLineChart
            title={`${title} share price`}
            points={series.sharePrice}
            valueLabel="Assets per share"
            emptyMessage="Fewer than two confirmed history.checkpoints rows expose assetsPerShare. Event ratios are not reconstructed."
          />
        </div>
      </Section>
      <Section
        title="Sparse checkpoint-page drawdown history"
        description={`The API supplies each sparse range-end checkpoint's drawdown with its formula and evidence (${series.drawdown.sourceFields}). Checkpoints paginate independently from lifecycle actions, and flattened pages retain the API-provided checkpoint-page values instead of recomputing a continuous running peak. This is not full-history or continuous drawdown and can miss an earlier, between-checkpoint, or intra-block peak.`}
      >
        <div className="p-5">
          <AccessibleLineChart
            title={`${title} drawdown`}
            points={series.drawdown.points}
            valueLabel={series.drawdown.valueLabel}
            emptyMessage="Fewer than two confirmed history.checkpoints rows expose drawdownUsdc. Drawdown is not reconstructed from sparse event history."
          />
        </div>
      </Section>
      <Section
        title="Solvency-coverage history"
        description="Coverage is derived only from HousePool assets and LONG/SHORT bounded-liability snapshots at the same confirmed block. The current HousePool ratio is not backfilled into history."
      >
        <div className="p-5">
          <AccessibleLineChart
            title={`${title} solvency coverage`}
            points={series.coverage}
            valueLabel="Gross coverage"
            emptyMessage="Fewer than two confirmed history.checkpoints rows expose coverageRatioBps. Coverage is not inferred from incomplete asset or liability snapshots."
          />
        </div>
      </Section>
    </div>
  )
}

function historySeries(
  actions: ProtocolAction[],
  checkpoints: TrancheHistoryCheckpoint[],
): HistorySeries {
  const chronological = [...actions].sort(compareActions)
  const chronologicalCheckpoints = [...checkpoints].sort(compareCheckpoints)
  return {
    principal: checkpointHistoryPoints(chronologicalCheckpoints, 'principalUsdc', formatUsdc),
    nav: checkpointHistoryPoints(chronologicalCheckpoints, 'navUsdc', formatUsdc),
    sharePrice: checkpointHistoryPoints(chronologicalCheckpoints, 'assetsPerShare', formatUsdc),
    drawdown: {
      points: checkpointHistoryPoints(chronologicalCheckpoints, 'drawdownUsdc', formatUsdc),
      valueLabel: 'Checkpoint-page drawdown',
      sourceFields: 'history.checkpoints.drawdownUsdc, USDC:6, API checkpoint-page scope',
    },
    coverage: checkpointHistoryPoints(chronologicalCheckpoints, 'coverageRatioBps', formatBps),
    cumulativeNetFlows: cumulativeNetFlowPoints(chronological),
  }
}

type CheckpointMetricField =
  | 'principalUsdc'
  | 'navUsdc'
  | 'assetsPerShare'
  | 'drawdownUsdc'
  | 'coverageRatioBps'

function checkpointHistoryPoints(
  checkpoints: TrancheHistoryCheckpoint[],
  field: CheckpointMetricField,
  formatter: (value: string) => string,
): ChartPoint[] {
  return checkpoints.flatMap((checkpoint) => {
    const raw = checkpoint[field]
    if (!isInteger(raw)) return []
    const value = Number(raw)
    if (!Number.isFinite(value)) return []
    return [{
      label: snapshotCheckpointLabel(checkpoint),
      value,
      displayValue: formatter(raw),
      source: snapshotCheckpointSource(checkpoint, field),
    }]
  })
}

function cumulativeNetFlowPoints(actions: ProtocolAction[]): ChartPoint[] {
  let cumulative = 0n
  return actions.flatMap((action) => {
    if (action.actionType !== 'tranche_deposit' && action.actionType !== 'tranche_withdraw') return []
    const assets = readString(readRecord(action.data).assets)
    if (assets === null || !/^\d+$/.test(assets)) return []
    const amount = BigInt(assets)
    const direction = action.actionType === 'tranche_deposit' ? 1n : -1n
    cumulative += direction * amount
    const value = Number(cumulative)
    if (!Number.isFinite(value)) return []
    return [{
      label: checkpointLabel(action),
      value,
      displayValue: formatUsdc(cumulative.toString()),
      source: `derived integer sum through ${action.actionId} · ${action.actionType} ${direction > 0n ? '+' : '-'}data.assets`,
    }]
  })
}

function compareActions(left: ProtocolAction, right: ProtocolAction): number {
  return compareIntegerStrings(left.blockNumber, right.blockNumber)
    || compareIntegerStrings(left.transactionIndex, right.transactionIndex)
    || compareIntegerStrings(left.logIndex, right.logIndex)
    || left.timestamp - right.timestamp
    || left.actionId.localeCompare(right.actionId)
}

function compareCheckpoints(
  left: TrancheHistoryCheckpoint,
  right: TrancheHistoryCheckpoint,
): number {
  return compareIntegerStrings(left.blockNumber, right.blockNumber)
    || left.timestamp - right.timestamp
    || left.blockHash.localeCompare(right.blockHash)
}

function compareIntegerStrings(left: string, right: string): number {
  if (/^\d+$/.test(left) && /^\d+$/.test(right)) {
    const leftValue = BigInt(left)
    const rightValue = BigInt(right)
    return leftValue < rightValue ? -1 : leftValue > rightValue ? 1 : 0
  }
  return left.localeCompare(right)
}

function checkpointLabel(action: ProtocolAction): string {
  return `${new Date(action.timestamp * 1000).toISOString()} · block ${action.blockNumber} · log ${action.logIndex}`
}

function snapshotCheckpointLabel(checkpoint: TrancheHistoryCheckpoint): string {
  return `${new Date(checkpoint.timestamp * 1000).toISOString()} · block ${checkpoint.blockNumber}`
}

function snapshotCheckpointSource(
  checkpoint: TrancheHistoryCheckpoint,
  field: CheckpointMetricField,
): string {
  const evidence =
    readString(checkpoint.evidence[field])
    ?? readString(checkpoint.evidence.level)
    ?? 'confirmed_range_end_contract_snapshot'
  const formula = readString(checkpoint.formula[field])
  const unit = checkpoint.units[field]
  const scopes = checkpoint.sourceScopes.length > 0
    ? `scopes ${checkpoint.sourceScopes.map((source) => source.scope).join(', ')}`
    : 'snapshot scopes unavailable'
  return [
    `history.checkpoints.${field}`,
    evidence,
    `formula ${checkpoint.formulaIdentifier}`,
    formula,
    unit,
    scopes,
    checkpoint.blockHash,
  ].filter((part): part is string => part !== null).join(' · ')
}

function isInteger(value: string | null): value is string {
  return value !== null && /^-?\d+$/.test(value)
}

function isPositiveInteger(value: string | null): boolean {
  if (value === null || !/^\d+$/.test(value)) return false
  return BigInt(value) > 0n
}

function formatBps(value: string): string {
  const amount = BigInt(value)
  const negative = amount < 0n
  const absolute = negative ? -amount : amount
  const whole = absolute / 100n
  const fraction = (absolute % 100n).toString().padStart(2, '0')
  return `${negative ? '-' : ''}${whole.toString()}.${fraction}%`
}

function paginationStatus(
  stream: 'action' | 'checkpoint',
  complete: boolean | undefined,
): string {
  if (complete === true) return `${stream} pagination complete`
  if (complete === false) return `more ${stream} pages available`
  return `${stream} pagination status unavailable`
}

function exportHistoryCsv(
  tranche: string,
  actions: ProtocolAction[],
  checkpoints: TrancheHistoryCheckpoint[],
) {
  const blob = new Blob([buildTrancheHistoryCsv(actions, checkpoints)], { type: 'text/csv;charset=utf-8' })
  const url = URL.createObjectURL(blob)
  const link = document.createElement('a')
  link.href = url
  link.download = `plether-${tranche}-history.csv`
  link.click()
  URL.revokeObjectURL(url)
}
