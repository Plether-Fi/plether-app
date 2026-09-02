import { Link } from 'react-router-dom'
import { useCurrentProtocolRelease, useProtocolOverview } from '../api'
import { AvailabilityList, Metric, ObjectTable, PageTitle, ProtocolMeta, Section } from '../components/Protocol'
import { ErrorState, LoadingState, Panel } from '../components/ui'
import { shortAddress } from '../utils/format'
import { humanize, readRecord, readString } from '../utils/protocol'
import { formatNativeWei } from '../utils/protocolWallets'

export function OverviewPage() {
  const release = useCurrentProtocolRelease()
  const releaseId = release.data?.releaseId ?? ''
  const overview = useProtocolOverview(releaseId)

  if (release.isLoading || overview.isLoading) return <OverviewLoading />
  if (release.isError) return <ErrorState title="Release manifest unavailable" message={release.error.message} onRetry={() => void release.refetch()} />
  if (overview.isError) return <ErrorState title="Protocol overview unavailable" message={overview.error.message} onRetry={() => void overview.refetch()} />
  const data = overview.data
  if (!data) return <ErrorState title="Protocol overview unavailable" />

  const counts = data.overview.counts
  const pool = readRecord(data.overview.housePool)
  const status = readRecord(data.overview.protocolStatus)

  return (
    <div className="space-y-7">
      <PageTitle
        title="Protocol overview"
        description="A release-aware view of protocol liveness, solvency, market state, keeper participation, and deterministic anomaly indicators."
      >
        <Link to="/transactions" className="border border-brand-orange bg-brand-orange px-4 py-2.5 text-sm font-semibold hover:bg-brand-peach hover:text-app-bg">
          Explore activity
        </Link>
      </PageTitle>
      <ProtocolMeta data={data} />
      <AvailabilityList items={data.availability} />

      <section aria-label="Protocol summary" className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
        <Metric
          label="Indexed actions · 24h"
          value={readString(counts.indexedActions24h) ?? '—'}
          hint={`${readString(counts.liquidations24h) ?? '—'} liquidations · ${readString(counts.nonSuccessOutcomes24h) ?? '—'} non-success outcomes in the same window.`}
        />
        <Metric label="Active keepers · 24h" value={readString(counts.activeKeepers24h) ?? '—'} hint="Successful permissionless action senders." />
        <Metric label="Pending orders" value={readString(counts.pendingOrders) ?? '—'} tone={Number(counts.ordersOlderThanMaxOrderAge) > 0 ? 'warning' : 'default'} hint={`${readString(counts.ordersOlderThanMaxOrderAge) ?? 'unavailable'} older than maxOrderAge.`} />
        <Metric label="Indexer lag" value={`${data.overview.indexerLagBlocks} blocks`} tone={Number(data.overview.indexerLagBlocks) > 20 ? 'warning' : 'positive'} hint="Confirmed chain head minus the release-scoped indexer cursor." />
      </section>

      <Section title="Anomaly indicators" description="Deterministic checks only. An empty list is not a security guarantee.">
        {data.overview.anomalies.length === 0 ? (
          <div className={`flex items-center gap-3 px-5 py-5 text-sm ${data.overview.anomalyEvaluation === 'complete' ? 'text-positive' : 'text-brand-yellow'}`}>
            <span className={`h-2.5 w-2.5 rounded-full ${data.overview.anomalyEvaluation === 'complete' ? 'bg-positive' : 'bg-brand-yellow'}`} />
            {data.overview.anomalyEvaluation === 'complete'
              ? 'No configured anomaly threshold is currently breached.'
              : 'No anomaly is currently proven, but one or more required checks are unavailable. Review the availability notices above.'}
          </div>
        ) : (
          <ul className="divide-y divide-brand-border/10">
            {data.overview.anomalies.map((item, index) => (
              <li key={anomalyKey(item, index)} className="flex gap-4 px-5 py-4">
                <span className={`mt-1.5 h-2.5 w-2.5 shrink-0 rounded-full ${item.severity === 'critical' ? 'bg-brand-orange' : item.severity === 'warning' ? 'bg-brand-yellow' : 'bg-brand-peach'}`} />
                <div className="min-w-0">
                  <p className="font-semibold">{item.message}</p>
                  <p className="mt-1 text-xs text-content-tertiary">{item.code}</p>
                  {item.code.startsWith('operational_wallet_') ? (
                    <OperationalWalletAnomalyDetails
                      value={item.details}
                      releaseId={data.releaseId}
                    />
                  ) : null}
                </div>
              </li>
            ))}
          </ul>
        )}
      </Section>

      <div className="grid gap-5 lg:grid-cols-2">
        <Section title="HousePool accounting" description="Confirmed-block state from the release HousePool.">
          <ObjectTable value={pool} evidence={data.evidence.overview} />
          <div className="border-t border-brand-border/15 px-5 py-4"><Link className="text-sm font-semibold text-brand-peach hover:underline" to="/house-pool">Open complete HousePool view →</Link></div>
        </Section>
        <Section title="Market and oracle state" description="Pause, trading, withdrawal, FAD, and oracle state at the same confirmed block.">
          <ObjectTable value={status} evidence={data.evidence.overview} />
          <div className="border-t border-brand-border/15 px-5 py-4"><Link className="text-sm font-semibold text-brand-peach hover:underline" to="/parameters">Inspect risk parameters →</Link></div>
        </Section>
      </div>
    </div>
  )
}

function OverviewLoading() {
  return <div className="space-y-7"><div className="skeleton h-36" /><div className="grid gap-3 sm:grid-cols-4">{Array.from({ length: 4 }, (_, index) => <div key={index} className="skeleton h-28" />)}</div><Panel><LoadingState rows={5} /></Panel></div>
}

function OperationalWalletAnomalyDetails({
  value,
  releaseId,
}: {
  value: unknown
  releaseId: string
}) {
  const details = readRecord(value)
  const address = readString(details.address)
  const role = readString(details.role)
  const nativeBalanceWei = readString(details.nativeBalanceWei)
  const estimatedTransactions =
    readString(details.estimatedTransactionsAtObservedGrossSpend)
    ?? readString(details.estimatedTransactionsRemaining)
    ?? readString(details.estimatedActionsRemaining)
  const validAddress = address !== null && /^0x[a-fA-F0-9]{40}$/.test(address)

  return (
    <div className="mt-2 flex flex-wrap items-center gap-x-3 gap-y-1 text-xs text-content-secondary">
      {validAddress ? (
        <Link
          to={`/protocol-wallets/${encodeURIComponent(address)}?release=${encodeURIComponent(releaseId)}`}
          className="font-mono font-semibold text-brand-peach hover:underline"
          aria-label={`Open operational wallet ${address}`}
        >
          {shortAddress(address)}
        </Link>
      ) : (
        <span className="font-mono text-content-tertiary">{address ?? 'Wallet unavailable'}</span>
      )}
      <span>{role === null ? 'Role unavailable' : humanize(role)}</span>
      <span title={nativeBalanceWei === null ? undefined : `${nativeBalanceWei} wei`}>
        Balance {formatNativeWei(nativeBalanceWei)}
      </span>
      <span>Est. txs at gross spend {estimatedTransactions ?? 'Unavailable'}</span>
    </div>
  )
}

function anomalyKey(
  anomaly: { code: string; details: Record<string, unknown> | null },
  index: number,
): string {
  const address = readString(readRecord(anomaly.details).address)
  return `${anomaly.code}:${address ?? String(index)}`
}
