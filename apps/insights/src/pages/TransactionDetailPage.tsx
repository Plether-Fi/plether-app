import { Link, useParams, useSearchParams } from 'react-router-dom'
import { useCurrentProtocolRelease, useProtocolTransaction } from '../api'
import { ActionTable, AvailabilityList, ObjectTable, PageTitle, ProtocolMeta, RawEvidence, Section } from '../components/Protocol'
import { ErrorState, LoadingState, Panel } from '../components/ui'
import { readRecord, readString } from '../utils/protocol'

export function TransactionDetailPage() {
  const { txHash = '' } = useParams()
  const [searchParams] = useSearchParams()
  const release = useCurrentProtocolRelease()
  const requestedRelease = searchParams.get('release')?.trim()
  const requestedReleaseId = requestedRelease === undefined || requestedRelease.length === 0
    ? 'current'
    : requestedRelease
  const resolvingCurrentRelease = requestedReleaseId === 'current'
  const releaseId = resolvingCurrentRelease
    ? release.data?.releaseId ?? ''
    : requestedReleaseId
  const query = useProtocolTransaction(releaseId, txHash)

  if ((resolvingCurrentRelease && release.isLoading) || query.isLoading) return <Panel><LoadingState rows={9} /></Panel>
  if (resolvingCurrentRelease && release.isError) return <ErrorState message={release.error.message} />
  if (query.isError) return <ErrorState title="Transaction not found" message={query.error.message} onRetry={() => void query.refetch()} />
  const data = query.data
  if (!data) return <ErrorState title="Transaction not found" />
  const chainTx = data.transaction.chainTransaction
  const explorerUrl = readString(readRecord(chainTx).explorerUrl)
  const stateImpact = readRecord(data.transaction.stateImpact)
  const analysis = readRecord(data.transaction.analysis)
  const accountImpacts = readRecordArray(stateImpact.accounts)
  const liquidationAnalyses = readRecordArray(analysis.liquidations)
  const marginAnalyses = readRecordArray(analysis.marginActions)
  const trancheAnalyses = readRecordArray(analysis.trancheActions)
  const impactAvailability = readAvailability(stateImpact.availability)
  const analysisAvailability = readAvailability(analysis.availability)
  const impactEvidence = {
    sourceBlocks: stateImpact.sourceBlocks ?? null,
    provenance: stateImpact.provenance ?? null,
    formula: stateImpact.formula ?? null,
    evidenceReferences: stateImpact.evidenceReferences ?? null,
  }
  const analysisEvidence = {
    provenance: analysis.provenance ?? null,
    formula: analysis.formula ?? null,
    evidenceReferences: analysis.evidenceReferences ?? null,
  }

  return (
    <div className="space-y-7">
      <PageTitle title="Canonical transaction" description="One chain transaction, available public receipt and calldata fields, every monitored raw log, and every protocol action projected from it. Missing RPC fields remain explicitly unavailable.">
        {explorerUrl ? <a href={explorerUrl} target="_blank" rel="noreferrer" className="border border-brand-border/40 px-4 py-2.5 text-sm font-semibold hover:border-brand-peach">View on explorer ↗</a> : null}
      </PageTitle>
      <ProtocolMeta data={data} />
      <AvailabilityList items={data.availability} />
      <Section title="Chain transaction" description={txHash}>
        <ObjectTable value={chainTx} evidence={data.evidence.transaction} />
        <RawEvidence value={chainTx} title="Raw transaction evidence" />
      </Section>
      <Section title={`Protocol actions · ${String(data.transaction.batchActionCount)}`} description="Batch execution is represented as several actions under this single canonical transaction.">
        <ActionTable actions={data.transaction.actions} releaseId={data.releaseId} />
      </Section>
      <Section title="State impact" description="Account, HousePool, and tranche state are compared at the source blocks shown below. Block-level deltas are not presented as transaction-exact when another protocol transaction can share the block.">
        <AvailabilityList items={impactAvailability} />
        <div className="grid gap-5 p-5 lg:grid-cols-2">
          <ImpactPanel title="HousePool" value={stateImpact.housePool} empty="HousePool state impact is unavailable for this transaction." />
          <ImpactPanel title="Senior tranche" value={stateImpact.senior} empty="Senior tranche state impact is unavailable for this transaction." />
          <ImpactPanel title="Junior tranche" value={stateImpact.junior} empty="Junior tranche state impact is unavailable for this transaction." />
          <ImpactPanel title="Source and attribution" value={impactEvidence} empty="State-impact provenance is unavailable." />
        </div>
        <div className="border-t border-brand-border/15">
          <div className="px-5 py-4">
            <h3 className="font-semibold">Trading accounts · {String(accountImpacts.length)}</h3>
            <p className="mt-1 text-xs text-content-tertiary">Every account-level before/after record attributed to this canonical transaction.</p>
          </div>
          {accountImpacts.length > 0 ? (
            <div className="divide-y divide-brand-border/15">
              {accountImpacts.map((impact, index) => (
                <div key={impactIdentity(impact, index)}>
                  <p className="px-5 pt-4 text-xs font-semibold uppercase tracking-wide text-brand-yellow">{impactLabel(impact, index)}</p>
                  <ObjectTable value={impact} evidence={impact.provenance ?? stateImpact.provenance} />
                </div>
              ))}
            </div>
          ) : (
            <ObjectTable value={null} empty="No account-level state impact is available for this transaction." />
          )}
        </div>
        <RawEvidence value={data.transaction.stateImpact} title="Complete state-impact evidence" />
      </Section>
      <Section title="Transaction analysis" description="Economics and action-specific interpretations are separated from the immutable receipt and log evidence. Their provenance and unavailable fields remain visible.">
        <AvailabilityList items={analysisAvailability} />
        <div className="grid gap-5 p-5 lg:grid-cols-2">
          <ImpactPanel title="Economics" value={analysis.economics} empty="Transaction economics are unavailable for this release." evidence={analysis.provenance} />
          <ImpactPanel title="Analysis provenance" value={analysisEvidence} empty="Analysis provenance is unavailable." />
        </div>
        <AnalysisCollection title="Liquidation analysis" items={liquidationAnalyses} empty="No liquidation analysis applies or is reconstructable for this transaction." />
        <AnalysisCollection title="Margin actions" items={marginAnalyses} empty="No margin-action analysis applies or is reconstructable for this transaction." />
        <AnalysisCollection title="Tranche actions" items={trancheAnalyses} empty="No tranche-action analysis applies or is reconstructable for this transaction." />
        <RawEvidence value={data.transaction.analysis} title="Complete derived transaction analysis" />
      </Section>
      <Section title={`Confirmed logs · ${String(data.transaction.events.length)}`} description="Raw topics/data are retained beside decoded fields so the projection can be independently checked.">
        <div className="divide-y divide-brand-border/15">
          {data.transaction.events.map((event, index) => (
            <div key={readString(event.logIndex) ?? String(index)}>
              <div className="flex flex-wrap items-center justify-between gap-2 px-5 py-4">
                <div><p className="font-semibold">{readString(event.eventName) ?? 'Unknown event'}</p><p className="mt-1 font-mono text-xs text-content-tertiary">{readString(event.contractAddress)}</p></div>
                <span className="text-xs text-content-tertiary">Log {readString(event.logIndex)}</span>
              </div>
              <RawEvidence value={event} title="Topics, data, and decoded fields" />
            </div>
          ))}
        </div>
      </Section>
      <Link to={`/transactions?release=${encodeURIComponent(data.releaseId)}`} className="inline-block text-sm font-semibold text-brand-peach hover:underline">← Back to activity</Link>
    </div>
  )
}

function ImpactPanel({
  title,
  value,
  empty,
  evidence,
}: {
  title: string
  value: unknown
  empty: string
  evidence?: unknown
}) {
  return (
    <div className="border border-brand-border/20">
      <h3 className="border-b border-brand-border/15 px-5 py-3 font-semibold">{title}</h3>
      <ObjectTable value={value} empty={empty} evidence={evidence} />
    </div>
  )
}

function AnalysisCollection({
  title,
  items,
  empty,
}: {
  title: string
  items: Record<string, unknown>[]
  empty: string
}) {
  return (
    <div className="border-t border-brand-border/15">
      <div className="px-5 py-4">
        <h3 className="font-semibold">{title} · {String(items.length)}</h3>
      </div>
      {items.length > 0 ? (
        <div className="divide-y divide-brand-border/15">
          {items.map((item, index) => (
            <div key={impactIdentity(item, index)}>
              <p className="px-5 pt-4 text-xs font-semibold uppercase tracking-wide text-brand-yellow">{`${title} ${String(index + 1)}`}</p>
              <ObjectTable value={item} evidence={item.provenance} />
            </div>
          ))}
        </div>
      ) : (
        <ObjectTable value={null} empty={empty} />
      )}
    </div>
  )
}

function readRecordArray(value: unknown): Record<string, unknown>[] {
  return Array.isArray(value) ? value.map(readRecord).filter((item) => Object.keys(item).length > 0) : []
}

function readAvailability(value: unknown): { field: string; reason: string }[] {
  if (!Array.isArray(value)) return []
  return value.flatMap((item) => {
    const record = readRecord(item)
    const field = readString(record.field)
    const reason = readString(record.reason)
    return field && reason ? [{ field, reason }] : []
  })
}

function impactIdentity(value: Record<string, unknown>, index: number): string {
  return readString(value.actionId)
    ?? readString(value.account)
    ?? readString(value.address)
    ?? readString(value.tranche)
    ?? String(index)
}

function impactLabel(value: Record<string, unknown>, index: number): string {
  return readString(value.account)
    ?? readString(value.address)
    ?? readString(value.tradingAccount)
    ?? `Account ${String(index + 1)}`
}
