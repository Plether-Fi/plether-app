import { Link, useParams } from 'react-router-dom'
import type { ProtocolAction } from '../api'
import { useCurrentProtocolRelease, useProtocolOrder } from '../api'
import { ActionTable, AvailabilityList, EvidenceBadge, ObjectTable, PageTitle, ProtocolMeta, RawEvidence, Section } from '../components/Protocol'
import { ErrorState, LoadingState, Panel } from '../components/ui'
import { readRecord, readString } from '../utils/protocol'

export function OrderDetailPage() {
  const params = useParams()
  const currentRelease = useCurrentProtocolRelease()
  const releaseId = params.releaseId === 'current' ? currentRelease.data?.releaseId ?? '' : params.releaseId ?? ''
  const orderId = params.orderId ?? ''
  const query = useProtocolOrder(releaseId, orderId)
  const resolvingCurrentRelease = params.releaseId === 'current'

  if ((resolvingCurrentRelease && currentRelease.isLoading) || query.isLoading) return <Panel><LoadingState rows={10} /></Panel>
  if (resolvingCurrentRelease && currentRelease.isError) return <ErrorState title="Release manifest unavailable" message={currentRelease.error.message} onRetry={() => void currentRelease.refetch()} />
  if (query.isError) return <ErrorState title="Order lifecycle unavailable" message={query.error.message} onRetry={() => void query.refetch()} />
  const data = query.data
  if (!data) return <ErrorState title="Order lifecycle unavailable" />

  const order = readRecord(data.order)
  const lifecycle = readRecord(order.lifecycle)
  const stateImpact = readRecord(order.stateImpact)
  const positionChange = readRecord(order.positionChange)
  const keeperEconomics = readRecord(order.keeperEconomics)
  const transactions = readRecord(order.transactions)
  const actions = Array.isArray(order.actions) ? order.actions as ProtocolAction[] : []

  return (
    <div className="space-y-7">
      <PageTitle title={`Order #${orderId}`} description="The logical lifecycle connecting commitment intent, reveal eligibility, terminal processing, before/after account state, HousePool impact, keeper identity, economics, and raw evidence." />
      <ProtocolMeta data={data} />
      <AvailabilityList items={data.availability} />

      <div className="grid gap-5 lg:grid-cols-3">
        {(['commitment', 'reveal', 'terminal'] as const).map((stage) => (
          <Section key={stage} title={stage[0].toUpperCase() + stage.slice(1)}>
            <ObjectTable value={lifecycle[stage]} />
          </Section>
        ))}
      </div>

      <div className="grid gap-5 lg:grid-cols-2">
        <Section title="Position before / after" description="Historical state at terminal block − 1 and terminal block. This is labelled block-level when the block contains multiple protocol transactions.">
          <ObjectTable value={readRecord(stateImpact.position).before} empty="Archive position state before the terminal block is unavailable." />
          <div className="border-t border-brand-border/20 px-5 py-2 text-xs font-semibold uppercase tracking-wide text-brand-yellow">After terminal block</div>
          <ObjectTable value={readRecord(stateImpact.position).after} empty="Archive position state after the terminal block is unavailable." />
        </Section>
        <Section title="HousePool before / after" description="Accounting delta at block granularity; never presented as transaction-exact when attribution is ambiguous.">
          <ObjectTable value={readRecord(stateImpact.housePool).before} empty="Archive pool state before the terminal block is unavailable." />
          <div className="border-t border-brand-border/20 px-5 py-2 text-xs font-semibold uppercase tracking-wide text-brand-yellow">After terminal block</div>
          <ObjectTable value={readRecord(stateImpact.housePool).after} empty="Archive pool state after the terminal block is unavailable." />
        </Section>
      </div>

      <div className="grid gap-5 lg:grid-cols-2">
        <Section title="Observed position change" description="Terminal position activity projected from the confirmed protocol log. Per-field evidence and units remain attached to the values.">
          <div className="flex items-center justify-between gap-3 border-b border-brand-border/15 px-5 py-3 text-xs text-content-tertiary">
            <span>Projection provenance</span>
            <EvidenceBadge level={data.evidence.positionChange} />
          </div>
          <ObjectTable
            value={positionChange}
            empty="No position-changing terminal activity was observed for this order."
          />
        </Section>
        <Section title="Keeper transaction economics" description="Exact receipt gas cost and transaction-native value are kept separate. Pyth fee isolation and historical native-to-USDC profit remain unavailable when not sourced.">
          <ObjectTable
            value={keeperEconomics}
            empty="The terminal keeper transaction or its receipt economics are unavailable."
          />
        </Section>
        <Section title="Economics" description="Exact event fields are shown; missing settlement components remain explicitly unavailable."><ObjectTable value={order.economics} /></Section>
        <Section title="Liquidation analysis" description="Populated only for liquidation terminal paths."><ObjectTable value={order.liquidation} empty="This order did not terminate through liquidation." /></Section>
      </div>

      <Section title="Lifecycle actions"><ActionTable actions={actions} releaseId={data.releaseId} /></Section>
      <Section title="Canonical transactions">
        <div className="grid gap-5 p-5 lg:grid-cols-2">
          {(['commitment', 'terminal'] as const).map((kind) => {
            const tx = readRecord(transactions[kind])
            const hash = readString(tx.transactionHash)
            return (
              <div key={kind} className="border border-brand-border/20 p-4">
                <h3 className="font-semibold capitalize">{kind}</h3>
                {hash ? <Link className="mt-2 block break-all font-mono text-xs text-brand-peach hover:underline" to={`/transactions/${encodeURIComponent(hash)}?release=${encodeURIComponent(data.releaseId)}`}>{hash}</Link> : <p className="mt-2 text-sm text-content-tertiary">Unavailable</p>}
              </div>
            )
          })}
        </div>
        <RawEvidence value={data.order} title="Complete order evidence payload" />
      </Section>
    </div>
  )
}
