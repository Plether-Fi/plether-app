import { Link, useParams, useSearchParams } from 'react-router-dom'
import { useCurrentProtocolRelease, useKeeper } from '../api'
import { ActionTable, AvailabilityList, ObjectTable, PageTitle, ProtocolMeta, Section } from '../components/Protocol'
import { ErrorState, LoadingState, Panel } from '../components/ui'
import { dedupeBy, readString } from '../utils/protocol'

const WINDOWS = ['24h', '7d', '30d']

export function KeeperDetailPage() {
  const { address = '' } = useParams()
  const [searchParams, setSearchParams] = useSearchParams()
  const window = WINDOWS.includes(searchParams.get('window') ?? '') ? searchParams.get('window') ?? '7d' : '7d'
  const release = useCurrentProtocolRelease()
  const requestedRelease = normalizedSearchParam(searchParams.get('release'))
  const resolvingCurrentRelease = requestedRelease === null || requestedRelease === 'current'
  const releaseId = resolvingCurrentRelease
    ? release.data?.releaseId ?? ''
    : requestedRelease
  const query = useKeeper(releaseId, address, window)

  if ((resolvingCurrentRelease && release.isLoading) || query.isLoading) return <Panel><LoadingState rows={9} /></Panel>
  if (resolvingCurrentRelease && release.isError) return <ErrorState title="Release manifest unavailable" message={release.error.message} onRetry={() => void release.refetch()} />
  if (query.isError) return <ErrorState title="Keeper activity unavailable" message={query.error.message} onRetry={() => void query.refetch()} />
  const pages = query.data?.pages ?? []
  const data = pages.at(0)
  if (!data) return <ErrorState title="Keeper activity unavailable" />
  const summary = keeperSummary(data.keeper.summary)
  const actions = dedupeBy(
    pages.flatMap((page) => page.keeper.actions),
    (action) => action.actionId,
  )
  const availability = dedupeBy(
    pages.flatMap((page) => page.availability),
    (item) => `${item.field}:${item.reason}`,
  )

  return (
    <div className="space-y-7">
      <PageTitle title="Keeper address" description={address}>
        <div className="flex border border-brand-border/30" role="group" aria-label="Keeper activity window">{WINDOWS.map((item) => <button aria-pressed={window === item} key={item} type="button" onClick={() => { setKeeperWindow(searchParams, setSearchParams, item); }} className={`px-4 py-2 text-sm font-semibold ${window === item ? 'bg-brand-orange' : 'text-content-secondary hover:bg-white/5'}`}>{item}</button>)}</div>
      </PageTitle>
      <ProtocolMeta data={data} />
      <p className="text-xs text-content-tertiary">All loaded keeper actions remain anchored to the confirmed block shown above.</p>
      <AvailabilityList items={availability} />
      <Section title="Window summary" description="USDC rewards are limited to observed terminal liquidation bounties. Native gas cost and transaction value are sums of available exact receipt and transaction values and may be partial; the Pyth component, complete reward total, conversion, and net result remain unavailable."><ObjectTable value={summary} evidence={data.evidence.summary} /></Section>
      <Section title="Successful onchain actions" description={`${String(actions.length)} unique actions loaded across ${String(pages.length)} anchored page${pages.length === 1 ? '' : 's'}. Confirmed executions, cleanups, and liquidations emitted by the current release appear here. Mark-update and LP-maintenance categories remain unavailable until the release emits attributable telemetry.`}>
        <ActionTable actions={actions} releaseId={data.releaseId} showKeeper={false} />
        {query.hasNextPage ? (
          <div className="border-t border-brand-border/15 p-4 text-center">
            <button
              type="button"
              onClick={() => void query.fetchNextPage()}
              disabled={query.isFetchingNextPage}
              className="border border-brand-border/40 px-5 py-2.5 text-sm font-semibold hover:border-brand-orange disabled:cursor-wait disabled:opacity-60"
            >
              {query.isFetchingNextPage ? 'Loading keeper actions…' : 'Load more keeper actions'}
            </button>
          </div>
        ) : null}
      </Section>
      <Link to={`/keepers?${keeperSearch(requestedRelease, window)}`} className="inline-block text-sm font-semibold text-brand-peach hover:underline">← All keepers</Link>
    </div>
  )
}

function keeperSummary(value: Record<string, unknown>): Record<string, unknown> {
  const summary = { ...value }
  const observedBounties =
    readString(summary.observedLiquidationRewardsUsdc)
    ?? readString(summary.grossRewardsUsdc)
  delete summary.observedLiquidationRewardsUsdc
  delete summary.grossRewardsUsdc
  if (observedBounties !== null) {
    summary.observedLiquidationBountiesUsdc = observedBounties
  }
  return summary
}

function normalizedSearchParam(value: string | null): string | null {
  const normalized = value?.trim() ?? ''
  return normalized.length > 0 ? normalized : null
}

function keeperSearch(release: string | null, window: string): string {
  const params = new URLSearchParams()
  if (release !== null) params.set('release', release)
  params.set('window', window)
  return params.toString()
}

function setKeeperWindow(
  current: URLSearchParams,
  setSearchParams: ReturnType<typeof useSearchParams>[1],
  window: string,
) {
  const next = new URLSearchParams(current)
  next.set('window', window)
  setSearchParams(next)
}
