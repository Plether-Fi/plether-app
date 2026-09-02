import { Link, useSearchParams } from 'react-router-dom'
import { useCurrentProtocolRelease, useKeepers } from '../api'
import { AccessibleDonutChart } from '../components/Charts'
import { AvailabilityList, Metric, PageTitle, ProtocolMeta, Section } from '../components/Protocol'
import { ErrorState, LoadingState, Panel } from '../components/ui'
import { formatUsdc, shortAddress } from '../utils/format'
import { dedupeBy, readRecord, readString } from '../utils/protocol'

const WINDOWS = ['24h', '7d', '30d']

export function KeepersPage() {
  const release = useCurrentProtocolRelease()
  const [searchParams, setSearchParams] = useSearchParams()
  const requestedRelease = normalizedSearchParam(searchParams.get('release'))
  const resolvingCurrentRelease = requestedRelease === null || requestedRelease === 'current'
  const releaseId = resolvingCurrentRelease
    ? release.data?.releaseId ?? ''
    : requestedRelease
  const window = WINDOWS.includes(searchParams.get('window') ?? '') ? searchParams.get('window') ?? '7d' : '7d'
  const query = useKeepers(releaseId, window)

  if ((resolvingCurrentRelease && release.isLoading) || query.isLoading) return <Panel><LoadingState rows={9} /></Panel>
  if (resolvingCurrentRelease && release.isError) return <ErrorState title="Release manifest unavailable" message={release.error.message} onRetry={() => void release.refetch()} />
  if (query.isError) return <ErrorState title="Keeper data unavailable" message={query.error.message} onRetry={() => void query.refetch()} />
  const pages = query.data?.pages ?? []
  const data = pages.at(0)
  if (!data) return <ErrorState title="Keeper data unavailable" />
  const keepers = data.keepers
  const keeperRows = dedupeBy(
    pages.flatMap((page) => page.keepers.keepers),
    (row) => (readString(readRecord(row).address) ?? JSON.stringify(row)).toLowerCase(),
  )
  const availability = dedupeBy(
    pages.flatMap((page) => page.availability),
    (item) => `${item.field}:${item.reason}`,
  )
  const latency = keepers.latencySeconds
  const nativeCosts = keepers.nativeGasAndPythCosts
  const observedLiquidationRewardsUsdc =
    keepers.observedLiquidationRewardsUsdc
    ?? keepers.grossRewardsUsdc
  const observedRewardSlices =
    keepers.observedRewardConcentration?.slices
    ?? keepers.rewardConcentration
    ?? []
  const topOneShareBps =
    keepers.observedRewardConcentration?.topOneShareBps
    ?? keepers.topOneRewardShareBps
    ?? null
  const topThreeShareBps =
    keepers.observedRewardConcentration?.topThreeShareBps
    ?? keepers.topThreeRewardShareBps
    ?? null
  const slices = observedRewardSlices.map((rawSlice) => {
    const slice = readRecord(rawSlice)
    const address = readString(slice.address) ?? 'Unknown'
    const amount =
      readString(slice.observedLiquidationRewardsUsdc)
      ?? readString(slice.grossRewardsUsdc)
      ?? '0'
    return {
      label: address === 'Other' ? 'Other' : shortAddress(address),
      value: toChartValue(amount),
      displayValue: formatUsdc(amount),
    }
  })

  return (
    <div className="space-y-7">
      <PageTitle title="Keeper transparency" description={keepers.definition}>
        <div className="flex border border-brand-border/30" role="group" aria-label="Keeper activity window">
          {WINDOWS.map((item) => <button aria-pressed={window === item} key={item} type="button" onClick={() => { setKeeperWindow(searchParams, setSearchParams, item); }} className={`px-4 py-2 text-sm font-semibold ${window === item ? 'bg-brand-orange text-content-primary' : 'text-content-secondary hover:bg-white/5'}`}>{item}</button>)}
        </div>
      </PageTitle>
      <ProtocolMeta data={data} />
      <p className="text-xs text-content-tertiary">All loaded keeper rows remain anchored to the confirmed block and window bounds shown on the first page.</p>
      <AvailabilityList items={availability} />
      <section className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
        <Metric label="Active keepers" value={keepers.activeKeeperCount} hint="Successful onchain action senders." />
        <Metric label="Actions" value={keepers.actionCount} hint={`${keepers.backlogProcessed} orders executed or cleaned.`} />
        <Metric label="Median terminal latency" value={seconds(latency.commitToTerminalMedian)} hint={`p90 ${seconds(latency.commitToTerminalP90)} · p99 ${seconds(latency.commitToTerminalP99)}`} />
        <Metric
          label="Observed liquidation bounties"
          value={formatUsdc(observedLiquidationRewardsUsdc)}
          hint="Terminal liquidation bounty events only. Native gas and transaction value are reported separately; execution/cleanup rewards and a net result remain unavailable."
        />
        <Metric label="Native gas cost" value={formatNativeWei(nativeCosts.gasCostWei)} hint={`${nativeCosts.missingGasReceiptCount} keeper transactions have incomplete receipt cost data.`} />
        <Metric label="Transaction-native value" value={formatNativeWei(nativeCosts.transactionNativeValueWei)} hint={`Sum of available exact transaction values; ${nativeCosts.missingNativeValueCount} keeper transactions are missing that value. The Pyth component is not isolated.`} />
      </section>
      <div className="grid gap-5 lg:grid-cols-[1.1fr_.9fr]">
        <Section title="Liquidation-bounty concentration" description={`Top one ${formatBps(topOneShareBps)} · top three ${formatBps(topThreeShareBps)} of observed liquidation bounties. This is not a view of total keeper earnings.`}>
          <div className="p-5">
            <AccessibleDonutChart
              title={`Keeper liquidation bounties · ${window}`}
              slices={slices}
              valueLabel="Observed bounties"
              description={`${String(slices.length)} recipients shown as shares of observed terminal liquidation bounties denominated in USDC.`}
              emptyTitle="No observed liquidation bounties"
              emptyMessage="No terminal liquidation bounty event was indexed in this window. Other keeper reward categories are unavailable for this release."
            />
          </div>
        </Section>
        <Section title="Action mix"><div className="grid gap-3 p-5 sm:grid-cols-3 lg:grid-cols-1">{Object.entries(keepers.actionMix).map(([label, value]) => <Metric key={label} label={label} value={value} />)}</div></Section>
      </div>
      <Section title="Keeper addresses" description={`${String(keeperRows.length)} unique keeper addresses loaded across ${String(pages.length)} anchored page${pages.length === 1 ? '' : 's'}. Only confirmed successful permissionless actions are counted. Amounts are observed terminal liquidation bounties, not total rewards.`}>
        <div className="overflow-x-auto">
          <table className="w-full min-w-[720px] text-left text-sm">
            <thead><tr className="border-b border-brand-border/20 text-xs uppercase tracking-wide text-content-tertiary"><th className="px-5 py-3">Keeper</th><th className="px-5 py-3">Actions</th><th className="px-5 py-3">Executions</th><th className="px-5 py-3">Cleanups</th><th className="px-5 py-3">Liquidations</th><th className="px-5 py-3 text-right">Observed liquidation bounties</th><th className="px-5 py-3 text-right">Native gas</th></tr></thead>
            <tbody>{keeperRows.map((row, index) => {
              const keeper = readRecord(row)
              const address = readString(keeper.address) ?? `unknown-${String(index)}`
              const observedBounties =
                readString(keeper.observedLiquidationRewardsUsdc)
                ?? readString(keeper.grossRewardsUsdc)
              const keeperNativeCosts = readRecord(keeper.nativeCosts)
              return <tr key={address} className="border-b border-brand-border/10"><td className="px-5 py-4"><Link to={`/keepers/${encodeURIComponent(address)}?${keeperSearch(requestedRelease, window)}`} className="font-mono text-brand-peach hover:underline">{shortAddress(address)}</Link></td><td className="px-5 py-4">{readString(keeper.actionCount)}</td><td className="px-5 py-4">{readString(keeper.executions)}</td><td className="px-5 py-4">{readString(keeper.cleanups)}</td><td className="px-5 py-4">{readString(keeper.liquidations)}</td><td className="px-5 py-4 text-right">{formatUsdc(observedBounties)}</td><td className="px-5 py-4 text-right">{formatNativeWei(readString(keeperNativeCosts.gasCostWei))}</td></tr>
            })}</tbody>
          </table>
        </div>
        {query.hasNextPage ? (
          <div className="border-t border-brand-border/15 p-4 text-center">
            <button
              type="button"
              onClick={() => void query.fetchNextPage()}
              disabled={query.isFetchingNextPage}
              className="border border-brand-border/40 px-5 py-2.5 text-sm font-semibold hover:border-brand-orange disabled:cursor-wait disabled:opacity-60"
            >
              {query.isFetchingNextPage ? 'Loading keeper addresses…' : 'Load more keeper addresses'}
            </button>
          </div>
        ) : null}
      </Section>
    </div>
  )
}

function seconds(value: string | null): string {
  return value === null ? 'Unavailable' : `${value}s`
}

function formatBps(value: string | null): string {
  return value === null ? 'unavailable' : `${(Number(value) / 100).toFixed(2)}%`
}

function toChartValue(value: string): number {
  const parsed = Number(value)
  return Number.isFinite(parsed) ? parsed : 0
}

function formatNativeWei(value: string | null): string {
  if (value === null || !/^\d+$/.test(value)) return 'Unavailable'
  const wei = BigInt(value)
  const scale = 10n ** 18n
  const whole = wei / scale
  const fraction = ((wei % scale) * 1_000_000n / scale).toString().padStart(6, '0')
  return `${whole.toString()}.${fraction} ETH`
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
