import { Link, useParams, useSearchParams } from 'react-router-dom'
import { useCurrentProtocolRelease, useProtocolWallet } from '../api'
import type { ProtocolWalletActivity } from '../api'
import {
  AvailabilityList,
  EvidenceBadge,
  Metric,
  ObjectTable,
  Outcome,
  PageTitle,
  ProtocolMeta,
  RawEvidence,
  Section,
} from '../components/Protocol'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { shortAddress } from '../utils/format'
import { dedupeBy, formatTimestamp, humanize } from '../utils/protocol'
import {
  formatNativeWei,
  updaterTelemetryIsUnattributable,
  walletStatusTone,
} from '../utils/protocolWallets'

const WINDOWS = ['24h', '7d', '30d'] as const

export function ProtocolWalletDetailPage() {
  const { address = '' } = useParams()
  const [searchParams, setSearchParams] = useSearchParams()
  const release = useCurrentProtocolRelease()
  const requestedRelease = searchParams.get('release')?.trim()
  const requestedReleaseId = requestedRelease === undefined || requestedRelease.length === 0
    ? 'current'
    : requestedRelease
  const resolvingCurrentRelease = requestedReleaseId === 'current'
  const releaseId = resolvingCurrentRelease
    ? release.data?.releaseId ?? ''
    : requestedReleaseId
  const window = readWindow(searchParams.get('window'))
  const query = useProtocolWallet(releaseId, address, window)

  function selectWindow(nextWindow: string) {
    const next = new URLSearchParams(searchParams)
    next.set('window', nextWindow)
    setSearchParams(next)
  }

  if ((resolvingCurrentRelease && release.isLoading) || query.isLoading) {
    return <Panel><LoadingState rows={9} /></Panel>
  }
  if (resolvingCurrentRelease && release.isError) {
    return <ErrorState title="Release unavailable" message={release.error.message} />
  }
  if (query.isError) {
    return <ErrorState title="Operational wallet not found" message={query.error.message} onRetry={() => void query.refetch()} />
  }

  const pages = query.data?.pages ?? []
  const data = pages.at(0)
  if (!data) return <ErrorState title="Operational wallet not found" />
  const wallet = data.wallet
  const activity = dedupeBy(
    pages.flatMap((page) => page.wallet.activity),
    (item) => item.activityId,
  )
  const availability = dedupeBy(
    [
      ...pages.flatMap((page) => page.availability),
      ...pages.flatMap((page) => page.wallet.availability),
      ...activity.flatMap((item) => item.availability),
    ],
    (item) => `${item.field}:${item.reason}`,
  )
  const roles = wallet.roles.length > 0 ? wallet.roles.map(humanize).join(', ') : 'Role unavailable'
  const updaterActivityUnavailable = availability.some((item) =>
    updaterTelemetryIsUnattributable(item.field, item.reason),
  )
  const releaseQuery = data.releaseId || releaseId
  const transactionsQuery = new URLSearchParams({
    release: releaseQuery,
    address: wallet.address || address,
  })

  return (
    <div className="space-y-7">
      <PageTitle
        title="Operational wallet"
        description={`${wallet.address || address} · ${roles}`}
      >
        <div className="flex border border-brand-border/30" role="group" aria-label="Operational wallet activity window">
          {WINDOWS.map((item) => (
            <button
              aria-pressed={window === item}
              key={item}
              type="button"
              onClick={() => { selectWindow(item) }}
              className={`px-4 py-2 text-sm font-semibold ${window === item ? 'bg-brand-orange text-content-primary' : 'text-content-secondary hover:bg-white/5'}`}
            >
              {item}
            </button>
          ))}
        </div>
      </PageTitle>
      <ProtocolMeta data={data} />
      <AvailabilityList items={availability} />

      {updaterActivityUnavailable ? (
        <div className="border border-brand-yellow/35 bg-brand-yellow/10 px-5 py-4 text-sm text-brand-yellow" role="status">
          <p className="font-semibold">Oracle updater activity attribution unavailable</p>
          <p className="mt-1 leading-6 text-content-secondary">
            This release cannot publicly attribute updater transactions or their gross native spend. The observed activity below may therefore be incomplete for the oracle-updater role.
          </p>
        </div>
      ) : null}

      <section className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
        <Metric
          label="Native funding status"
          value={humanize(wallet.status)}
          tone={walletStatusTone(wallet.status)}
          hint="A conservative gross-spend diagnostic for this address, not proof of protocol liveness."
        />
        <Metric
          label="Native balance"
          value={formatNativeWei(wallet.nativeBalanceWei)}
          hint="Canonical balance at the confirmed block."
        />
        <Metric
          label="Estimated txs at gross spend"
          value={wallet.estimatedTransactionsAtObservedGrossSpend ?? 'Unavailable'}
          tone={transactionCapacityTone(wallet.status)}
          hint={`Median complete distinct-transaction gross spend ${formatNativeWei(wallet.medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei)}.`}
        />
        <Metric
          label="Last successful activity"
          value={formatTimestamp(wallet.lastActivityTimestamp)}
          hint={`${wallet.observedActionCount ?? 'Unavailable'} actions across ${wallet.observedTransactionCount ?? 'Unavailable'} distinct transactions in ${window}.`}
        />
      </section>

      <div className="grid gap-5 lg:grid-cols-2">
        <Section
          title="Observed gross native spend"
          description="The diagnostic adds gas cost and the full native value sent by each distinct successful operational transaction. Gas and transaction value remain visible separately; refunds are not netted without trace or contract telemetry."
        >
          <ObjectTable
            value={{
              observedGasCostWei: wallet.observedGasCostWei,
              observedGasCost: formatNativeWei(wallet.observedGasCostWei),
              observedTransactionNativeValueWei: wallet.observedTransactionNativeValueWei,
              observedTransactionNativeValue: formatNativeWei(wallet.observedTransactionNativeValueWei),
              observedActionCount: wallet.observedActionCount,
              observedDistinctTransactionCount: wallet.observedTransactionCount,
              medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei:
                wallet.medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei,
              medianObservedSuccessfulOperationalTransactionGrossNativeSpend:
                formatNativeWei(wallet.medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei),
            }}
            evidence={
              wallet.evidence.nativeCosts
              ?? wallet.evidence.costs
              ?? wallet.evidence.activity
              ?? wallet.evidence.level
            }
          />
        </Section>
        <Section
          title="Transaction-capacity calculation"
          description="This is a conservative diagnostic at observed gross spend—not net cost, profit, a time runway, or an onchain guarantee. Refunds and returned native value are not netted."
        >
          <ObjectTable
            value={{
              status: wallet.status,
              nativeBalanceWei: wallet.nativeBalanceWei,
              estimatedTransactionsAtObservedGrossSpend:
                wallet.estimatedTransactionsAtObservedGrossSpend,
              formula: wallet.runwayFormula,
            }}
            evidence={wallet.evidence.runway ?? wallet.evidence.level ?? 'derived'}
          />
        </Section>
      </div>

      <Section
        title="Role provenance"
        description="Only public release metadata and onchain dependency or role getters are used. Operator identities are never inferred from private worker configuration. A role, contract, or Safe address can rely on a separately funded relayer, so its own balance alone does not prove protocol liveness."
      >
        <ObjectTable
          value={{
            address: wallet.address,
            roles: wallet.roles,
            roleSources: wallet.roleSources,
          }}
          evidence={wallet.evidence.roles ?? wallet.evidence.roleSources ?? wallet.evidence.level}
        />
      </Section>

      <Section
        title="Successful onchain activity"
        description={`${String(activity.length)} unique actions loaded across ${String(pages.length)} confirmed page${pages.length === 1 ? '' : 's'}. Reverted attempts are not inferred from successful logs.`}
      >
        <WalletActivityTable activity={activity} releaseId={releaseQuery} />
        {query.hasNextPage ? (
          <div className="border-t border-brand-border/15 p-4 text-center">
            <button
              type="button"
              onClick={() => void query.fetchNextPage()}
              disabled={query.isFetchingNextPage}
              className="border border-brand-border/40 px-5 py-2.5 text-sm font-semibold hover:border-brand-orange disabled:cursor-wait disabled:opacity-60"
            >
              {query.isFetchingNextPage ? 'Loading wallet activity…' : 'Load more wallet activity'}
            </button>
          </div>
        ) : null}
        <RawEvidence
          title="Complete wallet and activity evidence"
          value={{
            releaseId: data.releaseId,
            confirmedBlock: data.confirmedBlock,
            evidence: data.evidence,
            availability,
            wallet: wallet.raw,
            activity: activity.map((item) => item.raw),
          }}
        />
      </Section>

      <div className="flex flex-wrap gap-x-5 gap-y-2 text-sm font-semibold">
        <Link
          to={`/protocol-wallets?window=${encodeURIComponent(window)}&release=${encodeURIComponent(releaseQuery)}`}
          className="text-brand-peach hover:underline"
        >
          ← All operational wallets
        </Link>
        <Link
          to={`/transactions?${transactionsQuery.toString()}`}
          className="text-brand-peach hover:underline"
        >
          All protocol activity for {shortAddress(wallet.address || address)} →
        </Link>
      </div>
    </div>
  )
}

function WalletActivityTable({
  activity,
  releaseId,
}: {
  activity: ProtocolWalletActivity[]
  releaseId: string
}) {
  if (activity.length === 0) {
    return <EmptyState title="No successful activity" message="No confirmed successful protocol action is indexed for this wallet in the selected window." />
  }
  return (
    <div className="overflow-x-auto">
      <table className="w-full min-w-[940px] border-collapse text-left text-sm">
        <thead>
          <tr className="border-b border-brand-border/20 text-xs uppercase tracking-wide text-content-tertiary">
            <th className="px-5 py-3">Time</th>
            <th className="px-5 py-3">Action</th>
            <th className="px-5 py-3">Outcome</th>
            <th className="px-5 py-3 text-right">Gas cost</th>
            <th className="px-5 py-3 text-right">Native value</th>
            <th className="px-5 py-3">Transaction</th>
            <th className="px-5 py-3">Evidence</th>
          </tr>
        </thead>
        <tbody>
          {activity.map((item) => (
            <tr key={item.activityId} className="border-b border-brand-border/10 align-top hover:bg-white/[0.025]">
              <td className="whitespace-nowrap px-5 py-4 text-xs text-content-secondary">{formatTimestamp(item.timestamp)}</td>
              <td className="px-5 py-4 font-semibold">{humanize(item.actionType)}</td>
              <td className="px-5 py-4"><Outcome outcome={item.outcome} /></td>
              <td className="px-5 py-4 text-right tabular-nums">{formatNativeWei(item.gasCostWei)}</td>
              <td className="px-5 py-4 text-right tabular-nums">{formatNativeWei(item.nativeValueWei)}</td>
              <td className="px-5 py-4">
                {item.transactionHash ? (
                  <Link
                    className="font-mono text-xs text-brand-peach hover:underline"
                    to={`/transactions/${encodeURIComponent(item.transactionHash)}?release=${encodeURIComponent(releaseId)}`}
                  >
                    {shortAddress(item.transactionHash)}
                  </Link>
                ) : <span className="text-content-tertiary">Unavailable</span>}
              </td>
              <td className="px-5 py-4">
                <EvidenceBadge level={item.evidence} />
                {item.availability.length > 0 ? (
                  <div className="mt-1 text-xs text-brand-yellow">{item.availability.length} incomplete field{item.availability.length === 1 ? '' : 's'}</div>
                ) : null}
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

function readWindow(value: string | null): string {
  return WINDOWS.includes(value as typeof WINDOWS[number]) ? value as typeof WINDOWS[number] : '7d'
}

function transactionCapacityTone(status: string): 'positive' | 'warning' | 'critical' | 'default' {
  return walletStatusTone(status)
}
