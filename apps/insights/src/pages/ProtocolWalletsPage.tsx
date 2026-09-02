import { Link, useSearchParams } from 'react-router-dom'
import { useCurrentProtocolRelease, useProtocolWallets } from '../api'
import type { ProtocolWalletSummary } from '../api'
import {
  AvailabilityList,
  EvidenceBadge,
  Metric,
  PageTitle,
  ProtocolMeta,
  RawEvidence,
  Section,
} from '../components/Protocol'
import { ErrorState, LoadingState, Panel } from '../components/ui'
import { shortAddress } from '../utils/format'
import { dedupeBy, humanize, readString } from '../utils/protocol'
import {
  formatNativeWei,
  isOracleUpdaterRole,
  updaterTelemetryIsUnattributable,
  walletStatusTone,
} from '../utils/protocolWallets'

const WINDOWS = ['24h', '7d', '30d'] as const

export function ProtocolWalletsPage() {
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
  const query = useProtocolWallets(releaseId, window)

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
    return <ErrorState title="Operational wallet data unavailable" message={query.error.message} onRetry={() => void query.refetch()} />
  }

  const pages = query.data?.pages ?? []
  const data = pages.at(0)
  if (!data) return <ErrorState title="Operational wallet data unavailable" />
  const rows = dedupeBy(
    pages.flatMap((page) => page.wallets.items),
    (wallet) => wallet.address.toLowerCase(),
  )
  const availability = dedupeBy(
    [
      ...pages.flatMap((page) => page.availability),
      ...rows.flatMap((wallet) => wallet.availability),
    ],
    (item) => `${item.field}:${item.reason}`,
  )
  const identifiedOracleUpdater = rows.some((wallet) =>
    wallet.roles.some(isOracleUpdaterRole),
  )
  const oracleUpdaterPublished =
    identifiedOracleUpdater
    || data.wallets.oracleUpdaterIdentityAvailable === true
  const oracleUpdaterUnavailable =
    !oracleUpdaterPublished
  const updaterActivityUnavailable = availability.some((item) =>
    updaterTelemetryIsUnattributable(item.field, item.reason),
  ) || (
    oracleUpdaterPublished
    && data.wallets.oracleUpdaterActivityAttributable === false
  )
  const atRiskWallets = rows.filter((wallet) =>
    ['critical', 'depleted', 'warning'].includes(wallet.status.toLowerCase()),
  )
  const trackedWalletCount = data.wallets.totalTrackedWalletCount
  const atRiskWalletCount = data.wallets.totalAtRiskWalletCount
  const releaseQuery = encodeURIComponent(data.releaseId || releaseId)

  return (
    <div className="space-y-7">
      <PageTitle
        title="Operational wallets"
        description="Native-token balances and conservative observed gross-spend diagnostics for public operational role and sender addresses in this release."
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

      {oracleUpdaterUnavailable ? (
        <div className="border border-brand-yellow/35 bg-brand-yellow/10 px-5 py-4 text-sm text-brand-yellow" role="status">
          <p className="font-semibold">Oracle updater wallet identity unavailable</p>
          <p className="mt-1 leading-6 text-content-secondary">
            The current release does not publish a canonical oracle-updater address. Its ETH balance and conservative transaction-capacity diagnostic therefore cannot be monitored without guessing an operator identity.
          </p>
        </div>
      ) : null}

      {updaterActivityUnavailable ? (
        <div className="border border-brand-yellow/35 bg-brand-yellow/10 px-5 py-4 text-sm text-brand-yellow" role="status">
          <p className="font-semibold">Oracle updater activity attribution unavailable</p>
          <p className="mt-1 leading-6 text-content-secondary">
            This release does not emit enough public telemetry to attribute updater transactions or gross native spend. The wallet table must not be read as a complete oracle-updater activity or funding view.
          </p>
        </div>
      ) : null}

      <section className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
        <Metric
          label={trackedWalletCount === null ? 'Loaded wallet rows' : 'Tracked wallets'}
          value={trackedWalletCount ?? rows.length.toLocaleString()}
          hint={trackedWalletCount === null
            ? 'Rows loaded from the paginated public operational-wallet registry.'
            : `${rows.length.toLocaleString()} wallet rows loaded from the registry.`}
        />
        <Metric
          label={atRiskWalletCount === null ? 'Low-funding loaded rows' : 'Low-funding wallets'}
          value={atRiskWalletCount ?? atRiskWallets.length.toLocaleString()}
          tone={(atRiskWalletCount ?? String(atRiskWallets.length)) !== '0' ? 'warning' : 'positive'}
          hint={atRiskWalletCount === null
            ? 'Warning, critical, or depleted gross-spend status among loaded rows only.'
            : 'Warning, critical, or depleted gross-spend status.'}
        />
        <Metric
          label="Oracle updater"
          value={identifiedOracleUpdater
            ? 'Identified'
            : oracleUpdaterPublished
              ? 'Published (not loaded)'
              : 'Unavailable'}
          tone={oracleUpdaterPublished ? 'positive' : 'warning'}
          hint={identifiedOracleUpdater
            ? 'The updater identity is present in the loaded wallet rows.'
            : oracleUpdaterPublished
              ? 'The backend confirms a public updater identity outside the currently loaded page.'
              : 'Only a checked-in public release identity is accepted.'}
        />
        <Metric
          label="Observation window"
          value={window}
          hint={windowBounds(data.wallets.windowStart, data.wallets.windowEnd)}
        />
      </section>

      <div className="border border-brand-border/25 bg-app-bg-deep/45 px-5 py-4 text-sm text-content-secondary">
        <p className="font-semibold text-content-primary">Role-address balance is not proof of protocol liveness</p>
        <p className="mt-1 leading-6">
          Owner, pauser, treasury, contract, and Safe addresses may rely on separate relayers. Their own ETH balance can be useful context, but it does not reveal or guarantee the funding of every process needed to operate the protocol.
        </p>
      </div>

      <Section
        title="Native funding diagnostics"
        description={definitionSummary(data.wallets.definition)}
      >
        <OperationalWalletTable
          wallets={rows}
          releaseId={data.releaseId || releaseId}
          window={window}
        />
        {query.hasNextPage ? (
          <div className="border-t border-brand-border/15 p-4 text-center">
            <button
              type="button"
              onClick={() => void query.fetchNextPage()}
              disabled={query.isFetchingNextPage}
              className="border border-brand-border/40 px-5 py-2.5 text-sm font-semibold hover:border-brand-orange disabled:cursor-wait disabled:opacity-60"
            >
              {query.isFetchingNextPage ? 'Loading operational wallets…' : 'Load more operational wallets'}
            </button>
          </div>
        ) : null}
        <RawEvidence
          title="Wallet registry and balance evidence"
          value={{
            releaseId: data.releaseId,
            confirmedBlock: data.confirmedBlock,
            evidence: data.evidence,
            availability,
            units: data.wallets.units,
            definition: data.wallets.definition,
            wallets: rows.map((wallet) => wallet.raw),
          }}
        />
      </Section>

      <p className="text-xs leading-5 text-content-tertiary">
        Gross spend is gas cost plus the full transaction-native value sent. Native refunds are not netted without trace or contract telemetry, so this is a conservative historical diagnostic—not net cost, profit, a time runway, or a guarantee. Browse all activity for this release in{' '}
        <Link to={`/transactions?release=${releaseQuery}`} className="text-brand-peach hover:underline">Transactions</Link>.
      </p>
    </div>
  )
}

function OperationalWalletTable({
  wallets,
  releaseId,
  window,
}: {
  wallets: ProtocolWalletSummary[]
  releaseId: string
  window: string
}) {
  if (wallets.length === 0) {
    return (
      <div className="px-5 py-14 text-center">
        <p className="font-semibold">No public operational wallets</p>
        <p className="mt-2 text-sm text-content-secondary">This release has not published a monitorable operational-wallet identity.</p>
      </div>
    )
  }

  return (
    <div className="overflow-x-auto">
      <table className="w-full min-w-[1160px] border-collapse text-left text-sm">
        <thead>
          <tr className="border-b border-brand-border/20 text-xs uppercase tracking-wide text-content-tertiary">
            <th className="px-5 py-3">Wallet / role</th>
            <th className="px-5 py-3">Funding status</th>
            <th className="px-5 py-3 text-right">Native balance</th>
            <th className="px-5 py-3 text-right">Gross-spend inputs</th>
            <th className="px-5 py-3 text-right">Est. txs at gross spend</th>
            <th className="px-5 py-3">Last activity</th>
            <th className="px-5 py-3">Evidence</th>
          </tr>
        </thead>
        <tbody>
          {wallets.map((wallet) => {
            const detailQuery = new URLSearchParams({ window })
            if (releaseId) detailQuery.set('release', releaseId)
            return (
              <tr key={wallet.address} className="border-b border-brand-border/10 align-top hover:bg-white/[0.025]">
                <td className="px-5 py-4">
                  <Link
                    className="font-mono text-brand-peach hover:underline"
                    to={`/protocol-wallets/${encodeURIComponent(wallet.address)}?${detailQuery.toString()}`}
                  >
                    {shortAddress(wallet.address)}
                  </Link>
                  <div className="mt-2 flex flex-wrap gap-1.5">
                    {wallet.roles.length > 0
                      ? wallet.roles.map((role) => (
                        <span key={role} className="rounded-full border border-brand-border/30 px-2 py-0.5 text-[11px] text-content-secondary">{humanize(role)}</span>
                      ))
                      : <span className="text-xs text-content-tertiary">Role unavailable</span>}
                  </div>
                </td>
                <td className="px-5 py-4">
                  <Status status={wallet.status} />
                </td>
                <td className="px-5 py-4 text-right tabular-nums">{formatNativeWei(wallet.nativeBalanceWei)}</td>
                <td className="px-5 py-4 text-right tabular-nums">
                  <div>{formatNativeWei(wallet.observedGasCostWei)}</div>
                  <div className="mt-1 text-xs text-content-tertiary">
                    + {formatNativeWei(wallet.observedTransactionNativeValueWei)} tx value
                  </div>
                  <div className="mt-1 text-xs text-content-tertiary">
                    {wallet.observedActionCount ?? 'Unavailable'} actions · {wallet.observedTransactionCount ?? 'Unavailable'} txs
                  </div>
                </td>
                <td className="px-5 py-4 text-right tabular-nums">
                  <div className="text-base font-semibold">{wallet.estimatedTransactionsAtObservedGrossSpend ?? 'Unavailable'}</div>
                  <div className="mt-1 text-xs text-content-tertiary">
                    Median gross spend {formatNativeWei(wallet.medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei)}
                  </div>
                </td>
                <td className="px-5 py-4">
                  <div>{formatActivityTime(wallet.lastActivityTimestamp)}</div>
                  {wallet.lastActivityTransactionHash ? (
                    <Link
                      className="mt-1 block font-mono text-xs text-brand-peach hover:underline"
                      to={`/transactions/${encodeURIComponent(wallet.lastActivityTransactionHash)}?release=${encodeURIComponent(releaseId)}`}
                    >
                      {shortAddress(wallet.lastActivityTransactionHash)}
                    </Link>
                  ) : null}
                </td>
                <td className="px-5 py-4">
                  <EvidenceBadge level={wallet.evidence} />
                  {wallet.availability.length > 0 ? (
                    <div className="mt-1 text-xs text-brand-yellow">{wallet.availability.length} incomplete field{wallet.availability.length === 1 ? '' : 's'}</div>
                  ) : null}
                </td>
              </tr>
            )
          })}
        </tbody>
      </table>
    </div>
  )
}

function Status({ status }: { status: string }) {
  const tone = walletStatusTone(status)
  const classes = tone === 'positive'
    ? 'border-positive/35 bg-positive/10 text-positive'
    : tone === 'warning'
      ? 'border-brand-yellow/35 bg-brand-yellow/10 text-brand-yellow'
      : tone === 'critical'
        ? 'border-brand-orange/40 bg-brand-orange/10 text-brand-orange'
        : 'border-content-tertiary/30 bg-white/5 text-content-tertiary'
  return <span className={`inline-flex rounded-full border px-2 py-0.5 text-xs font-semibold ${classes}`}>{humanize(status)}</span>
}

function readWindow(value: string | null): string {
  return WINDOWS.includes(value as typeof WINDOWS[number]) ? value as typeof WINDOWS[number] : '7d'
}

function formatActivityTime(timestamp: number | null): string {
  if (timestamp === null) return 'Unavailable'
  return new Intl.DateTimeFormat('en-GB', {
    day: '2-digit',
    month: 'short',
    year: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
    timeZone: 'UTC',
    timeZoneName: 'short',
  }).format(new Date(timestamp * 1000))
}

function windowBounds(start: number | null, end: number | null): string {
  if (start === null || end === null) return 'Confirmed successful activity in the selected fixed window.'
  return `${formatActivityTime(start)} to ${formatActivityTime(end)}`
}

function definitionSummary(value: Record<string, unknown>): string {
  const trackedIdentity = readString(value.trackedIdentity)
    ?? 'Public release-scoped operational wallets are included when their identity is attributable without private configuration.'
  const interpretation = readString(value.interpretation)
    ?? 'Transaction capacity is a conservative gross-spend diagnostic; refunds are not netted, and it is not a time estimate, net cost, or profit calculation.'
  return `${trackedIdentity} ${interpretation}`
}
