import { useState } from 'react'
import { Link } from 'react-router-dom'
import {
  InsightsApiError,
  formatEthWei,
  formatRateBps,
  isLowEthBalance,
  useKeepersActivity,
  useKeepersStatus,
  type KeeperRoleId,
} from '../api'
import { ArbiscanButton } from '../components/ActionButtons'
import { AddressAvatar } from '../components/AddressAvatar'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { arbiscanLinkKey, resolvePerpsChain } from '../utils/chain'
import { formatCompactAssets, formatUtc, shortAddress } from '../utils/format'

type KeeperRoleFilter = 'all' | KeeperRoleId

interface RoleSurface {
  id: KeeperRoleId
  label: string
  contract: string
  detectionRule: string
  indexingStatus: 'indexed' | 'pending'
}

const ROLE_SURFACES: RoleSurface[] = [
  {
    id: 'order_execution',
    label: 'Order execution',
    contract: 'OrderRouter',
    detectionRule: 'tx.from on executed V3 open/close order transactions',
    indexingStatus: 'indexed',
  },
  {
    id: 'lp_settlement',
    label: 'LP settlement',
    contract: 'HousePool',
    detectionRule:
      'tx.from on canonical HousePool LpEpochSettled logs (including external permissionless callers)',
    indexingStatus: 'indexed',
  },
  {
    id: 'protection',
    label: 'TP/SL protection',
    contract: 'PositionProtectionBook',
    detectionRule: 'tx.from on PositionProtectionTriggered executions',
    indexingStatus: 'indexed',
  },
  {
    id: 'liquidation',
    label: 'Liquidation',
    contract: 'CfdEngine',
    detectionRule: 'tx.from on PositionLiquidated transactions + keeperBountyAssets',
    indexingStatus: 'indexed',
  },
  {
    id: 'oracle',
    label: 'Oracle updates',
    contract: 'PletherOracle',
    detectionRule:
      'Standalone on-chain PletherOracle price-push tx indexing is pending; live Pyth feed freshness is tracked on /infra.',
    indexingStatus: 'pending',
  },
]

const ROLE_LABELS: Record<string, string> = {
  order_execution: 'Orders',
  lp_settlement: 'LP Settlement',
  protection: 'Protection',
  liquidation: 'Liquidation',
  oracle: 'Oracle',
}

function isEndpointNotDeployed(error: unknown): boolean {
  return error instanceof InsightsApiError && error.status === 404
}

export function KeepersPage() {
  const protocol = useProtocolContext()
  const keepersQuery = useKeepersStatus()
  const activityQuery = useKeepersActivity(50)
  const [roleFilter, setRoleFilter] = useState<KeeperRoleFilter>('all')
  const [search, setSearch] = useState('')

  const statusData = keepersQuery.data
  const activityData = activityQuery.data
  const chainId =
    statusData?.network.chainId ?? activityData?.network.chainId ?? protocol.chainId
  const chain = resolvePerpsChain(chainId)
  const releaseVersion =
    statusData?.network.releaseVersion ??
    activityData?.network.releaseVersion ??
    protocol.releaseVersion

  const awaitingDeploy =
    isEndpointNotDeployed(keepersQuery.error) || isEndpointNotDeployed(activityQuery.error)

  const selectedSurface = ROLE_SURFACES.find((surface) => surface.id === roleFilter) ?? null
  const normalizedSearch = search.trim().toLowerCase()

  const filteredKeepers =
    statusData?.keepers.filter((keeper) => {
      const matchesRole = roleFilter === 'all' || keeper.roles.includes(roleFilter)
      const matchesSearch =
        normalizedSearch.length === 0 || keeper.address.toLowerCase().includes(normalizedSearch)
      return matchesRole && matchesSearch
    }) ?? []

  const filteredActivity =
    activityData?.activity.filter((item) => {
      const matchesRole = roleFilter === 'all' || item.role === roleFilter
      const matchesSearch =
        normalizedSearch.length === 0 ||
        item.keeperAddress.toLowerCase().includes(normalizedSearch) ||
        item.txHash.toLowerCase().includes(normalizedSearch)
      return matchesRole && matchesSearch
    }) ?? []

  const bannerTone: StatusTone = awaitingDeploy
    ? 'unknown'
    : keepersQuery.isError && activityQuery.isError
      ? 'blocked'
      : statusData && statusData.summary.activeKeepers24h > 0
        ? 'live'
        : keepersQuery.isLoading
          ? 'unknown'
          : 'stale'

  const bannerLabel = awaitingDeploy
    ? 'Awaiting Stage 2 backend deploy'
    : keepersQuery.isError && activityQuery.isError
      ? 'Keeper telemetry unavailable'
      : statusData
        ? `${String(statusData.summary.activeKeepers24h)} active keepers (24h)`
        : keepersQuery.isLoading
          ? 'Scanning on-chain keepers'
          : 'No keeper executions detected'

  const summary = statusData?.summary ?? null
  const latencyDisplay =
    summary && (summary.latencyP50Seconds !== null || summary.latencyP95Seconds !== null)
      ? `${summary.latencyP50Seconds !== null ? `${String(summary.latencyP50Seconds)}s` : '—'} / ${summary.latencyP95Seconds !== null ? `${String(summary.latencyP95Seconds)}s` : '—'}`
      : undefined

  return (
    <div className="space-y-8">
      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={protocol.blockNumber}
        observedAt={statusData?.observedAt ?? protocol.observedAt}
        tone={bannerTone}
        statusLabel={bannerLabel}
        note="Permissionless tx.from discovery + live eth_getBalance"
      />

      <header className="max-w-3xl space-y-3">
        <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">
          Permissionless On-Chain Keepers
        </p>
        <h1 className="text-3xl font-semibold tracking-tight sm:text-4xl">
          Detected Keepers, Latency &amp; ETH Balances
        </h1>
        <p className="text-sm leading-6 text-content-secondary">
          Protocol execution is permissionless—every address discovered on-chain executing orders,
          settling LP epochs, triggering TP/SL protections, running liquidations, or updating
          oracles is tracked identically, with no special treatment for Plether-run signers.
        </p>
      </header>

      {awaitingDeploy ? (
        <div
          role="status"
          className="border border-brand-yellow/40 bg-brand-yellow/10 px-4 py-3 text-sm text-brand-yellow"
        >
          <strong>Connected backend does not serve Stage 2 Keepers endpoints yet (HTTP 404).</strong>{' '}
          Deploy the updated backend containing{' '}
          <code className="font-mono text-xs">/api/insights/v1/keepers/status</code> and{' '}
          <code className="font-mono text-xs">/api/insights/v1/keepers/activity</code> to populate
          live detected keepers and ETH balances here.
        </div>
      ) : null}

      <section aria-labelledby="keepers-summary-title">
        <h2 id="keepers-summary-title" className="sr-only">
          Ecosystem summary
        </h2>
        <Panel className="space-y-4 p-4 sm:p-6">
          {keepersQuery.isLoading ? (
            <LoadingState rows={3} />
          ) : !summary ? (
            <div className="font-mono text-xs text-content-tertiary">
              Keeper fleet telemetry: <strong className="text-content-secondary">Unavailable</strong>
            </div>
          ) : (
            <>
              <div className="flex flex-wrap items-end justify-between gap-4">
                <div>
                  <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                    24h Permissionless Keeper Executions
                  </p>
                  <div className="mt-1 flex flex-wrap items-baseline gap-3">
                    <span className="font-mono text-3xl font-bold tracking-tight tabular-nums text-content-primary sm:text-4xl">
                      {summary.executions24h.toLocaleString('en-US')}
                    </span>
                    <span className="font-mono text-sm font-semibold text-positive">
                      {summary.activeKeepers24h.toLocaleString('en-US')} active keeper
                      {summary.activeKeepers24h === 1 ? '' : 's'}
                    </span>
                  </div>
                </div>

                <dl className="flex flex-wrap gap-x-6 gap-y-2 font-mono text-xs">
                  <div>
                    <dt className="text-content-tertiary">Latency (p50 / p95)</dt>
                    <dd className="mt-0.5 text-base font-bold tabular-nums text-brand-peach">
                      {latencyDisplay ?? 'Unavailable'}
                    </dd>
                  </div>
                  <div>
                    <dt className="text-content-tertiary">Bounties paid (24h)</dt>
                    <dd className="mt-0.5 text-base font-bold tabular-nums text-content-primary">
                      {formatCompactAssets(summary.bountiesPaid24hAssets)}
                    </dd>
                  </div>
                </dl>
              </div>

              {/* Fleet Execution Concentration Bar */}
              {(statusData?.keepers.length ?? 0) > 0 ? (
                <div className="space-y-1.5 border-t border-brand-border/20 pt-3.5">
                  <div className="flex flex-wrap items-center justify-between gap-2 font-mono text-[11px] text-content-tertiary">
                    <span>24h execution share by detected keeper</span>
                    <span>Orders, epochs, protections &amp; liquidations</span>
                  </div>
                  {(() => {
                    const colors = ['#6EE7B7', '#FFB78B', '#7CC4FA', '#C4B5FD', '#FDE047']
                    const topKeepers = (statusData?.keepers ?? []).slice(0, 5).filter((k) => (k.shareBps ?? 0) > 0)
                    const assignedBps = topKeepers.reduce((sum, k) => sum + (k.shareBps ?? 0), 0)
                    const remainderBps = Math.max(0, 10000 - assignedBps)
                    return (
                      <>
                        <div className="flex h-2.5 w-full overflow-hidden border border-brand-border/20 bg-app-bg">
                          {topKeepers.map((k, idx) => (
                            <div
                              key={k.address}
                              title={`${shortAddress(k.address)}: ${formatRateBps(k.shareBps)}`}
                              style={{
                                width: `${String((k.shareBps ?? 0) / 100)}%`,
                                background: colors[idx % colors.length],
                              }}
                            />
                          ))}
                          {remainderBps > 0 ? (
                            <div
                              title={`Other / unattributed: ${formatRateBps(remainderBps)}`}
                              className="bg-brand-border/30"
                              style={{ width: `${String(remainderBps / 100)}%` }}
                            />
                          ) : null}
                        </div>
                        <div className="flex flex-wrap gap-x-4 gap-y-1 font-mono text-[11px] text-content-secondary">
                          {topKeepers.map((k, idx) => (
                            <Link
                              key={k.address}
                              to={`/keepers/${k.address}`}
                              className="inline-flex items-center gap-1.5 hover:text-brand-peach"
                            >
                              <span
                                className="h-2 w-2"
                                style={{ background: colors[idx % colors.length] }}
                              />
                              <span>{shortAddress(k.address)}</span>
                              <span className="text-content-tertiary">
                                {formatRateBps(k.shareBps)}
                              </span>
                            </Link>
                          ))}
                        </div>
                      </>
                    )
                  })()}
                </div>
              ) : null}
            </>
          )}
        </Panel>
      </section>

      <section aria-labelledby="keepers-detected-title" className="space-y-4">
        <div className="flex flex-col justify-between gap-4 lg:flex-row lg:items-end">
          <div>
            <h2 id="keepers-detected-title" className="text-xl font-semibold">
              Detected on-chain keepers
            </h2>
            <p className="mt-1 text-xs text-content-tertiary">
              Ranked dynamically by on-chain executions · Live ETH balance read via eth_getBalance
            </p>
          </div>

          <div className="w-full sm:max-w-xs">
            <label htmlFor="keeper-address-filter" className="sr-only">
              Filter keeper address
            </label>
            <input
              id="keeper-address-filter"
              type="search"
              value={search}
              onChange={(event) => {
                setSearch(event.target.value)
              }}
              placeholder="Filter keeper address (0x…)"
              className="w-full border border-brand-border/35 bg-app-bg px-4 py-2 text-sm font-mono placeholder:font-sans placeholder:text-content-tertiary"
            />
          </div>
        </div>

        <div className="flex flex-wrap gap-2" role="group" aria-label="Filter keepers by role">
          <button
            type="button"
            aria-pressed={roleFilter === 'all'}
            onClick={() => {
              setRoleFilter('all')
            }}
            className={`border px-3 py-1.5 text-xs font-semibold transition-colors ${
              roleFilter === 'all'
                ? 'border-brand-orange bg-brand-orange text-content-primary'
                : 'border-brand-border/30 bg-surface-panel text-content-secondary hover:border-brand-orange/50 hover:text-content-primary'
            }`}
          >
            All roles
          </button>
          {ROLE_SURFACES.map((surface) => (
            <button
              key={surface.id}
              type="button"
              aria-pressed={roleFilter === surface.id}
              onClick={() => {
                setRoleFilter(surface.id)
              }}
              className={`border px-3 py-1.5 text-xs font-semibold transition-colors ${
                roleFilter === surface.id
                  ? 'border-brand-orange bg-brand-orange text-content-primary'
                  : 'border-brand-border/30 bg-surface-panel text-content-secondary hover:border-brand-orange/50 hover:text-content-primary'
              }`}
            >
              {surface.label}
            </button>
          ))}
        </div>

        <Panel>
          {keepersQuery.isLoading ? (
            <LoadingState rows={4} />
          ) : keepersQuery.isError && !awaitingDeploy ? (
            <ErrorState
              title="Keeper discovery unavailable"
              message={keepersQuery.error.message}
              onRetry={() => void keepersQuery.refetch()}
            />
          ) : (
            <>
              <div className="overflow-x-auto">
                <table className="w-full min-w-[820px] text-left text-sm">
                  <caption className="sr-only">
                    Permissionless on-chain keepers detected by the indexer
                  </caption>
                  <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                    <tr>
                      <th scope="col" className="px-4 py-3.5">Keeper address</th>
                      <th scope="col" className="px-4 py-3.5">Observed roles</th>
                      <th scope="col" className="px-4 py-3.5">Executions (24h)</th>
                      <th scope="col" className="px-4 py-3.5">Share</th>
                      <th scope="col" className="px-4 py-3.5">Latency (p50 / p95)</th>
                      <th scope="col" className="px-4 py-3.5">ETH balance</th>
                      <th scope="col" className="px-4 py-3.5">Bounties earned</th>
                      <th scope="col" className="px-4 py-3.5">Last active</th>
                    </tr>
                  </thead>
                  <tbody>
                    {(() => {
                      const seenKeeperKeys = new Set<string>()
                      return filteredKeepers.map((keeper) => {
                        const lowEth = isLowEthBalance(keeper.ethBalanceWei)
                        const sharePct =
                          keeper.shareBps !== null ? Math.min(100, keeper.shareBps / 100) : 0
                        const keeperKey = arbiscanLinkKey('address', keeper.address, chainId)
                        const showArbiscan = !seenKeeperKeys.has(keeperKey)
                        if (showArbiscan) seenKeeperKeys.add(keeperKey)
                        return (
                        <tr
                          key={keeper.address}
                          className="border-b border-brand-border/15 last:border-0"
                        >
                          <td className="px-4 py-3.5 font-mono text-xs">
                            <div className="flex items-center gap-2">
                              <Link
                                to={`/keepers/${keeper.address.toLowerCase()}`}
                                title={keeper.address}
                                className="inline-flex items-center gap-2 text-brand-peach hover:underline"
                              >
                                <AddressAvatar address={keeper.address} size={18} />
                                {shortAddress(keeper.address)}
                              </Link>
                              {showArbiscan ? (
                                <ArbiscanButton
                                  kind="address"
                                  value={keeper.address}
                                  chainId={chainId}
                                />
                              ) : null}
                            </div>
                          </td>
                          <td className="px-4 py-3.5">
                            <div className="flex flex-wrap gap-1">
                              {keeper.roles.map((role) => (
                                <span
                                  key={role}
                                  className="border border-brand-border/30 bg-app-bg px-2 py-0.5 text-[11px] font-semibold text-content-secondary"
                                >
                                  {ROLE_LABELS[role] ?? role}
                                </span>
                              ))}
                            </div>
                          </td>
                          <td className="px-4 py-3.5 font-mono tabular-nums">
                            {keeper.executions24h.toLocaleString('en-US')}
                          </td>
                          <td className="px-4 py-3.5">
                            <div className="flex items-center gap-2">
                              <span className="font-mono text-xs tabular-nums">
                                {formatRateBps(keeper.shareBps)}
                              </span>
                              <div className="hidden h-1.5 w-14 overflow-hidden bg-surface-raised sm:block">
                                <div
                                  className="h-full bg-brand-peach"
                                  style={{ width: `${sharePct.toFixed(1)}%` }}
                                />
                              </div>
                            </div>
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            {keeper.latencyP50Seconds !== null || keeper.latencyP95Seconds !== null
                              ? `${keeper.latencyP50Seconds !== null ? `${String(keeper.latencyP50Seconds)}s` : '—'} / ${keeper.latencyP95Seconds !== null ? `${String(keeper.latencyP95Seconds)}s` : '—'}`
                              : '—'}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            <div className="flex items-center gap-2">
                              <span className={lowEth ? 'text-brand-yellow' : 'text-content-primary'}>
                                {formatEthWei(keeper.ethBalanceWei)}
                              </span>
                              {lowEth ? <StatusPill tone="degraded" label="Low" /> : null}
                            </div>
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            {formatCompactAssets(keeper.bountiesEarnedAssets)}
                          </td>
                          <td className="px-4 py-3.5 text-xs text-content-secondary">
                            <div className="font-mono">
                              #{Number(keeper.lastActiveBlock).toLocaleString('en-US')}
                            </div>
                            <div className="text-[11px] text-content-tertiary">
                              {formatUtc(keeper.lastActiveAt)}
                            </div>
                          </td>
                        </tr>
                      )
                    })})()}
                  </tbody>
                </table>
              </div>
              {filteredKeepers.length === 0 ? (
                <EmptyState
                  title={
                    statusData
                      ? 'No matching keepers'
                      : 'Keeper discovery feed awaiting backend endpoint'
                  }
                  message={
                    statusData
                      ? 'No detected on-chain keepers match the active role and address filters.'
                      : selectedSurface
                        ? `Once /api/insights/v1/keepers/status is connected, every on-chain address detected executing ${selectedSurface.label.toLowerCase()} (${selectedSurface.contract}) will appear here.`
                        : 'Once /api/insights/v1/keepers/status is connected, every on-chain address detected across permissionless protocol keeper surfaces will appear here with live ETH balances and latency.'
                  }
                />
              ) : null}
            </>
          )}
        </Panel>
      </section>

      <section aria-labelledby="keepers-surfaces-title" className="space-y-4">
        <div className="flex flex-wrap items-baseline justify-between gap-2">
          <h2 id="keepers-surfaces-title" className="text-xl font-semibold">
            Indexed permissionless surfaces
          </h2>
          <Link to="/infra" className="text-sm font-semibold text-brand-peach hover:underline">
            AA bundler &amp; infra readiness →
          </Link>
        </div>
        <div className="grid gap-3 md:grid-cols-2 lg:grid-cols-5">
          {ROLE_SURFACES.map((surface) => (
            <Panel key={surface.id} className="flex flex-col gap-2 p-4">
              <div>
                <StatusPill
                  tone={surface.indexingStatus === 'indexed' ? 'live' : 'unknown'}
                  label={surface.indexingStatus === 'indexed' ? 'Indexed' : 'Indexer pending'}
                />
              </div>
              <div className="min-w-0">
                <p
                  className="truncate font-mono text-xs text-brand-peach"
                  title={surface.contract}
                >
                  {surface.contract}
                </p>
                <h3 className="mt-1 text-sm font-semibold">{surface.label}</h3>
              </div>
              <p className="text-xs leading-5 text-content-secondary">
                {surface.detectionRule}
              </p>
            </Panel>
          ))}
        </div>
      </section>

      <section aria-labelledby="keepers-feed-title" className="space-y-4">
        <h2 id="keepers-feed-title" className="text-xl font-semibold">
          Recent permissionless keeper activity
        </h2>
        <Panel>
          {activityQuery.isLoading ? (
            <LoadingState rows={4} />
          ) : activityQuery.isError && !awaitingDeploy ? (
            <ErrorState
              title="Keeper activity unavailable"
              message={activityQuery.error.message}
              onRetry={() => void activityQuery.refetch()}
            />
          ) : filteredActivity.length > 0 ? (
            <div className="overflow-x-auto">
              <table className="w-full min-w-[760px] text-left text-sm">
                <caption className="sr-only">Recent on-chain permissionless keeper executions</caption>
                <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                  <tr>
                    <th scope="col" className="px-4 py-3.5">Time (UTC)</th>
                    <th scope="col" className="px-4 py-3.5">Role</th>
                    <th scope="col" className="px-4 py-3.5">Keeper</th>
                    <th scope="col" className="px-4 py-3.5">Latency</th>
                    <th scope="col" className="px-4 py-3.5">Bounty</th>
                    <th scope="col" className="px-4 py-3.5">Block</th>
                    <th scope="col" className="px-4 py-3.5">Tx</th>
                  </tr>
                </thead>
                <tbody>
                  {(() => {
                    const seenTxKeys = new Set<string>()
                    return filteredActivity.map((item, index) => {
                      const txKey = arbiscanLinkKey('tx', item.txHash, chainId)
                      const showArbiscan = !seenTxKeys.has(txKey)
                      if (showArbiscan) seenTxKeys.add(txKey)
                      return (
                        <tr
                          key={`${item.txHash}-${item.role}-${String(index)}`}
                          className="border-b border-brand-border/15 last:border-0"
                        >
                      <td className="px-4 py-3.5 text-xs text-content-secondary">
                        {formatUtc(item.occurredAt)}
                      </td>
                      <td className="px-4 py-3.5">
                        <span className="border border-brand-border/30 bg-app-bg px-2 py-0.5 text-xs font-semibold text-content-secondary">
                          {ROLE_LABELS[item.role] ?? item.role}
                        </span>
                      </td>
                      <td className="px-4 py-3.5 font-mono text-xs">
                        <Link
                          to={`/keepers/${item.keeperAddress.toLowerCase()}`}
                          title={item.keeperAddress}
                          className="inline-flex items-center gap-2 text-brand-peach hover:underline"
                        >
                          <AddressAvatar address={item.keeperAddress} size={16} />
                          {shortAddress(item.keeperAddress)}
                        </Link>
                      </td>
                      <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                        {item.latencySeconds !== null ? `${String(item.latencySeconds)}s` : '—'}
                      </td>
                      <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                        {item.bountyAssets ? formatCompactAssets(item.bountyAssets) : '—'}
                      </td>
                      <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                        {Number(item.blockNumber).toLocaleString('en-US')}
                      </td>
                      <td className="px-4 py-3.5 font-mono text-xs">
                        <div className="flex items-center gap-2">
                          <Link
                            to={`/tx/${item.txHash.toLowerCase()}`}
                            state={{
                              from: '/keepers',
                              fromLabel: 'Back to Keepers',
                            }}
                            className="text-brand-peach hover:underline"
                          >
                            {shortAddress(item.txHash)}
                          </Link>
                          {showArbiscan ? (
                            <ArbiscanButton kind="tx" value={item.txHash} chainId={chainId} />
                          ) : null}
                        </div>
                      </td>
                    </tr>
                      )
                    })
                  })()}
                </tbody>
              </table>
            </div>
          ) : (
            <EmptyState
              title={
                activityData
                  ? 'No matching keeper activity'
                  : 'Keeper activity stream awaiting backend endpoint'
              }
              message={
                activityData
                  ? 'No recent transactions match your current role or address filter.'
                  : 'Recent on-chain order fills, LP epoch settlements, TP/SL triggers, liquidations, and oracle updates across all detected keepers will stream here with latency, bounties, and explorer links.'
              }
            />
          )}
        </Panel>
      </section>
    </div>
  )
}
