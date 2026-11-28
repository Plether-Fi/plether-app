import { useState } from 'react'
import { Link, useParams } from 'react-router-dom'
import {
  formatEthWei,
  formatRateBps,
  isLowEthBalance,
  useKeeperDetail,
} from '../api'
import { ArbiscanButton, CopyButton } from '../components/ActionButtons'
import { AddressAvatar } from '../components/AddressAvatar'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { BreadcrumbActionLink, PageBreadcrumbBar } from '../components/PageBreadcrumbBar'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { arbiscanLinkKey, resolvePerpsChain } from '../utils/chain'
import { formatCompactAssets, formatUtc, shortAddress } from '../utils/format'

const ROLE_LABELS: Record<string, string> = {
  order_execution: 'Orders',
  lp_settlement: 'LP Settlement',
  protection: 'Protection',
  liquidation: 'Liquidation',
  oracle: 'Oracle',
}

export function KeeperDetailPage() {
  const { address } = useParams()
  const normalized = address?.trim().toLowerCase() ?? ''
  const validAddress = /^0x[0-9a-f]{40}$/.test(normalized)

  const protocol = useProtocolContext()
  const keeperQuery = useKeeperDetail(address, 50)
  const [roleFilter, setRoleFilter] = useState<string>('all')

  const data = keeperQuery.data
  const keeper = data?.keeper ?? null
  const chainId = data?.network.chainId ?? protocol.chainId
  const chain = resolvePerpsChain(chainId)
  const releaseVersion = data?.network.releaseVersion ?? protocol.releaseVersion

  const lowEth = isLowEthBalance(keeper?.ethBalanceWei)
  const filteredActivity =
    data?.activity.filter((item) => roleFilter === 'all' || item.role === roleFilter) ?? []

  const bannerTone: StatusTone = !validAddress
    ? 'unknown'
    : keeperQuery.isError
      ? 'blocked'
      : keeper && keeper.executions24h > 0
        ? lowEth
          ? 'stale'
          : 'live'
        : keeperQuery.isLoading
          ? 'unknown'
          : 'stale'

  const bannerLabel = !validAddress
    ? 'Invalid keeper address'
    : keeperQuery.isError
      ? 'Keeper telemetry unavailable'
      : keeper
        ? keeper.executions24h > 0
          ? `${keeper.executions24h.toLocaleString('en-US')} executions (24h)${lowEth ? ' · Low ETH' : ''}`
          : 'No executions in last 24h'
        : 'Loading keeper telemetry'

  const latency = keeper?.latency ?? null
  const latencyDisplay =
    latency && (latency.p50 != null || latency.p95 != null)
      ? `${latency.p50 != null ? `${String(latency.p50)}s` : '—'} / ${latency.p95 != null ? `${String(latency.p95)}s` : '—'}`
      : undefined

  return (
    <div className="space-y-6 sm:space-y-8">
      <PageBreadcrumbBar
        items={[
          { label: 'System', to: '/protocol' },
          { label: 'Keepers', to: '/keepers' },
          { label: validAddress ? shortAddress(normalized) : 'Invalid address' },
        ]}
        rightSlot={
          validAddress ? (
            <BreadcrumbActionLink to={`/wallets/${normalized}`}>
              Wallet Ledger →
            </BreadcrumbActionLink>
          ) : undefined
        }
      />

      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={keeper?.lastActiveBlock ? Number(keeper.lastActiveBlock) : protocol.blockNumber}
        observedAt={data?.observedAt ?? protocol.observedAt}
        tone={bannerTone}
        statusLabel={bannerLabel}
        note="Permissionless on-chain tx.from attribution + live eth_getBalance"
      />

      <header className="flex flex-col gap-4 sm:flex-row sm:items-end sm:justify-between">
        <div className="flex items-start gap-3.5">
          {validAddress ? <AddressAvatar address={normalized} size={44} /> : null}
          <div className="min-w-0 space-y-1.5">
            <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">
              Permissionless Keeper Profile
            </p>
            <div className="flex flex-wrap items-center gap-2">
              <h1 className="font-mono text-2xl font-semibold tracking-tight sm:text-3xl">
                {validAddress ? shortAddress(normalized) : (address ?? 'Invalid address')}
              </h1>
              {validAddress ? (
                <>
                  <CopyButton value={normalized} />
                  <ArbiscanButton kind="address" value={normalized} chainId={chainId} />
                </>
              ) : null}
            </div>
            {validAddress ? (
              <p className="break-all font-mono text-xs text-content-tertiary">{normalized}</p>
            ) : null}
          </div>
        </div>

        {keeper ? (
          <div className="flex flex-wrap items-center gap-1.5">
            {keeper.roles.map((role) => (
              <span
                key={role}
                className="border border-brand-border/35 bg-surface-panel px-2.5 py-1 text-xs font-semibold text-content-secondary"
              >
                {ROLE_LABELS[role] ?? role}
              </span>
            ))}
            {lowEth ? (
              <StatusPill tone="degraded" label="Low ETH runway" />
            ) : keeper.ethBalanceWei != null ? (
              <StatusPill tone="live" label="ETH funded" />
            ) : null}
          </div>
        ) : null}
      </header>

      {!validAddress ? (
        <Panel>
          <EmptyState
            title="Invalid keeper address"
            message="Keeper addresses must be valid 42-character 0x-prefixed EVM addresses."
          />
        </Panel>
      ) : keeperQuery.isLoading ? (
        <Panel>
          <LoadingState rows={5} />
        </Panel>
      ) : keeperQuery.isError ? (
        <ErrorState
          title="Unable to load keeper profile"
          message={keeperQuery.error.message}
          onRetry={() => void keeperQuery.refetch()}
        />
      ) : data && keeper ? (
        <>
          <section aria-labelledby="keeper-summary-title">
            <h2 id="keeper-summary-title" className="sr-only">
              Keeper 24h summary
            </h2>
            <Panel className="overflow-hidden">
              <div className="flex flex-wrap items-end justify-between gap-4 p-4 sm:p-6">
                <div className="min-w-0 flex-1 space-y-2">
                  <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                    24h Executions &amp; Fleet Share
                  </p>
                  <div className="flex flex-wrap items-baseline gap-3">
                    <span
                      className={`font-mono text-3xl font-bold tracking-tight tabular-nums sm:text-4xl ${
                        keeper.executions24h > 0 ? 'text-positive' : 'text-content-primary'
                      }`}
                    >
                      {keeper.executions24h.toLocaleString('en-US')}
                    </span>
                    {keeper.shareBps != null ? (
                      <span className="font-mono text-sm text-content-secondary">
                        {formatRateBps(keeper.shareBps)} of 24h protocol executions
                      </span>
                    ) : null}
                  </div>
                  {keeper.shareBps != null ? (
                    <div className="h-2 max-w-md overflow-hidden border border-brand-border/20 bg-app-bg">
                      <div
                        className="h-full bg-positive"
                        style={{ width: `${String(Math.min(100, keeper.shareBps / 100))}%` }}
                      />
                    </div>
                  ) : null}
                </div>

                <div className="text-left font-mono sm:text-right">
                  <p className="text-xs text-content-tertiary">Live ETH Balance</p>
                  <p
                    className={`mt-1 text-2xl font-bold tabular-nums ${
                      lowEth ? 'text-brand-yellow' : 'text-positive'
                    }`}
                  >
                    {keeper.ethBalanceWei != null ? formatEthWei(keeper.ethBalanceWei) : 'Unavailable'}
                  </p>
                  <p className="mt-0.5 text-xs text-content-tertiary">
                    {lowEth ? 'Below 0.01 ETH gas runway threshold' : 'Live eth_getBalance read'}
                  </p>
                </div>
              </div>

              <dl className="grid grid-cols-1 divide-y divide-brand-border/15 border-t border-brand-border/20 bg-app-bg/35 sm:grid-cols-2 sm:divide-x sm:divide-y-0">
                <div className="px-4 py-3.5 sm:px-6">
                  <dt className="text-xs text-content-tertiary">Latency (p50 / p95)</dt>
                  <dd className="mt-1 font-mono text-lg font-bold tabular-nums text-brand-peach">
                    {latencyDisplay ?? 'Unavailable'}
                  </dd>
                  <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
                    {latency && latency.sampleCount > 0
                      ? `p99 ${latency.p99 != null ? `${String(latency.p99)}s` : '—'} · max ${latency.max != null ? `${String(latency.max)}s` : '—'} (${String(latency.sampleCount)} samples)`
                      : 'Commit or trigger → confirmation'}
                  </p>
                </div>
                <div className="px-4 py-3.5 sm:px-6">
                  <dt className="text-xs text-content-tertiary">Bounties earned (24h)</dt>
                  <dd className="mt-1 font-mono text-lg font-bold tabular-nums text-content-primary">
                    {formatCompactAssets(keeper.bountiesEarnedAssets)}
                  </dd>
                  <p className="mt-0.5 font-mono text-[11px] text-content-tertiary">
                    {keeper.lastActiveAt
                      ? `Last active ${formatUtc(keeper.lastActiveAt)}`
                      : 'No executions in last 24h'}
                  </p>
                </div>
              </dl>
            </Panel>
          </section>

          {/* Per-Role Breakdown */}
          {data.roleBreakdown.length > 0 ? (
            <section aria-labelledby="keeper-roles-title" className="space-y-3">
              <h2 id="keeper-roles-title" className="text-lg font-semibold">
                Role breakdown (24h)
              </h2>
              <Panel className="overflow-x-auto">
                <table className="w-full min-w-[640px] text-left text-sm">
                  <thead className="border-b border-brand-border/25 bg-app-bg/40 text-xs font-medium text-content-secondary">
                    <tr>
                      <th scope="col" className="px-4 py-3">Role</th>
                      <th scope="col" className="px-4 py-3">Executions</th>
                      <th scope="col" className="px-4 py-3">Share of keeper</th>
                      <th scope="col" className="px-4 py-3">Latency (p50 / p95)</th>
                      <th scope="col" className="px-4 py-3">Bounties earned</th>
                    </tr>
                  </thead>
                  <tbody>
                    {data.roleBreakdown.map((rb) => {
                      const pct = rb.shareBps != null ? Math.min(100, rb.shareBps / 100) : 0
                      return (
                        <tr
                          key={rb.role}
                          className="border-b border-brand-border/15 last:border-0"
                        >
                          <td className="px-4 py-3.5">
                            <span className="border border-brand-border/30 bg-app-bg px-2.5 py-0.5 text-xs font-semibold text-content-primary">
                              {ROLE_LABELS[rb.role] ?? rb.role}
                            </span>
                          </td>
                          <td className="px-4 py-3.5 font-mono tabular-nums">
                            {rb.executions24h.toLocaleString('en-US')}
                          </td>
                          <td className="px-4 py-3.5">
                            <div className="flex items-center gap-2.5">
                              <span className="font-mono text-xs tabular-nums">
                                {formatRateBps(rb.shareBps)}
                              </span>
                              <div className="h-1.5 w-20 overflow-hidden bg-surface-raised">
                                <div
                                  className="h-full bg-brand-peach"
                                  style={{ width: `${pct.toFixed(1)}%` }}
                                />
                              </div>
                            </div>
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            {rb.latencyP50Seconds != null || rb.latencyP95Seconds != null
                              ? `${rb.latencyP50Seconds != null ? `${String(rb.latencyP50Seconds)}s` : '—'} / ${rb.latencyP95Seconds != null ? `${String(rb.latencyP95Seconds)}s` : '—'}`
                              : '—'}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            {formatCompactAssets(rb.bountiesEarnedAssets)}
                          </td>
                        </tr>
                      )
                    })}
                  </tbody>
                </table>
              </Panel>
            </section>
          ) : null}

          {/* Recent Executions */}
          <section aria-labelledby="keeper-activity-title" className="space-y-3">
            <div className="flex flex-wrap items-center justify-between gap-3">
              <h2 id="keeper-activity-title" className="text-lg font-semibold">
                Recent on-chain executions ({filteredActivity.length})
              </h2>
              {keeper.roles.length > 1 ? (
                <div className="flex flex-wrap gap-1.5" role="tablist" aria-label="Filter executions by role">
                  {(['all', ...keeper.roles] as const).map((role) => {
                    const active = roleFilter === role
                    return (
                      <button
                        key={role}
                        type="button"
                        role="tab"
                        aria-selected={active}
                        onClick={() => {
                          setRoleFilter(role)
                        }}
                        className={`border px-2.5 py-1 text-xs font-semibold transition-colors ${
                          active
                            ? 'border-brand-orange bg-brand-orange text-content-primary'
                            : 'border-brand-border/35 bg-surface-panel text-content-secondary hover:border-brand-peach/50 hover:text-content-primary'
                        }`}
                      >
                        {role === 'all' ? 'All roles' : (ROLE_LABELS[role] ?? role)}
                      </button>
                    )
                  })}
                </div>
              ) : null}
            </div>

            <Panel>
              {filteredActivity.length > 0 ? (
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[700px] text-left text-sm">
                    <thead className="border-b border-brand-border/25 bg-app-bg/40 text-xs font-medium text-content-secondary">
                      <tr>
                        <th scope="col" className="px-4 py-3">Time (UTC)</th>
                        <th scope="col" className="px-4 py-3">Role</th>
                        <th scope="col" className="px-4 py-3">Latency</th>
                        <th scope="col" className="px-4 py-3">Bounty</th>
                        <th scope="col" className="px-4 py-3">Block</th>
                        <th scope="col" className="px-4 py-3">Transaction</th>
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
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            {item.latencySeconds != null ? `${String(item.latencySeconds)}s` : '—'}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            {item.bountyAssets ? formatCompactAssets(item.bountyAssets) : '—'}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs tabular-nums">
                            #{Number(item.blockNumber).toLocaleString('en-US')}
                          </td>
                          <td className="px-4 py-3.5 font-mono text-xs">
                            <div className="flex items-center gap-2">
                              <Link
                                to={`/tx/${item.txHash}`}
                                state={{
                                  from: `/keepers/${normalized}`,
                                  fromLabel: `Back to Keeper ${shortAddress(normalized)}`,
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
                  title="No executions recorded in the last 24 hours"
                  message="On-chain order fills, LP settlements, TP/SL triggers, and liquidations executed by this address will appear here automatically."
                />
              )}
            </Panel>
          </section>
        </>
      ) : null}
    </div>
  )
}
