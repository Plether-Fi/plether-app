import { useMemo, useState } from 'react'
import { Link } from 'react-router-dom'
import {
  InsightsApiError,
  formatBpsPercent,
  formatDurationSeconds,
  formatSponsoredEthWei,
  shortHash,
  useTradingAccountsOverview,
  type KernelOperationItem,
  type KernelStackStatus,
} from '../api'
import { ArbiscanButton, CopyButton } from '../components/ActionButtons'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { StatCard } from '../components/StatCard'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { TraderIdentity } from '../components/TraderIdentity'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { arbiscanLinkKey, resolvePerpsChain } from '../utils/chain'
import { formatUtc, shortAddress } from '../utils/format'

type OpModeFilter = 'all' | 'sponsored' | 'eth' | 'cancel'
type OpStatusFilter = 'all' | 'pending' | 'included' | 'retired'

function stackTone(status: KernelStackStatus | undefined): StatusTone {
  if (status === 'healthy') return 'live'
  if (status === 'paused' || status === 'degraded') return 'degraded'
  if (status === 'disabled') return 'blocked'
  return 'unknown'
}

function stackLabel(status: KernelStackStatus | undefined): string {
  if (status === 'healthy') return 'Kernel v4 healthy'
  if (status === 'paused') return 'Kernel reconciler paused'
  if (status === 'degraded') return 'Kernel stack degraded'
  if (status === 'disabled') return 'Kernel submission disabled'
  if (status === 'unconfigured') return 'Kernel v4 gateway not configured'
  return 'Loading Kernel telemetry'
}

function opStatusBadge(op: KernelOperationItem): { label: string; className: string } {
  if (op.status === 'included') {
    if (op.executionSuccess === false) {
      return {
        label: 'Reverted',
        className: 'border-brand-orange/40 bg-brand-orange/10 text-brand-orange',
      }
    }
    return {
      label: 'Included',
      className: 'border-positive/40 bg-positive/10 text-positive',
    }
  }
  if (op.status === 'pending') {
    return {
      label: 'Pending',
      className: 'border-brand-yellow/40 bg-brand-yellow/10 text-brand-yellow',
    }
  }
  if (op.status === 'nonce_consumed') {
    return {
      label: op.cancellationHash ? 'Cancelled' : 'Nonce consumed',
      className: 'border-brand-peach/40 bg-brand-peach/10 text-brand-peach',
    }
  }
  return {
    label: 'Expired',
    className: 'border-brand-border/40 bg-app-bg text-content-tertiary',
  }
}

export function TradingAccountsPage() {
  const protocol = useProtocolContext()
  const query = useTradingAccountsOverview()
  const [accountSearch, setAccountSearch] = useState('')
  const [modeFilter, setModeFilter] = useState<OpModeFilter>('all')
  const [statusFilter, setStatusFilter] = useState<OpStatusFilter>('all')

  const data = query.data
  const awaitingDeploy = query.error instanceof InsightsApiError && query.error.status === 404
  const chain = resolvePerpsChain(data?.network.chainId ?? protocol.chainId)
  const releaseVersion = data?.network.releaseVersion ?? protocol.releaseVersion

  const adoption = data?.adoption
  const lifecycle = data?.lifecycle
  const profile = data?.profile
  const controls = data?.controls
  const pipeline = data?.pipeline

  const totalModeOps = (adoption?.sponsoredOpsTotal ?? 0) + (adoption?.ethOpsTotal ?? 0)
  const sponsoredSharePct =
    totalModeOps > 0 ? Math.round(((adoption?.sponsoredOpsTotal ?? 0) / totalModeOps) * 1000) / 10 : 0
  const ethSharePct = totalModeOps > 0 ? Math.max(0, Number((100 - sponsoredSharePct).toFixed(1))) : 0

  const filteredAccounts = useMemo(() => {
    const items = data?.accounts ?? []
    const q = accountSearch.trim().replace(/^@/, '').toLowerCase()
    if (!q) return items
    return items.filter(
      (a) =>
        a.sender.toLowerCase().includes(q) ||
        a.ownerAddress.toLowerCase().includes(q) ||
        (a.discordUsername?.toLowerCase().includes(q) ?? false),
    )
  }, [data?.accounts, accountSearch])

  const filteredOperations = useMemo(() => {
    const items = data?.operations ?? []
    return items.filter((op) => {
      if (modeFilter === 'sponsored' && op.mode !== 'sponsored') return false
      if (modeFilter === 'eth' && (op.mode !== 'eth' || op.cancellationOnly)) return false
      if (modeFilter === 'cancel' && !op.cancellationOnly && !op.cancellationHash) return false
      if (statusFilter === 'pending' && op.status !== 'pending') return false
      if (statusFilter === 'included' && op.status !== 'included') return false
      if (
        statusFilter === 'retired' &&
        op.status !== 'nonce_consumed' &&
        op.status !== 'sponsorship_expired'
      ) {
        return false
      }
      return true
    })
  }, [data?.operations, modeFilter, statusFilter])

  const seenArbiscanKeys = new Set<string>()
  function claimArbiscan(kind: 'address' | 'tx', value: string): boolean {
    const key = arbiscanLinkKey(kind, value, chain.chainId)
    if (seenArbiscanKeys.has(key)) return false
    seenArbiscanKeys.add(key)
    return true
  }

  const pinnedContracts = profile
    ? [
        {
          label: `EntryPoint v${profile.entryPointVersion}`,
          address: profile.entryPoint,
          codeHash: profile.entryPointCodeHash,
        },
        {
          label: 'KernelFactory',
          address: profile.factory,
          codeHash: profile.factoryCodeHash,
        },
        {
          label: 'KernelUUPS Implementation',
          address: profile.implementation,
          codeHash: profile.implementationCodeHash,
        },
        {
          label: 'ECDSAValidator',
          address: profile.validator,
          codeHash: profile.validatorCodeHash,
        },
        ...(profile.paymaster
          ? [
              {
                label: 'Verifying Paymaster',
                address: profile.paymaster,
                codeHash: profile.policyId ? `Policy ${profile.policyId}` : null,
              },
            ]
          : []),
      ]
    : []

  return (
    <div className="space-y-6 sm:space-y-8">
      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={pipeline?.reconcilerHeadBlock ?? protocol.blockNumber}
        observedAt={data?.observedAt ?? protocol.observedAt}
        tone={awaitingDeploy ? 'unknown' : query.isError ? 'blocked' : stackTone(data?.status)}
        statusLabel={
          awaitingDeploy
            ? 'Awaiting Kernel Trading Accounts backend deploy'
            : query.isError
              ? 'Kernel Trading Accounts telemetry unavailable'
              : stackLabel(data?.status)
        }
        note={pipeline?.reconcilerPausedReason ?? undefined}
      />

      <header className="border border-brand-border/30 bg-surface-panel p-5 sm:p-6">
        <div className="flex flex-col justify-between gap-4 lg:flex-row lg:items-end">
          <div className="min-w-0">
            <p className="text-xs font-semibold uppercase tracking-[0.16em] text-brand-peach">
              ZeroDev Kernel v4 · ERC-4337 EntryPoint v{profile?.entryPointVersion ?? '0.9'}
            </p>
            <h1 className="mt-1 text-2xl font-bold tracking-tight sm:text-3xl">
              Trading Accounts (Smart Wallets)
            </h1>
            <p className="mt-1.5 max-w-2xl text-sm text-content-secondary">
              Smart wallet deployment stack, Sponsored vs. ETH-paid gas payment modes, owner-to-account
              mapping, nonce-lane recovery, and live Kernel operation journal.
            </p>
          </div>

          <div className="flex flex-wrap items-center gap-3 text-xs font-semibold">
            <Link to="/sponsored" className="text-brand-peach hover:underline">
              Paymaster &amp; budgets →
            </Link>
            <Link to="/infra" className="text-content-secondary hover:text-brand-peach hover:underline">
              Alto &amp; infra →
            </Link>
          </div>
        </div>

        {controls && pipeline ? (
          <div className="mt-4 flex flex-wrap items-center gap-2 border-t border-brand-border/20 pt-4 text-xs">
            <StatusPill
              tone={controls.sponsorshipEnabled ? 'live' : 'unknown'}
              label={controls.sponsorshipEnabled ? 'Sponsored Gas: On' : 'Sponsored Gas: Off'}
            />
            <StatusPill
              tone={controls.ethModeEnabled ? 'live' : 'unknown'}
              label={controls.ethModeEnabled ? 'ETH-Paid Gas: On' : 'ETH-Paid Gas: Off'}
            />
            <StatusPill
              tone={controls.protectionCommitsEnabled ? 'live' : 'unknown'}
              label={
                controls.protectionCommitsEnabled ? 'TP/SL Commits: On' : 'TP/SL Commits: Off'
              }
            />
            <StatusPill
              tone={controls.ownerAdmissionMode === 'open' ? 'live' : 'degraded'}
              label={
                controls.ownerAdmissionMode === 'canary'
                  ? `Admission: Canary${controls.admittedOwnerCount != null ? ` (${String(controls.admittedOwnerCount)} owners)` : ''}`
                  : 'Admission: Open'
              }
            />
            <StatusPill
              tone={
                pipeline.bundlerReachable && pipeline.bundlerEntryPointSupported
                  ? 'live'
                  : 'blocked'
              }
              label={
                pipeline.bundlerReachable && pipeline.bundlerEntryPointSupported
                  ? 'Alto v0.9 SafeMode OK'
                  : `Alto: ${pipeline.bundlerStatus}`
              }
            />
            {pipeline.reconcilerSafeBlock ? (
              <StatusPill
                tone={pipeline.reconcilerFresh ? 'live' : 'stale'}
                label={
                  pipeline.reconcilerLagSeconds != null
                    ? `Reconciler · ${formatDurationSeconds(pipeline.reconcilerLagSeconds)} lag`
                    : 'Reconciler active'
                }
              />
            ) : null}
          </div>
        ) : null}
      </header>

      {query.isLoading && !data ? (
        <Panel>
          <LoadingState rows={5} />
        </Panel>
      ) : !data && awaitingDeploy ? (
        <Panel>
          <EmptyState
            title="Kernel Trading Accounts telemetry awaiting backend deploy"
            message="Deploy the backend serving GET /api/insights/v1/trading-accounts/overview to inspect ZeroDev Kernel v4 smart wallet telemetry."
          />
        </Panel>
      ) : !data && query.isError ? (
        <ErrorState
          title="Unable to load Kernel Trading Accounts telemetry"
          message={query.error instanceof Error ? query.error.message : 'Unexpected error'}
          onRetry={() => void query.refetch()}
        />
      ) : data && !data.configured ? (
        <Panel>
          <EmptyState
            title="ZeroDev Kernel v4 gateway not active on this environment"
            message="The backend currently runs without a configured Kernel v4 gateway profile. Once the Kernel v4 canary or public rollout is enabled, smart wallet adoption, ETH vs. Sponsored gas split, pinned contracts, and the operation journal will appear here automatically."
          />
        </Panel>
      ) : data && adoption && lifecycle ? (
        <>
          {/* 4-Card KPI Strip */}
          <section aria-label="Kernel smart account KPIs" className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
            <StatCard
              label="Kernel Smart Accounts"
              value={adoption.uniqueAccounts.toLocaleString('en-US')}
              detail={`${adoption.uniqueOwners.toLocaleString('en-US')} owners · ${adoption.activeAccounts24h.toLocaleString('en-US')} active (24h)`}
            />
            <StatCard
              label="Kernel Operations"
              value={adoption.totalOperations.toLocaleString('en-US')}
              detail={`${adoption.operations24h.toLocaleString('en-US')} in 24h · ${adoption.pendingOpsCount.toLocaleString('en-US')} pending${adoption.oldestPendingAgeSeconds != null ? ` (oldest ${formatDurationSeconds(adoption.oldestPendingAgeSeconds)})` : ''}`}
            />
            <StatCard
              label="Gas Mode Split"
              value={
                totalModeOps > 0
                  ? `${sponsoredSharePct.toFixed(0)}% / ${ethSharePct.toFixed(0)}%`
                  : '—'
              }
              detail={`Sponsored ${adoption.sponsoredOpsTotal.toLocaleString('en-US')} · ETH-paid ${adoption.ethOpsTotal.toLocaleString('en-US')} · ${formatSponsoredEthWei(lifecycle.totalGasCostWei, 4)}`}
            />
            <StatCard
              label="Execution & Recovery"
              value={formatBpsPercent(lifecycle.successRateBps)}
              tone={
                lifecycle.successRateBps != null && lifecycle.successRateBps >= 9500
                  ? 'positive'
                  : lifecycle.successRateBps != null && lifecycle.successRateBps < 9000
                    ? 'negative'
                    : 'neutral'
              }
              detail={`${lifecycle.cancellationsAccepted.toLocaleString('en-US')} ETH cancellations · ${lifecycle.sponsorshipExpired.toLocaleString('en-US')} expired`}
            />
          </section>

          {/* Payment Mode Split & Lifecycle Funnel */}
          <section aria-labelledby="kernel-modes-title" className="space-y-3">
            <h2 id="kernel-modes-title" className="text-lg font-semibold">
              Gas Payment Mode Split &amp; Operation Lifecycle
            </h2>
            <Panel className="space-y-4 p-5">
              <div>
                <div className="mb-1.5 flex flex-wrap items-center justify-between gap-2 font-mono text-xs">
                  <div className="flex flex-wrap items-center gap-4">
                    <span className="font-semibold text-positive">
                      Sponsored {sponsoredSharePct.toFixed(1)}% (
                      {adoption.sponsoredOpsTotal.toLocaleString('en-US')} ops ·{' '}
                      {adoption.sponsoredOps24h.toLocaleString('en-US')} 24h ·{' '}
                      {formatSponsoredEthWei(lifecycle.sponsoredGasCostWei, 4)})
                    </span>
                    <span className="font-semibold text-brand-peach">
                      ETH-Paid {ethSharePct.toFixed(1)}% (
                      {adoption.ethOpsTotal.toLocaleString('en-US')} ops ·{' '}
                      {adoption.ethOps24h.toLocaleString('en-US')} 24h ·{' '}
                      {formatSponsoredEthWei(lifecycle.ethGasCostWei, 4)})
                    </span>
                  </div>
                  <span className="text-[11px] text-content-tertiary">
                    {adoption.cancellationOpsTotal.toLocaleString('en-US')} cancellation-only ETH ops
                  </span>
                </div>
                <div className="flex h-2.5 w-full overflow-hidden bg-app-bg">
                  {totalModeOps > 0 ? (
                    <>
                      <div
                        className="bg-positive transition-all"
                        style={{ width: `${sponsoredSharePct.toFixed(2)}%` }}
                      />
                      <div
                        className="bg-brand-peach transition-all"
                        style={{ width: `${ethSharePct.toFixed(2)}%` }}
                      />
                    </>
                  ) : null}
                </div>
              </div>

              <dl className="grid grid-cols-2 gap-2.5 border-t border-brand-border/20 pt-4 sm:grid-cols-3 lg:grid-cols-6">
                <div className="border border-brand-border/20 bg-app-bg p-3">
                  <dt className="text-[11px] text-content-tertiary">Prepared</dt>
                  <dd className="mt-1 font-mono text-base font-semibold tabular-nums text-content-primary">
                    {adoption.totalPreparations.toLocaleString('en-US')}
                  </dd>
                </div>
                <div className="border border-brand-border/20 bg-app-bg p-3">
                  <dt className="text-[11px] text-content-tertiary">Included OK</dt>
                  <dd className="mt-1 font-mono text-base font-semibold tabular-nums text-positive">
                    {lifecycle.includedSucceeded.toLocaleString('en-US')}
                  </dd>
                </div>
                <div className="border border-brand-border/20 bg-app-bg p-3">
                  <dt className="text-[11px] text-content-tertiary">Included Reverted</dt>
                  <dd className="mt-1 font-mono text-base font-semibold tabular-nums text-brand-orange">
                    {lifecycle.includedReverted.toLocaleString('en-US')}
                  </dd>
                </div>
                <div className="border border-brand-border/20 bg-app-bg p-3">
                  <dt className="text-[11px] text-content-tertiary">Pending</dt>
                  <dd className="mt-1 font-mono text-base font-semibold tabular-nums text-brand-yellow">
                    {lifecycle.pending.toLocaleString('en-US')}
                  </dd>
                </div>
                <div className="border border-brand-border/20 bg-app-bg p-3">
                  <dt className="text-[11px] text-content-tertiary">Nonce Consumed</dt>
                  <dd className="mt-1 font-mono text-base font-semibold tabular-nums text-brand-peach">
                    {lifecycle.nonceConsumed.toLocaleString('en-US')}
                  </dd>
                </div>
                <div className="border border-brand-border/20 bg-app-bg p-3">
                  <dt className="text-[11px] text-content-tertiary">Expired / Cancelled</dt>
                  <dd className="mt-1 font-mono text-base font-semibold tabular-nums text-content-secondary">
                    {lifecycle.sponsorshipExpired.toLocaleString('en-US')} /{' '}
                    {lifecycle.cancellationsAccepted.toLocaleString('en-US')}
                  </dd>
                </div>
              </dl>
            </Panel>
          </section>

          {/* Pinned ZeroDev Kernel v4 Contract Stack */}
          {profile ? (
            <section aria-labelledby="kernel-contracts-title" className="space-y-3">
              <div className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 id="kernel-contracts-title" className="text-lg font-semibold">
                  Pinned Kernel v4 Contract Stack
                </h2>
                <span className="font-mono text-xs text-content-tertiary">
                  Profile: {profile.deployment} · {profile.accountVersion} · Proxy code hash{' '}
                  {shortHash(profile.accountCodeHash)}
                </span>
              </div>
              <Panel>
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[720px] text-left text-sm">
                    <caption className="sr-only">
                      Pinned ZeroDev Kernel v4 smart account contracts and runtime code hashes
                    </caption>
                    <thead className="border-b border-brand-border/25 text-xs uppercase tracking-[0.08em] text-content-tertiary">
                      <tr>
                        <th scope="col" className="px-4 py-3">Component</th>
                        <th scope="col" className="px-4 py-3">Contract Address</th>
                        <th scope="col" className="px-4 py-3">Pinned Code Hash / Policy</th>
                      </tr>
                    </thead>
                    <tbody className="divide-y divide-brand-border/15 font-mono text-xs">
                      {pinnedContracts.map((item) => {
                        const showArb = claimArbiscan('address', item.address)
                        return (
                          <tr key={item.label} className="hover:bg-brand-peach/[0.03]">
                            <td className="px-4 py-3 font-sans text-xs font-semibold text-content-primary">
                              {item.label}
                            </td>
                            <td className="px-4 py-3">
                              <div className="inline-flex items-center gap-2">
                                <span className="text-brand-peach" title={item.address}>
                                  {item.address}
                                </span>
                                <CopyButton value={item.address} />
                                {showArb ? (
                                  <ArbiscanButton
                                    kind="address"
                                    value={item.address}
                                    chainId={chain.chainId}
                                  />
                                ) : null}
                              </div>
                            </td>
                            <td className="px-4 py-3 text-content-tertiary">
                              {item.codeHash ? (
                                <span title={item.codeHash}>
                                  {item.codeHash.startsWith('0x')
                                    ? shortHash(item.codeHash)
                                    : item.codeHash}
                                </span>
                              ) : (
                                '—'
                              )}
                            </td>
                          </tr>
                        )
                      })}
                    </tbody>
                  </table>
                </div>
              </Panel>
            </section>
          ) : null}

          {/* Active Kernel Smart Accounts (Sender ↔ Owner) */}
          <section aria-labelledby="kernel-accounts-title" className="space-y-3">
            <div className="flex flex-col justify-between gap-3 sm:flex-row sm:items-end">
              <div>
                <h2 id="kernel-accounts-title" className="text-lg font-semibold">
                  Kernel Smart Accounts (Sender ↔ Owner)
                </h2>
                <p className="mt-0.5 text-xs text-content-tertiary">
                  Recently active ZeroDev Kernel v4 Trading Accounts, their controlling Privy/EOA
                  owner wallet, gas mode usage, and highest nonce.
                </p>
              </div>
              <div className="w-full sm:w-64">
                <label htmlFor="kernel-acct-search" className="sr-only">
                  Filter Kernel smart accounts
                </label>
                <input
                  id="kernel-acct-search"
                  type="search"
                  value={accountSearch}
                  onChange={(event) => {
                    setAccountSearch(event.target.value)
                  }}
                  placeholder="Filter 0x or @handle"
                  className="w-full border border-brand-border/35 bg-app-bg px-3 py-1.5 font-mono text-xs text-content-primary placeholder:font-sans placeholder:text-content-tertiary focus:border-brand-peach/70 focus:outline-none"
                />
              </div>
            </div>

            <Panel>
              {filteredAccounts.length > 0 ? (
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[820px] text-left text-sm">
                    <caption className="sr-only">
                      ZeroDev Kernel v4 smart accounts and owner wallets
                    </caption>
                    <thead className="border-b border-brand-border/25 text-xs uppercase tracking-[0.08em] text-content-tertiary">
                      <tr>
                        <th scope="col" className="px-4 py-3">Smart Account (Sender)</th>
                        <th scope="col" className="px-4 py-3">Owner (Privy / EOA)</th>
                        <th scope="col" className="px-4 py-3">Last Mode</th>
                        <th scope="col" className="px-4 py-3 text-right">Sponsored / ETH</th>
                        <th scope="col" className="px-4 py-3 text-right">Included / Pend / Ret</th>
                        <th scope="col" className="px-4 py-3 text-right">Max Nonce</th>
                        <th scope="col" className="px-4 py-3 text-right">Last Active</th>
                      </tr>
                    </thead>
                    <tbody className="divide-y divide-brand-border/15">
                      {filteredAccounts.map((acct) => {
                        const showOwnerArb = claimArbiscan('address', acct.ownerAddress)
                        return (
                          <tr key={acct.sender} className="hover:bg-brand-peach/[0.03]">
                            <td className="whitespace-nowrap px-4 py-3">
                              <TraderIdentity
                                address={acct.sender}
                                discordUsername={acct.discordUsername}
                                to={`/trading-accounts/${acct.sender}`}
                                avatarSize={18}
                                showCopy
                              />
                            </td>
                            <td className="whitespace-nowrap px-4 py-3 font-mono text-xs text-content-secondary">
                              <div className="inline-flex items-center gap-1.5">
                                <span title={acct.ownerAddress}>
                                  {shortAddress(acct.ownerAddress)}
                                </span>
                                <CopyButton value={acct.ownerAddress} />
                                {showOwnerArb ? (
                                  <ArbiscanButton
                                    kind="address"
                                    value={acct.ownerAddress}
                                    chainId={chain.chainId}
                                  />
                                ) : null}
                              </div>
                            </td>
                            <td className="whitespace-nowrap px-4 py-3 font-mono text-xs">
                              <span
                                className={`border px-1.5 py-0.5 text-[10px] font-semibold uppercase ${
                                  acct.lastMode === 'sponsored'
                                    ? 'border-positive/40 bg-positive/10 text-positive'
                                    : 'border-brand-peach/40 bg-brand-peach/10 text-brand-peach'
                                }`}
                              >
                                {acct.lastMode === 'sponsored' ? 'Sponsored' : 'ETH-Paid'}
                              </span>
                            </td>
                            <td className="whitespace-nowrap px-4 py-3 text-right font-mono text-xs tabular-nums">
                              <span className="text-positive">{acct.sponsoredCount}</span>
                              <span className="text-content-tertiary"> / </span>
                              <span className="text-brand-peach">{acct.ethCount}</span>
                            </td>
                            <td className="whitespace-nowrap px-4 py-3 text-right font-mono text-xs tabular-nums text-content-secondary">
                              <span className="text-content-primary">{acct.includedCount}</span>
                              <span className="text-content-tertiary"> / </span>
                              <span className={acct.pendingCount > 0 ? 'text-brand-yellow' : ''}>
                                {acct.pendingCount}
                              </span>
                              <span className="text-content-tertiary"> / </span>
                              <span>{acct.retiredCount}</span>
                            </td>
                            <td className="whitespace-nowrap px-4 py-3 text-right font-mono text-xs tabular-nums text-content-secondary">
                              {acct.highestNonce}
                            </td>
                            <td className="whitespace-nowrap px-4 py-3 text-right font-mono text-xs tabular-nums text-content-tertiary">
                              {formatUtc(acct.lastActivityAt)}
                            </td>
                          </tr>
                        )
                      })}
                    </tbody>
                  </table>
                </div>
              ) : (
                <EmptyState
                  title="No matching Kernel smart accounts"
                  message="No Kernel v4 preparations or operations match the current filter."
                />
              )}
            </Panel>
          </section>

          {/* Unified Kernel Operation Journal (Sponsored + ETH-Paid + Cancellations) */}
          <section aria-labelledby="kernel-ops-title" className="space-y-3">
            <div className="flex flex-col justify-between gap-3 lg:flex-row lg:items-end">
              <div>
                <h2 id="kernel-ops-title" className="text-lg font-semibold">
                  Kernel Operation Journal (Sponsored &amp; ETH-Paid)
                </h2>
                <p className="mt-0.5 text-xs text-content-tertiary">
                  Unified ERC-4337 EntryPoint v0.9 operation ledger across both payment modes and
                  ETH nonce-cancellation recoveries.
                </p>
              </div>

              <div className="flex flex-wrap items-center gap-2">
                <div className="flex flex-wrap items-center gap-1" role="group" aria-label="Filter operation mode">
                  {(
                    [
                      ['all', 'All Modes'],
                      ['sponsored', 'Sponsored'],
                      ['eth', 'ETH-Paid'],
                      ['cancel', 'Cancellations'],
                    ] as const
                  ).map(([id, label]) => (
                    <button
                      key={id}
                      type="button"
                      aria-pressed={modeFilter === id}
                      onClick={() => {
                        setModeFilter(id)
                      }}
                      className={`cursor-pointer border px-2.5 py-1.5 text-xs font-semibold transition-colors ${
                        modeFilter === id
                          ? 'border-brand-orange bg-brand-orange text-content-primary'
                          : 'border-brand-border/35 bg-surface-panel text-content-secondary hover:border-brand-peach/50 hover:text-content-primary'
                      }`}
                    >
                      {label}
                    </button>
                  ))}
                </div>

                <select
                  aria-label="Filter operation status"
                  value={statusFilter}
                  onChange={(event) => {
                    setStatusFilter(event.target.value as OpStatusFilter)
                  }}
                  className="border border-brand-border/35 bg-surface-panel px-2.5 py-1.5 text-xs font-semibold text-content-primary focus:border-brand-peach/70 focus:outline-none"
                >
                  <option value="all">All statuses</option>
                  <option value="pending">Pending</option>
                  <option value="included">Included</option>
                  <option value="retired">Retired / Expired</option>
                </select>
              </div>
            </div>

            <Panel>
              {filteredOperations.length > 0 ? (
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[760px] text-left text-sm">
                    <caption className="sr-only">
                      Recent ZeroDev Kernel v4 UserOperations across Sponsored and ETH-paid gas modes
                    </caption>
                    <thead className="border-b border-brand-border/25 text-xs uppercase tracking-[0.08em] text-content-tertiary">
                      <tr>
                        <th scope="col" className="py-3 pl-4 pr-2.5">UserOp</th>
                        <th scope="col" className="px-2.5 py-3">Smart Account</th>
                        <th scope="col" className="px-2.5 py-3">Mode</th>
                        <th scope="col" className="px-2.5 py-3 text-right">Nonce</th>
                        <th scope="col" className="px-2.5 py-3">Status</th>
                        <th scope="col" className="px-2.5 py-3 text-right">Gas Cost</th>
                        <th scope="col" className="px-2.5 py-3">Tx</th>
                        <th scope="col" className="py-3 pl-2.5 pr-4 text-right">Created</th>
                      </tr>
                    </thead>
                    <tbody className="divide-y divide-brand-border/15 font-mono text-xs">
                      {filteredOperations.map((op) => {
                        const badge = opStatusBadge(op)
                        const showTxArb = op.txHash ? claimArbiscan('tx', op.txHash) : false
                        return (
                          <tr key={op.userOpHash} className="hover:bg-brand-peach/[0.03]">
                            <td className="whitespace-nowrap py-2.5 pl-4 pr-2.5">
                              <div className="inline-flex items-center gap-1.5">
                                <span title={op.userOpHash} className="text-content-primary">
                                  {shortAddress(op.userOpHash)}
                                </span>
                                <CopyButton value={op.userOpHash} />
                              </div>
                            </td>
                            <td className="whitespace-nowrap px-2.5 py-2.5">
                              <TraderIdentity
                                address={op.sender}
                                to={`/trading-accounts/${op.sender}`}
                                avatarSize={16}
                              />
                            </td>
                            <td className="whitespace-nowrap px-2.5 py-2.5">
                              <span
                                className={`border px-1.5 py-0.5 text-[10px] font-semibold uppercase ${
                                  op.cancellationOnly
                                    ? 'border-brand-yellow/40 bg-brand-yellow/10 text-brand-yellow'
                                    : op.mode === 'sponsored'
                                      ? 'border-positive/40 bg-positive/10 text-positive'
                                      : 'border-brand-peach/40 bg-brand-peach/10 text-brand-peach'
                                }`}
                              >
                                {op.cancellationOnly
                                  ? 'ETH Cancel'
                                  : op.mode === 'sponsored'
                                    ? 'Sponsored'
                                    : 'ETH-Paid'}
                              </span>
                            </td>
                            <td className="whitespace-nowrap px-2.5 py-2.5 text-right tabular-nums text-content-secondary">
                              {op.nonce}
                            </td>
                            <td className="whitespace-nowrap px-2.5 py-2.5">
                              <span
                                className={`border px-1.5 py-0.5 text-[10px] font-semibold uppercase ${badge.className}`}
                              >
                                {badge.label}
                              </span>
                            </td>
                            <td className="whitespace-nowrap px-2.5 py-2.5 text-right tabular-nums text-content-secondary">
                              {op.actualGasCostWei
                                ? formatSponsoredEthWei(op.actualGasCostWei, 5)
                                : '—'}
                            </td>
                            <td className="whitespace-nowrap px-2.5 py-2.5">
                              {op.txHash ? (
                                <div className="inline-flex items-center gap-1.5">
                                  <Link
                                    to={`/tx/${op.txHash}`}
                                    className="text-brand-peach hover:underline"
                                    title={op.txHash}
                                  >
                                    {shortAddress(op.txHash)}
                                  </Link>
                                  {showTxArb ? (
                                    <ArbiscanButton
                                      kind="tx"
                                      value={op.txHash}
                                      chainId={chain.chainId}
                                    />
                                  ) : null}
                                </div>
                              ) : (
                                <span className="text-content-tertiary">—</span>
                              )}
                            </td>
                            <td className="whitespace-nowrap py-2.5 pl-2.5 pr-4 text-right tabular-nums text-content-tertiary">
                              {formatUtc(op.createdAt)}
                            </td>
                          </tr>
                        )
                      })}
                    </tbody>
                  </table>
                </div>
              ) : (
                <EmptyState
                  title="No matching Kernel operations"
                  message="Switch the payment mode or status filter to see other recorded Kernel UserOperations."
                />
              )}
            </Panel>
          </section>
        </>
      ) : null}
    </div>
  )
}
