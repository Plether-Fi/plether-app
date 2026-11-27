import { useMemo, useState } from 'react'
import { Link, useParams } from 'react-router-dom'
import {
  InsightsApiError,
  formatBpsPercent,
  formatDurationSeconds,
  formatSponsoredEthWei,
  shortHash,
  useTradingAccountDetail,
  type KernelAccountOperationItem,
} from '../api'
import { ArbiscanButton, CopyButton } from '../components/ActionButtons'
import { AddressAvatar } from '../components/AddressAvatar'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { StatCard } from '../components/StatCard'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { arbiscanLinkKey, resolvePerpsChain } from '../utils/chain'
import { formatUtc, isWalletAddress, shortAddress } from '../utils/format'

type OpModeFilter = 'all' | 'sponsored' | 'eth' | 'cancel'
type OpStatusFilter = 'all' | 'pending' | 'included' | 'retired'

const LOW_ETH_GAS_THRESHOLD_WEI = 500_000_000_000_000n // 0.0005 ETH

function parseWeiBigInt(raw: string | null): bigint | null {
  if (!raw || !/^\d+$/.test(raw)) return null
  try {
    return BigInt(raw)
  } catch {
    return null
  }
}

function utilizationBarClass(bps: number | null): string {
  if (bps == null) return 'bg-brand-border/50'
  if (bps >= 9000) return 'bg-brand-orange'
  if (bps >= 7000) return 'bg-brand-yellow'
  return 'bg-positive'
}

function opStatusBadge(op: KernelAccountOperationItem): { label: string; className: string } {
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
      label: op.cancellationHash ? 'Cancelled' : 'Nonce Used',
      className: 'border-brand-peach/40 bg-brand-peach/10 text-brand-peach',
    }
  }
  return {
    label: 'Expired',
    className: 'border-brand-border/40 bg-app-bg text-content-tertiary',
  }
}

function deploymentPill(
  rpcAvailable: boolean,
  deployed: boolean,
  codeHashMatchesPinned: boolean | null,
): { tone: StatusTone; label: string } {
  if (!rpcAvailable) return { tone: 'unknown', label: 'RPC Unavailable' }
  if (!deployed) return { tone: 'degraded', label: 'Counterfactual (Not Deployed)' }
  if (codeHashMatchesPinned === false) return { tone: 'blocked', label: 'Code Hash Mismatch' }
  if (codeHashMatchesPinned === true) return { tone: 'live', label: 'Deployed · Pinned Kernel v4' }
  return { tone: 'live', label: 'Deployed' }
}

export function TradingAccountDetailPage() {
  const { address = '' } = useParams()
  const normalizedAddress = address.trim().toLowerCase()
  const validAddress = isWalletAddress(normalizedAddress)

  const ctx = useProtocolContext()
  const query = useTradingAccountDetail(validAddress ? normalizedAddress : '')
  const [modeFilter, setModeFilter] = useState<OpModeFilter>('all')
  const [statusFilter, setStatusFilter] = useState<OpStatusFilter>('all')

  const data = query.data
  const chain = resolvePerpsChain(data?.network.chainId ?? ctx.chainId)
  const awaitingDeploy =
    query.isError && query.error instanceof InsightsApiError && query.error.status === 404

  const seenArbiscan = new Set<string>()
  const claimArbiscan = (kind: 'address' | 'tx', value: string): boolean => {
    const key = arbiscanLinkKey(kind, value)
    if (seenArbiscan.has(key)) return false
    seenArbiscan.add(key)
    return true
  }

  const filteredOperations = useMemo(() => {
    const ops = data?.operations ?? []
    return ops.filter((op) => {
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

  if (!validAddress) {
    return (
      <div className="space-y-4">
        <Link
          to="/trading-accounts"
          className="inline-flex items-center gap-1.5 font-mono text-xs text-brand-peach hover:underline"
        >
          ← Trading Accounts (Kernel v4)
        </Link>
        <Panel>
          <EmptyState
            title="Invalid smart account address"
            message={`"${address}" is not a valid 20-byte EVM address.`}
          />
        </Panel>
      </div>
    )
  }

  const onchain = data?.onchain
  const summary = data?.summary
  const sponsorship = data?.sponsorship
  const ethBalWei = parseWeiBigInt(onchain?.ethBalanceWei ?? null)
  const lowEthGas =
    ethBalWei !== null && ethBalWei < LOW_ETH_GAS_THRESHOLD_WEI && (summary?.ethCount ?? 0) > 0
  const totalModeOps = (summary?.sponsoredCount ?? 0) + (summary?.ethCount ?? 0)
  const sponsoredSharePct =
    totalModeOps > 0 ? (((summary?.sponsoredCount ?? 0) / totalModeOps) * 100) : 0
  const ethSharePct = totalModeOps > 0 ? (((summary?.ethCount ?? 0) / totalModeOps) * 100) : 0

  const depPill = onchain
    ? deploymentPill(onchain.rpcAvailable, onchain.deployed, onchain.codeHashMatchesPinned)
    : { tone: 'unknown' as StatusTone, label: 'Loading on-chain state' }

  const freshnessTone: StatusTone = !data
    ? query.isError
      ? 'blocked'
      : 'unknown'
    : !data.configured
      ? 'unknown'
      : onchain?.codeHashMatchesPinned === false || onchain?.nonceSynced === false
        ? 'degraded'
        : 'live'

  const showSenderArb = claimArbiscan('address', normalizedAddress)
  const showOwnerArb = data?.ownerAddress ? claimArbiscan('address', data.ownerAddress) : false

  return (
    <div className="space-y-6">
      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={data?.network.releaseVersion ?? ctx.releaseVersion}
        blockNumber={ctx.blockNumber}
        observedAt={data?.observedAt ?? ctx.observedAt}
        tone={freshnessTone}
        statusLabel={
          onchain?.codeHashMatchesPinned === false
            ? 'Smart account code hash mismatch'
            : onchain?.nonceSynced === false
              ? 'EntryPoint nonce drift detected'
              : data?.configured
                ? 'Kernel v4 smart account live'
                : 'Kernel v4 gateway unconfigured'
        }
      />

      {/* Breadcrumb & Cross-Navigation */}
      <div className="flex flex-wrap items-center justify-between gap-2 font-mono text-xs">
        <Link to="/trading-accounts" className="text-brand-peach hover:underline">
          ← Trading Accounts (Kernel v4)
        </Link>
        <div className="flex flex-wrap items-center gap-4">
          <Link to={`/wallets/${normalizedAddress}`} className="text-brand-peach hover:underline">
            Trading positions &amp; PnL →
          </Link>
          <Link to="/sponsored" className="text-content-secondary hover:text-content-primary">
            Paymaster &amp; budgets →
          </Link>
        </div>
      </div>

      {/* Header */}
      <header className="border border-brand-border/30 bg-surface-panel p-6">
        <div className="flex flex-wrap items-start justify-between gap-4">
          <div className="flex items-start gap-3.5">
            <AddressAvatar address={normalizedAddress} size={36} className="mt-0.5 shrink-0" />
            <div>
              <p className="font-mono text-xs uppercase tracking-[0.14em] text-brand-peach">
                ZeroDev Kernel v4 · Smart Wallet Inspector
              </p>
              <div className="mt-1 flex flex-wrap items-center gap-2">
                <h1 className="font-mono text-lg font-semibold text-content-primary sm:text-xl">
                  {data?.discordUsername ? `@${data.discordUsername}` : shortAddress(normalizedAddress)}
                </h1>
                {data?.profile ? (
                  <span className="border border-brand-border/35 bg-app-bg px-2 py-0.5 font-mono text-[11px] text-content-tertiary">
                    {data.profile.deployment} · EP v{data.profile.entryPointVersion}
                  </span>
                ) : null}
              </div>

              {/* Sender & Owner identity rows */}
              <div className="mt-2.5 flex flex-col gap-1.5 font-mono text-xs">
                <div className="flex flex-wrap items-center gap-2">
                  <span className="w-24 text-[11px] uppercase tracking-[0.08em] text-content-tertiary">
                    Smart Acct
                  </span>
                  <span className="break-all text-content-primary">{normalizedAddress}</span>
                  <CopyButton value={normalizedAddress} />
                  {showSenderArb ? (
                    <ArbiscanButton
                      kind="address"
                      value={normalizedAddress}
                      chainId={chain.chainId}
                    />
                  ) : null}
                </div>
                {data?.ownerAddress ? (
                  <div className="flex flex-wrap items-center gap-2">
                    <span className="w-24 text-[11px] uppercase tracking-[0.08em] text-content-tertiary">
                      Owner EOA
                    </span>
                    <span className="break-all text-brand-peach">{data.ownerAddress}</span>
                    <CopyButton value={data.ownerAddress} />
                    {showOwnerArb ? (
                      <ArbiscanButton
                        kind="address"
                        value={data.ownerAddress}
                        chainId={chain.chainId}
                      />
                    ) : null}
                  </div>
                ) : null}
              </div>
            </div>
          </div>

          {data?.firstSeenAt || data?.lastActivityAt ? (
            <div className="flex flex-col items-start gap-1 font-mono text-[11px] text-content-tertiary sm:items-end">
              {data.firstSeenAt ? <span>First seen: {formatUtc(data.firstSeenAt)}</span> : null}
              {data.lastActivityAt ? <span>Last active: {formatUtc(data.lastActivityAt)}</span> : null}
            </div>
          ) : null}
        </div>

        {/* Status Pills */}
        {data?.configured && onchain ? (
          <div className="mt-4 flex flex-wrap gap-2 border-t border-brand-border/20 pt-4">
            <StatusPill tone={depPill.tone} label={depPill.label} />
            {onchain.nonceSynced != null ? (
              <StatusPill
                tone={onchain.nonceSynced ? 'live' : 'blocked'}
                label={
                  onchain.nonceSynced
                    ? `Nonce Synced (EP #${onchain.entryPointNonce ?? '0'})`
                    : `Nonce Drift (EP #${onchain.entryPointNonce ?? '?'} vs DB #${onchain.dbHighestNonce ?? '?'})`
                }
              />
            ) : null}
            {onchain.ethBalanceWei != null ? (
              <StatusPill
                tone={lowEthGas ? 'degraded' : 'live'}
                label={
                  lowEthGas
                    ? `Low ETH Gas · ${formatSponsoredEthWei(onchain.ethBalanceWei, 5)}`
                    : `ETH Gas · ${formatSponsoredEthWei(onchain.ethBalanceWei, 4)}`
                }
              />
            ) : null}
            {sponsorship ? (
              <StatusPill
                tone={
                  !sponsorship.enabled
                    ? 'blocked'
                    : (sponsorship.hourlyUtilizationBps ?? 0) >= 9000
                      ? 'degraded'
                      : 'live'
                }
                label={
                  !sponsorship.enabled
                    ? 'Sponsorship Disabled'
                    : `Hourly Cap ${formatBpsPercent(sponsorship.hourlyUtilizationBps)}`
                }
              />
            ) : null}
            {onchain.codeHash ? (
              <span
                className="inline-flex items-center border border-brand-border/30 bg-app-bg px-2 py-0.5 font-mono text-[11px] text-content-tertiary"
                title={onchain.codeHash}
              >
                CodeHash {shortHash(onchain.codeHash)}
              </span>
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
            title="Per-account Kernel telemetry awaiting backend deploy"
            message="Deploy the backend serving GET /api/insights/v1/trading-accounts/:address to inspect individual ZeroDev Kernel v4 smart wallets."
          />
        </Panel>
      ) : !data && query.isError ? (
        <ErrorState
          title="Unable to load Kernel smart account telemetry"
          message={query.error instanceof Error ? query.error.message : 'Unexpected error'}
          onRetry={() => void query.refetch()}
        />
      ) : data && !data.configured ? (
        <Panel>
          <EmptyState
            title="ZeroDev Kernel v4 gateway not active on this environment"
            message="Once the Kernel v4 gateway is configured on this environment, on-chain bytecode verification, sponsorship quota usage, nonce lanes, and operation history for this address will appear here."
          />
        </Panel>
      ) : data && onchain && summary ? (
        <>
          {!data.found ? (
            <Panel className="border-brand-yellow/40 bg-brand-yellow/5 p-4">
              <p className="font-mono text-xs text-brand-yellow">
                No Kernel v4 preparations or UserOperations recorded for this address yet. On-chain
                EntryPoint v0.9 and ETH balance state is shown below.
              </p>
            </Panel>
          ) : null}

          {/* 4-Card KPI Strip */}
          <section
            aria-label="Kernel smart account detail KPIs"
            className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4"
          >
            <StatCard
              label="Smart Account ETH Gas"
              value={
                onchain.ethBalanceWei != null
                  ? formatSponsoredEthWei(onchain.ethBalanceWei, 4)
                  : '—'
              }
              tone={lowEthGas ? 'negative' : 'neutral'}
              detail={`EP v0.9 nonce ${onchain.entryPointNonce ?? '—'} · DB max ${onchain.dbHighestNonce ?? '—'}`}
            />
            <StatCard
              label="Hourly Sponsorship Quota"
              value={
                sponsorship
                  ? formatBpsPercent(sponsorship.hourlyUtilizationBps)
                  : 'Not configured'
              }
              tone={
                sponsorship && (sponsorship.hourlyUtilizationBps ?? 0) >= 9000
                  ? 'negative'
                  : 'neutral'
              }
              detail={
                sponsorship
                  ? `${formatSponsoredEthWei(sponsorship.hourlyUsedWei, 4)} / ${formatSponsoredEthWei(sponsorship.hourlyCapWei, 4)} · ${String(sponsorship.activeReservedCount)} active`
                  : 'ETH-paid gas mode only'
              }
            />
            <StatCard
              label="Gas Mode Split"
              value={
                totalModeOps > 0
                  ? `${sponsoredSharePct.toFixed(0)}% / ${ethSharePct.toFixed(0)}%`
                  : '—'
              }
              detail={`Sponsored ${summary.sponsoredCount.toLocaleString('en-US')} · ETH-paid ${summary.ethCount.toLocaleString('en-US')} · ${formatSponsoredEthWei(summary.totalGasCostWei, 4)}`}
            />
            <StatCard
              label="Execution & Recovery"
              value={formatBpsPercent(summary.successRateBps)}
              tone={
                summary.successRateBps != null && summary.successRateBps >= 9500
                  ? 'positive'
                  : summary.successRateBps != null && summary.successRateBps < 9000
                    ? 'negative'
                    : 'neutral'
              }
              detail={`${summary.includedSucceeded.toLocaleString('en-US')} ok · ${summary.includedReverted.toLocaleString('en-US')} rev · ${summary.cancellationsAccepted.toLocaleString('en-US')} ETH cancels`}
            />
          </section>

          {/* Active Pending Nonce Lanes */}
          <section aria-labelledby="pending-lanes-title" className="space-y-3">
            <div className="flex flex-wrap items-baseline justify-between gap-2">
              <h2 id="pending-lanes-title" className="text-lg font-semibold">
                Active Pending Nonce Lanes
              </h2>
              <span className="font-mono text-xs text-content-tertiary">
                {data.pendingOperations.length === 0
                  ? 'All nonce lanes clear'
                  : `${String(data.pendingOperations.length)} pending${summary.oldestPendingAgeSeconds != null ? ` · oldest ${formatDurationSeconds(summary.oldestPendingAgeSeconds)}` : ''}`}
              </span>
            </div>

            {data.pendingOperations.length > 0 ? (
              <Panel className="border-brand-yellow/40">
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[740px] text-left text-sm">
                    <caption className="sr-only">
                      Currently pending Kernel UserOperations and nonce lane state
                    </caption>
                    <thead className="border-b border-brand-border/25 text-xs uppercase tracking-[0.08em] text-content-tertiary">
                      <tr>
                        <th scope="col" className="py-3 pl-4 pr-2.5">UserOp</th>
                        <th scope="col" className="px-2.5 py-3 text-right">Nonce</th>
                        <th scope="col" className="px-2.5 py-3">Mode</th>
                        <th scope="col" className="px-2.5 py-3 text-right">Age</th>
                        <th scope="col" className="px-2.5 py-3">Safe → Scan Block</th>
                        <th scope="col" className="px-2.5 py-3 text-right">Reserved Max</th>
                        <th scope="col" className="px-2.5 py-3">Cancel State</th>
                        <th scope="col" className="py-3 pl-2.5 pr-4 text-right">Valid Until</th>
                      </tr>
                    </thead>
                    <tbody className="divide-y divide-brand-border/15 font-mono text-xs">
                      {data.pendingOperations.map((p) => (
                        <tr key={p.userOpHash} className="bg-brand-yellow/[0.03]">
                          <td className="whitespace-nowrap py-2.5 pl-4 pr-2.5">
                            <div className="inline-flex items-center gap-1.5">
                              <span title={p.userOpHash} className="text-content-primary">
                                {shortAddress(p.userOpHash)}
                              </span>
                              <CopyButton value={p.userOpHash} />
                            </div>
                          </td>
                          <td className="whitespace-nowrap px-2.5 py-2.5 text-right tabular-nums text-content-primary">
                            {p.nonce}
                          </td>
                          <td className="whitespace-nowrap px-2.5 py-2.5">
                            <span
                              className={`border px-1.5 py-0.5 text-[10px] font-semibold uppercase ${
                                p.cancellationOnly
                                  ? 'border-brand-yellow/40 bg-brand-yellow/10 text-brand-yellow'
                                  : p.mode === 'sponsored'
                                    ? 'border-positive/40 bg-positive/10 text-positive'
                                    : 'border-brand-peach/40 bg-brand-peach/10 text-brand-peach'
                              }`}
                            >
                              {p.cancellationOnly
                                ? 'ETH Cancel'
                                : p.mode === 'sponsored'
                                  ? 'Sponsored'
                                  : 'ETH-Paid'}
                            </span>
                          </td>
                          <td className="whitespace-nowrap px-2.5 py-2.5 text-right tabular-nums text-brand-yellow">
                            {formatDurationSeconds(p.ageSeconds)}
                          </td>
                          <td className="whitespace-nowrap px-2.5 py-2.5 tabular-nums text-content-secondary">
                            {p.preparedSafeBlock ?? '—'} → {p.nonceScanThroughBlock ?? '—'}
                          </td>
                          <td className="whitespace-nowrap px-2.5 py-2.5 text-right tabular-nums text-content-secondary">
                            {p.reservedMaxCostWei
                              ? formatSponsoredEthWei(p.reservedMaxCostWei, 5)
                              : '—'}
                          </td>
                          <td className="whitespace-nowrap px-2.5 py-2.5">
                            {p.cancellationHash ? (
                              <span className="border border-brand-peach/40 bg-brand-peach/10 px-1.5 py-0.5 text-[10px] font-semibold uppercase text-brand-peach">
                                Accepted
                              </span>
                            ) : p.cancellationPrepared ? (
                              <span className="border border-brand-yellow/40 bg-brand-yellow/10 px-1.5 py-0.5 text-[10px] font-semibold uppercase text-brand-yellow">
                                Prepared
                              </span>
                            ) : (
                              <span className="text-content-tertiary">None</span>
                            )}
                          </td>
                          <td className="whitespace-nowrap py-2.5 pl-2.5 pr-4 text-right tabular-nums text-content-tertiary">
                            {p.sponsorshipValidUntil ? formatUtc(p.sponsorshipValidUntil) : '—'}
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              </Panel>
            ) : (
              <Panel className="px-4 py-3">
                <p className="font-mono text-xs text-content-secondary">
                  No pending Kernel UserOperations — all EntryPoint v0.9 nonce lanes are settled or
                  clear.
                </p>
              </Panel>
            )}
          </section>

          {/* Per-Account Sponsorship Quota & Gas Mode Breakdown */}
          {sponsorship ? (
            <section aria-labelledby="sponsorship-quota-title" className="space-y-3">
              <div className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 id="sponsorship-quota-title" className="text-lg font-semibold">
                  Per-Account Sponsorship Quota &amp; Gas Spend
                </h2>
                <span className="font-mono text-xs text-content-tertiary">
                  Max cost / op: {formatSponsoredEthWei(sponsorship.maxCostPerOpWei, 4)} · Sponsored
                  gas: {formatSponsoredEthWei(summary.sponsoredGasCostWei, 4)} · Self-paid ETH:{' '}
                  {formatSponsoredEthWei(summary.ethGasCostWei, 4)}
                </span>
              </div>

              <Panel className="grid gap-4 p-5 md:grid-cols-2">
                <div className="space-y-2">
                  <div className="flex items-center justify-between font-mono text-xs">
                    <span className="font-semibold text-content-primary">
                      Rolling 1h Sponsorship Cap
                    </span>
                    <span className="tabular-nums text-content-secondary">
                      {formatSponsoredEthWei(sponsorship.hourlyUsedWei, 5)} /{' '}
                      {formatSponsoredEthWei(sponsorship.hourlyCapWei, 4)} (
                      {formatBpsPercent(sponsorship.hourlyUtilizationBps)})
                    </span>
                  </div>
                  <div className="h-2.5 w-full overflow-hidden bg-app-bg">
                    <div
                      className={`h-full transition-all ${utilizationBarClass(sponsorship.hourlyUtilizationBps)}`}
                      style={{
                        width: `${Math.min(100, Math.max(0, (sponsorship.hourlyUtilizationBps ?? 0) / 100)).toFixed(2)}%`,
                      }}
                    />
                  </div>
                  <p className="text-[11px] text-content-tertiary">
                    Active reserved authorizations plus settled sponsored gas over the last hour.
                    Hitting 100% routes new trades to ETH-Paid mode.
                  </p>
                </div>

                <div className="space-y-2">
                  <div className="flex items-center justify-between font-mono text-xs">
                    <span className="font-semibold text-content-primary">
                      Concurrent Outstanding Cap
                    </span>
                    <span className="tabular-nums text-content-secondary">
                      {formatSponsoredEthWei(sponsorship.outstandingUsedWei, 5)} /{' '}
                      {formatSponsoredEthWei(sponsorship.outstandingCapWei, 4)} (
                      {formatBpsPercent(sponsorship.outstandingUtilizationBps)})
                    </span>
                  </div>
                  <div className="h-2.5 w-full overflow-hidden bg-app-bg">
                    <div
                      className={`h-full transition-all ${utilizationBarClass(sponsorship.outstandingUtilizationBps)}`}
                      style={{
                        width: `${Math.min(100, Math.max(0, (sponsorship.outstandingUtilizationBps ?? 0) / 100)).toFixed(2)}%`,
                      }}
                    />
                  </div>
                  <p className="text-[11px] text-content-tertiary">
                    {sponsorship.activeReservedCount} currently reserved/signed sponsorship
                    authorization{sponsorship.activeReservedCount === 1 ? '' : 's'} awaiting
                    settlement.
                  </p>
                </div>
              </Panel>
            </section>
          ) : null}

          {/* Per-Account Kernel Operation & Sponsorship Journal */}
          <section aria-labelledby="account-ops-title" className="space-y-3">
            <div className="flex flex-col justify-between gap-3 xl:flex-row xl:items-end">
              <div>
                <h2 id="account-ops-title" className="text-lg font-semibold">
                  Kernel Operation &amp; Sponsorship Ledger
                </h2>
                <p className="mt-0.5 text-xs text-content-tertiary">
                  Every recorded EntryPoint v0.9 UserOperation for this smart account across
                  Sponsored, ETH-Paid, and ETH nonce-cancellation modes.
                </p>
              </div>

              <div className="flex shrink-0 flex-wrap items-center gap-2">
                <div
                  className="flex items-center gap-1"
                  role="group"
                  aria-label="Filter account operation mode"
                >
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
                      className={`cursor-pointer whitespace-nowrap border px-2.5 py-1.5 text-xs font-semibold transition-colors ${
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
                  aria-label="Filter account operation status"
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
                  <table className="w-full min-w-[740px] text-left text-sm">
                    <caption className="sr-only">
                      Kernel v4 UserOperations for this smart account
                    </caption>
                    <thead className="border-b border-brand-border/25 text-xs uppercase tracking-[0.08em] text-content-tertiary">
                      <tr>
                        <th scope="col" className="py-3 pl-4 pr-2.5">UserOp</th>
                        <th scope="col" className="px-2.5 py-3 text-right">Nonce</th>
                        <th scope="col" className="px-2.5 py-3">Mode</th>
                        <th scope="col" className="px-2.5 py-3">Status</th>
                        <th scope="col" className="px-2.5 py-3 text-right">Gas (Actual / Max)</th>
                        <th scope="col" className="px-2.5 py-3">Tx</th>
                        <th scope="col" className="px-2.5 py-3">Cancel Hash</th>
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
                            <td className="whitespace-nowrap px-2.5 py-2.5 text-right tabular-nums text-content-secondary">
                              {op.nonce}
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
                            <td className="whitespace-nowrap px-2.5 py-2.5">
                              <span
                                className={`border px-1.5 py-0.5 text-[10px] font-semibold uppercase ${badge.className}`}
                              >
                                {badge.label}
                              </span>
                            </td>
                            <td className="whitespace-nowrap px-2.5 py-2.5 text-right tabular-nums">
                              <div className="text-content-primary">
                                {op.actualGasCostWei
                                  ? formatSponsoredEthWei(op.actualGasCostWei, 5)
                                  : '—'}
                              </div>
                              {op.reservedMaxCostWei ? (
                                <div className="text-[10px] text-content-tertiary">
                                  max {formatSponsoredEthWei(op.reservedMaxCostWei, 5)}
                                </div>
                              ) : null}
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
                            <td className="whitespace-nowrap px-2.5 py-2.5 text-content-tertiary">
                              {op.cancellationHash ? (
                                <div className="inline-flex items-center gap-1.5">
                                  <span title={op.cancellationHash} className="text-brand-yellow">
                                    {shortAddress(op.cancellationHash)}
                                  </span>
                                  <CopyButton value={op.cancellationHash} />
                                </div>
                              ) : (
                                '—'
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
                  message="Switch the payment mode or status filter to inspect other UserOperations for this smart account."
                />
              )}
            </Panel>
          </section>

          {/* Cancellations & Unsubmitted Preparations Audit Trail */}
          <section aria-label="Cancellations and unsubmitted preparations" className="grid gap-4 xl:grid-cols-2">
            <div className="space-y-2.5">
              <div className="flex items-baseline justify-between">
                <h2 className="text-base font-semibold">ETH Nonce Cancellations</h2>
                <span className="font-mono text-xs text-content-tertiary">
                  {summary.cancellationsAccepted} accepted / {summary.cancellationsPrepared} prepared
                </span>
              </div>
              <Panel>
                {data.cancellations.length > 0 ? (
                  <div className="overflow-x-auto">
                    <table className="w-full text-left text-xs">
                      <caption className="sr-only">
                        ETH nonce-cancellation recoveries for this smart account
                      </caption>
                      <thead className="border-b border-brand-border/25 uppercase tracking-[0.08em] text-content-tertiary">
                        <tr>
                          <th scope="col" className="py-2.5 pl-4 pr-2">Original Op</th>
                          <th scope="col" className="px-2 py-2.5 text-right">Nonce</th>
                          <th scope="col" className="px-2 py-2.5">State</th>
                          <th scope="col" className="py-2.5 pl-2 pr-4 text-right">Created</th>
                        </tr>
                      </thead>
                      <tbody className="divide-y divide-brand-border/15 font-mono">
                        {data.cancellations.map((c) => (
                          <tr key={c.cancellationPreparationId} className="hover:bg-brand-peach/[0.03]">
                            <td className="whitespace-nowrap py-2.5 pl-4 pr-2">
                              <div className="inline-flex items-center gap-1.5">
                                <span title={c.originalHash} className="text-content-primary">
                                  {shortAddress(c.originalHash)}
                                </span>
                                <CopyButton value={c.originalHash} />
                              </div>
                            </td>
                            <td className="whitespace-nowrap px-2 py-2.5 text-right tabular-nums text-content-secondary">
                              {c.nonce}
                            </td>
                            <td className="whitespace-nowrap px-2 py-2.5">
                              <span
                                className={`border px-1.5 py-0.5 text-[10px] font-semibold uppercase ${
                                  c.accepted
                                    ? 'border-positive/40 bg-positive/10 text-positive'
                                    : 'border-brand-yellow/40 bg-brand-yellow/10 text-brand-yellow'
                                }`}
                              >
                                {c.accepted ? 'Accepted' : 'Prepared'}
                              </span>
                            </td>
                            <td className="whitespace-nowrap py-2.5 pl-2 pr-4 text-right tabular-nums text-content-tertiary">
                              {formatUtc(c.createdAt)}
                            </td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                ) : (
                  <EmptyState
                    title="No ETH nonce cancellations"
                    message="This smart account has not required any ETH nonce-lane cancellations."
                  />
                )}
              </Panel>
            </div>

            <div className="space-y-2.5">
              <div className="flex items-baseline justify-between">
                <h2 className="text-base font-semibold">Unsubmitted Preparations</h2>
                <span className="font-mono text-xs text-content-tertiary">
                  {summary.unsubmittedPreparations} of {summary.totalPreparations} total preps
                </span>
              </div>
              <Panel>
                {data.unsubmittedPreparations.length > 0 ? (
                  <div className="overflow-x-auto">
                    <table className="w-full text-left text-xs">
                      <caption className="sr-only">
                        Prepared Kernel UserOperations that were never submitted
                      </caption>
                      <thead className="border-b border-brand-border/25 uppercase tracking-[0.08em] text-content-tertiary">
                        <tr>
                          <th scope="col" className="py-2.5 pl-4 pr-2">Prep ID</th>
                          <th scope="col" className="px-2 py-2.5">Mode</th>
                          <th scope="col" className="px-2 py-2.5 text-right">Nonce</th>
                          <th scope="col" className="px-2 py-2.5">State</th>
                          <th scope="col" className="py-2.5 pl-2 pr-4 text-right">Created</th>
                        </tr>
                      </thead>
                      <tbody className="divide-y divide-brand-border/15 font-mono">
                        {data.unsubmittedPreparations.map((p) => (
                          <tr key={p.preparationId} className="hover:bg-brand-peach/[0.03]">
                            <td className="whitespace-nowrap py-2.5 pl-4 pr-2 text-content-primary" title={p.preparationId}>
                              {p.preparationId.length > 16
                                ? `${p.preparationId.slice(0, 12)}…`
                                : p.preparationId}
                            </td>
                            <td className="whitespace-nowrap px-2 py-2.5">
                              <span
                                className={`border px-1.5 py-0.5 text-[10px] font-semibold uppercase ${
                                  p.mode === 'sponsored'
                                    ? 'border-positive/40 bg-positive/10 text-positive'
                                    : 'border-brand-peach/40 bg-brand-peach/10 text-brand-peach'
                                }`}
                              >
                                {p.mode === 'sponsored' ? 'Sponsored' : 'ETH-Paid'}
                              </span>
                            </td>
                            <td className="whitespace-nowrap px-2 py-2.5 text-right tabular-nums text-content-secondary">
                              {p.nonce}
                            </td>
                            <td className="whitespace-nowrap px-2 py-2.5">
                              <span
                                className={`border px-1.5 py-0.5 text-[10px] font-semibold uppercase ${
                                  p.expired
                                    ? 'border-brand-border/40 bg-app-bg text-content-tertiary'
                                    : 'border-brand-yellow/40 bg-brand-yellow/10 text-brand-yellow'
                                }`}
                              >
                                {p.expired ? 'Expired' : 'Open'}
                              </span>
                            </td>
                            <td className="whitespace-nowrap py-2.5 pl-2 pr-4 text-right tabular-nums text-content-tertiary">
                              {formatUtc(p.createdAt)}
                            </td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                ) : (
                  <EmptyState
                    title="No unsubmitted preparations"
                    message="Every prepared UserOperation for this account was signed and submitted."
                  />
                )}
              </Panel>
            </div>
          </section>
        </>
      ) : null}
    </div>
  )
}
