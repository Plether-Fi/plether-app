import { Link } from 'react-router-dom'
import { InsightsApiError } from '../api'
import { formatSponsoredEthWei } from '../api/sponsored'
import { useTreasuryOverview, type TreasuryWindowSummary } from '../api/treasury'
import { ArbiscanButton, CopyButton } from '../components/ActionButtons'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { StatCard, type StatTone } from '../components/StatCard'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { TreasuryRevenueChart } from '../components/TreasuryRevenueChart'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { arbiscanLinkKey, resolvePerpsChain } from '../utils/chain'
import { formatAssets, formatCompactAssets, formatUtc, shortAddress } from '../utils/format'

function shortTx(hash: string): string {
  return hash.length > 14 ? `${hash.slice(0, 8)}…${hash.slice(-6)}` : hash
}

function formatBpsRate(bps: number | null): string {
  if (bps === null) return 'Unavailable'
  return `${bps.toFixed(bps % 1 === 0 ? 0 : 2)} bps (${(bps / 100).toFixed(2)}%)`
}

function movementKindBadge(kind: string): { label: string; tone: StatusTone } {
  switch (kind) {
    case 'clearinghouse_withdraw':
      return { label: 'Clearinghouse withdraw', tone: 'degraded' }
    case 'clearinghouse_deposit':
      return { label: 'Clearinghouse deposit', tone: 'live' }
    case 'usdg_transfer_in':
      return { label: 'USDG in', tone: 'live' }
    case 'usdg_transfer_out':
      return { label: 'USDG out', tone: 'degraded' }
    default:
      return { label: kind, tone: 'unknown' }
  }
}

function ProfitLedgerRow({ label, row }: { label: string; row: TreasuryWindowSummary }) {
  const surplusBig = BigInt(row.netUsdgSurplusAssets || '0')
  return (
    <tr className="border-b border-brand-border/15 last:border-0">
      <td className="whitespace-nowrap px-2.5 py-2.5 font-mono text-xs font-semibold text-content-primary">{label}</td>
      <td className="whitespace-nowrap px-2.5 py-2.5 font-mono text-xs tabular-nums text-content-secondary">
        {row.executedOrders.toLocaleString('en-US')}
      </td>
      <td className="whitespace-nowrap px-2.5 py-2.5 font-mono text-xs tabular-nums text-content-secondary">
        {formatCompactAssets(row.volumeAssets)}
      </td>
      <td className="whitespace-nowrap px-2.5 py-2.5 font-mono text-xs font-semibold tabular-nums text-positive">
        {formatAssets(row.executionFeeAssessedAssets)}
      </td>
      <td className="whitespace-nowrap px-2.5 py-2.5 font-mono text-xs tabular-nums text-content-secondary">
        {row.effectiveFeeBps !== null ? `${row.effectiveFeeBps.toFixed(2)} bps` : '—'}
      </td>
      <td className="whitespace-nowrap px-2.5 py-2.5 font-mono text-xs tabular-nums text-content-secondary">
        {formatAssets(row.closeAssistanceUsdgAssets)}
      </td>
      <td
        className={`whitespace-nowrap px-2.5 py-2.5 font-mono text-xs font-semibold tabular-nums ${
          surplusBig > 0n
            ? 'text-positive'
            : surplusBig < 0n
              ? 'text-brand-orange'
              : 'text-content-secondary'
        }`}
      >
        {formatAssets(row.netUsdgSurplusAssets)}
      </td>
      <td className="whitespace-nowrap px-2.5 py-2.5 text-right font-mono text-xs tabular-nums text-content-secondary">
        {formatSponsoredEthWei(row.sponsoredGasSpentWei, 5)}
      </td>
    </tr>
  )
}

function HoldingRow({
  label,
  value,
  sublabel,
  pct,
}: {
  label: string
  value: string
  sublabel?: string
  pct?: number | null
}) {
  return (
    <div className="space-y-1.5 border border-brand-border/20 bg-app-bg p-3">
      <div className="flex items-baseline justify-between gap-2">
        <span className="text-xs font-medium text-content-primary">{label}</span>
        <span className="font-mono text-xs font-semibold tabular-nums text-content-primary">
          {value}
        </span>
      </div>
      {pct != null ? (
        <div className="h-1.5 w-full overflow-hidden bg-surface-panel">
          <div
            className="h-full bg-brand-orange transition-all"
            style={{ width: `${String(Math.min(100, Math.max(pct > 0 ? 1.5 : 0, pct)))}%` }}
          />
        </div>
      ) : null}
      {sublabel ? (
        <p className="font-mono text-[11px] text-content-tertiary">{sublabel}</p>
      ) : null}
    </div>
  )
}

function sharePct(partRaw: string | null, totalRaw: string | null): number | null {
  if (!partRaw || !totalRaw) return null
  const part = Number(partRaw)
  const total = Number(totalRaw)
  if (!Number.isFinite(part) || !Number.isFinite(total) || total <= 0) return null
  return (part / total) * 100
}

export function TreasuryPage() {
  const ctx = useProtocolContext()
  const query = useTreasuryOverview()
  const data = query.data
  const chain = resolvePerpsChain(data?.network.chainId ?? ctx.chain.chainId)

  const isEndpointMissing =
    query.isError
    && query.error instanceof InsightsApiError
    && query.error.status === 404

  const banner: { tone: StatusTone; label: string; note: string } = data
    ? data.rpcAvailable
      ? {
          tone: 'live',
          label: 'Treasury verified on-chain',
          note: data.treasury.address
            ? `Live CfdEngine.protocolTreasury() ${shortAddress(data.treasury.address)} · Clearinghouse, ERC-20, and LP vault balances reconciled`
            : 'Protocol fee ledger and on-chain policy live',
        }
      : {
          tone: 'degraded',
          label: 'RPC degraded · Indexed fee ledger',
          note: 'Live on-chain wallet balances unavailable; displaying verified PostgreSQL revenue & subsidy ledger',
        }
    : query.isLoading
      ? {
          tone: 'unknown',
          label: 'Reading protocol treasury…',
          note: 'Loading on-chain treasury balances, fee policy, and historical protocol revenue',
        }
      : isEndpointMissing
        ? {
            tone: 'unknown',
            label: 'Awaiting treasury backend deploy',
            note: 'Connected backend does not serve /api/insights/v1/treasury/overview yet',
          }
        : {
            tone: 'blocked',
            label: 'Treasury telemetry unavailable',
            note: 'Could not read protocol treasury balances or fee ledger',
          }

  const allTime = data?.profitOverview.allTime
  const win24 = data?.profitOverview.window24h
  const win7 = data?.profitOverview.window7d
  const win30 = data?.profitOverview.window30d

  const netSurplusTone: StatTone = allTime
    ? BigInt(allTime.netUsdgSurplusAssets || '0') > 0n
      ? 'positive'
      : BigInt(allTime.netUsdgSurplusAssets || '0') < 0n
        ? 'negative'
        : 'neutral'
    : 'neutral'

  const seenExternalKeys = new Set<string>()
  function claimAddressArbiscan(address: string | null | undefined): boolean {
    if (!address) return false
    const key = arbiscanLinkKey('address', address, chain.chainId)
    if (seenExternalKeys.has(key)) return false
    seenExternalKeys.add(key)
    return true
  }
  const showTreasuryArbiscan = claimAddressArbiscan(data?.treasury.address)
  const showPendingRotationArbiscan = claimAddressArbiscan(data?.treasury.pendingRotation?.treasury)
  const showMaintenanceRecipientArbiscan = claimAddressArbiscan(
    data?.feePolicy.juniorMaintenanceFeeRecipient,
  )

  return (
    <div className="space-y-6 sm:space-y-8">
      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={data?.network.releaseVersion ?? ctx.releaseVersion}
        blockNumber={data?.blockNumber ?? ctx.blockNumber}
        observedAt={data?.observedAt ?? ctx.observedAt}
        tone={banner.tone}
        statusLabel={banner.label}
        note={banner.note}
      />

      <header className="flex flex-wrap items-end justify-between gap-4">
        <div className="max-w-3xl space-y-2.5">
          <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">
            Protocol Treasury &amp; Profit Overview
          </p>
          <h1 className="text-3xl font-semibold tracking-tight sm:text-4xl">
            Treasury Wallet &amp; Revenue Ledger
          </h1>
          <p className="text-sm leading-6 text-content-secondary">
            Live balance sheet for <code className="font-mono text-xs">CfdEngine.protocolTreasury()</code>,
            cumulative protocol execution fees, LP maintenance fee policy, gas sponsorship &amp;
            close-assistance subsidies, and verifiable treasury cash flows.
          </p>
        </div>

        {data?.treasury.address ? (
          <div className="border border-brand-border/30 bg-surface-panel px-4 py-3 text-xs">
            <div className="flex items-center justify-between gap-4">
              <span className="text-content-tertiary">Protocol Treasury</span>
              <StatusPill
                tone={data.treasury.addressSource === 'onchain' ? 'live' : 'unknown'}
                label={data.treasury.addressSource === 'onchain' ? 'On-chain' : 'Manifest'}
              />
            </div>
            <div className="mt-1.5 flex flex-wrap items-center gap-2 font-mono">
              <span className="text-content-primary" title={data.treasury.address}>
                {shortAddress(data.treasury.address)}
              </span>
              <CopyButton value={data.treasury.address} />
              {showTreasuryArbiscan ? (
                <ArbiscanButton
                  kind="address"
                  value={data.treasury.address}
                  chainId={chain.chainId}
                />
              ) : null}
              <Link
                to={`/wallets/${data.treasury.address}`}
                className="ml-1 text-content-secondary hover:text-content-primary hover:underline"
              >
                Wallet explorer →
              </Link>
            </div>
            <p className="mt-1 font-mono text-[11px] text-content-tertiary">
              {data.treasury.isLocked && data.treasury.lockedUntil
                ? `Governance locked until ${formatUtc(data.treasury.lockedUntil)}`
                : 'No active governance treasury lock'}
            </p>
          </div>
        ) : null}
      </header>

      {data?.treasury.pendingRotation ? (
        <Panel className="border-brand-yellow/40 bg-brand-yellow/10 p-4">
          <div className="flex flex-wrap items-center justify-between gap-3">
            <div className="space-y-1">
              <p className="text-xs font-semibold uppercase tracking-wider text-brand-yellow">
                Pending Treasury Rotation Proposed
              </p>
              <div className="flex flex-wrap items-center gap-1.5 font-mono text-xs text-content-primary">
                <span>Next treasury: {shortAddress(data.treasury.pendingRotation.treasury)}</span>
                <CopyButton value={data.treasury.pendingRotation.treasury} />
                {showPendingRotationArbiscan ? (
                  <ArbiscanButton
                    kind="address"
                    value={data.treasury.pendingRotation.treasury}
                    chainId={chain.chainId}
                  />
                ) : null}
                <span>· Activates {formatUtc(data.treasury.pendingRotation.activationTime)}</span>
              </div>
            </div>
            <StatusPill tone="degraded" label="Timelock active" />
          </div>
        </Panel>
      ) : null}

      <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
        <StatCard
          label="Total treasury holdings"
          loading={query.isLoading}
          unavailable={!data || data.holdings.totalUsdgAssets === null}
          tone="positive"
          value={
            data?.holdings.totalUsdgAssets != null
              ? formatAssets(data.holdings.totalUsdgAssets)
              : undefined
          }
          detail={
            data?.holdings.clearinghouseSettlementAssets != null
              ? `Clearinghouse ${formatAssets(data.holdings.clearinghouseSettlementAssets)} · Wallet ${formatAssets(data.holdings.walletUsdgAssets)}`
              : 'Clearinghouse + Wallet USDG + LP Vaults'
          }
        />

        <StatCard
          label="All-time protocol fees"
          loading={query.isLoading}
          unavailable={!allTime}
          tone="positive"
          value={allTime ? formatAssets(allTime.executionFeeAssessedAssets) : undefined}
          detail={
            allTime
              ? `${allTime.executedOrders.toLocaleString('en-US')} fills · ${formatAssets(allTime.volumeAssets)} vol · ${
                  allTime.effectiveFeeBps !== null ? `${allTime.effectiveFeeBps.toFixed(2)} bps eff.` : '—'
                }`
              : 'Cumulative execution fees assessed'
          }
        />

        <StatCard
          label="7d annualized run-rate"
          loading={query.isLoading}
          unavailable={!data || data.profitOverview.annualizedExecutionFeeRunRate7dAssets === null}
          tone="neutral"
          value={
            data?.profitOverview.annualizedExecutionFeeRunRate7dAssets != null
              ? formatAssets(data.profitOverview.annualizedExecutionFeeRunRate7dAssets)
              : undefined
          }
          detail={
            win24 && win7
              ? `24h: ${formatAssets(win24.executionFeeAssessedAssets)} · 7d: ${formatAssets(win7.executionFeeAssessedAssets)}`
              : 'Extrapolated from trailing 7d execution fees'
          }
        />

        <StatCard
          label="Net USDG surplus (all-time)"
          loading={query.isLoading}
          unavailable={!allTime}
          tone={netSurplusTone}
          value={allTime ? formatAssets(allTime.netUsdgSurplusAssets) : undefined}
          detail={
            allTime
              ? `Close assist ${formatAssets(allTime.closeAssistanceUsdgAssets)} · Gas sponsored ${formatSponsoredEthWei(allTime.sponsoredGasSpentWei, 4)}`
              : 'Execution fees minus USDG subsidies'
          }
        />
      </div>

      {query.isLoading ? (
        <Panel>
          <LoadingState rows={6} />
        </Panel>
      ) : isEndpointMissing ? (
        <Panel className="p-6">
          <h2 className="text-lg font-semibold">Treasury endpoint deployment pending</h2>
          <p className="mt-2 max-w-2xl text-sm leading-6 text-content-secondary">
            Connected backend does not serve{' '}
            <code className="font-mono text-xs">/api/insights/v1/treasury/overview</code> yet. Once
            deployed to {chain.name}, this page will surface live on-chain treasury holdings,
            protocol fee revenue curves, subsidy costs, and treasury wallet movements.
          </p>
          <div className="mt-4 flex flex-wrap items-center gap-4 text-sm font-semibold">
            <button
              type="button"
              onClick={() => { void query.refetch() }}
              className="border border-brand-border/40 px-3.5 py-1.5 text-content-primary hover:border-brand-peach hover:text-brand-peach"
            >
              Recheck endpoint
            </button>
            <Link to="/" className="text-brand-peach hover:underline">
              ← Back to Overview
            </Link>
          </div>
        </Panel>
      ) : query.isError || !data ? (
        <ErrorState
          title="Treasury telemetry unavailable"
          message="Could not fetch protocol treasury overview. Please try again in a moment."
          onRetry={() => { void query.refetch() }}
        />
      ) : (
        <>
          <Panel className="p-4 sm:p-6">
            <TreasuryRevenueChart series={data.dailySeries} />
          </Panel>

          <section aria-labelledby="profit-holdings-title" className="grid gap-4 lg:grid-cols-12">
            <Panel className="p-4 sm:p-6 lg:col-span-7">
              <div className="flex flex-wrap items-start justify-between gap-3">
                <div>
                  <h2 id="profit-holdings-title" className="text-base font-semibold">
                    Protocol profit &amp; subsidy ledger
                  </h2>
                  <p className="mt-0.5 text-xs text-content-tertiary">
                    Execution fee revenue vs. emergency close assistance (USDG) and AA gas sponsorship (ETH)
                  </p>
                </div>
                {allTime ? (
                  <span className="font-mono text-[11px] text-content-tertiary">
                    Direct CH credits: {formatAssets(allTime.executionFeeCreditedAssets)}
                  </span>
                ) : null}
              </div>

              <div className="mt-4 overflow-x-auto border border-brand-border/20 bg-app-bg">
                <table className="w-full text-left">
                  <caption className="sr-only">Protocol revenue and subsidy windows</caption>
                  <thead className="border-b border-brand-border/20 text-[11px] font-medium text-content-secondary">
                    <tr>
                      <th scope="col" className="whitespace-nowrap px-2.5 py-2">Window</th>
                      <th scope="col" className="whitespace-nowrap px-2.5 py-2">Fills</th>
                      <th scope="col" className="whitespace-nowrap px-2.5 py-2">Volume</th>
                      <th scope="col" className="whitespace-nowrap px-2.5 py-2">Exec Fees</th>
                      <th scope="col" className="whitespace-nowrap px-2.5 py-2">Rate</th>
                      <th scope="col" className="whitespace-nowrap px-2.5 py-2">Subsidies</th>
                      <th scope="col" className="whitespace-nowrap px-2.5 py-2">Net USDG</th>
                      <th scope="col" className="whitespace-nowrap px-2.5 py-2 text-right">Gas Paid</th>
                    </tr>
                  </thead>
                  <tbody>
                    {win24 ? <ProfitLedgerRow label="24h" row={win24} /> : null}
                    {win7 ? <ProfitLedgerRow label="7d" row={win7} /> : null}
                    {win30 ? <ProfitLedgerRow label="30d" row={win30} /> : null}
                    {allTime ? <ProfitLedgerRow label="All-time" row={allTime} /> : null}
                  </tbody>
                </table>
              </div>

              <div className="mt-4 space-y-1.5 border border-brand-border/20 bg-app-bg p-3.5 text-xs leading-5 text-content-secondary">
                <p>
                  <strong className="text-content-primary">Accounting basis:</strong> Execution fees
                  are assessed on every executed open and close in{' '}
                  <code className="font-mono text-[11px]">OrderFinalized</code> receipts and accrue
                  to the treasury inside <code className="font-mono text-[11px]">MarginClearinghouse</code>{' '}
                  (or via HousePool top-up on negative-VPI rebates).
                </p>
                <p>
                  <strong className="text-content-primary">Honest cost separation:</strong>{' '}
                  Emergency close-assistance grants are USDG-denominated and subtracted directly into{' '}
                  <em>Net USDG</em>. Paymaster gas sponsorship is reported natively in{' '}
                  <code className="font-mono text-[11px]">ETH</code> without a synthetic ETH/USDG
                  rate. Permissionless keeper bounties (
                  {allTime ? formatAssets(allTime.keeperBountyAssets) : '—'} execution /{' '}
                  {allTime ? formatAssets(allTime.liquidationKeeperBountyAssets) : '—'} liquidation)
                  are funded by traders and liquidated collateral, not the treasury.
                </p>
              </div>
            </Panel>

            <Panel className="space-y-4 p-5 sm:p-6 lg:col-span-5">
              <div>
                <h2 className="text-base font-semibold">Live treasury balance sheet</h2>
                <p className="mt-0.5 text-xs text-content-tertiary">
                  Real-time on-chain positions held by the active protocol treasury
                </p>
              </div>

              <div className="space-y-2.5">
                <HoldingRow
                  label="MarginClearinghouse balance"
                  value={
                    data.holdings.clearinghouseSettlementAssets != null
                      ? formatAssets(data.holdings.clearinghouseSettlementAssets)
                      : 'Unavailable'
                  }
                  pct={sharePct(data.holdings.clearinghouseSettlementAssets, data.holdings.totalUsdgAssets)}
                  sublabel={
                    data.holdings.clearinghouseFreeAssets != null
                      ? `Withdrawable free: ${formatAssets(data.holdings.clearinghouseFreeAssets)}${
                          data.holdings.clearinghouseClaimAssets && data.holdings.clearinghouseClaimAssets !== '0'
                            ? ` · Claim: ${formatAssets(data.holdings.clearinghouseClaimAssets)}`
                            : ''
                        }`
                      : 'Accrued protocol fees inside MarginClearinghouse'
                  }
                />

                <HoldingRow
                  label="Wallet liquid USDG"
                  value={
                    data.holdings.walletUsdgAssets != null
                      ? formatAssets(data.holdings.walletUsdgAssets)
                      : 'Unavailable'
                  }
                  pct={sharePct(data.holdings.walletUsdgAssets, data.holdings.totalUsdgAssets)}
                  sublabel="ERC-20 USDG held directly in the treasury EOA / multisig"
                />

                <HoldingRow
                  label="Junior Vault LP position"
                  value={
                    data.holdings.juniorVaultAssets != null
                      ? formatAssets(data.holdings.juniorVaultAssets)
                      : 'Unavailable'
                  }
                  pct={sharePct(data.holdings.juniorVaultAssets, data.holdings.totalUsdgAssets)}
                  sublabel={
                    data.holdings.pendingMaintenanceFeeAssets != null
                      ? `Pending unminted maintenance fee: ${formatAssets(data.holdings.pendingMaintenanceFeeAssets)}`
                      : 'Treasury-owned Junior tranche LP equity'
                  }
                />

                <HoldingRow
                  label="Senior Vault LP position"
                  value={
                    data.holdings.seniorVaultAssets != null
                      ? formatAssets(data.holdings.seniorVaultAssets)
                      : 'Unavailable'
                  }
                  pct={sharePct(data.holdings.seniorVaultAssets, data.holdings.totalUsdgAssets)}
                  sublabel="Treasury-owned Senior tranche LP equity"
                />

                <HoldingRow
                  label="Native ETH gas reserve"
                  value={
                    data.holdings.walletEthWei != null
                      ? formatSponsoredEthWei(data.holdings.walletEthWei, 4)
                      : 'Unavailable'
                  }
                  sublabel="Direct ETH balance on the treasury address"
                />
              </div>

              <div className="border-t border-brand-border/20 pt-4">
                <h3 className="text-xs font-semibold uppercase tracking-wider text-content-secondary">
                  Active On-Chain Fee Policy
                </h3>
                <dl className="mt-2.5 space-y-2 text-xs">
                  <div className="flex items-center justify-between gap-2">
                    <dt className="text-content-tertiary">Execution fee rate</dt>
                    <dd className="font-mono font-semibold text-content-primary">
                      {formatBpsRate(data.feePolicy.executionFeeBps)}
                    </dd>
                  </div>
                  <div className="flex items-center justify-between gap-2">
                    <dt className="text-content-tertiary">Liquidation treasury share</dt>
                    <dd className="font-mono text-content-primary">
                      {formatBpsRate(data.feePolicy.liquidationProtocolShareBps)}
                    </dd>
                  </div>
                  <div className="flex items-center justify-between gap-2">
                    <dt className="text-content-tertiary">Liquidation keeper share</dt>
                    <dd className="font-mono text-content-secondary">
                      {formatBpsRate(data.feePolicy.liquidationKeeperShareBps)}
                    </dd>
                  </div>
                  <div className="flex items-center justify-between gap-2">
                    <dt className="text-content-tertiary">Junior maintenance fee APR</dt>
                    <dd className="font-mono text-content-primary">
                      {formatBpsRate(data.feePolicy.juniorMaintenanceFeeAprBps)}
                    </dd>
                  </div>
                  {data.feePolicy.juniorMaintenanceFeeRecipient ? (
                    <div className="flex items-center justify-between gap-2">
                      <dt className="text-content-tertiary">Maintenance fee recipient</dt>
                      <dd className="inline-flex items-center gap-1.5 font-mono text-content-primary">
                        <span title={data.feePolicy.juniorMaintenanceFeeRecipient}>
                          {shortAddress(data.feePolicy.juniorMaintenanceFeeRecipient)}
                        </span>
                        <CopyButton value={data.feePolicy.juniorMaintenanceFeeRecipient} />
                        {showMaintenanceRecipientArbiscan ? (
                          <ArbiscanButton
                            kind="address"
                            value={data.feePolicy.juniorMaintenanceFeeRecipient}
                            chainId={chain.chainId}
                          />
                        ) : null}
                      </dd>
                    </div>
                  ) : null}
                </dl>
              </div>
            </Panel>
          </section>

          <section aria-labelledby="treasury-audit-title" className="grid gap-4 lg:grid-cols-2">
            <Panel className="p-5 sm:p-6">
              <div className="flex items-center justify-between gap-3">
                <div>
                  <h2 id="treasury-audit-title" className="text-base font-semibold">
                    Recent fee-generating fills
                  </h2>
                  <p className="mt-0.5 text-xs text-content-tertiary">
                    Latest executed orders contributing protocol execution fees
                  </p>
                </div>
                <Link to="/execution" className="text-xs font-semibold text-brand-peach hover:underline">
                  All executions →
                </Link>
              </div>

              {data.recentFeeOrders.length === 0 ? (
                <div className="mt-4">
                  <EmptyState
                    title="No fee-generating fills indexed yet"
                    message="Executed trades with non-zero executionFeeAssets will appear here."
                  />
                </div>
              ) : (
                <div className="mt-4 overflow-x-auto border border-brand-border/20 bg-app-bg">
                  <table className="w-full min-w-[520px] text-left">
                    <caption className="sr-only">Recent protocol fee-generating trades</caption>
                    <thead className="border-b border-brand-border/20 text-[11px] font-medium text-content-secondary">
                      <tr>
                        <th scope="col" className="px-3 py-2">Order</th>
                        <th scope="col" className="px-3 py-2">Trader</th>
                        <th scope="col" className="px-3 py-2">Volume</th>
                        <th scope="col" className="px-3 py-2">Fee</th>
                        <th scope="col" className="px-3 py-2 text-right">Tx</th>
                      </tr>
                    </thead>
                    <tbody>
                      {data.recentFeeOrders.map((row) => (
                        <tr
                          key={`${row.orderId}-${row.txHash}`}
                          className="border-b border-brand-border/15 last:border-0"
                        >
                          <td className="whitespace-nowrap px-2.5 py-2 font-mono text-xs">
                            <span className="font-semibold text-content-primary">#{row.orderId}</span>{' '}
                            <span
                              className={
                                row.side === 'Long'
                                  ? 'text-positive'
                                  : row.side === 'Short'
                                    ? 'text-brand-orange'
                                    : 'text-content-tertiary'
                              }
                            >
                              {row.side}
                            </span>
                          </td>
                          <td className="whitespace-nowrap px-2.5 py-2 font-mono text-xs">
                            <Link
                              to={`/wallets/${row.account}`}
                              className="text-content-secondary hover:text-brand-peach hover:underline"
                            >
                              {shortAddress(row.account)}
                            </Link>
                          </td>
                          <td className="whitespace-nowrap px-2.5 py-2 font-mono text-xs tabular-nums text-content-secondary">
                            {formatCompactAssets(row.volumeAssets)}
                          </td>
                          <td className="whitespace-nowrap px-2.5 py-2 font-mono text-xs font-semibold tabular-nums text-positive">
                            {formatAssets(row.executionFeeAssets)}
                          </td>
                          <td className="whitespace-nowrap px-2.5 py-2 text-right font-mono text-xs">
                            {row.txHash ? (
                              <Link
                                to={`/tx/${row.txHash}`}
                                className="text-brand-peach hover:underline"
                              >
                                {shortTx(row.txHash)}
                              </Link>
                            ) : (
                              '—'
                            )}
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              )}
            </Panel>

            <Panel className="p-5 sm:p-6">
              <div className="flex items-center justify-between gap-3">
                <div>
                  <h2 className="text-base font-semibold">Treasury wallet movements</h2>
                  <p className="mt-0.5 text-xs text-content-tertiary">
                    Clearinghouse withdrawals, deposits, and direct USDG transfers on the treasury wallet
                  </p>
                </div>
                {data.treasury.address ? (
                  <Link
                    to={`/wallets/${data.treasury.address}`}
                    className="text-xs font-semibold text-brand-peach hover:underline"
                  >
                    Full ledger →
                  </Link>
                ) : null}
              </div>

              {data.treasuryMovements.length === 0 ? (
                <div className="mt-4">
                  <EmptyState
                    title="No treasury withdrawals or transfers yet"
                    message="Protocol fees currently remain retained inside MarginClearinghouse. Any treasury withdrawal or direct USDG transfer will be logged here."
                  />
                </div>
              ) : (
                <div className="mt-4 overflow-x-auto border border-brand-border/20 bg-app-bg">
                  <table className="w-full min-w-[480px] text-left">
                    <caption className="sr-only">Treasury wallet transfers and withdrawals</caption>
                    <thead className="border-b border-brand-border/20 text-[11px] font-medium text-content-secondary">
                      <tr>
                        <th scope="col" className="px-3 py-2">Type</th>
                        <th scope="col" className="px-3 py-2">Amount</th>
                        <th scope="col" className="px-3 py-2">Time (UTC)</th>
                        <th scope="col" className="px-3 py-2 text-right">Tx</th>
                      </tr>
                    </thead>
                    <tbody>
                      {data.treasuryMovements.map((mov) => {
                        const badge = movementKindBadge(mov.kind)
                        return (
                          <tr
                            key={`${mov.txHash}-${mov.kind}-${mov.blockNumber}`}
                            className="border-b border-brand-border/15 last:border-0"
                          >
                            <td className="whitespace-nowrap px-2.5 py-2">
                              <StatusPill tone={badge.tone} label={badge.label} />
                            </td>
                            <td className="whitespace-nowrap px-2.5 py-2 font-mono text-xs font-semibold tabular-nums text-content-primary">
                              {formatAssets(mov.amountAssets)}
                            </td>
                            <td className="whitespace-nowrap px-2.5 py-2 font-mono text-xs text-content-secondary">
                              {formatUtc(mov.occurredAt)}
                            </td>
                            <td className="whitespace-nowrap px-2.5 py-2 text-right font-mono text-xs">
                              <Link
                                to={`/tx/${mov.txHash}`}
                                className="text-brand-peach hover:underline"
                              >
                                {shortTx(mov.txHash)}
                              </Link>
                            </td>
                          </tr>
                        )
                      })}
                    </tbody>
                  </table>
                </div>
              )}
            </Panel>
          </section>
        </>
      )}
    </div>
  )
}
