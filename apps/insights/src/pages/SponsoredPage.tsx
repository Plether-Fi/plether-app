import { useState } from 'react'
import { Link } from 'react-router-dom'
import {
  InsightsApiError,
  altoFundingPill,
  budgetGaugeTone,
  deriveSponsoredBanner,
  formatBpsPercent,
  formatDurationSeconds,
  formatSponsoredEthWei,
  formatRunwayHours,
  shortHash,
  useSponsoredOperations,
  useSponsoredStatus,
  type SponsoredBudgetGauge,
  type SponsoredOpStatusFilter,
  type SponsoredWindowMetrics,
} from '../api'
import { ArbiscanButton, CopyButton } from '../components/ActionButtons'
import { AddressAvatar } from '../components/AddressAvatar'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { arbiscanLinkKey, resolvePerpsChain } from '../utils/chain'
import { formatAssets, formatUtc, shortAddress } from '../utils/format'

const OP_FILTER_TABS: { id: SponsoredOpStatusFilter; label: string }[] = [
  { id: 'all', label: 'All' },
  { id: 'succeeded', label: 'Succeeded' },
  { id: 'reverted', label: 'Reverted' },
]

function barFillClass(tone: StatusTone): string {
  if (tone === 'blocked') return 'bg-brand-orange'
  if (tone === 'degraded' || tone === 'stale') return 'bg-brand-yellow'
  if (tone === 'live' || tone === 'ready') return 'bg-positive'
  return 'bg-content-tertiary'
}

function BudgetGaugeBar({
  label,
  gauge,
  subtitle,
}: {
  label: string
  gauge: SponsoredBudgetGauge
  subtitle?: string
}) {
  const tone = budgetGaugeTone(gauge.utilizationBps)
  const pct = gauge.utilizationBps != null ? Math.min(100, gauge.utilizationBps / 100) : 0

  return (
    <div className="space-y-2 border border-brand-border/20 bg-app-bg p-3.5">
      <div className="flex flex-wrap items-baseline justify-between gap-2">
        <div>
          <p className="text-xs font-semibold text-content-primary">{label}</p>
          {subtitle ? <p className="mt-0.5 text-[11px] text-content-tertiary">{subtitle}</p> : null}
        </div>
        <div className="text-right font-mono text-xs tabular-nums">
          <span className="font-semibold text-content-primary">{formatSponsoredEthWei(gauge.usedWei, 4)}</span>
          <span className="text-content-tertiary"> / {formatSponsoredEthWei(gauge.capWei, 4)}</span>
          <span className="ml-2 text-brand-peach">{formatBpsPercent(gauge.utilizationBps)}</span>
        </div>
      </div>
      <div
        className="h-2 w-full overflow-hidden bg-surface-panel"
        role="progressbar"
        aria-label={label}
        aria-valuenow={Math.round(pct)}
        aria-valuemin={0}
        aria-valuemax={100}
      >
        <div
          className={`h-full transition-all ${barFillClass(tone)}`}
          style={{ width: `${String(Math.max(pct > 0 ? 1.5 : 0, pct))}%` }}
        />
      </div>
    </div>
  )
}

function CohortRow({ label, metrics }: { label: string; metrics: SponsoredWindowMetrics }) {
  return (
    <tr className="border-b border-brand-border/15 last:border-0">
      <td className="px-3 py-2.5 font-mono text-xs font-semibold text-content-primary">{label}</td>
      <td className="px-3 py-2.5 font-mono text-xs tabular-nums text-content-primary">
        {metrics.totalOps.toLocaleString('en-US')}
      </td>
      <td className="px-3 py-2.5 font-mono text-xs tabular-nums">
        <span className="text-positive">{metrics.succeededOps.toLocaleString('en-US')}</span>
        <span className="text-content-tertiary"> / </span>
        <span className={metrics.revertedOps > 0 ? 'text-brand-orange' : 'text-content-tertiary'}>
          {metrics.revertedOps.toLocaleString('en-US')}
        </span>
      </td>
      <td className="px-3 py-2.5 font-mono text-xs font-semibold tabular-nums text-content-primary">
        {formatBpsPercent(metrics.successRateBps)}
      </td>
      <td className="px-3 py-2.5 font-mono text-xs tabular-nums text-content-secondary">
        {formatSponsoredEthWei(metrics.gasSpentWei, 5)}
      </td>
      <td className="px-3 py-2.5 text-right font-mono text-xs tabular-nums text-content-secondary">
        {formatBpsPercent(metrics.gasEfficiencyBps)}
      </td>
    </tr>
  )
}

export function SponsoredPage() {
  const ctx = useProtocolContext()
  const [opFilter, setOpFilter] = useState<SponsoredOpStatusFilter>('all')

  const statusQuery = useSponsoredStatus()
  const opsQuery = useSponsoredOperations(opFilter, 50)

  const data = statusQuery.data
  const chain = resolvePerpsChain(data?.network.chainId ?? ctx.chain.chainId)
  const isEndpointMissing =
    statusQuery.isError
    && statusQuery.error instanceof InsightsApiError
    && statusQuery.error.status === 404

  const banner = data
    ? deriveSponsoredBanner(data)
    : statusQuery.isLoading
      ? {
          tone: 'unknown' as StatusTone,
          label: 'Checking sponsorship health…',
          note: 'Reading Paymaster EntryPoint deposit, AA reconciler lag, and Alto bundler status',
        }
      : isEndpointMissing
        ? {
            tone: 'unknown' as StatusTone,
            label: 'Awaiting Stage 5 backend deploy',
            note: 'Connected backend does not serve /api/insights/v1/sponsored/* yet',
          }
        : {
            tone: 'blocked' as StatusTone,
            label: 'Sponsorship telemetry unavailable',
            note: 'Could not read ERC-4337 paymaster or bundler status',
          }

  const gateCard = (() => {
    if (!data) return { label: undefined, tone: 'unknown' as StatusTone, detail: 'Paymaster & issuance kill-switch' }
    if (!data.configured) return { label: 'Not configured', tone: 'unknown' as StatusTone, detail: 'No active AA sponsorship stack' }
    if (data.gate.status === 'issuing') {
      return {
        label: data.gate.rolloutMode === 'canary' ? 'Issuing (Canary)' : 'Issuing',
        tone: 'live' as StatusTone,
        detail: `Rollout: ${data.gate.rolloutMode} · On-chain paymaster unpaused`,
      }
    }
    if (data.gate.status === 'paused') {
      return {
        label: 'Paused',
        tone: 'blocked' as StatusTone,
        detail: data.gate.paymasterOnchainPaused
          ? 'Paymaster contract paused on-chain'
          : data.gate.pausedReason ?? 'Issuance kill-switch engaged',
      }
    }
    if (data.gate.status === 'degraded') {
      return {
        label: 'Degraded',
        tone: 'degraded' as StatusTone,
        detail: !data.reconciler.fresh ? 'Reconciler lag exceeds safe window' : 'Bundler or control check degraded',
      }
    }
    return {
      label: 'Disabled',
      tone: 'unknown' as StatusTone,
      detail: 'Sponsorship feature flag turned off',
    }
  })()

  const altoBadge = data
    ? altoFundingPill(data.paymaster.altoFundingState, data.paymaster.altoFundingReason)
    : { tone: 'unknown' as StatusTone, label: 'Unverified' }

  const funnel = data?.execution.funnel24h
  const funnelTotal = funnel
    ? funnel.reserved + funnel.signed + funnel.submitted + funnel.settled + funnel.expired + funnel.cancelled
    : 0

  const funnelSegments = funnel && funnelTotal > 0
    ? [
        { key: 'settled', label: 'Settled', count: funnel.settled, color: 'bg-positive' },
        {
          key: 'inflight',
          label: 'In-flight',
          count: funnel.reserved + funnel.signed + funnel.submitted,
          color: 'bg-brand-peach',
        },
        { key: 'expired', label: 'Expired', count: funnel.expired, color: 'bg-brand-yellow' },
        { key: 'cancelled', label: 'Cancelled', count: funnel.cancelled, color: 'bg-brand-orange' },
      ].filter((s) => s.count > 0)
    : []

  const operations = opsQuery.data?.operations ?? []
  const controlEvents = opsQuery.data?.controlEvents ?? []

  return (
    <div className="space-y-6 sm:space-y-8">
      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={data?.network.releaseVersion ?? ctx.releaseVersion}
        blockNumber={data?.reconciler.headBlock ?? ctx.blockNumber}
        observedAt={data?.observedAt ?? ctx.observedAt}
        tone={banner.tone}
        statusLabel={banner.label}
        note={banner.note}
      />

      <header className="flex flex-wrap items-end justify-between gap-4">
        <div className="max-w-3xl space-y-2.5">
          <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">
            ERC-4337 Gasless Operations
          </p>
          <h1 className="text-3xl font-semibold tracking-tight sm:text-4xl">
            Sponsored Transactions &amp; Paymaster Health
          </h1>
          <p className="text-sm leading-6 text-content-secondary">
            Live monitoring for every dependency keeping gasless trading operational: EntryPoint
            ETH deposit &amp; runway, issuance kill-switch, global budget caps, AA reconciler lag,
            Alto bundler readiness, and finalized UserOperations.
          </p>
        </div>

        {data?.configured && data.paymasterAddress && data.entryPointAddress ? (
          <div className="border border-brand-border/30 bg-surface-panel px-4 py-2.5 text-xs">
            <div className="flex items-center justify-between gap-4">
              <span className="text-content-tertiary">Paymaster</span>
              <span className="inline-flex items-center gap-1.5 font-mono text-content-primary">
                <span title={data.paymasterAddress}>{shortAddress(data.paymasterAddress)}</span>
                <CopyButton value={data.paymasterAddress} />
                <ArbiscanButton
                  kind="address"
                  value={data.paymasterAddress}
                  chainId={chain.chainId}
                />
              </span>
            </div>
            <div className="mt-1.5 flex items-center justify-between gap-4">
              <span className="text-content-tertiary">EntryPoint v{data.entryPointVersion ?? '0.8'}</span>
              <span className="inline-flex items-center gap-1.5 font-mono text-content-secondary">
                <span title={data.entryPointAddress}>{shortAddress(data.entryPointAddress)}</span>
                <CopyButton value={data.entryPointAddress} />
                {arbiscanLinkKey('address', data.entryPointAddress, chain.chainId) !==
                arbiscanLinkKey('address', data.paymasterAddress, chain.chainId) ? (
                  <ArbiscanButton
                    kind="address"
                    value={data.entryPointAddress}
                    chainId={chain.chainId}
                  />
                ) : null}
              </span>
            </div>
          </div>
        ) : null}
      </header>

      {/* Paymaster ETH Runway Fuel Gauge + Live Health Strip */}
      <Panel className="p-4 sm:p-6">
        {statusQuery.isLoading ? (
          <LoadingState rows={3} />
        ) : !data?.configured ? (
          <div className="flex flex-wrap items-center justify-between gap-2 font-mono text-xs text-content-tertiary">
            <span>Sponsorship gate: <strong className="text-content-secondary">{gateCard.label ?? 'Unavailable'}</strong></span>
            <span>{gateCard.detail}</span>
          </div>
        ) : (
          <div className="grid gap-5 lg:grid-cols-12 lg:items-center">
            {/* Left: EntryPoint Deposit Fuel Gauge & Runway */}
            <div className="space-y-3 lg:col-span-7 lg:border-r lg:border-brand-border/20 lg:pr-6">
              <div className="flex flex-wrap items-end justify-between gap-4">
                <div>
                  <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                    Paymaster EntryPoint Deposit
                  </p>
                  <p
                    className={`mt-1 font-mono text-3xl font-bold tracking-tight tabular-nums sm:text-4xl ${
                      data.paymaster.depositHealthy === false
                        ? 'text-brand-orange'
                        : data.paymaster.depositHealthy === true
                          ? 'text-positive'
                          : 'text-content-primary'
                    }`}
                  >
                    {data.paymaster.entryPointDepositWei != null
                      ? formatSponsoredEthWei(data.paymaster.entryPointDepositWei, 4)
                      : 'Unavailable'}
                  </p>
                </div>
                <div className="text-left font-mono sm:text-right">
                  <p className="text-xs text-content-tertiary">Estimated Runway</p>
                  <p className="mt-0.5 text-lg font-bold tabular-nums text-content-primary">
                    {formatRunwayHours(data.paymaster.estimatedRunwayHours)}
                  </p>
                </div>
              </div>

              {(() => {
                const dep = BigInt(data.paymaster.entryPointDepositWei ?? '0')
                const floor = BigInt(data.paymaster.minDepositWei ?? '0')
                const scale = floor > 0n ? (dep > floor * 10n ? dep : floor * 10n) : dep > 0n ? dep : 1n
                const fillPct = dep > 0n ? Math.min(100, Number((dep * 10000n) / scale) / 100) : 0
                const floorPct = floor > 0n ? Math.min(95, Math.max(4, Number((floor * 10000n) / scale) / 100)) : 0
                return (
                  <div className="space-y-1.5">
                    <div className="relative h-2.5 w-full overflow-hidden border border-brand-border/20 bg-app-bg">
                      <div
                        className={data.paymaster.depositHealthy === false ? 'h-full bg-brand-orange' : 'h-full bg-positive'}
                        style={{ width: `${String(fillPct)}%` }}
                      />
                      {floorPct > 0 ? (
                        <span
                          className="absolute inset-y-0 w-0.5 bg-brand-orange"
                          style={{ left: `${String(floorPct)}%` }}
                          title={`Minimum floor: ${formatSponsoredEthWei(data.paymaster.minDepositWei, 3)}`}
                        />
                      ) : null}
                    </div>
                    <div className="flex flex-wrap justify-between gap-2 font-mono text-[11px] tabular-nums text-content-tertiary">
                      <span>Floor: {formatSponsoredEthWei(data.paymaster.minDepositWei, 3)}</span>
                      <span>24h burn: {formatSponsoredEthWei(data.paymaster.burn24hWei, 4)}</span>
                    </div>
                  </div>
                )
              })()}
            </div>

            {/* Right: 24h UserOp Success + Gate / Alto / Reconciler Strip */}
            <div className="space-y-3 lg:col-span-5">
              <div className="flex flex-wrap items-baseline justify-between gap-2">
                <span className="text-xs text-content-tertiary">24h UserOp Success</span>
                <span
                  className={`font-mono text-2xl font-bold tabular-nums ${
                    data.execution.window24h.successRateBps != null
                      ? data.execution.window24h.successRateBps >= 9800
                        ? 'text-positive'
                        : data.execution.window24h.successRateBps >= 9000
                          ? 'text-brand-yellow'
                          : 'text-brand-orange'
                      : 'text-content-tertiary'
                  }`}
                >
                  {data.execution.window24h.totalOps > 0
                    ? formatBpsPercent(data.execution.window24h.successRateBps)
                    : '—'}
                </span>
              </div>
              <p className="font-mono text-xs tabular-nums text-content-tertiary">
                {data.execution.window24h.totalOps > 0
                  ? `${data.execution.window24h.succeededOps.toLocaleString('en-US')} ok · ${data.execution.window24h.revertedOps.toLocaleString('en-US')} reverted · p50 ${formatDurationSeconds(data.execution.latencyP50Seconds)}`
                  : 'No finalized sponsored UserOps in last 24h'}
              </p>
              <div className="flex flex-wrap items-center gap-2 border-t border-brand-border/15 pt-3 font-mono text-xs">
                <StatusPill tone={gateCard.tone} label={`Gate: ${gateCard.label ?? 'Unknown'}`} />
                <StatusPill
                  tone={data.bundler.reachable && data.bundler.entryPointSupported ? 'live' : 'blocked'}
                  label={data.bundler.reachable ? 'Alto OK' : 'Alto Down'}
                />
                <StatusPill
                  tone={data.reconciler.fresh ? 'live' : 'degraded'}
                  label={
                    data.reconciler.fresh
                      ? `Reconciler ${formatDurationSeconds(data.reconciler.lagSeconds)} lag`
                      : `Stale (${formatDurationSeconds(data.reconciler.lagSeconds)})`
                  }
                />
              </div>
            </div>
          </div>
        )}
      </Panel>

      {statusQuery.isLoading ? (
        <Panel>
          <LoadingState rows={6} />
        </Panel>
      ) : isEndpointMissing ? (
        <Panel className="p-6">
          <h2 className="text-lg font-semibold">Stage 5 backend deployment pending</h2>
          <p className="mt-2 max-w-2xl text-sm leading-6 text-content-secondary">
            Connected backend does not serve{' '}
            <code className="font-mono text-xs">/api/insights/v1/sponsored/status</code> yet. Once
            Stage 5 is deployed to {chain.name}, this page will surface live Paymaster EntryPoint
            deposits, budget utilization, AA reconciler lag, Alto bundler readiness, close-assistance
            health, and recent sponsored UserOperations.
          </p>
          <div className="mt-4 flex flex-wrap items-center gap-4 text-sm font-semibold">
            <button
              type="button"
              onClick={() => { void statusQuery.refetch() }}
              className="border border-brand-border/40 px-3.5 py-1.5 text-content-primary hover:border-brand-peach hover:text-brand-peach"
            >
              Recheck endpoint
            </button>
            <Link to="/" className="text-brand-peach hover:underline">
              ← Back to Overview
            </Link>
          </div>
        </Panel>
      ) : statusQuery.isError || !data ? (
        <ErrorState
          title="Sponsored telemetry unavailable"
          message="Could not fetch ERC-4337 sponsorship status. Please try again in a moment."
          onRetry={() => { void statusQuery.refetch() }}
        />
      ) : !data.configured ? (
        <Panel className="p-6">
          <h2 className="text-lg font-semibold">Sponsorship backend not configured</h2>
          <p className="mt-2 max-w-2xl text-sm leading-6 text-content-secondary">
            This backend deployment currently has neither Native AA (EntryPoint v0.8) nor Kernel AA
            (EntryPoint v0.9) paymaster sponsorship configured. Telemetry fails closed without
            synthetic placeholders.
          </p>
          <Link to="/protocol" className="mt-4 inline-block text-sm font-semibold text-brand-peach hover:underline">
            Inspect verified protocol contracts →
          </Link>
        </Panel>
      ) : (
        <>
          <section aria-labelledby="paymaster-budgets-title" className="grid gap-4 lg:grid-cols-2">
            <Panel className="flex flex-col justify-between p-5 sm:p-6">
              <div>
                <div className="flex items-start justify-between gap-3">
                  <div>
                    <h2 id="paymaster-budgets-title" className="text-base font-semibold">
                      Paymaster deposit &amp; ETH runway
                    </h2>
                    <p className="mt-0.5 text-xs text-content-tertiary">
                      On-chain EntryPoint balance vs. safety floor and 24h gas burn
                    </p>
                  </div>
                  <StatusPill
                    tone={
                      data.paymaster.depositHealthy === true
                        ? 'live'
                        : data.paymaster.depositHealthy === false
                          ? 'blocked'
                          : 'unknown'
                    }
                    label={
                      data.paymaster.depositHealthy === true
                        ? 'Funded'
                        : data.paymaster.depositHealthy === false
                          ? 'Low deposit'
                          : 'Unchecked'
                    }
                  />
                </div>

                <dl className="mt-4 divide-y divide-brand-border/15 text-xs">
                  <div className="flex items-center justify-between gap-4 py-2.5">
                    <dt className="text-content-secondary">Free headroom (deposit − reserved)</dt>
                    <dd className="font-mono font-semibold tabular-nums text-content-primary">
                      {formatSponsoredEthWei(data.paymaster.freeHeadroomWei, 5)}
                    </dd>
                  </div>
                  <div className="flex items-center justify-between gap-4 py-2.5">
                    <dt className="text-content-secondary">Reconciler blocks behind</dt>
                    <dd className="font-mono tabular-nums text-content-primary">
                      {data.reconciler.blocksBehind != null
                        ? `${data.reconciler.blocksBehind.toLocaleString('en-US')} blocks`
                        : '—'}
                    </dd>
                  </div>
                  <div className="flex items-center justify-between gap-4 py-2.5">
                    <dt className="text-content-secondary">Rollout gate policy</dt>
                    <dd className="font-mono text-content-secondary">{gateCard.detail}</dd>
                  </div>
                  <div className="flex items-center justify-between gap-4 py-2.5">
                    <dt className="text-content-secondary">Alto executor ETH funding</dt>
                    <dd>
                      <StatusPill tone={altoBadge.tone} label={altoBadge.label} />
                    </dd>
                  </div>
                </dl>
              </div>
            </Panel>

            <Panel className="p-5 sm:p-6">
              <div className="flex items-start justify-between gap-3">
                <div>
                  <h2 className="text-base font-semibold">Global budget utilization</h2>
                  <p className="mt-0.5 text-xs text-content-tertiary">
                    Hard caps preventing runaway gas drain or leaked reservations
                  </p>
                </div>
                {data.execution.activeUnfinalizedCount > 0 ? (
                  <StatusPill
                    tone={
                      !data.reconciler.fresh
                      || (data.execution.oldestUnfinalizedAgeSeconds ?? 0)
                        > Math.max(900, (data.reconciler.maxSafeLagSeconds ?? 600) + 300)
                        ? 'degraded'
                        : 'unknown'
                    }
                    label={`${String(data.execution.activeUnfinalizedCount)} active`}
                  />
                ) : (
                  <StatusPill tone="live" label="0 stuck" />
                )}
              </div>

              {data.budgets ? (
                <div className="mt-4 space-y-3">
                  <BudgetGaugeBar
                    label="Global outstanding reserved"
                    gauge={data.budgets.globalOutstanding}
                    subtitle={
                      data.execution.activeUnfinalizedCount > 0
                        ? `${String(data.execution.activeUnfinalizedCount)} unfinalized · oldest ${formatDurationSeconds(data.execution.oldestUnfinalizedAgeSeconds)}`
                        : 'No active unfinalized reservations'
                    }
                  />
                  <BudgetGaugeBar label="Global 1h spend" gauge={data.budgets.globalHourly} />
                  <BudgetGaugeBar label="Global 24h spend" gauge={data.budgets.globalDaily} />

                  <div className="flex flex-wrap items-center justify-between gap-2 pt-1 font-mono text-[11px] text-content-tertiary">
                    <span>Per-op max: {formatSponsoredEthWei(data.budgets.maxCostPerOpWei, 4)}</span>
                    <span>Account 1h cap: {formatSponsoredEthWei(data.budgets.accountHourlyCapWei, 4)}</span>
                  </div>
                </div>
              ) : (
                <EmptyState
                  title="Budget limits unavailable"
                  message="No global paymaster budget thresholds are exposed on this deployment."
                />
              )}
            </Panel>
          </section>

          <section aria-labelledby="pipeline-execution-title" className="grid gap-4 lg:grid-cols-2">
            <Panel className="p-5 sm:p-6">
              <h2 id="pipeline-execution-title" className="text-base font-semibold">
                Pipeline liveness &amp; close assistance
              </h2>
              <p className="mt-0.5 text-xs text-content-tertiary">
                Reconciler safe-block lag, Alto EntryPoint probe, and emergency close grants
              </p>

              <div className="mt-4 space-y-3">
                <div className="border border-brand-border/20 bg-app-bg p-3.5">
                  <div className="flex items-center justify-between gap-3">
                    <p className="text-xs font-semibold text-content-primary">AA Reconciler</p>
                    <StatusPill
                      tone={data.reconciler.fresh ? 'live' : 'blocked'}
                      label={data.reconciler.fresh ? 'Fresh' : 'Stale'}
                    />
                  </div>
                  <div className="mt-2 grid grid-cols-2 gap-2 font-mono text-xs">
                    <div>
                      <span className="text-content-tertiary">Safe block: </span>
                      <span className="text-content-primary">
                        {data.reconciler.safeBlock
                          ? Number(data.reconciler.safeBlock).toLocaleString('en-US')
                          : 'Unavailable'}
                      </span>
                    </div>
                    <div>
                      <span className="text-content-tertiary">Behind head: </span>
                      <span className="text-content-primary">
                        {data.reconciler.blocksBehind != null
                          ? `${data.reconciler.blocksBehind.toLocaleString('en-US')} blocks`
                          : 'Unavailable'}
                      </span>
                    </div>
                    <div>
                      <span className="text-content-tertiary">Heartbeat lag: </span>
                      <span className="text-content-primary">
                        {formatDurationSeconds(data.reconciler.lagSeconds)}
                        {data.reconciler.maxSafeLagSeconds != null
                          ? ` / ${formatDurationSeconds(data.reconciler.maxSafeLagSeconds)}`
                          : ''}
                      </span>
                    </div>
                    <div>
                      <span className="text-content-tertiary">Last success: </span>
                      <span className="text-content-secondary">
                        {data.reconciler.lastSuccessAt
                          ? formatUtc(data.reconciler.lastSuccessAt)
                          : 'Unavailable'}
                      </span>
                    </div>
                  </div>
                </div>

                <div className="border border-brand-border/20 bg-app-bg p-3.5">
                  <div className="flex items-center justify-between gap-3">
                    <p className="text-xs font-semibold text-content-primary">Alto Bundler</p>
                    <StatusPill
                      tone={
                        data.bundler.reachable && data.bundler.entryPointSupported
                          ? 'live'
                          : data.bundler.reachable
                            ? 'degraded'
                            : 'blocked'
                      }
                      label={
                        data.bundler.reachable && data.bundler.entryPointSupported
                          ? `EntryPoint v${data.entryPointVersion ?? '0.8'} Ready`
                          : data.bundler.reachable
                            ? 'Unsupported EntryPoint'
                            : 'Unreachable'
                      }
                    />
                  </div>
                  <p className="mt-1.5 text-xs text-content-secondary">
                    Live <code className="font-mono text-[11px]">eth_supportedEntryPoints</code>{' '}
                    probe against private bundler RPC.
                  </p>
                </div>

                <div className="border border-brand-border/20 bg-app-bg p-3.5">
                  <div className="flex items-center justify-between gap-3">
                    <p className="text-xs font-semibold text-content-primary">
                      Emergency Close Assistance
                    </p>
                    <StatusPill
                      tone={
                        !data.closeAssistance.available
                          ? 'unknown'
                          : data.closeAssistance.pendingUnverifiedCount > 0
                            ? 'degraded'
                            : 'live'
                      }
                      label={
                        !data.closeAssistance.available
                          ? 'Unavailable'
                          : data.closeAssistance.pendingUnverifiedCount > 0
                            ? `${String(data.closeAssistance.pendingUnverifiedCount)} pending`
                            : 'Clear'
                      }
                    />
                  </div>
                  {data.closeAssistance.available ? (
                    <div className="mt-2 grid grid-cols-2 gap-2 font-mono text-xs">
                      <div>
                        <span className="text-content-tertiary">Verified (24h / 7d): </span>
                        <span className="text-content-primary">
                          {data.closeAssistance.verifiedCount24h} /{' '}
                          {data.closeAssistance.verifiedCount7d}
                        </span>
                      </div>
                      <div>
                        <span className="text-content-tertiary">Assisted (7d): </span>
                        <span className="text-content-primary">
                          {formatAssets(data.closeAssistance.totalAssisted7dAssets)}
                        </span>
                      </div>
                      <div>
                        <span className="text-content-tertiary">Pending unverified: </span>
                        <span
                          className={
                            data.closeAssistance.pendingUnverifiedCount > 0
                              ? 'font-semibold text-brand-yellow'
                              : 'text-content-primary'
                          }
                        >
                          {data.closeAssistance.pendingUnverifiedCount}
                        </span>
                      </div>
                      <div>
                        <span className="text-content-tertiary">Verification rate: </span>
                        <span className="text-content-primary">
                          {formatBpsPercent(data.closeAssistance.verificationRateBps)}
                        </span>
                      </div>
                    </div>
                  ) : (
                    <p className="mt-1.5 text-xs text-content-tertiary">
                      Close-assistance telemetry table unavailable on this deployment.
                    </p>
                  )}
                </div>
              </div>
            </Panel>

            <Panel className="p-5 sm:p-6">
              <div className="flex items-start justify-between gap-3">
                <div>
                  <h2 className="text-base font-semibold">Execution quality &amp; 24h funnel</h2>
                  <p className="mt-0.5 text-xs text-content-tertiary">
                    UserOperation success rate, gas efficiency (actual / reserved), and latency
                  </p>
                </div>
                <div className="text-right font-mono text-xs">
                  <span className="text-content-tertiary">p50 / p95: </span>
                  <span className="font-semibold text-content-primary">
                    {formatDurationSeconds(data.execution.latencyP50Seconds)} /{' '}
                    {formatDurationSeconds(data.execution.latencyP95Seconds)}
                  </span>
                </div>
              </div>

              <div className="mt-4 overflow-x-auto border border-brand-border/20 bg-app-bg">
                <table className="w-full min-w-[440px] text-left">
                  <caption className="sr-only">Sponsored UserOperation cohorts</caption>
                  <thead className="border-b border-brand-border/20 text-[11px] font-medium text-content-secondary">
                    <tr>
                      <th scope="col" className="px-3 py-2">Window</th>
                      <th scope="col" className="px-3 py-2">Ops</th>
                      <th scope="col" className="px-3 py-2">Ok / Rev</th>
                      <th scope="col" className="px-3 py-2">Success</th>
                      <th scope="col" className="px-3 py-2">Gas spent</th>
                      <th scope="col" className="px-3 py-2 text-right">Eff.</th>
                    </tr>
                  </thead>
                  <tbody>
                    <CohortRow label="1h" metrics={data.execution.window1h} />
                    <CohortRow label="24h" metrics={data.execution.window24h} />
                    <CohortRow label="7d" metrics={data.execution.window7d} />
                  </tbody>
                </table>
              </div>

              <div className="mt-4 space-y-2.5 border border-brand-border/20 bg-app-bg p-3.5">
                <div className="flex items-center justify-between text-xs">
                  <span className="font-semibold text-content-primary">
                    24h authorization funnel
                  </span>
                  <span className="font-mono text-content-tertiary">
                    {funnelTotal.toLocaleString('en-US')} total
                  </span>
                </div>

                {funnelSegments.length > 0 ? (
                  <div className="flex h-2.5 w-full overflow-hidden bg-surface-panel">
                    {funnelSegments.map((seg) => (
                      <div
                        key={seg.key}
                        className={seg.color}
                        style={{ width: `${String((seg.count / funnelTotal) * 100)}%` }}
                        title={`${seg.label}: ${String(seg.count)}`}
                      />
                    ))}
                  </div>
                ) : (
                  <div className="h-2.5 w-full bg-surface-panel" />
                )}

                {funnel ? (
                  <div className="grid grid-cols-3 gap-2 pt-1 font-mono text-[11px] sm:grid-cols-6">
                    <div>
                      <span className="block text-content-tertiary">Settled</span>
                      <span className="font-semibold text-positive">{funnel.settled}</span>
                    </div>
                    <div>
                      <span className="block text-content-tertiary">Reserved</span>
                      <span className="text-content-primary">{funnel.reserved}</span>
                    </div>
                    <div>
                      <span className="block text-content-tertiary">Signed</span>
                      <span className="text-content-primary">{funnel.signed}</span>
                    </div>
                    <div>
                      <span className="block text-content-tertiary">Submitted</span>
                      <span className="text-content-primary">{funnel.submitted}</span>
                    </div>
                    <div>
                      <span className="block text-content-tertiary">Expired</span>
                      <span className={funnel.expired > 0 ? 'text-brand-yellow' : 'text-content-tertiary'}>
                        {funnel.expired}
                      </span>
                    </div>
                    <div>
                      <span className="block text-content-tertiary">Cancelled</span>
                      <span className={funnel.cancelled > 0 ? 'text-brand-orange' : 'text-content-tertiary'}>
                        {funnel.cancelled}
                      </span>
                    </div>
                  </div>
                ) : null}
              </div>
            </Panel>
          </section>

          <section aria-labelledby="sponsored-ops-title" className="space-y-3">
            <div className="flex flex-wrap items-end justify-between gap-3">
              <div>
                <h2 id="sponsored-ops-title" className="text-lg font-semibold">
                  Recent sponsored UserOperations
                </h2>
                <p className="mt-1 text-xs text-content-tertiary">
                  Finalized on-chain UserOperations settled by the AA reconciler
                </p>
              </div>
              <div className="flex flex-wrap gap-1.5" role="tablist" aria-label="UserOperation status filter">
                {OP_FILTER_TABS.map((tab) => {
                  const active = opFilter === tab.id
                  return (
                    <button
                      key={tab.id}
                      type="button"
                      role="tab"
                      aria-selected={active}
                      onClick={() => { setOpFilter(tab.id) }}
                      className={`border px-3 py-1.5 text-xs font-semibold transition-colors ${
                        active
                          ? 'border-brand-orange bg-brand-orange text-content-primary'
                          : 'border-brand-border/30 bg-surface-panel text-content-secondary hover:border-brand-peach/50 hover:text-content-primary'
                      }`}
                    >
                      {tab.label}
                    </button>
                  )
                })}
              </div>
            </div>

            <Panel>
              {opsQuery.isLoading ? (
                <LoadingState rows={4} />
              ) : operations.length > 0 ? (
                <div className="overflow-x-auto">
                  <table className="w-full min-w-[680px] text-left text-sm">
                    <caption className="sr-only">Finalized sponsored UserOperations</caption>
                    <thead className="border-b border-brand-border/25 bg-app-bg/40 text-xs font-medium text-content-secondary">
                      <tr>
                        <th scope="col" className="px-3.5 py-3">Time (UTC)</th>
                        <th scope="col" className="px-3.5 py-3">Trading Account</th>
                        <th scope="col" className="px-3.5 py-3">UserOp &amp; Tx</th>
                        <th scope="col" className="px-3.5 py-3">Outcome</th>
                        <th scope="col" className="px-3.5 py-3">Gas cost / Max</th>
                        <th scope="col" className="px-3.5 py-3 text-right">Latency</th>
                      </tr>
                    </thead>
                    <tbody>
                      {(() => {
                        const seenTxKeys = new Set<string>()
                        return operations.map((op) => {
                          let showArbiscan = false
                          if (op.txHash) {
                            const txKey = arbiscanLinkKey('tx', op.txHash, chain.chainId)
                            if (!seenTxKeys.has(txKey)) {
                              seenTxKeys.add(txKey)
                              showArbiscan = true
                            }
                          }
                          return (
                            <tr
                              key={op.userOpHash}
                              className="border-b border-brand-border/15 transition-colors last:border-0 hover:bg-app-bg/35"
                            >
                          <td className="whitespace-nowrap px-3.5 py-3 font-mono text-xs text-content-secondary">
                            {formatUtc(op.occurredAt)}
                          </td>
                          <td className="whitespace-nowrap px-3.5 py-3">
                            <Link
                              to={`/wallets/${op.sender}`}
                              className="inline-flex items-center gap-2 font-mono text-xs text-brand-peach hover:underline"
                            >
                              <AddressAvatar address={op.sender} size={16} />
                              <span>{shortAddress(op.sender)}</span>
                            </Link>
                          </td>
                          <td className="whitespace-nowrap px-3.5 py-3 font-mono text-xs">
                            <div className="inline-flex items-center gap-1.5">
                              <span className="text-content-primary" title={op.userOpHash}>
                                {shortHash(op.userOpHash)}
                              </span>
                              {op.txHash && showArbiscan ? (
                                <ArbiscanButton
                                  kind="tx"
                                  value={op.txHash}
                                  chainId={chain.chainId}
                                />
                              ) : null}
                            </div>
                          </td>
                          <td className="whitespace-nowrap px-3.5 py-3">
                            <StatusPill
                              tone={op.success ? 'live' : 'blocked'}
                              label={op.success ? 'Succeeded' : 'Reverted'}
                            />
                          </td>
                          <td className="whitespace-nowrap px-3.5 py-3 font-mono text-xs tabular-nums">
                            <div className="font-semibold text-content-primary">
                              {formatSponsoredEthWei(op.actualGasCostWei, 6)}
                            </div>
                            <div className="text-[11px] text-content-tertiary">
                              max {formatSponsoredEthWei(op.maxCostWei, 5)}
                              {op.efficiencyBps != null
                                ? ` · ${formatBpsPercent(op.efficiencyBps)}`
                                : ''}
                            </div>
                          </td>
                          <td className="whitespace-nowrap px-3.5 py-3 text-right font-mono text-xs tabular-nums text-content-secondary">
                            {op.latencySeconds != null
                              ? formatDurationSeconds(op.latencySeconds)
                              : '—'}
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
                  title="No sponsored UserOperations found"
                  message={
                    opFilter === 'all'
                      ? 'Finalized sponsored transactions will appear here as soon as they settle on-chain.'
                      : `No ${opFilter} sponsored UserOperations recorded in the current history window.`
                  }
                />
              )}
            </Panel>
          </section>

          {controlEvents.length > 0 ? (
            <section aria-labelledby="control-events-title" className="space-y-3">
              <div>
                <h2 id="control-events-title" className="text-lg font-semibold">
                  Sponsorship control audit trail
                </h2>
                <p className="mt-1 text-xs text-content-tertiary">
                  Sanitized pause and resume state transitions on the issuance kill-switch
                </p>
              </div>
              <Panel className="divide-y divide-brand-border/15">
                {controlEvents.map((ev) => (
                  <div
                    key={ev.id}
                    className="flex flex-wrap items-center justify-between gap-3 px-4 py-3 text-xs"
                  >
                    <div className="flex flex-wrap items-center gap-2.5">
                      <StatusPill
                        tone={ev.action === 'resume' ? 'live' : 'blocked'}
                        label={ev.action === 'resume' ? 'Resumed' : 'Paused'}
                      />
                      <span className="text-content-primary">{ev.reason}</span>
                    </div>
                    <span className="font-mono text-content-tertiary">
                      {formatUtc(ev.createdAt)}
                    </span>
                  </div>
                ))}
              </Panel>
            </section>
          ) : null}
        </>
      )}
    </div>
  )
}
