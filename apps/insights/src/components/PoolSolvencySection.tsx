import { InsightsApiError } from '../api/client'
import {
  formatPoolBpsPercent,
  type LiquiditySolvencyResponse,
} from '../api/liquidity'
import { formatDurationCompact } from '../api/positions'
import { StatusPill, type StatusTone } from './StatusPill'
import { EmptyState, ErrorState, LoadingState, Panel } from './ui'
import { formatCompactAssets, formatSignedAssets } from '../utils/format'

export interface PoolSolvencySectionProps {
  data: LiquiditySolvencyResponse | undefined
  isLoading: boolean
  isError: boolean
  error?: Error | null
  onRetry?: () => void
}

function parseAssetsNum(raw: string | null | undefined): number {
  if (!raw || !/^-?\d+$/.test(raw)) return 0
  const n = Number(raw)
  return Number.isFinite(n) ? n : 0
}

function formatCoverageMultiple(bps: number | null | undefined): string {
  if (bps == null || !Number.isFinite(bps)) return '—'
  const mult = bps / 10_000
  if (mult >= 100) return `${mult.toFixed(0)}×`
  return `${mult.toFixed(2)}×`
}

function formatBpsRate(bps: number | null | undefined, digits = 2): string {
  if (bps == null || !Number.isFinite(bps)) return '—'
  return `${(bps / 100).toFixed(digits)}%`
}

function formatSharesCompact(raw: string | null | undefined): string {
  if (!raw || !/^\d+$/.test(raw)) return '—'
  const val = Number(raw) / 1e9
  if (!Number.isFinite(val)) return '—'
  if (val === 0) return '0'
  if (val >= 1_000_000) return `${(val / 1_000_000).toFixed(2)}M`
  if (val >= 1_000) return `${(val / 1_000).toFixed(2)}k`
  return val.toFixed(2)
}

function ParityCheckCard({
  title,
  passed,
  primary,
  secondary,
}: {
  title: string
  passed: boolean | null | undefined
  primary: string
  secondary: string
}) {
  const tone: StatusTone =
    passed === true ? 'live' : passed === false ? 'blocked' : 'unknown'
  const label = passed === true ? 'Verified' : passed === false ? 'Mismatch' : 'Unavailable'
  return (
    <div
      className={`border p-3.5 ${
        passed === false
          ? 'border-brand-orange/45 bg-brand-orange/[0.06]'
          : 'border-brand-border/20 bg-app-bg/55'
      }`}
    >
      <div className="flex items-start justify-between gap-2">
        <h4 className="text-xs font-semibold text-content-primary">{title}</h4>
        <StatusPill tone={tone} label={label} />
      </div>
      <p className="mt-2 font-mono text-xs font-semibold tabular-nums text-content-primary">
        {primary}
      </p>
      <p className="mt-1 font-mono text-[11px] tabular-nums text-content-tertiary">{secondary}</p>
    </div>
  )
}

export function PoolSolvencySection({
  data,
  isLoading,
  isError,
  error,
  onRetry,
}: PoolSolvencySectionProps) {
  const awaitingDeploy = error instanceof InsightsApiError && error.status === 404

  if (awaitingDeploy || (!isLoading && !isError && !data)) {
    return null
  }

  const solv = data?.solvency
  const wf = data?.waterfall
  const policy = data?.policy
  const audit = data?.custodyAudit

  const terminalDeficit = parseAssetsNum(solv?.terminalDeficitAssets)
  const isDegraded = solv?.degradedMode === true
  const isSeniorImpaired = solv?.seniorImpaired === true
  const hasCriticalFaults = (audit?.criticalFaults.activeBits.length ?? 0) > 0
  const hasCustodyIssue =
    audit?.custodyBalanced === false ||
    audit?.seniorEscrowBalanced === false ||
    audit?.juniorEscrowBalanced === false ||
    audit?.navBookCapMatchesEngine === false

  const overallTone: StatusTone =
    isDegraded || terminalDeficit > 0 || hasCriticalFaults || hasCustodyIssue
      ? 'blocked'
      : isSeniorImpaired || solv?.canIncreaseRisk === false
        ? 'degraded'
        : solv
          ? 'live'
          : 'unknown'

  const overallLabel =
    isDegraded
      ? 'Degraded Mode'
      : terminalDeficit > 0
        ? 'Terminal Deficit'
        : hasCriticalFaults || hasCustodyIssue
          ? 'Custody Alert'
          : isSeniorImpaired
            ? 'Senior Impaired'
            : solv
              ? 'Solvency Healthy'
              : 'Loading'

  const poolTotalNum = Math.max(0, parseAssetsNum(solv?.poolTotalAssets))
  const maxLiabNum = Math.max(0, parseAssetsNum(solv?.maxDirectionalLiabilityAssets))
  const claimsNum = Math.max(0, parseAssetsNum(solv?.totalTraderClaimsAssets))
  const freeNum = Math.max(0, parseAssetsNum(solv?.freeSettlementAssets))
  const reserveNum = Math.max(0, parseAssetsNum(solv?.withdrawalReserveAssets))

  const liabPct = poolTotalNum > 0 ? Math.min(100, (maxLiabNum / poolTotalNum) * 100) : 0
  const claimsPct =
    poolTotalNum > 0 ? Math.min(100 - liabPct, (claimsNum / poolTotalNum) * 100) : 0
  const headroomPct = Math.max(0, 100 - liabPct - claimsPct)

  const freePct = poolTotalNum > 0 ? Math.min(100, (freeNum / poolTotalNum) * 100) : 0
  const reservePct = poolTotalNum > 0 ? Math.min(100 - freePct, (reserveNum / poolTotalNum) * 100) : 0

  const terminalDeltaNum = parseAssetsNum(solv?.terminalLpPriceDeltaAssets)
  const netVpiNum = parseAssetsNum(wf?.netVpiAccruedAssets)

  return (
    <section id="solvency" aria-labelledby="pool-solvency-title" className="space-y-4">
      <div className="flex flex-wrap items-baseline justify-between gap-3">
        <div>
          <h2 id="pool-solvency-title" className="text-xl font-semibold">
            Protocol Solvency, LP Waterfall &amp; Custody Parity
          </h2>
          <p className="mt-1 text-xs text-content-tertiary">
            Atomic on-chain snapshot from CfdEngine, HousePool, TrancheVaults &amp;
            SettlementMonitorLens
            {data?.observedBlock
              ? ` at block ${Number(data.observedBlock).toLocaleString('en-US')}`
              : ''}
          </p>
        </div>
        {solv ? (
          <div className="flex flex-wrap items-center gap-2">
            <StatusPill tone={overallTone} label={overallLabel} />
            <StatusPill
              tone={terminalDeficit > 0 ? 'blocked' : 'live'}
              label={
                terminalDeficit > 0
                  ? `Deficit ${formatCompactAssets(solv.terminalDeficitAssets)}`
                  : 'Terminal Deficit: 0'
              }
            />
            <StatusPill
              tone={isSeniorImpaired ? 'degraded' : 'live'}
              label={isSeniorImpaired ? 'Senior Impaired' : 'Senior Unimpaired'}
            />
            <StatusPill
              tone={solv.canIncreaseRisk === false ? 'degraded' : 'live'}
              label={
                solv.canIncreaseRisk === false ? 'Risk Increase Blocked' : 'Risk Increase Allowed'
              }
            />
            {policy?.pendingPoolConfigTimelockActive ? (
              <StatusPill
                tone="degraded"
                label={`Config Timelock · ${formatDurationCompact(policy.secondsUntilPoolConfigActivation)}`}
              />
            ) : null}
          </div>
        ) : null}
      </div>

      {isLoading ? (
        <Panel>
          <LoadingState rows={4} />
        </Panel>
      ) : isError || !data || !solv || !wf || !policy ? (
        <ErrorState
          title="Protocol solvency telemetry unavailable"
          message={error?.message ?? 'Live solvency Multicall3 snapshot could not be loaded.'}
          onRetry={onRetry}
        />
      ) : (
        <div className="space-y-4">
          {/* 1. Solvency & Worst-Case Liability Envelope */}
          <Panel className="space-y-5 p-5">
            <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-5">
              <div className="border border-brand-border/20 bg-app-bg/55 p-3.5">
                <p className="font-mono text-[11px] uppercase tracking-wider text-content-tertiary">
                  HousePool Assets
                </p>
                <p className="mt-1.5 font-mono text-xl font-bold tabular-nums text-content-primary">
                  {formatCompactAssets(solv.poolTotalAssets)}
                </p>
                <p className="mt-1 font-mono text-[11px] tabular-nums text-content-tertiary">
                  Excess: {formatCompactAssets(solv.excessAssets)}
                </p>
              </div>

              <div className="border border-brand-border/20 bg-app-bg/55 p-3.5">
                <p className="font-mono text-[11px] uppercase tracking-wider text-content-tertiary">
                  Solvency Coverage
                </p>
                <p
                  className={`mt-1.5 font-mono text-xl font-bold tabular-nums ${
                    solv.solvencyCoverageBps != null && solv.solvencyCoverageBps < 12_000
                      ? 'text-brand-orange'
                      : 'text-positive'
                  }`}
                >
                  {maxLiabNum === 0
                    ? '∞'
                    : formatCoverageMultiple(solv.solvencyCoverageBps)}
                </p>
                <p className="mt-1 font-mono text-[11px] tabular-nums text-content-tertiary">
                  {maxLiabNum === 0
                    ? 'No open directional liability'
                    : `${formatPoolBpsPercent(solv.solvencyCoverageBps, 0)} of worst-case`}
                </p>
              </div>

              <div className="border border-brand-border/20 bg-app-bg/55 p-3.5">
                <p className="font-mono text-[11px] uppercase tracking-wider text-content-tertiary">
                  Max Directional Liab.
                </p>
                <p className="mt-1.5 font-mono text-xl font-bold tabular-nums text-brand-peach">
                  {formatCompactAssets(solv.maxDirectionalLiabilityAssets)}
                </p>
                <p className="mt-1 font-mono text-[11px] tabular-nums text-content-tertiary">
                  Book v{solv.bookVersion != null ? String(solv.bookVersion) : '—'} ·{' '}
                  {solv.hasOpenPositions ? 'Open OI' : 'Flat'}
                </p>
              </div>

              <div className="border border-brand-border/20 bg-app-bg/55 p-3.5">
                <p className="font-mono text-[11px] uppercase tracking-wider text-content-tertiary">
                  Senior Trader Claims
                </p>
                <p className="mt-1.5 font-mono text-xl font-bold tabular-nums text-content-primary">
                  {formatCompactAssets(solv.totalTraderClaimsAssets)}
                </p>
                <p className="mt-1 font-mono text-[11px] tabular-nums text-content-tertiary">
                  Eff. Solvency: {formatCompactAssets(solv.effectiveSolvencyAssets)}
                </p>
              </div>

              <div className="border border-brand-border/20 bg-app-bg/55 p-3.5">
                <p className="font-mono text-[11px] uppercase tracking-wider text-content-tertiary">
                  Free Settlement Cash
                </p>
                <p className="mt-1.5 font-mono text-xl font-bold tabular-nums text-positive">
                  {formatCompactAssets(solv.freeSettlementAssets)}
                </p>
                <p className="mt-1 font-mono text-[11px] tabular-nums text-content-tertiary">
                  {formatPoolBpsPercent(solv.freeLiquidityShareBps, 1)} of physical pool
                </p>
              </div>
            </div>

            {/* Dual Proportional Envelope Bars */}
            <div className="grid gap-4 border-t border-brand-border/15 pt-4 lg:grid-cols-2">
              <div className="space-y-2">
                <div className="flex flex-wrap items-baseline justify-between gap-2 font-mono text-xs">
                  <span className="text-content-secondary">
                    Worst-Case Liability Envelope vs. Pool Backing
                  </span>
                  <span className="tabular-nums text-content-tertiary">
                    Headroom {headroomPct.toFixed(1)}%
                  </span>
                </div>
                <div
                  className="flex h-3 w-full overflow-hidden border border-brand-border/25 bg-app-bg"
                  role="img"
                  aria-label="HousePool worst-case liability envelope breakdown"
                >
                  {liabPct > 0 ? (
                    <div
                      className="h-full bg-brand-peach"
                      style={{ width: `${liabPct.toFixed(2)}%` }}
                      title={`Max Directional Liability: ${formatCompactAssets(solv.maxDirectionalLiabilityAssets)}`}
                    />
                  ) : null}
                  {claimsPct > 0 ? (
                    <div
                      className="h-full bg-[#C4B5FD]"
                      style={{ width: `${claimsPct.toFixed(2)}%` }}
                      title={`Senior Trader Claims: ${formatCompactAssets(solv.totalTraderClaimsAssets)}`}
                    />
                  ) : null}
                  {headroomPct > 0 ? (
                    <div
                      className="h-full bg-positive/55"
                      style={{ width: `${headroomPct.toFixed(2)}%` }}
                      title={`Unencumbered Solvency Headroom: ${headroomPct.toFixed(1)}%`}
                    />
                  ) : null}
                </div>
                <div className="flex flex-wrap gap-x-4 gap-y-1 font-mono text-[11px] text-content-tertiary">
                  <span className="inline-flex items-center gap-1.5">
                    <span className="h-2 w-2 bg-brand-peach" />
                    Max Liability ({liabPct.toFixed(1)}%)
                  </span>
                  <span className="inline-flex items-center gap-1.5">
                    <span className="h-2 w-2 bg-[#C4B5FD]" />
                    Trader Claims ({claimsPct.toFixed(1)}%)
                  </span>
                  <span className="inline-flex items-center gap-1.5">
                    <span className="h-2 w-2 bg-positive/70" />
                    Solvency Headroom ({headroomPct.toFixed(1)}%)
                  </span>
                </div>
              </div>

              <div className="space-y-2">
                <div className="flex flex-wrap items-baseline justify-between gap-2 font-mono text-xs">
                  <span className="text-content-secondary">
                    Settlement Cash Availability
                  </span>
                  <span className="tabular-nums text-content-tertiary">
                    Free {freePct.toFixed(1)}% · Reserved {reservePct.toFixed(1)}%
                  </span>
                </div>
                <div
                  className="flex h-3 w-full overflow-hidden border border-brand-border/25 bg-app-bg"
                  role="img"
                  aria-label="HousePool free settlement cash versus reserve"
                >
                  {freePct > 0 ? (
                    <div
                      className="h-full bg-positive/75"
                      style={{ width: `${freePct.toFixed(2)}%` }}
                      title={`Free Settlement Cash: ${formatCompactAssets(solv.freeSettlementAssets)}`}
                    />
                  ) : null}
                  {reservePct > 0 ? (
                    <div
                      className="h-full bg-brand-yellow/65"
                      style={{ width: `${reservePct.toFixed(2)}%` }}
                      title={`Engine & Withdrawal Reserves: ${formatCompactAssets(solv.withdrawalReserveAssets)}`}
                    />
                  ) : null}
                </div>
                <div className="flex flex-wrap gap-x-4 gap-y-1 font-mono text-[11px] text-content-tertiary">
                  <span className="inline-flex items-center gap-1.5">
                    <span className="h-2 w-2 bg-positive/75" />
                    Free Cash ({formatCompactAssets(solv.freeSettlementAssets)})
                  </span>
                  <span className="inline-flex items-center gap-1.5">
                    <span className="h-2 w-2 bg-brand-yellow/75" />
                    Reserves ({formatCompactAssets(solv.withdrawalReserveAssets)})
                  </span>
                </div>
              </div>
            </div>
          </Panel>

          {/* 2. LP Waterfall, Withdrawal Headroom & Policy Strip */}
          <Panel className="p-5">
            <div className="flex flex-wrap items-baseline justify-between gap-2 pb-4">
              <div>
                <h3 className="text-sm font-semibold text-content-primary">
                  LP Waterfall, Withdrawal Headroom &amp; Fee Policy
                </h3>
                <p className="mt-0.5 text-xs text-content-tertiary">
                  Live Senior/Junior principal buffers, instant redemption headroom, VPI balance,
                  and protocol LP rates
                </p>
              </div>
              <div className="flex flex-wrap items-center gap-2 font-mono text-[11px] text-content-tertiary">
                <span>
                  Ordinary Deposits:{' '}
                  <strong
                    className={
                      solv.canAcceptOrdinaryDeposits ? 'text-positive' : 'text-brand-yellow'
                    }
                  >
                    {solv.canAcceptOrdinaryDeposits === true
                      ? 'Open'
                      : solv.canAcceptOrdinaryDeposits === false
                        ? 'Restricted'
                        : '—'}
                  </strong>
                </span>
                <span>·</span>
                <span>
                  Senior Reservations:{' '}
                  <strong
                    className={
                      solv.seniorReservationsWithinLimits ? 'text-positive' : 'text-brand-orange'
                    }
                  >
                    {solv.seniorReservationsWithinLimits === true
                      ? 'Within Limits'
                      : solv.seniorReservationsWithinLimits === false
                        ? 'Exceeded'
                        : '—'}
                  </strong>
                </span>
              </div>
            </div>

            <dl className="grid gap-3 border-t border-brand-border/15 pt-4 sm:grid-cols-2 lg:grid-cols-4">
              <div className="border border-brand-border/20 bg-app-bg/50 p-3.5">
                <dt className="font-mono text-[11px] text-content-tertiary">
                  Max Senior Withdraw
                </dt>
                <dd className="mt-1 font-mono text-base font-semibold tabular-nums text-content-primary">
                  {formatCompactAssets(wf.maxSeniorWithdrawAssets)}
                </dd>
                <dd className="mt-0.5 font-mono text-[11px] tabular-nums text-content-tertiary">
                  Principal: {formatCompactAssets(wf.seniorPrincipalAssets)}
                </dd>
              </div>

              <div className="border border-brand-border/20 bg-app-bg/50 p-3.5">
                <dt className="font-mono text-[11px] text-content-tertiary">
                  Max Junior Withdraw
                </dt>
                <dd className="mt-1 font-mono text-base font-semibold tabular-nums text-content-primary">
                  {formatCompactAssets(wf.maxJuniorWithdrawAssets)}
                </dd>
                <dd className="mt-0.5 font-mono text-[11px] tabular-nums text-content-tertiary">
                  First-loss buffer: {formatCompactAssets(wf.juniorPrincipalAssets)}
                </dd>
              </div>

              <div className="border border-brand-border/20 bg-app-bg/50 p-3.5">
                <dt className="font-mono text-[11px] text-content-tertiary">
                  Senior Deposit Capacity
                </dt>
                <dd className="mt-1 font-mono text-base font-semibold tabular-nums text-brand-peach">
                  {formatCompactAssets(wf.seniorDepositCapacityAssets)}
                </dd>
                <dd className="mt-0.5 font-mono text-[11px] tabular-nums text-content-tertiary">
                  Reserved: {formatCompactAssets(wf.reservedSeniorDepositAssets)}
                </dd>
              </div>

              <div className="border border-brand-border/20 bg-app-bg/50 p-3.5">
                <dt className="font-mono text-[11px] text-content-tertiary">
                  Senior vs. High-Water Mark
                </dt>
                <dd
                  className={`mt-1 font-mono text-base font-semibold tabular-nums ${
                    wf.seniorHwmCoverageBps != null && wf.seniorHwmCoverageBps < 10_000
                      ? 'text-brand-orange'
                      : 'text-positive'
                  }`}
                >
                  {formatPoolBpsPercent(wf.seniorHwmCoverageBps, 2)}
                </dd>
                <dd className="mt-0.5 font-mono text-[11px] tabular-nums text-content-tertiary">
                  HWM: {formatCompactAssets(wf.seniorHighWaterMarkAssets)}
                </dd>
              </div>

              <div className="border border-brand-border/20 bg-app-bg/50 p-3.5">
                <dt className="font-mono text-[11px] text-content-tertiary">
                  Terminal LP Price Delta
                </dt>
                <dd
                  className={`mt-1 font-mono text-base font-semibold tabular-nums ${
                    terminalDeltaNum > 0
                      ? 'text-positive'
                      : terminalDeltaNum < 0
                        ? 'text-brand-orange'
                        : 'text-content-primary'
                  }`}
                >
                  {solv.terminalLpPriceDeltaAssets != null
                    ? formatSignedAssets(solv.terminalLpPriceDeltaAssets)
                    : '—'}
                </dd>
                <dd className="mt-0.5 font-mono text-[11px] tabular-nums text-content-tertiary">
                  Canonical unrealized LP PnL
                </dd>
              </div>

              <div className="border border-brand-border/20 bg-app-bg/50 p-3.5">
                <dt className="font-mono text-[11px] text-content-tertiary">
                  Net VPI Pool Balance
                </dt>
                <dd
                  className={`mt-1 font-mono text-base font-semibold tabular-nums ${
                    netVpiNum >= 0 ? 'text-positive' : 'text-brand-yellow'
                  }`}
                >
                  {wf.netVpiAccruedAssets != null
                    ? formatSignedAssets(wf.netVpiAccruedAssets)
                    : '—'}
                </dd>
                <dd className="mt-0.5 font-mono text-[11px] tabular-nums text-content-tertiary">
                  +{formatCompactAssets(wf.positiveVpiAssets)} / −
                  {formatCompactAssets(wf.negativeVpiAssets)}
                </dd>
              </div>

              <div className="border border-brand-border/20 bg-app-bg/50 p-3.5">
                <dt className="font-mono text-[11px] text-content-tertiary">
                  Pending Revenue &amp; Recap
                </dt>
                <dd className="mt-1 font-mono text-base font-semibold tabular-nums text-content-primary">
                  {formatCompactAssets(wf.pendingTradingRevenueAssets)}
                </dd>
                <dd className="mt-0.5 font-mono text-[11px] tabular-nums text-content-tertiary">
                  Recap: {formatCompactAssets(wf.pendingRecapitalizationAssets)} · Unassigned:{' '}
                  {formatCompactAssets(wf.unassignedAssets)}
                </dd>
              </div>

              <div className="border border-brand-border/20 bg-app-bg/50 p-3.5">
                <dt className="font-mono text-[11px] text-content-tertiary">
                  Live LP Rates &amp; Frozen Fees
                </dt>
                <dd className="mt-1 font-mono text-base font-semibold tabular-nums text-content-primary">
                  {formatBpsRate(policy.seniorRateBps)} Senior APR
                </dd>
                <dd className="mt-0.5 font-mono text-[11px] tabular-nums text-content-tertiary">
                  Frozen fee: Sen {formatBpsRate(policy.seniorFrozenLpFeeBps)} · Jun{' '}
                  {formatBpsRate(policy.juniorFrozenLpFeeBps)}
                </dd>
              </div>
            </dl>
          </Panel>

          {/* 3. Live Custody & Invariant Parity Audit */}
          <Panel className="space-y-4 p-5">
            <div className="flex flex-wrap items-center justify-between gap-2">
              <div>
                <h3 className="text-sm font-semibold text-content-primary">
                  Live Custody &amp; Invariant Parity Audit
                </h3>
                <p className="mt-0.5 text-xs text-content-tertiary">
                  Direct verification from{' '}
                  <code className="font-mono text-[11px]">
                    SettlementMonitorLens.getSettlementHealth()
                  </code>{' '}
                  comparing ERC-20 custody, tranche escrows, and TerminalNavBookV2 ↔ CfdEngine
                </p>
              </div>
              {audit ? (
                <StatusPill
                  tone={
                    audit.healthState === 'healthy'
                      ? 'live'
                      : audit.healthState === 'faulted'
                        ? 'blocked'
                        : 'degraded'
                  }
                  label={`Health: ${audit.healthState}`}
                />
              ) : null}
            </div>

            {audit ? (
              <>
                <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
                  <ParityCheckCard
                    title="Pool ERC-20 Custody"
                    passed={audit.custodyBalanced}
                    primary={`Raw ${formatCompactAssets(audit.pool.rawAssets)} · Acct ${formatCompactAssets(audit.pool.accountedAssets)}`}
                    secondary={`Deficit ${formatCompactAssets(audit.pool.custodyDeficitAssets)} · Surplus ${formatCompactAssets(audit.pool.custodySurplusAssets)}`}
                  />
                  <ParityCheckCard
                    title="Senior Vault Escrows"
                    passed={audit.seniorEscrowBalanced}
                    primary={`Assets ${formatCompactAssets(audit.escrowAudit.senior.actualAssetEscrow)} / ${formatCompactAssets(audit.escrowAudit.senior.requiredAssetEscrow)}`}
                    secondary={`Pending dep ${formatCompactAssets(data.vaultEscrows.senior.pendingDepositAssets)} · Claim sh ${formatSharesCompact(data.vaultEscrows.senior.depositClaimShares)}`}
                  />
                  <ParityCheckCard
                    title="Junior Vault Escrows"
                    passed={audit.juniorEscrowBalanced}
                    primary={`Assets ${formatCompactAssets(audit.escrowAudit.junior.actualAssetEscrow)} / ${formatCompactAssets(audit.escrowAudit.junior.requiredAssetEscrow)}`}
                    secondary={`Pending dep ${formatCompactAssets(data.vaultEscrows.junior.pendingDepositAssets)} · Claim sh ${formatSharesCompact(data.vaultEscrows.junior.depositClaimShares)}`}
                  />
                  <ParityCheckCard
                    title="NAV Book ↔ Engine Cap"
                    passed={audit.navBookCapMatchesEngine}
                    primary={`${String(audit.navBookParity.bookActiveCurveCount)} active curves · Cap ${(Number(audit.navBookParity.bookCapPrice) / 1e8).toFixed(2)}`}
                    secondary={`Critical faults: ${String(audit.criticalFaults.activeBits.length)} · Dep errors: ${String(audit.dependencyFailures.activeBits.length)}`}
                  />
                </div>

                {(audit.criticalFaults.labels && audit.criticalFaults.labels.length > 0) ||
                (audit.dependencyFailures.labels && audit.dependencyFailures.labels.length > 0) ? (
                  <div className="flex flex-wrap items-center gap-2 border-t border-brand-border/15 pt-3">
                    <span className="font-mono text-[11px] font-semibold uppercase tracking-wider text-brand-orange">
                      Active Monitor Flags:
                    </span>
                    {(audit.criticalFaults.labels ?? []).map((label) => (
                      <span
                        key={`crit-${label}`}
                        className="border border-brand-orange/45 bg-brand-orange/15 px-2 py-0.5 font-mono text-[11px] font-semibold text-brand-orange"
                      >
                        {label}
                      </span>
                    ))}
                    {(audit.dependencyFailures.labels ?? []).map((label) => (
                      <span
                        key={`dep-${label}`}
                        className="border border-brand-yellow/45 bg-brand-yellow/15 px-2 py-0.5 font-mono text-[11px] font-semibold text-brand-yellow"
                      >
                        Dep: {label}
                      </span>
                    ))}
                  </div>
                ) : null}
              </>
            ) : (
              <EmptyState
                title="SettlementMonitorLens health audit unavailable"
                message="getSettlementHealth() did not return a snapshot on this block."
              />
            )}
          </Panel>
        </div>
      )}
    </section>
  )
}
