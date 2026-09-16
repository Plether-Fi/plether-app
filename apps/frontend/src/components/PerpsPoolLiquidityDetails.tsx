import type { ReactNode } from 'react'
import { formatPerpsUsdc } from '../utils/perps'
import type { PerpsSeniorCapitalStatus } from '../utils/perpsPoolCapital'
import { DocsLink, TokenAmount, type TooltipDocsLink } from './ui'

export interface PerpsPoolLiquidityDetailsProps {
  poolAssetsUsdc?: bigint
  freeUsdc?: bigint
  juniorPrincipal?: ReactNode
  seniorPrincipal?: ReactNode
  juniorSharePercent?: number
  seniorSharePercent?: number
  seniorStatus?: PerpsSeniorCapitalStatus
  seniorImpairment?: ReactNode
  isJuniorExhausted?: boolean
  isEmpty?: boolean
  isLoading?: boolean
  docsLink?: TooltipDocsLink
}

function poolAmount(value: bigint | undefined, isLoading: boolean): ReactNode {
  if (isLoading) return '...'
  if (value === undefined) return '--'
  return <TokenAmount amount={value > 0n && value < 10_000n ? '<0.01' : formatPerpsUsdc(value)} />
}

function reservedShare(poolAssetsUsdc: bigint | undefined, reservedUsdc: bigint | undefined): string | undefined {
  if (poolAssetsUsdc === undefined || poolAssetsUsdc <= 0n || reservedUsdc === undefined) return undefined
  if (reservedUsdc > 0n && reservedUsdc * 1_000n < poolAssetsUsdc) return '<0.1%'
  if (reservedUsdc < poolAssetsUsdc && reservedUsdc * 1_000n > poolAssetsUsdc * 999n) return '>99.9%'
  const tenths = (reservedUsdc * 1_000n + poolAssetsUsdc / 2n) / poolAssetsUsdc
  return `${(Number(tenths) / 10).toFixed(1)}%`
}

function displayPercent(value: number | undefined): string {
  if (value === undefined || !Number.isFinite(value)) return '--'
  return `${value.toLocaleString('en-US', { maximumFractionDigits: 1 })}%`
}

function barWidth(value: number | undefined): string {
  if (value === undefined || !Number.isFinite(value)) return '0%'
  return `${Math.min(100, Math.max(0, value)).toString()}%`
}

function CapitalStatus({
  seniorStatus,
  seniorImpairment,
  isJuniorExhausted,
  isEmpty,
  isLoading,
}: Pick<
  PerpsPoolLiquidityDetailsProps,
  'seniorStatus' | 'seniorImpairment' | 'isJuniorExhausted' | 'isEmpty' | 'isLoading'
>) {
  if (isLoading) {
    return <span className="text-content-secondary">Updating pool funds</span>
  }

  if (isEmpty) {
    return <span className="text-content-secondary">No vault capital yet</span>
  }

  if (seniorStatus === 'impaired') {
    return (
      <span className="text-brand-orange">
        Senior is below its protected balance by <span className="font-semibold">{seniorImpairment ?? '--'}</span>
      </span>
    )
  }

  if (isJuniorExhausted) {
    return <span className="text-warning">Junior protection is depleted</span>
  }

  if (seniorStatus === 'at-high-water-mark') {
    return <span className="text-positive">Senior is at its protected balance</span>
  }

  if (seniorStatus === 'not-impaired') {
    return <span className="text-positive">Senior has no unrecovered losses</span>
  }

  return <span className="text-content-secondary">Pool breakdown unavailable</span>
}

export function PerpsPoolLiquidityDetails({
  poolAssetsUsdc,
  freeUsdc,
  juniorPrincipal,
  seniorPrincipal,
  juniorSharePercent,
  seniorSharePercent,
  seniorStatus,
  seniorImpairment,
  isJuniorExhausted = false,
  isEmpty = false,
  isLoading = false,
  docsLink,
}: PerpsPoolLiquidityDetailsProps) {
  const hasCapitalComposition = juniorSharePercent !== undefined && seniorSharePercent !== undefined
  const juniorLabel = displayPercent(juniorSharePercent)
  const seniorLabel = displayPercent(seniorSharePercent)
  const reservedUsdc = poolAssetsUsdc !== undefined && freeUsdc !== undefined
    && freeUsdc >= 0n && poolAssetsUsdc >= freeUsdc
    ? poolAssetsUsdc - freeUsdc
    : undefined
  const reservedShareLabel = isLoading ? undefined : reservedShare(poolAssetsUsdc, reservedUsdc)
  const barLabel = hasCapitalComposition
    ? `Vault capital: Senior ${seniorLabel}; Junior ${juniorLabel}`
    : 'Vault capital breakdown unavailable'

  return (
    <div className="w-full text-left">
      <dl className="grid grid-cols-1 gap-3 text-xs sm:grid-cols-2 sm:gap-6">
        <div className="grid min-w-0 grid-cols-[1fr_auto] content-start items-baseline gap-x-3">
          <dt className="text-content-secondary">Total pool assets</dt>
          <dd className="font-semibold text-content-primary">{poolAmount(poolAssetsUsdc, isLoading)}</dd>
          <dd className="col-span-2 mt-1 text-[11px] leading-4 text-content-secondary">Pool assets reported by the protocol.</dd>
        </div>
        <div className="grid min-w-0 grid-cols-[1fr_auto] content-start items-baseline gap-x-3 sm:border-l sm:border-brand-border/20 sm:pl-6">
          <dt className="text-content-secondary">Reserved pool assets</dt>
          <dd className="font-semibold text-content-primary">{poolAmount(reservedUsdc, isLoading)}</dd>
          <dd className="col-span-2 mt-1 text-[11px] leading-4 text-content-secondary">
            Total pool assets minus free liquidity: assets backing existing positions and other protocol obligations.
            {reservedShareLabel !== undefined ? <span className="mt-1 block">Share of pool assets reserved: <span className="font-semibold text-content-primary">{reservedShareLabel}</span></span> : null}
          </dd>
        </div>
      </dl>

      <div className="mt-4 space-y-2 border-y border-brand-border/20 py-3 text-xs leading-5 text-content-secondary">
        <h3 className="font-semibold text-content-primary">What this means for trading</h3>
        <p>Existing positions can reserve most of the pool’s assets. New orders must satisfy both the directional limit and payout-backing requirements. Low free liquidity can restrict new exposure; some orders may still fit without increasing the pool’s maximum payout obligation.</p>
        <p>Use Max in the trade ticket to check your current estimated order size. Final acceptance depends on conditions when the order executes.</p>
      </div>

      <div className="mt-4 flex flex-wrap items-center justify-between gap-2">
        <h3 className="text-xs font-semibold text-content-primary">LP capital and loss protection</h3>
        <p className="text-[11px] text-content-secondary">Losses affect Junior before Senior</p>
      </div>
      <p className="mt-1 text-[11px] leading-4 text-content-secondary">Tranche balances are accounting values, not available trading liquidity.</p>

      <div
        className="mt-3 flex h-6 w-full overflow-hidden bg-app-bg/70"
        role="img"
        aria-label={barLabel}
      >
        <div
          className="flex min-w-0 items-center justify-center overflow-hidden bg-[#FFAB96] text-[10px] font-semibold text-app-bg transition-[width] duration-200 motion-reduce:transition-none"
          style={{ width: barWidth(seniorSharePercent) }}
        >
          {seniorSharePercent !== undefined && seniorSharePercent >= 16 ? (
            <span className="block w-full min-w-0 truncate px-1 text-center">Senior · {seniorLabel}</span>
          ) : null}
        </div>
        <div
          className="flex min-w-0 items-center justify-center overflow-hidden bg-brand-orange text-[10px] font-semibold text-content-primary transition-[width] duration-200 motion-reduce:transition-none"
          style={{ width: barWidth(juniorSharePercent) }}
        >
          {juniorSharePercent !== undefined && juniorSharePercent >= 16 ? (
            <span className="block w-full min-w-0 truncate px-1 text-center">Junior · {juniorLabel}</span>
          ) : null}
        </div>
      </div>

      <dl className="mt-2 grid grid-cols-1 gap-2 text-xs sm:grid-cols-2 sm:gap-6">
        <div className="flex min-w-0 items-baseline justify-between gap-3">
          <dt className="flex min-w-0 items-center gap-2 text-content-secondary">
            <span className="h-2 w-2 shrink-0 bg-[#FFAB96]" aria-hidden="true" />
            <span>Senior · protected by Junior</span>
          </dt>
          <dd className="shrink-0 font-semibold text-content-primary">{seniorPrincipal ?? '--'}</dd>
        </div>
        <div className="flex min-w-0 items-baseline justify-between gap-3 sm:border-l sm:border-brand-border/20 sm:pl-6">
          <dt className="flex min-w-0 items-center gap-2 text-content-secondary">
            <span className="h-2 w-2 shrink-0 bg-brand-orange" aria-hidden="true" />
            <span>Junior · absorbs losses first</span>
          </dt>
          <dd className="shrink-0 font-semibold text-content-primary">{juniorPrincipal ?? '--'}</dd>
        </div>
      </dl>

      <div className="mt-4 flex flex-wrap items-center justify-between gap-2 border-t border-brand-border/20 pt-3 text-xs">
        <CapitalStatus
          seniorStatus={seniorStatus}
          seniorImpairment={seniorImpairment}
          isJuniorExhausted={isJuniorExhausted}
          isEmpty={isEmpty}
          isLoading={isLoading}
        />
        {!isLoading && !isEmpty && seniorStatus !== 'impaired' && !isJuniorExhausted ? (
          <span className="text-content-secondary">
            Junior protects Senior from the first <span className="font-semibold text-content-primary">{juniorPrincipal ?? '--'}</span> of pool losses
          </span>
        ) : null}
        {!isLoading && !isEmpty && seniorStatus !== 'impaired' && isJuniorExhausted ? (
          <span className="text-content-secondary">Further pool losses would reduce Senior's value</span>
        ) : null}
      </div>

      <div className="mt-3 flex flex-wrap items-start justify-between gap-2 border-t border-brand-border/20 pt-3 text-[11px] leading-4 text-content-secondary">
        <p>Withdrawals depend on the liquidity available at each hourly processing time.</p>
        {docsLink ? (
          <DocsLink
            href={docsLink.href}
            title={docsLink.title}
            className="shrink-0"
          >
            Learn more
          </DocsLink>
        ) : null}
      </div>
    </div>
  )
}
