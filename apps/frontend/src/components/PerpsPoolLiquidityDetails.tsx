import type { ReactNode } from 'react'
import type { PerpsSeniorCapitalStatus } from '../utils/perpsPoolCapital'
import { DocsLink, type TooltipDocsLink } from './ui'

export interface PerpsPoolLiquidityDetailsProps {
  longCapacity?: ReactNode
  shortCapacity?: ReactNode
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
  longCapacity,
  shortCapacity,
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
  const barLabel = hasCapitalComposition
    ? `Vault capital: Junior ${juniorLabel}; Senior ${seniorLabel}`
    : 'Vault capital breakdown unavailable'

  return (
    <div className="w-full text-left">
      <dl className="grid grid-cols-1 gap-3 border-b border-brand-border/20 pb-4 text-xs sm:grid-cols-2 sm:gap-6">
        <div className="flex min-w-0 items-baseline justify-between gap-3">
          <dt className="text-content-secondary">Estimated LONG trading capacity</dt>
          <dd className="shrink-0 font-semibold text-content-primary">{longCapacity ?? '--'}</dd>
        </div>
        <div className="flex min-w-0 items-baseline justify-between gap-3 sm:border-l sm:border-brand-border/20 sm:pl-6">
          <dt className="text-content-secondary">Estimated SHORT trading capacity</dt>
          <dd className="shrink-0 font-semibold text-content-primary">{shortCapacity ?? '--'}</dd>
        </div>
      </dl>

      <div className="mt-4 flex flex-wrap items-center justify-between gap-2">
        <h3 className="text-xs font-semibold text-content-primary">How losses are shared</h3>
        <p className="text-[11px] text-content-secondary">Losses affect Junior before Senior</p>
      </div>

      <div
        className="mt-3 flex h-6 w-full overflow-hidden bg-app-bg/70"
        role="img"
        aria-label={barLabel}
      >
        <div
          className="flex min-w-0 items-center justify-center overflow-hidden bg-brand-orange text-[10px] font-semibold text-content-primary transition-[width] duration-200 motion-reduce:transition-none"
          style={{ width: barWidth(juniorSharePercent) }}
        >
          {juniorSharePercent !== undefined && juniorSharePercent >= 16 ? (
            <span className="block w-full min-w-0 truncate px-1 text-center">Junior · {juniorLabel}</span>
          ) : null}
        </div>
        <div
          className="flex min-w-0 items-center justify-center overflow-hidden bg-[#FFAB96] text-[10px] font-semibold text-app-bg transition-[width] duration-200 motion-reduce:transition-none"
          style={{ width: barWidth(seniorSharePercent) }}
        >
          {seniorSharePercent !== undefined && seniorSharePercent >= 16 ? (
            <span className="block w-full min-w-0 truncate px-1 text-center">Senior · {seniorLabel}</span>
          ) : null}
        </div>
      </div>

      <dl className="mt-2 grid grid-cols-1 gap-2 text-xs sm:grid-cols-2 sm:gap-6">
        <div className="flex min-w-0 items-baseline justify-between gap-3">
          <dt className="flex min-w-0 items-center gap-2 text-content-secondary">
            <span className="h-2 w-2 shrink-0 bg-brand-orange" aria-hidden="true" />
            <span>Junior · absorbs losses first</span>
          </dt>
          <dd className="shrink-0 font-semibold text-content-primary">{juniorPrincipal ?? '--'}</dd>
        </div>
        <div className="flex min-w-0 items-baseline justify-between gap-3 sm:border-l sm:border-brand-border/20 sm:pl-6">
          <dt className="flex min-w-0 items-center gap-2 text-content-secondary">
            <span className="h-2 w-2 shrink-0 bg-[#FFAB96]" aria-hidden="true" />
            <span>Senior · protected by Junior</span>
          </dt>
          <dd className="shrink-0 font-semibold text-content-primary">{seniorPrincipal ?? '--'}</dd>
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
        <p>Available trading capacity is an estimate and can change before a trade is submitted. Withdrawals depend on the liquidity available at each hourly processing time.</p>
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
