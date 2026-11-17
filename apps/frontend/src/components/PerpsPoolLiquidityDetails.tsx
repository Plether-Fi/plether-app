import { PERPS_SETTLEMENT_TOKEN_LABEL } from '../contracts/perpsAddresses'
import type { ReactNode } from 'react'
import { formatPerpsAssets } from '../utils/perps'
import type { PerpsSeniorCapitalStatus } from '../utils/perpsPoolCapital'
import { DocsLink, TokenAmount, type TooltipDocsLink } from './ui'

export interface PerpsPoolLiquidityDetailsProps {
  poolAssets?: bigint
  freeAssets?: bigint
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
  return <TokenAmount token={PERPS_SETTLEMENT_TOKEN_LABEL} amount={value > 0n && value < 10_000n ? '<0.01' : formatPerpsAssets(value)} />
}

function reservedShare(poolAssets: bigint | undefined, reservedAssets: bigint | undefined): string | undefined {
  if (poolAssets === undefined || poolAssets <= 0n || reservedAssets === undefined) return undefined
  if (reservedAssets > 0n && reservedAssets * 1_000n < poolAssets) return '<0.1%'
  if (reservedAssets < poolAssets && reservedAssets * 1_000n > poolAssets * 999n) return '>99.9%'
  const tenths = (reservedAssets * 1_000n + poolAssets / 2n) / poolAssets
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
  poolAssets,
  freeAssets,
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
  const hasCapitalComposition = !isLoading && !isEmpty
    && juniorSharePercent !== undefined && Number.isFinite(juniorSharePercent)
    && seniorSharePercent !== undefined && Number.isFinite(seniorSharePercent)
    && juniorSharePercent >= 0 && juniorSharePercent <= 100
    && seniorSharePercent >= 0 && seniorSharePercent <= 100
  const juniorLabel = hasCapitalComposition ? displayPercent(juniorSharePercent) : '--'
  const seniorLabel = hasCapitalComposition ? displayPercent(seniorSharePercent) : '--'
  const reservedAssets = poolAssets !== undefined && freeAssets !== undefined
    && freeAssets >= 0n && poolAssets >= freeAssets
    ? poolAssets - freeAssets
    : undefined
  const reservedShareLabel = isLoading ? undefined : reservedShare(poolAssets, reservedAssets)
  const freeShareLabel = isLoading ? undefined : reservedShare(poolAssets, reservedAssets === undefined ? undefined : freeAssets)
  const reservedPercent = reservedShareLabel !== undefined && poolAssets !== undefined && reservedAssets !== undefined
    ? Number(reservedAssets * 1_000_000n / poolAssets) / 10_000
    : undefined
  const barLabel = hasCapitalComposition
    ? `Vault capital: Senior ${seniorLabel}; Junior ${juniorLabel}`
    : 'Vault capital breakdown unavailable'

  return (
    <div className="@container w-full min-w-0 text-left">
      <div className="grid min-w-0 gap-4 @min-[30rem]:grid-cols-2 @min-[30rem]:gap-6">
        <div className="min-w-0">
          <h3 className="text-[11px] font-medium text-content-secondary">Pool USDG</h3>
          <dl className="mt-2 space-y-2 text-xs">
            <div className="flex min-w-0 items-baseline justify-between gap-3">
              <dt className="text-content-secondary">Total pool USDG</dt>
              <dd className="shrink-0 font-medium tabular-nums text-content-primary">{poolAmount(poolAssets, isLoading)}</dd>
            </div>
            <div className="flex min-w-0 items-baseline justify-between gap-3">
              <dt className="text-content-secondary">Reserved pool USDG</dt>
              <dd className="shrink-0 font-medium tabular-nums text-content-primary">{poolAmount(reservedAssets, isLoading)}</dd>
            </div>
            <div className="flex min-w-0 items-baseline justify-between gap-3">
              <dt className="text-content-secondary">Free liquidity</dt>
              <dd className="shrink-0 font-medium tabular-nums text-content-primary">{poolAmount(freeAssets, isLoading)}</dd>
            </div>
          </dl>
          {reservedShareLabel !== undefined && freeShareLabel !== undefined && reservedPercent !== undefined ? (
            <div className="mt-3">
              <div className="flex justify-between gap-3 text-[11px] leading-4 text-content-secondary">
                <span>Reserved <span className="text-content-primary">{reservedShareLabel}</span></span>
                <span>Free <span className="text-content-primary">{freeShareLabel}</span></span>
              </div>
              <div
                className="mt-2 flex h-1.5 overflow-hidden bg-app-bg/70"
                role="img"
                aria-label={`Pool USDG: ${reservedShareLabel} reserved; ${freeShareLabel} free`}
              >
                <div className="h-full bg-content-secondary/50" style={{ width: barWidth(reservedPercent) }} />
                <div className="h-full bg-positive/70" style={{ width: barWidth(100 - reservedPercent) }} />
              </div>
            </div>
          ) : null}
        </div>

        <div className="min-w-0 border-t border-brand-border/20 pt-3 @min-[30rem]:border-l @min-[30rem]:border-t-0 @min-[30rem]:pl-6 @min-[30rem]:pt-0">
          <h3 className="text-[11px] font-medium text-content-secondary">Vault capital</h3>
          <dl className="mt-2 space-y-2 text-xs">
            <div className="flex min-w-0 items-baseline justify-between gap-3">
              <dt className="inline-flex min-w-0 flex-wrap items-center gap-x-2 text-content-secondary">
                <span className="h-1.5 w-1.5 shrink-0 bg-[#FFAB96]" aria-hidden="true" />
                <span>Senior</span>
                {hasCapitalComposition ? <span className="text-[11px]">{seniorLabel}</span> : null}
              </dt>
              <dd className="shrink-0 font-medium tabular-nums text-content-primary">{isLoading ? '...' : seniorPrincipal ?? '--'}</dd>
            </div>
            <div className="flex min-w-0 items-baseline justify-between gap-3">
              <dt className="inline-flex min-w-0 flex-wrap items-center gap-x-2 text-content-secondary">
                <span className="h-1.5 w-1.5 shrink-0 bg-brand-orange" aria-hidden="true" />
                <span>Junior</span>
                {hasCapitalComposition ? <span className="text-[11px]">{juniorLabel}</span> : null}
              </dt>
              <dd className="shrink-0 font-medium tabular-nums text-content-primary">{isLoading ? '...' : juniorPrincipal ?? '--'}</dd>
            </div>
          </dl>
          {hasCapitalComposition ? (
            <div
              className="mt-3 flex h-1.5 overflow-hidden bg-app-bg/70"
              role="img"
              aria-label={barLabel}
            >
              <div className="h-full bg-[#FFAB96]" style={{ width: barWidth(seniorSharePercent) }} />
              <div className="h-full bg-brand-orange" style={{ width: barWidth(juniorSharePercent) }} />
            </div>
          ) : null}
          <div className="mt-3 text-[11px] leading-4">
            <CapitalStatus
              seniorStatus={seniorStatus}
              seniorImpairment={seniorImpairment}
              isJuniorExhausted={isJuniorExhausted}
              isEmpty={isEmpty}
              isLoading={isLoading}
            />
            {!isLoading && !isEmpty && seniorStatus !== 'impaired' && isJuniorExhausted ? (
              <p className="mt-1 text-content-secondary">Further pool losses would reduce Senior's value</p>
            ) : null}
          </div>
        </div>
      </div>

      <div className="mt-4 flex flex-wrap items-start justify-between gap-2 border-t border-brand-border/20 pt-3 text-[11px] leading-4 text-content-secondary">
        <p className="min-w-0 flex-1">
          Free liquidity alone does not set trade size. Vault balances are accounting values;
          Junior absorbs losses first. Withdrawals depend on liquidity at hourly processing.
        </p>
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
