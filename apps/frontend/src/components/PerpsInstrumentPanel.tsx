import { useId, useState, type ReactNode } from 'react'
import type { PerpsOracleFreshness } from '../utils/perps'
import type { PerpsDirectionalLimitSide } from '../utils/perpsDirectionalLimit'
import { INFO_TOOLTIP_PANEL_CLASS_NAME, TokenAmount, Tooltip, type TooltipDocsLink } from './ui'

export interface PerpsDirectionalLimitDetails {
  usagePercent?: number
  side?: PerpsDirectionalLimitSide
  netExposure?: ReactNode
  limit?: ReactNode
  isLoading?: boolean
}

interface PerpsInstrumentStatBase {
  label: string
  value?: ReactNode
  values?: {
    label: string
    value: ReactNode
    tone?: 'default' | 'positive' | 'negative'
  }[]
  tone?: 'default' | 'positive' | 'negative'
  freshness?: PerpsOracleFreshness
  freshnessTooltip?: string
  directionalLimit?: PerpsDirectionalLimitDetails
}

export type PerpsInstrumentStat = PerpsInstrumentStatBase & (
  | {
      tooltip?: undefined
      tooltipDocsLink?: never
      tooltipClassName?: never
      tooltipPosition?: never
    }
  | {
      tooltip: ReactNode
      tooltipDocsLink: TooltipDocsLink
      tooltipClassName?: string
      tooltipPosition?: 'top' | 'bottom' | 'left' | 'right'
    }
)

export interface PerpsInstrumentPanelProps {
  icon?: string
  name?: string
  description?: string
  stats?: PerpsInstrumentStat[]
  directionalLimitDetailsExpanded?: boolean
}

const DEFAULT_STATS: PerpsInstrumentStat[] = [
  { label: 'plDXY Perp price', value: '1.0091', freshness: 'fresh' },
  { label: '24h change', value: '-0.16%', tone: 'negative' },
  { label: '24h volume', value: <TokenAmount amount="2.4M" /> },
  {
    label: 'Directional limit used',
    directionalLimit: {
      usagePercent: 87,
      side: 'long',
      netExposure: <TokenAmount amount="3.07M" />,
      limit: <TokenAmount amount="3.53M" />,
    },
  },
  { label: 'Pool liquidity', value: <TokenAmount amount="6.3M" /> },
  { label: 'Cost of carry', value: '5.24%' },
]

function statToneClass(tone: PerpsInstrumentStat['tone']): string {
  if (tone === 'positive') return 'text-positive'
  if (tone === 'negative') return 'text-brand-orange'
  return 'text-content-primary'
}

function freshnessToneClass(freshness: NonNullable<PerpsInstrumentStat['freshness']>): string {
  if (freshness === 'fresh') return 'text-positive'
  if (freshness === 'market-closed') return 'text-warning'
  if (freshness === 'stale') return 'text-brand-orange'
  return 'text-[#FFAB96]'
}

function StatValue({ stat }: { stat: PerpsInstrumentStat }) {
  if (stat.values) {
    return (
      <dd className="mt-2 flex flex-wrap gap-x-4 gap-y-1">
        {stat.values.map((item) => (
          <span key={item.label} className="min-w-0">
            <span className={`text-xl font-semibold ${statToneClass(item.tone)}`}>{item.value}</span>
          </span>
        ))}
      </dd>
    )
  }

  return (
    <dd className={`mt-2 flex min-w-0 items-center gap-2 text-xl font-semibold 2xl:text-2xl ${statToneClass(stat.tone)}`}>
      {stat.freshness ? (
        <Tooltip content={stat.freshnessTooltip ?? `Oracle ${stat.freshness}`} position="bottom">
          <span
            className={`h-2 w-2 shrink-0 rounded-full bg-current ${freshnessToneClass(stat.freshness)}`}
            aria-label={`Oracle ${stat.freshness}`}
            tabIndex={0}
          />
        </Tooltip>
      ) : null}
      <span className="min-w-0 max-w-full">{stat.value}</span>
    </dd>
  )
}

function InstrumentStat({ stat }: { stat: PerpsInstrumentStat }) {
  return (
    <div className="min-w-0">
      <dt className="flex min-w-0 items-center gap-1.5 text-xs font-medium text-content-secondary">
        <span className="min-w-0 truncate" title={stat.label}>{stat.label}</span>
        {stat.tooltip ? (
          <Tooltip
            content={stat.tooltip}
            position={stat.tooltipPosition ?? 'bottom'}
            className={stat.tooltipClassName ?? INFO_TOOLTIP_PANEL_CLASS_NAME}
            docsLink={stat.tooltipDocsLink}
          >
            <span
              className="inline-flex h-3.5 w-3.5 shrink-0 items-center justify-center rounded-full border border-current text-[9px] font-semibold leading-none text-content-secondary/80 transition-colors hover:text-[#FFAB96]"
              aria-label={`${stat.label} details`}
              tabIndex={0}
            >
              i
            </span>
          </Tooltip>
        ) : null}
      </dt>
      <StatValue stat={stat} />
    </div>
  )
}

function directionalBadgeClass(side: PerpsDirectionalLimitSide | undefined): string {
  if (side === 'long') return 'border-positive/40 bg-positive/10 text-positive'
  if (side === 'short') return 'border-brand-orange/40 bg-brand-orange/10 text-brand-orange'
  return 'border-brand-border/30 bg-app-bg/30 text-content-secondary'
}

function directionalBadgeLabel(
  side: PerpsDirectionalLimitSide | undefined,
  isLoading: boolean
): string {
  if (isLoading) return 'CHECKING'
  if (side === 'long') return 'LONG-HEAVY'
  if (side === 'short') return 'SHORT-HEAVY'
  if (side === 'balanced') return 'BALANCED'
  return 'UNAVAILABLE'
}

function directionalBarClass(usagePercent: number | undefined): string {
  if (usagePercent !== undefined && usagePercent >= 100) return 'bg-brand-orange'
  if (usagePercent !== undefined && usagePercent >= 70) return 'bg-warning'
  return 'bg-positive'
}

function directionalConstraintText(
  details: PerpsDirectionalLimitDetails,
  displayUsagePercent: number | undefined
): string | undefined {
  if (
    details.isLoading ||
    displayUsagePercent === undefined ||
    displayUsagePercent < 100 ||
    details.side === undefined ||
    details.side === 'balanced'
  ) return undefined

  const heavySide = details.side.toUpperCase()
  const rebalancingSide = details.side === 'long' ? 'SHORT' : 'LONG'
  return `New ${heavySide}s are unavailable. ${rebalancingSide}s can still rebalance the market.`
}

function DirectionalLimitStat({
  stat,
  trailingStats,
  forceExpanded = false,
}: {
  stat: PerpsInstrumentStat
  trailingStats: PerpsInstrumentStat[]
  forceExpanded?: boolean
}) {
  const details = stat.directionalLimit
  const detailsId = useId()
  const [isHovered, setIsHovered] = useState(false)
  const [isFocused, setIsFocused] = useState(false)

  if (!details) return null

  const hasUsage = details.usagePercent !== undefined && Number.isFinite(details.usagePercent)
  const displayUsagePercent = hasUsage ? Math.max(0, Math.round(details.usagePercent ?? 0)) : undefined
  const progressPercent = hasUsage ? Math.min(100, Math.max(0, details.usagePercent ?? 0)) : 0
  const isExpanded = forceExpanded || isHovered || isFocused
  const sideLabel = details.side === 'balanced' ? 'Net exposure' : `Net ${details.side?.toUpperCase() ?? ''} exposure`
  const valueLabel = details.isLoading
    ? '...'
    : displayUsagePercent === undefined
      ? '--'
      : `${displayUsagePercent.toString()}%`
  const constraintText = directionalConstraintText(details, displayUsagePercent)

  return (
    <>
      <div
        className="min-w-0 sm:col-span-2"
        onMouseEnter={() => { setIsHovered(true) }}
        onMouseLeave={() => { setIsHovered(false) }}
      >
        <dt className="text-xs font-medium text-content-secondary">{stat.label}</dt>
        <dd>
          <button
            type="button"
            className="mt-2 flex min-w-0 max-w-full items-center gap-2 text-left"
            aria-label={`${stat.label} details`}
            aria-controls={detailsId}
            aria-expanded={isExpanded}
            onFocus={() => { setIsFocused(true) }}
            onBlur={() => { setIsFocused(false) }}
          >
            <span className={`text-xl font-semibold 2xl:text-2xl ${
              displayUsagePercent !== undefined && displayUsagePercent >= 100
                ? 'text-brand-orange'
                : 'text-content-primary'
            }`}>
              {valueLabel}
            </span>
            <span
              className={`whitespace-nowrap border px-2 py-1 text-[10px] font-semibold tracking-[0.08em] ${directionalBadgeClass(details.side)}`}
            >
              {directionalBadgeLabel(details.side, details.isLoading ?? false)}
            </span>
          </button>
        </dd>
      </div>

      <div className={isExpanded ? 'col-span-full grid min-w-0' : 'contents'}>
        <div
          className={isExpanded
            ? 'invisible col-start-1 row-start-1 grid min-w-0 grid-cols-[repeat(auto-fit,minmax(min(8.5rem,100%),1fr))] gap-x-3 gap-y-4 2xl:gap-x-4'
            : 'contents'}
          aria-hidden={isExpanded || undefined}
        >
          {trailingStats.map((trailingStat) => (
            <InstrumentStat key={trailingStat.label} stat={trailingStat} />
          ))}
        </div>

        <div
          className={isExpanded
            ? 'pointer-events-none z-10 col-start-1 row-start-1 min-w-0 bg-surface-panel'
            : 'hidden'}
        >
          <dt className="sr-only">{stat.label} details</dt>
          <dd
            id={detailsId}
            aria-hidden={!isExpanded}
          >
            <div className="border-t border-brand-border/20 pt-3">
              <div className="relative h-1.5 overflow-hidden rounded-full bg-app-bg/70">
                <div
                  className={`h-full rounded-full transition-[width] duration-200 motion-reduce:transition-none ${directionalBarClass(details.usagePercent)}`}
                  style={{ width: `${progressPercent.toString()}%` }}
                />
              </div>
              <div className="mt-1 flex items-center justify-between gap-3 text-[11px] text-content-secondary">
                <span>{valueLabel} used</span>
                <span>
                  {displayUsagePercent === undefined
                    ? '--'
                    : `${Math.max(0, 100 - displayUsagePercent).toString()}% remaining`}
                </span>
              </div>

              <div className="mt-3 grid grid-cols-2 gap-4 text-xs">
                <div className="min-w-0">
                  <div className="text-content-secondary">{sideLabel}</div>
                  <div className="mt-0.5 font-semibold text-content-primary">{details.netExposure ?? '--'}</div>
                </div>
                <div className="min-w-0 text-right">
                  <div className="text-content-secondary">Directional limit</div>
                  <div className="mt-0.5 font-semibold text-content-primary">{details.limit ?? '--'}</div>
                </div>
              </div>

              {constraintText ? (
                <p className="mt-3 text-xs leading-5 text-content-secondary">{constraintText}</p>
              ) : null}
            </div>
          </dd>
        </div>
      </div>
    </>
  )
}

function DxyInstrumentMark() {
  return (
    <div
      className="flex h-12 w-12 shrink-0 items-center justify-center border border-brand-border/50 bg-app-bg"
      aria-hidden="true"
    >
      <div className="relative flex h-4 w-4 items-center justify-center bg-[#FFAB96]">
        <div className="h-3 w-3 rounded-full bg-app-bg" />
      </div>
    </div>
  )
}

export function PerpsInstrumentPanel({
  name = 'plDXY Perp',
  description = 'Dollar Index Perpetual',
  stats = DEFAULT_STATS,
  directionalLimitDetailsExpanded = false,
}: PerpsInstrumentPanelProps) {
  const hasDirectionalLimit = stats.some((stat) => stat.directionalLimit !== undefined)
  const directionalLimitIndex = stats.findIndex((stat) => stat.directionalLimit !== undefined)
  const visibleStats = directionalLimitIndex === -1
    ? stats
    : stats.slice(0, directionalLimitIndex + 1)
  const trailingStats = directionalLimitIndex === -1
    ? []
    : stats.slice(directionalLimitIndex + 1)

  return (
    <section className="bg-surface-panel border border-brand-border/30 overflow-visible">
      <div className={`flex flex-col gap-4 px-3 py-3 sm:px-5 sm:py-4 lg:flex-row ${
        hasDirectionalLimit ? 'lg:items-start' : 'lg:items-center'
      }`}>
        <div className="flex min-w-0 shrink-0 items-center gap-3 sm:min-w-[200px]">
          <DxyInstrumentMark />
          <div className="min-w-0">
            <h2 className="text-xl font-semibold text-content-primary sm:text-2xl">{name}</h2>
            <p className="mt-1 text-sm text-content-secondary">{description}</p>
          </div>
        </div>

        <div className="hidden h-14 w-px shrink-0 bg-brand-border/25 lg:block" />

        <dl className="grid flex-1 grid-cols-[repeat(auto-fit,minmax(min(8.5rem,100%),1fr))] gap-x-3 gap-y-4 2xl:gap-x-4">
          {visibleStats.map((stat) => stat.directionalLimit ? (
            <DirectionalLimitStat
              key={stat.label}
              stat={stat}
              trailingStats={trailingStats}
              forceExpanded={directionalLimitDetailsExpanded}
            />
          ) : (
            <InstrumentStat key={stat.label} stat={stat} />
          ))}
        </dl>
      </div>
    </section>
  )
}
