import {
  useEffect,
  useId,
  useRef,
  useState,
  type FocusEventHandler,
  type MouseEventHandler,
  type ReactNode,
  type RefObject,
} from 'react'
import { DOCS_LINKS } from '../config/docs'
import type { PerpsOracleFreshness } from '../utils/perps'
import type { PerpsDirectionalLimitSide } from '../utils/perpsDirectionalLimit'
import { INFO_TOOLTIP_PANEL_CLASS_NAME, TokenAmount, Tooltip, type TooltipDocsLink } from './ui'

export interface PerpsDirectionalLimitDetails {
  usagePercent?: number
  side?: PerpsDirectionalLimitSide
  totalExposure?: ReactNode
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
  hoverDetails?: ReactNode
  hoverDetailsLabel?: string
  hoverDetailsType?: 'price' | 'pool-liquidity'
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
  priceDetailsExpanded?: boolean
  poolLiquidityDetailsExpanded?: boolean
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
      totalExposure: <TokenAmount amount="8.36M" />,
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

function FreshnessIndicator({ stat }: { stat: PerpsInstrumentStat }) {
  if (!stat.freshness) return null

  return (
    <Tooltip content={stat.freshnessTooltip ?? `Oracle ${stat.freshness}`} position="bottom">
      <span
        className={`h-2 w-2 shrink-0 rounded-full bg-current ${freshnessToneClass(stat.freshness)}`}
        aria-label={`Oracle ${stat.freshness}`}
        tabIndex={0}
      />
    </Tooltip>
  )
}

function StatValueContent({ stat }: { stat: PerpsInstrumentStat }) {
  return (
    <>
      <FreshnessIndicator stat={stat} />
      <span className="min-w-0 max-w-full">{stat.value}</span>
    </>
  )
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
      <StatValueContent stat={stat} />
    </dd>
  )
}

function InstrumentStatLabel({ stat }: { stat: PerpsInstrumentStat }) {
  return (
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
  )
}

function InstrumentStat({ stat }: { stat: PerpsInstrumentStat }) {
  return (
    <div className="min-w-0">
      <InstrumentStatLabel stat={stat} />
      <StatValue stat={stat} />
    </div>
  )
}

interface InstrumentDetailsOverlayProps {
  id: string
  label: string
  isExpanded: boolean
  interactive?: boolean
  overlayRef?: RefObject<HTMLDivElement | null>
  onMouseEnter?: MouseEventHandler<HTMLDivElement>
  onMouseLeave?: MouseEventHandler<HTMLDivElement>
  onFocusCapture?: FocusEventHandler<HTMLDivElement>
  onBlurCapture?: FocusEventHandler<HTMLDivElement>
  children: ReactNode
}

function InstrumentDetailsOverlay({
  id,
  label,
  isExpanded,
  interactive = false,
  overlayRef,
  onMouseEnter,
  onMouseLeave,
  onFocusCapture,
  onBlurCapture,
  children,
}: InstrumentDetailsOverlayProps) {
  return (
    <div
      ref={overlayRef}
      className={`${interactive && isExpanded ? 'pointer-events-auto' : 'pointer-events-none'} absolute -inset-x-px top-full z-20 grid min-w-0 overflow-hidden border-x border-b bg-surface-panel px-3 transition-[grid-template-rows,opacity,transform,box-shadow,border-color,padding-bottom] duration-200 ease-out motion-reduce:transform-none motion-reduce:transition-none sm:px-5 ${
        isExpanded
          ? 'grid-rows-[1fr] translate-y-0 border-brand-border/30 pb-3 opacity-100 shadow-[0_20px_32px_-16px_rgba(0,0,0,0.8)] sm:pb-4'
          : 'grid-rows-[0fr] -translate-y-1 border-transparent pb-0 opacity-0 shadow-none'
      }`}
      onMouseEnter={onMouseEnter}
      onMouseLeave={onMouseLeave}
      onFocusCapture={onFocusCapture}
      onBlurCapture={onBlurCapture}
    >
      <dt className="sr-only">{label}</dt>
      <dd
        id={id}
        aria-hidden={!isExpanded}
        inert={!isExpanded}
        className="min-h-0 overflow-hidden"
      >
        <div className="pt-3">{children}</div>
      </dd>
    </div>
  )
}

const HOVER_DETAILS_CLOSE_DELAY_MS = 250

function HoverDetailsStat({
  stat,
  forceExpanded = false,
  isSuppressed = false,
  onActivate,
}: {
  stat: PerpsInstrumentStat
  forceExpanded?: boolean
  isSuppressed?: boolean
  onActivate: () => void
}) {
  const detailsId = useId()
  const metricRef = useRef<HTMLDivElement>(null)
  const overlayRef = useRef<HTMLDivElement>(null)
  const closeTimerRef = useRef<number | undefined>(undefined)
  const isPointerInsideRef = useRef(false)
  const isFocusInsideRef = useRef(false)
  const [isInteracting, setIsInteracting] = useState(false)
  const [isPinned, setIsPinned] = useState(false)

  const clearCloseTimer = () => {
    if (closeTimerRef.current === undefined) return
    window.clearTimeout(closeTimerRef.current)
    closeTimerRef.current = undefined
  }

  const openFromPointer = () => {
    onActivate()
    isPointerInsideRef.current = true
    clearCloseTimer()
    setIsInteracting(true)
  }

  const openFromFocus = () => {
    onActivate()
    isFocusInsideRef.current = true
    clearCloseTimer()
    setIsInteracting(true)
  }

  const scheduleClose = () => {
    clearCloseTimer()
    closeTimerRef.current = window.setTimeout(() => {
      if (!isPointerInsideRef.current && !isFocusInsideRef.current) {
        setIsInteracting(false)
      }
    }, HOVER_DETAILS_CLOSE_DELAY_MS)
  }

  const leaveFromPointer = () => {
    isPointerInsideRef.current = false
    scheduleClose()
  }

  const leaveFromFocus = () => {
    isFocusInsideRef.current = false
    scheduleClose()
  }

  useEffect(() => () => {
    clearCloseTimer()
  }, [])

  useEffect(() => {
    if (!isPinned) return undefined

    const closeOnOutsideInteraction = (event: PointerEvent | FocusEvent) => {
      const target = event.target
      if (!(target instanceof Node)) return
      if (metricRef.current?.contains(target) || overlayRef.current?.contains(target)) return

      isPointerInsideRef.current = false
      isFocusInsideRef.current = false
      clearCloseTimer()
      setIsInteracting(false)
      setIsPinned(false)
    }

    document.addEventListener('pointerdown', closeOnOutsideInteraction)
    document.addEventListener('focusin', closeOnOutsideInteraction)
    return () => {
      document.removeEventListener('pointerdown', closeOnOutsideInteraction)
      document.removeEventListener('focusin', closeOnOutsideInteraction)
    }
  }, [isPinned])

  if (!stat.hoverDetails) return <InstrumentStat stat={stat} />

  const isExpanded = !isSuppressed && (forceExpanded || isPinned || isInteracting)
  const detailsLabel = stat.hoverDetailsLabel ?? `${stat.label} basket components`

  return (
    <>
      <div ref={metricRef} className="min-w-0">
        <InstrumentStatLabel stat={stat} />
        <dd className={`mt-2 flex min-w-0 items-center gap-2 text-xl font-semibold 2xl:text-2xl ${statToneClass(stat.tone)}`}>
          <FreshnessIndicator stat={stat} />
          <button
            type="button"
            className="min-w-0 max-w-full text-left"
            aria-label={detailsLabel}
            aria-controls={detailsId}
            aria-expanded={isExpanded}
            onMouseEnter={openFromPointer}
            onMouseLeave={leaveFromPointer}
            onFocus={openFromFocus}
            onBlur={leaveFromFocus}
            onClick={() => { setIsPinned((value) => !value) }}
          >
            <span className="min-w-0 max-w-full">{stat.value}</span>
          </button>
        </dd>
      </div>

      <InstrumentDetailsOverlay
        id={detailsId}
        label={detailsLabel}
        isExpanded={isExpanded}
        interactive
        overlayRef={overlayRef}
        onMouseEnter={openFromPointer}
        onMouseLeave={leaveFromPointer}
        onFocusCapture={openFromFocus}
        onBlurCapture={leaveFromFocus}
      >
        {stat.hoverDetails}
      </InstrumentDetailsOverlay>
    </>
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

function totalExposureLabel(side: PerpsDirectionalLimitSide | undefined): string {
  if (side === 'long') return 'Total LONG exposure'
  if (side === 'short') return 'Total SHORT exposure'
  if (side === 'balanced') return 'Exposure per side'
  return 'Total exposure'
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
  forceExpanded = false,
  isSuppressed = false,
  onActivate,
}: {
  stat: PerpsInstrumentStat
  forceExpanded?: boolean
  isSuppressed?: boolean
  onActivate: () => void
}) {
  const details = stat.directionalLimit
  const detailsId = useId()
  const metricRef = useRef<HTMLDivElement>(null)
  const overlayRef = useRef<HTMLDivElement>(null)
  const [isHovered, setIsHovered] = useState(false)
  const [isFocused, setIsFocused] = useState(false)

  if (!details) return null

  const hasUsage = details.usagePercent !== undefined && Number.isFinite(details.usagePercent)
  const displayUsagePercent = hasUsage ? Math.max(0, Math.round(details.usagePercent ?? 0)) : undefined
  const progressPercent = hasUsage ? Math.min(100, Math.max(0, details.usagePercent ?? 0)) : 0
  const isExpanded = !isSuppressed && (forceExpanded || isHovered || isFocused)
  const totalSideLabel = totalExposureLabel(details.side)
  const sideLabel = details.side === 'balanced' ? 'Net exposure' : `Net ${details.side?.toUpperCase() ?? ''} exposure`
  const valueLabel = details.isLoading
    ? '...'
    : displayUsagePercent === undefined
      ? '--'
      : `${displayUsagePercent.toString()}%`
  const usedLabel = `${valueLabel} used`
  const remainingLabel = displayUsagePercent === undefined
    ? 'Remaining unavailable'
    : `${Math.max(0, 100 - displayUsagePercent).toString()}% remaining`
  const constraintText = directionalConstraintText(details, displayUsagePercent)
  const isInsideDetails = (target: EventTarget | null) => (
    target instanceof Node && (
      metricRef.current?.contains(target) === true ||
      overlayRef.current?.contains(target) === true
    )
  )

  return (
    <>
      <div
        ref={metricRef}
        className="min-w-0 sm:col-span-2 xl:col-span-1"
        onMouseEnter={() => {
          onActivate()
          setIsHovered(true)
        }}
        onMouseLeave={(event) => {
          if (!isInsideDetails(event.relatedTarget)) setIsHovered(false)
        }}
      >
        <dt className="text-xs font-medium text-content-secondary">{stat.label}</dt>
        <dd>
          <button
            type="button"
            className="mt-2 flex min-w-0 max-w-full items-center gap-2 text-left"
            aria-label={`${stat.label} details`}
            aria-controls={detailsId}
            aria-expanded={isExpanded}
            onFocus={() => {
              onActivate()
              setIsFocused(true)
            }}
            onBlur={(event) => {
              if (!isInsideDetails(event.relatedTarget)) setIsFocused(false)
            }}
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

      <InstrumentDetailsOverlay
        id={detailsId}
        label={`${stat.label} details`}
        isExpanded={isExpanded}
        interactive
        overlayRef={overlayRef}
        onMouseEnter={() => {
          onActivate()
          setIsHovered(true)
        }}
        onMouseLeave={(event) => {
          if (!isInsideDetails(event.relatedTarget)) setIsHovered(false)
        }}
        onFocusCapture={() => {
          onActivate()
          setIsFocused(true)
        }}
        onBlurCapture={(event) => {
          if (!isInsideDetails(event.relatedTarget)) setIsFocused(false)
        }}
      >
        <div
          className="flex h-6 w-full overflow-hidden bg-app-bg/70"
          role="img"
          aria-label={displayUsagePercent === undefined
            ? 'Directional limit usage unavailable'
            : `${usedLabel}; ${remainingLabel}`}
        >
          <div
            className={`flex min-w-0 items-center justify-center overflow-hidden text-[10px] font-semibold text-app-bg transition-[width] duration-200 motion-reduce:transition-none ${directionalBarClass(details.usagePercent)}`}
            style={{ width: `${progressPercent.toString()}%` }}
            title={usedLabel}
          >
            <span className="block w-full min-w-0 truncate px-2 text-center">{usedLabel}</span>
          </div>
          <div
            className="flex min-w-0 items-center justify-center overflow-hidden text-[10px] font-semibold text-content-secondary transition-[width] duration-200 motion-reduce:transition-none"
            style={{ width: `${(100 - progressPercent).toString()}%` }}
            title={remainingLabel}
          >
            <span className="block w-full min-w-0 truncate px-2 text-center">{remainingLabel}</span>
          </div>
        </div>

        <dl className="mt-3 grid grid-cols-1 gap-3 text-xs sm:grid-cols-3 sm:gap-4">
          <div className="min-w-0">
            <dt className="text-content-secondary">{totalSideLabel}</dt>
            <dd className="mt-0.5 font-semibold text-content-primary">{details.totalExposure ?? '--'}</dd>
          </div>
          <div className="min-w-0 sm:text-center">
            <dt className="text-content-secondary">{sideLabel}</dt>
            <dd className="mt-0.5 font-semibold text-content-primary">{details.netExposure ?? '--'}</dd>
          </div>
          <div className="min-w-0 sm:text-right">
            <dt className="text-content-secondary">Directional limit</dt>
            <dd className="mt-0.5 font-semibold text-content-primary">{details.limit ?? '--'}</dd>
          </div>
        </dl>

        {constraintText ? (
          <p className="mt-3 text-xs leading-5 text-content-secondary">{constraintText}</p>
        ) : null}

        <div className="mt-3 flex flex-wrap items-start justify-between gap-2 border-t border-brand-border/20 pt-3 text-[11px] leading-4 text-content-secondary">
          <p className="min-w-0 flex-1">
            Market-wide LONG/SHORT imbalance, not an order quote. It affects VPI and trading costs,
            can change before execution, and other checks apply.
          </p>
          <a
            href={DOCS_LINKS.virtualPriceImpact.href}
            aria-label={`Read: ${DOCS_LINKS.virtualPriceImpact.title}`}
            target="_blank"
            rel="noopener noreferrer"
            className="shrink-0 font-medium text-[#FFAB96] underline underline-offset-4 transition-colors hover:text-content-primary focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-[#FFAB96]"
          >
            Learn more
          </a>
        </div>
      </InstrumentDetailsOverlay>
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
  priceDetailsExpanded = false,
  poolLiquidityDetailsExpanded = false,
  directionalLimitDetailsExpanded = false,
}: PerpsInstrumentPanelProps) {
  const [activeDetails, setActiveDetails] = useState<'price' | 'pool-liquidity' | 'directional' | undefined>(undefined)
  const hasDirectionalLimit = stats.some((stat) => stat.directionalLimit !== undefined)

  return (
    <section className="relative z-10 overflow-visible border border-brand-border/30 bg-surface-panel">
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
          {stats.map((stat) => {
            if (stat.directionalLimit) {
              return (
                <DirectionalLimitStat
                  key={stat.label}
                  stat={stat}
                  forceExpanded={directionalLimitDetailsExpanded}
                  isSuppressed={activeDetails !== undefined && activeDetails !== 'directional'}
                  onActivate={() => { setActiveDetails('directional') }}
                />
              )
            }

            if (stat.hoverDetails) {
              const detailsType = stat.hoverDetailsType ?? 'price'
              return (
                <HoverDetailsStat
                  key={stat.label}
                  stat={stat}
                  forceExpanded={detailsType === 'pool-liquidity'
                    ? poolLiquidityDetailsExpanded
                    : priceDetailsExpanded}
                  isSuppressed={activeDetails !== undefined && activeDetails !== detailsType}
                  onActivate={() => { setActiveDetails(detailsType) }}
                />
              )
            }

            return <InstrumentStat key={stat.label} stat={stat} />
          })}
        </dl>
      </div>
    </section>
  )
}
