import { type ReactNode } from 'react'
import type { PerpsOracleFreshness } from '../utils/perps'
import { TokenAmount, Tooltip } from './ui'

export interface PerpsInstrumentStat {
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
  tooltip?: ReactNode
  tooltipClassName?: string
  tooltipPosition?: 'top' | 'bottom' | 'left' | 'right'
}

export interface PerpsInstrumentPanelProps {
  icon?: string
  name?: string
  description?: string
  stats?: PerpsInstrumentStat[]
}

const DEFAULT_STATS: PerpsInstrumentStat[] = [
  { label: 'plDXY Perp price', value: '1.0091', freshness: 'fresh' },
  { label: '24h change', value: '-0.16%', tone: 'negative' },
  { label: '24h volume', value: <TokenAmount amount="2.4M" /> },
  { label: 'Long open interest', value: <TokenAmount amount="10.8M" />, tone: 'positive' },
  { label: 'Short open interest', value: <TokenAmount amount="7.9M" />, tone: 'negative' },
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
    <dd className={`mt-2 flex items-center gap-2 text-2xl font-semibold ${statToneClass(stat.tone)}`}>
      {stat.freshness ? (
        <Tooltip content={stat.freshnessTooltip ?? `Oracle ${stat.freshness}`} position="bottom">
          <span
            className={`h-2 w-2 shrink-0 rounded-full bg-current ${freshnessToneClass(stat.freshness)}`}
            aria-label={`Oracle ${stat.freshness}`}
            tabIndex={0}
          />
        </Tooltip>
      ) : null}
      <span>{stat.value}</span>
    </dd>
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
}: PerpsInstrumentPanelProps) {
  return (
    <section className="bg-surface-panel border border-brand-border/30 overflow-visible">
      <div className="flex flex-col gap-4 px-5 py-4 lg:flex-row lg:items-center">
        <div className="flex min-w-[220px] shrink-0 items-center gap-4">
          <DxyInstrumentMark />
          <div className="min-w-0">
            <h2 className="text-2xl font-semibold text-content-primary">{name}</h2>
            <p className="mt-1 text-sm text-content-secondary">{description}</p>
          </div>
        </div>

        <div className="hidden h-14 w-px shrink-0 bg-brand-border/25 lg:block" />

        <dl className="grid flex-1 grid-cols-2 gap-x-5 gap-y-4 md:grid-cols-3 xl:grid-cols-7">
          {stats.map((stat) => (
            <div key={stat.label} className="min-w-0">
              <dt className="flex min-w-0 items-center gap-1.5 text-xs font-medium text-content-secondary">
                <span className="min-w-0 truncate" title={stat.label}>{stat.label}</span>
                {stat.tooltip ? (
                  <Tooltip
                    content={stat.tooltip}
                    position={stat.tooltipPosition ?? 'bottom'}
                    className={stat.tooltipClassName ?? 'max-w-80 whitespace-normal'}
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
          ))}
        </dl>
      </div>
    </section>
  )
}
