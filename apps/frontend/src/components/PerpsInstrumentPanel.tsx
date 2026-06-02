import { type ReactNode } from 'react'
import { TokenAmount } from './ui'

export interface PerpsInstrumentStat {
  label: string
  value?: ReactNode
  values?: {
    label: string
    value: ReactNode
    tone?: 'default' | 'positive' | 'negative'
  }[]
  tone?: 'default' | 'positive' | 'negative'
}

export interface PerpsInstrumentPanelProps {
  icon?: string
  name?: string
  description?: string
  stats?: PerpsInstrumentStat[]
}

const DEFAULT_STATS: PerpsInstrumentStat[] = [
  { label: 'Oracle price', value: '0.9909' },
  { label: '24h change', value: '-0.16%', tone: 'negative' },
  { label: '24h volume', value: <TokenAmount amount="2.4M" /> },
  {
    label: 'Open interest (LONG/SHORT)',
    values: [
      { label: 'long', value: <TokenAmount amount="10.8M" />, tone: 'positive' },
      { label: 'short', value: <TokenAmount amount="7.9M" />, tone: 'negative' },
    ],
  },
  { label: 'Available liquidity', value: <TokenAmount amount="6.3M" /> },
  { label: 'Cost of carry', value: '5.24%' },
]

function statToneClass(tone: PerpsInstrumentStat['tone']): string {
  if (tone === 'positive') return 'text-cyber-neon-green'
  if (tone === 'negative') return 'text-cyber-electric-fuchsia'
  return 'text-cyber-text-primary'
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

  return <dd className={`mt-2 text-2xl font-semibold ${statToneClass(stat.tone)}`}>{stat.value}</dd>
}

export function PerpsInstrumentPanel({
  icon = 'token',
  name = 'DXY Perp',
  description = 'DXY Basket Perpetual',
  stats = DEFAULT_STATS,
}: PerpsInstrumentPanelProps) {
  return (
    <section className="bg-cyber-surface-dark border border-cyber-border-glow/30 shadow-lg shadow-cyber-border-glow/10 overflow-hidden">
      <div className="flex flex-col gap-4 px-5 py-4 lg:flex-row lg:items-center">
        <div className="flex min-w-[220px] shrink-0 items-center gap-4">
          <div className="flex h-12 w-12 shrink-0 items-center justify-center border border-cyber-bright-blue/50 bg-cyber-bg/50 text-cyber-bright-blue">
            <span className="material-symbols-outlined text-3xl">{icon}</span>
          </div>
          <div className="min-w-0">
            <h2 className="text-2xl font-semibold text-cyber-text-primary">{name}</h2>
            <p className="mt-1 text-sm text-cyber-text-secondary">{description}</p>
          </div>
        </div>

        <div className="hidden h-14 w-px shrink-0 bg-cyber-border-glow/25 lg:block" />

        <dl className="grid flex-1 grid-cols-2 gap-x-5 gap-y-4 md:grid-cols-3 xl:grid-cols-6">
          {stats.map((stat) => (
            <div key={stat.label} className="min-w-0">
              <dt className="text-xs font-medium uppercase text-cyber-text-secondary">{stat.label}</dt>
              <StatValue stat={stat} />
            </div>
          ))}
        </dl>
      </div>
    </section>
  )
}
