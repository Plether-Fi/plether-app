export interface PerpsInstrumentStat {
  label: string
  value?: string
  values?: {
    label: string
    value: string
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
  { label: '24h volume', value: '2.4M USDC' },
  {
    label: 'Open interest',
    values: [
      { label: 'Long', value: '10.8M USDC', tone: 'positive' },
      { label: 'Short', value: '7.9M USDC', tone: 'negative' },
    ],
  },
  { label: 'Available liquidity', value: '6.3M USDC' },
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
            <span className="mr-1 text-xs font-medium uppercase text-cyber-text-secondary">{item.label}</span>
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
