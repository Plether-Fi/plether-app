import { EmptyState } from './ui'

export interface ChartPoint {
  label: string
  value: number
  displayValue: string
  source?: string
}

export function AccessibleLineChart({
  title,
  points,
  valueLabel,
  emptyMessage = 'The current release has not produced enough sourced checkpoints for this chart.',
}: {
  title: string
  points: ChartPoint[]
  valueLabel: string
  emptyMessage?: string
}) {
  if (points.length < 2) {
    return (
      <div>
        <EmptyState title={`${title} history unavailable`} message={emptyMessage} />
        {points.length === 1 ? (
          <div className="border-t border-brand-border/15 p-4">
            <p className="mb-2 text-xs font-semibold text-content-secondary">One sourced checkpoint is available; two are required to draw a trend.</p>
            <ChartDataTable points={points} valueLabel={valueLabel} />
          </div>
        ) : null}
      </div>
    )
  }
  const width = 720
  const height = 220
  const padding = 22
  const min = Math.min(...points.map((point) => point.value))
  const max = Math.max(...points.map((point) => point.value))
  const spread = max - min || 1
  const x = (index: number) => padding + index * ((width - padding * 2) / (points.length - 1))
  const y = (value: number) => height - padding - ((value - min) / spread) * (height - padding * 2)
  const path = points.map((point, index) => `${index === 0 ? 'M' : 'L'} ${x(index).toFixed(2)} ${y(point.value).toFixed(2)}`).join(' ')
  return (
    <div>
      <svg viewBox={`0 0 ${String(width)} ${String(height)}`} role="img" aria-labelledby={`${slug(title)}-title ${slug(title)}-desc`} className="h-auto w-full">
        <title id={`${slug(title)}-title`}>{title}</title>
        <desc id={`${slug(title)}-desc`}>{points.length} confirmed points. Minimum {String(min)}, maximum {String(max)}.</desc>
        <path d={path} fill="none" stroke="#ffab96" strokeWidth="3" vectorEffect="non-scaling-stroke" />
        {points.map((point, index) => <circle key={`${point.label}-${String(index)}`} cx={x(index)} cy={y(point.value)} r="4" fill="#00ff99" />)}
      </svg>
      <details className="mt-3 border-t border-brand-border/15">
        <summary className="cursor-pointer pt-3 text-xs font-semibold text-content-secondary">Accessible data table</summary>
        <div className="mt-3 max-h-64 overflow-auto">
          <ChartDataTable points={points} valueLabel={valueLabel} />
        </div>
      </details>
    </div>
  )
}

function ChartDataTable({ points, valueLabel }: { points: ChartPoint[]; valueLabel: string }) {
  const showSource = points.some((point) => point.source !== undefined)
  return (
    <table className="w-full text-left text-xs">
      <thead><tr><th className="py-2">Checkpoint</th><th className="py-2">{valueLabel}</th>{showSource ? <th className="py-2">Source</th> : null}</tr></thead>
      <tbody>{points.map((point, index) => <tr key={`${point.label}-${String(index)}`} className="border-t border-brand-border/10"><td className="py-2">{point.label}</td><td className="py-2 tabular-nums">{point.displayValue}</td>{showSource ? <td className="py-2 text-content-tertiary">{point.source ?? 'Unavailable'}</td> : null}</tr>)}</tbody>
    </table>
  )
}

export function AccessibleDonutChart({
  title,
  slices,
  valueLabel,
  description,
  emptyTitle,
  emptyMessage,
}: {
  title: string
  slices: { label: string; value: number; displayValue: string }[]
  valueLabel: string
  description: string
  emptyTitle: string
  emptyMessage: string
}) {
  const total = slices.reduce((sum, slice) => sum + Math.max(0, slice.value), 0)
  if (total === 0) {
    return <EmptyState title={emptyTitle} message={emptyMessage} />
  }
  const colors = ['#00ff99', '#ff572d', '#f7d977', '#ffab96', '#b6a1ff', '#54d6ff', '#ff86c8', '#99a6b3', '#6c5360']
  const segments = slices.map((slice, index) => {
    const segment = (Math.max(0, slice.value) / total) * 276.46
    const offset = slices
      .slice(0, index)
      .reduce((sum, previous) => sum + (Math.max(0, previous.value) / total) * 276.46, 0)
    return { slice, segment, offset, index }
  })
  return (
    <div className="grid gap-6 sm:grid-cols-[13rem_1fr] sm:items-center">
      <svg viewBox="0 0 120 120" role="img" aria-labelledby={`${slug(title)}-title ${slug(title)}-desc`} className="mx-auto h-52 w-52 -rotate-90">
        <title id={`${slug(title)}-title`}>{title}</title>
        <desc id={`${slug(title)}-desc`}>{description}</desc>
        <circle cx="60" cy="60" r="44" fill="none" stroke="rgba(255,255,255,.07)" strokeWidth="20" />
        {segments.map(({ slice, segment, offset: segmentOffset, index }) => (
            <circle
              key={slice.label}
              cx="60"
              cy="60"
              r="44"
              fill="none"
              stroke={colors[index % colors.length]}
              strokeWidth="20"
              strokeDasharray={`${String(segment)} ${String(276.46 - segment)}`}
              strokeDashoffset={-segmentOffset}
            />
          ))}
      </svg>
      <table className="w-full text-left text-xs">
        <thead><tr><th className="pb-2">Recipient</th><th className="pb-2 text-right">{valueLabel}</th><th className="pb-2 text-right">Share</th></tr></thead>
        <tbody>
          {slices.map((slice, index) => (
            <tr key={slice.label} className="border-t border-brand-border/10">
              <td className="py-2"><span className="mr-2 inline-block h-2.5 w-2.5 rounded-full" style={{ backgroundColor: colors[index % colors.length] }} />{slice.label}</td>
              <td className="py-2 text-right tabular-nums">{slice.displayValue}</td>
              <td className="py-2 text-right tabular-nums">{((slice.value / total) * 100).toFixed(1)}%</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

function slug(value: string): string {
  return value.toLowerCase().replace(/[^a-z0-9]+/g, '-')
}
