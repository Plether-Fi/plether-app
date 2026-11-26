export function Sparkline({
  values,
  label,
  tone = 'accent',
  width = 180,
  height = 44,
  className = '',
}: {
  values: number[]
  label: string
  tone?: 'positive' | 'negative' | 'accent' | 'neutral'
  width?: number
  height?: number
  className?: string
}) {
  const finite = values.filter((value) => Number.isFinite(value))
  if (finite.length < 2) {
    return (
      <div
        role="img"
        aria-label={`${label}: chart unavailable`}
        className={`flex items-center justify-center border border-dashed border-brand-border/25 text-xs text-content-tertiary ${className}`}
        style={{ height }}
      >
        Unavailable
      </div>
    )
  }

  const padding = 3
  const min = Math.min(...finite)
  const max = Math.max(...finite)
  const span = max - min
  const plotWidth = width - padding * 2
  const plotHeight = height - padding * 2

  const points = finite
    .map((value, index) => {
      const x = padding + (index / (finite.length - 1)) * plotWidth
      const ratio = span === 0 ? 0.5 : (value - min) / span
      const y = padding + (1 - ratio) * plotHeight
      return `${x.toFixed(1)},${y.toFixed(1)}`
    })
    .join(' ')

  const strokeClass =
    tone === 'positive'
      ? 'stroke-positive'
      : tone === 'negative'
        ? 'stroke-brand-orange'
        : tone === 'accent'
          ? 'stroke-brand-peach'
          : 'stroke-content-secondary'

  return (
    <svg
      role="img"
      aria-label={label}
      viewBox={`0 0 ${String(width)} ${String(height)}`}
      className={`h-auto w-full overflow-visible ${className}`}
      preserveAspectRatio="none"
    >
      <polyline
        fill="none"
        strokeWidth="2"
        strokeLinejoin="round"
        strokeLinecap="round"
        className={strokeClass}
        points={points}
      />
    </svg>
  )
}
