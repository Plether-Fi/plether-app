export type StatusTone = 'ready' | 'degraded' | 'blocked' | 'unknown' | 'live' | 'stale'

const TONE_STYLES: Record<StatusTone, { container: string; dot: string }> = {
  ready: {
    container: 'border-positive/35 bg-positive/10 text-positive',
    dot: 'bg-positive',
  },
  live: {
    container: 'border-positive/35 bg-positive/10 text-positive',
    dot: 'bg-positive',
  },
  degraded: {
    container: 'border-brand-yellow/40 bg-brand-yellow/10 text-brand-yellow',
    dot: 'bg-brand-yellow',
  },
  stale: {
    container: 'border-brand-yellow/40 bg-brand-yellow/10 text-brand-yellow',
    dot: 'bg-brand-yellow',
  },
  blocked: {
    container: 'border-brand-orange/45 bg-brand-orange/15 text-brand-peach',
    dot: 'bg-brand-orange',
  },
  unknown: {
    container: 'border-brand-border/35 bg-surface-raised/60 text-content-secondary',
    dot: 'bg-content-tertiary',
  },
}

export function StatusPill({
  tone,
  label,
  title,
  className = '',
}: {
  tone: StatusTone
  label: string
  title?: string
  className?: string
}) {
  const styles = TONE_STYLES[tone]
  return (
    <span
      title={title}
      className={`inline-flex items-center gap-1.5 whitespace-nowrap border px-2.5 py-1 text-xs font-semibold ${styles.container} ${className}`}
    >
      <span className={`h-1.5 w-1.5 shrink-0 rounded-full ${styles.dot}`} aria-hidden="true" />
      {label}
    </span>
  )
}
