import type { ReactNode } from 'react'

export type StatTone = 'neutral' | 'positive' | 'negative' | 'warning' | 'accent'

const VALUE_TONES: Record<StatTone, string> = {
  neutral: 'text-content-primary',
  positive: 'text-positive',
  negative: 'text-brand-orange',
  warning: 'text-brand-yellow',
  accent: 'text-brand-peach',
}

export function StatCard({
  label,
  value,
  detail,
  tone = 'neutral',
  loading = false,
  unavailable = false,
  action,
}: {
  label: string
  value?: ReactNode
  detail?: ReactNode
  tone?: StatTone
  loading?: boolean
  unavailable?: boolean
  action?: ReactNode
}) {
  return (
    <div className="flex min-w-0 flex-col justify-between border border-brand-border/30 bg-surface-panel p-4 sm:p-5">
      <div className="flex items-start justify-between gap-2">
        <p className="text-xs font-medium text-content-secondary">
          {label}
        </p>
        {action ? <div className="shrink-0">{action}</div> : null}
      </div>
      <div className="mt-2.5 min-w-0">
        {loading ? (
          <div className="skeleton h-8 w-28" aria-label={`Loading ${label}`} />
        ) : unavailable || value === undefined || value === null ? (
          <p className="font-mono text-xl font-semibold text-content-tertiary sm:text-2xl">Unavailable</p>
        ) : (
          <div className={`break-words font-mono text-xl font-semibold tabular-nums sm:text-2xl ${VALUE_TONES[tone]}`}>
            {value}
          </div>
        )}
        {detail ? (
          <div className="mt-2 text-xs leading-5 text-content-tertiary">{detail}</div>
        ) : null}
      </div>
    </div>
  )
}
