import type { ReactNode } from 'react'

export function WalletMetric({ label, children }: { label: string; children: ReactNode }) {
  return (
    <div className="border-l border-brand-border/20 px-4 py-4 first:border-l-0 sm:px-5">
      <dt className="text-[11px] font-semibold uppercase tracking-[0.14em] text-content-tertiary">{label}</dt>
      <dd className="mt-1 text-base font-semibold tabular-nums sm:text-lg">{children}</dd>
    </div>
  )
}
