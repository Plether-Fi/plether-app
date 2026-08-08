import { type ReactNode } from 'react'
import { TokenAmount } from '../ui'

type ReconciliationTone = 'positive' | 'negative' | 'warning' | 'neutral'

export interface CloseSettlementLineItem {
  label: string
  amount: string
  tone?: ReconciliationTone
  detail?: string
}

interface CloseSettlementReconciliationPanelProps {
  settlementItems: CloseSettlementLineItem[]
  fundingItems: CloseSettlementLineItem[]
  message: ReactNode
  transactionHash?: string
}

function toneClass(tone: ReconciliationTone): string {
  switch (tone) {
    case 'positive':
      return 'text-positive'
    case 'negative':
      return 'text-brand-peach'
    case 'warning':
      return 'text-warning'
    case 'neutral':
      return 'text-content-primary'
  }
}

function ReconciliationRow({
  label,
  amount,
  tone = 'neutral',
  detail,
}: CloseSettlementLineItem) {
  return (
    <div className="grid grid-cols-[minmax(0,1fr)_auto] gap-4 border-b border-brand-border/15 py-3 last:border-b-0">
      <dt>
        <div className="text-sm text-content-secondary">{label}</div>
        {detail ? <div className="mt-1 text-xs leading-4 text-content-secondary/80">{detail}</div> : null}
      </dt>
      <dd className={`text-right text-sm font-semibold ${toneClass(tone)}`}>
        <TokenAmount amount={amount} />
      </dd>
    </div>
  )
}

function ReconciliationSection({
  eyebrow,
  title,
  items,
}: {
  eyebrow: string
  title: string
  items: CloseSettlementLineItem[]
}) {
  return (
    <section className="border border-brand-border/20 bg-app-bg">
      <div className="border-b border-brand-border/20 px-4 py-3">
        <div className="text-xs font-medium uppercase tracking-wide text-content-secondary">{eyebrow}</div>
        <h3 className="mt-1 text-base font-semibold text-content-primary">{title}</h3>
      </div>
      <dl className="px-4">
        {items.map((item) => <ReconciliationRow key={item.label} {...item} />)}
      </dl>
    </section>
  )
}

export function CloseSettlementReconciliationPanel({
  settlementItems,
  fundingItems,
  message,
  transactionHash,
}: CloseSettlementReconciliationPanelProps) {
  return (
    <article className="border border-positive/35 bg-surface-panel">
      <div className="flex flex-wrap items-start justify-between gap-4 border-b border-brand-border/20 px-5 py-4">
        <div>
          <div className="text-xs font-medium uppercase tracking-wide text-content-secondary">Final settlement</div>
          <h2 className="mt-1 text-xl font-semibold text-content-primary">Full close reconciliation</h2>
        </div>
        <div className="flex flex-wrap gap-2 text-xs font-semibold">
          <span className="border border-positive/35 bg-positive/10 px-3 py-1.5 text-positive">Position closed</span>
          <span className="border border-warning/35 bg-warning/10 px-3 py-1.5 text-warning">Claim recorded</span>
        </div>
      </div>

      <div className="space-y-5 p-5">
        <div className="grid gap-5 lg:grid-cols-2">
          <ReconciliationSection
            eyebrow="Close calculation"
            title="Realized result and costs"
            items={settlementItems}
          />
          <ReconciliationSection
            eyebrow="Funding result"
            title="Margin credit and trader claim"
            items={fundingItems}
          />
        </div>

        <div className="border border-warning/30 bg-warning/10 px-4 py-3 text-sm leading-5 text-content-secondary">
          {message}
        </div>

        {transactionHash ? (
          <div className="border-t border-brand-border/20 pt-4">
            <div className="text-xs text-content-secondary">Finalization transaction</div>
            <div className="mt-1 break-all font-mono text-xs text-content-primary">{transactionHash}</div>
          </div>
        ) : null}
      </div>
    </article>
  )
}
