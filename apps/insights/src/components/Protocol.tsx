import type { ReactNode } from 'react'
import { Link } from 'react-router-dom'
import type { AvailabilityReason, ProtocolAction, ProtocolEnvelope } from '../api'
import { formatUsdc, shortAddress } from '../utils/format'
import { displayText, formatTimestamp, humanize, readRecord } from '../utils/protocol'
import { EmptyState, Panel } from './ui'

export function PageTitle({
  eyebrow = 'Protocol transparency explorer',
  title,
  description,
  children,
}: {
  eyebrow?: string
  title: string
  description: string
  children?: ReactNode
}) {
  return (
    <header className="flex flex-col gap-5 border-b border-brand-border/20 pb-7 lg:flex-row lg:items-end lg:justify-between">
      <div className="max-w-3xl">
        <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">{eyebrow}</p>
        <h1 className="mt-2 text-3xl font-semibold tracking-tight sm:text-4xl">{title}</h1>
        <p className="mt-3 max-w-2xl text-sm leading-6 text-content-secondary sm:text-base">{description}</p>
      </div>
      {children}
    </header>
  )
}

export function ProtocolMeta({ data }: { data: ProtocolEnvelope }) {
  return (
    <div className="flex flex-wrap items-center gap-x-5 gap-y-2 border border-brand-border/20 bg-app-bg-deep/45 px-4 py-3 text-xs text-content-tertiary">
      <span><strong className="text-content-secondary">Release</strong> {data.releaseId}</span>
      <span><strong className="text-content-secondary">Chain</strong> {data.chainId}</span>
      <span><strong className="text-content-secondary">Confirmed block</strong> {Number(data.confirmedBlock.number).toLocaleString()}</span>
      <span title={data.confirmedBlock.hash}><strong className="text-content-secondary">Block hash</strong> <code>{shortAddress(data.confirmedBlock.hash)}</code></span>
      <span><strong className="text-content-secondary">Block time</strong> {formatTimestamp(data.confirmedBlock.timestamp)}</span>
      <span><strong className="text-content-secondary">Indexed</strong> {formatTimestamp(data.indexerTimestamp)}</span>
      <span><strong className="text-content-secondary">Calculation</strong> {data.calculationVersion}</span>
    </div>
  )
}

export function EvidenceBadge({ level }: { level: unknown }) {
  const summary = summarizeEvidence(level)
  const text = summary.text
  const normalized = text.toLowerCase()
  const delta = normalized.includes('block-level')
  const derived =
    normalized.includes('derived')
    || normalized.includes('versioned')
    || normalized.includes('mixed')
    || normalized.includes('projection')
  const incomplete =
    normalized.includes('unavailable')
    || normalized.includes('best_effort')
    || normalized.includes('partial')
  const exact =
    !delta
    && !derived
    && !incomplete
    && (normalized.includes('exact') || normalized.includes('confirmed'))
  const className = delta
    ? 'border-brand-yellow/40 bg-brand-yellow/10 text-brand-yellow'
    : incomplete
      ? 'border-brand-yellow/40 bg-brand-yellow/10 text-brand-yellow'
      : derived
        ? 'border-brand-peach/35 bg-brand-peach/10 text-brand-peach'
      : exact
        ? 'border-positive/35 bg-positive/10 text-positive'
        : 'border-content-tertiary/30 bg-white/5 text-content-tertiary'
  const badge = (
    <span className={`inline-flex rounded-full border px-2 py-0.5 text-[11px] font-semibold ${className}`}>
      {text}
    </span>
  )
  if (!summary.structured) return badge
  return (
    <details aria-label={`Evidence details: ${text}`} className="inline-block max-w-full align-middle">
      <summary className="cursor-pointer list-none [&::-webkit-details-marker]:hidden">
        {badge}
        <span className="sr-only"> — expand evidence details</span>
      </summary>
      <pre className="mt-2 max-h-72 max-w-[min(42rem,80vw)] overflow-auto border border-brand-border/20 bg-app-bg-deep/90 p-3 text-left text-xs font-normal leading-5 text-brand-peach">
        {safeJson(level)}
      </pre>
    </details>
  )
}

export function AvailabilityList({ items }: { items: AvailabilityReason[] }) {
  if (items.length === 0) return null
  return (
    <details className="border border-brand-yellow/25 bg-brand-yellow/5">
      <summary className="cursor-pointer px-4 py-3 text-sm font-semibold text-brand-yellow">
        {items.length} field{items.length === 1 ? '' : 's'} unavailable or incomplete
      </summary>
      <ul className="grid gap-2 border-t border-brand-yellow/15 px-4 py-3 text-xs text-content-secondary sm:grid-cols-2">
        {items.map((item, index) => (
          <li key={`${item.field}-${item.reason}-${String(index)}`}>
            <code className="text-brand-peach">{item.field}</code>: {humanize(item.reason)}
          </li>
        ))}
      </ul>
    </details>
  )
}

export function Metric({
  label,
  value,
  hint,
  tone = 'default',
}: {
  label: string
  value: ReactNode
  hint?: string
  tone?: 'default' | 'positive' | 'warning' | 'critical'
}) {
  const toneClass = tone === 'positive'
    ? 'text-positive'
    : tone === 'warning'
      ? 'text-brand-yellow'
      : tone === 'critical'
        ? 'text-brand-orange'
        : 'text-content-primary'
  return (
    <div className="border border-brand-border/20 bg-surface-panel p-4">
      <p className="text-xs uppercase tracking-wide text-content-tertiary">{label}</p>
      <div className={`mt-2 break-words text-xl font-semibold tabular-nums ${toneClass}`}>{value}</div>
      {hint ? <p className="mt-2 text-xs leading-5 text-content-tertiary">{hint}</p> : null}
    </div>
  )
}

export function Section({
  title,
  description,
  children,
  className = '',
}: {
  title: string
  description?: string
  children: ReactNode
  className?: string
}) {
  return (
    <Panel className={className}>
      <div className="border-b border-brand-border/20 px-5 py-4">
        <h2 className="text-lg font-semibold">{title}</h2>
        {description ? <p className="mt-1 text-sm text-content-secondary">{description}</p> : null}
      </div>
      {children}
    </Panel>
  )
}

export function ObjectTable({
  value,
  empty = 'No data is available for this section.',
  evidence,
}: {
  value: unknown
  empty?: string
  evidence?: unknown
}) {
  const entries = recordEntries(value)
  if (entries.length === 0) return <EmptyState title="Unavailable" message={empty} />
  return (
    <div className="overflow-x-auto">
      <table className="w-full border-collapse text-left text-sm">
        <tbody>
          {entries.map(([key, item]) => (
            <tr key={key} className="border-b border-brand-border/10 last:border-0">
              <th scope="row" className="w-1/3 min-w-44 px-5 py-3 font-medium text-content-secondary">{humanize(key)}</th>
              <td className="px-5 py-3 text-content-primary">
                <DisplayValue value={item} field={key} />
              </td>
            </tr>
          ))}
        </tbody>
      </table>
      {evidence !== undefined ? (
        <div className="flex flex-wrap items-start gap-2 border-t border-brand-border/15 px-5 py-3">
          <span className="pt-0.5 text-xs font-semibold uppercase tracking-wide text-content-tertiary">Evidence</span>
          <EvidenceBadge level={evidence} />
        </div>
      ) : null}
    </div>
  )
}

export function ActionTable({
  actions,
  releaseId = 'current',
  showKeeper = true,
}: {
  actions: ProtocolAction[]
  releaseId?: string
  showKeeper?: boolean
}) {
  if (actions.length === 0) {
    return <EmptyState title="No confirmed actions" message="No matching successful or terminal onchain actions are indexed for this selection." />
  }
  return (
    <div className="overflow-x-auto">
      <table className="w-full min-w-[860px] border-collapse text-left text-sm">
        <thead>
          <tr className="border-b border-brand-border/20 text-xs uppercase tracking-wide text-content-tertiary">
            <th className="px-5 py-3">Time</th>
            <th className="px-5 py-3">Action</th>
            <th className="px-5 py-3">Outcome</th>
            <th className="px-5 py-3">Account / order</th>
            {showKeeper ? <th className="px-5 py-3">Keeper</th> : null}
            <th className="px-5 py-3">Transaction</th>
            <th className="px-5 py-3">Evidence</th>
          </tr>
        </thead>
        <tbody>
          {actions.map((action) => (
            <tr key={action.actionId} className="border-b border-brand-border/10 align-top hover:bg-white/[0.025]">
              <td className="whitespace-nowrap px-5 py-4 text-xs text-content-secondary">{formatTimestamp(action.timestamp)}</td>
              <td className="px-5 py-4 font-semibold">{humanize(action.actionType)}</td>
              <td className="px-5 py-4"><Outcome outcome={action.outcome} /></td>
              <td className="px-5 py-4">
                {action.account ? <Link className="font-mono text-xs text-brand-peach hover:underline" to={`/transactions?release=${encodeURIComponent(releaseId)}&account=${encodeURIComponent(action.account)}`}>{shortAddress(action.account)}</Link> : '—'}
                {action.orderId ? (
                  <div>
                    <Link
                      className="text-xs text-content-secondary hover:text-brand-peach"
                      to={`/orders/${encodeURIComponent(releaseId)}/${encodeURIComponent(action.orderId)}`}
                    >
                      Order #{action.orderId}
                    </Link>
                  </div>
                ) : null}
              </td>
              {showKeeper ? (
                <td className="px-5 py-4">
                  {action.keeper ? <Link className="font-mono text-xs text-brand-peach hover:underline" to={`/keepers/${encodeURIComponent(action.keeper)}?release=${encodeURIComponent(releaseId)}`}>{shortAddress(action.keeper)}</Link> : '—'}
                </td>
              ) : null}
              <td className="px-5 py-4">
                <Link className="font-mono text-xs text-brand-peach hover:underline" to={`/transactions/${encodeURIComponent(action.transactionHash)}?release=${encodeURIComponent(releaseId)}`}>
                  {shortAddress(action.transactionHash)}
                </Link>
                <div className="mt-1 text-xs text-content-tertiary">Block {Number(action.blockNumber).toLocaleString()} · log {action.logIndex}</div>
              </td>
              <td className="px-5 py-4"><EvidenceBadge level={readRecord(action.evidence).level ?? 'unavailable'} /></td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

export function RawEvidence({ value, title = 'Raw evidence' }: { value: unknown; title?: string }) {
  return (
    <details className="border-t border-brand-border/15">
      <summary className="cursor-pointer px-5 py-3 text-sm font-semibold text-content-secondary hover:text-content-primary">{title}</summary>
      <pre className="max-h-[34rem] overflow-auto border-t border-brand-border/10 bg-app-bg-deep/70 p-5 text-xs leading-5 text-brand-peach">
        {JSON.stringify(value, null, 2)}
      </pre>
    </details>
  )
}

export function Outcome({ outcome }: { outcome: string }) {
  const success = outcome === 'success'
  const pending = outcome === 'pending'
  const classes = success
    ? 'border-positive/30 bg-positive/10 text-positive'
    : pending
      ? 'border-brand-yellow/30 bg-brand-yellow/10 text-brand-yellow'
      : 'border-brand-orange/30 bg-brand-orange/10 text-brand-orange'
  return <span className={`inline-flex rounded-full border px-2 py-0.5 text-xs font-semibold ${classes}`}>{humanize(outcome)}</span>
}

export function DisplayValue({ value, field }: { value: unknown; field?: string }) {
  if (value === null || value === undefined || value === '') return <span className="text-content-tertiary">Unavailable</span>
  if (typeof value === 'boolean') return <span>{value ? 'Yes' : 'No'}</span>
  if (typeof value === 'object') return <code className="break-all whitespace-pre-wrap text-xs text-brand-peach">{JSON.stringify(value)}</code>
  const text = displayText(value)
  if (field?.toLowerCase().includes('usdc') && /^-?\d+$/.test(text)) return <span className="tabular-nums">{formatUsdc(text)}</span>
  if (/^0x[a-fA-F0-9]{40,64}$/.test(text)) return <code className="break-all text-xs text-brand-peach">{text}</code>
  return <span className="break-words tabular-nums">{text}</span>
}

function recordEntries(value: unknown): [string, unknown][] {
  return Object.entries(readRecord(value))
}

interface EvidenceSummary {
  text: string
  structured: boolean
}

function summarizeEvidence(value: unknown): EvidenceSummary {
  const record = readRecord(value)
  const structured = Object.keys(record).length > 0
  if (!structured) {
    return {
      text: displayText(value, 'unavailable'),
      structured: false,
    }
  }

  const explicitLevel = displayText(record.level, '').trim()
  if (explicitLevel !== '') {
    return { text: explicitLevel, structured: true }
  }

  const signals = collectEvidenceSignals(value)
  const usable = signals.exact || signals.derived || signals.delta
  if (signals.incomplete && usable) {
    return { text: 'partial structured evidence', structured: true }
  }
  if (signals.incomplete) {
    return {
      text: signals.partial ? 'partial structured evidence' : 'unavailable',
      structured: true,
    }
  }
  if (signals.delta) {
    return { text: 'block-level delta', structured: true }
  }
  if (signals.derived && signals.exact) {
    return { text: 'mixed exact and derived', structured: true }
  }
  if (signals.derived) {
    return { text: 'derived structured evidence', structured: true }
  }
  if (signals.exact) {
    return { text: 'exact structured evidence', structured: true }
  }
  return { text: 'structured evidence', structured: true }
}

function collectEvidenceSignals(value: unknown): {
  exact: boolean
  derived: boolean
  delta: boolean
  incomplete: boolean
  partial: boolean
} {
  const signals = {
    exact: false,
    derived: false,
    delta: false,
    incomplete: false,
    partial: false,
  }
  const visit = (item: unknown) => {
    if (Array.isArray(item)) {
      item.forEach(visit)
      return
    }
    const record = readRecord(item)
    if (Object.keys(record).length > 0) {
      Object.values(record).forEach(visit)
      return
    }
    if (typeof item !== 'string') return
    const normalized = item.toLowerCase()
    if (normalized.includes('block-level')) signals.delta = true
    if (
      normalized.includes('derived')
      || normalized.includes('versioned')
      || normalized.includes('projection')
    ) signals.derived = true
    if (normalized.includes('exact') || normalized.includes('confirmed')) signals.exact = true
    if (
      normalized.includes('unavailable')
      || normalized.includes('best_effort')
      || normalized.includes('partial')
    ) signals.incomplete = true
    if (normalized.includes('partial')) signals.partial = true
  }
  visit(value)
  return signals
}

function safeJson(value: unknown): string {
  try {
    return JSON.stringify(value, null, 2)
  } catch {
    return 'Structured evidence could not be serialized.'
  }
}
