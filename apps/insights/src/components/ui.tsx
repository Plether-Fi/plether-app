import type { ReactNode } from 'react'
import { Link } from 'react-router-dom'
import type { Competition, Standing } from '../api/types'
import { formatSignedUsdc, shortAddress, xProfileUrl } from '../utils/format'
import { eligibilityPresentation } from '../utils/eligibility'

export function Panel({ children, className = '' }: { children: ReactNode; className?: string }) {
  return <section className={`border border-brand-border/25 bg-surface-panel ${className}`}>{children}</section>
}

export function StatusBadge({ eligible, label, neutral = false, title }: { eligible: boolean; label?: string; neutral?: boolean; title?: string }) {
  return (
    <span title={title} className={`inline-flex whitespace-nowrap items-center gap-1.5 rounded-full border px-2.5 py-1 text-xs font-semibold ${neutral ? 'border-brand-border/35 bg-surface-panel text-content-secondary' : eligible ? 'border-positive/35 bg-positive/10 text-positive' : 'border-brand-orange/40 bg-brand-orange/10 text-brand-peach'}`}>
      <span className={`h-1.5 w-1.5 rounded-full ${neutral ? 'bg-content-secondary' : eligible ? 'bg-positive' : 'bg-brand-orange'}`} />
      {label ?? (eligible ? 'Eligible' : 'Not eligible')}
    </span>
  )
}

export function EligibilityBadge({ standing, competitionStatus }: { standing: Pick<Standing, 'eligible' | 'eligibilityStatus'>; competitionStatus: Competition['status'] }) {
  const presentation = eligibilityPresentation(standing, competitionStatus)
  return <StatusBadge eligible={standing.eligible} label={presentation.label} neutral={presentation.neutral} title={presentation.explanation ?? undefined} />
}

export function ProvisionalNotice() {
  return (
    <div className="flex gap-3 border border-brand-yellow/40 bg-brand-yellow/10 px-4 py-3 text-sm text-brand-yellow" role="status">
      <span aria-hidden="true">●</span>
      <p><strong>Provisional standings.</strong> Registered traders have completed their entry. Prize eligibility is reviewed after the competition ends.</p>
    </div>
  )
}

export function IntegrityNotice({ competition }: { competition: Pick<Competition, 'integrityStatus'> }) {
  if (competition.integrityStatus !== 'pending' && competition.integrityStatus !== 'stale') return null
  return (
    <div className="border border-brand-yellow/40 bg-brand-yellow/10 px-4 py-3 text-sm text-brand-yellow" role="status">
      <strong>Integrity checks updating.</strong> P&amp;L and trading rank remain available. Provisional prize eligibility will update when checks finish.
    </div>
  )
}

export function Pnl({ value, className = '', usdcKind }: { value: string | null | undefined; className?: string; usdcKind?: 'real' | 'mock' }) {
  const positive = value != null && /^\+?[1-9]\d*$/.test(value)
  const negative = value?.startsWith('-') === true
  const formatted = formatSignedUsdc(value)
  return (
    <span className={`tabular-nums ${positive ? 'text-positive' : negative ? 'text-brand-orange' : 'text-content-primary'} ${className}`}>
      {usdcKind ? formatted.replace(/ USDC$/, ` ${usdcKind} USDC`) : formatted}
    </span>
  )
}

export function WalletIdentity({ address, displayName, competitionSlug }: { address: string; displayName: string | null; competitionSlug: string }) {
  const publicXProfile = competitionSlug === 'testnet-trading-2026-09' ? xProfileUrl(displayName) : null
  return (
    <div className="min-w-0">
      {displayName ? publicXProfile ? (
        <a href={publicXProfile} target="_blank" rel="noreferrer" className="block truncate font-semibold text-content-primary hover:text-brand-peach hover:underline">
          @{displayName.replace(/^@/, '')} ↗
        </a>
      ) : <div className="truncate font-semibold text-content-primary">{displayName}</div> : null}
      <Link to={`/competitions/${encodeURIComponent(competitionSlug)}/wallets/${address}`} className={`whitespace-nowrap font-mono text-xs text-brand-peach hover:underline ${displayName ? '' : 'text-sm'}`}>
        {shortAddress(address)}
      </Link>
    </div>
  )
}

export function LoadingState({ rows = 5 }: { rows?: number }) {
  return (
    <div className="space-y-3 p-5" aria-label="Loading" aria-busy="true">
      {Array.from({ length: rows }, (_, index) => (
        <div key={index} className="skeleton h-14 w-full" />
      ))}
    </div>
  )
}

export function ErrorState({ title = 'Unable to load data', message, onRetry }: { title?: string; message?: string; onRetry?: () => void }) {
  return (
    <Panel className="p-8 text-center" >
      <div className="mx-auto mb-4 flex h-10 w-10 items-center justify-center rounded-full border border-brand-orange/40 bg-brand-orange/10 text-brand-orange" aria-hidden="true">!</div>
      <h2 className="text-lg font-semibold">{title}</h2>
      <p className="mx-auto mt-2 max-w-lg text-sm text-content-secondary">{message ?? 'The Insights API did not respond. Please try again in a moment.'}</p>
      {onRetry ? <button type="button" onClick={onRetry} className="mt-5 border border-brand-orange bg-brand-orange px-4 py-2 text-sm font-semibold hover:bg-brand-peach hover:text-app-bg">Try again</button> : null}
    </Panel>
  )
}

export function EmptyState({ title, message }: { title: string; message: string }) {
  return (
    <div className="px-5 py-14 text-center">
      <p className="text-base font-semibold text-content-primary">{title}</p>
      <p className="mx-auto mt-2 max-w-md text-sm text-content-secondary">{message}</p>
    </div>
  )
}
