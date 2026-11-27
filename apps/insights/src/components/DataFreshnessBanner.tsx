import { StatusPill, type StatusTone } from './StatusPill'
import { formatUtc } from '../utils/format'

const TRANSIENT_LOADING_PREFIXES = ['checking', 'loading']

/**
 * High-signal inline telemetry banner.
 *
 * When a page's telemetry is healthy (`tone === 'live'`), live network / release /
 * block status is already displayed in the sticky header pill, so we omit the
 * redundant top-of-page bar to keep headlines and primary KPIs above the fold.
 * Whenever telemetry is degraded, stale, blocked, or awaiting deployment, we
 * render a prominent inline alert strip.
 */
export function DataFreshnessBanner({
  networkName,
  isTestnet,
  releaseVersion,
  blockNumber,
  observedAt,
  tone,
  statusLabel,
  note,
}: {
  networkName: string
  isTestnet?: boolean
  releaseVersion?: string | null
  blockNumber?: string | number | null
  observedAt?: string | null
  tone: StatusTone
  statusLabel: string
  note?: string | null
}) {
  if (tone === 'live') {
    return null
  }
  if (
    tone === 'unknown' &&
    TRANSIENT_LOADING_PREFIXES.some((prefix) => statusLabel.trim().toLowerCase().startsWith(prefix))
  ) {
    return null
  }

  const formattedBlock =
    blockNumber != null && /^\d+$/.test(String(blockNumber))
      ? Number(blockNumber).toLocaleString('en-US')
      : null

  const surfaceClass =
    tone === 'blocked'
      ? 'border-brand-orange/45 bg-brand-orange/10'
      : tone === 'degraded' || tone === 'stale'
        ? 'border-brand-yellow/40 bg-brand-yellow/5'
        : 'border-brand-border/30 bg-app-bg'

  return (
    <div
      className={`flex flex-wrap items-center justify-between gap-3 border px-4 py-2.5 text-xs text-content-secondary ${surfaceClass}`}
    >
      <div className="flex flex-wrap items-center gap-x-3 gap-y-1.5">
        <StatusPill tone={tone} label={statusLabel} />
        <span className="font-mono font-medium text-content-primary">
          {networkName}
          {isTestnet ? ' · Testnet' : ''}
        </span>
        {releaseVersion ? (
          <>
            <span className="text-content-tertiary" aria-hidden="true">·</span>
            <span className="font-mono text-brand-peach">Core {releaseVersion}</span>
          </>
        ) : null}
        {note ? (
          <>
            <span className="text-content-tertiary" aria-hidden="true">·</span>
            <span className="text-content-secondary">{note}</span>
          </>
        ) : null}
      </div>
      <div className="flex flex-wrap items-center gap-x-4 gap-y-1 font-mono text-content-tertiary">
        {formattedBlock ? <span>Block {formattedBlock}</span> : null}
        {observedAt ? <span>Observed {formatUtc(observedAt)}</span> : null}
      </div>
    </div>
  )
}
