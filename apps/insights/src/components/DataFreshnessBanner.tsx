import { StatusPill, type StatusTone } from './StatusPill'
import { formatUtc } from '../utils/format'

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
  const formattedBlock =
    blockNumber != null && /^\d+$/.test(String(blockNumber))
      ? Number(blockNumber).toLocaleString('en-US')
      : null

  return (
    <div className="flex flex-wrap items-center justify-between gap-3 border border-brand-border/20 bg-app-bg-deep/60 px-4 py-3 text-xs text-content-secondary">
      <div className="flex flex-wrap items-center gap-2.5">
        <StatusPill tone={tone} label={statusLabel} />
        <span className="border border-brand-border/25 bg-surface-panel px-2 py-0.5 font-mono text-content-primary">
          {networkName}
          {isTestnet ? ' · Testnet' : ''}
        </span>
        {releaseVersion ? (
          <span className="border border-brand-border/25 bg-surface-panel px-2 py-0.5 font-mono text-brand-peach">
            Core {releaseVersion}
          </span>
        ) : null}
        {note ? <span className="text-content-secondary">{note}</span> : null}
      </div>
      <div className="flex flex-wrap items-center gap-x-4 gap-y-1 font-mono text-content-tertiary">
        {formattedBlock ? <span>Block {formattedBlock}</span> : null}
        {observedAt ? <span>Observed {formatUtc(observedAt)}</span> : null}
      </div>
    </div>
  )
}
