import { useState } from 'react'
import { Link } from 'react-router-dom'
import type { LpSettlementRecord, ReturnReportAudit } from '../api'
import { formatCompactAssets, formatUtc } from '../utils/format'
import {
  classifyReportBlock,
  classifySettlementBlock,
  computeResidualAccuracy,
  type ContinuityBlock,
  type ContinuityLevel,
} from '../utils/liquidityVisuals'
import { StatusPill } from './StatusPill'

function blockBgClass(level: ContinuityLevel, selected: boolean): string {
  const ring = selected ? ' ring-1 ring-content-primary ring-offset-1 ring-offset-app-bg' : ''
  switch (level) {
    case 'clear':
      return `bg-positive/85 hover:bg-positive${ring}`
    case 'warning':
      return `bg-brand-yellow/85 hover:bg-brand-yellow${ring}`
    case 'blocked':
      return `bg-brand-orange/90 hover:bg-brand-orange${ring}`
    case 'unknown':
      return `bg-content-tertiary/40 hover:bg-content-tertiary/65${ring}`
  }
}

export function SettlementContinuityStrip({
  settlements,
  reports,
}: {
  settlements: LpSettlementRecord[]
  reports?: ReturnReportAudit[]
}) {
  const mode = settlements.length > 0 ? 'settlements' : reports && reports.length > 0 ? 'reports' : 'empty'

  const blocks: ContinuityBlock[] =
    mode === 'settlements'
      ? [...settlements].reverse().map((row) => {
          const flags: string[] = []
          if (row.seniorBacklog) flags.push('Senior backlog')
          if (row.juniorBacklog) flags.push('Junior backlog')
          if (row.entriesDeferred) flags.push('Deferred')
          if (row.replacementCount > 0) flags.push(`Replaced ×${String(row.replacementCount)}`)
          const latency =
            row.confirmationLatencySeconds != null
              ? `${String(row.confirmationLatencySeconds)}s`
              : '—'
          return {
            id: `${String(row.epoch)}-${row.txHash ?? row.createdAt}`,
            label: `Epoch #${row.epoch.toLocaleString('en-US')}`,
            level: classifySettlementBlock(row),
            detail: `${row.status} · ${latency}${flags.length > 0 ? ` · ${flags.join(', ')}` : ' · Clean'}`,
            timestamp: row.confirmedAt ?? row.createdAt,
            epoch: row.epoch,
          }
        })
      : mode === 'reports' && reports
        ? [...reports].reverse().map((rep) => {
            const exact = rep.unexplainedUsdg === '0' || rep.unexplainedPp === 0
            return {
              id: `${rep.startBlockNumber}-${rep.endBlockNumber}`,
              label: `Blocks ${Number(rep.startBlockNumber).toLocaleString('en-US')}→${Number(rep.endBlockNumber).toLocaleString('en-US')}`,
              level: classifyReportBlock(rep),
              detail: exact
                ? `Schema v${String(rep.schemaVersion)} · Exact (0.00 residual)`
                : `Schema v${String(rep.schemaVersion)} · Residual ${formatCompactAssets(rep.unexplainedUsdg)}`,
              timestamp: rep.updatedAt,
            }
          })
        : []

  const [selectedId, setSelectedId] = useState<string | null>(null)
  if (blocks.length === 0) return null

  const activeBlock = blocks.find((b) => b.id === selectedId) ?? blocks[blocks.length - 1]
  const clearCount = blocks.filter((b) => b.level === 'clear').length
  const warnCount = blocks.filter((b) => b.level === 'warning').length
  const blockedCount = blocks.filter((b) => b.level === 'blocked').length

  return (
    <div className="space-y-3 border border-brand-border/25 bg-app-bg p-4">
      <div className="flex flex-wrap items-center justify-between gap-2">
        <div className="flex flex-wrap items-center gap-2">
          <span className="text-xs font-semibold uppercase tracking-[0.12em] text-content-secondary">
            {mode === 'settlements' ? 'Epoch settlement continuity' : 'Attribution report continuity'}
          </span>
          <span className="font-mono text-xs text-content-tertiary">
            ({blocks.length} recent {mode === 'settlements' ? 'epochs' : 'reports'})
          </span>
        </div>

        <div className="flex flex-wrap items-center gap-3 font-mono text-[11px] tabular-nums">
          <span className="inline-flex items-center gap-1.5 text-positive">
            <span className="h-2 w-2 rounded-full bg-positive" aria-hidden="true" />
            {clearCount} clear
          </span>
          <span className="inline-flex items-center gap-1.5 text-brand-yellow">
            <span className="h-2 w-2 rounded-full bg-brand-yellow" aria-hidden="true" />
            {warnCount} warn
          </span>
          <span className="inline-flex items-center gap-1.5 text-brand-orange">
            <span className="h-2 w-2 rounded-full bg-brand-orange" aria-hidden="true" />
            {blockedCount} blocked
          </span>
        </div>
      </div>

      <div
        className="flex h-7 w-full items-stretch gap-1"
        role="group"
        aria-label={
          mode === 'settlements'
            ? `Epoch settlement continuity: ${String(clearCount)} clear, ${String(warnCount)} warnings, ${String(blockedCount)} blocked`
            : `Attribution report continuity: ${String(clearCount)} clear, ${String(warnCount)} warnings, ${String(blockedCount)} blocked`
        }
      >
        {blocks.map((block) => {
          const isSelected = activeBlock.id === block.id
          const className = `min-w-[6px] flex-1 cursor-pointer transition-colors ${blockBgClass(block.level, isSelected)}`
          if (block.epoch != null) {
            return (
              <Link
                key={block.id}
                to={`/liquidity/epochs/${String(block.epoch)}`}
                onMouseEnter={() => { setSelectedId(block.id) }}
                onFocus={() => { setSelectedId(block.id) }}
                aria-label={`${block.label}: ${block.detail}`}
                title={`${block.label} · ${block.detail}`}
                className={className}
              />
            )
          }
          return (
            <button
              key={block.id}
              type="button"
              onMouseEnter={() => { setSelectedId(block.id) }}
              onFocus={() => { setSelectedId(block.id) }}
              onClick={() => { setSelectedId(block.id) }}
              aria-label={`${block.label}: ${block.detail}`}
              title={`${block.label} · ${block.detail}`}
              className={className}
            />
          )
        })}
      </div>

      <div className="flex flex-wrap items-center justify-between gap-2 border-t border-brand-border/15 pt-2 text-xs">
        <div className="flex flex-wrap items-center gap-x-2 gap-y-0.5 font-mono">
          {activeBlock.epoch != null ? (
            <Link
              to={`/liquidity/epochs/${String(activeBlock.epoch)}`}
              className="font-semibold text-brand-peach hover:underline"
            >
              {activeBlock.label} →
            </Link>
          ) : (
            <span className="font-semibold text-content-primary">{activeBlock.label}</span>
          )}
          <span className="text-content-tertiary">·</span>
          <span className="text-content-secondary">{activeBlock.detail}</span>
        </div>
        <span className="font-mono text-[11px] text-content-tertiary">
          {formatUtc(activeBlock.timestamp)}
        </span>
      </div>
    </div>
  )
}

export function ResidualAccuracyMeter({ report }: { report: ReturnReportAudit }) {
  const accuracy = computeResidualAccuracy(report)

  if (!accuracy.available) {
    return <span className="font-mono text-xs text-content-tertiary">—</span>
  }

  const residualSevere = accuracy.residualPct != null && accuracy.residualPct >= 5

  return (
    <div className="min-w-[170px] space-y-1.5">
      <div className="flex items-center justify-between gap-2">
        {accuracy.exact ? (
          <StatusPill tone="ready" label="Exact (0.00)" />
        ) : (
          <StatusPill
            tone={residualSevere ? 'blocked' : 'degraded'}
            label={formatCompactAssets(report.unexplainedUsdg)}
          />
        )}
        <span className="font-mono text-[11px] tabular-nums text-content-secondary">
          {accuracy.attributedPct != null ? `${accuracy.attributedPct.toFixed(2)}%` : '—'}
        </span>
      </div>

      <div
        className="flex h-1.5 w-full overflow-hidden bg-app-bg ring-1 ring-inset ring-brand-border/20"
        role="meter"
        aria-label={`Attribution accuracy: ${accuracy.attributedPct != null ? accuracy.attributedPct.toFixed(2) : '0'}% attributed`}
        aria-valuemin={0}
        aria-valuemax={100}
        aria-valuenow={accuracy.attributedPct != null ? Math.round(accuracy.attributedPct) : 0}
      >
        {accuracy.attributedPct != null && accuracy.attributedPct > 0 ? (
          <div
            className="bg-positive"
            style={{ width: `${String(accuracy.attributedPct)}%` }}
          />
        ) : null}
        {!accuracy.exact && accuracy.residualPct != null && accuracy.residualPct > 0 ? (
          <div
            className={residualSevere ? 'bg-brand-orange' : 'bg-brand-yellow'}
            style={{ width: `${String(accuracy.residualPct)}%`, minWidth: '3px' }}
          />
        ) : null}
      </div>
    </div>
  )
}
