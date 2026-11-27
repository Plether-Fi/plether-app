import { useState } from 'react'
import {
  formatRateBps,
  type LatencyDistribution,
  type OrderReliabilityBucket,
  type OrderReliabilityCohort,
} from '../api'
import { EmptyState, Panel } from './ui'

function cohortTitle(seconds: number): string {
  if (seconds === 3600) return '1 hour'
  if (seconds === 86400) return '24 hours'
  return `${String(seconds)}s`
}

function cohortStartLabel(seconds: number): string {
  if (seconds === 3600) return '1h ago'
  if (seconds === 86400) return '24h ago'
  return `-${String(seconds)}s`
}

function bucketWidthLabel(windowSeconds: number, count: number): string {
  if (count <= 0) return ''
  const secs = Math.round(windowSeconds / count)
  if (secs >= 3600 && secs % 3600 === 0) return `${String(secs / 3600)}h / block`
  if (secs >= 60 && secs % 60 === 0) return `${String(secs / 60)}m / block`
  return `${String(secs)}s / block`
}

function formatUtcRange(startIso: string, endIso: string): string {
  const fmt = (iso: string) => {
    const d = new Date(iso)
    if (Number.isNaN(d.getTime())) return iso
    const hh = String(d.getUTCHours()).padStart(2, '0')
    const mm = String(d.getUTCMinutes()).padStart(2, '0')
    return `${hh}:${mm}`
  }
  return `${fmt(startIso)}–${fmt(endIso)} UTC`
}

function pctOf(value: number, total: number): number {
  if (total <= 0 || value <= 0) return 0
  return Math.min(100, (value / total) * 100)
}

type OutcomeKind = 'executed' | 'pending' | 'expired' | 'failed' | 'empty'

function classifyBucket(bucket: OrderReliabilityBucket): OutcomeKind {
  if (bucket.totalOrders <= 0) return 'empty'
  if (bucket.otherFailedOrders > 0) return 'failed'
  if (bucket.expiredOrders > 0) return 'expired'
  if (bucket.pendingOrders > 0 && bucket.executedOrders === 0) return 'pending'
  return 'executed'
}

function bucketTooltip(bucket: OrderReliabilityBucket): string {
  const range = formatUtcRange(bucket.bucketStart, bucket.bucketEnd)
  if (bucket.totalOrders <= 0) {
    return `${range} · No committed orders`
  }
  const parts = [
    `${bucket.totalOrders.toLocaleString('en-US')} orders`,
    `${bucket.executedOrders.toLocaleString('en-US')} exec`,
  ]
  if (bucket.expiredOrders > 0) parts.push(`${bucket.expiredOrders.toLocaleString('en-US')} exp`)
  if (bucket.otherFailedOrders > 0) parts.push(`${bucket.otherFailedOrders.toLocaleString('en-US')} fail`)
  if (bucket.pendingOrders > 0) parts.push(`${bucket.pendingOrders.toLocaleString('en-US')} pending`)
  return `${range} · ${parts.join(' · ')}`
}

function outcomeBlockClass(kind: OutcomeKind): string {
  switch (kind) {
    case 'executed':
      return 'bg-positive/85 hover:bg-positive'
    case 'pending':
      return 'bg-brand-peach/65 hover:bg-brand-peach'
    case 'expired':
      return 'bg-brand-yellow/90 hover:bg-brand-yellow'
    case 'failed':
      return 'bg-brand-orange hover:bg-brand-orange/90'
    case 'empty':
      return 'bg-surface-panel/80 hover:bg-surface-panel'
  }
}

export function OrderOutcomeProportionCard({
  cohort,
}: {
  cohort: OrderReliabilityCohort
}) {
  const [selectedIndex, setSelectedIndex] = useState<number | null>(null)
  const total = cohort.totalOrders
  const timeline = cohort.timeline && cohort.timeline.length > 0 ? cohort.timeline : null
  const selectedBucket =
    timeline && selectedIndex != null && selectedIndex >= 0 && selectedIndex < timeline.length
      ? timeline[selectedIndex]
      : null

  const execPct = pctOf(cohort.executedOrders, total)
  const expPct = pctOf(cohort.expiredOrders, total)
  const failPct = pctOf(cohort.otherFailedOrders, total)
  const pendPct = pctOf(cohort.pendingOrders, total)

  return (
    <div className="space-y-2.5 border border-brand-border/20 bg-app-bg p-4">
      <div className="flex flex-wrap items-baseline justify-between gap-2">
        <div className="flex items-baseline gap-2">
          <span className="text-xs font-semibold uppercase tracking-[0.12em] text-content-secondary">
            {cohortTitle(cohort.windowSeconds)}
          </span>
          <span className="font-mono text-base font-semibold tabular-nums text-content-primary">
            {formatRateBps(cohort.fillRateBps)}
          </span>
          <span className="text-[11px] text-content-tertiary">filled</span>
        </div>
        <span className="font-mono text-xs tabular-nums text-content-tertiary">
          {total.toLocaleString('en-US')} {total === 1 ? 'order' : 'orders'}
        </span>
      </div>

      {timeline ? (
        <div className="space-y-1.5">
          <div
            className="flex h-6 w-full items-stretch gap-[3px]"
            role="group"
            aria-label={`${cohortTitle(cohort.windowSeconds)} order timeline: ${String(cohort.executedOrders)} executed, ${String(cohort.expiredOrders)} expired, ${String(cohort.otherFailedOrders)} failed`}
          >
            {timeline.map((bucket, index) => {
              const kind = classifyBucket(bucket)
              const active = selectedIndex === index
              const tip = bucketTooltip(bucket)
              return (
                <button
                  key={bucket.bucketStart}
                  type="button"
                  title={tip}
                  aria-label={tip}
                  aria-pressed={active}
                  onClick={() => {
                    setSelectedIndex((prev) => (prev === index ? null : index))
                  }}
                  className={`flex-1 transition-all focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-content-primary ${outcomeBlockClass(kind)} ${
                    active ? 'ring-1 ring-content-primary' : ''
                  }`}
                />
              )
            })}
          </div>
          <div className="flex items-center justify-between font-mono text-[10px] text-content-tertiary">
            <span>{cohortStartLabel(cohort.windowSeconds)}</span>
            <span>{bucketWidthLabel(cohort.windowSeconds, timeline.length)}</span>
            <span>Now</span>
          </div>
          {selectedBucket ? (
            <div className="flex flex-wrap items-center justify-between gap-2 border border-brand-border/25 bg-surface-panel/60 px-2.5 py-1.5 font-mono text-[11px]">
              <span className="text-content-primary">{bucketTooltip(selectedBucket)}</span>
              <button
                type="button"
                onClick={() => {
                  setSelectedIndex(null)
                }}
                className="text-[10px] text-content-tertiary hover:text-content-primary"
              >
                Clear
              </button>
            </div>
          ) : null}
        </div>
      ) : (
        <div className="space-y-1.5">
          <div
            className="flex h-3 w-full overflow-hidden bg-surface-panel"
            role="img"
            aria-label={`${cohortTitle(cohort.windowSeconds)} order outcomes: ${String(cohort.executedOrders)} executed, ${String(cohort.expiredOrders)} expired, ${String(cohort.otherFailedOrders)} failed`}
          >
            {total > 0 ? (
              <>
                {execPct > 0 ? (
                  <div
                    className="h-full bg-positive transition-all"
                    style={{ width: `${execPct.toFixed(2)}%` }}
                    title={`Executed: ${cohort.executedOrders.toLocaleString('en-US')} (${execPct.toFixed(2)}%)`}
                  />
                ) : null}
                {pendPct > 0 ? (
                  <div
                    className="h-full bg-brand-peach/70 transition-all"
                    style={{ width: `${pendPct.toFixed(2)}%` }}
                    title={`Pending: ${cohort.pendingOrders.toLocaleString('en-US')} (${pendPct.toFixed(2)}%)`}
                  />
                ) : null}
                {expPct > 0 ? (
                  <div
                    className="h-full bg-brand-yellow transition-all"
                    style={{ width: `${Math.max(expPct, 0.75).toFixed(2)}%` }}
                    title={`Expired (r=2): ${cohort.expiredOrders.toLocaleString('en-US')} (${expPct.toFixed(2)}%)`}
                  />
                ) : null}
                {failPct > 0 ? (
                  <div
                    className="h-full bg-brand-orange transition-all"
                    style={{ width: `${Math.max(failPct, 0.75).toFixed(2)}%` }}
                    title={`Failed: ${cohort.otherFailedOrders.toLocaleString('en-US')} (${failPct.toFixed(2)}%)`}
                  />
                ) : null}
              </>
            ) : null}
          </div>
          <p className="font-mono text-[10px] text-content-tertiary">
            Chronological per-window blocks activate after backend deploy
          </p>
        </div>
      )}

      <div className="flex flex-wrap items-center justify-between gap-x-4 gap-y-1 font-mono text-[11px] tabular-nums">
        <div className="flex flex-wrap items-center gap-x-3 gap-y-1">
          <span className="inline-flex items-center gap-1.5 text-content-secondary">
            <span className="h-1.5 w-1.5 rounded-full bg-positive" aria-hidden="true" />
            <span className="text-positive">{cohort.executedOrders.toLocaleString('en-US')}</span>
            <span className="text-content-tertiary">exec</span>
          </span>
          <span className="inline-flex items-center gap-1.5 text-content-secondary">
            <span className="h-1.5 w-1.5 rounded-full bg-brand-yellow" aria-hidden="true" />
            <span className={cohort.expiredOrders > 0 ? 'text-brand-yellow' : 'text-content-tertiary'}>
              {cohort.expiredOrders.toLocaleString('en-US')}
            </span>
            <span className="text-content-tertiary">exp</span>
          </span>
          <span className="inline-flex items-center gap-1.5 text-content-secondary">
            <span className="h-1.5 w-1.5 rounded-full bg-brand-orange" aria-hidden="true" />
            <span className={cohort.otherFailedOrders > 0 ? 'text-brand-orange' : 'text-content-tertiary'}>
              {cohort.otherFailedOrders.toLocaleString('en-US')}
            </span>
            <span className="text-content-tertiary">fail</span>
          </span>
          {cohort.pendingOrders > 0 ? (
            <span className="inline-flex items-center gap-1.5 text-content-secondary">
              <span className="h-1.5 w-1.5 rounded-full bg-brand-peach/70" aria-hidden="true" />
              <span className="text-brand-peach">{cohort.pendingOrders.toLocaleString('en-US')}</span>
              <span className="text-content-tertiary">pend</span>
            </span>
          ) : null}
        </div>
        <span className="text-content-tertiary">
          {cohort.affectedAccounts}/{cohort.activeAccounts} accts affected
        </span>
      </div>
    </div>
  )
}

function posPct(value: number | null, maxVal: number): number | null {
  if (value == null || !Number.isFinite(value) || maxVal <= 0) return null
  return Math.max(0, Math.min(100, (value / maxVal) * 100))
}

function formatLatencyVal(value: number | null, unit: string): string {
  if (value == null || !Number.isFinite(value)) return '—'
  return `${value.toLocaleString('en-US')}${unit}`
}

export function ExecutionLatencyWhisker({
  seconds,
  blocks,
}: {
  seconds: LatencyDistribution
  blocks: LatencyDistribution
}) {
  const [mode, setMode] = useState<'seconds' | 'blocks'>('seconds')
  const dist = mode === 'seconds' ? seconds : blocks
  const unit = mode === 'seconds' ? 's' : ' blk'
  const hasSamples = dist.sampleCount > 0 && (dist.max != null || dist.p99 != null || dist.p95 != null || dist.p50 != null)

  const scaleMax = Math.max(
    dist.max ?? 0,
    dist.p99 ?? 0,
    dist.p95 ?? 0,
    dist.p50 ?? 0,
    1,
  )

  const p50Pos = posPct(dist.p50, scaleMax)
  const p95Pos = posPct(dist.p95, scaleMax)
  const p99Pos = posPct(dist.p99, scaleMax)
  const maxPos = posPct(dist.max, scaleMax)

  return (
    <Panel className="flex flex-col justify-between p-5">
      <div>
        <div className="flex flex-wrap items-start justify-between gap-3">
          <div>
            <h3 className="text-base font-semibold">Latency distribution whisker</h3>
            <p className="mt-0.5 text-xs text-content-tertiary">
              Commit-to-execution median vs. tail spread ({dist.sampleCount.toLocaleString('en-US')}{' '}
              {dist.sampleCount === 1 ? 'fill' : 'fills'})
            </p>
          </div>
          <div className="inline-flex border border-brand-border/30 bg-app-bg p-0.5 text-xs font-semibold">
            <button
              type="button"
              aria-pressed={mode === 'seconds'}
              onClick={() => { setMode('seconds') }}
              className={`px-2.5 py-1 transition-colors ${
                mode === 'seconds'
                  ? 'bg-brand-orange text-content-primary'
                  : 'text-content-secondary hover:text-content-primary'
              }`}
            >
              Seconds
            </button>
            <button
              type="button"
              aria-pressed={mode === 'blocks'}
              onClick={() => { setMode('blocks') }}
              className={`px-2.5 py-1 transition-colors ${
                mode === 'blocks'
                  ? 'bg-brand-orange text-content-primary'
                  : 'text-content-secondary hover:text-content-primary'
              }`}
            >
              Blocks
            </button>
          </div>
        </div>

        {hasSamples ? (
          <div className="mt-5 space-y-4">
            <div className="border border-brand-border/20 bg-app-bg px-4 py-4">
              <div
                className="relative flex h-6 items-center"
                role="img"
                aria-label={`Execution latency whisker (${mode}): p50 ${formatLatencyVal(dist.p50, unit)}, p95 ${formatLatencyVal(dist.p95, unit)}, p99 ${formatLatencyVal(dist.p99, unit)}, max ${formatLatencyVal(dist.max, unit)}`}
              >
                {/* Full whisker rail from 0 to max */}
                <div className="h-0.5 w-full bg-brand-border/25" />

                {/* 0 -> p50 core median band */}
                {p50Pos != null ? (
                  <div
                    className="absolute left-0 h-2 bg-positive/35"
                    style={{ width: `${String(p50Pos)}%` }}
                  />
                ) : null}

                {/* p50 -> p95 interquartile/tail band */}
                {p50Pos != null && p95Pos != null && p95Pos >= p50Pos ? (
                  <div
                    className="absolute h-2 bg-brand-peach/50"
                    style={{
                      left: `${String(p50Pos)}%`,
                      width: `${String(Math.max(1, p95Pos - p50Pos))}%`,
                    }}
                  />
                ) : null}

                {/* p95 -> p99 tail whisker */}
                {p95Pos != null && p99Pos != null && p99Pos >= p95Pos ? (
                  <div
                    className="absolute h-1 bg-brand-yellow/75"
                    style={{
                      left: `${String(p95Pos)}%`,
                      width: `${String(Math.max(0.5, p99Pos - p95Pos))}%`,
                    }}
                  />
                ) : null}

                {/* p99 -> max extreme tail whisker */}
                {p99Pos != null && maxPos != null && maxPos >= p99Pos ? (
                  <div
                    className="absolute h-0.5 bg-brand-orange/85"
                    style={{
                      left: `${String(p99Pos)}%`,
                      width: `${String(Math.max(0.5, maxPos - p99Pos))}%`,
                    }}
                  />
                ) : null}

                {/* Start cap at 0 */}
                <span className="absolute left-0 h-3 w-0.5 bg-brand-border/45" aria-hidden="true" />

                {/* p50 median dot */}
                {p50Pos != null ? (
                  <span
                    className="absolute h-3 w-3 -translate-x-1/2 rounded-full border border-app-bg bg-positive"
                    style={{ left: `${String(p50Pos)}%` }}
                    title={`p50 median: ${formatLatencyVal(dist.p50, unit)}`}
                  />
                ) : null}

                {/* p95 dot */}
                {p95Pos != null ? (
                  <span
                    className="absolute h-2.5 w-2.5 -translate-x-1/2 rounded-full border border-app-bg bg-brand-peach"
                    style={{ left: `${String(p95Pos)}%` }}
                    title={`p95: ${formatLatencyVal(dist.p95, unit)}`}
                  />
                ) : null}

                {/* p99 tick */}
                {p99Pos != null ? (
                  <span
                    className="absolute h-3.5 w-0.5 -translate-x-1/2 bg-brand-yellow"
                    style={{ left: `${String(p99Pos)}%` }}
                    title={`p99: ${formatLatencyVal(dist.p99, unit)}`}
                  />
                ) : null}

                {/* max end-cap */}
                {maxPos != null ? (
                  <span
                    className="absolute h-4 w-0.5 -translate-x-1/2 bg-brand-orange"
                    style={{ left: `${String(maxPos)}%` }}
                    title={`Max: ${formatLatencyVal(dist.max, unit)}`}
                  />
                ) : null}
              </div>

              <div className="mt-2 flex items-center justify-between font-mono text-[10px] tabular-nums text-content-tertiary">
                <span>0{unit}</span>
                <span>{formatLatencyVal(dist.max ?? scaleMax, unit)} max</span>
              </div>
            </div>

            <div className="grid grid-cols-2 gap-2.5 sm:grid-cols-4">
              <div className="border border-brand-border/20 bg-app-bg px-3 py-2.5">
                <div className="flex items-center gap-1.5 text-[11px] text-content-tertiary">
                  <span className="h-2 w-2 rounded-full bg-positive" aria-hidden="true" />
                  <span>p50 median</span>
                </div>
                <p className="mt-1 font-mono text-sm font-semibold tabular-nums text-positive">
                  {formatLatencyVal(dist.p50, unit)}
                </p>
              </div>
              <div className="border border-brand-border/20 bg-app-bg px-3 py-2.5">
                <div className="flex items-center gap-1.5 text-[11px] text-content-tertiary">
                  <span className="h-2 w-2 rounded-full bg-brand-peach" aria-hidden="true" />
                  <span>p95</span>
                </div>
                <p className="mt-1 font-mono text-sm font-semibold tabular-nums text-brand-peach">
                  {formatLatencyVal(dist.p95, unit)}
                </p>
              </div>
              <div className="border border-brand-border/20 bg-app-bg px-3 py-2.5">
                <div className="flex items-center gap-1.5 text-[11px] text-content-tertiary">
                  <span className="h-2 w-0.5 bg-brand-yellow" aria-hidden="true" />
                  <span>p99</span>
                </div>
                <p className="mt-1 font-mono text-sm font-semibold tabular-nums text-brand-yellow">
                  {formatLatencyVal(dist.p99, unit)}
                </p>
              </div>
              <div className="border border-brand-border/20 bg-app-bg px-3 py-2.5">
                <div className="flex items-center gap-1.5 text-[11px] text-content-tertiary">
                  <span className="h-2 w-0.5 bg-brand-orange" aria-hidden="true" />
                  <span>Max</span>
                </div>
                <p className="mt-1 font-mono text-sm font-semibold tabular-nums text-brand-orange">
                  {formatLatencyVal(dist.max, unit)}
                </p>
              </div>
            </div>
          </div>
        ) : (
          <EmptyState
            title="Latency distribution unavailable"
            message="Commit-to-execution latency percentiles will appear once finalized fills are indexed."
          />
        )}
      </div>
    </Panel>
  )
}
