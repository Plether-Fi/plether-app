import { useState } from 'react'
import type { OracleHealthBucket, OracleSliceStatus } from '../api'
import { Panel } from './ui'

type OracleWindowId = '1h' | '24h'

function formatUtcHm(iso: string): string {
  const d = new Date(iso)
  if (Number.isNaN(d.getTime())) return iso
  const hh = String(d.getUTCHours()).padStart(2, '0')
  const mm = String(d.getUTCMinutes()).padStart(2, '0')
  return `${hh}:${mm}`
}

function sliceColorClass(status: OracleSliceStatus): string {
  switch (status) {
    case 'healthy':
      return 'bg-positive'
    case 'rejected':
      return 'bg-brand-orange'
    case 'stale':
    case 'degraded':
      return 'bg-brand-yellow'
    default:
      return 'bg-brand-border/25'
  }
}

function sliceLabel(status: OracleSliceStatus): string {
  switch (status) {
    case 'healthy':
      return 'Healthy'
    case 'rejected':
      return 'Rejected'
    case 'stale':
      return 'Stale (FX closed)'
    case 'degraded':
      return 'Near limit'
    default:
      return 'No updates'
  }
}

function sliceBadgeToneClass(status: OracleSliceStatus): string {
  switch (status) {
    case 'healthy':
      return 'text-positive'
    case 'rejected':
      return 'text-brand-orange'
    case 'stale':
    case 'degraded':
      return 'text-brand-yellow'
    default:
      return 'text-content-tertiary'
  }
}

export function OracleHealthBlocks({
  window1h,
  window24h,
  divergenceLimitSeconds,
}: {
  window1h: OracleHealthBucket[]
  window24h: OracleHealthBucket[]
  divergenceLimitSeconds: number
}) {
  const [activeWindow, setActiveWindow] = useState<OracleWindowId>('24h')
  const [selectedIdx, setSelectedIdx] = useState<number | null>(null)

  const timeline = activeWindow === '1h' ? window1h : window24h
  const sliceMinutes = activeWindow === '1h' ? 2 : 30
  const fallbackIndex = (() => {
    for (let i = timeline.length - 1; i >= 0; i -= 1) {
      const b = timeline[i]
      if (b.rejectedCount > 0 || b.admittedCount > 0 || b.staleCount > 0) return i
    }
    return timeline.length > 0 ? timeline.length - 1 : -1
  })()
  const activeIdx =
    selectedIdx !== null && selectedIdx >= 0 && selectedIdx < timeline.length
      ? selectedIdx
      : fallbackIndex
  const activeSlice = activeIdx >= 0 ? timeline[activeIdx] : null

  const counts = timeline.reduce(
    (acc, item) => {
      if (item.status === 'healthy') acc.healthy += 1
      else if (item.status === 'rejected') acc.rejected += 1
      else if (item.status === 'stale' || item.status === 'degraded') acc.warn += 1
      else acc.idle += 1
      return acc
    },
    { healthy: 0, warn: 0, rejected: 0, idle: 0 },
  )

  const firstStart = timeline[0]?.bucketStart
  const midStart = timeline[Math.floor(timeline.length / 2)]?.bucketStart
  const lastEnd = timeline[timeline.length - 1]?.bucketEnd

  return (
    <Panel className="p-5 sm:p-6">
      <div className="flex flex-wrap items-start justify-between gap-3">
        <div>
          <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
            Oracle Update Health Timeline
          </p>
          <p className="mt-1 text-xs text-content-secondary">
            Chronological {String(sliceMinutes)}-minute UTC slices across admitted Pyth FX updates,
            near-limit spread warnings, stale market-closed polls, and rejected payloads.
          </p>
        </div>
        <div className="flex items-center gap-1" role="group" aria-label="Oracle timeline window">
          {(['1h', '24h'] as const).map((win) => (
            <button
              key={win}
              type="button"
              aria-pressed={activeWindow === win}
              onClick={() => {
                setActiveWindow(win)
                setSelectedIdx(null)
              }}
              className={`cursor-pointer border px-2.5 py-1 font-mono text-xs font-semibold transition-colors ${
                activeWindow === win
                  ? 'border-brand-orange bg-brand-orange text-content-primary'
                  : 'border-brand-border/30 bg-app-bg text-content-secondary hover:border-brand-peach/50 hover:text-content-primary'
              }`}
            >
              {win}
            </button>
          ))}
        </div>
      </div>

      {timeline.length > 0 ? (
        <>
          <div
            className="mt-4 flex h-9 w-full items-stretch gap-[2px]"
            role="listbox"
            aria-label={`Oracle health ${activeWindow} slices`}
          >
            {timeline.map((bucket, idx) => {
              const isSelected = idx === activeIdx
              const range = `${formatUtcHm(bucket.bucketStart)}–${formatUtcHm(bucket.bucketEnd)} UTC`
              return (
                <button
                  key={bucket.bucketStart}
                  type="button"
                  role="option"
                  aria-selected={isSelected}
                  aria-label={`${range}: ${sliceLabel(bucket.status)}, ${String(bucket.admittedCount)} admitted, ${String(bucket.rejectedCount)} rejected`}
                  title={`${range} · ${sliceLabel(bucket.status)} · ${String(bucket.admittedCount)} admitted · ${String(bucket.rejectedCount)} rejected`}
                  onClick={() => {
                    setSelectedIdx(idx)
                  }}
                  onMouseEnter={() => {
                    setSelectedIdx(idx)
                  }}
                  className={`flex-1 cursor-pointer transition-opacity ${sliceColorClass(bucket.status)} ${
                    isSelected
                      ? 'opacity-100 ring-2 ring-brand-peach ring-offset-1 ring-offset-surface-panel'
                      : 'opacity-85 hover:opacity-100'
                  }`}
                />
              )
            })}
          </div>

          <div className="mt-1.5 flex items-center justify-between font-mono text-[10px] text-content-tertiary">
            <span>{firstStart ? `${formatUtcHm(firstStart)} UTC` : ''}</span>
            <span>{midStart ? `${formatUtcHm(midStart)} UTC` : ''}</span>
            <span>{lastEnd ? `${formatUtcHm(lastEnd)} UTC (Now)` : ''}</span>
          </div>

          <div className="mt-3 flex flex-wrap items-center gap-x-4 gap-y-1.5 text-[11px] text-content-secondary">
            <span className="inline-flex items-center gap-1.5">
              <span aria-hidden="true" className="h-2.5 w-2.5 bg-positive" />
              Healthy ({counts.healthy})
            </span>
            <span className="inline-flex items-center gap-1.5">
              <span aria-hidden="true" className="h-2.5 w-2.5 bg-brand-yellow" />
              Near limit / Stale ({counts.warn})
            </span>
            <span className="inline-flex items-center gap-1.5">
              <span aria-hidden="true" className="h-2.5 w-2.5 bg-brand-orange" />
              Rejected ({counts.rejected})
            </span>
            <span className="inline-flex items-center gap-1.5">
              <span aria-hidden="true" className="h-2.5 w-2.5 bg-brand-border/35" />
              Idle ({counts.idle})
            </span>
          </div>

          {activeSlice ? (
            <div className="mt-4 flex flex-wrap items-center justify-between gap-3 border-t border-brand-border/20 pt-3 font-mono text-xs">
              <div className="flex flex-wrap items-center gap-2">
                <span className="font-semibold text-content-primary">
                  {formatUtcHm(activeSlice.bucketStart)}–{formatUtcHm(activeSlice.bucketEnd)} UTC
                </span>
                <span className={`font-semibold ${sliceBadgeToneClass(activeSlice.status)}`}>
                  · {sliceLabel(activeSlice.status)}
                </span>
              </div>
              <div className="flex flex-wrap items-center gap-x-4 gap-y-1 text-content-secondary tabular-nums">
                <span>
                  Admitted:{' '}
                  <strong className="text-positive">
                    {activeSlice.admittedCount.toLocaleString('en-US')}
                  </strong>
                  {activeSlice.revealCount > 0 ? ` (${String(activeSlice.revealCount)} reveal)` : ''}
                </span>
                <span>
                  Rejected:{' '}
                  <strong
                    className={
                      activeSlice.rejectedCount > 0 ? 'text-brand-orange' : 'text-content-primary'
                    }
                  >
                    {activeSlice.rejectedCount.toLocaleString('en-US')}
                  </strong>
                </span>
                {activeSlice.staleCount > 0 ? (
                  <span>
                    Stale polls:{' '}
                    <strong className="text-brand-yellow">
                      {activeSlice.staleCount.toLocaleString('en-US')}
                    </strong>
                  </span>
                ) : null}
                <span>
                  Max spread:{' '}
                  <strong className="text-content-primary">
                    {activeSlice.maxSpreadSeconds != null
                      ? `${String(activeSlice.maxSpreadSeconds)}s / ${String(divergenceLimitSeconds)}s`
                      : '—'}
                  </strong>
                </span>
                <span>
                  Max lag:{' '}
                  <strong className="text-content-primary">
                    {activeSlice.maxLagSeconds != null ? `${String(activeSlice.maxLagSeconds)}s` : '—'}
                  </strong>
                </span>
              </div>
            </div>
          ) : null}
        </>
      ) : (
        <p className="mt-4 text-xs text-content-tertiary">
          No chronological oracle slices available for this window yet.
        </p>
      )}
    </Panel>
  )
}
