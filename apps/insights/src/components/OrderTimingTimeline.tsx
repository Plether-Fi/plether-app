import { useState } from 'react'
import type {
  FinalizedOrderRecord,
  ProtocolLatencyBenchmark,
  TransactionBatchSummary,
} from '../api'
import { arbiscanLinkKey } from '../utils/chain'
import {
  formatPrice,
  formatTimeSecondsUtc,
  formatUtcWithSeconds,
  shortAddress,
} from '../utils/format'
import { ArbiscanButton, CopyButton } from './ActionButtons'
import { OraclePriceLink } from './OraclePriceLink'
import { StatusPill, type StatusTone } from './StatusPill'
import { Panel } from './ui'

export interface OrderTimingTimelineProps {
  finalizedOrders: FinalizedOrderRecord[]
  activeOrderId?: string | null
  batchSummary?: TransactionBatchSummary | null
  latencyBenchmark?: ProtocolLatencyBenchmark | null
  fallbackFinalizedAt?: string | null
  fallbackFinalizedBlock?: string | null
  txHash?: string | null
  chainId?: number | null
  excludeExternalKeys?: ReadonlySet<string>
}

function parseUnixSeconds(iso: string | null | undefined): number | null {
  if (!iso) return null
  const ms = Date.parse(iso)
  return Number.isFinite(ms) ? Math.floor(ms / 1000) : null
}

function formatSignedDelta(seconds: number | null | undefined): string {
  if (seconds == null || !Number.isFinite(seconds)) return '—'
  if (seconds === 0) return '0s'
  return seconds > 0 ? `+${String(seconds)}s` : `${String(seconds)}s`
}

function formatBlock(block: string | null | undefined): string | null {
  if (!block || !/^\d+$/.test(block)) return null
  return `#${Number(block).toLocaleString('en-US')}`
}

const ARBITRUM_BLOCK_MS = 250

function formatArbitrumBlockEstimateMs(blocks: number | null | undefined): string | null {
  if (blocks == null || !Number.isFinite(blocks) || blocks < 0) return null
  return `~${Math.round(blocks * ARBITRUM_BLOCK_MS).toLocaleString('en-US')} ms`
}

function percentileSummary(
  percentile: number | null | undefined,
  latencySeconds: number | null | undefined,
  benchmark: ProtocolLatencyBenchmark | null | undefined,
): { tone: StatusTone; label: string; context: string | null } {
  if (percentile != null) {
    const fasterThan = Math.max(0, Math.min(99, 100 - percentile))
    if (percentile <= 25) {
      return {
        tone: 'live',
        label: `P${String(percentile)} · Top quartile`,
        context: `Faster than ${String(fasterThan)}% of recent orders`,
      }
    }
    if (percentile <= 60) {
      return {
        tone: 'live',
        label: `P${String(percentile)} · Typical`,
        context:
          benchmark?.p50Seconds != null
            ? `At protocol median (p50 ${String(benchmark.p50Seconds)}s)`
            : `Faster than ${String(fasterThan)}% of recent orders`,
      }
    }
    if (percentile <= 90) {
      return {
        tone: 'ready',
        label: `P${String(percentile)} · Above median`,
        context:
          benchmark?.p50Seconds != null && latencySeconds != null
            ? `+${String(Math.max(0, latencySeconds - benchmark.p50Seconds))}s vs p50 (${String(benchmark.p50Seconds)}s)`
            : `Slower than ${String(percentile)}% of recent orders`,
      }
    }
    if (percentile <= 95) {
      return {
        tone: 'degraded',
        label: `P${String(percentile)} · Near p95`,
        context:
          benchmark?.p95Seconds != null
            ? `Approaching protocol p95 (${String(benchmark.p95Seconds)}s)`
            : 'Upper decile latency',
      }
    }
    return {
      tone: 'blocked',
      label: `P${String(percentile)} · Slow tail (> p95)`,
      context:
        benchmark?.p95Seconds != null
          ? `Above protocol p95 (${String(benchmark.p95Seconds)}s)`
          : 'Tail execution latency',
    }
  }

  if (latencySeconds != null && benchmark?.p50Seconds != null) {
    if (latencySeconds <= benchmark.p50Seconds) {
      return {
        tone: 'live',
        label: `≤ p50 (${String(benchmark.p50Seconds)}s)`,
        context: 'At or faster than protocol median',
      }
    }
    if (benchmark.p95Seconds != null && latencySeconds > benchmark.p95Seconds) {
      return {
        tone: 'blocked',
        label: `> p95 (${String(benchmark.p95Seconds)}s)`,
        context: 'Tail execution latency',
      }
    }
    return {
      tone: 'ready',
      label: `${String(latencySeconds)}s total`,
      context: `Protocol p50 ${String(benchmark.p50Seconds)}s`,
    }
  }

  return {
    tone: 'unknown',
    label: latencySeconds != null ? `${String(latencySeconds)}s total` : 'Timing recorded',
    context: null,
  }
}

function resolveExecutionWindowLimit(order: FinalizedOrderRecord): number | null {
  const check = order.orderProtections?.executionChecks.find(
    (item) => item.key === 'execution_window',
  )
  if (!check?.limitValue || !/^\d+$/.test(check.limitValue)) return null
  const parsed = Number(check.limitValue)
  return Number.isFinite(parsed) && parsed > 0 ? parsed : null
}

function LatencyDistributionGauge({
  orderLatency,
  percentile,
  benchmark,
}: {
  orderLatency: number
  percentile: number | null
  benchmark: ProtocolLatencyBenchmark
}) {
  const p50 = benchmark.p50Seconds
  const p95 = benchmark.p95Seconds
  const p99 = benchmark.p99Seconds
  const maxRef = Math.max(p99 ?? 0, p95 ?? 0, p50 ?? 0, orderLatency, 4)
  const scaleMax = Math.ceil(maxRef * 1.12)
  const pctPos = (seconds: number): number =>
    Math.max(1.5, Math.min(98.5, (seconds / scaleMax) * 100))

  const p50Pos = p50 != null ? pctPos(p50) : null
  const p95Pos = p95 != null ? pctPos(p95) : null
  const p99Pos = p99 != null ? pctPos(p99) : null
  const orderPos = pctPos(orderLatency)

  const pinColor =
    percentile != null && percentile > 95
      ? 'bg-brand-orange border-brand-orange text-brand-orange'
      : percentile != null && percentile > 85
        ? 'bg-brand-yellow border-brand-yellow text-brand-yellow'
        : 'bg-positive border-positive text-positive'

  const ticks = [
    ...(p50 != null ? [{ key: 'p50', label: `p50 ${String(p50)}s`, pos: pctPos(p50) }] : []),
    ...(p95 != null && p95 !== p50
      ? [{ key: 'p95', label: `p95 ${String(p95)}s`, pos: pctPos(p95) }]
      : []),
    ...(p99 != null && p99 !== p95 && p99 !== p50
      ? [{ key: 'p99', label: `p99 ${String(p99)}s`, pos: pctPos(p99) }]
      : []),
  ]

  // Prevent tick labels from colliding when p50/p95/p99 are close together on narrow phones
  const spacedTicks: { key: string; label: string; pos: number; labelPos: number }[] = []
  for (const tick of ticks) {
    const labelPos =
      spacedTicks.length > 0
        ? Math.max(tick.pos, spacedTicks[spacedTicks.length - 1].labelPos + 19)
        : tick.pos
    spacedTicks.push({ ...tick, labelPos: Math.min(92, labelPos) })
  }

  return (
    <div className="space-y-2.5 border-t border-brand-border/20 pt-4">
      <div className="flex flex-wrap items-baseline justify-between gap-2">
        <p className="font-mono text-[11px] font-semibold uppercase tracking-[0.1em] text-content-secondary">
          Protocol Latency Position ({benchmark.windowLabel} ·{' '}
          {benchmark.sampleCount.toLocaleString('en-US')} executed orders)
        </p>
        <p className="font-mono text-xs tabular-nums text-content-primary">
          This order: <span className="font-semibold">{String(orderLatency)}s</span>
          {percentile != null ? (
            <span className="text-brand-peach"> (P{String(percentile)})</span>
          ) : null}
        </p>
      </div>

      {/* Horizontal distribution track */}
      <div className="relative pb-6 pt-5">
        {/* Order callout pin above track */}
        <div
          className="pointer-events-none absolute top-0 -translate-x-1/2 whitespace-nowrap font-mono text-[10px] font-semibold"
          style={{ left: `${Math.max(8, Math.min(92, orderPos)).toFixed(2)}%` }}
        >
          <span className="border border-brand-border/40 bg-app-bg px-1.5 py-0.5 text-content-primary">
            {String(orderLatency)}s{percentile != null ? ` · P${String(percentile)}` : ''}
          </span>
        </div>

        {/* Banded bar */}
        <div className="relative h-2.5 w-full overflow-hidden border border-brand-border/25 bg-app-bg">
          {p50Pos != null ? (
            <div
              className="absolute inset-y-0 left-0 bg-positive/25"
              style={{ width: `${p50Pos.toFixed(2)}%` }}
            />
          ) : null}
          {p50Pos != null && p95Pos != null && p95Pos > p50Pos ? (
            <div
              className="absolute inset-y-0 bg-brand-peach/20"
              style={{
                left: `${p50Pos.toFixed(2)}%`,
                width: `${(p95Pos - p50Pos).toFixed(2)}%`,
              }}
            />
          ) : null}
          {p95Pos != null && p99Pos != null && p99Pos > p95Pos ? (
            <div
              className="absolute inset-y-0 bg-brand-orange/25"
              style={{
                left: `${p95Pos.toFixed(2)}%`,
                width: `${(p99Pos - p95Pos).toFixed(2)}%`,
              }}
            />
          ) : null}

          {/* Benchmark vertical hairline ticks */}
          {spacedTicks.map((tick) => (
            <div
              key={tick.key}
              className="absolute inset-y-0 w-px bg-content-tertiary/70"
              style={{ left: `${tick.pos.toFixed(2)}%` }}
            />
          ))}
        </div>

        {/* Highlighted marker dot for this order */}
        <div
          aria-hidden="true"
          className={`absolute top-[18px] h-3.5 w-3.5 -translate-x-1/2 rounded-full border-2 border-app-bg shadow ${pinColor.split(' ')[0]}`}
          style={{ left: `${orderPos.toFixed(2)}%` }}
        />

        {/* Tick labels below track */}
        <div className="relative mt-1.5 h-4 font-mono text-[10px] text-content-tertiary">
          <span className="absolute left-0">0s</span>
          {spacedTicks.map((tick) => (
            <span
              key={tick.key}
              className="absolute -translate-x-1/2 whitespace-nowrap tabular-nums"
              style={{ left: `${tick.labelPos.toFixed(2)}%` }}
            >
              {tick.label}
            </span>
          ))}
        </div>
      </div>

      {/* Benchmark percentile breakdown strip */}
      <div className="flex flex-wrap items-center justify-between gap-2 font-mono text-[11px] text-content-tertiary">
        <div className="flex flex-wrap items-center gap-x-3 gap-y-1 tabular-nums">
          {benchmark.minSeconds != null ? <span>min {String(benchmark.minSeconds)}s</span> : null}
          {benchmark.p25Seconds != null ? <span>p25 {String(benchmark.p25Seconds)}s</span> : null}
          {benchmark.p50Seconds != null ? (
            <span className="text-content-secondary">p50 {String(benchmark.p50Seconds)}s</span>
          ) : null}
          {benchmark.p75Seconds != null ? <span>p75 {String(benchmark.p75Seconds)}s</span> : null}
          {benchmark.p95Seconds != null ? (
            <span className="text-brand-yellow">p95 {String(benchmark.p95Seconds)}s</span>
          ) : null}
          {benchmark.p99Seconds != null ? (
            <span className="text-brand-orange">p99 {String(benchmark.p99Seconds)}s</span>
          ) : null}
        </div>
        {benchmark.p50CommitToOracleSeconds != null ||
        benchmark.p50OracleToFinalizeSeconds != null ? (
          <span className="tabular-nums">
            Median split: Commit→Oracle {formatSignedDelta(benchmark.p50CommitToOracleSeconds)} ·
            Oracle→Finalize {formatSignedDelta(benchmark.p50OracleToFinalizeSeconds)}
          </span>
        ) : null}
      </div>
    </div>
  )
}

export function OrderTimingTimelinePanel({
  finalizedOrders,
  activeOrderId,
  batchSummary,
  latencyBenchmark,
  fallbackFinalizedAt,
  fallbackFinalizedBlock,
  txHash,
  chainId,
  excludeExternalKeys,
}: OrderTimingTimelineProps) {
  const [manualOrderId, setManualOrderId] = useState<string | null>(null)

  if (finalizedOrders.length === 0) return null

  const selectedOrder =
    (activeOrderId ? finalizedOrders.find((o) => o.orderId === activeOrderId) : null) ??
    (manualOrderId ? finalizedOrders.find((o) => o.orderId === manualOrderId) : null) ??
    finalizedOrders[0]

  const commitAt = selectedOrder.commitAt
  const commitBlock = selectedOrder.commitBlock
  const commitTxHash = selectedOrder.commitTxHash
  const finalizedAt = selectedOrder.finalizedAt ?? fallbackFinalizedAt ?? null
  const finalizedBlock = selectedOrder.finalizedBlock ?? fallbackFinalizedBlock ?? null
  const oracleMin = selectedOrder.oracleMinPublishTime ?? batchSummary?.oracleMinPublishTime ?? null
  const oracleMax = selectedOrder.oracleMaxPublishTime ?? batchSummary?.oracleMaxPublishTime ?? null

  const commitSec = parseUnixSeconds(commitAt)
  const oracleMinSec = parseUnixSeconds(oracleMin)
  const oracleMaxSec = parseUnixSeconds(oracleMax)
  const finalizedSec = parseUnixSeconds(finalizedAt)

  const oracleSpreadSeconds =
    selectedOrder.oracleSpreadSeconds ??
    (oracleMinSec != null && oracleMaxSec != null
      ? Math.max(0, oracleMaxSec - oracleMinSec)
      : batchSummary?.oracleSpreadSeconds ?? null)

  const commitToOracleSeconds =
    selectedOrder.commitToOracleMaxSeconds ??
    (oracleMaxSec != null && commitSec != null ? oracleMaxSec - commitSec : null)

  const oracleToFinalizeSeconds =
    selectedOrder.oracleToFinalizeSeconds ??
    (finalizedSec != null && oracleMaxSec != null ? finalizedSec - oracleMaxSec : null)

  const totalLatencySeconds =
    selectedOrder.latencySeconds ??
    (finalizedSec != null && commitSec != null ? Math.max(0, finalizedSec - commitSec) : null)

  const totalLatencyBlocks = selectedOrder.latencyBlocks
  const estimatedBlockMs = formatArbitrumBlockEstimateMs(totalLatencyBlocks)
  const executionWindowLimit = resolveExecutionWindowLimit(selectedOrder)
  const windowUsedPct =
    executionWindowLimit != null && totalLatencySeconds != null
      ? Math.min(100, (totalLatencySeconds / executionWindowLimit) * 100)
      : null

  const percentile = selectedOrder.latencyPercentile ?? null
  const pctBadge = percentileSummary(percentile, totalLatencySeconds, latencyBenchmark)

  const oraclePrice = selectedOrder.oraclePrice ?? batchSummary?.oraclePrice ?? null
  const oracleUpdateId = selectedOrder.oracleUpdateId ?? batchSummary?.oracleUpdateId ?? null
  const oracleFrozen = selectedOrder.oracleFrozen ?? batchSummary?.oracleFrozen ?? false

  const commitExternalKey =
    commitTxHash != null ? arbiscanLinkKey('tx', commitTxHash, chainId) : null
  const showCommitArb =
    commitTxHash != null &&
    commitExternalKey != null &&
    !(excludeExternalKeys?.has(commitExternalKey) ?? false)

  // Phase bar proportions when both positive segments are known
  const positiveCommitToOracle =
    commitToOracleSeconds != null ? Math.max(0, commitToOracleSeconds) : null
  const positiveOracleToFinalize =
    oracleToFinalizeSeconds != null ? Math.max(0, oracleToFinalizeSeconds) : null
  const phaseSum =
    positiveCommitToOracle != null && positiveOracleToFinalize != null
      ? positiveCommitToOracle + positiveOracleToFinalize
      : 0
  const commitToOracleSharePct =
    phaseSum > 0 && positiveCommitToOracle != null
      ? (positiveCommitToOracle / phaseSum) * 100
      : null
  const oracleToFinalizeSharePct =
    phaseSum > 0 && positiveOracleToFinalize != null
      ? (positiveOracleToFinalize / phaseSum) * 100
      : null

  const oraclePublishedBeforeCommit =
    commitToOracleSeconds != null && commitToOracleSeconds < 0

  return (
    <section aria-labelledby="order-timing-title">
      <Panel className="space-y-5 p-4 sm:p-6">
        {/* Header */}
        <div className="flex flex-wrap items-start justify-between gap-3">
          <div className="space-y-1">
            <div className="flex flex-wrap items-center gap-2">
              <p className="font-mono text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                Order Execution Timing
              </p>
              <span className="border border-brand-border/35 bg-app-bg px-2 py-0.5 font-mono text-[11px] text-content-secondary">
                Order #{selectedOrder.orderId}
              </span>
              {selectedOrder.side ? (
                <span
                  className={`font-mono text-[11px] font-semibold uppercase ${
                    selectedOrder.side === 'long'
                      ? 'text-positive'
                      : selectedOrder.side === 'short'
                        ? 'text-brand-orange'
                        : 'text-content-tertiary'
                  }`}
                >
                  {selectedOrder.side}
                </span>
              ) : null}
            </div>
            <h2 id="order-timing-title" className="text-base font-semibold text-content-primary">
              Commit → Oracle Publish → Finalize Timeline
            </h2>
            {pctBadge.context ? (
              <p className="text-xs text-content-secondary">{pctBadge.context}</p>
            ) : null}
          </div>

          <div className="flex flex-wrap items-center gap-2">
            {totalLatencySeconds != null ? (
              <span
                className="border border-brand-border/35 bg-app-bg px-2.5 py-1 font-mono text-xs font-semibold tabular-nums text-content-primary"
                title={
                  estimatedBlockMs
                    ? `On-chain block.timestamp has 1s resolution; ${estimatedBlockMs} estimated from +${String(totalLatencyBlocks)} Arbitrum blocks (~250ms/block)`
                    : undefined
                }
              >
                {String(totalLatencySeconds)}s
                {totalLatencyBlocks != null ? ` · +${String(totalLatencyBlocks)} blk` : ''}
                {estimatedBlockMs ? (
                  <span className="font-normal text-content-secondary">
                    {' '}
                    ({estimatedBlockMs} est.)
                  </span>
                ) : null}
              </span>
            ) : null}
            <StatusPill tone={pctBadge.tone} label={pctBadge.label} />
          </div>
        </div>

        {/* Batch order selector pills (only when multiple orders exist and caller did not lock activeOrderId) */}
        {finalizedOrders.length > 1 && !activeOrderId ? (
          <div className="flex flex-wrap items-center gap-1.5" role="group" aria-label="Select order timing">
            <span className="mr-1 font-mono text-[11px] text-content-tertiary">Inspect order:</span>
            {finalizedOrders.slice(0, 12).map((order) => {
              const active = order.orderId === selectedOrder.orderId
              return (
                <button
                  key={order.orderId}
                  type="button"
                  aria-pressed={active}
                  onClick={() => {
                    setManualOrderId(order.orderId)
                  }}
                  className={`cursor-pointer border px-2 py-0.5 font-mono text-[11px] font-semibold transition-colors ${
                    active
                      ? 'border-brand-orange bg-brand-orange text-content-primary'
                      : 'border-brand-border/35 bg-app-bg text-content-secondary hover:border-brand-peach/50 hover:text-content-primary'
                  }`}
                >
                  #{order.orderId}
                  {order.latencySeconds != null ? ` (${String(order.latencySeconds)}s)` : ''}
                </button>
              )
            })}
          </div>
        ) : null}

        {/* 3-Stage Connected Timeline */}
        <div className="grid gap-3 lg:grid-cols-[minmax(0,1fr)_auto_minmax(0,1fr)_auto_minmax(0,1fr)] lg:items-stretch">
          {/* Stage 1: Committed */}
          <div className="flex min-w-0 flex-col justify-between border border-brand-border/25 bg-app-bg/60 p-3.5">
            <div>
              <div className="flex items-center gap-2">
                <span className="inline-flex h-5 w-5 shrink-0 items-center justify-center border border-brand-peach/50 bg-brand-peach/15 font-mono text-[10px] font-bold text-brand-peach">
                  1
                </span>
                <p className="font-mono text-[11px] font-semibold uppercase tracking-[0.08em] text-content-secondary">
                  Order Committed
                </p>
              </div>
              <p className="mt-2 whitespace-nowrap font-mono text-[11px] font-semibold tabular-nums text-content-primary">
                {commitAt ? formatUtcWithSeconds(commitAt) : 'Not recorded'}
              </p>
            </div>
            <div className="mt-2.5 space-y-1 border-t border-brand-border/15 pt-2 font-mono text-[11px] text-content-tertiary">
              <div className="flex items-center justify-between gap-2">
                <span>Block</span>
                <span className="whitespace-nowrap tabular-nums text-content-secondary">
                  {formatBlock(commitBlock) ?? '—'}
                </span>
              </div>
              {commitTxHash ? (
                <div className="flex items-center justify-between gap-2">
                  <span className="whitespace-nowrap">Tx</span>
                  <div className="inline-flex items-center gap-1 whitespace-nowrap">
                    <span className="text-brand-peach" title={commitTxHash}>
                      {shortAddress(commitTxHash)}
                    </span>
                    <CopyButton value={commitTxHash} />
                    {showCommitArb ? (
                      <ArbiscanButton kind="tx" value={commitTxHash} chainId={chainId} />
                    ) : null}
                  </div>
                </div>
              ) : null}
            </div>
          </div>

          {/* Connector 1 → 2 */}
          <div className="flex flex-row items-center justify-center gap-2 lg:flex-col lg:px-1">
            <span className="hidden h-px w-4 bg-brand-border/40 lg:block" aria-hidden="true" />
            <div className="flex flex-col items-center text-center">
              <span
                className={`border px-2 py-0.5 font-mono text-xs font-semibold tabular-nums ${
                  oraclePublishedBeforeCommit
                    ? 'border-positive/40 bg-positive/10 text-positive'
                    : 'border-brand-border/40 bg-app-bg text-content-primary'
                }`}
              >
                {formatSignedDelta(commitToOracleSeconds)}
              </span>
              <span className="mt-1 whitespace-nowrap font-mono text-[10px] uppercase tracking-[0.06em] text-content-tertiary">
                {oraclePublishedBeforeCommit ? 'Pre-commit feed' : 'Commit → Oracle'}
              </span>
              {latencyBenchmark?.p50CommitToOracleSeconds != null ? (
                <span className="whitespace-nowrap font-mono text-[10px] tabular-nums text-content-tertiary">
                  p50 {formatSignedDelta(latencyBenchmark.p50CommitToOracleSeconds)}
                </span>
              ) : null}
            </div>
            <span className="hidden h-px w-4 bg-brand-border/40 lg:block" aria-hidden="true" />
          </div>

          {/* Stage 2: Oracle Published */}
          <div className="flex min-w-0 flex-col justify-between border border-brand-border/25 bg-app-bg/60 p-3.5">
            <div>
              <div className="flex items-center justify-between gap-2">
                <div className="flex min-w-0 items-center gap-2">
                  <span className="inline-flex h-5 w-5 shrink-0 items-center justify-center border border-brand-yellow/50 bg-brand-yellow/15 font-mono text-[10px] font-bold text-brand-yellow">
                    2
                  </span>
                  <p className="truncate font-mono text-[11px] font-semibold uppercase tracking-[0.08em] text-content-secondary">
                    Oracle Published
                  </p>
                </div>
                <span
                  className={`shrink-0 whitespace-nowrap border px-1.5 py-0.5 font-mono text-[10px] uppercase ${
                    oracleFrozen
                      ? 'border-brand-orange/40 bg-brand-orange/10 text-brand-orange'
                      : 'border-positive/40 bg-positive/10 text-positive'
                  }`}
                >
                  {oracleFrozen ? 'Frozen' : 'Live'}
                </span>
              </div>
              <p className="mt-2 whitespace-nowrap font-mono text-[11px] font-semibold tabular-nums text-content-primary">
                {oracleMax
                  ? oracleMin && oracleMin !== oracleMax
                    ? `${formatTimeSecondsUtc(oracleMin)} → ${formatTimeSecondsUtc(oracleMax)} UTC`
                    : formatUtcWithSeconds(oracleMax)
                  : 'Not recorded'}
              </p>
            </div>
            <div className="mt-2.5 space-y-1 border-t border-brand-border/15 pt-2 font-mono text-[11px] text-content-tertiary">
              <div className="flex items-center justify-between gap-2">
                <span className="whitespace-nowrap">6-feed spread</span>
                <span className="whitespace-nowrap tabular-nums text-content-secondary">
                  {oracleSpreadSeconds != null ? `${String(oracleSpreadSeconds)}s` : '—'}
                </span>
              </div>
              <div className="flex items-center justify-between gap-2">
                <span className="whitespace-nowrap">Oracle mark</span>
                <span className="whitespace-nowrap tabular-nums">
                  {oraclePrice ? (
                    <OraclePriceLink updateId={oracleUpdateId} className="font-semibold">
                      {formatPrice(oraclePrice)} plDXY →
                    </OraclePriceLink>
                  ) : (
                    <span className="text-content-secondary">—</span>
                  )}
                </span>
              </div>
            </div>
          </div>

          {/* Connector 2 → 3 */}
          <div className="flex flex-row items-center justify-center gap-2 lg:flex-col lg:px-1">
            <span className="hidden h-px w-4 bg-brand-border/40 lg:block" aria-hidden="true" />
            <div className="flex flex-col items-center text-center">
              <span className="border border-brand-border/40 bg-app-bg px-2 py-0.5 font-mono text-xs font-semibold tabular-nums text-content-primary">
                {formatSignedDelta(oracleToFinalizeSeconds)}
              </span>
              <span className="mt-1 whitespace-nowrap font-mono text-[10px] uppercase tracking-[0.06em] text-content-tertiary">
                Oracle → Finalize
              </span>
              {latencyBenchmark?.p50OracleToFinalizeSeconds != null ? (
                <span className="whitespace-nowrap font-mono text-[10px] tabular-nums text-content-tertiary">
                  p50 {formatSignedDelta(latencyBenchmark.p50OracleToFinalizeSeconds)}
                </span>
              ) : null}
            </div>
            <span className="hidden h-px w-4 bg-brand-border/40 lg:block" aria-hidden="true" />
          </div>

          {/* Stage 3: Finalized */}
          <div className="flex min-w-0 flex-col justify-between border border-brand-border/25 bg-app-bg/60 p-3.5">
            <div>
              <div className="flex items-center justify-between gap-2">
                <div className="flex min-w-0 items-center gap-2">
                  <span className="inline-flex h-5 w-5 shrink-0 items-center justify-center border border-positive/50 bg-positive/15 font-mono text-[10px] font-bold text-positive">
                    3
                  </span>
                  <p className="truncate font-mono text-[11px] font-semibold uppercase tracking-[0.08em] text-content-secondary">
                    Order Finalized
                  </p>
                </div>
                <span className="shrink-0 whitespace-nowrap font-mono text-[10px] uppercase text-content-tertiary">
                  {selectedOrder.terminalStatus}
                </span>
              </div>
              <p className="mt-2 whitespace-nowrap font-mono text-[11px] font-semibold tabular-nums text-content-primary">
                {finalizedAt ? formatUtcWithSeconds(finalizedAt) : 'Not recorded'}
              </p>
            </div>
            <div className="mt-2.5 space-y-1 border-t border-brand-border/15 pt-2 font-mono text-[11px] text-content-tertiary">
              <div className="flex items-center justify-between gap-2">
                <span>Block</span>
                <span className="whitespace-nowrap tabular-nums text-content-secondary">
                  {formatBlock(finalizedBlock) ?? '—'}
                  {totalLatencyBlocks != null ? ` (+${String(totalLatencyBlocks)})` : ''}
                </span>
              </div>
              {estimatedBlockMs ? (
                <div
                  className="flex items-center justify-between gap-2"
                  title="Estimated from Arbitrum's ~250ms block cadence (on-chain block.timestamp is 1s resolution)"
                >
                  <span className="whitespace-nowrap">Cadence est.</span>
                  <span className="whitespace-nowrap tabular-nums text-content-secondary">
                    {estimatedBlockMs} (~250ms/blk)
                  </span>
                </div>
              ) : null}
              <div className="flex items-center justify-between gap-2">
                <span className="whitespace-nowrap">
                  {executionWindowLimit != null ? 'Window' : 'Tx'}
                </span>
                <span className="whitespace-nowrap tabular-nums text-content-secondary">
                  {executionWindowLimit != null && totalLatencySeconds != null && windowUsedPct != null
                    ? `${String(totalLatencySeconds)}s / ${String(executionWindowLimit)}s (${windowUsedPct.toFixed(1)}%)`
                    : txHash
                      ? shortAddress(txHash)
                      : '—'}
                </span>
              </div>
            </div>
          </div>
        </div>

        {/* Phase Breakdown Bar */}
        {commitToOracleSharePct != null && oracleToFinalizeSharePct != null ? (
          <div className="space-y-1.5">
            <div className="flex flex-wrap items-center justify-between gap-2 font-mono text-[11px]">
              <div className="flex flex-wrap items-center gap-4">
                <span className="text-brand-yellow">
                  Commit → Oracle: {formatSignedDelta(commitToOracleSeconds)} (
                  {commitToOracleSharePct.toFixed(0)}%)
                </span>
                <span className="text-positive">
                  Oracle → Finalize: {formatSignedDelta(oracleToFinalizeSeconds)} (
                  {oracleToFinalizeSharePct.toFixed(0)}%)
                </span>
              </div>
              {oraclePublishedBeforeCommit ? (
                <span className="text-content-tertiary">
                  Pyth quote already published before commit block; latency is keeper inclusion
                </span>
              ) : null}
            </div>
            <div className="flex h-2 w-full overflow-hidden bg-app-bg">
              {commitToOracleSharePct > 0 ? (
                <div
                  className="bg-brand-yellow/80"
                  style={{ width: `${commitToOracleSharePct.toFixed(2)}%` }}
                />
              ) : null}
              {oracleToFinalizeSharePct > 0 ? (
                <div
                  className="bg-positive/80"
                  style={{ width: `${oracleToFinalizeSharePct.toFixed(2)}%` }}
                />
              ) : null}
            </div>
          </div>
        ) : null}

        {/* Protocol Latency Percentile Gauge */}
        {latencyBenchmark && totalLatencySeconds != null ? (
          <LatencyDistributionGauge
            orderLatency={totalLatencySeconds}
            percentile={percentile}
            benchmark={latencyBenchmark}
          />
        ) : null}
      </Panel>
    </section>
  )
}
