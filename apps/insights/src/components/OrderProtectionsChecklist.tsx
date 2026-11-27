import { useState } from 'react'
import type {
  AccountProtectionSnapshot,
  LinkedPositionProtection,
  OrderProtectionCheck,
  OrderProtectionsPayload,
  ProtectionCheckStatus,
} from '../api'
import { formatAssets, formatPrice } from '../utils/format'
import {
  formatCheckValue,
  isActivePositionProtectionStatus,
  normalizeAccountProtection,
  protectionLifecycleTone,
} from '../utils/orderProtections'
import { StatusPill, type StatusTone } from './StatusPill'
import { Panel } from './ui'

function CheckMark({ status }: { status: ProtectionCheckStatus | 'active' | 'triggered' }) {
  if (status === 'passed' || status === 'active') {
    return (
      <span
        aria-hidden="true"
        className="inline-flex h-4 w-4 shrink-0 items-center justify-center border border-positive/55 bg-positive/15 font-mono text-[10px] font-bold text-positive"
      >
        ✓
      </span>
    )
  }
  if (status === 'triggered') {
    return (
      <span
        aria-hidden="true"
        className="inline-flex h-4 w-4 shrink-0 items-center justify-center border border-brand-peach/55 bg-brand-peach/15 font-mono text-[10px] font-bold text-brand-peach"
      >
        ✓
      </span>
    )
  }
  if (status === 'failed') {
    return (
      <span
        aria-hidden="true"
        className="inline-flex h-4 w-4 shrink-0 items-center justify-center border border-brand-orange/60 bg-brand-orange/15 font-mono text-[10px] font-bold text-brand-orange"
      >
        ✕
      </span>
    )
  }
  return (
    <span
      aria-hidden="true"
      className="inline-flex h-4 w-4 shrink-0 items-center justify-center border border-dashed border-brand-border/40 font-mono text-[10px] text-content-tertiary"
    >
      ○
    </span>
  )
}

function limitComparator(check: OrderProtectionCheck, side?: string | null): string {
  if (check.key === 'min_post_balance' || check.key === 'min_post_equity') return '≥ '
  if (check.key === 'price_limit') {
    if (side === 'long') return '≤ '
    if (side === 'short') return '≥ '
    return ''
  }
  if (
    check.key === 'execution_window' ||
    check.key.startsWith('max_')
  ) {
    return '≤ '
  }
  return ''
}

function CheckRow({
  check,
  side,
}: {
  check: OrderProtectionCheck
  side?: string | null
}) {
  const limitLabel =
    check.status === 'not_set'
      ? 'Unbounded'
      : check.limitValue != null
        ? `${limitComparator(check, side)}${formatCheckValue(check.limitValue, check.unit)}`
        : '—'
  const actualLabel =
    check.actualValue != null ? formatCheckValue(check.actualValue, check.unit) : null

  return (
    <li className="flex items-start justify-between gap-3 border-b border-brand-border/15 py-2.5 last:border-0">
      <div className="flex min-w-0 items-start gap-2.5">
        <div className="mt-0.5">
          <CheckMark status={check.status} />
        </div>
        <div className="min-w-0">
          <p
            className={`text-xs font-semibold ${
              check.status === 'failed'
                ? 'text-brand-orange'
                : check.status === 'not_set'
                  ? 'text-content-tertiary'
                  : 'text-content-primary'
            }`}
          >
            {check.label}
          </p>
          <p className="mt-0.5 text-[11px] leading-4 text-content-tertiary">{check.detail}</p>
        </div>
      </div>
      <div className="shrink-0 text-right font-mono text-[11px] tabular-nums">
        {actualLabel && check.unit !== 'hash' ? (
          <div className="text-content-primary">{actualLabel}</div>
        ) : null}
        <div
          className={
            check.status === 'failed'
              ? 'text-brand-orange'
              : check.status === 'passed'
                ? 'text-positive/85'
                : 'text-content-tertiary'
          }
        >
          {limitLabel}
        </div>
      </div>
    </li>
  )
}

function legStatusForChecklist(
  hasPrice: boolean,
  protection: LinkedPositionProtection | null,
  legName: 'TakeProfit' | 'StopLoss',
): { mark: ProtectionCheckStatus | 'active' | 'triggered'; badge: string; tone: StatusTone } {
  if (!hasPrice || !protection) {
    return { mark: 'not_set', badge: 'Not set', tone: 'unknown' }
  }
  const lower = protection.status.toLowerCase()
  const wasTriggeredLeg = protection.triggeredLeg === legName
  if (wasTriggeredLeg && (lower === 'executed' || lower === 'triggered')) {
    return {
      mark: 'triggered',
      badge: lower === 'executed' ? 'Executed' : 'Triggered',
      tone: 'ready',
    }
  }
  if (lower === 'armed' || lower === 'pendingopen') {
    return { mark: 'active', badge: protection.status, tone: 'live' }
  }
  if (lower === 'latched') {
    return { mark: 'triggered', badge: 'Latched', tone: 'degraded' }
  }
  if (lower === 'failed' || lower === 'liquidated') {
    return { mark: 'failed', badge: protection.status, tone: 'blocked' }
  }
  return { mark: 'not_set', badge: protection.status, tone: 'unknown' }
}

/**
 * Compact checklist badges rendered directly on each Open row in /wallets/:address and /execution.
 */
export function OrderProtectionsInlineChecklist({
  protections,
}: {
  protections?: OrderProtectionsPayload | null
}) {
  if (!protections) {
    return <span className="font-mono text-xs text-content-tertiary">—</span>
  }

  const { summary, executionChecks, positionProtection } = protections
  const v3Failed = summary.v3FailedCount > 0
  const v3Passed = summary.v3PassedCount
  const v3Enforced = summary.v3EnforcedCount

  const tpPrice = positionProtection?.takeProfitTriggerPrice ?? null
  const slPrice = positionProtection?.stopLossTriggerPrice ?? null
  const posStatus = positionProtection?.status ?? null
  const posActiveOrExecuted =
    posStatus != null &&
    ['armed', 'pendingopen', 'triggered', 'executed', 'latched'].includes(posStatus.toLowerCase())

  const tooltip = [
    ...executionChecks
      .filter((c) => c.status === 'passed' || c.status === 'failed')
      .map((c) => `${c.status === 'passed' ? '✓' : '✕'} ${c.label}: ${c.detail}`),
    tpPrice ? `✓ TP ${formatPrice(tpPrice)} (${posStatus ?? 'Attached'})` : '○ Take Profit: Not set',
    slPrice ? `✓ SL ${formatPrice(slPrice)} (${posStatus ?? 'Attached'})` : '○ Stop Loss: Not set',
  ].join('\n')

  return (
    <div
      className="flex flex-wrap items-center gap-1"
      title={tooltip}
      aria-label="Open order protections checklist"
    >
      {v3Enforced > 0 ? (
        <span
          className={`inline-flex items-center gap-1 border px-1.5 py-0.5 font-mono text-[10px] font-semibold leading-none ${
            v3Failed
              ? 'border-brand-orange/50 bg-brand-orange/15 text-brand-orange'
              : 'border-positive/40 bg-positive/10 text-positive'
          }`}
        >
          <span aria-hidden="true">{v3Failed ? '✕' : '✓'}</span>
          <span>
            {v3Passed}/{v3Enforced} V3
          </span>
        </span>
      ) : null}

      <span
        className={`inline-flex items-center gap-1 border px-1.5 py-0.5 font-mono text-[10px] leading-none ${
          tpPrice
            ? posActiveOrExecuted
              ? 'border-positive/40 bg-positive/10 text-positive'
              : 'border-brand-border/35 bg-surface-panel text-content-secondary'
            : 'border-dashed border-brand-border/25 text-content-tertiary'
        }`}
      >
        <span aria-hidden="true">{tpPrice ? '✓' : '○'}</span>
        <span>TP{tpPrice ? ` ${formatPrice(tpPrice)}` : ''}</span>
      </span>

      <span
        className={`inline-flex items-center gap-1 border px-1.5 py-0.5 font-mono text-[10px] leading-none ${
          slPrice
            ? posActiveOrExecuted
              ? 'border-brand-peach/45 bg-brand-peach/10 text-brand-peach'
              : 'border-brand-border/35 bg-surface-panel text-content-secondary'
            : 'border-dashed border-brand-border/25 text-content-tertiary'
        }`}
      >
        <span aria-hidden="true">{slPrice ? '✓' : '○'}</span>
        <span>SL{slPrice ? ` ${formatPrice(slPrice)}` : ''}</span>
      </span>
    </div>
  )
}

/**
 * Full protocol-level order & position protections checklist shown on /tx/:txHash for Open orders.
 */
export function OrderProtectionsChecklistPanel({
  protections,
  side,
}: {
  protections: OrderProtectionsPayload
  side?: string | null
}) {
  const coreChecks = protections.executionChecks.filter((c) => c.category === 'core')
  const accountingChecks = protections.executionChecks.filter((c) => c.category !== 'core')
  const hasEnforcedAccounting = accountingChecks.some(
    (c) => c.status === 'passed' || c.status === 'failed',
  )
  const [showAllAccounting, setShowAllAccounting] = useState(hasEnforcedAccounting)

  const { summary, positionProtection } = protections
  const tpState = legStatusForChecklist(summary.hasTakeProfit, positionProtection, 'TakeProfit')
  const slState = legStatusForChecklist(summary.hasStopLoss, positionProtection, 'StopLoss')

  const v3BadgeTone: StatusTone =
    summary.v3FailedCount > 0 ? 'blocked' : summary.v3PassedCount > 0 ? 'live' : 'unknown'
  const v3BadgeLabel =
    summary.v3FailedCount > 0
      ? `${String(summary.v3FailedCount)} V3 guard failed`
      : `${String(summary.v3PassedCount)}/${String(summary.v3EnforcedCount)} V3 guards passed`

  const posBadgeLabel = positionProtection
    ? `${summary.hasTakeProfit && summary.hasStopLoss ? 'TP & SL' : summary.hasTakeProfit ? 'TP' : summary.hasStopLoss ? 'SL' : 'Protection'} · ${positionProtection.status}`
    : 'No TP/SL attached'

  const sourceLabel =
    protections.boundsSource === 'commit_receipt'
      ? 'Verified from commit tx receipt'
      : protections.boundsSource === 'indexed_event'
        ? 'Indexed V3 IntentRegistered'
        : 'Order outcome record'

  return (
    <Panel className="p-5 sm:p-6">
      <div className="flex flex-wrap items-start justify-between gap-3 border-b border-brand-border/20 pb-4">
        <div>
          <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
            Order &amp; Position Protections Checklist
          </p>
          <p className="mt-1 text-xs text-content-secondary">
            On-chain V3 execution guardrails committed in Order #{protections.orderId} and attached
            PositionProtectionBook TP/SL legs.
          </p>
        </div>
        <div className="flex flex-wrap items-center gap-2">
          <StatusPill tone={v3BadgeTone} label={v3BadgeLabel} />
          <StatusPill
            tone={positionProtection ? protectionLifecycleTone(positionProtection.status) : 'unknown'}
            label={posBadgeLabel}
          />
          <span className="border border-brand-border/25 bg-app-bg/60 px-2 py-0.5 font-mono text-[10px] text-content-tertiary">
            {sourceLabel}
          </span>
        </div>
      </div>

      <div className="mt-5 grid gap-6 lg:grid-cols-12">
        <div className="lg:col-span-7">
          <div className="mb-2 flex items-center justify-between gap-2">
            <h3 className="text-xs font-semibold uppercase tracking-wider text-content-tertiary">
              V3 Execution Guardrails
            </h3>
            {summary.v3UnboundedCount > 0 ? (
              <span className="font-mono text-[11px] text-content-tertiary">
                {summary.v3EnforcedCount} enforced · {summary.v3UnboundedCount} unbounded
              </span>
            ) : null}
          </div>

          {coreChecks.length > 0 ? (
            <ul className="border border-brand-border/20 bg-app-bg/45 px-3.5" aria-label="Core V3 execution protections">
              {coreChecks.map((check) => (
                <CheckRow key={check.key} check={check} side={side} />
              ))}
            </ul>
          ) : (
            <p className="border border-brand-border/20 bg-app-bg/45 px-3.5 py-3 text-xs text-content-tertiary">
              No V3 intent bounds were indexed for this order.
            </p>
          )}

          {accountingChecks.length > 0 ? (
            <div className="mt-3">
              <button
                type="button"
                aria-expanded={showAllAccounting}
                onClick={() => {
                  setShowAllAccounting((prev) => !prev)
                }}
                className="flex w-full cursor-pointer items-center justify-between border border-brand-border/20 bg-surface-panel/60 px-3.5 py-2 text-left text-xs font-semibold text-content-secondary transition-colors hover:border-brand-peach/40 hover:text-content-primary"
              >
                <span>
                  Strict accounting bounds ({accountingChecks.length}{' '}
                  {hasEnforcedAccounting ? 'checked' : 'unbounded on web ticket'})
                </span>
                <span className="font-mono text-[11px] text-brand-peach">
                  {showAllAccounting ? 'Hide −' : 'Inspect +'}
                </span>
              </button>
              {showAllAccounting ? (
                <ul
                  className="border-x border-b border-brand-border/20 bg-app-bg/35 px-3.5"
                  aria-label="Strict V3 accounting bounds"
                >
                  {accountingChecks.map((check) => (
                    <CheckRow key={check.key} check={check} side={side} />
                  ))}
                </ul>
              ) : null}
            </div>
          ) : null}
        </div>

        <div className="lg:col-span-5">
          <div className="mb-2 flex items-center justify-between gap-2">
            <h3 className="text-xs font-semibold uppercase tracking-wider text-content-tertiary">
              TP / SL Position Protections
            </h3>
            {positionProtection ? (
              <span className="font-mono text-[11px] text-content-tertiary">
                Protection #{positionProtection.protectionId}
              </span>
            ) : null}
          </div>

          <div className="border border-brand-border/20 bg-app-bg/45 px-3.5">
            <ul aria-label="Take Profit and Stop Loss checklist">
              <li className="flex items-start justify-between gap-3 border-b border-brand-border/15 py-3">
                <div className="flex items-start gap-2.5">
                  <div className="mt-0.5">
                    <CheckMark status={tpState.mark} />
                  </div>
                  <div>
                    <p
                      className={`text-xs font-semibold ${
                        summary.hasTakeProfit ? 'text-content-primary' : 'text-content-tertiary'
                      }`}
                    >
                      Take Profit (TP)
                    </p>
                    <p className="mt-0.5 text-[11px] text-content-tertiary">
                      {positionProtection?.takeProfitTriggerPrice
                        ? `Triggers close at ${formatPrice(positionProtection.takeProfitTriggerPrice)} plDXY`
                        : 'No take-profit trigger attached to this open'}
                    </p>
                  </div>
                </div>
                <div className="shrink-0 text-right">
                  <div className="font-mono text-xs font-semibold text-content-primary tabular-nums">
                    {positionProtection?.takeProfitTriggerPrice
                      ? formatPrice(positionProtection.takeProfitTriggerPrice)
                      : '—'}
                  </div>
                  <div className="mt-1">
                    <StatusPill tone={tpState.tone} label={tpState.badge} />
                  </div>
                </div>
              </li>

              <li className="flex items-start justify-between gap-3 py-3">
                <div className="flex items-start gap-2.5">
                  <div className="mt-0.5">
                    <CheckMark status={slState.mark} />
                  </div>
                  <div>
                    <p
                      className={`text-xs font-semibold ${
                        summary.hasStopLoss ? 'text-content-primary' : 'text-content-tertiary'
                      }`}
                    >
                      Stop Loss (SL)
                    </p>
                    <p className="mt-0.5 text-[11px] text-content-tertiary">
                      {positionProtection?.stopLossTriggerPrice
                        ? `Triggers close at ${formatPrice(positionProtection.stopLossTriggerPrice)} plDXY`
                        : 'No stop-loss trigger attached to this open'}
                    </p>
                  </div>
                </div>
                <div className="shrink-0 text-right">
                  <div className="font-mono text-xs font-semibold text-content-primary tabular-nums">
                    {positionProtection?.stopLossTriggerPrice
                      ? formatPrice(positionProtection.stopLossTriggerPrice)
                      : '—'}
                  </div>
                  <div className="mt-1">
                    <StatusPill tone={slState.tone} label={slState.badge} />
                  </div>
                </div>
              </li>
            </ul>

            {positionProtection ? (
              <dl className="grid grid-cols-2 gap-x-4 gap-y-2 border-t border-brand-border/20 py-3 text-[11px]">
                <div>
                  <dt className="text-content-tertiary">Triggered leg</dt>
                  <dd className="mt-0.5 font-mono text-content-primary">
                    {positionProtection.triggeredLeg ?? 'Not triggered'}
                  </dd>
                </div>
                <div>
                  <dt className="text-content-tertiary">Trigger bounty</dt>
                  <dd className="mt-0.5 font-mono text-content-primary tabular-nums">
                    {positionProtection.triggerBountyAssets
                      ? formatAssets(positionProtection.triggerBountyAssets)
                      : '—'}
                  </dd>
                </div>
                {positionProtection.triggerMarkPrice ? (
                  <div>
                    <dt className="text-content-tertiary">Trigger mark price</dt>
                    <dd className="mt-0.5 font-mono text-content-primary tabular-nums">
                      {formatPrice(positionProtection.triggerMarkPrice)}
                    </dd>
                  </div>
                ) : null}
                {positionProtection.linkedOrderId ? (
                  <div>
                    <dt className="text-content-tertiary">Linked close order</dt>
                    <dd className="mt-0.5 font-mono text-content-primary">
                      #{positionProtection.linkedOrderId}
                    </dd>
                  </div>
                ) : null}
                {positionProtection.armedBlock ? (
                  <div>
                    <dt className="text-content-tertiary">Armed block</dt>
                    <dd className="mt-0.5 font-mono text-content-primary tabular-nums">
                      {Number(positionProtection.armedBlock).toLocaleString('en-US')}
                    </dd>
                  </div>
                ) : null}
              </dl>
            ) : null}
          </div>
        </div>
      </div>
    </Panel>
  )
}

/**
 * Compact active TP/SL checklist strip shown inside the Open Perpetual Position card on /wallets/:address.
 */
export function ActivePositionProtectionStrip({
  protections,
}: {
  protections: AccountProtectionSnapshot[]
}) {
  const normalized = protections.map((item, idx) =>
    normalizeAccountProtection(item, String(idx + 1)),
  )
  const active = normalized.find((p) => isActivePositionProtectionStatus(p.status)) ?? null

  return (
    <div
      className="border-t border-brand-border/15 bg-app-bg/40 px-5 py-3"
      aria-label="Active position TP/SL protections"
    >
      <div className="flex flex-wrap items-center justify-between gap-2">
        <span className="text-[11px] font-semibold uppercase tracking-wider text-content-tertiary">
          Position TP / SL Checklist
        </span>
        {active ? (
          <StatusPill
            tone={protectionLifecycleTone(active.status)}
            label={`#${active.protectionId} · ${active.status}`}
          />
        ) : (
          <span className="font-mono text-[11px] text-content-tertiary">No active TP/SL armed</span>
        )}
      </div>
      <div className="mt-2 flex flex-wrap gap-3 text-xs">
        <div className="inline-flex items-center gap-1.5">
          <CheckMark status={active?.takeProfitTriggerPrice ? 'active' : 'not_set'} />
          <span className="text-content-secondary">Take Profit:</span>
          <span className="font-mono font-semibold text-content-primary tabular-nums">
            {active?.takeProfitTriggerPrice ? formatPrice(active.takeProfitTriggerPrice) : 'Not set'}
          </span>
        </div>
        <div className="inline-flex items-center gap-1.5">
          <CheckMark status={active?.stopLossTriggerPrice ? 'active' : 'not_set'} />
          <span className="text-content-secondary">Stop Loss:</span>
          <span className="font-mono font-semibold text-content-primary tabular-nums">
            {active?.stopLossTriggerPrice ? formatPrice(active.stopLossTriggerPrice) : 'Not set'}
          </span>
        </div>
      </div>
    </div>
  )
}
