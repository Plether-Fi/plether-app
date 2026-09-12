import { useEffect, useSyncExternalStore } from 'react'
import { reportReadiness } from '../analytics/readiness'

export type ReadinessStatus = 'ready' | 'blocked' | 'unknown'
export type ReadinessAction = 'deposit' | 'open' | 'close' | 'protection'
export interface ReadinessCheck { component: string; status: ReadinessStatus; reason: string }
export interface ReadinessSnapshot {
  version: 1
  observedAt: number
  expiresAt: number
  enforcementEnabled: boolean
  actions: Record<ReadinessAction, ReadinessCheck[]>
  /** Background funding observations; never universal action blockers. */
  workers?: ReadinessCheck[]
}
const actions = ['deposit', 'open', 'close', 'protection'] as const
const statuses = ['ready', 'blocked', 'unknown']
export function parseReadiness(value: unknown, now = Date.now()): ReadinessSnapshot {
  if (!value || typeof value !== 'object') throw new Error('Invalid readiness')
  const v = value as Record<string, unknown>
  if (v.version !== 1 || typeof v.enforcementEnabled !== 'boolean'
    || typeof v.observedAt !== 'number' || typeof v.expiresAt !== 'number'
    || !Number.isSafeInteger(v.observedAt) || !Number.isSafeInteger(v.expiresAt)
    || v.expiresAt <= v.observedAt || v.expiresAt - v.observedAt > 15_000
    || v.observedAt > now + 5_000 || !v.actions || typeof v.actions !== 'object') throw new Error('Invalid readiness')
  const parsed = {} as ReadinessSnapshot['actions']
  for (const action of actions) {
    const checks: unknown = (v.actions as Record<string, unknown>)[action]
    if (!Array.isArray(checks) || checks.length === 0 || checks.length > 12) throw new Error('Invalid readiness')
    parsed[action] = checks.map((value: unknown) => {
      if (!value || typeof value !== 'object') throw new Error('Invalid readiness')
      const check = value as Record<string, unknown>
      if (typeof check.status !== 'string' || !statuses.includes(check.status)
        || typeof check.component !== 'string' || !/^[a-z_]{1,40}$/.test(check.component)
        || typeof check.reason !== 'string' || !/^[A-Z_]{1,64}$/.test(check.reason)) throw new Error('Invalid readiness')
      return { component: check.component, status: check.status as ReadinessStatus, reason: check.reason }
    })
  }
  let workers: ReadinessCheck[] | undefined
  if (v.workers !== undefined) {
    if (!Array.isArray(v.workers) || v.workers.length > 6) throw new Error('Invalid funding readiness')
    const allowed = ['alto', 'keeper', 'oracle', 'liquidation', 'protection', 'lp_settlement']
    workers = v.workers.map((w: unknown) => {
      if (!w || typeof w !== 'object') throw new Error('Invalid funding readiness')
      const c = w as Record<string, unknown>
      if (typeof c.component !== 'string' || !allowed.includes(c.component)
        || typeof c.status !== 'string' || !statuses.includes(c.status)
        || typeof c.reason !== 'string' || !['READY','FUNDING_LOW','FUNDING_UNVERIFIED','WORKER_INSUFFICIENT_FUNDS'].includes(c.reason)) throw new Error('Invalid funding readiness')
      return { component: c.component, status: c.status as ReadinessStatus, reason: c.reason }
    })
    if (new Set(workers.map(w => w.component)).size !== workers.length) throw new Error('Duplicate funding readiness')
  }
  return { version: 1, observedAt: v.observedAt, expiresAt: v.expiresAt, enforcementEnabled: v.enforcementEnabled, actions: parsed, workers }
}
export function readinessChecks(snapshot: ReadinessSnapshot | undefined, action: ReadinessAction, now = Date.now()): ReadinessCheck[] {
  return !snapshot || snapshot.expiresAt <= now || snapshot.observedAt > now + 5_000
    ? [{ component: 'readiness', status: 'unknown', reason: 'READINESS_UNAVAILABLE' }]
    : snapshot.actions[action]
}
export function readinessBlocker(snapshot: ReadinessSnapshot | undefined, action: ReadinessAction, now = Date.now()): ReadinessCheck | undefined {
  return snapshot?.enforcementEnabled ? readinessChecks(snapshot, action, now).find(c => c.status === 'blocked') : undefined
}
export function readinessWorkers(snapshot: ReadinessSnapshot | undefined, now = Date.now()): ReadinessCheck[] {
  return !snapshot || snapshot.expiresAt <= now || snapshot.observedAt > now+5_000 ? [] : snapshot.workers ?? []
}

let snapshot: ReadinessSnapshot | undefined
let inflight: Promise<void> | undefined
let lastStarted = 0
let revision = 0
const listeners = new Set<() => void>()
let timer: ReturnType<typeof setInterval> | undefined
function publish() { revision++; listeners.forEach(fn => { fn() }) }
export function currentReadiness() { return snapshot }
export function refreshReadiness(force = false): Promise<void> {
  if (inflight) return inflight
  if (!force && Date.now() - lastStarted < 10_000) return Promise.resolve()
  lastStarted = Date.now()
  inflight = (async () => {
    try {
      const response = await fetch('/api/perps/v1/readiness', { credentials: 'same-origin', cache: 'no-store', signal: AbortSignal.timeout(4_000) })
      if (!response.ok) throw new Error('Readiness unavailable')
      snapshot = parseReadiness(await response.json())
      reportReadiness([...actions.flatMap(action => readinessChecks(snapshot, action)), ...(snapshot.workers ?? [])])
    } catch {
      // A stale response must not remain a hard blocker. Authorization is independent.
      snapshot = undefined
      reportReadiness([{ component: 'readiness', status: 'unknown', reason: 'READINESS_UNAVAILABLE' }])
    } finally { inflight = undefined; publish() }
  })()
  return inflight
}
function tick() { publish(); if (document.visibilityState !== 'hidden') void refreshReadiness() }
function subscribe(listener: () => void) {
  listeners.add(listener)
  if (!timer) {
    timer = setInterval(tick, 1_000)
    document.addEventListener('visibilitychange', tick)
  }
  return () => {
    listeners.delete(listener)
    if (!listeners.size && timer) { clearInterval(timer); timer = undefined; document.removeEventListener('visibilitychange', tick) }
  }
}
const noSubscribe = () => () => { /* Disabled consumers do not own polling. */ }
export function useTradingReadiness(enabled: boolean) {
  useSyncExternalStore(enabled ? subscribe : noSubscribe, () => revision, () => 0)
  useEffect(() => { if (enabled) void refreshReadiness() }, [enabled])
  return snapshot
}

const messages: Record<string, string> = {
  SPONSORSHIP_DISABLED: 'Gas sponsorship is temporarily disabled.',
  PAYMASTER_PAUSED: 'Gas sponsorship is paused.',
  RECONCILIATION_STALE: 'Sponsorship accounting is catching up.',
  BUNDLER_UNAVAILABLE: 'Transaction submission could not be verified.',
  KEEPER_INSUFFICIENT_FUNDS: 'The trade execution worker needs funding.',
  WORKER_INSUFFICIENT_FUNDS: 'This worker has no execution funds available.',
  WORKER_HEARTBEAT_STALE: 'The execution worker has not reported a recent status.',
  ORACLE_UNAVAILABLE: 'Execution price availability could not be verified.',
  OPEN_EXECUTION_UNAVAILABLE: 'The current execution mode does not allow opening positions. Closing is checked separately.',
  EXIT_MODE_REQUIRES_VALIDATION: 'Close execution depends on the current exit policy and price payload; availability is not yet verified.',
  PROTECTION_TRIGGER_UNAVAILABLE: 'New protection triggers are unavailable while the oracle is frozen. Voluntary closes, cancellation and previously triggered retries follow separate rules.',
  FUNDING_LOW: 'An execution worker has fewer than ten estimated executions in reserve.',
  FUNDING_UNVERIFIED: 'Execution funding could not be verified.',
  KEEPER_RPC_TIMEOUT: 'An execution attempt timed out while communicating with the chain.',
  KEEPER_EXECUTION_FAILED: 'The execution worker recorded an unsuccessful attempt. A more specific cause is unavailable.',
  READINESS_UNAVAILABLE: 'Trading status is unavailable. Existing transaction security checks still apply.',
}
export function readinessMessage(reason: string) { return messages[reason] ?? 'A trading dependency could not be verified.' }
