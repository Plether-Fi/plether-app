import { deadlineNow } from '../perps-aa/deadlineClock'
import { useEffect, useMemo, useSyncExternalStore } from 'react'
import type { PreparedPerpsOrderV2 } from '../contracts/perpsOrderV2'
import { captureAnalyticsEvent } from '../analytics/client'
import { getPreparationFailureProperties, preparationFailure } from '../utils/perpsPreparationDiagnostics'
import { isPerpsOracleSyncError } from '../utils/perpsErrors'

export const PREPARATION_IDLE_MS = 500
export const PREPARATION_REUSE_MS = 10_000
export const PREPARATION_TIMEOUT_MS = 30_000
export const REVIEW_REFRESH_SECONDS = 45
export const ORACLE_RECOVERY_RETRY_MS = 2_000
export const ORACLE_RECOVERY_UNAVAILABLE = 'Market prices are temporarily unavailable. Your order details are saved.'

/** Value keys preserve bigint and Infinity; never send these keys to analytics. */
export function orderPreparationKey(value: unknown): string {
  return JSON.stringify(value, (_, item: unknown) => {
    if (typeof item === 'bigint') return { bigint: item.toString() }
    if (typeof item === 'number' && !Number.isFinite(item)) return { number: String(item) }
    if (item !== null && typeof item === 'object' && !Array.isArray(item)) {
      return Object.fromEntries(Object.entries(item).sort(([a], [b]) => a.localeCompare(b)))
    }
    return item
  })
}

type Mode = 'inactive' | 'background' | 'review'
type Source = 'background' | 'cold' | 'refresh'
interface Candidate<T> { key: string; input: T }
interface Options<T> {
  candidate?: Candidate<T>
  identityKey: string
  contextKey?: string
  reviewValid?: boolean
  mode: Mode
  prepare: (input: T, signal?: AbortSignal) => Promise<PreparedPerpsOrderV2>
}
interface PreparationState {
  key?: string
  identityKey?: string
  contextKey?: string
  status: 'idle' | 'pending' | 'ready' | 'error'
  result?: PreparedPerpsOrderV2
  previous?: PreparedPerpsOrderV2
  error?: unknown
  startedAt: number
  visible: boolean
  slow: boolean
  refreshing: boolean
  recoveringOracle: boolean
}
const initialState: PreparationState = {
  key: undefined, identityKey: undefined, contextKey: undefined, result: undefined, previous: undefined, error: undefined,
  status: 'idle', startedAt: 0, visible: true, slow: false, refreshing: false, recoveringOracle: false,
}

// A small external store lets an already prepared result be used on the first
// modal render, while effects schedule RPC work and never run it during render.
class PreparationController<T> {
  state = { ...initialState, visible: typeof document === 'undefined' || document.visibilityState !== 'hidden' }
  listeners = new Set<() => void>()
  options?: Options<T>
  abortController?: AbortController
  generation = 0
  timer?: ReturnType<typeof setTimeout>
  slowTimer?: ReturnType<typeof setTimeout>
  jobs = new Set<ReturnType<typeof setTimeout>>()
  oracleRecoveryExhausted = false
  oracleRecovery?: { startedAt: number; deadline: number; error: unknown }
  resumeRecovery?: () => void
  backgroundGeneration?: number
  disposed = false
  openedAt?: number
  admission?: 'completed_reuse' | 'pending_reuse' | 'cold' | 'refresh'
  subscribe = (listener: () => void) => { this.listeners.add(listener); return () => { this.listeners.delete(listener) } }
  snapshot = () => this.state
  publish(update: Partial<PreparationState>) {
    this.state = { ...this.state, ...update }
    this.listeners.forEach(listener => { listener(); })
  }
  clearTimers() {
    clearTimeout(this.timer); clearTimeout(this.slowTimer)
  }
  invalidate() { this.oracleRecovery = undefined; this.resumeRecovery = undefined; this.generation++; this.abortController?.abort(); this.clearTimers() }
  reusable() {
    return performance.now() - this.state.startedAt < PREPARATION_REUSE_MS &&
      (this.state.status === 'pending' || (this.state.status === 'ready' && this.hasDeadline()))
  }
  hasDeadline() {
    try { return !!this.state.result && Number(this.state.result.protection.validUntil) * 1000 - deadlineNow() > REVIEW_REFRESH_SECONDS * 1000 } catch { return false }
  }
  configure(options: Options<T>) {
    this.disposed = false
    const old = this.options
    this.options = options
    const changed = old?.candidate?.key !== options.candidate?.key || old?.identityKey !== options.identityKey ||
      (old.mode === 'review' && options.mode !== 'review')
    const contextChanged = old?.contextKey !== options.contextKey
    if (!changed && old.mode === options.mode && !contextChanged) {
      if (this.state.status === 'ready') this.trackReady()
      return
    }
    if (!changed && contextChanged && options.mode === 'review') {
      if (this.oracleRecoveryExhausted) { this.publish({ contextKey: options.contextKey }); return }
      this.start('refresh')
      return
    }
    if (options.mode === 'inactive' || !options.candidate) {
      this.invalidate()
      this.openedAt = undefined
      this.publish({ ...initialState, visible: this.state.visible })
      return
    }
    if (changed || contextChanged) {
      this.invalidate()
      this.publish({ ...initialState, visible: this.state.visible, key: options.candidate.key, identityKey: options.identityKey, contextKey: options.contextKey })
    }
    const opening = options.mode === 'review' && old?.mode !== 'review'
    if (opening) this.openedAt = performance.now()
    if (options.mode === 'review') {
      if (!this.state.visible) return
      if (changed || contextChanged || opening) {
        if (!changed && !contextChanged && this.reusable()) {
          this.admission = this.state.status === 'ready' ? 'completed_reuse' : 'pending_reuse'
          if (this.state.status === 'ready') { this.trackReady(); this.scheduleRefresh() }
        } else {
          this.admission = 'cold'
          this.start('cold')
        }
      }
    } else {
      this.openedAt = undefined
      if (changed || contextChanged || old.mode !== 'background') this.scheduleBackground()
    }
  }
  scheduleBackground() {
    clearTimeout(this.timer)
    if (!this.state.visible || this.backgroundGeneration !== undefined) return
    this.timer = setTimeout(() => { this.start('background'); }, PREPARATION_IDLE_MS)
  }
  scheduleRefresh() {
    clearTimeout(this.timer)
    if (!this.state.visible || this.options?.mode !== 'review' || !this.state.result || this.state.status !== 'ready') return
    this.timer = setTimeout(() => { this.start('refresh'); }, Math.max(0,
      Number(this.state.result.protection.validUntil) * 1000 - deadlineNow() - REVIEW_REFRESH_SECONDS * 1000))
  }
  trackReady() {
    if (!this.state.visible || this.options?.mode !== 'review' || this.options.reviewValid === false || this.openedAt === undefined) return
    captureAnalyticsEvent('perps review ready', {
      surface: 'perps', duration_ms: performance.now() - this.openedAt, reason_code: this.admission ?? 'cold',
    })
    this.openedAt = undefined
  }
  start(source: Source) {
    const options = this.options
    if (!options?.candidate || options.mode === 'inactive' || !this.state.visible || this.disposed) return
    if (source === 'background' && (options.mode !== 'background' || this.backgroundGeneration !== undefined)) return
    const continuingRecovery = source === 'refresh' && this.state.recoveringOracle &&
      this.state.key === options.candidate.key && this.state.identityKey === options.identityKey
      ? this.oracleRecovery : undefined
    this.invalidate()
    this.oracleRecovery = continuingRecovery
    this.oracleRecoveryExhausted = false
    const abortController = new AbortController()
    this.abortController = abortController
    const candidate = options.candidate
    const generation = this.generation
    const startedAt = continuingRecovery ? continuingRecovery.deadline - PREPARATION_TIMEOUT_MS : performance.now()
    const previous = source === 'refresh' ? this.state.result : undefined
    if (source === 'background') this.backgroundGeneration = generation
    if (source === 'refresh') { this.openedAt = startedAt; this.admission = 'refresh' }
    this.publish({ key: options.candidate.key, identityKey: options.identityKey, contextKey: options.contextKey, status: 'pending', startedAt,
      result: previous, previous, error: undefined, slow: false, recoveringOracle: continuingRecovery !== undefined, refreshing: source === 'refresh' })
    const current = () => !this.disposed && generation === this.generation
    let finished = false
    let recoveryStartedAt = continuingRecovery?.startedAt
    let lastRecoveryError = continuingRecovery?.error
    let inFlight = false
    let retryAt = 0
    const trackRecovery = (reason: 'started' | 'succeeded' | 'exhausted' | 'failed') => {
      captureAnalyticsEvent('perps oracle recovery', {
        surface: 'perps', reason_code: reason, duration_ms: performance.now() - (recoveryStartedAt ?? performance.now()),
      })
    }
    const finish = (result?: PreparedPerpsOrderV2, error?: unknown) => {
      if (finished) return
      finished = true
      releaseBackground()
      clearTimeout(jobTimeout)
      this.jobs.delete(jobTimeout)
      if (current() && result) {
        try {
          if (Number(result.protection.validUntil) * 1000 - deadlineNow() <= REVIEW_REFRESH_SECONDS * 1000) {
            throw new Error('This review has expired or is about to expire. Retry review for fresh order terms.')
          }
        } catch (cause) {
          error = preparationFailure(cause, 'review_freshness')
          result = undefined
        }
      }
      if (!this.disposed) captureAnalyticsEvent('perps order preparation finished', {
        surface: 'perps', duration_ms: performance.now() - startedAt, reason_code: source,
        error_category: !current() ? 'cancelled' : error ? 'preparation_failed' : 'none',
        ...(current() && error ? getPreparationFailureProperties(error) : {}),
      })
      if (current()) {
        this.clearTimers()
        this.resumeRecovery = undefined
        this.oracleRecovery = undefined
        this.oracleRecoveryExhausted = error instanceof Error && error.message === ORACLE_RECOVERY_UNAVAILABLE
        if (recoveryStartedAt !== undefined) trackRecovery(error
          ? error instanceof Error && error.message === ORACLE_RECOVERY_UNAVAILABLE ? 'exhausted' : 'failed'
          : 'succeeded')
        this.publish({ status: error ? 'error' : 'ready', result: result ?? previous, error, slow: false, recoveringOracle: false })
        if (!error) { this.trackReady(); this.scheduleRefresh() }
      }
    }
    const releaseBackground = () => {
      if (source !== 'background' || this.backgroundGeneration !== generation) return
      this.backgroundGeneration = undefined
      if (!this.disposed && this.options?.mode === 'background' && this.state.status === 'idle') this.scheduleBackground()
    }
    this.slowTimer = setTimeout(() => { if (current()) this.publish({ slow: true }) }, 3000)
    const jobTimeout = setTimeout(() => {
      abortController.abort()
      finish(undefined, recoveryStartedAt === undefined
        ? preparationFailure(new Error('Order checks took too long. Retry review.'), 'preparation_timeout')
        : new Error(ORACLE_RECOVERY_UNAVAILABLE, { cause: lastRecoveryError }))
    }, Math.max(0, startedAt + PREPARATION_TIMEOUT_MS - performance.now()))
    this.jobs.add(jobTimeout)
    const scheduleRecovery = () => {
      if (!current() || finished || inFlight || !this.state.visible || this.options?.mode !== 'review') return
      clearTimeout(this.timer)
      this.timer = setTimeout(() => { runAttempt() }, Math.max(0, retryAt - performance.now()))
    }
    const runAttempt = () => {
      if (!current() || finished || inFlight || !this.state.visible) return
      inFlight = true
      const failed = (error: unknown) => {
        inFlight = false
        if (current() && !finished && this.options?.mode === 'review' && isPerpsOracleSyncError(error)) {
          lastRecoveryError = error
          if (recoveryStartedAt === undefined) { recoveryStartedAt = performance.now(); trackRecovery('started') }
          this.oracleRecovery = { startedAt: recoveryStartedAt, deadline: startedAt + PREPARATION_TIMEOUT_MS, error }
          this.publish({ recoveringOracle: true })
          retryAt = performance.now() + ORACLE_RECOVERY_RETRY_MS
          this.resumeRecovery = scheduleRecovery
          scheduleRecovery()
        } else finish(undefined, error)
      }
      try {
        void options.prepare(candidate.input, abortController.signal).then(
          result => { inFlight = false; finish(result) }, failed)
      } catch (error) { failed(error) }
    }
    runAttempt()
  }

  retry = () => { this.admission = 'cold'; this.openedAt = performance.now(); this.start(this.state.result ? 'refresh' : 'cold') }
  setVisible = (visible: boolean) => {
    if (visible === this.state.visible) return
    this.publish({ visible })
    clearTimeout(this.timer)
    if (!visible) return
    if (this.options?.mode === 'review') {
      if (this.options.contextKey !== this.state.contextKey) this.start(this.state.result ? 'refresh' : 'cold')
      else if (this.state.recoveringOracle) { this.resumeRecovery?.(); return }
      // Monotonic timers may pause during device sleep. A visible review gets
      // fresh server/chain context even if its local elapsed time looks short.
      else if (this.state.status === 'ready' || (this.state.status === 'pending' && !this.reusable())) this.start(this.state.result ? 'refresh' : 'cold')
      else if (this.state.status === 'idle') this.start('cold')
      else this.scheduleRefresh()
    } else if (this.options?.mode === 'background' && this.state.status === 'idle') this.scheduleBackground()
  }
  dispose = () => { this.disposed = true; this.invalidate(); this.jobs.forEach(clearTimeout); this.jobs.clear(); this.options = undefined; this.state = initialState; this.backgroundGeneration = undefined }
}

export function usePerpsOrderPreparation<T>(options: Options<T>) {
  const controller = useMemo(() => new PreparationController<T>(), [])
  const state = useSyncExternalStore(controller.subscribe, controller.snapshot, controller.snapshot)
  useEffect(() => { controller.configure(options) }, [controller, options])
  useEffect(() => {
    const onVisibility = () => { controller.setVisible(document.visibilityState !== 'hidden'); }
    onVisibility()
    document.addEventListener('visibilitychange', onVisibility)
    return () => { document.removeEventListener('visibilitychange', onVisibility); controller.dispose() }
  }, [controller])
  const matches = options.candidate?.key === state.key && options.identityKey === state.identityKey && options.contextKey === state.contextKey
  const ready = options.mode !== 'inactive' && options.reviewValid !== false && matches && state.status === 'ready' && state.visible &&
    document.visibilityState !== 'hidden' && controller.hasDeadline() &&
    (options.mode === 'review' && controller.options?.mode === 'review' || controller.reusable())
  return { ...state, ready, matches, retry: controller.retry }
}
