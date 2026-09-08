import { useEffect, useMemo, useSyncExternalStore } from 'react'
import type { PreparedPerpsOrderV2 } from '../contracts/perpsOrderV2'
import { captureAnalyticsEvent } from '../analytics/client'

export const PREPARATION_IDLE_MS = 500
export const PREPARATION_REUSE_MS = 10_000
export const PREPARATION_TIMEOUT_MS = 30_000
export const REVIEW_REFRESH_SECONDS = 10

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
  prepare: (input: T) => Promise<PreparedPerpsOrderV2>
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
}
const initialState: PreparationState = {
  key: undefined, identityKey: undefined, contextKey: undefined, result: undefined, previous: undefined, error: undefined,
  status: 'idle', startedAt: 0, visible: true, slow: false, refreshing: false,
}

// A small external store lets an already prepared result be used on the first
// modal render, while effects schedule RPC work and never run it during render.
class PreparationController<T> {
  state = { ...initialState, visible: typeof document === 'undefined' || document.visibilityState !== 'hidden' }
  listeners = new Set<() => void>()
  options?: Options<T>
  generation = 0
  timer?: ReturnType<typeof setTimeout>
  slowTimer?: ReturnType<typeof setTimeout>
  jobs = new Set<ReturnType<typeof setTimeout>>()
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
  invalidate() { this.generation++; this.clearTimers() }
  reusable() {
    return Date.now() - this.state.startedAt < PREPARATION_REUSE_MS &&
      (this.state.status === 'pending' || (this.state.status === 'ready' && this.hasDeadline()))
  }
  hasDeadline() {
    return !!this.state.result && Number(this.state.result.protection.validUntil) * 1000 - Date.now() > REVIEW_REFRESH_SECONDS * 1000
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
    if (opening) this.openedAt = Date.now()
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
      Number(this.state.result.protection.validUntil) * 1000 - Date.now() - REVIEW_REFRESH_SECONDS * 1000))
  }
  trackReady() {
    if (!this.state.visible || this.options?.mode !== 'review' || this.options.reviewValid === false || this.openedAt === undefined) return
    captureAnalyticsEvent('perps review ready', {
      surface: 'perps', duration_ms: Date.now() - this.openedAt, reason_code: this.admission ?? 'cold',
    })
    this.openedAt = undefined
  }
  start(source: Source) {
    const options = this.options
    if (!options?.candidate || options.mode === 'inactive' || !this.state.visible || this.disposed) return
    if (source === 'background' && (options.mode !== 'background' || this.backgroundGeneration !== undefined)) return
    this.invalidate()
    const candidate = options.candidate
    const generation = this.generation
    const startedAt = Date.now()
    const previous = source === 'refresh' ? this.state.result : undefined
    if (source === 'background') this.backgroundGeneration = generation
    if (source === 'refresh') { this.openedAt = startedAt; this.admission = 'refresh' }
    this.publish({ key: options.candidate.key, identityKey: options.identityKey, contextKey: options.contextKey, status: 'pending', startedAt,
      result: previous, previous, error: undefined, slow: false, refreshing: source === 'refresh' })
    const current = () => !this.disposed && generation === this.generation
    let finished = false
    const finish = (result?: PreparedPerpsOrderV2, error?: unknown) => {
      if (finished) return
      finished = true
      clearTimeout(jobTimeout)
      this.jobs.delete(jobTimeout)
      if (current() && result && Number(result.protection.validUntil) * 1000 - Date.now() <= REVIEW_REFRESH_SECONDS * 1000) {
        error = new Error('This review has expired or is about to expire. Retry review for fresh order terms.')
        result = undefined
      }
      if (!this.disposed) captureAnalyticsEvent('perps order preparation finished', {
        surface: 'perps', duration_ms: Date.now() - startedAt, reason_code: source,
        error_category: !current() ? 'cancelled' : error ? 'preparation_failed' : 'none',
      })
      if (current()) {
        this.clearTimers()
        this.publish({ status: error ? 'error' : 'ready', result: result ?? previous, error, slow: false })
        if (!error) { this.trackReady(); this.scheduleRefresh() }
      }
    }
    const releaseBackground = () => {
      if (source !== 'background' || this.backgroundGeneration !== generation) return
      this.backgroundGeneration = undefined
      if (!this.disposed && this.options?.mode === 'background' && this.state.status === 'idle') this.scheduleBackground()
    }
    this.slowTimer = setTimeout(() => { if (current()) this.publish({ slow: true }) }, 3000)
    const jobTimeout = setTimeout(() => { finish(undefined, new Error('Order checks took too long. Retry review.')) }, PREPARATION_TIMEOUT_MS)
    this.jobs.add(jobTimeout)
    try {
      void options.prepare(candidate.input).then(
        result => { finish(result) }, (error: unknown) => { finish(undefined, error) }).finally(releaseBackground)
    } catch (error) { finish(undefined, error); releaseBackground() }
  }
  retry = () => { this.admission = 'cold'; this.openedAt = Date.now(); this.start(this.state.result ? 'refresh' : 'cold') }
  setVisible = (visible: boolean) => {
    if (visible === this.state.visible) return
    this.publish({ visible })
    clearTimeout(this.timer)
    if (!visible) return
    if (this.options?.mode === 'review') {
      if (this.options.contextKey !== this.state.contextKey) this.start(this.state.result ? 'refresh' : 'cold')
      else if ((this.state.status === 'ready' || this.state.status === 'pending') && !this.reusable()) this.start(this.state.result ? 'refresh' : 'cold')
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
