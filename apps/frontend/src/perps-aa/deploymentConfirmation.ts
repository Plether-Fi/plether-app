import { asSponsorRequestError, SponsorRequestError, sponsorReasonMessage } from './errors'

const CHECK_INTERVAL_MS = 15_000
export type DeploymentConfirmationStatus = 'idle' | 'waiting' | 'check-unavailable' | 'ready'
export interface DeploymentConfirmationDetails {
  status: DeploymentConfirmationStatus
  waitingSince?: number
  lastCheckedAt?: number
  lastSuccessfulCheckAt?: number
}
export interface DeploymentConfirmationMonitor {
  getSnapshot: () => DeploymentConfirmationStatus
  notePending?: (waitingSince?: number) => void
  getDetails?: () => DeploymentConfirmationDetails
  subscribe: (listener: () => void) => () => void
  start: () => () => void
}
export function confirmationPending(status: DeploymentConfirmationStatus) {
  return status === 'waiting' || status === 'check-unavailable'
}
interface Persistence {
  scope: string
  legacyScope?: string
  storage?: () => Pick<Storage, 'getItem' | 'setItem' | 'removeItem'>
}

/** Advisory only. Every manual retry still passes backend verification. */
export function createDeploymentConfirmationGate(
  hasSafeCode: () => Promise<boolean>,
  now: () => number = () => performance.now(),
  persistence?: Persistence,
) {
  const key = persistence ? `plether:deployment-confirmation:v1:${persistence.scope.toLowerCase()}` : undefined
  const storage = () => persistence?.storage ? persistence.storage() : window.localStorage
  let details: DeploymentConfirmationDetails = { status: 'idle' }
  try {
    const legacyKey = persistence?.legacyScope ? `plether:deployment-confirmation:v1:${persistence.legacyScope.toLowerCase()}` : undefined
    const saved = key && (storage().getItem(key) ?? (legacyKey ? storage().getItem(legacyKey) : null))
    if (saved === 'waiting') details = { status: 'waiting', waitingSince: Date.now() }
    else if (saved) {
      const parsed = JSON.parse(saved) as { waitingSince?: unknown }
      if (typeof parsed.waitingSince === 'number' && Number.isFinite(parsed.waitingSince)
        && parsed.waitingSince > 0 && parsed.waitingSince <= Date.now()) {
        details = { status: 'waiting', waitingSince: parsed.waitingSince }
      }
    }
    if (key && legacyKey && key !== legacyKey && confirmationPending(details.status)) {
      storage().setItem(key, JSON.stringify({ waitingSince: details.waitingSince }))
      storage().removeItem(legacyKey)
    }
  } catch { /* Advisory persistence must not block recovery. */ }
  const listeners = new Set<() => void>()
  let nextCheck = 0
  let checking: Promise<void> | undefined
  let revision = 0
  let observers = 0
  let timer: ReturnType<typeof setInterval> | undefined
  let stopEvents: (() => void) | undefined
  function update(next: DeploymentConfirmationDetails) {
    details = next
    if (next.status === 'ready' || next.status === 'idle') {
      clearInterval(timer); timer = undefined
    } else if (observers > 0 && timer === undefined) {
      timer = setInterval(() => { void refresh() }, CHECK_INTERVAL_MS)
    }
    try {
      if (key) {
        if (confirmationPending(next.status)) storage().setItem(key, JSON.stringify({ waitingSince: next.waitingSince }))
        else storage().removeItem(key)
      }
    } catch { /* Storage denial must not break preparation. */ }
    listeners.forEach(listener => { listener() })
  }
  function pending() {
    const error = new SponsorRequestError({ reason: 'ACCOUNT_DEPLOYMENT_PENDING', retryable: true, message: '' })
    return new SponsorRequestError({ reason: error.reason, retryable: true, message: sponsorReasonMessage(error) })
  }
  async function refresh(force = false) {
    if (!confirmationPending(details.status) || document.visibilityState === 'hidden') return
    if (!checking && (force || now() >= nextCheck)) {
      nextCheck = now() + CHECK_INTERVAL_MS
      const checkedRevision = revision
      checking = (async () => {
        try {
          const confirmed = await hasSafeCode()
          if (revision !== checkedRevision) return
          update({ ...details, status: confirmed ? 'ready' : 'waiting', lastCheckedAt: Date.now(), lastSuccessfulCheckAt: Date.now() })
        } catch {
          if (revision === checkedRevision) update({ ...details, status: 'check-unavailable', lastCheckedAt: Date.now() })
        }
      })().finally(() => { checking = undefined })
    }
    await checking
  }
  async function prepare<T>(action: () => Promise<T>): Promise<T> {
    await refresh()
    if (confirmationPending(details.status)) throw pending()
    try {
      const result = await action()
      if (details.status === 'ready') update({ status: 'idle' })
      return result
    } catch (error) {
      if (asSponsorRequestError(error).reason === 'ACCOUNT_DEPLOYMENT_PENDING') {
        revision += 1
        nextCheck = now() + CHECK_INTERVAL_MS
        update({ status: 'waiting', waitingSince: details.waitingSince ?? Date.now() })
        throw pending()
      }
      throw error
    }
  }
  return Object.assign(prepare, {
    notePending(waitingSince) {
      if (details.status !== 'idle') return
      const start = typeof waitingSince === 'number' && Number.isFinite(waitingSince) && waitingSince > 0 && waitingSince <= Date.now() ? waitingSince : Date.now()
      update({ status: 'waiting', waitingSince: start })
    },
    getSnapshot: () => details.status,
    getDetails: () => details,
    subscribe(listener: () => void) { listeners.add(listener); return () => { listeners.delete(listener) } },
    start() {
      observers += 1
      if (observers === 1) {
        void refresh()
        timer = setInterval(() => { void refresh() }, CHECK_INTERVAL_MS)
        const focus = () => { void refresh(true) }
        window.addEventListener('focus', focus)
        document.addEventListener('visibilitychange', focus)
        stopEvents = () => { window.removeEventListener('focus', focus); document.removeEventListener('visibilitychange', focus) }
      }
      let stopped = false
      return () => {
        if (stopped) return
        stopped = true
        observers -= 1
        if (observers === 0) {
          clearInterval(timer); stopEvents?.()
          revision += 1 // Ignore reads still running when the saved scope unmounts.
        }
      }
    },
  } satisfies DeploymentConfirmationMonitor)
}
