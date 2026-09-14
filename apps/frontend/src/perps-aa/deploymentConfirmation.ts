import { asSponsorRequestError, SponsorRequestError, sponsorReasonMessage } from './errors'

const CHECK_INTERVAL_MS = 15_000

export type DeploymentConfirmationStatus = 'idle' | 'waiting' | 'ready'
export interface DeploymentConfirmationMonitor {
  getSnapshot: () => DeploymentConfirmationStatus
  subscribe: (listener: () => void) => () => void
  start: () => () => void
}

interface Persistence {
  scope: string
  storage?: () => Pick<Storage, 'getItem' | 'setItem' | 'removeItem'>
}

/** Advisory backoff only. Safe code is not identity proof: every manual retry
 * still passes backend verification. Background checks never execute actions.
 */
export function createDeploymentConfirmationGate(
  hasSafeCode: () => Promise<boolean>,
  now: () => number = () => performance.now(),
  persistence?: Persistence,
) {
  const key = persistence ? `plether:deployment-confirmation:v1:${persistence.scope.toLowerCase()}` : undefined
  function storage() { return persistence?.storage ? persistence.storage() : window.localStorage }
  let status: DeploymentConfirmationStatus = 'idle'
  try { if (key && storage().getItem(key) === 'waiting') status = 'waiting' } catch { /* advisory only */ }
  const listeners = new Set<() => void>()
  let nextCheck = 0
  let checking: Promise<boolean> | undefined
  let revision = 0
  function update(next: DeploymentConfirmationStatus) {
    status = next
    try {
      if (key) {
        if (next === 'waiting') storage().setItem(key, 'waiting')
        else storage().removeItem(key)
      }
    } catch { /* Storage denial must not break transaction preparation. */ }
    listeners.forEach(listener => { listener() })
  }
  function pending() {
    const error = new SponsorRequestError({
      reason: 'ACCOUNT_DEPLOYMENT_PENDING', retryable: true, message: '',
    })
    return new SponsorRequestError({ reason: error.reason, retryable: true, message: sponsorReasonMessage(error) })
  }
  async function refresh() {
    if (status !== 'waiting') return
    if (!checking && now() >= nextCheck) {
      nextCheck = now() + CHECK_INTERVAL_MS
      const checkedRevision = revision
      checking = hasSafeCode()
        // Unavailable evidence cannot unblock preparation or erase waiting.
        .catch(() => false)
        .then(confirmed => {
          // A newer backend rejection invalidates an already-running read.
          if (confirmed && revision === checkedRevision) update('ready')
          return confirmed
        })
        .finally(() => { checking = undefined })
    }
    await checking
  }
  async function prepare<T>(action: () => Promise<T>): Promise<T> {
    await refresh()
    if (status === 'waiting') throw pending()
    try {
      const result = await action()
      if (status === 'ready') update('idle')
      return result
    } catch (error) {
      if (asSponsorRequestError(error).reason === 'ACCOUNT_DEPLOYMENT_PENDING') {
        revision += 1
        nextCheck = now() + CHECK_INTERVAL_MS
        update('waiting')
        throw pending()
      }
      throw error
    }
  }
  return Object.assign(prepare, {
    getSnapshot: () => status,
    subscribe(listener: () => void) {
      listeners.add(listener)
      return () => { listeners.delete(listener) }
    },
    start() {
      void refresh()
      const timer = setInterval(() => { void refresh() }, CHECK_INTERVAL_MS)
      return () => { clearInterval(timer) }
    },
  } satisfies DeploymentConfirmationMonitor)
}
