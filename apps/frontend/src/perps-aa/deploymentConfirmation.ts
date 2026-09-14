import { asSponsorRequestError, SponsorRequestError, sponsorReasonMessage } from './errors'

const CHECK_INTERVAL_MS = 15_000

/** Advisory backoff only. A positive read never replaces backend verification.
 * No timers, automatic preparation, signing, or submission run here.
 */
export function createDeploymentConfirmationGate(
  hasSafeCode: () => Promise<boolean>,
  now: () => number = () => performance.now(),
) {
  let waiting = false
  let nextCheck = 0
  let checking: Promise<boolean> | undefined
  function pending() {
    const error = new SponsorRequestError({
      reason: 'ACCOUNT_DEPLOYMENT_PENDING', retryable: true, message: '',
    })
    return new SponsorRequestError({ reason: error.reason, retryable: true, message: sponsorReasonMessage(error) })
  }
  async function check() {
    if (!waiting) return
    if (!checking && now() >= nextCheck) {
      nextCheck = now() + CHECK_INTERVAL_MS
      checking = hasSafeCode()
        // Unavailable evidence cannot unblock preparation or erase waiting.
        .catch(() => false)
        .finally(() => { checking = undefined })
    }
    const confirmed = checking ? await checking : false
    if (!confirmed) throw pending()
    waiting = false
  }
  return async function prepare<T>(action: () => Promise<T>): Promise<T> {
    await check()
    try {
      return await action()
    } catch (error) {
      if (asSponsorRequestError(error).reason === 'ACCOUNT_DEPLOYMENT_PENDING') {
        waiting = true
        nextCheck = now() + CHECK_INTERVAL_MS
        throw pending()
      }
      throw error
    }
  }
}
