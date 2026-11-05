import { responseClock } from './responseClock'

export interface DeadlineClock { now(): number }
let clock: DeadlineClock | undefined

/** Never infer authorization freshness from the client's wall clock. */
export function deadlineNow(): number {
  if (!clock) throw new Error('Server time is unavailable. Refresh the review before approving this order.')
  return clock.now()
}

export function observeDeadlineResponse(response: Response, startedAt: number): DeadlineClock {
  clock = responseClock(response, startedAt)
  return clock
}

/** Refresh after wallet interaction too: monotonic timers can pause during sleep. */
export async function refreshDeadlineClock(signal?: AbortSignal): Promise<DeadlineClock> {
  const startedAt = performance.now()
  const response = await fetch('/api/perps/v1/readiness', {
    credentials: 'same-origin', cache: 'no-store', redirect: 'error',
    signal: signal ? AbortSignal.any([signal, AbortSignal.timeout(4_000)]) : AbortSignal.timeout(4_000),
  })
  if (!response.ok) throw new Error('Server time is unavailable. Refresh the review before approving this order.')
  return observeDeadlineResponse(response, startedAt)
}
