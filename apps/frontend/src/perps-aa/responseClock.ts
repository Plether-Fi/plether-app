/** A clock scoped to one uncached HTTPS response, never to transaction authority.
 * HTTP Date has second precision. Include that uncertainty and the full request
 * duration so transport delays cannot extend a response's apparent lifetime.
 * Elapsed time is monotonic and does not depend on the device's wall clock.
 */
export function responseClock(response: Response, requestStartedAt: number): { now(): number } {
  const receivedAt = performance.now()
  const date = response.headers.get('Date')
  const serverTime = date === null ? NaN : Date.parse(date)
  const age = response.headers.get('Age')
  const ageSeconds = age === null ? 0 : /^\d+$/.test(age) ? Number(age) : NaN
  const elapsed = receivedAt - requestStartedAt
  const upperTime = serverTime + 1_000 + ageSeconds * 1_000 + elapsed
  if (!Number.isFinite(serverTime) || !Number.isSafeInteger(ageSeconds)
    || ageSeconds < 0 || !Number.isFinite(elapsed) || elapsed < 0
    || !Number.isSafeInteger(Math.ceil(upperTime))) throw new Error('Response time unavailable')
  return { now: () => upperTime + performance.now() - receivedAt }
}
