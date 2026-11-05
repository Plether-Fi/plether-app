import { deadlineNow } from './deadlineClock'
import { BundlerRequestError } from './errors'
import { isSignedButUnsubmitted, type SponsoredOperation } from './operationStore'

export const SIGNING_HEADROOM_SECONDS = 45n
export const SUBMISSION_HEADROOM_SECONDS = 30n

export function orderDeadlineNeedsReview(deadline: string | undefined, nowMs = deadlineNow()): boolean {
  if (deadline === undefined) return false
  return !/^\d+$/.test(deadline) || BigInt(deadline) - BigInt(Math.ceil(nowMs / 1000)) < SIGNING_HEADROOM_SECONDS
}

export function operationNeedsFreshOrderReview(operation: SponsoredOperation, nowMs?: number): boolean {
  if (nowMs === undefined) { try { nowMs = deadlineNow() } catch { return operation.orderRequestV3 !== undefined } }
  return (!operation.userOperationHash || isSignedButUnsubmitted(operation)) && (operation.reason === 'INVALID_ORDER_DEADLINE'
    || (operation.orderRequestV3 !== undefined && (operation.reason === 'DEADLINE_TOO_CLOSE'
      || orderDeadlineNeedsReview(operation.orderRequestV3.submitBy, nowMs))))
}

export function requireDeadlineHeadroom(sponsorshipDeadline: bigint, orderDeadline: string | undefined, phase: 'signing' | 'submission', nowMs = deadlineNow()): void {
  const deadline = orderDeadline === undefined ? sponsorshipDeadline : BigInt(orderDeadline) < sponsorshipDeadline ? BigInt(orderDeadline) : sponsorshipDeadline
  const minimum = phase === 'signing' ? SIGNING_HEADROOM_SECONDS : SUBMISSION_HEADROOM_SECONDS
  if (deadline - BigInt(Math.ceil(nowMs / 1000)) < minimum) {
    // A signed operation remains a liability until authoritative recovery proves expiry.
    throw new BundlerRequestError({ reason: 'DEADLINE_TOO_CLOSE', terminalStatus: phase === 'signing' ? 'expired' : 'signed-not-submitted', retryable: false,
      message: phase === 'signing'
        ? 'The reviewed transaction is too close to expiry. Review a fresh order; this operation was not submitted.'
        : 'Wallet approval finished too late, so Plether did not send this transaction. Check recovery to unlock a fresh review once the saved authorization has safely expired.' })
  }
}
