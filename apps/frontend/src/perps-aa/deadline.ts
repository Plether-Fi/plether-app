import { BundlerRequestError } from './errors'

export const SIGNING_HEADROOM_SECONDS = 20n
export const SUBMISSION_HEADROOM_SECONDS = 10n

export function requireDeadlineHeadroom(sponsorshipDeadline: bigint, orderDeadline: string | undefined, phase: 'signing' | 'submission', nowMs = Date.now()): void {
  const deadline = orderDeadline === undefined ? sponsorshipDeadline : BigInt(orderDeadline) < sponsorshipDeadline ? BigInt(orderDeadline) : sponsorshipDeadline
  const minimum = phase === 'signing' ? SIGNING_HEADROOM_SECONDS : SUBMISSION_HEADROOM_SECONDS
  if (deadline - BigInt(Math.ceil(nowMs / 1000)) < minimum) {
    // A signed operation remains a liability until authoritative recovery proves expiry.
    throw new BundlerRequestError({ reason: 'DEADLINE_TOO_CLOSE', terminalStatus: phase === 'signing' ? 'expired' : 'receipt-timeout', retryable: false,
      message: phase === 'signing'
        ? 'The reviewed transaction is too close to expiry. Review a fresh order; this operation was not submitted.'
        : 'Signing finished too close to expiry. The signed payload was saved but not submitted. Wait for safe recovery before reviewing a fresh order.' })
  }
}
