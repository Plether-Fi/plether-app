import { recoveryHttp } from './recoveryTransport'

export const SUBMISSION_TIMEOUT_MS = 30_000

/** A lost acknowledgement is ambiguous. Wait for it, but never retry a send
 * automatically: the durable signed operation must be reconciled by hash. */
export function submissionHttp(url: string, headers: Record<string, string> = {}) {
  return recoveryHttp(url, {
    timeout: SUBMISSION_TIMEOUT_MS,
    retryCount: 0,
    fetchOptions: { headers },
  })
}
