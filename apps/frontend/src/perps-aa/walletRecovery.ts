import type { Address, Hex } from 'viem'
import { recoveryFetch } from './recoveryTransport'
import { preparationIdentifier } from './nativePreparation'
import { parsePreparationStatus, type PreparationStatusV1 } from './preparedOperation'

export const PREPARATION_RECOVERY_HEADER = 'X-Plether-AA-Preparation-Recovery'
export type WalletRecoveryResult = (PreparationStatusV1 & { recoveryVerified: true; canRetire: boolean; retirementReason?: string | null }) | {
  version: 1
  recoveryState: 'missing' | 'ambiguous' | 'unresolved' | 'retired'
  reason: string
  canRetire: boolean
  operationHashes: Hex[]
  operationOutcomes?: { hash: Hex; state: string; transactionHash: Hex | null; executionSuccess: boolean | null }[]
}
export interface WalletPreparationRecovery {
  verify(preparationId: string): Promise<void>
  status(preparationId: string): Promise<WalletRecoveryResult>
  retire(preparationId: string): Promise<WalletRecoveryResult>
  headers(preparationId: string): Record<string, string>
  bindOperation(preparationId: string, hash: Hex): void
  operationHeaders(hash: Hex): Record<string, string>
}
export class PreparationRecoveryError extends Error {
  readonly reason: string
  constructor(reason: string) { super(recoveryMessage(reason)); this.reason = reason; this.name = 'PreparationRecoveryError' }
}
export function recoveryReason(error: unknown): string | undefined {
  const seen = new Set<object>()
  for (let value = error; value && typeof value === 'object' && !seen.has(value);) {
    seen.add(value)
    const row = value as { reason?: unknown; data?: { reason?: unknown }; cause?: unknown; code?: unknown; name?: unknown; details?: unknown }
    const reason = row.reason ?? row.data?.reason
    if (typeof reason === 'string' && /^[A-Z_]{1,64}$/.test(reason)) return reason
    if (row.code === 4001) return 'WALLET_SIGNATURE_DECLINED'
    if (row.code === -32002) return 'WALLET_REQUEST_PENDING'
    if (row.name === 'TimeoutError') return 'RECOVERY_TIMEOUT'
    // viem stores non-2xx JSON-RPC errors in HttpRequestError.details instead
    // of cause/data. Read only a bounded structured reason, never raw text.
    if (row.name === 'HttpRequestError' && typeof row.details === 'string' && row.details.length <= 8_192) {
      try {
        const parsed: unknown = JSON.parse(row.details)
        if (parsed && typeof parsed === 'object' && 'data' in parsed) {
          const data = parsed.data
          if (data && typeof data === 'object' && 'reason' in data
            && typeof data.reason === 'string' && /^[A-Z_]{1,64}$/.test(data.reason)) return data.reason
        }
      } catch { /* Unstructured transport details are not a recovery reason. */ }
    }
    value = row.cause
  }
}
export function recoveryMessage(reason?: string): string {
  switch (reason) {
    case 'INVALID_ORDER_DEADLINE':
    case 'DEADLINE_TOO_CLOSE': return 'This order has expired or is too close to expiry. Verify recovery to safely discard it, then review the order again with a fresh deadline.'
    case 'RECOVERY_VERIFICATION_REQUIRED':
    case 'PREPARATION_NOT_AUTHORIZED': return 'Verify your owner wallet to recover this saved attempt.'
    case 'RECOVERY_DISABLED': return 'Wallet recovery is not enabled on this deployment yet. Your saved attempt is retained.'
    case 'RECOVERY_RETIREMENT_DISABLED': return 'Verified discard is not enabled on this deployment yet. Your saved attempt is retained.'
    case 'RECOVERY_MULTIPLE_PREPARATIONS': return 'More than one preparation matches this attempt. Check their existing outcomes before continuing.'
    case 'RECOVERY_BINDING_UNRESOLVED': return 'The original deployment could not be verified. The saved attempt is retained.'
    case 'RECOVERY_LIABILITY_PENDING': return 'Waiting for the original sponsorship or an in-progress preparation to resolve.'
    case 'PREPARATION_NOT_CREATED': return 'No preparation was found for this verified attempt. Discard it when available, then review a new transaction.'
    case 'PREPARATION_RETIRED': return 'This saved attempt has been safely retired. Review current account activity before starting another transaction.'
    case 'ACCOUNT_DEPLOYMENT_PENDING': return 'Waiting for Trading Account confirmation. No transaction will be sent automatically.'
    case 'RATE_LIMITED': return 'Too many recovery requests. Wait a moment and try again.'
    case 'WALLET_SIGNATURE_DECLINED': return 'The ownership signature was declined. Choose Verify wallet to recover and approve the gas-free message to continue.'
    case 'WALLET_REQUEST_PENDING': return 'A wallet request is already open. Open your wallet and complete or dismiss it, then try verification again.'
    case 'RECOVERY_TIMEOUT': return 'The recovery request timed out. Your saved attempt is retained; check recovery again.'
    case 'RECOVERY_CHALLENGE_EXPIRED': return 'The ownership message expired. Verify your wallet again to request a fresh message.'
    case 'RECOVERY_CLOCK_MISMATCH': return 'Your device clock does not match the recovery service. Enable automatic date and time, then verify your wallet again.'
    case 'INVALID_RECOVERY_RESPONSE': return 'The recovery response could not be verified. Refresh the app and try again. If this continues, contact support with the support reference.'
    case 'POLICY_DENIED': return 'The recovery request was rejected. Refresh the app and verify the original owner wallet. If this continues, contact support with the support reference.'
    case 'PREPARATION_UNUSABLE': return 'The saved preparation can no longer be signed. Check recovery to discard it when available.'
    case 'RECOVERY_UNAVAILABLE': return 'The recovery service could not be reached. Your saved attempt is retained; check recovery again.'
    default: return 'This recovery step could not be completed. Your saved attempt is retained. Check recovery again; if this continues, contact support with the support reference.'
  }
}
function record(value: unknown): Record<string, unknown> {
  if (!value || typeof value !== 'object' || Array.isArray(value)) throw new PreparationRecoveryError('INVALID_RECOVERY_RESPONSE')
  return value as Record<string, unknown>
}
export function parseWalletRecoveryResult(value: unknown): WalletRecoveryResult {
  const row = record(value)
  if (row.version !== 1 || typeof row.canRetire !== 'boolean') throw new PreparationRecoveryError('INVALID_RECOVERY_RESPONSE')
  if ('recoveryState' in row) {
    if (!['missing', 'ambiguous', 'unresolved', 'retired'].includes(String(row.recoveryState))
      || typeof row.reason !== 'string' || !/^[A-Z_]{1,64}$/.test(row.reason)
      || !Array.isArray(row.operationHashes) || row.operationHashes.length > 20
      || !row.operationHashes.every(hash => typeof hash === 'string' && /^0x[0-9a-f]{64}$/.test(hash))) {
      throw new PreparationRecoveryError('INVALID_RECOVERY_RESPONSE')
    }
    if (row.operationOutcomes !== undefined && (!Array.isArray(row.operationOutcomes) || row.operationOutcomes.length > 20
      || !row.operationOutcomes.every(value => {
        const outcome = record(value)
        return typeof outcome.hash === 'string' && /^0x[0-9a-f]{64}$/.test(outcome.hash)
          && ['reserved', 'signed', 'submitted', 'settled', 'expired', 'cancelled'].includes(String(outcome.state))
          && (outcome.transactionHash === null || (typeof outcome.transactionHash === 'string' && /^0x[0-9a-f]{64}$/.test(outcome.transactionHash)))
          && (outcome.executionSuccess === null || typeof outcome.executionSuccess === 'boolean')
      }))) throw new PreparationRecoveryError('INVALID_RECOVERY_RESPONSE')
    return row as unknown as WalletRecoveryResult
  }
  if (row.retirementReason !== undefined && row.retirementReason !== null
    && (typeof row.retirementReason !== 'string' || !/^[A-Z_]{1,64}$/.test(row.retirementReason))) {
    throw new PreparationRecoveryError('INVALID_RECOVERY_RESPONSE')
  }
  if (row.recoveryVerified !== true) throw new PreparationRecoveryError('INVALID_RECOVERY_RESPONSE')
  return { ...parsePreparationStatus(row), recoveryVerified: true, canRetire: row.canRetire,
    ...(row.retirementReason !== undefined ? { retirementReason: row.retirementReason } : {}) }
}
export function recoveryChallengeMessage(input: {
  origin: string; chainId: number; paymaster: string; sender: string; preparationId: string; owner: string; nonce: string; expiresAt: number
}) {
  return ['Plether Trading Account Recovery', `Origin: ${input.origin}`, `Chain ID: ${String(input.chainId)}`,
    `Paymaster: ${input.paymaster.toLowerCase()}`, `Trading Account: ${input.sender.toLowerCase()}`,
    `Preparation ID: ${input.preparationId}`, `Owner: ${input.owner.toLowerCase()}`,
    'Purpose: Recover this saved attempt and allow retiring it only after backend safety checks.',
    'This message does not authorize a blockchain transaction or spending.',
    `Nonce: ${input.nonce}`, `Expires At (Unix seconds): ${String(input.expiresAt)}`].join('\n')
}
/** Tokens live only in this account/deployment runtime. Never persist or log. */
export function createWalletPreparationRecovery(input: {
  rpcUrl: string; chainId: number; paymaster: Address; sender: Address; owner: Address
  signMessage(message: string): Promise<Hex>
  fetcher?: typeof fetch
  credentialScope?: string
}): WalletPreparationRecovery {
  const sessions = new Map<string, { token: string; expiresAt: number }>()
  const operationIds = new Map<string, string>()
  const origin = globalThis.location.origin
  const endpoint = new URL(input.rpcUrl, origin).href
  const request = recoveryFetch(input.rpcUrl, input.fetcher ?? fetch, input.credentialScope ?? input.rpcUrl)
  const headers = (id: string): Record<string, string> => {
    const session = sessions.get(id)
    if (session && session.expiresAt > Date.now()) return { [PREPARATION_RECOVERY_HEADER]: session.token }
    sessions.delete(id)
    return {}
  }
  async function rpc(method: string, id: string, extra: Record<string, unknown> = {}) {
    let response: Response
    try {
      response = await request(endpoint, { method: 'POST', credentials: 'same-origin', cache: 'no-store', redirect: 'error',
        signal: AbortSignal.timeout(30_000),
        headers: { 'Content-Type': 'application/json', ...headers(id), ...(/^[0-9a-f-]{36}$/i.test(id) ? { 'X-Plether-Attempt-Id': id } : {}) },
        body: JSON.stringify({ jsonrpc: '2.0', id: 1, method, params: [{ version: 1, chainId: `0x${input.chainId.toString(16)}`,
          sender: input.sender.toLowerCase(), preparationId: preparationIdentifier(id), ...extra }] }),
      })
    } catch (cause) { throw new PreparationRecoveryError(recoveryReason(cause) ?? 'RECOVERY_UNAVAILABLE') }
    let body: Record<string, unknown>
    try { body = record(await response.json()) } catch { throw new PreparationRecoveryError('RECOVERY_UNAVAILABLE') }
    if (!response.ok || body.error) {
      const reason = recoveryReason(body.error) ?? 'RECOVERY_UNAVAILABLE'
      if (reason === 'RECOVERY_VERIFICATION_REQUIRED') sessions.delete(id)
      throw new PreparationRecoveryError(reason)
    }
    return body.result
  }
  return {
    headers,
    bindOperation(id, hash) { if (sessions.has(id)) operationIds.set(hash.toLowerCase(), id) },
    operationHeaders(hash) { const id = operationIds.get(hash.toLowerCase()); return id ? headers(id) : {} },
    async verify(id) {
      const challenge = record(await rpc('plether_getRecoveryChallenge', id, { owner: input.owner.toLowerCase(), origin }))
      if (challenge.version !== 1 || typeof challenge.challengeId !== 'string' || !/^[0-9a-f]{64}$/.test(challenge.challengeId)
        || typeof challenge.expiresAt !== 'number' || !Number.isSafeInteger(challenge.expiresAt)) throw new PreparationRecoveryError('INVALID_RECOVERY_RESPONSE')
      if (challenge.expiresAt * 1000 <= Date.now()) throw new PreparationRecoveryError('RECOVERY_CHALLENGE_EXPIRED')
      if (challenge.expiresAt * 1000 > Date.now() + 305_000) throw new PreparationRecoveryError('RECOVERY_CLOCK_MISMATCH')
      const message = recoveryChallengeMessage({ ...input, origin, preparationId: preparationIdentifier(id), nonce: challenge.challengeId, expiresAt: challenge.expiresAt })
      if (challenge.message !== message) throw new PreparationRecoveryError('INVALID_RECOVERY_RESPONSE')
      const signature = await input.signMessage(message)
      const result = record(await rpc('plether_verifyRecoveryChallenge', id, { challengeId: challenge.challengeId, signature: signature.toLowerCase() }))
      if (result.version !== 1 || typeof result.sessionToken !== 'string' || !/^[0-9a-f]{64}$/.test(result.sessionToken) || result.expiresIn !== 900) throw new PreparationRecoveryError('INVALID_RECOVERY_RESPONSE')
      sessions.set(id, { token: result.sessionToken, expiresAt: Date.now() + 895_000 })
    },
    status: async id => {
      const result = parseWalletRecoveryResult(await rpc('plether_getRecoveryStatus', id))
      if ('phase' in result && result.userOperationHash) operationIds.set(result.userOperationHash.toLowerCase(), id)
      return result
    },
    retire: async id => parseWalletRecoveryResult(await rpc('plether_retirePreparation', id)),
  }
}
