import type { PerpsActionPlan } from '@plether-fi/perps-aa-client'
import type { Hex } from 'viem'
import type { PerpsAaDeploymentManifestV2 } from './manifest'
import { persistManagedUserOperation, readPersistedManagedUserOperation, type PersistedManagedUserOperationV1 } from './persistedUserOperation'
import type { ManagedUserOperation } from './runtimeContext'

/** The request exists before the first RPC, so lost responses reuse its ID. */
export interface NativePreparationRequestV1 {
  version: 1
  preparationId: string
  reviewedState?: string
  manifest: PerpsAaDeploymentManifestV2
  action: Omit<PerpsActionPlan, 'calls'> & {
    calls: { to: `0x${string}`; value: string; data: Hex }[]
  }
}

/** Sponsor authorization is deliberately distinct from an owner-signed operation. */
export interface PreparedOperationV1 {
  version: 1
  operation: Omit<PersistedManagedUserOperationV1, 'signature'>
  expectedHash: Hex
  validUntil: string
}

export function persistReviewedAction(action: PerpsActionPlan): NativePreparationRequestV1['action'] {
  return { kind: action.kind, account: action.account,
    calls: action.calls.map(call => ({ ...call, value: call.value.toString() })) }
}

export function restoreReviewedAction(action: NativePreparationRequestV1['action']): PerpsActionPlan {
  return { ...action, calls: action.calls.map(call => ({ ...call, value: BigInt(call.value) })) }
}

export function persistPreparedOperation(operation: ManagedUserOperation, expectedHash: Hex, validUntil: bigint): PreparedOperationV1 {
  const unsigned = persistManagedUserOperation({ ...operation, signature: '0x' })
  const payload: Omit<PersistedManagedUserOperationV1, 'signature'> & { signature?: Hex } = unsigned
  delete payload.signature
  return { version: 1, operation: payload, expectedHash, validUntil: validUntil.toString() }
}

export function restorePreparedOperation(prepared: PreparedOperationV1): ManagedUserOperation {
  if (!isPreparationVersionSupported(prepared.version) || 'signature' in prepared.operation) throw new Error('Invalid unsigned preparation')
  const operation = readPersistedManagedUserOperation({ ...prepared.operation, signature: '0x' })
  if (!operation) throw new Error('The saved preparation is incomplete; review a new transaction')
  return operation
}

export function isExplicitSignatureRejection(error: unknown): boolean {
  const seen = new Set<unknown>()
  for (let current = error; current && typeof current === 'object' && !seen.has(current);) {
    seen.add(current)
    const value = current as { code?: unknown; cause?: unknown }
    if (value.code === 4001) return true
    current = value.cause
  }
  return false
}

export interface PreparationStatusV1 {
  version: 1
  authorizationState: string
  validUntil: string | null
  serverTime: string
  safeBlockTimestamp: string | null
  phase: 'prepared' | 'submitted' | 'included' | 'settled' | 'expiry-awaiting-reconciliation' | 'resolved'
  reason: string
  recoverable: boolean
  freshReviewAllowed: boolean
  userOperationHash: Hex | null
  transactionHash: Hex | null
}

export function isPreparationVersionSupported(value: unknown): boolean { return value === 1 }

export function parsePreparationStatus(value: unknown): PreparationStatusV1 {
  if (!value || typeof value !== 'object') throw new Error('Invalid preparation status')
  const row = value as Record<string, unknown>
  if (row.version !== 1 || typeof row.recoverable !== 'boolean' || typeof row.freshReviewAllowed !== 'boolean'
    || typeof row.reason !== 'string' || typeof row.serverTime !== 'string' || !/^\d+$/.test(row.serverTime)
    || !['prepared','submitted','included','settled','expiry-awaiting-reconciliation','resolved'].includes(String(row.phase))) {
    throw new Error('Unsupported preparation status')
  }
  return row as unknown as PreparationStatusV1
}
