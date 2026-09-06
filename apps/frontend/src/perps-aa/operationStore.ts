import type {
  PerpsActionKind,
  SponsoredExecutionStatus,
} from '@plether/perps-aa-client'
import type { Address, Hex } from 'viem'
import type { PersistedProtectionIntent } from '../contracts/positionProtection'
import { create } from 'zustand'
import {
  devtools,
  persist,
  type PersistStorage,
  type StorageValue,
} from 'zustand/middleware'
import type {
  StableSponsorReason,
  UserOperationTerminalStatus,
} from './errors'
import {
  persistManagedUserOperation,
  type PersistedManagedUserOperationV1,
} from './persistedUserOperation'
import {
  acquireSponsoredOperationBrowserLane,
  type ReleaseSponsoredOperationBrowserLock,
} from './laneLock'
import { SponsoredOperationLockedError } from './operationLockError'
import type { ManagedUserOperation } from './runtimeContext'
import type { PersistedPerpsOrderRequestV2 } from '../contracts/perpsOrderV2'

export { SponsoredOperationLockedError } from './operationLockError'

export type SponsoredOperationStatus =
  | SponsoredExecutionStatus
  | 'failed'
  | 'cancelled'
  | 'outcome-unknown'
  | UserOperationTerminalStatus

export interface SponsoredOperation {
  id: string
  ownerAddress: Address
  accountAddress: Address
  chainId: number
  accountMode: string
  manifestVersion: string
  action: PerpsActionKind
  /** Immutable bounded-order intent, journaled before UserOperation signing. */
  orderRequestV2?: PersistedPerpsOrderRequestV2
  protectionIntent?: PersistedProtectionIntent
  authorizationToken?: Address
  /** EIP-3009 nonce paired with authorizationToken for owned cleanup. */
  authorizationNonce?: Hex
  lane: string
  status: SponsoredOperationStatus
  sponsorshipAccepted: boolean
  userOperationHash?: Hex
  signedUserOperation?: PersistedManagedUserOperationV1
  submissionMetadataVersion?: 1
  hashRecordedAt?: number
  automaticRecoveryStartedAt?: number
  lastAutomaticRecoveryAttemptAt?: number
  automaticRecoveryAttemptCount?: number
  automaticRecoveryExhaustedAt?: number
  automaticRecoveryExpired?: true
  forcedLegacyUnlock?: boolean
  protocolNonceAdvanced?: true
  legacyManualUnlockEligible?: boolean
  legacyInboxIdentity?: true
  /**
   * Monotonic record that the lane was released after observing an exact,
   * canonical, successful inclusion. The observation can still be reorged,
   * but this marker must not be erased and relock the lane behind a newer
   * operation. Recovery keeps tracking the original operation independently.
   */
  laneReleasedAfterSuccessfulInclusion?: true
  /**
   * Exact latest-chain inclusion observed before the receipt reached the
   * RPC's safe head. This evidence is deliberately distinct from the
   * safe-confirmed transactionHash below so recovery can continue tracking it
   * after the submission lane has been released.
   */
  includedTransactionHash?: Hex
  includedBlockNumber?: string
  includedBlockHash?: Hex
  inclusionObservedAt?: number
  /**
   * Orders unsafe-inclusion observations and retractions across tabs. A
   * revision with no includedTransactionHash means a direct canonical block
   * check proved that the prior observation was reorged.
   */
  inclusionEvidenceRevision?: number
  transactionHash?: Hex
  transactionHashVerified?: boolean
  reason?: StableSponsorReason
  retryable?: boolean
  replacementUserOperationHash?: Hex
  retryCount: number
  createdAt: number
  updatedAt: number
  statusTimestamps: Partial<Record<SponsoredOperationStatus, number>>
  attentionRevision?: number
  acknowledgedAttentionRevision?: number
}

interface BeginSponsoredOperationInput {
  id: string
  ownerAddress: Address
  accountAddress: Address
  chainId: number
  accountMode: string
  manifestVersion: string
  action: PerpsActionKind
  orderRequestV2?: PersistedPerpsOrderRequestV2
  protectionIntent?: PersistedProtectionIntent
  authorizationToken?: Address
  authorizationNonce?: Hex
  lane?: string
}

export interface SponsoredOperationInclusionObservation {
  transactionHash: Hex
  blockNumber?: string
  blockHash?: Hex
  /**
   * Callers set this only after both the exact UserOperation receipt and its
   * canonical transaction receipt report success. Recording evidence alone
   * never releases a lane; releaseLaneAfterSuccessfulInclusion requires true.
   */
  success?: boolean
}

export type SuccessfulSponsoredOperationInclusionObservation =
  SponsoredOperationInclusionObservation & { success: true }

interface SponsoredOperationState {
  operations: SponsoredOperation[]
  activeLanes: Record<string, string>

  beginOperation: (input: BeginSponsoredOperationInput) => void
  transition: (id: string, status: SponsoredOperationStatus) => void
  recordUserOperationHash: (
    id: string,
    hash: Hex,
    metadata?: {
      signedUserOperation: ManagedUserOperation
    }
  ) => boolean
  recordObservedInclusion: (
    id: string,
    observation: SponsoredOperationInclusionObservation
  ) => boolean
  releaseLaneAfterSuccessfulInclusion: (
    id: string,
    observation: SuccessfulSponsoredOperationInclusionObservation
  ) => boolean
  /** Call only after a direct canonical block-hash check proves a reorg. */
  clearObservedInclusion: (id: string) => boolean
  recordTransactionHash: (id: string, hash: Hex) => void
  incrementRetry: (id: string) => void
  recordAutomaticRecoveryAttempt: (id: string, attemptedAt: number) => boolean
  exhaustAutomaticRecovery: (id: string, exhaustedAt: number) => void
  acknowledgeOperations: (operations: {
    id: string
    attentionRevision: number
  }[]) => void
  failOperation: (input: {
    id: string
    status?: 'failed' | 'outcome-unknown' | UserOperationTerminalStatus
    reason?: StableSponsorReason
    retryable: boolean
    replacementUserOperationHash?: Hex
    protocolNonceAdvanced?: true
  }) => void
  cancelOperation: (id: string) => void
  releaseLane: (id: string) => void
  cleanupOperations: () => void
  getActiveOperation: (accountAddress: Address, lane?: string) => SponsoredOperation | undefined
}

export const SPONSORED_OPERATION_STORAGE_NAME =
  'plether_perps_sponsored_operations'
export const SPONSORED_OPERATION_JOURNAL_PREFIX =
  `${SPONSORED_OPERATION_STORAGE_NAME}:operation:`
export const SPONSORED_OPERATION_LANE_HEAD_PREFIX =
  `${SPONSORED_OPERATION_STORAGE_NAME}:lane:`
export const SPONSORED_OPERATION_RESOLUTION_PREFIX =
  `${SPONSORED_OPERATION_STORAGE_NAME}:resolution:`
export const SPONSORED_OPERATION_LANE_RELEASE_PREFIX =
  `${SPONSORED_OPERATION_STORAGE_NAME}:lane-release:`
export const DEFAULT_SPONSORED_OPERATION_LANE = 'default'
export const LEGACY_AMBIGUOUS_OPERATION_MANIFEST_VERSION =
  'perps-aa-arbitrum-sepolia-20260717-v1'
export const SPONSORED_OPERATION_STORAGE_VERSION = 1
export const SPONSORED_OPERATION_AUTOMATIC_RECOVERY_INITIAL_DELAY_MS = 5_000
export const SPONSORED_OPERATION_AUTOMATIC_RECOVERY_MAX_DELAY_MS =
  5 * 60 * 1000
export const SPONSORED_OPERATION_AUTOMATIC_RECOVERY_WINDOW_MS =
  30 * 60 * 1000
export const SPONSORED_OPERATION_STALE_RECOVERY_AGE_MS =
  24 * 60 * 60 * 1000

function laneKey(
  accountAddress: Address,
  lane = DEFAULT_SPONSORED_OPERATION_LANE
): string {
  return `${accountAddress.toLowerCase()}:${lane}`
}

function updateOperation(
  operations: SponsoredOperation[],
  id: string,
  update: (operation: SponsoredOperation) => SponsoredOperation
): SponsoredOperation[] {
  return operations.map((operation) => operation.id === id ? update(operation) : operation)
}

export function isSponsoredOperationTerminal(
  status: SponsoredOperationStatus
): boolean {
  return [
    'confirmed',
    'failed',
    'cancelled',
    'execution-reverted',
    'dropped',
    'replaced',
    'expired',
    'outcome-unknown',
  ].includes(status)
}

function sponsoredOperationAutomaticRecoveryStartedAt(
  operation: SponsoredOperation
): number {
  return operation.automaticRecoveryStartedAt ??
    operation.statusTimestamps['receipt-timeout'] ??
    operation.hashRecordedAt ??
    operation.createdAt
}

export function sponsoredOperationAutomaticRecoveryDelayMs(
  completedAttemptCount: number
): number {
  const exponent = Math.max(0, Math.min(completedAttemptCount - 1, 16))
  return Math.min(
    SPONSORED_OPERATION_AUTOMATIC_RECOVERY_INITIAL_DELAY_MS * (2 ** exponent),
    SPONSORED_OPERATION_AUTOMATIC_RECOVERY_MAX_DELAY_MS
  )
}

export function sponsoredOperationAutomaticRecoveryIsDue(
  operation: SponsoredOperation,
  now: number
): boolean {
  if (
    operation.automaticRecoveryExhaustedAt !== undefined ||
    now - sponsoredOperationAutomaticRecoveryStartedAt(operation) >=
      SPONSORED_OPERATION_AUTOMATIC_RECOVERY_WINDOW_MS
  ) {
    return false
  }
  if (operation.lastAutomaticRecoveryAttemptAt === undefined) return true
  return now - operation.lastAutomaticRecoveryAttemptAt >=
    sponsoredOperationAutomaticRecoveryDelayMs(
      operation.automaticRecoveryAttemptCount ?? 1
    )
}

export function sponsoredOperationAutomaticRecoveryIsExhausted(
  operation: SponsoredOperation,
  now: number
): boolean {
  return operation.automaticRecoveryExhaustedAt !== undefined ||
    now - sponsoredOperationAutomaticRecoveryStartedAt(operation) >=
      SPONSORED_OPERATION_AUTOMATIC_RECOVERY_WINDOW_MS
}

function sponsoredOperationRecoveryIsStale(
  operation: SponsoredOperation,
  now: number
): boolean {
  return operation.userOperationHash !== undefined &&
    !isSponsoredOperationTerminal(operation.status) &&
    now - sponsoredOperationAutomaticRecoveryStartedAt(operation) >=
      SPONSORED_OPERATION_STALE_RECOVERY_AGE_MS
}

/**
 * The lifecycle and lane-serialization state machines intentionally diverge
 * after an exact successful inclusion: recovery remains nonterminal while a
 * newer operation may use the lane.
 */
export function isSponsoredOperationLaneBlocking(
  operation: SponsoredOperation
): boolean {
  return !isSponsoredOperationTerminal(operation.status) &&
    operation.laneReleasedAfterSuccessfulInclusion !== true
}

function canResolveTerminalOperation(
  currentStatus: SponsoredOperationStatus,
  nextStatus: SponsoredOperationStatus
): boolean {
  if (currentStatus === 'confirmed') return false
  return nextStatus === 'execution-reverted' ||
    (
      currentStatus === 'outcome-unknown' &&
      nextStatus === 'expired'
    )
}

export function isSponsoredOperationAttentionStatus(
  status: SponsoredOperationStatus
): boolean {
  return [
    'receipt-timeout',
    'failed',
    'execution-reverted',
    'dropped',
    'replaced',
    'expired',
    'outcome-unknown',
  ].includes(status)
}

export function hasObservedSponsoredOperationInclusion(
  operation: SponsoredOperation
): operation is SponsoredOperation & { includedTransactionHash: Hex } {
  return operation.includedTransactionHash !== undefined
}

function sponsoredOperationInclusionEvidenceRevision(
  operation: SponsoredOperation | undefined
): number {
  return operation?.inclusionEvidenceRevision ??
    (operation?.includedTransactionHash === undefined ? 0 : 1)
}

function operationMatchesInclusionObservation(
  operation: SponsoredOperation | undefined,
  observation: SponsoredOperationInclusionObservation
): boolean {
  return operation?.includedTransactionHash?.toLowerCase() ===
      observation.transactionHash.toLowerCase() &&
    (
      observation.blockNumber === undefined ||
      operation.includedBlockNumber === observation.blockNumber
    ) &&
    (
      observation.blockHash === undefined ||
      operation.includedBlockHash?.toLowerCase() ===
        observation.blockHash.toLowerCase()
    )
}

function observationReportsSuccessfulInclusion(
  observation: SponsoredOperationInclusionObservation
): observation is SuccessfulSponsoredOperationInclusionObservation {
  return observation.success === true
}

function sponsoredOperationInclusionEvidenceTieBreakKey(
  operation: SponsoredOperation
): string {
  return [
    operation.includedTransactionHash?.toLowerCase() ?? '',
    operation.includedBlockNumber ?? '',
    operation.includedBlockHash?.toLowerCase() ?? '',
  ].join(':')
}

export function getSponsoredOperationAttentionRevision(
  operation: SponsoredOperation
): number {
  return operation.attentionRevision ??
    (isSponsoredOperationAttentionStatus(operation.status) ? 1 : 0)
}

function transitionAttentionRevision(
  operation: SponsoredOperation,
  nextStatus: SponsoredOperationStatus
): number {
  const currentRevision = getSponsoredOperationAttentionRevision(operation)
  const enteredNewAttentionStatus =
    isSponsoredOperationAttentionStatus(nextStatus) &&
    (!isSponsoredOperationAttentionStatus(operation.status) ||
      operation.status !== nextStatus)

  return enteredNewAttentionStatus ? currentRevision + 1 : currentRevision
}

function failureAttentionRevision(
  operation: SponsoredOperation,
  nextStatus: SponsoredOperationStatus
): number {
  const currentRevision = getSponsoredOperationAttentionRevision(operation)
  return isSponsoredOperationAttentionStatus(nextStatus)
    ? currentRevision + 1
    : currentRevision
}

export function canCancelSponsoredOperationLocally(
  operation: SponsoredOperation
): boolean {
  return operationAbortControllers.has(operation.id) &&
    operation.userOperationHash === undefined &&
    ![
      'submitting',
      'confirming',
      'confirmed',
      'failed',
      'cancelled',
      'execution-reverted',
      'dropped',
      'replaced',
      'expired',
      'outcome-unknown',
      'receipt-timeout',
    ].includes(operation.status)
}

function releaseOperationLane(
  activeLanes: Record<string, string>,
  operationId: string
): Record<string, string> {
  return Object.fromEntries(
    Object.entries(activeLanes).filter(([, activeId]) => activeId !== operationId)
  )
}

interface PersistedSponsoredOperationState {
  operations: SponsoredOperation[]
  activeLanes: Record<string, string>
}

function persistedOperationState(
  value: unknown
): PersistedSponsoredOperationState {
  if (!value || typeof value !== 'object') {
    return { operations: [], activeLanes: {} }
  }
  const record = value as {
    operations?: unknown
    activeLanes?: unknown
  }
  const operations = Array.isArray(record.operations)
    ? record.operations.filter(
        (operation): operation is SponsoredOperation =>
          Boolean(operation) && typeof operation === 'object'
      )
    : []
  const activeLanes = record.activeLanes &&
      typeof record.activeLanes === 'object'
    ? Object.fromEntries(
        Object.entries(record.activeLanes).filter(
          (entry): entry is [string, string] =>
            typeof entry[1] === 'string'
        )
      )
    : {}
  return { operations, activeLanes }
}

/**
 * Version 0 could persist Pimlico's diagnostic terminal labels and transaction
 * hash as if they proved a canonical outcome. Normalize those records before
 * the UI or lane lock consumes them.
 */
export function migrateSponsoredOperationState(
  persistedState: unknown,
  persistedVersion: number
): PersistedSponsoredOperationState {
  const persisted = persistedOperationState(persistedState)
  if (persistedVersion > SPONSORED_OPERATION_STORAGE_VERSION) {
    throw new Error(
      'The sponsored-operation store was written by a newer app version'
    )
  }
  if (persistedVersion === SPONSORED_OPERATION_STORAGE_VERSION) {
    return {
      operations: persisted.operations,
      activeLanes: activeLanesForOperations(persisted.operations),
    }
  }

  const relockIds = new Set<string>()
  const operations = persisted.operations.map((operation) => {
    if (
      operation.submissionMetadataVersion === 1 &&
      operation.userOperationHash !== undefined &&
      operation.signedUserOperation !== undefined
    ) {
      return {
        ...operation,
        legacyInboxIdentity: true as const,
      }
    }

    if (operation.userOperationHash === undefined) {
      if (operation.status !== 'confirmed') {
        return operation.transactionHash === undefined
          ? operation
          : {
              ...operation,
              transactionHash: undefined,
              transactionHashVerified: undefined,
            }
      }

      const nextStatus: SponsoredOperationStatus = 'outcome-unknown'
      return {
        ...operation,
        status: nextStatus,
        reason: undefined,
        retryable: false,
        transactionHash: undefined,
        transactionHashVerified: undefined,
        replacementUserOperationHash: undefined,
        attentionRevision: transitionAttentionRevision(operation, nextStatus),
        statusTimestamps: {
          ...operation.statusTimestamps,
          [nextStatus]:
            operation.statusTimestamps[nextStatus] ?? operation.updatedAt,
        },
      }
    }

    const nextStatus: SponsoredOperationStatus = 'receipt-timeout'
    relockIds.add(operation.id)

    return {
      ...operation,
      status: nextStatus,
      reason: 'BUNDLER_UNAVAILABLE',
      retryable: false,
      forcedLegacyUnlock: undefined,
      legacyManualUnlockEligible: true,
      legacyInboxIdentity: true as const,
      // Version 0 also copied Pimlico's diagnostic bundle hash. It is not a
      // verified EntryPoint inclusion transaction.
      transactionHash: undefined,
      transactionHashVerified: undefined,
      replacementUserOperationHash: undefined,
      attentionRevision: transitionAttentionRevision(operation, nextStatus),
      statusTimestamps: {
        ...operation.statusTimestamps,
        [nextStatus]:
          operation.statusTimestamps[nextStatus] ?? operation.updatedAt,
      },
    }
  })

  const operationById = new Map(
    operations.map((operation) => [operation.id, operation])
  )
  const activeLanes = Object.fromEntries(
    Object.entries(persisted.activeLanes).filter(([, operationId]) => {
      const operation = operationById.get(operationId)
      return operation !== undefined &&
        isSponsoredOperationLaneBlocking(operation)
    })
  )
  const relockCandidates = operations
    .filter((operation) => relockIds.has(operation.id))
    .sort((left, right) => right.updatedAt - left.updatedAt)
  for (const operation of relockCandidates) {
    const key = laneKey(operation.accountAddress, operation.lane)
    activeLanes[key] ??= operation.id
  }

  return { operations, activeLanes }
}

const operationAbortControllers = new Map<string, AbortController>()

function operationMergeScore(operation: SponsoredOperation): number {
  return (
    (isSponsoredOperationTerminal(operation.status) ? 16 : 0) +
    (operation.transactionHashVerified === true ? 8 : 0) +
    (operation.submissionMetadataVersion === 1 ? 4 : 0) +
    (operation.userOperationHash !== undefined ? 2 : 0) +
    (
      operation.includedTransactionHash !== undefined ||
      operation.transactionHash !== undefined
        ? 1
        : 0
    )
  )
}

function operationSubmissionEvidenceScore(
  operation: SponsoredOperation
): number {
  if (
    operation.submissionMetadataVersion === 1 &&
    operation.userOperationHash !== undefined &&
    operation.signedUserOperation !== undefined
  ) {
    return 2
  }
  if (operation.userOperationHash !== undefined) return 1
  return 0
}

function operationResolutionEvidenceScore(
  operation: SponsoredOperation
): number {
  if (
    operation.status === 'confirmed' &&
    operation.transactionHashVerified === true
  ) {
    return 5
  }
  if (
    operation.status === 'execution-reverted' &&
    operation.transactionHashVerified === true
  ) {
    return 4
  }
  if (
    operation.status === 'expired' &&
    operation.submissionMetadataVersion === 1
  ) {
    return 3
  }
  if (
    operation.status === 'outcome-unknown' &&
    (
      operation.forcedLegacyUnlock === true ||
      operation.protocolNonceAdvanced === true ||
      operation.automaticRecoveryExpired === true
    )
  ) {
    return 2
  }
  return isSponsoredOperationTerminal(operation.status) ? 1 : 0
}

function mergeOperationRecord(
  current: SponsoredOperation,
  persisted: SponsoredOperation
): SponsoredOperation {
  const preferLiveCurrent =
    operationAbortControllers.has(current.id) &&
    !isSponsoredOperationTerminal(current.status)

  const currentEvidence = operationSubmissionEvidenceScore(current)
  const persistedEvidence = operationSubmissionEvidenceScore(persisted)
  const currentResolution = operationResolutionEvidenceScore(current)
  const persistedResolution = operationResolutionEvidenceScore(persisted)
  const preferred = preferLiveCurrent
    ? current
    : persistedEvidence > currentEvidence
    ? persisted
    : persistedEvidence < currentEvidence
      ? current
      : persistedResolution > currentResolution
        ? persisted
        : persistedResolution < currentResolution
          ? current
          : persisted.updatedAt > current.updatedAt
            ? persisted
            : persisted.updatedAt < current.updatedAt
              ? current
              : operationMergeScore(persisted) >
                  operationMergeScore(current)
                ? persisted
                : current
  const other = preferred === current ? persisted : current
  const preferredInclusionEvidenceRevision =
    sponsoredOperationInclusionEvidenceRevision(preferred)
  const otherInclusionEvidenceRevision =
    sponsoredOperationInclusionEvidenceRevision(other)
  const preferredInclusionEvidence =
    preferredInclusionEvidenceRevision > otherInclusionEvidenceRevision
      ? preferred
      : preferredInclusionEvidenceRevision < otherInclusionEvidenceRevision
        ? other
        : sponsoredOperationInclusionEvidenceTieBreakKey(preferred) >=
            sponsoredOperationInclusionEvidenceTieBreakKey(other)
          ? preferred
          : other
  const preferredWithoutDerivedFlags = { ...preferred }
  delete preferredWithoutDerivedFlags.laneReleasedAfterSuccessfulInclusion
  delete preferredWithoutDerivedFlags.transactionHashVerified
  return {
    ...preferredWithoutDerivedFlags,
    // Submission identity and canonical evidence are monotonic. Rehydrating a
    // stale whole-store snapshot must not erase them.
    userOperationHash:
      preferred.userOperationHash ?? other.userOperationHash,
    signedUserOperation:
      preferred.signedUserOperation ?? other.signedUserOperation,
    orderRequestV2:
      preferred.orderRequestV2 ?? other.orderRequestV2,
    protectionIntent: preferred.protectionIntent ?? other.protectionIntent,
    submissionMetadataVersion:
      preferred.submissionMetadataVersion ??
      other.submissionMetadataVersion,
    hashRecordedAt: preferred.hashRecordedAt ?? other.hashRecordedAt,
    automaticRecoveryStartedAt:
      preferred.automaticRecoveryStartedAt === undefined
        ? other.automaticRecoveryStartedAt
        : other.automaticRecoveryStartedAt === undefined
          ? preferred.automaticRecoveryStartedAt
          : Math.min(
              preferred.automaticRecoveryStartedAt,
              other.automaticRecoveryStartedAt
            ),
    lastAutomaticRecoveryAttemptAt: Math.max(
      preferred.lastAutomaticRecoveryAttemptAt ?? 0,
      other.lastAutomaticRecoveryAttemptAt ?? 0
    ) || undefined,
    automaticRecoveryAttemptCount: Math.max(
      preferred.automaticRecoveryAttemptCount ?? 0,
      other.automaticRecoveryAttemptCount ?? 0
    ) || undefined,
    automaticRecoveryExhaustedAt: Math.max(
      preferred.automaticRecoveryExhaustedAt ?? 0,
      other.automaticRecoveryExhaustedAt ?? 0
    ) || undefined,
    ...(
      preferred.automaticRecoveryExpired === true ||
      other.automaticRecoveryExpired === true
        ? { automaticRecoveryExpired: true as const }
        : {}
    ),
    includedTransactionHash:
      preferredInclusionEvidence.includedTransactionHash,
    includedBlockNumber:
      preferredInclusionEvidence.includedBlockNumber,
    includedBlockHash:
      preferredInclusionEvidence.includedBlockHash,
    inclusionObservedAt:
      preferredInclusionEvidence.inclusionObservedAt,
    inclusionEvidenceRevision:
      preferredInclusionEvidence.inclusionEvidenceRevision,
    ...(
      preferred.laneReleasedAfterSuccessfulInclusion === true ||
      other.laneReleasedAfterSuccessfulInclusion === true
        ? { laneReleasedAfterSuccessfulInclusion: true as const }
        : {}
    ),
    transactionHash:
      preferred.transactionHashVerified === true
        ? preferred.transactionHash
        : other.transactionHashVerified === true
          ? other.transactionHash
          : preferred.transactionHash,
    ...(
      preferred.transactionHashVerified === true ||
      other.transactionHashVerified === true
        ? { transactionHashVerified: true as const }
        : {}
    ),
    attentionRevision: Math.max(
      preferred.attentionRevision ?? 0,
      other.attentionRevision ?? 0
    ),
    acknowledgedAttentionRevision: Math.max(
      preferred.acknowledgedAttentionRevision ?? 0,
      other.acknowledgedAttentionRevision ?? 0
    ),
  }
}

function activeLanesForOperations(
  operations: SponsoredOperation[]
): Record<string, string> {
  const activeLanes: Record<string, string> = {}
  const blocking = operations
    .filter(isSponsoredOperationLaneBlocking)
    .sort((left, right) => right.updatedAt - left.updatedAt)
  for (const operation of blocking) {
    const key = laneKey(operation.accountAddress, operation.lane)
    activeLanes[key] ??= operation.id
  }
  return activeLanes
}

export function mergeSponsoredOperationState(
  persistedState: unknown,
  currentState: SponsoredOperationState
): SponsoredOperationState {
  const persisted = mergePersistedOperationStates(
    mergeExactOperationJournals(
      persistedOperationState(persistedState)
    ),
    mergeExactOperationJournals(durableJournalState())
  )
  const operationsById = new Map(
    currentState.operations.map((operation) => [operation.id, operation])
  )
  for (const persistedOperation of persisted.operations) {
    const currentOperation = operationsById.get(persistedOperation.id)
    operationsById.set(
      persistedOperation.id,
      currentOperation
        ? mergeOperationRecord(currentOperation, persistedOperation)
        : persistedOperation
    )
  }
  const operations = mergeExactOperationJournals({
    operations: [...operationsById.values()],
    activeLanes: {},
  }).operations
    .sort((left, right) => left.createdAt - right.createdAt)
  assertUniqueUserOperationHashes(operations)

  return {
    ...currentState,
    operations,
    activeLanes: activeLanesForOperations(operations),
  }
}

function mergePersistedOperationStates(
  incoming: PersistedSponsoredOperationState,
  existing: PersistedSponsoredOperationState
): PersistedSponsoredOperationState {
  const operationsById = new Map(
    incoming.operations.map((operation) => [operation.id, operation])
  )
  for (const existingOperation of existing.operations) {
    const incomingOperation = operationsById.get(existingOperation.id)
    operationsById.set(
      existingOperation.id,
      incomingOperation
        ? mergeOperationRecord(incomingOperation, existingOperation)
        : existingOperation
    )
  }
  const terminalCutoff = Date.now() - 24 * 60 * 60 * 1000
  const operations = [...operationsById.values()]
    .filter((operation) =>
      !isSponsoredOperationTerminal(operation.status) ||
      operation.updatedAt > terminalCutoff
    )
    .sort((left, right) => left.createdAt - right.createdAt)
  assertUniqueUserOperationHashes(operations)
  return {
    operations,
    activeLanes: activeLanesForOperations(operations),
  }
}

function parseStorageValue(
  value: string | null
): StorageValue<unknown> | undefined {
  if (value === null) return undefined
  try {
    const parsed: unknown = JSON.parse(value)
    if (!parsed || typeof parsed !== 'object') return undefined
    return parsed as StorageValue<unknown>
  } catch {
    return undefined
  }
}

interface SponsoredOperationJournalV1 {
  version: 1
  operation: SponsoredOperation
}

type SponsoredOperationResolutionStatus =
  | 'outcome-unknown'
  | 'expired'
  | 'execution-reverted'
  | 'confirmed'

interface SponsoredOperationResolutionV1 {
  version: 1
  operationId: string
  userOperationHash: Hex
  chainId: number
  accountAddress: Address
  lane: string
  status: SponsoredOperationResolutionStatus
  resolvedAt: number
  forcedLegacyUnlock?: true
  protocolNonceAdvanced?: true
  automaticRecoveryExpired?: true
  legacyInboxIdentity?: true
  transactionHash?: Hex
  transactionHashVerified?: true
  reason?: StableSponsorReason
  retryable?: boolean
  attentionRevision?: number
  acknowledgedAttentionRevision?: number
}

interface SponsoredOperationLaneReleaseV1 {
  version: 1
  operationId: string
  userOperationHash: Hex
  chainId: number
  accountAddress: Address
  lane: string
  transactionHash: Hex
  blockNumber?: string
  blockHash?: Hex
  releasedAt: number
}

interface SponsoredOperationLaneHeadV1 {
  version: 1
  chainId: number
  accountAddress: Address
  lane: string
  operations: {
    operationId: string
    userOperationHash: Hex
  }[]
}

function sponsoredOperationJournalKey(operationId: string): string {
  return `${SPONSORED_OPERATION_JOURNAL_PREFIX}${operationId}`
}

function sponsoredOperationResolutionKey(
  operation: {
    id: string
    userOperationHash: Hex
  },
  status: SponsoredOperationResolutionStatus
): string {
  return SPONSORED_OPERATION_RESOLUTION_PREFIX +
    `${encodeURIComponent(operation.id)}:` +
    `${operation.userOperationHash.toLowerCase()}:` +
    status
}

function sponsoredOperationLaneReleaseKey(operation: {
  id: string
  userOperationHash: Hex
}): string {
  return SPONSORED_OPERATION_LANE_RELEASE_PREFIX +
    `${encodeURIComponent(operation.id)}:` +
    operation.userOperationHash.toLowerCase()
}

function sponsoredOperationLaneHeadKey(input: {
  chainId: number
  accountAddress: Address
  lane: string
}): string {
  return SPONSORED_OPERATION_LANE_HEAD_PREFIX +
    `${input.chainId.toString()}:` +
    `${input.accountAddress.toLowerCase()}:` +
    encodeURIComponent(input.lane)
}

function parseOperationJournal(
  value: string | null
): SponsoredOperationJournalV1 | undefined {
  if (value === null) return undefined
  try {
    const parsed: unknown = JSON.parse(value)
    if (!parsed || typeof parsed !== 'object') return undefined
    const record = parsed as {
      version?: unknown
      operation?: unknown
    }
    if (
      typeof record.version === 'number' &&
      record.version > SPONSORED_OPERATION_STORAGE_VERSION
    ) {
      throw new Error(
        'A sponsored-operation journal was written by a newer app version'
      )
    }
    if (
      record.version !== 1 ||
      !record.operation ||
      typeof record.operation !== 'object'
    ) {
      return undefined
    }
    return {
      version: 1,
      operation: record.operation as SponsoredOperation,
    }
  } catch (error) {
    if (
      error instanceof Error &&
      error.message.includes('newer app version')
    ) {
      throw error
    }
    return undefined
  }
}

function readExactOperationJournal(
  operationId: string
): SponsoredOperation | undefined {
  const value = globalThis.localStorage.getItem(
    sponsoredOperationJournalKey(operationId)
  )
  const journal = parseOperationJournal(value)
  if (value !== null && journal === undefined) {
    throw new Error(
      'The sponsored-operation recovery journal is unreadable'
    )
  }
  if (journal && journal.operation.id !== operationId) {
    throw new Error(
      'The sponsored-operation recovery journal has a mismatched identity'
    )
  }
  return journal?.operation
}

const sponsoredOperationResolutionStatuses:
  SponsoredOperationResolutionStatus[] = [
    'outcome-unknown',
    'expired',
    'execution-reverted',
    'confirmed',
  ]

function sponsoredOperationResolutionStatus(
  operation: SponsoredOperation
): SponsoredOperationResolutionStatus | undefined {
  if (
    operation.userOperationHash === undefined ||
    operationResolutionEvidenceScore(operation) < 2
  ) {
    return undefined
  }
  return sponsoredOperationResolutionStatuses.includes(
      operation.status as SponsoredOperationResolutionStatus
    )
    ? operation.status as SponsoredOperationResolutionStatus
    : undefined
}

function parseSponsoredOperationResolution(
  value: string | null
): SponsoredOperationResolutionV1 | undefined {
  if (value === null) return undefined
  try {
    const parsed: unknown = JSON.parse(value)
    if (!parsed || typeof parsed !== 'object') return undefined
    const record = parsed as Record<string, unknown>
    if (
      typeof record.version === 'number' &&
      record.version > SPONSORED_OPERATION_STORAGE_VERSION
    ) {
      throw new Error(
        'A sponsored-operation resolution was written by a newer app version'
      )
    }
    if (
      record.version !== 1 ||
      typeof record.operationId !== 'string' ||
      typeof record.userOperationHash !== 'string' ||
      !record.userOperationHash.startsWith('0x') ||
      typeof record.chainId !== 'number' ||
      typeof record.accountAddress !== 'string' ||
      !record.accountAddress.startsWith('0x') ||
      typeof record.lane !== 'string' ||
      typeof record.status !== 'string' ||
      !sponsoredOperationResolutionStatuses.includes(
        record.status as SponsoredOperationResolutionStatus
      ) ||
      typeof record.resolvedAt !== 'number'
    ) {
      return undefined
    }
    return record as unknown as SponsoredOperationResolutionV1
  } catch (error) {
    if (
      error instanceof Error &&
      error.message.includes('newer app version')
    ) {
      throw error
    }
    return undefined
  }
}

interface SponsoredOperationIdentity {
  id: string
  userOperationHash: Hex
  chainId: number
  accountAddress: Address
  lane: string
}

function readExactOperationResolution(
  identity: SponsoredOperationIdentity
): SponsoredOperationResolutionV1 | undefined {
  let resolved: SponsoredOperationResolutionV1 | undefined
  for (const status of sponsoredOperationResolutionStatuses) {
    const key = sponsoredOperationResolutionKey(identity, status)
    const value = globalThis.localStorage.getItem(key)
    const resolution = parseSponsoredOperationResolution(value)
    if (value !== null && resolution === undefined) {
      throw new Error(
        'The sponsored-operation resolution tombstone is unreadable'
      )
    }
    if (!resolution) continue
    if (
      resolution.operationId !== identity.id ||
      resolution.userOperationHash.toLowerCase() !==
        identity.userOperationHash.toLowerCase() ||
      resolution.chainId !== identity.chainId ||
      resolution.accountAddress.toLowerCase() !==
        identity.accountAddress.toLowerCase() ||
      resolution.lane !== identity.lane ||
      resolution.status !== status
    ) {
      throw new Error(
        'The sponsored-operation resolution tombstone has a mismatched identity'
      )
    }
    if (
      !resolved ||
      sponsoredOperationResolutionStatuses.indexOf(status) >
        sponsoredOperationResolutionStatuses.indexOf(resolved.status)
    ) {
      resolved = resolution
    }
  }
  return resolved
}

function parseSponsoredOperationLaneRelease(
  value: string | null
): SponsoredOperationLaneReleaseV1 | undefined {
  if (value === null) return undefined
  try {
    const parsed: unknown = JSON.parse(value)
    if (!parsed || typeof parsed !== 'object') return undefined
    const record = parsed as Record<string, unknown>
    if (
      typeof record.version === 'number' &&
      record.version > SPONSORED_OPERATION_STORAGE_VERSION
    ) {
      throw new Error(
        'A sponsored-operation lane release was written by a newer app version'
      )
    }
    if (
      record.version !== 1 ||
      typeof record.operationId !== 'string' ||
      typeof record.userOperationHash !== 'string' ||
      !record.userOperationHash.startsWith('0x') ||
      typeof record.chainId !== 'number' ||
      typeof record.accountAddress !== 'string' ||
      !record.accountAddress.startsWith('0x') ||
      typeof record.lane !== 'string' ||
      typeof record.transactionHash !== 'string' ||
      !record.transactionHash.startsWith('0x') ||
      (
        record.blockNumber !== undefined &&
        typeof record.blockNumber !== 'string'
      ) ||
      (
        record.blockHash !== undefined &&
        (
          typeof record.blockHash !== 'string' ||
          !record.blockHash.startsWith('0x')
        )
      ) ||
      typeof record.releasedAt !== 'number'
    ) {
      return undefined
    }
    return record as unknown as SponsoredOperationLaneReleaseV1
  } catch (error) {
    if (
      error instanceof Error &&
      error.message.includes('newer app version')
    ) {
      throw error
    }
    return undefined
  }
}

function readExactSponsoredOperationLaneRelease(
  identity: SponsoredOperationIdentity
): SponsoredOperationLaneReleaseV1 | undefined {
  const value = globalThis.localStorage.getItem(
    sponsoredOperationLaneReleaseKey(identity)
  )
  const release = parseSponsoredOperationLaneRelease(value)
  if (value !== null && release === undefined) {
    throw new Error(
      'The sponsored-operation lane-release tombstone is unreadable'
    )
  }
  if (
    release &&
    (
      release.operationId !== identity.id ||
      release.userOperationHash.toLowerCase() !==
        identity.userOperationHash.toLowerCase() ||
      release.chainId !== identity.chainId ||
      release.accountAddress.toLowerCase() !==
        identity.accountAddress.toLowerCase() ||
      release.lane !== identity.lane
    )
  ) {
    throw new Error(
      'The sponsored-operation lane-release tombstone has a mismatched identity'
    )
  }
  return release
}

function writeSponsoredOperationLaneRelease(
  operation: SponsoredOperation & { userOperationHash: Hex },
  observation: SuccessfulSponsoredOperationInclusionObservation
): void {
  const key = sponsoredOperationLaneReleaseKey(operation)
  const existingValue = globalThis.localStorage.getItem(key)
  if (existingValue !== null) {
    const existing = readExactSponsoredOperationLaneRelease({
      id: operation.id,
      userOperationHash: operation.userOperationHash,
      chainId: operation.chainId,
      accountAddress: operation.accountAddress,
      lane: operation.lane,
    })
    if (!existing) {
      throw new Error(
        'The sponsored-operation lane-release tombstone is unreadable'
      )
    }
    return
  }
  globalThis.localStorage.setItem(
    key,
    JSON.stringify({
      version: 1,
      operationId: operation.id,
      userOperationHash: operation.userOperationHash,
      chainId: operation.chainId,
      accountAddress: operation.accountAddress,
      lane: operation.lane,
      transactionHash: observation.transactionHash,
      blockNumber: observation.blockNumber,
      blockHash: observation.blockHash,
      releasedAt: Date.now(),
    } satisfies SponsoredOperationLaneReleaseV1)
  )
}

function applyOperationResolution(
  operation: SponsoredOperation,
  resolution: SponsoredOperationResolutionV1
): SponsoredOperation {
  const operationWithoutVerifiedFlag = { ...operation }
  delete operationWithoutVerifiedFlag.transactionHashVerified
  return {
    ...operationWithoutVerifiedFlag,
    status: resolution.status,
    forcedLegacyUnlock: resolution.forcedLegacyUnlock,
    protocolNonceAdvanced: resolution.protocolNonceAdvanced,
    automaticRecoveryExpired: resolution.automaticRecoveryExpired,
    legacyInboxIdentity:
      resolution.legacyInboxIdentity ?? operation.legacyInboxIdentity,
    transactionHash:
      resolution.transactionHashVerified === true
        ? resolution.transactionHash
        : operation.transactionHashVerified === true
          ? operation.transactionHash
          : undefined,
    ...(
      resolution.transactionHashVerified === true ||
      operation.transactionHashVerified === true
        ? { transactionHashVerified: true as const }
        : {}
    ),
    reason: resolution.reason,
    retryable: resolution.retryable,
    updatedAt: Math.max(operation.updatedAt, resolution.resolvedAt),
    attentionRevision: Math.max(
      operation.attentionRevision ?? 0,
      resolution.attentionRevision ?? 0
    ),
    acknowledgedAttentionRevision: Math.max(
      operation.acknowledgedAttentionRevision ?? 0,
      resolution.acknowledgedAttentionRevision ?? 0
    ),
    statusTimestamps: {
      ...operation.statusTimestamps,
      [resolution.status]: Math.max(
        operation.statusTimestamps[resolution.status] ?? 0,
        resolution.resolvedAt
      ),
    },
  }
}

function writeSponsoredOperationResolution(
  operation: SponsoredOperation
): void {
  const status = sponsoredOperationResolutionStatus(operation)
  if (!status || operation.userOperationHash === undefined) return
  const key = sponsoredOperationResolutionKey(
    {
      id: operation.id,
      userOperationHash: operation.userOperationHash,
    },
    status
  )
  const existingValue = globalThis.localStorage.getItem(key)
  if (existingValue !== null) {
    const existing = parseSponsoredOperationResolution(existingValue)
    const existingMatches =
      existing?.operationId === operation.id &&
      existing.userOperationHash.toLowerCase() ===
        operation.userOperationHash.toLowerCase() &&
      existing.chainId === operation.chainId &&
      existing.accountAddress.toLowerCase() ===
        operation.accountAddress.toLowerCase() &&
      existing.lane === operation.lane &&
      existing.status === status
    if (!existingMatches) {
      throw new Error(
        'The sponsored-operation resolution tombstone is unreadable'
      )
    }
    return
  }
  globalThis.localStorage.setItem(
    key,
    JSON.stringify({
      version: 1,
      operationId: operation.id,
      userOperationHash: operation.userOperationHash,
      chainId: operation.chainId,
      accountAddress: operation.accountAddress,
      lane: operation.lane,
      status,
      resolvedAt:
        operation.statusTimestamps[status] ?? operation.updatedAt,
      ...(operation.forcedLegacyUnlock === true
        ? { forcedLegacyUnlock: true as const }
        : {}),
      ...(operation.protocolNonceAdvanced === true
        ? { protocolNonceAdvanced: true as const }
        : {}),
      ...(operation.automaticRecoveryExpired === true
        ? { automaticRecoveryExpired: true as const }
        : {}),
      ...(operation.legacyInboxIdentity === true
        ? { legacyInboxIdentity: true as const }
        : {}),
      ...(operation.transactionHashVerified === true
        ? {
            transactionHash: operation.transactionHash,
            transactionHashVerified: true as const,
          }
        : {}),
      reason: operation.reason,
      retryable: operation.retryable,
      attentionRevision: operation.attentionRevision,
      acknowledgedAttentionRevision:
        operation.acknowledgedAttentionRevision,
    } satisfies SponsoredOperationResolutionV1)
  )
}

function assertCompatibleOperationHashes(
  left: SponsoredOperation,
  right: SponsoredOperation
): void {
  if (
    left.userOperationHash !== undefined &&
    right.userOperationHash !== undefined &&
    left.userOperationHash.toLowerCase() !==
      right.userOperationHash.toLowerCase()
  ) {
    throw new Error(
      'Conflicting sponsored-operation hashes share one operation ID'
    )
  }
}

function assertUniqueUserOperationHashes(
  operations: SponsoredOperation[]
): void {
  const operationIdsByHash = new Map<string, string>()
  for (const operation of operations) {
    if (operation.userOperationHash === undefined) continue
    const normalizedHash = operation.userOperationHash.toLowerCase()
    const existingOperationId = operationIdsByHash.get(normalizedHash)
    if (
      existingOperationId !== undefined &&
      existingOperationId !== operation.id
    ) {
      throw new Error(
        'One sponsored UserOperation hash belongs to multiple operation records'
      )
    }
    operationIdsByHash.set(normalizedHash, operation.id)
  }
}

function readDurableOperation(
  identity: SponsoredOperationIdentity
): SponsoredOperation | undefined {
  const journalOperation = readExactOperationJournal(identity.id)
  if (
    journalOperation &&
    (
      journalOperation.userOperationHash?.toLowerCase() !==
        identity.userOperationHash.toLowerCase() ||
      !operationMatchesLane(journalOperation, identity)
    )
  ) {
    throw new Error(
      'The sponsored-operation recovery journal has a mismatched identity'
    )
  }
  const resolution = readExactOperationResolution(identity)
  const laneRelease = readExactSponsoredOperationLaneRelease(identity)
  if (!journalOperation) return undefined
  const resolvedOperation = resolution
    ? applyOperationResolution(journalOperation, resolution)
    : journalOperation
  return laneRelease
    ? {
        ...resolvedOperation,
        laneReleasedAfterSuccessfulInclusion: true,
      }
    : resolvedOperation
}

function mergeExactOperationJournals(
  state: PersistedSponsoredOperationState
): PersistedSponsoredOperationState {
  const operations = state.operations.map((operation) => {
    if (operation.userOperationHash === undefined) {
      const journalOperation = readExactOperationJournal(operation.id)
      if (!journalOperation) return operation
      assertCompatibleOperationHashes(operation, journalOperation)
      return mergeOperationRecord(operation, journalOperation)
    }
    const preSignJournal = readExactOperationJournal(operation.id)
    if (
      preSignJournal !== undefined &&
      preSignJournal.userOperationHash === undefined
    ) {
      if (
        (operation.orderRequestV2 === undefined && operation.protectionIntent === undefined) ||
        JSON.stringify(operation.orderRequestV2) !==
          JSON.stringify(preSignJournal.orderRequestV2) ||
        JSON.stringify(operation.protectionIntent) !== JSON.stringify(preSignJournal.protectionIntent) ||
        !operationMatchesLane(preSignJournal, operation)
      ) {
        throw new Error(
          'The sponsored-operation pre-sign journal has a mismatched identity'
        )
      }
      return mergeOperationRecord(operation, preSignJournal)
    }
    const durableOperation = readDurableOperation({
      id: operation.id,
      userOperationHash: operation.userOperationHash,
      chainId: operation.chainId,
      accountAddress: operation.accountAddress,
      lane: operation.lane,
    })
    if (!durableOperation) {
      const resolution = readExactOperationResolution({
        id: operation.id,
        userOperationHash: operation.userOperationHash,
        chainId: operation.chainId,
        accountAddress: operation.accountAddress,
        lane: operation.lane,
      })
      return resolution
        ? applyOperationResolution(operation, resolution)
        : operation
    }
    assertCompatibleOperationHashes(operation, durableOperation)
    return mergeOperationRecord(operation, durableOperation)
  })
  return {
    operations,
    activeLanes: activeLanesForOperations(operations),
  }
}

function parseSponsoredOperationLaneHead(
  value: string | null
): SponsoredOperationLaneHeadV1 | undefined {
  if (value === null) return undefined
  try {
    const parsed: unknown = JSON.parse(value)
    if (!parsed || typeof parsed !== 'object') return undefined
    const record = parsed as {
      version?: unknown
      chainId?: unknown
      accountAddress?: unknown
      lane?: unknown
      operations?: unknown
    }
    if (
      typeof record.version === 'number' &&
      record.version > SPONSORED_OPERATION_STORAGE_VERSION
    ) {
      throw new Error(
        'A sponsored-operation lane head was written by a newer app version'
      )
    }
    if (
      record.version !== 1 ||
      typeof record.chainId !== 'number' ||
      typeof record.accountAddress !== 'string' ||
      !record.accountAddress.startsWith('0x') ||
      typeof record.lane !== 'string' ||
      !Array.isArray(record.operations) ||
      record.operations.length === 0 ||
      record.operations.some((operation) => {
        if (!operation || typeof operation !== 'object') return true
        const candidate = operation as {
          operationId?: unknown
          userOperationHash?: unknown
        }
        return typeof candidate.operationId !== 'string' ||
          typeof candidate.userOperationHash !== 'string' ||
          !candidate.userOperationHash.startsWith('0x')
      })
    ) {
      return undefined
    }
    return {
      version: 1,
      chainId: record.chainId,
      accountAddress: record.accountAddress as Address,
      lane: record.lane,
      operations: record.operations as SponsoredOperationLaneHeadV1['operations'],
    }
  } catch (error) {
    if (
      error instanceof Error &&
      error.message.includes('newer app version')
    ) {
      throw error
    }
    return undefined
  }
}

function writeSponsoredOperationLaneHead(
  operation: SponsoredOperation,
  options: {
    rejectOtherLaneBlocking?: boolean
  } = {}
): void {
  if (operation.userOperationHash === undefined) {
    throw new Error(
      'Cannot journal a sponsored-operation lane without its operation hash'
    )
  }
  const key = sponsoredOperationLaneHeadKey(operation)
  const existingValue = globalThis.localStorage.getItem(key)
  const existingHead = parseSponsoredOperationLaneHead(existingValue)
  if (
    existingValue !== null &&
    (
      existingHead?.chainId !== operation.chainId ||
      existingHead.accountAddress.toLowerCase() !==
        operation.accountAddress.toLowerCase() ||
      existingHead.lane !== operation.lane
    )
  ) {
    throw new Error(
      'The sponsored-operation lane recovery head is unreadable'
    )
  }

  const operations = new Map<string, {
    operationId: string
    userOperationHash: Hex
  }>()
  const operationIdsByHash = new Map<string, string>()
  for (const entry of existingHead?.operations ?? []) {
    const journalOperation = readDurableOperation({
      id: entry.operationId,
      userOperationHash: entry.userOperationHash,
      chainId: operation.chainId,
      accountAddress: operation.accountAddress,
      lane: operation.lane,
    })
    if (
      journalOperation?.id !== entry.operationId ||
      journalOperation.userOperationHash?.toLowerCase() !==
        entry.userOperationHash.toLowerCase() ||
      journalOperation.chainId !== operation.chainId ||
      journalOperation.accountAddress.toLowerCase() !==
        operation.accountAddress.toLowerCase() ||
      journalOperation.lane !== operation.lane
    ) {
      throw new Error(
        'The sponsored-operation lane recovery journal is unreadable'
      )
    }
    const normalizedHash = entry.userOperationHash.toLowerCase()
    const operationIdForHash = operationIdsByHash.get(normalizedHash)
    if (
      (operationIdForHash !== undefined &&
        operationIdForHash !== entry.operationId) ||
      (
        entry.operationId !== operation.id &&
        normalizedHash === operation.userOperationHash.toLowerCase()
      )
    ) {
      throw new Error(
        'One sponsored UserOperation hash belongs to multiple operation records'
      )
    }
    operationIdsByHash.set(normalizedHash, entry.operationId)
    if (!isSponsoredOperationTerminal(journalOperation.status)) {
      if (
        options.rejectOtherLaneBlocking === true &&
        entry.operationId !== operation.id &&
        isSponsoredOperationLaneBlocking(journalOperation)
      ) {
        throw new SponsoredOperationLockedError(entry.operationId)
      }
      operations.set(entry.operationId, entry)
    }
  }
  const operationHashOwner = operationIdsByHash.get(
    operation.userOperationHash.toLowerCase()
  )
  if (
    operationHashOwner !== undefined &&
    operationHashOwner !== operation.id
  ) {
    throw new Error(
      'One sponsored UserOperation hash belongs to multiple operation records'
    )
  }
  operations.set(operation.id, {
    operationId: operation.id,
    userOperationHash: operation.userOperationHash,
  })

  globalThis.localStorage.setItem(
    key,
    JSON.stringify({
      version: 1,
      chainId: operation.chainId,
      accountAddress: operation.accountAddress,
      lane: operation.lane,
      operations: [...operations.values()],
    } satisfies SponsoredOperationLaneHeadV1)
  )
}

function operationIsCurrentLaneHead(
  operation: SponsoredOperation
): boolean {
  try {
    const head = parseSponsoredOperationLaneHead(
      globalThis.localStorage.getItem(
        sponsoredOperationLaneHeadKey(operation)
      )
    )
    return head?.operations.some(
      (entry) => entry.operationId === operation.id
    ) === true
  } catch {
    // Retain the operation journal if its lane head cannot be understood.
    // Removing the only recoverable record would turn corruption into an
    // unsafe retry.
    return true
  }
}

function operationJournalEntries(): {
  key: string
  journal: SponsoredOperationJournalV1
}[] {
  const journals: {
    key: string
    journal: SponsoredOperationJournalV1
  }[] = []
  for (let index = 0; index < globalThis.localStorage.length; index += 1) {
    const key = globalThis.localStorage.key(index)
    if (!key?.startsWith(SPONSORED_OPERATION_JOURNAL_PREFIX)) continue
    const journal = parseOperationJournal(
      globalThis.localStorage.getItem(key)
    )
    if (journal) journals.push({ key, journal })
  }
  return journals
}

function durableJournalState(): PersistedSponsoredOperationState {
  return {
    operations: operationJournalEntries()
      .map(({ journal }) => journal.operation),
    activeLanes: {},
  }
}

function writeExactOperationJournal(
  operation: SponsoredOperation
): SponsoredOperation | undefined {
  // Enumeration is only a discovery aid. Every identity being written must
  // first be read directly so a concurrent key insertion cannot hide it.
  const existing = readExactOperationJournal(operation.id)
  if (
    operation.userOperationHash === undefined &&
    existing === undefined &&
    operation.orderRequestV2 === undefined && operation.protectionIntent === undefined
  ) {
    return undefined
  }
  if (existing) assertCompatibleOperationHashes(operation, existing)

  let durableOperation = existing
    ? mergeOperationRecord(operation, existing)
    : operation
  if (durableOperation.userOperationHash !== undefined) {
    const resolution = readExactOperationResolution({
      id: durableOperation.id,
      userOperationHash: durableOperation.userOperationHash,
      chainId: durableOperation.chainId,
      accountAddress: durableOperation.accountAddress,
      lane: durableOperation.lane,
    })
    if (resolution) {
      durableOperation = applyOperationResolution(
        durableOperation,
        resolution
      )
    }
  }
  // Resolution evidence is append-only and written before the mutable
  // journal. An unrelated tab may still complete a stale journal write, but
  // mergeOperationRecord makes successful-inclusion lane release monotonic.
  writeSponsoredOperationResolution(durableOperation)
  globalThis.localStorage.setItem(
    sponsoredOperationJournalKey(operation.id),
    JSON.stringify({
      version: 1,
      operation: durableOperation,
    } satisfies SponsoredOperationJournalV1)
  )
  return durableOperation
}

function writeOperationJournals(
  operations: SponsoredOperation[]
): void {
  const retainedIds = new Set(operations.map((operation) => operation.id))
  const existingEntries = operationJournalEntries()
  for (const operation of operations) {
    writeExactOperationJournal(operation)
  }
  for (const { key, journal } of existingEntries) {
    if (
      !retainedIds.has(journal.operation.id) &&
      isSponsoredOperationTerminal(journal.operation.status) &&
      journal.operation.updatedAt <=
        Date.now() - 24 * 60 * 60 * 1000 &&
      journal.operation.legacyInboxIdentity !== true &&
      journal.operation.legacyManualUnlockEligible !== true &&
      journal.operation.forcedLegacyUnlock !== true &&
      !operationIsCurrentLaneHead(journal.operation)
    ) {
      globalThis.localStorage.removeItem(key)
    }
  }
}

function operationMatchesLane(
  operation: SponsoredOperation,
  input: {
    chainId: number
    accountAddress: Address
    lane: string
  }
): boolean {
  return operation.chainId === input.chainId &&
    operation.accountAddress.toLowerCase() ===
      input.accountAddress.toLowerCase() &&
    operation.lane === input.lane
}

/**
 * Publishes one conservative lane head while the caller holds the lane's
 * browser-wide Web Lock. Candidate journals may not exist yet: the guard is
 * intentionally written first so a crash between the head and journal writes
 * fails closed.
 */
function publishSponsoredOperationLaneHeadBeforeJournals(
  input: {
    chainId: number
    accountAddress: Address
    lane: string
  },
  candidates: SponsoredOperation[]
): SponsoredOperation[] {
  const candidatesById = new Map<string, SponsoredOperation>()
  const addCandidate = (candidate: SponsoredOperation) => {
    if (!operationMatchesLane(candidate, input)) return
    const existing = candidatesById.get(candidate.id)
    if (
      existing?.userOperationHash !== undefined &&
      candidate.userOperationHash !== undefined &&
      existing.userOperationHash.toLowerCase() !==
        candidate.userOperationHash.toLowerCase()
    ) {
      throw new Error(
        'Conflicting sponsored-operation hashes share one operation ID'
      )
    }
    candidatesById.set(
      candidate.id,
      existing ? mergeOperationRecord(existing, candidate) : candidate
    )
  }
  for (const candidate of candidates) addCandidate(candidate)

  const key = sponsoredOperationLaneHeadKey(input)
  const existingValue = globalThis.localStorage.getItem(key)
  const existingHead = parseSponsoredOperationLaneHead(existingValue)
  if (
    existingValue !== null &&
    (
      existingHead?.chainId !== input.chainId ||
      existingHead.accountAddress.toLowerCase() !==
        input.accountAddress.toLowerCase() ||
      existingHead.lane !== input.lane
    )
  ) {
    throw new Error(
      'The sponsored-operation lane recovery head is unreadable'
    )
  }

  for (const entry of existingHead?.operations ?? []) {
    const journalOperation = readDurableOperation({
      id: entry.operationId,
      userOperationHash: entry.userOperationHash,
      chainId: input.chainId,
      accountAddress: input.accountAddress,
      lane: input.lane,
    })
    const snapshotOperation = candidatesById.get(entry.operationId)
    if (
      snapshotOperation?.userOperationHash !== undefined &&
      snapshotOperation.userOperationHash.toLowerCase() !==
        entry.userOperationHash.toLowerCase()
    ) {
      throw new Error(
        'The sponsored-operation lane recovery snapshot conflicts with its head'
      )
    }
    const journalMatches = journalOperation?.id === entry.operationId &&
      journalOperation.userOperationHash?.toLowerCase() ===
        entry.userOperationHash.toLowerCase() &&
      operationMatchesLane(journalOperation, input)
    const snapshotMatches =
      snapshotOperation?.id === entry.operationId &&
      snapshotOperation.userOperationHash?.toLowerCase() ===
        entry.userOperationHash.toLowerCase() &&
      operationMatchesLane(snapshotOperation, input)
    if (!journalMatches && !snapshotMatches) {
      throw new Error(
        'The sponsored-operation lane recovery journal is unreadable'
      )
    }
    if (journalMatches && snapshotMatches) {
      addCandidate(
        mergeOperationRecord(snapshotOperation, journalOperation)
      )
    } else if (journalMatches) {
      addCandidate(journalOperation)
    } else if (snapshotOperation) {
      addCandidate(snapshotOperation)
    } else {
      throw new Error(
        'The sponsored-operation lane recovery snapshot is unreadable'
      )
    }
  }

  const mergedCandidates = [...candidatesById.values()]
  assertUniqueUserOperationHashes(mergedCandidates)
  const guardedOperations = mergedCandidates
    .filter((
      operation
    ): operation is SponsoredOperation & { userOperationHash: Hex } =>
      operation.userOperationHash !== undefined &&
        !isSponsoredOperationTerminal(operation.status)
    )
    .map((operation) => ({
      operationId: operation.id,
      userOperationHash: operation.userOperationHash,
    }))

  if (guardedOperations.length > 0) {
    globalThis.localStorage.setItem(
      key,
      JSON.stringify({
        version: 1,
        chainId: input.chainId,
        accountAddress: input.accountAddress,
        lane: input.lane,
        operations: guardedOperations,
      } satisfies SponsoredOperationLaneHeadV1)
    )
  }

  return mergedCandidates
}

function hasDurableOperationJournal(
  operation: SponsoredOperation,
  userOperationHash: Hex,
  requireSubmissionMetadata: boolean,
  requireOnlyLaneBlocker = false
): boolean {
  const journalOperation = readDurableOperation({
    id: operation.id,
    userOperationHash,
    chainId: operation.chainId,
    accountAddress: operation.accountAddress,
    lane: operation.lane,
  })
  const laneHead = parseSponsoredOperationLaneHead(
    globalThis.localStorage.getItem(
      sponsoredOperationLaneHeadKey(operation)
    )
  )
  if (
    journalOperation === undefined ||
    journalOperation.userOperationHash?.toLowerCase() !==
      userOperationHash.toLowerCase() ||
    isSponsoredOperationTerminal(journalOperation.status) ||
    journalOperation.chainId !== operation.chainId ||
    journalOperation.accountAddress.toLowerCase() !==
      operation.accountAddress.toLowerCase() ||
    journalOperation.lane !== operation.lane ||
    (
      requireSubmissionMetadata &&
      (
        journalOperation.submissionMetadataVersion !== 1 ||
        journalOperation.signedUserOperation === undefined ||
        operation.submissionMetadataVersion !== 1 ||
        operation.signedUserOperation === undefined ||
        JSON.stringify(journalOperation.signedUserOperation) !==
          JSON.stringify(operation.signedUserOperation)
      )
    ) ||
    laneHead?.chainId !== operation.chainId ||
    laneHead.accountAddress.toLowerCase() !==
      operation.accountAddress.toLowerCase() ||
    laneHead.lane !== operation.lane
  ) {
    return false
  }

  const durableHeadOperations: SponsoredOperation[] = []
  const operationIdsByHash = new Map<string, string>()
  let containsOperation = false
  for (const entry of laneHead.operations) {
    const durableHeadOperation = readDurableOperation({
      id: entry.operationId,
      userOperationHash: entry.userOperationHash,
      chainId: operation.chainId,
      accountAddress: operation.accountAddress,
      lane: operation.lane,
    })
    if (
      durableHeadOperation?.id !== entry.operationId ||
      durableHeadOperation.userOperationHash?.toLowerCase() !==
        entry.userOperationHash.toLowerCase() ||
      !operationMatchesLane(durableHeadOperation, operation)
    ) {
      return false
    }
    const normalizedHash = entry.userOperationHash.toLowerCase()
    const existingOperationId = operationIdsByHash.get(normalizedHash)
    if (
      existingOperationId !== undefined &&
      existingOperationId !== entry.operationId
    ) {
      return false
    }
    operationIdsByHash.set(normalizedHash, entry.operationId)
    durableHeadOperations.push(durableHeadOperation)
    if (
      entry.operationId === operation.id &&
      normalizedHash === userOperationHash.toLowerCase()
    ) {
      containsOperation = true
    }
  }
  if (!containsOperation) return false

  const laneBlockers = durableHeadOperations.filter(
    isSponsoredOperationLaneBlocking
  )
  if (laneBlockers.length > 1) return false
  if (
    requireOnlyLaneBlocker &&
    (
      laneBlockers.length !== 1 ||
      laneBlockers[0]?.id !== operation.id ||
      !storedSnapshotHasNoCompetingLaneOperation(operation)
    )
  ) {
    return false
  }
  return true
}

function storedSnapshotHasNoCompetingLaneOperation(
  operation: SponsoredOperation
): boolean {
  const storedValue = globalThis.localStorage.getItem(
    SPONSORED_OPERATION_STORAGE_NAME
  )
  if (storedValue === null) return true
  const parsed = parseStorageValue(storedValue)
  if (!parsed) return false
  const rawVersion = (
    parsed as StorageValue<unknown> & { version?: unknown }
  ).version
  if (rawVersion !== undefined && typeof rawVersion !== 'number') {
    return false
  }
  const persistedVersion = rawVersion ?? 0
  const rawState = persistedOperationState(parsed.state)
  const isDurablyResolved = (candidate: SponsoredOperation): boolean => {
    if (candidate.userOperationHash === undefined) return false
    return readExactOperationResolution({
      id: candidate.id,
      userOperationHash: candidate.userOperationHash,
      chainId: candidate.chainId,
      accountAddress: candidate.accountAddress,
      lane: candidate.lane,
    }) !== undefined
  }
  const isDurablyLaneReleased = (
    candidate: SponsoredOperation
  ): boolean => {
    if (candidate.laneReleasedAfterSuccessfulInclusion === true) return true
    if (candidate.userOperationHash === undefined) return false
    try {
      return readDurableOperation({
        id: candidate.id,
        userOperationHash: candidate.userOperationHash,
        chainId: candidate.chainId,
        accountAddress: candidate.accountAddress,
        lane: candidate.lane,
      })?.laneReleasedAfterSuccessfulInclusion === true
    } catch {
      return false
    }
  }
  const rawConflict = rawState.operations.some((candidate) => {
    if (!operationMatchesLane(candidate, operation)) return false
    if (candidate.id === operation.id) {
      return candidate.userOperationHash !== undefined &&
        candidate.userOperationHash.toLowerCase() !==
          operation.userOperationHash?.toLowerCase()
    }
    if (
      candidate.userOperationHash?.toLowerCase() ===
        operation.userOperationHash?.toLowerCase()
    ) {
      return true
    }
    if (isDurablyResolved(candidate)) return false
    if (isDurablyLaneReleased(candidate)) return false
    return (
      persistedVersion < SPONSORED_OPERATION_STORAGE_VERSION &&
      candidate.userOperationHash !== undefined
    ) || isSponsoredOperationLaneBlocking(candidate)
  })
  if (rawConflict) return false
  const persisted = migrateSponsoredOperationState(
    parsed.state,
    persistedVersion
  )
  return !persisted.operations.some((candidate) => {
    if (
      candidate.id === operation.id ||
      !operationMatchesLane(candidate, operation)
    ) {
      return false
    }
    if (
      candidate.userOperationHash?.toLowerCase() ===
        operation.userOperationHash?.toLowerCase()
    ) {
      return true
    }
    return !isDurablyResolved(candidate) &&
      !isDurablyLaneReleased(candidate) &&
      isSponsoredOperationLaneBlocking(candidate)
  })
}

function hasExactSponsoredOperationLaneHead(
  operation: SponsoredOperation
): boolean {
  if (operation.userOperationHash === undefined) return false
  const laneHead = parseSponsoredOperationLaneHead(
    globalThis.localStorage.getItem(
      sponsoredOperationLaneHeadKey(operation)
    )
  )
  return laneHead?.chainId === operation.chainId &&
    laneHead.accountAddress.toLowerCase() ===
      operation.accountAddress.toLowerCase() &&
    laneHead.lane === operation.lane &&
    laneHead.operations.some((entry) =>
      entry.operationId === operation.id &&
      entry.userOperationHash.toLowerCase() ===
        operation.userOperationHash?.toLowerCase()
    )
}

function anotherOperationUsesUserOperationHash(
  operationId: string,
  userOperationHash: Hex
): boolean {
  const normalizedHash = userOperationHash.toLowerCase()
  if (useSponsoredOperationStore.getState().operations.some((candidate) =>
    candidate.id !== operationId &&
    candidate.userOperationHash?.toLowerCase() === normalizedHash
  )) {
    return true
  }

  try {
    for (let index = 0; index < globalThis.localStorage.length; index += 1) {
      const key = globalThis.localStorage.key(index)
      if (!key) continue
      if (
        key.startsWith(SPONSORED_OPERATION_LANE_RELEASE_PREFIX) &&
        key.endsWith(`:${normalizedHash}`) &&
        key !== sponsoredOperationLaneReleaseKey({
          id: operationId,
          userOperationHash,
        })
      ) {
        return true
      }
      if (key.startsWith(SPONSORED_OPERATION_JOURNAL_PREFIX)) {
        const value = globalThis.localStorage.getItem(key)
        const journal = parseOperationJournal(value)
        if (value !== null && journal === undefined) return true
        if (
          journal?.operation.id !== operationId &&
          journal?.operation.userOperationHash?.toLowerCase() === normalizedHash
        ) {
          return true
        }
        continue
      }
      if (key.startsWith(SPONSORED_OPERATION_LANE_HEAD_PREFIX)) {
        const value = globalThis.localStorage.getItem(key)
        const head = parseSponsoredOperationLaneHead(value)
        if (value !== null && head === undefined) return true
        if (head?.operations.some((entry) =>
          entry.operationId !== operationId &&
          entry.userOperationHash.toLowerCase() === normalizedHash
        )) {
          return true
        }
      }
    }

    const storedValue = globalThis.localStorage.getItem(
      SPONSORED_OPERATION_STORAGE_NAME
    )
    if (storedValue === null) return false
    const stored = parseStorageValue(storedValue)
    if (!stored) return true
    return persistedOperationState(stored.state).operations.some((candidate) =>
      candidate.id !== operationId &&
      candidate.userOperationHash?.toLowerCase() === normalizedHash
    )
  } catch {
    // Unreadable or newer recovery state must fail closed before submission.
    return true
  }
}

let sponsoredOperationPersistenceBlockedRevision = 0
const sponsoredOperationSubmissionRevisions = new Map<string, number>()

const sponsoredOperationStorage:
  PersistStorage<PersistedSponsoredOperationState> = {
    getItem: (name) => {
      const rawValue = globalThis.localStorage.getItem(name)
      const parsed = parseStorageValue(rawValue)
      if (!parsed) return null

      const rawVersion = (
        parsed as StorageValue<unknown> & { version?: unknown }
      ).version
      if (rawVersion !== undefined && typeof rawVersion !== 'number') {
        throw new Error(
          'The sponsored-operation activity store has an invalid version'
        )
      }
      const persistedVersion = rawVersion ?? 0
      if (persistedVersion > SPONSORED_OPERATION_STORAGE_VERSION) {
        throw new Error(
          'The sponsored-operation store was written by a newer app version'
        )
      }
      // Hydration is deliberately read-only with respect to lane heads. A
      // storage event can run in any tab, so publishing a guard here could
      // overwrite the verified head of a live submission.
      return parsed as StorageValue<PersistedSponsoredOperationState>
    },
    setItem: (name, value) => {
      const incoming = persistedOperationState(value.state)
      const stored = parseStorageValue(globalThis.localStorage.getItem(name))
      if (
        typeof stored?.version === 'number' &&
        stored.version > SPONSORED_OPERATION_STORAGE_VERSION
      ) {
        throw new Error(
          'A newer sponsored-operation storage version is already active'
        )
      }
      const storedState = mergeExactOperationJournals(
        stored
          ? migrateSponsoredOperationState(
              stored.state,
              stored.version ?? 0
            )
          : { operations: [], activeLanes: {} }
      )
      const existing = mergePersistedOperationStates(
        storedState,
        mergeExactOperationJournals(durableJournalState())
      )
      const merged = mergePersistedOperationStates(
        mergeExactOperationJournals(incoming),
        existing
      )
      // Lane heads have a stricter single-writer protocol: locked restore
      // publishes version-0 migration guards, while new submissions mutate a
      // head only while holding that same lane's Web Lock.
      // Generic persistence may update per-ID journals, but must never race
      // those lane-head writers or mutate the legacy whole-store inbox.
      const guardedOperationIds = new Set(
        merged.operations
          .filter((operation) =>
            operation.userOperationHash !== undefined &&
            !isSponsoredOperationTerminal(operation.status) &&
            hasExactSponsoredOperationLaneHead(operation)
          )
          .map((operation) => operation.id)
      )
      const hasUnguardedSubmission = merged.operations.some((operation) =>
        operation.userOperationHash !== undefined &&
        !isSponsoredOperationTerminal(operation.status) &&
        !guardedOperationIds.has(operation.id)
      )
      if (hasUnguardedSubmission) {
        sponsoredOperationPersistenceBlockedRevision += 1
        // Zustand invokes setItem after an unlocked version-0 hydration.
        // Leave that snapshot at version 0 until locked restore publishes its
        // lane guards. Guarded operations may still advance their own
        // per-ID journals, but no headless submission becomes durable here.
        // The revision also makes an active pre-send persistence barrier fail.
        writeOperationJournals(
          merged.operations.filter((operation) =>
            operation.userOperationHash === undefined ||
            isSponsoredOperationTerminal(operation.status) ||
            guardedOperationIds.has(operation.id)
          )
        )
        return
      }
      writeOperationJournals(merged.operations)
      // This whole-store key is a read-only legacy inbox. Version-0 tabs can
      // write it without participating in our Web Locks, and localStorage has
      // no compare-and-swap primitive: any current tab that performed a
      // get/merge/set here could erase a legacy operation written between its
      // read and write. Version 1 therefore persists only directly addressed
      // per-operation journals and lane heads. Keeping the legacy key intact
      // also lets every pre-send restore observe late rollout evidence.
    },
    // The legacy inbox is read-only for this app version, including through
    // Zustand's optional clearStorage API. Deleting it could erase the only
    // evidence produced by a still-open version-0 tab.
    removeItem: (name) => {
      void name
    },
  }

export const useSponsoredOperationStore = create<SponsoredOperationState>()(
  devtools(
    persist(
      (set, get) => ({
        operations: [],
        activeLanes: {},

        beginOperation: (input) => {
          const lane = input.lane ?? DEFAULT_SPONSORED_OPERATION_LANE
          const key = laneKey(input.accountAddress, lane)
          const activeOperationId = get().activeLanes[key]
          if (activeOperationId) {
            throw new SponsoredOperationLockedError(activeOperationId)
          }

          const now = Date.now()
          const operation: SponsoredOperation = {
            ...input,
            lane,
            status: 'building',
            sponsorshipAccepted: false,
            retryCount: 0,
            createdAt: now,
            updatedAt: now,
            attentionRevision: 0,
            statusTimestamps: {
              building: now,
            },
          }

          set((state) => ({
            operations: [...state.operations, operation],
            activeLanes: {
              ...state.activeLanes,
              [key]: input.id,
            },
          }))
        },

        transition: (id, status) => {
          const now = Date.now()
          set((state) => {
            const operations = updateOperation(
              state.operations,
              id,
              (operation) => {
                if (
                  isSponsoredOperationTerminal(operation.status) &&
                  status !== 'confirmed'
                ) {
                  return operation
                }
                return {
                  ...operation,
                  status,
                  sponsorshipAccepted:
                    operation.sponsorshipAccepted ||
                    status === 'awaiting-signature' ||
                    status === 'submitting' ||
                    status === 'confirming' ||
                    status === 'confirmed',
                  ...(status === 'confirmed'
                    ? { reason: undefined, retryable: undefined }
                    : {}),
                  updatedAt: now,
                  attentionRevision: transitionAttentionRevision(
                    operation,
                    status
                  ),
                  statusTimestamps: {
                    ...operation.statusTimestamps,
                    [status]: now,
                  },
                }
              }
            )
            return {
              operations,
              ...(isSponsoredOperationTerminal(status)
                ? { activeLanes: activeLanesForOperations(operations) }
                : {}),
            }
          })
        },

        recordUserOperationHash: (id, hash, metadata) => {
          const currentOperation = get().operations.find(
            (operation) => operation.id === id
          )
          if (
            !currentOperation ||
            isSponsoredOperationTerminal(currentOperation.status) ||
            currentOperation.userOperationHash !== undefined ||
            anotherOperationUsesUserOperationHash(id, hash) ||
            get().operations.some((operation) =>
              operation.id !== currentOperation.id &&
              operationMatchesLane(operation, currentOperation) &&
              isSponsoredOperationLaneBlocking(operation)
            )
          ) {
            return false
          }

          const now = Date.now()
          const signedUserOperation = metadata
            ? persistManagedUserOperation(metadata.signedUserOperation)
            : undefined
          const pendingOperation: SponsoredOperation = {
            ...currentOperation,
            // Publish the first recoverable submission identity before the
            // per-operation journal. A crash between those writes leaves an
            // unreadable guard, which safely blocks resubmission.
            status: 'submitting',
            sponsorshipAccepted: true,
            userOperationHash: hash,
            ...(signedUserOperation
              ? {
                  signedUserOperation,
                  submissionMetadataVersion: 1 as const,
                }
              : {}),
            hashRecordedAt: now,
            updatedAt: now,
            statusTimestamps: {
              ...currentOperation.statusTimestamps,
              submitting: now,
            },
          }
          let retainSubmissionRevision = false
          const requiresSubmissionRevision =
            operationAbortControllers.has(id)
          try {
            const persistenceRevision =
              sponsoredOperationPersistenceBlockedRevision
            writeSponsoredOperationLaneHead(pendingOperation, {
              rejectOtherLaneBlocking: true,
            })
            set((state) => ({
              operations: updateOperation(
                state.operations,
                id,
                (operation) => {
                  if (
                    isSponsoredOperationTerminal(operation.status) ||
                    operation.userOperationHash !== undefined
                  ) {
                    return operation
                  }
                  return {
                    ...operation,
                    status: pendingOperation.status,
                    sponsorshipAccepted: true,
                    userOperationHash: hash,
                    ...(signedUserOperation
                      ? {
                          signedUserOperation,
                          submissionMetadataVersion: 1 as const,
                        }
                      : {}),
                    hashRecordedAt: now,
                    updatedAt: now,
                    statusTimestamps: {
                      ...operation.statusTimestamps,
                      submitting: now,
                    },
                  }
                }
              ),
            }))
            if (
              sponsoredOperationPersistenceBlockedRevision !==
                persistenceRevision
            ) {
              return false
            }
            const recordedOperation = get().operations.find(
              (operation) => operation.id === id
            )
            if (
              recordedOperation?.userOperationHash?.toLowerCase() !==
                hash.toLowerCase()
            ) {
              return false
            }
            const isDurable = hasDurableOperationJournal(
              recordedOperation,
              hash,
              metadata !== undefined,
              true
            )
            if (isDurable && requiresSubmissionRevision) {
              sponsoredOperationSubmissionRevisions.set(
                recordedOperation.id,
                sponsoredOperationPersistenceBlockedRevision
              )
              retainSubmissionRevision = true
            }
            return isDurable
          } catch {
            return false
          } finally {
            if (requiresSubmissionRevision && !retainSubmissionRevision) {
              sponsoredOperationSubmissionRevisions.delete(id)
            }
          }
        },

        recordObservedInclusion: (id, observation) => {
          const currentOperation = get().operations.find(
            (operation) => operation.id === id
          )
          if (
            currentOperation?.userOperationHash === undefined ||
            (
              currentOperation.transactionHashVerified === true &&
              currentOperation.transactionHash?.toLowerCase() !==
                observation.transactionHash.toLowerCase()
            )
          ) {
            return false
          }
          if (isSponsoredOperationTerminal(currentOperation.status)) {
            return currentOperation.status === 'confirmed' &&
              currentOperation.transactionHashVerified === true &&
              currentOperation.transactionHash?.toLowerCase() ===
                observation.transactionHash.toLowerCase()
          }

          let durableBefore: SponsoredOperation | undefined
          try {
            durableBefore = readDurableOperation({
              id: currentOperation.id,
              userOperationHash: currentOperation.userOperationHash,
              chainId: currentOperation.chainId,
              accountAddress: currentOperation.accountAddress,
              lane: currentOperation.lane,
            })
          } catch {
            return false
          }
          if (
            currentOperation.status === 'confirming' &&
            durableBefore?.status === 'confirming' &&
            operationMatchesInclusionObservation(
              currentOperation,
              observation
            ) &&
            operationMatchesInclusionObservation(
              durableBefore,
              observation
            ) &&
            currentOperation.reason === undefined &&
            currentOperation.retryable === undefined &&
            hasDurableOperationJournal(
              currentOperation,
              currentOperation.userOperationHash,
              false
            )
          ) {
            return true
          }

          const nextEvidenceRevision = Math.max(
            sponsoredOperationInclusionEvidenceRevision(currentOperation),
            sponsoredOperationInclusionEvidenceRevision(durableBefore)
          ) + 1
          const now = Math.max(
            Date.now(),
            currentOperation.updatedAt + 1,
            (durableBefore?.updatedAt ?? 0) + 1
          )
          try {
            set((state) => ({
              operations: updateOperation(
                state.operations,
                id,
                (operation) => {
                  if (
                    isSponsoredOperationTerminal(operation.status) ||
                    operation.userOperationHash === undefined ||
                    (
                      operation.transactionHashVerified === true &&
                      operation.transactionHash?.toLowerCase() !==
                        observation.transactionHash.toLowerCase()
                    )
                  ) {
                    return operation
                  }
                  const sameTransaction =
                    operation.includedTransactionHash?.toLowerCase() ===
                      observation.transactionHash.toLowerCase()
                  return {
                    ...operation,
                    status: 'confirming',
                    sponsorshipAccepted: true,
                    includedTransactionHash: observation.transactionHash,
                    includedBlockNumber:
                      observation.blockNumber ??
                      (sameTransaction
                        ? operation.includedBlockNumber
                        : undefined),
                    includedBlockHash:
                      observation.blockHash ??
                      (sameTransaction
                        ? operation.includedBlockHash
                        : undefined),
                    inclusionObservedAt:
                      sameTransaction
                        ? operation.inclusionObservedAt ?? now
                        : now,
                    inclusionEvidenceRevision: nextEvidenceRevision,
                    reason: undefined,
                    retryable: undefined,
                    updatedAt: now,
                    attentionRevision: transitionAttentionRevision(
                      operation,
                      'confirming'
                    ),
                    statusTimestamps: {
                      ...operation.statusTimestamps,
                      confirming: now,
                    },
                  }
                }
              ),
            }))

            const recordedOperation = get().operations.find(
              (operation) => operation.id === id
            )
            if (
              recordedOperation?.userOperationHash === undefined ||
              recordedOperation.status !== 'confirming' ||
              !operationMatchesInclusionObservation(
                recordedOperation,
                observation
              ) ||
              recordedOperation.inclusionEvidenceRevision !==
                nextEvidenceRevision
            ) {
              return false
            }
            const durableOperation = readDurableOperation({
              id: recordedOperation.id,
              userOperationHash: recordedOperation.userOperationHash,
              chainId: recordedOperation.chainId,
              accountAddress: recordedOperation.accountAddress,
              lane: recordedOperation.lane,
            })
            return durableOperation?.status === 'confirming' &&
              operationMatchesInclusionObservation(
                durableOperation,
                observation
              ) &&
              durableOperation.inclusionEvidenceRevision ===
                nextEvidenceRevision &&
              hasDurableOperationJournal(
                recordedOperation,
                recordedOperation.userOperationHash,
                false
              )
          } catch {
            return false
          }
        },

        releaseLaneAfterSuccessfulInclusion: (id, observation) => {
          if (!observationReportsSuccessfulInclusion(observation)) return false

          const currentOperation = get().operations.find(
            (operation) => operation.id === id
          )
          if (currentOperation?.userOperationHash === undefined) return false
          if (isSponsoredOperationTerminal(currentOperation.status)) {
            return currentOperation.status === 'confirmed' &&
              currentOperation.transactionHashVerified === true &&
              currentOperation.transactionHash?.toLowerCase() ===
                observation.transactionHash.toLowerCase()
          }

          let durableBefore: SponsoredOperation | undefined
          try {
            durableBefore = readDurableOperation({
              id: currentOperation.id,
              userOperationHash: currentOperation.userOperationHash,
              chainId: currentOperation.chainId,
              accountAddress: currentOperation.accountAddress,
              lane: currentOperation.lane,
            })
            if (
              !operationMatchesInclusionObservation(
                currentOperation,
                observation
              ) ||
              !operationMatchesInclusionObservation(
                durableBefore,
                observation
              ) ||
              !hasDurableOperationJournal(
                currentOperation,
                currentOperation.userOperationHash,
                false
              )
            ) {
              return false
            }

            const releaseCandidate: SponsoredOperation & {
              userOperationHash: Hex
            } = {
              ...currentOperation,
              userOperationHash: currentOperation.userOperationHash,
              laneReleasedAfterSuccessfulInclusion: true,
            }
            writeSponsoredOperationLaneRelease(
              releaseCandidate,
              observation
            )

            // The append-only tombstone readback is the release barrier. The
            // inclusion evidence was already persisted above; old v1 tabs may
            // overwrite its mutable journal but cannot erase this record.
            const releaseTombstone =
              readExactSponsoredOperationLaneRelease({
                id: currentOperation.id,
                userOperationHash: currentOperation.userOperationHash,
                chainId: currentOperation.chainId,
                accountAddress: currentOperation.accountAddress,
                lane: currentOperation.lane,
              })
            if (releaseTombstone === undefined) return false

            const durableReleased = readDurableOperation({
              id: currentOperation.id,
              userOperationHash: currentOperation.userOperationHash,
              chainId: currentOperation.chainId,
              accountAddress: currentOperation.accountAddress,
              lane: currentOperation.lane,
            })
            if (
              durableReleased?.laneReleasedAfterSuccessfulInclusion !== true ||
              !operationMatchesInclusionObservation(
                durableReleased,
                observation
              )
            ) {
              return false
            }

            try {
              set((state) => {
                const operations = updateOperation(
                  state.operations,
                  id,
                  (operation) => {
                    if (
                      operation.userOperationHash?.toLowerCase() !==
                        currentOperation.userOperationHash?.toLowerCase()
                    ) {
                      return operation
                    }
                    return {
                      ...operation,
                      laneReleasedAfterSuccessfulInclusion: true,
                    }
                  }
                )
                return {
                  operations,
                  activeLanes: activeLanesForOperations(operations),
                }
              })
            } catch {
              // Zustand applies the state projection before invoking custom
              // persistence. Even if that later write fails, the append-only
              // tombstone above is the authoritative release boundary.
            }
            return true
          } catch {
            return false
          }
        },

        clearObservedInclusion: (id) => {
          const currentOperation = get().operations.find(
            (operation) => operation.id === id
          )
          if (
            !currentOperation ||
            isSponsoredOperationTerminal(currentOperation.status)
          ) {
            return false
          }
          let durableBefore: SponsoredOperation | undefined
          try {
            if (currentOperation.userOperationHash !== undefined) {
              durableBefore = readDurableOperation({
                id: currentOperation.id,
                userOperationHash: currentOperation.userOperationHash,
                chainId: currentOperation.chainId,
                accountAddress: currentOperation.accountAddress,
                lane: currentOperation.lane,
              })
            }
          } catch {
            return false
          }
          if (
            currentOperation.includedTransactionHash === undefined &&
            durableBefore?.includedTransactionHash === undefined
          ) {
            return true
          }

          const nextEvidenceRevision = Math.max(
            sponsoredOperationInclusionEvidenceRevision(currentOperation),
            sponsoredOperationInclusionEvidenceRevision(durableBefore)
          ) + 1
          const now = Math.max(
            Date.now(),
            currentOperation.updatedAt + 1,
            (durableBefore?.updatedAt ?? 0) + 1
          )
          try {
            set((state) => ({
              operations: updateOperation(
                state.operations,
                id,
                (operation) => {
                  if (
                    isSponsoredOperationTerminal(operation.status)
                  ) {
                    return operation
                  }
                  return {
                    ...operation,
                    includedTransactionHash: undefined,
                    includedBlockNumber: undefined,
                    includedBlockHash: undefined,
                    inclusionObservedAt: undefined,
                    inclusionEvidenceRevision: nextEvidenceRevision,
                    updatedAt: now,
                  }
                }
              ),
            }))
            const clearedOperation = get().operations.find(
              (operation) => operation.id === id
            )
            if (
              clearedOperation?.userOperationHash === undefined ||
              clearedOperation.includedTransactionHash !== undefined ||
              clearedOperation.inclusionEvidenceRevision !==
                nextEvidenceRevision
            ) {
              return false
            }
            const durableOperation = readDurableOperation({
              id: clearedOperation.id,
              userOperationHash: clearedOperation.userOperationHash,
              chainId: clearedOperation.chainId,
              accountAddress: clearedOperation.accountAddress,
              lane: clearedOperation.lane,
            })
            return durableOperation?.includedTransactionHash === undefined &&
              durableOperation?.inclusionEvidenceRevision ===
                nextEvidenceRevision &&
              hasDurableOperationJournal(
                clearedOperation,
                clearedOperation.userOperationHash,
                false
              )
          } catch {
            return false
          }
        },

        recordTransactionHash: (id, hash) => {
          set((state) => ({
            operations: updateOperation(state.operations, id, (operation) => {
              if (
                operation.transactionHashVerified === true &&
                operation.transactionHash?.toLowerCase() !== hash.toLowerCase()
              ) {
                return operation
              }
              return {
                ...operation,
                transactionHash: hash,
                transactionHashVerified: true,
                updatedAt: Date.now(),
              }
            }),
          }))
        },

        incrementRetry: (id) => {
          set((state) => ({
            operations: updateOperation(state.operations, id, (operation) => ({
              ...operation,
              retryCount: operation.retryCount + 1,
              updatedAt: Date.now(),
            })),
          }))
        },

        recordAutomaticRecoveryAttempt: (id, attemptedAt) => {
          const currentOperation = get().operations.find(
            (operation) => operation.id === id
          )
          if (
            currentOperation?.userOperationHash === undefined ||
            isSponsoredOperationTerminal(currentOperation.status) ||
            currentOperation.automaticRecoveryExhaustedAt !== undefined
          ) {
            return false
          }

          const now = Math.max(attemptedAt, currentOperation.updatedAt + 1)
          set((state) => ({
            operations: updateOperation(state.operations, id, (operation) => ({
              ...operation,
              automaticRecoveryStartedAt:
                operation.automaticRecoveryStartedAt ??
                sponsoredOperationAutomaticRecoveryStartedAt(operation),
              lastAutomaticRecoveryAttemptAt: now,
              automaticRecoveryAttemptCount:
                (operation.automaticRecoveryAttemptCount ?? 0) + 1,
              updatedAt: now,
            })),
          }))
          return true
        },

        exhaustAutomaticRecovery: (id, exhaustedAt) => {
          const currentOperation = get().operations.find(
            (operation) => operation.id === id
          )
          if (
            currentOperation?.userOperationHash === undefined ||
            isSponsoredOperationTerminal(currentOperation.status) ||
            currentOperation.automaticRecoveryExhaustedAt !== undefined
          ) {
            return
          }

          const now = Math.max(exhaustedAt, currentOperation.updatedAt + 1)
          set((state) => ({
            operations: updateOperation(state.operations, id, (operation) => ({
              ...operation,
              automaticRecoveryStartedAt:
                operation.automaticRecoveryStartedAt ??
                sponsoredOperationAutomaticRecoveryStartedAt(operation),
              automaticRecoveryExhaustedAt: now,
              updatedAt: now,
            })),
          }))
        },

        acknowledgeOperations: (operations) => {
          if (operations.length === 0) return

          const acknowledgements = new Map(
            operations.map((operation) => [
              operation.id,
              operation.attentionRevision,
            ])
          )
          set((state) => ({
            operations: state.operations.map((operation) => {
              const acknowledgedRevision = acknowledgements.get(operation.id)
              const currentRevision =
                getSponsoredOperationAttentionRevision(operation)
              if (
                acknowledgedRevision === undefined ||
                acknowledgedRevision !== currentRevision ||
                !isSponsoredOperationAttentionStatus(operation.status)
              ) {
                return operation
              }

              return {
                ...operation,
                attentionRevision: currentRevision,
                acknowledgedAttentionRevision: currentRevision,
              }
            }),
          }))
        },

        failOperation: ({
          id,
          status = 'failed',
          reason,
          retryable,
          replacementUserOperationHash,
          protocolNonceAdvanced,
        }) => {
          const currentOperation = get().operations
            .find((operation) => operation.id === id)
          if (
            !currentOperation ||
            (
              currentOperation.status === status &&
              currentOperation.reason === reason &&
              currentOperation.retryable === retryable &&
              currentOperation.replacementUserOperationHash ===
                replacementUserOperationHash &&
              currentOperation.protocolNonceAdvanced ===
                protocolNonceAdvanced
            ) ||
            (
              isSponsoredOperationTerminal(currentOperation.status) &&
              !canResolveTerminalOperation(
                currentOperation.status,
                status
              )
            )
          ) {
            return
          }
          const now = Math.max(
            Date.now(),
            currentOperation.updatedAt + 1
          )
          set((state) => {
            const operations = updateOperation(
              state.operations,
              id,
              (operation) => {
                if (
                  isSponsoredOperationTerminal(operation.status) &&
                  !canResolveTerminalOperation(
                    operation.status,
                    status
                  )
                ) {
                  return operation
                }
                return {
                  ...operation,
                  status,
                  reason,
                  retryable,
                  replacementUserOperationHash,
                  ...(protocolNonceAdvanced === true
                    ? { protocolNonceAdvanced: true as const }
                    : {}),
                  updatedAt: now,
                  attentionRevision: failureAttentionRevision(
                    operation,
                    status
                  ),
                  statusTimestamps: {
                    ...operation.statusTimestamps,
                    [status]: now,
                  },
                }
              }
            )
            return {
              operations,
              ...(isSponsoredOperationTerminal(status)
                ? { activeLanes: activeLanesForOperations(operations) }
                : {}),
            }
          })
        },

        cancelOperation: (id) => {
          const operation = get().operations.find((item) => item.id === id)
          if (!operation || !canCancelSponsoredOperationLocally(operation)) {
            return
          }
          get().transition(id, 'cancelled')
        },

        releaseLane: (id) => {
          set((state) => ({
            activeLanes: releaseOperationLane(state.activeLanes, id),
          }))
        },

        cleanupOperations: () => {
          const now = Date.now()
          const terminalCutoff = now - 24 * 60 * 60 * 1000
          const state = get()
          const activeOperationIds = new Set(
            Object.values(state.activeLanes)
          )
          const migratedOperations = state.operations.map((operation) => {
            if (!sponsoredOperationRecoveryIsStale(operation, now)) {
              return operation
            }
            const status: SponsoredOperationStatus = 'outcome-unknown'
            return {
              ...operation,
              status,
              reason: undefined,
              retryable: false,
              replacementUserOperationHash: undefined,
              automaticRecoveryExpired: true as const,
              automaticRecoveryExhaustedAt:
                operation.automaticRecoveryExhaustedAt ?? now,
              updatedAt: now,
              attentionRevision: failureAttentionRevision(operation, status),
              statusTimestamps: {
                ...operation.statusTimestamps,
                [status]: now,
              },
            }
          })
          const operations = migratedOperations.filter((operation) => {
            if (isSponsoredOperationTerminal(operation.status)) {
              return operation.updatedAt > terminalCutoff
            }
            if (operation.userOperationHash !== undefined) {
              return true
            }
            // Time alone cannot prove a wallet-approval flow in another tab
            // is abandoned. Recovery removes it only while holding the
            // browser-wide lane lock.
            return activeOperationIds.has(operation.id)
          })
          const activeLanes = activeLanesForOperations(operations)
          const operationsChanged =
            operations.length !== state.operations.length ||
            operations.some(
              (operation, index) => operation !== state.operations[index]
            )
          const activeLanesChanged =
            Object.keys(activeLanes).length !==
              Object.keys(state.activeLanes).length ||
            Object.entries(activeLanes).some(
              ([key, operationId]) =>
                state.activeLanes[key] !== operationId
            )
          if (!operationsChanged && !activeLanesChanged) return
          set({ operations, activeLanes })
        },

        getActiveOperation: (
          accountAddress,
          lane = DEFAULT_SPONSORED_OPERATION_LANE
        ) => {
          const state = get()
          const id = state.activeLanes[laneKey(accountAddress, lane)]
          return state.operations.find((operation) => operation.id === id)
        },
      }),
      {
        name: SPONSORED_OPERATION_STORAGE_NAME,
        storage: sponsoredOperationStorage,
        version: SPONSORED_OPERATION_STORAGE_VERSION,
        migrate: (persistedState, persistedVersion) =>
          migrateSponsoredOperationState(
            persistedState,
            persistedVersion
          ) as SponsoredOperationState,
        merge: (persistedState, currentState) =>
          mergeSponsoredOperationState(persistedState, currentState),
        partialize: (state) => ({
          operations: state.operations,
          activeLanes: state.activeLanes,
        }),
      }
    ),
    { name: 'SponsoredOperationStore' }
  )
)

/**
 * Restores the exact lane while its browser-wide lane lock is held.
 *
 * Zustand intentionally swallows hydration failures, and enumerating
 * localStorage keys is not a transactional snapshot. New submissions
 * therefore publish a directly addressable lane head as their pre-send
 * barrier. This function also reads the shared snapshot directly so a
 * malformed store cannot silently turn into an empty, retryable lane.
 */
export function restoreSponsoredOperationLane(input: {
  chainId: number
  accountAddress: Address
  lane: string
}): void {
  let candidates: SponsoredOperation[] = [
    ...useSponsoredOperationStore.getState().operations.filter(
      (operation) => operationMatchesLane(operation, input)
    ),
    ...durableJournalState().operations.filter(
      (operation) => operationMatchesLane(operation, input)
    ),
  ]
  const storedValue = globalThis.localStorage.getItem(
    SPONSORED_OPERATION_STORAGE_NAME
  )
  if (storedValue !== null) {
    const parsed = parseStorageValue(storedValue)
    if (!parsed) {
      throw new Error('The sponsored-operation activity store is unreadable')
    }
    const rawVersion = (
      parsed as StorageValue<unknown> & { version?: unknown }
    ).version
    if (rawVersion !== undefined && typeof rawVersion !== 'number') {
      throw new Error(
        'The sponsored-operation activity store has an invalid version'
      )
    }
    const persisted = migrateSponsoredOperationState(
      parsed.state,
      rawVersion ?? 0
    )
    candidates.push(
      ...persisted.operations.filter((operation) =>
        operationMatchesLane(operation, input)
      )
    )
  }

  candidates = mergeExactOperationJournals({
    operations: candidates,
    activeLanes: {},
  }).operations

  // This is the only legacy migration publisher and the caller holds the
  // same browser-wide lane lock used by submission. Publish the complete
  // multi-candidate guard before setState persists any missing journals.
  candidates = publishSponsoredOperationLaneHeadBeforeJournals(
    input,
    candidates
  )

  if (candidates.length === 0) return

  useSponsoredOperationStore.setState((state) => {
    const operationsById = new Map(
      state.operations.map((operation) => [operation.id, operation])
    )
    for (const candidate of candidates) {
      const current = operationsById.get(candidate.id)
      operationsById.set(
        candidate.id,
        current ? mergeOperationRecord(current, candidate) : candidate
      )
    }
    const operations = [...operationsById.values()]
      .sort((left, right) => left.createdAt - right.createdAt)
    return {
      operations,
      activeLanes: activeLanesForOperations(operations),
    }
  })

  // Validate the head-to-journal barrier after persistence. Callers must not
  // sign or submit if any direct entry is missing, mismatched, or if a
  // nonterminal hash-bearing lane candidate was omitted from the guard.
  const laneHeadValue = globalThis.localStorage.getItem(
    sponsoredOperationLaneHeadKey(input)
  )
  const laneHead = parseSponsoredOperationLaneHead(laneHeadValue)
  if (laneHeadValue !== null && !laneHead) {
    throw new Error(
      'The sponsored-operation lane recovery head is unreadable'
    )
  }
  for (const entry of laneHead?.operations ?? []) {
    const journalOperation = readDurableOperation({
      id: entry.operationId,
      userOperationHash: entry.userOperationHash,
      chainId: input.chainId,
      accountAddress: input.accountAddress,
      lane: input.lane,
    })
    if (
      journalOperation?.id !== entry.operationId ||
      journalOperation.userOperationHash?.toLowerCase() !==
        entry.userOperationHash.toLowerCase() ||
      !operationMatchesLane(journalOperation, input)
    ) {
      throw new Error(
        'The sponsored-operation lane recovery journal is unreadable'
      )
    }
  }
  for (
    const candidate of useSponsoredOperationStore.getState().operations
  ) {
    if (
      operationMatchesLane(candidate, input) &&
      candidate.userOperationHash !== undefined &&
      !isSponsoredOperationTerminal(candidate.status) &&
      laneHead?.operations.some((entry) =>
        entry.operationId === candidate.id &&
        entry.userOperationHash.toLowerCase() ===
          candidate.userOperationHash?.toLowerCase()
      ) !== true
    ) {
      throw new Error(
        'The sponsored-operation lane recovery head is incomplete'
      )
    }
  }
}

export function createSponsoredOperationSignal(operationId: string): AbortSignal {
  const existing = operationAbortControllers.get(operationId)
  if (existing) return existing.signal

  const controller = new AbortController()
  operationAbortControllers.set(operationId, controller)
  return controller.signal
}

export function hasDurableSponsoredOperationSubmission(
  operationId: string,
  userOperationHash: Hex
): boolean {
  try {
    const operation = useSponsoredOperationStore.getState().operations
      .find((candidate) => candidate.id === operationId)
    if (
      !operation ||
      operation.userOperationHash?.toLowerCase() !==
        userOperationHash.toLowerCase() ||
      isSponsoredOperationTerminal(operation.status) ||
      sponsoredOperationSubmissionRevisions.get(operationId) !==
        sponsoredOperationPersistenceBlockedRevision ||
      anotherOperationUsesUserOperationHash(operationId, userOperationHash) ||
      useSponsoredOperationStore.getState().operations.some((candidate) =>
        candidate.id !== operation.id &&
        operationMatchesLane(candidate, operation) &&
        isSponsoredOperationLaneBlocking(candidate)
      )
    ) {
      return false
    }
    return hasDurableOperationJournal(
      operation,
      userOperationHash,
      true,
      true
    )
  } catch {
    return false
  } finally {
    sponsoredOperationSubmissionRevisions.delete(operationId)
  }
}

export function hasDurableSponsoredOperationOrderIntent(
  operationId: string,
  expected: PersistedPerpsOrderRequestV2
): boolean {
  try {
    const operation = useSponsoredOperationStore.getState().operations
      .find((candidate) => candidate.id === operationId)
    const journal = readExactOperationJournal(operationId)
    return operation?.orderRequestV2 !== undefined &&
      journal?.orderRequestV2 !== undefined &&
      JSON.stringify(operation.orderRequestV2) === JSON.stringify(expected) &&
      JSON.stringify(journal.orderRequestV2) === JSON.stringify(expected)
  } catch {
    return false
  }
}

export function hasDurableSponsoredProtectionIntent(operationId: string, expected: PersistedProtectionIntent): boolean {
  try {
    const operation = useSponsoredOperationStore.getState().operations.find(candidate => candidate.id === operationId)
    const journal = readExactOperationJournal(operationId)
    return JSON.stringify(operation?.protectionIntent) === JSON.stringify(expected) && JSON.stringify(journal?.protectionIntent) === JSON.stringify(expected)
  } catch { return false }
}

export function cancelSponsoredOperationRequest(operationId: string): void {
  const operation = useSponsoredOperationStore.getState().operations
    .find((item) => item.id === operationId)
  if (!operation || !canCancelSponsoredOperationLocally(operation)) {
    return
  }

  operationAbortControllers.get(operationId)?.abort(
    new DOMException('Sponsored operation request cancelled', 'AbortError')
  )
  useSponsoredOperationStore.getState().cancelOperation(operationId)
  operationAbortControllers.delete(operationId)
}

export function canForceUnlockLegacySponsoredOperation(
  operation: SponsoredOperation
): boolean {
  return (
    operation.legacyManualUnlockEligible === true ||
    operation.manifestVersion ===
      LEGACY_AMBIGUOUS_OPERATION_MANIFEST_VERSION
  ) &&
    operation.submissionMetadataVersion === undefined &&
    operation.userOperationHash !== undefined &&
    operation.status === 'receipt-timeout'
}

export async function forceUnlockLegacySponsoredOperation(
  operationId: string
): Promise<boolean> {
  const initialOperation = useSponsoredOperationStore.getState().operations
    .find((operation) => operation.id === operationId)
  if (
    !initialOperation ||
    !canForceUnlockLegacySponsoredOperation(initialOperation)
  ) {
    return false
  }

  let releaseBrowserLane: ReleaseSponsoredOperationBrowserLock | undefined
  try {
    releaseBrowserLane = await acquireSponsoredOperationBrowserLane({
      chainId: initialOperation.chainId,
      accountAddress: initialOperation.accountAddress,
      lane: initialOperation.lane,
    })
    restoreSponsoredOperationLane({
      chainId: initialOperation.chainId,
      accountAddress: initialOperation.accountAddress,
      lane: initialOperation.lane,
    })
    await useSponsoredOperationStore.persist.rehydrate()
    restoreSponsoredOperationLane({
      chainId: initialOperation.chainId,
      accountAddress: initialOperation.accountAddress,
      lane: initialOperation.lane,
    })
    const now = Date.now()
    let unlocked = false
    useSponsoredOperationStore.setState((state) => {
      const candidate = state.operations.find(
        (operation) => operation.id === operationId
      )
      if (!candidate || !canForceUnlockLegacySponsoredOperation(candidate)) {
        return state
      }

      unlocked = true
      const operations = updateOperation(
        state.operations,
        operationId,
        (operation) => {
          return {
            ...operation,
            status: 'outcome-unknown',
            forcedLegacyUnlock: true,
            reason: undefined,
            retryable: false,
            replacementUserOperationHash: undefined,
            updatedAt: now,
            attentionRevision: failureAttentionRevision(
              operation,
              'outcome-unknown'
            ),
            statusTimestamps: {
              ...operation.statusTimestamps,
              'outcome-unknown': now,
            },
          }
        }
      )

      return {
        operations,
        activeLanes: activeLanesForOperations(operations),
      }
    })
    return unlocked
  } catch {
    return false
  } finally {
    await releaseBrowserLane?.()
  }
}

export function releaseSponsoredOperationSignal(operationId: string): void {
  operationAbortControllers.delete(operationId)
  sponsoredOperationSubmissionRevisions.delete(operationId)
}

export function hasSponsoredOperationSignal(operationId: string): boolean {
  return operationAbortControllers.has(operationId)
}
