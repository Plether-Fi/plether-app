import type {
  PerpsActionKind,
  SponsoredExecutionStatus,
} from '@plether/perps-aa-client'
import type { Address, Hex } from 'viem'
import { trackPerpsSponsoredOperation } from '../analytics/perps'
import {
  findBundlerRequestError,
  findSponsorRequestError,
  findSponsoredPreflightError,
  type StablePreflightReason,
} from './errors'
import { SponsoredOperationCoordinationError } from './laneLock'
import {
  createSponsoredOperationSignal,
  isSponsoredOperationTerminal,
  releaseSponsoredOperationSignal,
  SponsoredOperationLockedError,
  type SponsoredOperationInclusionObservation,
  useSponsoredOperationStore,
} from './operationStore'
import type { ManagedUserOperation } from './runtimeContext'

export interface SponsoredOperationAnalyticsMetadata {
  accountMode?: string
  manifestVersion?: string
  action: PerpsActionKind
  walletFamily?: string
  walletVersion?: string
}

export interface SponsoredOperationMetadata
  extends SponsoredOperationAnalyticsMetadata {
  id?: string
  ownerAddress: Address
  accountAddress: Address
  chainId: number
  accountMode: string
  manifestVersion: string
  authorizationToken?: Address
  authorizationNonce?: Hex
  lane?: string
}

export interface SponsoredOperationTracker {
  id: string
  signal: AbortSignal
  onStatus: (status: SponsoredExecutionStatus) => void
  onUserOperationHash: (
    hash: Hex,
    metadata: {
      signedUserOperation: ManagedUserOperation
    }
  ) => boolean
  onObservedInclusion: (
    observation: SponsoredOperationInclusionObservation
  ) => boolean
  onInclusionRetracted: () => boolean
  onTransactionHash: (hash: Hex) => void
  onEstimationRestart: () => void
  fail: (error: unknown) => void
  release: () => void
}

function operationId(): string {
  return globalThis.crypto.randomUUID()
}

function analyticsProperties(
  metadata: SponsoredOperationAnalyticsMetadata,
  extra: Record<string, string | number | boolean | undefined> = {}
) {
  return {
    manifest_version: metadata.manifestVersion,
    account_mode: metadata.accountMode,
    action_kind: metadata.action,
    wallet_family: metadata.walletFamily,
    wallet_version: metadata.walletVersion,
    ...extra,
  }
}

function findCause<T>(
  error: unknown,
  predicate: (value: unknown) => value is T
): T | undefined {
  let current = error
  const seen = new Set<object>()

  for (let depth = 0; depth < 8 && current !== undefined; depth += 1) {
    if (predicate(current)) return current
    if (!current || typeof current !== 'object' || seen.has(current)) {
      return undefined
    }
    seen.add(current)
    current = (current as { cause?: unknown }).cause
  }

  return undefined
}

export function sponsoredPreflightFailureReason(
  error: unknown
): StablePreflightReason {
  const preflightError = findSponsoredPreflightError(error)
  if (preflightError) return preflightError.reason

  const lockedError = findCause(
    error,
    (value): value is SponsoredOperationLockedError =>
      value instanceof SponsoredOperationLockedError
  )
  if (lockedError) return 'LANE_BUSY'

  const coordinationError = findCause(
    error,
    (value): value is SponsoredOperationCoordinationError =>
      value instanceof SponsoredOperationCoordinationError
  )
  if (coordinationError) return 'BROWSER_COORDINATION_UNAVAILABLE'

  const sponsorError = findSponsorRequestError(error)
  if (sponsorError?.reason === 'SPONSOR_UNAVAILABLE') {
    return 'SPONSORSHIP_DISABLED'
  }
  if (sponsorError?.reason === 'ACCOUNT_NOT_TRUSTED') {
    return 'ACCOUNT_NOT_TRUSTED'
  }

  return 'UNKNOWN'
}

export function trackSponsoredOperationPreflightFailure(
  metadata: SponsoredOperationAnalyticsMetadata,
  error: unknown
): StablePreflightReason {
  const reason = sponsoredPreflightFailureReason(error)
  trackPerpsSponsoredOperation('preflight_failed', analyticsProperties(metadata, {
    sponsorship_accepted: false,
    retry_count: 0,
    reason_code: reason,
    terminal_outcome: 'preflight_failed',
  }))
  return reason
}

export function beginSponsoredOperationTracking(
  metadata: SponsoredOperationMetadata
): SponsoredOperationTracker {
  const id = metadata.id ?? operationId()
  const store = useSponsoredOperationStore.getState()
  store.beginOperation({
    id,
    ownerAddress: metadata.ownerAddress,
    accountAddress: metadata.accountAddress,
    chainId: metadata.chainId,
    accountMode: metadata.accountMode,
    manifestVersion: metadata.manifestVersion,
    action: metadata.action,
    authorizationToken: metadata.authorizationToken,
    authorizationNonce: metadata.authorizationNonce,
    lane: metadata.lane,
  })

  trackPerpsSponsoredOperation('building', analyticsProperties(metadata, {
    sponsorship_accepted: false,
    retry_count: 0,
  }))

  return {
    id,
    signal: createSponsoredOperationSignal(id),

    onStatus: (status) => {
      useSponsoredOperationStore.getState().transition(id, status)
      const operation = useSponsoredOperationStore.getState().operations
        .find((item) => item.id === id)
      trackPerpsSponsoredOperation(status, analyticsProperties(metadata, {
        sponsorship_accepted: operation?.sponsorshipAccepted ?? false,
        retry_count: operation?.retryCount ?? 0,
        ...(status === 'confirmed' ? { terminal_outcome: 'confirmed' } : {}),
      }))
    },

    onUserOperationHash: (hash, submissionMetadata) =>
      useSponsoredOperationStore.getState().recordUserOperationHash(
        id,
        hash,
        submissionMetadata
      ),

    onObservedInclusion: (observation) =>
      useSponsoredOperationStore.getState().recordObservedInclusion(
        id,
        observation
      ),

    onInclusionRetracted: () =>
      useSponsoredOperationStore.getState().clearObservedInclusion(id),

    onTransactionHash: (hash) => {
      useSponsoredOperationStore.getState().recordTransactionHash(id, hash)
    },

    onEstimationRestart: () => {
      useSponsoredOperationStore.getState().incrementRetry(id)
      useSponsoredOperationStore.getState().transition(id, 'building')
    },

    fail: (error) => {
      const currentOperation = useSponsoredOperationStore.getState().operations
        .find((item) => item.id === id)
      if (currentOperation?.status === 'cancelled') {
        return
      }
      if (
        currentOperation &&
        isSponsoredOperationTerminal(currentOperation.status)
      ) {
        return
      }
      const sponsorError = findSponsorRequestError(error)
      const bundlerError = findBundlerRequestError(error)
      const hasPersistedHash = currentOperation?.userOperationHash !== undefined
      const terminalStatus = bundlerError?.terminalStatus
      const operationStatus =
        terminalStatus && terminalStatus !== 'receipt-timeout'
          ? terminalStatus
          : hasPersistedHash
            ? 'receipt-timeout'
            : 'failed'
      const reason = sponsorError?.reason ??
        terminalStatus ??
        (hasPersistedHash
          ? 'BUNDLER_UNAVAILABLE'
          : undefined) ??
        'UNKNOWN'
      const retryable = sponsorError?.retryable ??
        bundlerError?.retryable ??
        false

      if (
        operationStatus === 'receipt-timeout' &&
        currentOperation?.includedTransactionHash !== undefined
      ) {
        // Exact latest-chain inclusion is already persisted. A timeout here
        // only means that the RPC's safe head has not caught up yet, so keep
        // the lane locked and let background recovery finish verification.
        return
      }

      useSponsoredOperationStore.getState().failOperation({
        id,
        status: operationStatus,
        reason,
        retryable,
        replacementUserOperationHash:
          bundlerError?.replacementUserOperationHash as Hex | undefined,
      })
      trackPerpsSponsoredOperation(
        operationStatus,
        analyticsProperties(metadata, {
          reason_code: reason,
          retry_count:
            useSponsoredOperationStore.getState().operations
              .find((item) => item.id === id)?.retryCount ?? 0,
          ...(operationStatus === 'receipt-timeout'
            ? {}
            : { terminal_outcome: operationStatus }),
        })
      )
    },

    release: () => {
      releaseSponsoredOperationSignal(id)
    },
  }
}
