import type {
  PerpsActionKind,
  SponsoredExecutionStatus,
} from '@plether/perps-aa-client'
import type { Address, Hex } from 'viem'
import {
  trackPerpsSponsoredOperation,
  type PerpsSponsoredOperationStatus,
} from '../analytics/perps'
import {
  findBundlerRequestError,
  findSponsorRequestError,
} from './errors'
import {
  createSponsoredOperationSignal,
  releaseSponsoredOperationSignal,
  useSponsoredOperationStore,
} from './operationStore'

export interface SponsoredOperationMetadata {
  id?: string
  ownerAddress: Address
  accountAddress: Address
  chainId: number
  accountMode: string
  manifestVersion: string
  action: PerpsActionKind
  authorizationToken?: Address
  lane?: string
  walletFamily?: string
  walletVersion?: string
}

export interface SponsoredOperationTracker {
  id: string
  signal: AbortSignal
  onStatus: (status: SponsoredExecutionStatus) => void
  onUserOperationHash: (hash: Hex) => void
  onTransactionHash: (hash: Hex) => void
  onEstimationRestart: () => void
  fail: (error: unknown) => void
  release: () => void
}

function operationId(): string {
  return globalThis.crypto.randomUUID()
}

function analyticsProperties(
  metadata: SponsoredOperationMetadata,
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

    onUserOperationHash: (hash) => {
      useSponsoredOperationStore.getState().recordUserOperationHash(id, hash)
    },

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
      const sponsorError = findSponsorRequestError(error)
      const bundlerError = findBundlerRequestError(error)
      const hasSubmittedHash = currentOperation?.userOperationHash !== undefined
      const terminalStatus = bundlerError?.terminalStatus
      const operationStatus =
        terminalStatus && terminalStatus !== 'receipt-timeout'
          ? terminalStatus
          : bundlerError && hasSubmittedHash
            ? 'receipt-timeout'
            : 'failed'
      const reason = sponsorError?.reason ??
        terminalStatus ??
        (bundlerError && hasSubmittedHash
          ? 'BUNDLER_UNAVAILABLE'
          : undefined) ??
        'UNKNOWN'
      const retryable = sponsorError?.retryable ??
        bundlerError?.retryable ??
        false

      useSponsoredOperationStore.getState().failOperation({
        id,
        status: operationStatus,
        reason,
        retryable,
        replacementUserOperationHash:
          bundlerError?.replacementUserOperationHash as Hex | undefined,
      })
      trackPerpsSponsoredOperation(
        operationStatus as PerpsSponsoredOperationStatus,
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
