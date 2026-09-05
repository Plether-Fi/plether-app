import { useEffect } from 'react'
import {
  clearDepositAuthorization,
  clearLegacyDepositAuthorization,
} from './authorizationStore'
import {
  hasSponsoredOperationSignal,
  hasObservedSponsoredOperationInclusion,
  isSponsoredOperationLaneBlocking,
  isSponsoredOperationTerminal,
  restoreSponsoredOperationLane,
  sponsoredOperationAutomaticRecoveryIsDue,
  sponsoredOperationAutomaticRecoveryIsExhausted,
  SPONSORED_OPERATION_JOURNAL_PREFIX,
  SPONSORED_OPERATION_LANE_HEAD_PREFIX,
  SPONSORED_OPERATION_LANE_RELEASE_PREFIX,
  SPONSORED_OPERATION_RESOLUTION_PREFIX,
  SPONSORED_OPERATION_STORAGE_NAME,
  type SponsoredOperation,
  useSponsoredOperationStore,
} from './operationStore'
import {
  acquireSponsoredOperationBrowserLane,
  acquireSponsoredOperationBrowserRecoveryLock,
  type ReleaseSponsoredOperationBrowserLock,
} from './laneLock'
import { reconcileUserOperation } from './operationReconciler'
import { resolveProtocolOperation } from './protocolOperationResolution'
import {
  type PerpsAaSmartAccountRuntime,
  usePerpsAaRuntime,
} from './runtimeContext'

const PROTOCOL_RECOVERY_CHECK_INTERVAL_MS = 60_000

function clearRecoveredAuthorization(operation: SponsoredOperation): void {
  if (!operation.authorizationToken) return
  try {
    if (!operation.authorizationNonce) {
      // Legacy v1 records did not persist nonce ownership. Only a safe receipt
      // authorizes retiring that reuse-based cache; never touch the v2 entry.
      clearLegacyDepositAuthorization({
        chainId: operation.chainId,
        ownerAddress: operation.ownerAddress,
        accountAddress: operation.accountAddress,
        token: operation.authorizationToken,
      })
      return
    }
    clearDepositAuthorization({
      chainId: operation.chainId,
      ownerAddress: operation.ownerAddress,
      accountAddress: operation.accountAddress,
      token: operation.authorizationToken,
      expectedNonce: operation.authorizationNonce,
    })
  } catch {
    // Chain confirmation is authoritative. A local cleanup failure must not
    // downgrade it, retain the lane, or invite the user to submit again.
  }
}

function clearAuthorizationBeforeIncludedRelease(
  operation: SponsoredOperation
): boolean {
  if (!operation.authorizationToken) return true
  // Old persisted records did not bind the cache entry to an EIP-3009 nonce.
  // Keep those operations serialized until safe confirmation rather than risk
  // deleting an authorization created by a later deposit.
  if (!operation.authorizationNonce) return false
  try {
    clearDepositAuthorization({
      chainId: operation.chainId,
      ownerAddress: operation.ownerAddress,
      accountAddress: operation.accountAddress,
      token: operation.authorizationToken,
      expectedNonce: operation.authorizationNonce,
    })
    // Nonce-owned cleanup cannot remove a newer v2 authorization. Reaching
    // this point means the consumed nonce was durably retired.
    return true
  } catch {
    return false
  }
}

function operationMatchesRuntime(
  operation: SponsoredOperation,
  runtime: PerpsAaSmartAccountRuntime,
  accountAddress: `0x${string}`
): boolean {
  return operation.chainId === runtime.chainId &&
    runtime.manifestVersion !== undefined &&
    (
      operation.manifestVersion === runtime.manifestVersion ||
      // A v1 signed preimage is independently rebound to the current
      // chain/EntryPoint hash before any nonce or expiry decision. This keeps
      // recovery live across a metadata-only manifest version bump while a
      // changed EntryPoint still fails closed at hash verification.
      operation.submissionMetadataVersion === 1
    ) &&
    operation.ownerAddress.toLowerCase() ===
      runtime.ownerAddress.toLowerCase() &&
    operation.accountAddress.toLowerCase() === accountAddress.toLowerCase()
}

async function observedInclusionIsReorged(
  operation: SponsoredOperation,
  runtime: PerpsAaSmartAccountRuntime
): Promise<boolean> {
  if (
    operation.includedTransactionHash === undefined ||
    operation.includedBlockNumber === undefined ||
    operation.includedBlockHash === undefined ||
    runtime.verifyObservedInclusion === undefined
  ) {
    return false
  }
  try {
    return await runtime.verifyObservedInclusion({
      transactionHash: operation.includedTransactionHash,
      blockNumber: BigInt(operation.includedBlockNumber),
      blockHash: operation.includedBlockHash,
    }) === 'reorged'
  } catch {
    return false
  }
}

export function SponsoredOperationRecovery() {
  const runtime = usePerpsAaRuntime()
  const accountAddress = runtime?.smartAccount.accountAddress

  useEffect(() => {
    const onStorage = (event: StorageEvent) => {
      if (
        event.key === SPONSORED_OPERATION_STORAGE_NAME ||
        event.key?.startsWith(SPONSORED_OPERATION_JOURNAL_PREFIX) ||
        event.key?.startsWith(SPONSORED_OPERATION_LANE_HEAD_PREFIX) ||
        event.key?.startsWith(SPONSORED_OPERATION_LANE_RELEASE_PREFIX) ||
        event.key?.startsWith(SPONSORED_OPERATION_RESOLUTION_PREFIX)
      ) {
        void useSponsoredOperationStore.persist.rehydrate()
      }
    }
    globalThis.addEventListener('storage', onStorage)
    return () => {
      globalThis.removeEventListener('storage', onStorage)
    }
  }, [])

  useEffect(() => {
    if (!runtime || !accountAddress) return

    const recovering = new Set<string>()
    const nextProtocolCheckAt = new Map<string, number>()

    const scan = () => {
      const store = useSponsoredOperationStore.getState()
      store.cleanupOperations()

      const currentOperations = useSponsoredOperationStore.getState().operations
        .filter((operation) =>
          operationMatchesRuntime(operation, runtime, accountAddress)
        )

      for (const operation of currentOperations) {
        if (
          !isSponsoredOperationTerminal(operation.status) &&
          operation.userOperationHash === undefined &&
          !hasSponsoredOperationSignal(operation.id) &&
          !recovering.has(operation.id)
        ) {
          // Abort controllers are tab-local. Acquire the same browser-wide
          // exclusive lane used by submission before declaring this pre-hash
          // record abandoned, otherwise a second tab can cancel a live wallet
          // approval and let the first tab submit an untracked operation.
          recovering.add(operation.id)
          void acquireSponsoredOperationBrowserLane({
            chainId: operation.chainId,
            accountAddress: operation.accountAddress,
            lane: operation.lane,
          }).then(async (releaseBrowserLane) => {
            try {
              restoreSponsoredOperationLane({
                chainId: operation.chainId,
                accountAddress: operation.accountAddress,
                lane: operation.lane,
              })
              await useSponsoredOperationStore.persist.rehydrate()
              restoreSponsoredOperationLane({
                chainId: operation.chainId,
                accountAddress: operation.accountAddress,
                lane: operation.lane,
              })
              const latestOperation =
                useSponsoredOperationStore.getState().operations
                  .find((item) => item.id === operation.id)
              if (
                latestOperation &&
                !isSponsoredOperationTerminal(latestOperation.status) &&
                latestOperation.userOperationHash === undefined &&
                !hasSponsoredOperationSignal(latestOperation.id) &&
                latestOperation.lane === operation.lane &&
                operationMatchesRuntime(
                  latestOperation,
                  runtime,
                  accountAddress
                )
              ) {
                useSponsoredOperationStore.getState().failOperation({
                  id: latestOperation.id,
                  reason: 'UNKNOWN',
                  retryable: true,
                })
              }
            } finally {
              await releaseBrowserLane()
            }
          }).catch(() => {
            // A held or unavailable Web Lock is not proof of abandonment.
          }).finally(() => {
            recovering.delete(operation.id)
          })
        }
      }

      const recoverable = useSponsoredOperationStore.getState().operations
        .filter((operation) =>
          operationMatchesRuntime(operation, runtime, accountAddress) &&
          operation.userOperationHash !== undefined &&
          (
            !isSponsoredOperationTerminal(operation.status) ||
            operation.status === 'outcome-unknown'
          ) &&
          !hasSponsoredOperationSignal(operation.id) &&
          !recovering.has(operation.id)
        )

      for (const operation of recoverable) {
        const userOperationHash = operation.userOperationHash
        if (!userOperationHash) continue
        const wallClockNow = Date.now()
        if (
          operation.status !== 'outcome-unknown' &&
          sponsoredOperationAutomaticRecoveryIsExhausted(
            operation,
            wallClockNow
          )
        ) {
          store.exhaustAutomaticRecovery(operation.id, wallClockNow)
          continue
        }
        if (
          operation.status !== 'outcome-unknown' &&
          !sponsoredOperationAutomaticRecoveryIsDue(operation, wallClockNow)
        ) {
          continue
        }
        const now = globalThis.performance.now()
        if (
          operation.status === 'outcome-unknown' &&
          (nextProtocolCheckAt.get(operation.id) ?? 0) > now
        ) {
          continue
        }

        recovering.add(operation.id)
        const initiallyLaneBlocking =
          isSponsoredOperationLaneBlocking(operation)
        const acquireRecoveryLock = initiallyLaneBlocking
          ? acquireSponsoredOperationBrowserLane({
              chainId: operation.chainId,
              accountAddress: operation.accountAddress,
              lane: operation.lane,
            })
          : acquireSponsoredOperationBrowserRecoveryLock({
              chainId: operation.chainId,
              accountAddress: operation.accountAddress,
              operationId: operation.id,
            })
        void acquireRecoveryLock.then(async (initialReleaseBrowserLock) => {
          let releaseBrowserLock:
            ReleaseSponsoredOperationBrowserLock | undefined =
            initialReleaseBrowserLock
          let holdsSubmissionLane = initiallyLaneBlocking
          try {
            if (holdsSubmissionLane) {
              restoreSponsoredOperationLane({
                chainId: operation.chainId,
                accountAddress: operation.accountAddress,
                lane: operation.lane,
              })
            }
            await useSponsoredOperationStore.persist.rehydrate()
            if (holdsSubmissionLane) {
              restoreSponsoredOperationLane({
                chainId: operation.chainId,
                accountAddress: operation.accountAddress,
                lane: operation.lane,
              })
            }

            const latestOperation =
              useSponsoredOperationStore.getState().operations
                .find((item) => item.id === operation.id)
            const isStillRecoverable = (
              candidate: SponsoredOperation | undefined
            ): candidate is SponsoredOperation => Boolean(
              candidate &&
              candidate.userOperationHash?.toLowerCase() ===
                userOperationHash.toLowerCase() &&
              (
                !isSponsoredOperationTerminal(candidate.status) ||
                candidate.status === 'outcome-unknown'
              ) &&
              !hasSponsoredOperationSignal(candidate.id) &&
              operationMatchesRuntime(candidate, runtime, accountAddress)
            )
            if (!isStillRecoverable(latestOperation)) return

            const protocolOnly = latestOperation.status === 'outcome-unknown'
            if (!protocolOnly) {
              const attemptNow = Date.now()
              if (
                sponsoredOperationAutomaticRecoveryIsExhausted(
                  latestOperation,
                  attemptNow
                )
              ) {
                useSponsoredOperationStore.getState().exhaustAutomaticRecovery(
                  latestOperation.id,
                  attemptNow
                )
                return
              }
              if (
                !sponsoredOperationAutomaticRecoveryIsDue(
                  latestOperation,
                  attemptNow
                ) ||
                !useSponsoredOperationStore.getState()
                  .recordAutomaticRecoveryAttempt(
                    latestOperation.id,
                    attemptNow
                  )
              ) {
                return
              }
            }

            const latestLaneBlocking =
              isSponsoredOperationLaneBlocking(latestOperation)
            if (!holdsSubmissionLane && latestLaneBlocking) {
              // A reconciliation lock never authorizes work for a blocking
              // operation. This should be impossible for the monotonic release
              // marker, but fail closed if corrupted or legacy data says so.
              return
            }
            if (holdsSubmissionLane && !latestLaneBlocking) {
              // Hydration may reveal that another tab already persisted the
              // successful-inclusion release. Do not perform any RPC while
              // unnecessarily occupying the account submission lane.
              await releaseBrowserLock()
              releaseBrowserLock = undefined
              holdsSubmissionLane = false
              // The next scan will acquire the per-operation reconciliation
              // lock. Returning avoids a hand-off window in which this tab
              // could race a tab that already owns that lock.
              return
            }

            let outcome
            try {
              if (!protocolOnly) {
                outcome = await reconcileUserOperation({
                  runtime,
                  userOperationHash,
                })
              }
            } catch {
              const currentOperation =
                useSponsoredOperationStore.getState().operations
                  .find((item) => item.id === latestOperation.id)
              const hadObservedInclusion =
                currentOperation !== undefined &&
                hasObservedSponsoredOperationInclusion(currentOperation)
              const inclusionWasReorged =
                hadObservedInclusion &&
                await observedInclusionIsReorged(
                  currentOperation,
                  runtime
                )
              const inclusionWasRetracted =
                inclusionWasReorged &&
                useSponsoredOperationStore
                  .getState()
                  .clearObservedInclusion(latestOperation.id)
              if (
                inclusionWasRetracted ||
                (
                  currentOperation !== undefined &&
                  isSponsoredOperationLaneBlocking(currentOperation) &&
                  !hadObservedInclusion
                )
              ) {
                useSponsoredOperationStore.getState().failOperation({
                  id: latestOperation.id,
                  status: 'receipt-timeout',
                  reason: 'BUNDLER_UNAVAILABLE',
                  retryable: false,
                })
              }
            }

            if (outcome?.kind === 'included') {
              // A failed exact receipt is not a successful nonce-consumption
              // boundary. Keep the lane blocked until the safe head makes the
              // execution-reverted terminal result authoritative.
              if (
                !outcome.receipt.success ||
                outcome.receipt.receipt.status !== 'success'
              ) {
                const currentOperation =
                  useSponsoredOperationStore.getState().operations
                    .find((item) => item.id === latestOperation.id)
                const inclusionWasReorged =
                  currentOperation !== undefined &&
                  hasObservedSponsoredOperationInclusion(currentOperation) &&
                  await observedInclusionIsReorged(
                    currentOperation,
                    runtime
                  )
                const inclusionWasRetracted =
                  inclusionWasReorged &&
                  useSponsoredOperationStore
                    .getState()
                    .clearObservedInclusion(latestOperation.id)
                if (inclusionWasRetracted) {
                  useSponsoredOperationStore.getState().failOperation({
                    id: latestOperation.id,
                    status: 'receipt-timeout',
                    reason: 'BUNDLER_UNAVAILABLE',
                    retryable: false,
                  })
                }
                return
              }
              const observation = {
                transactionHash: outcome.transactionHash,
                blockNumber:
                  outcome.receipt.receipt.blockNumber.toString(),
                blockHash: outcome.receipt.receipt.blockHash,
                success: true as const,
              }
              const inclusionPersisted =
                useSponsoredOperationStore
                  .getState()
                  .recordObservedInclusion(
                    latestOperation.id,
                    observation
                  )
              if (!inclusionPersisted) {
                const currentOperation =
                  useSponsoredOperationStore.getState().operations
                    .find((item) => item.id === latestOperation.id)
                if (
                  currentOperation &&
                  isSponsoredOperationLaneBlocking(currentOperation) &&
                  !hasObservedSponsoredOperationInclusion(currentOperation)
                ) {
                  useSponsoredOperationStore.getState().failOperation({
                    id: latestOperation.id,
                    status: 'receipt-timeout',
                    reason: 'BUNDLER_UNAVAILABLE',
                    retryable: false,
                  })
                }
                return
              }

              const operationWithEvidence =
                useSponsoredOperationStore.getState().operations
                  .find((item) => item.id === latestOperation.id)
              if (
                !operationWithEvidence ||
                !clearAuthorizationBeforeIncludedRelease(
                  operationWithEvidence
                )
              ) {
                return
              }
              const released = useSponsoredOperationStore
                .getState()
                .releaseLaneAfterSuccessfulInclusion(
                  latestOperation.id,
                  observation
                )
              if (released && holdsSubmissionLane) {
                await releaseBrowserLock()
                releaseBrowserLock = undefined
                holdsSubmissionLane = false
              }
              return
            }

            if (outcome?.kind === 'pending') {
              const currentOperation =
                useSponsoredOperationStore.getState().operations
                  .find((item) => item.id === latestOperation.id)
              const hasObservedInclusion =
                currentOperation !== undefined &&
                hasObservedSponsoredOperationInclusion(currentOperation)
              const inclusionWasReorged =
                hasObservedInclusion &&
                await observedInclusionIsReorged(
                  currentOperation,
                  runtime
                )
              const inclusionWasRetracted =
                inclusionWasReorged &&
                useSponsoredOperationStore
                  .getState()
                  .clearObservedInclusion(latestOperation.id)
              if (
                inclusionWasRetracted ||
                (
                  currentOperation !== undefined &&
                  isSponsoredOperationLaneBlocking(currentOperation) &&
                  !hasObservedInclusion
                )
              ) {
                // A proven reorg remains nonretryable attention but the
                // monotonic successful-inclusion marker prevents this older
                // nonce from reclaiming a lane now owned by newer work.
                useSponsoredOperationStore.getState().failOperation({
                  id: latestOperation.id,
                  status: 'receipt-timeout',
                  reason: 'BUNDLER_UNAVAILABLE',
                  retryable: false,
                })
              }
            }

            if (
              outcome &&
              (
                outcome.kind === 'confirmed' ||
                (
                  outcome.kind === 'terminal' &&
                  outcome.terminalStatus === 'execution-reverted'
                )
              ) &&
              outcome.transactionHash
            ) {
              useSponsoredOperationStore.getState().recordTransactionHash(
                latestOperation.id,
                outcome.transactionHash
              )
            }

            if (outcome?.kind === 'confirmed') {
              nextProtocolCheckAt.delete(latestOperation.id)
              clearRecoveredAuthorization(latestOperation)
              useSponsoredOperationStore.getState().transition(
                latestOperation.id,
                'confirmed'
              )
              return
            }

            if (outcome?.kind === 'terminal') {
              nextProtocolCheckAt.delete(latestOperation.id)
              useSponsoredOperationStore.getState().failOperation({
                id: latestOperation.id,
                status: outcome.terminalStatus,
                reason: outcome.terminalStatus,
                retryable: false,
              })
              return
            }

            const protocolNow = globalThis.performance.now()
            const nextCheckAt = nextProtocolCheckAt.get(latestOperation.id)
            if (
              nextCheckAt !== undefined &&
              protocolNow < nextCheckAt
            ) {
              return
            }
            nextProtocolCheckAt.set(
              latestOperation.id,
              protocolNow + PROTOCOL_RECOVERY_CHECK_INTERVAL_MS
            )
            const protocolResolution = await resolveProtocolOperation({
              operation: latestOperation,
              runtime,
              userOperationHash,
            })
            if (
              protocolResolution &&
              'transactionHash' in protocolResolution
            ) {
              useSponsoredOperationStore.getState().recordTransactionHash(
                latestOperation.id,
                protocolResolution.transactionHash
              )
            }
            if (protocolResolution?.status === 'confirmed') {
              nextProtocolCheckAt.delete(latestOperation.id)
              clearRecoveredAuthorization(latestOperation)
              useSponsoredOperationStore.getState().transition(
                latestOperation.id,
                'confirmed'
              )
              return
            }
            if (!protocolResolution) return

            if (protocolResolution.status !== 'outcome-unknown') {
              nextProtocolCheckAt.delete(latestOperation.id)
            }
            useSponsoredOperationStore.getState().failOperation({
              id: latestOperation.id,
              status: protocolResolution.status,
              reason: protocolResolution.status === 'expired'
                ? 'expired'
                : undefined,
              retryable: protocolResolution.status === 'expired',
              protocolNonceAdvanced:
                protocolResolution.status === 'outcome-unknown'
                  ? protocolResolution.protocolNonceAdvanced
                  : undefined,
            })
          } finally {
            await releaseBrowserLock?.()
          }
        }).catch(() => {
          // Another tab owns either the live submission lane or this exact
          // operation's reconciliation lock. Any coordination or hydration
          // failure remains fail-closed.
        }).finally(() => {
          recovering.delete(operation.id)
        })
      }
    }

    scan()
    const interval = globalThis.setInterval(scan, 5_000)

    return () => {
      globalThis.clearInterval(interval)
      recovering.clear()
      nextProtocolCheckAt.clear()
    }
  }, [accountAddress, runtime])

  return null
}
