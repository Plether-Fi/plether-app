import { useEffect } from 'react'
import { isAddressEqual, type Hex } from 'viem'
import { clearDepositAuthorization } from './authorizationStore'
import {
  hasSponsoredOperationSignal,
  hasObservedSponsoredOperationInclusion,
  isSponsoredOperationTerminal,
  restoreSponsoredOperationLane,
  SPONSORED_OPERATION_JOURNAL_PREFIX,
  SPONSORED_OPERATION_LANE_HEAD_PREFIX,
  SPONSORED_OPERATION_RESOLUTION_PREFIX,
  SPONSORED_OPERATION_STORAGE_NAME,
  type SponsoredOperation,
  useSponsoredOperationStore,
} from './operationStore'
import { acquireSponsoredOperationBrowserLane } from './laneLock'
import { reconcilePimlicoUserOperation } from './operationReconciler'
import { pimlicoSponsorshipValidUntil } from './paymasterValidity'
import { readPersistedManagedUserOperation } from './persistedUserOperation'
import {
  type ManagedUserOperation,
  type PerpsAaSmartAccountRuntime,
  usePerpsAaRuntime,
} from './runtimeContext'

const PROTOCOL_RECOVERY_CHECK_INTERVAL_MS = 60_000

type ProtocolOperationResolution =
  | {
      status: 'confirmed' | 'execution-reverted'
      transactionHash: Hex
    }
  | {
      status: 'expired'
    }
  | {
      status: 'outcome-unknown'
      protocolNonceAdvanced: true
    }

function clearRecoveredAuthorization(operation: SponsoredOperation): void {
  if (!operation.authorizationToken) return
  try {
    clearDepositAuthorization({
      chainId: operation.chainId,
      ownerAddress: operation.ownerAddress,
      accountAddress: operation.accountAddress,
      token: operation.authorizationToken,
    })
  } catch {
    // Chain confirmation is authoritative. A local cleanup failure must not
    // downgrade it, retain the lane, or invite the user to submit again.
  }
}

function verifiedPersistedUserOperation(
  operation: SponsoredOperation,
  runtime: PerpsAaSmartAccountRuntime,
  userOperationHash: Hex
): ManagedUserOperation | undefined {
  if (operation.submissionMetadataVersion !== 1) {
    return undefined
  }
  const signedOperation = readPersistedManagedUserOperation(
    operation.signedUserOperation
  )
  if (
    !signedOperation ||
    !isAddressEqual(
      signedOperation.sender,
      operation.accountAddress
    )
  ) {
    return undefined
  }
  const recomputedHash =
    runtime.smartAccount.getUserOperationHash(signedOperation)
  if (recomputedHash.toLowerCase() !== userOperationHash.toLowerCase()) {
    return undefined
  }
  return signedOperation
}

async function resolveProtocolOperation(input: {
  operation: SponsoredOperation
  runtime: PerpsAaSmartAccountRuntime
  userOperationHash: Hex
}): Promise<ProtocolOperationResolution | undefined> {
  if (input.runtime.getRecoverySnapshot === undefined) {
    return undefined
  }

  try {
    const signedOperation = verifiedPersistedUserOperation(
      input.operation,
      input.runtime,
      input.userOperationHash
    )
    const operationNonce = signedOperation?.nonce
    const validUntil = signedOperation
      ? pimlicoSponsorshipValidUntil(
          signedOperation.paymaster,
          signedOperation.paymasterData
        )
      : undefined

    const snapshot = await input.runtime.getRecoverySnapshot(
      input.userOperationHash,
      operationNonce === undefined ? 0n : operationNonce >> 64n
    )
    if (snapshot.userOperationEvidence.kind === 'included') {
      return {
        status: snapshot.userOperationEvidence.success
          ? 'confirmed'
          : 'execution-reverted',
        transactionHash:
          snapshot.userOperationEvidence.transactionHash,
      }
    }
    if (snapshot.userOperationEvidence.kind === 'not-safe-yet') {
      return undefined
    }

    if (operationNonce !== undefined) {
      if (snapshot.accountNonce > operationNonce) {
        return {
          status: 'outcome-unknown',
          protocolNonceAdvanced: true,
        }
      }
      if (
        validUntil !== undefined &&
        snapshot.blockTimestamp > validUntil &&
        snapshot.accountNonce === operationNonce
      ) {
        return { status: 'expired' }
      }
      if (snapshot.accountNonce < operationNonce) return undefined
    }

    return undefined
  } catch {
    // Corrupt persisted metadata or an unavailable chain/index read cannot
    // prove that rebuilding is safe.
    return undefined
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
              releaseBrowserLane()
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
        const now = globalThis.performance.now()
        if (
          operation.status === 'outcome-unknown' &&
          (nextProtocolCheckAt.get(operation.id) ?? 0) > now
        ) {
          continue
        }

        recovering.add(operation.id)
        const laneWasReleased = operation.status === 'outcome-unknown'
        const acquireRecoveryLane = laneWasReleased
          ? Promise.resolve<(() => void) | undefined>(undefined)
          : acquireSponsoredOperationBrowserLane({
              chainId: operation.chainId,
              accountAddress: operation.accountAddress,
              lane: operation.lane,
            })
        void acquireRecoveryLane.then(async (releaseBrowserLane) => {
          try {
            if (!laneWasReleased) {
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
            }
            const latestOperation =
              useSponsoredOperationStore.getState().operations
                .find((item) => item.id === operation.id)
            if (
              !latestOperation ||
              latestOperation.userOperationHash?.toLowerCase() !==
                userOperationHash.toLowerCase() ||
              (
                laneWasReleased
                  ? latestOperation.status !== 'outcome-unknown'
                  : isSponsoredOperationTerminal(latestOperation.status)
              ) ||
              hasSponsoredOperationSignal(latestOperation.id) ||
              !operationMatchesRuntime(
                latestOperation,
                runtime,
                accountAddress
              )
            ) {
              return
            }

            let outcome
            try {
              outcome = await reconcilePimlicoUserOperation({
                runtime,
                userOperationHash,
              })
            } catch {
              const currentOperation =
                useSponsoredOperationStore.getState().operations
                  .find((item) => item.id === latestOperation.id)
              let retainsObservedInclusion =
                currentOperation !== undefined &&
                hasObservedSponsoredOperationInclusion(currentOperation)
              if (
                currentOperation &&
                hasObservedSponsoredOperationInclusion(currentOperation) &&
                await observedInclusionIsReorged(
                  currentOperation,
                  runtime
                )
              ) {
                retainsObservedInclusion =
                  !useSponsoredOperationStore
                    .getState()
                    .clearObservedInclusion(latestOperation.id)
              }
              if (
                !laneWasReleased &&
                !retainsObservedInclusion
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
              const inclusionPersisted =
                useSponsoredOperationStore
                  .getState()
                  .recordObservedInclusion(
                    latestOperation.id,
                    {
                      transactionHash: outcome.transactionHash,
                      blockNumber:
                        outcome.receipt.receipt.blockNumber.toString(),
                      blockHash: outcome.receipt.receipt.blockHash,
                    }
                  )
              if (!inclusionPersisted) {
                const currentOperation =
                  useSponsoredOperationStore.getState().operations
                    .find((item) => item.id === latestOperation.id)
                if (
                  !laneWasReleased &&
                  (
                    !currentOperation ||
                    !hasObservedSponsoredOperationInclusion(currentOperation)
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
              return
            }

            if (outcome?.kind === 'pending' && !laneWasReleased) {
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
              if (!hasObservedInclusion || inclusionWasRetracted) {
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

            if (
              outcome?.kind === 'terminal' &&
              (
                !laneWasReleased ||
                outcome.terminalStatus === 'execution-reverted'
              )
            ) {
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
            releaseBrowserLane?.()
          }
        }).catch(() => {
          // A live submission in another tab owns the same browser lane. Any
          // coordination or hydration failure remains fail-closed.
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
