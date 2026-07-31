import type {
  PerpsActionPlan,
  SponsoredExecutionStatus,
} from '@plether/perps-aa-client'
import {
  isAddressEqual,
  type Address,
  type Hex,
} from 'viem'
import {
  clearDepositAuthorization,
  clearLegacyDepositAuthorization,
} from './authorizationStore'
import {
  asSponsorRequestError,
  BundlerRequestError,
  SponsorRequestError,
  SponsoredPreflightError,
} from './errors'
import {
  acquireSponsoredOperationBrowserLane,
  type ReleaseSponsoredOperationBrowserLock,
} from './laneLock'
import type { PerpsAaDeploymentManifest } from './manifest'
import {
  beginSponsoredOperationTracking,
  trackSponsoredOperationPreflightFailure,
} from './operationTracker'
import { reconcilePimlicoUserOperation } from './operationReconciler'
import { pimlicoSponsorshipValidUntil } from './paymasterValidity'
import {
  DEFAULT_SPONSORED_OPERATION_LANE,
  hasDurableSponsoredOperationSubmission,
  restoreSponsoredOperationLane,
  type SponsoredOperationInclusionObservation,
  useSponsoredOperationStore,
} from './operationStore'
import type {
  ManagedUserOperationReceipt,
  PerpsAaSmartAccountRuntime,
} from './runtimeContext'

export interface ExecuteSponsoredPerpsActionInput {
  manifest: PerpsAaDeploymentManifest
  ownerAddress: Address
  action: PerpsActionPlan
  runtime: PerpsAaSmartAccountRuntime
  authorizationTokenToClearOnConfirmation?: Address
  authorizationNonceToClearOnConfirmation?: Hex
  lane?: string
  onStatus?: (status: SponsoredExecutionStatus) => void
  onIncluded?: (result: ExecuteSponsoredPerpsActionResult) => void
}

export interface ExecuteSponsoredPerpsActionResult {
  userOperationHash: Hex
  receipt: ManagedUserOperationReceipt
  transactionHash: Hex
}

type PimlicoWaitOutcome =
  | {
      kind: 'included'
      receipt: ManagedUserOperationReceipt
    }
  | {
      kind: 'confirmed'
      receipt: ManagedUserOperationReceipt
      inclusionReleased: boolean
    }

function wait(milliseconds: number, signal: AbortSignal): Promise<void> {
  return new Promise((resolve, reject) => {
    signal.throwIfAborted()
    const timeout = globalThis.setTimeout(() => {
      signal.removeEventListener('abort', onAbort)
      resolve()
    }, milliseconds)
    const onAbort = () => {
      globalThis.clearTimeout(timeout)
      reject(
        signal.reason instanceof Error
          ? signal.reason
          : new DOMException('Pimlico request cancelled', 'AbortError')
      )
    }
    signal.addEventListener('abort', onAbort, { once: true })
  })
}

function asBundlerError(error: unknown): BundlerRequestError {
  if (error instanceof BundlerRequestError) return error
  return new BundlerRequestError({
    message: error instanceof Error ? error.message : String(error),
    // The exact operation hash is persisted before submission. A transport
    // error cannot prove that Pimlico did not receive it, so submitting again
    // is unsafe; recovery must reconcile the existing hash.
    retryable: false,
    terminalStatus: 'receipt-timeout',
    cause: error,
  })
}

async function waitForPimlicoOutcome(input: {
  runtime: PerpsAaSmartAccountRuntime
  userOperationHash: Hex
  signal: AbortSignal
  onTransactionHash: (hash: Hex) => void
  onObservedInclusion: (
    observation: SponsoredOperationInclusionObservation
  ) => boolean
  onInclusionRetracted?: () => boolean
  onSuccessfulInclusion: (
    observation: SponsoredOperationInclusionObservation & { success: true }
  ) => Promise<boolean>
  onIncluded?: (receipt: ManagedUserOperationReceipt) => void
  timeoutMs?: number
  pollIntervalMs?: number
}): Promise<PimlicoWaitOutcome> {
  const startedAt = Date.now()
  const timeoutMs = input.timeoutMs ?? 120_000
  const pollIntervalMs = input.pollIntervalMs ?? 1_500
  let persistedInclusion:
    SponsoredOperationInclusionObservation | undefined
  let reportedInclusionHash: Hex | undefined
  let lastReconciliationError: unknown

  const reportInclusion = async (
    receipt: ManagedUserOperationReceipt
  ): Promise<boolean> => {
    // A not-yet-safe exact receipt can also prove execution failure. Keep the
    // submission lane locked until that result reaches the safe head and can
    // be recorded as an authoritative terminal outcome.
    if (!receipt.success || receipt.receipt.status !== 'success') {
      return false
    }
    const transactionHash = receipt.receipt.transactionHash
    const observation: SponsoredOperationInclusionObservation & {
      success: true
    } = {
      transactionHash,
      blockNumber: receipt.receipt.blockNumber.toString(),
      blockHash: receipt.receipt.blockHash,
      success: true,
    }
    try {
      if (!input.onObservedInclusion(observation)) {
        throw new Error(
          'The latest-chain inclusion could not be persisted for recovery'
        )
      }
      persistedInclusion = observation
      if (!await input.onSuccessfulInclusion(observation)) {
        throw new Error(
          'The successful inclusion could not durably release its submission lane'
        )
      }
      if (
        reportedInclusionHash?.toLowerCase() !==
          transactionHash.toLowerCase()
      ) {
        try {
          input.onIncluded?.(receipt)
        } catch {
          // The canonical successful receipt is already durable and its lane
          // is released. An optional UI consumer cannot turn that fact into a
          // retry or keep later account work blocked.
        }
        reportedInclusionHash = transactionHash
      }
      lastReconciliationError = undefined
      return true
    } catch (error) {
      lastReconciliationError = error
      return false
    }
  }

  const retractInclusionIfReorged = async () => {
    if (
      persistedInclusion?.blockNumber === undefined ||
      persistedInclusion.blockHash === undefined ||
      input.runtime.verifyObservedInclusion === undefined
    ) {
      return
    }
    let canonicality
    try {
      canonicality = await input.runtime.verifyObservedInclusion({
        transactionHash: persistedInclusion.transactionHash,
        blockNumber: BigInt(persistedInclusion.blockNumber),
        blockHash: persistedInclusion.blockHash,
      })
    } catch {
      return
    }
    if (canonicality !== 'reorged') return

    try {
      if (
        input.onInclusionRetracted &&
        !input.onInclusionRetracted()
      ) {
        throw new Error(
          'The reorged latest-chain inclusion could not be retracted'
        )
      }
      persistedInclusion = undefined
      reportedInclusionHash = undefined
      lastReconciliationError = undefined
    } catch (error) {
      lastReconciliationError = error
    }
  }

  while (Date.now() - startedAt < timeoutMs) {
    input.signal.throwIfAborted()
    try {
      const outcome = await reconcilePimlicoUserOperation({
        runtime: input.runtime,
        userOperationHash: input.userOperationHash,
      })
      if (outcome.kind === 'included') {
        if (await reportInclusion(outcome.receipt)) {
          return { kind: 'included', receipt: outcome.receipt }
        }
      }
      if (outcome.kind === 'pending') {
        // Pimlico can temporarily miss a receipt it previously indexed.
        // Retract only when the chain RPC proves that the exact observed block
        // hash was replaced.
        await retractInclusionIfReorged()
      }
      if (
        (outcome.kind === 'confirmed' || outcome.kind === 'terminal') &&
        outcome.transactionHash
      ) {
        input.onTransactionHash(outcome.transactionHash)
      }
      if (outcome.kind === 'confirmed') {
        // A safe successful receipt is terminal authority even if provisional
        // evidence or the early-release marker cannot be persisted. The
        // terminal transition below has its own durable lane release path.
        const inclusionReleased = await reportInclusion(outcome.receipt)
        return {
          kind: 'confirmed',
          receipt: outcome.receipt,
          inclusionReleased,
        }
      }
      if (outcome.kind === 'terminal') {
        throw new BundlerRequestError({
          message: outcome.message,
          retryable: false,
          terminalStatus: outcome.terminalStatus,
        })
      }
    } catch (error) {
      if (
        error instanceof BundlerRequestError &&
        error.terminalStatus !== undefined
      ) {
        throw error
      }
      lastReconciliationError = error
      await retractInclusionIfReorged()
      // Receipt and status requests can race with Pimlico's indexer or fail
      // transiently. Keep reconciling the already-persisted local hash.
    }

    await wait(pollIntervalMs, input.signal)
  }

  throw new BundlerRequestError({
    message:
      'Timed out reconciling the locally persisted UserOperation hash with Pimlico',
    retryable: false,
    terminalStatus: 'receipt-timeout',
    cause: lastReconciliationError,
  })
}

export async function executeSponsoredPerpsAction(
  input: ExecuteSponsoredPerpsActionInput
): Promise<ExecuteSponsoredPerpsActionResult> {
  const lane = input.lane ?? DEFAULT_SPONSORED_OPERATION_LANE
  const analyticsMetadata = {
    accountMode: input.manifest.smartAccountMode,
    manifestVersion: input.manifest.version,
    action: input.action.kind,
    walletFamily: input.runtime.walletFamily,
    walletVersion: input.runtime.walletVersion,
  }
  let releaseBrowserLane: ReleaseSponsoredOperationBrowserLock | undefined
  let tracker: ReturnType<typeof beginSponsoredOperationTracking> | undefined

  try {
    if (!input.manifest.sponsorshipEnabled) {
      throw new SponsorRequestError({
        reason: 'SPONSOR_UNAVAILABLE',
        message: 'Gas sponsorship is disabled by the deployment manifest',
        retryable: true,
      })
    }
    if (
      input.runtime.chainId !== input.manifest.chainId ||
      !isAddressEqual(input.runtime.ownerAddress, input.ownerAddress) ||
      !isAddressEqual(
        input.runtime.factoryAddress,
        input.manifest.smartAccountFactory
      ) ||
      input.runtime.accountVersion !== input.manifest.smartAccountVersion ||
      input.runtime.accountIndex !== input.manifest.smartAccountIndex ||
      !isAddressEqual(
        input.runtime.smartAccount.entryPoint,
        input.manifest.entryPoint
      )
    ) {
      throw new SponsorRequestError({
        reason: 'ACCOUNT_NOT_TRUSTED',
        message:
          'The permissionless.js Trading Account does not match the reviewed manifest',
        retryable: false,
      })
    }
    if (
      !isAddressEqual(
        input.action.account,
        input.runtime.smartAccount.accountAddress
      )
    ) {
      throw new SponsorRequestError({
        reason: 'ACCOUNT_NOT_TRUSTED',
        message: 'The Plether action is bound to a different Trading Account',
        retryable: false,
      })
    }

    releaseBrowserLane = await acquireSponsoredOperationBrowserLane({
      chainId: input.manifest.chainId,
      accountAddress: input.runtime.smartAccount.accountAddress,
      lane,
    })
    try {
      restoreSponsoredOperationLane({
        chainId: input.manifest.chainId,
        accountAddress: input.runtime.smartAccount.accountAddress,
        lane,
      })
      await useSponsoredOperationStore.persist.rehydrate()
      restoreSponsoredOperationLane({
        chainId: input.manifest.chainId,
        accountAddress: input.runtime.smartAccount.accountAddress,
        lane,
      })
    } catch (error) {
      throw new SponsoredPreflightError({
        reason: 'OPERATION_STORE_UNAVAILABLE',
        message: 'Unable to restore the sponsored operation activity store',
        cause: error,
      })
    }
    const activeTracker = beginSponsoredOperationTracking({
      ownerAddress: input.ownerAddress,
      accountAddress: input.runtime.smartAccount.accountAddress,
      chainId: input.manifest.chainId,
      accountMode: input.manifest.smartAccountMode,
      manifestVersion: input.manifest.version,
      action: input.action.kind,
      authorizationToken: input.authorizationTokenToClearOnConfirmation,
      authorizationNonce: input.authorizationNonceToClearOnConfirmation,
      lane,
      walletFamily: input.runtime.walletFamily,
      walletVersion: input.runtime.walletVersion,
    })
    tracker = activeTracker

    const status = (next: SponsoredExecutionStatus) => {
      activeTracker.onStatus(next)
      input.onStatus?.(next)
    }

    activeTracker.signal.throwIfAborted()
    status('requesting-sponsorship')
    let operation
    try {
      operation = await input.runtime.smartAccount.prepareUserOperation({
        calls: input.action.calls,
        action: input.action.kind,
      })
    } catch (error) {
      throw asSponsorRequestError(error)
    }
    if (
      pimlicoSponsorshipValidUntil(
        operation.paymaster,
        operation.paymasterData
      ) === undefined
    ) {
      throw new SponsorRequestError({
        reason: 'SPONSOR_UNAVAILABLE',
        message:
          'Pimlico returned a sponsorship format without a recoverable validity deadline',
        retryable: false,
      })
    }

    activeTracker.signal.throwIfAborted()
    status('awaiting-signature')
    const signedOperation =
      await input.runtime.smartAccount.signUserOperation(operation)
    activeTracker.signal.throwIfAborted()

    // Wallet approval can remain open long enough for another tab's legacy
    // storage event to hydrate into this tab. Re-read and bulk-guard the lane
    // under the still-held Web Lock immediately before binding the signed
    // hash; recordUserOperationHash will then reject every competing record.
    restoreSponsoredOperationLane({
      chainId: input.manifest.chainId,
      accountAddress: input.runtime.smartAccount.accountAddress,
      lane,
    })

    // The EntryPoint hash excludes the account signature, but includes all
    // nonce, gas, factory and managed-paymaster fields. Persist it before the
    // first network submission so an ambiguous response can only be reconciled,
    // never retried as a fresh owner-EOA transaction.
    const localUserOperationHash =
      input.runtime.smartAccount.getUserOperationHash(signedOperation)
    const submissionStatePersisted =
      activeTracker.onUserOperationHash(localUserOperationHash, {
        signedUserOperation: signedOperation,
      })
    if (!submissionStatePersisted) {
      throw new SponsoredPreflightError({
        reason: 'OPERATION_STORE_UNAVAILABLE',
        message:
          'The signed UserOperation could not be bound to its recovery record',
      })
    }

    status('submitting')
    // No user callback runs after this point. Reconcile any storage event that
    // landed during the status update, then exact-check the singleton head,
    // signed journal, shared snapshot, and persistence revision immediately
    // before invoking Pimlico.
    restoreSponsoredOperationLane({
      chainId: input.manifest.chainId,
      accountAddress: input.runtime.smartAccount.accountAddress,
      lane,
    })
    if (
      !hasDurableSponsoredOperationSubmission(
        activeTracker.id,
        localUserOperationHash
      )
    ) {
      throw new SponsoredPreflightError({
        reason: 'OPERATION_STORE_UNAVAILABLE',
        message:
          'The signed UserOperation recovery record changed before submission',
      })
    }
    let returnedUserOperationHash: Hex
    try {
      returnedUserOperationHash =
        await input.runtime.smartAccount.sendUserOperation(signedOperation)
    } catch (error) {
      throw asBundlerError(error)
    }
    if (
      returnedUserOperationHash.toLowerCase() !==
      localUserOperationHash.toLowerCase()
    ) {
      throw new BundlerRequestError({
        message:
          'Pimlico returned a different hash for the submitted UserOperation',
        retryable: false,
        terminalStatus: 'receipt-timeout',
      })
    }

    status('confirming')
    const outcome = await waitForPimlicoOutcome({
      runtime: input.runtime,
      userOperationHash: localUserOperationHash,
      signal: activeTracker.signal,
      onTransactionHash: activeTracker.onTransactionHash,
      onObservedInclusion: activeTracker.onObservedInclusion,
      onInclusionRetracted: activeTracker.onInclusionRetracted,
      onSuccessfulInclusion: async (observation) => {
        if (input.authorizationTokenToClearOnConfirmation) {
          // New EIP-3009 submissions always carry the authorization nonce.
          // Legacy records without it stay lane-blocking until safe recovery
          // because an unowned cleanup could erase a newer authorization.
          if (!input.authorizationNonceToClearOnConfirmation) return false
          clearDepositAuthorization({
            chainId: input.manifest.chainId,
            ownerAddress: input.ownerAddress,
            accountAddress: input.runtime.smartAccount.accountAddress,
            token: input.authorizationTokenToClearOnConfirmation,
            expectedNonce: input.authorizationNonceToClearOnConfirmation,
          })
        }
        const released = useSponsoredOperationStore
          .getState()
          .releaseLaneAfterSuccessfulInclusion(
            activeTracker.id,
            observation
          )
        if (released) {
          // Release before the optional onIncluded consumer runs so a callback
          // that starts the next action sees the durable lane state and the
          // matching browser-wide lock state atomically.
          await releaseBrowserLane?.()
          releaseBrowserLane = undefined
        }
        return released
      },
      onIncluded: (includedReceipt) => {
        input.onIncluded?.({
          userOperationHash: localUserOperationHash,
          receipt: includedReceipt,
          transactionHash: includedReceipt.receipt.transactionHash,
        })
      },
    })
    if (outcome.kind === 'confirmed') {
      if (
        input.authorizationTokenToClearOnConfirmation &&
        !outcome.inclusionReleased
      ) {
        try {
          if (input.authorizationNonceToClearOnConfirmation) {
            clearDepositAuthorization({
              chainId: input.manifest.chainId,
              ownerAddress: input.ownerAddress,
              accountAddress: input.runtime.smartAccount.accountAddress,
              token: input.authorizationTokenToClearOnConfirmation,
              expectedNonce:
                input.authorizationNonceToClearOnConfirmation,
            })
          } else {
            // Token-only metadata belongs to the old singleton cache. At the
            // safe head retire only v1; deleting v2 here could erase a newer
            // authorization created by another tab.
            clearLegacyDepositAuthorization({
              chainId: input.manifest.chainId,
              ownerAddress: input.ownerAddress,
              accountAddress: input.runtime.smartAccount.accountAddress,
              token: input.authorizationTokenToClearOnConfirmation,
            })
          }
        } catch {
          // Safe chain confirmation remains authoritative. This best-effort
          // cleanup runs before the terminal transition; storage failure must
          // not downgrade the operation or invite an onchain retry.
        }
      }
      status('confirmed')
    }

    return {
      userOperationHash: localUserOperationHash,
      receipt: outcome.receipt,
      transactionHash: outcome.receipt.receipt.transactionHash,
    }
  } catch (error) {
    if (tracker) {
      tracker.fail(error)
    } else {
      trackSponsoredOperationPreflightFailure(analyticsMetadata, error)
    }
    throw error
  } finally {
    tracker?.release()
    await releaseBrowserLane?.()
  }
}
