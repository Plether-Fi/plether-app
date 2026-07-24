import type {
  PerpsActionPlan,
  SponsoredExecutionStatus,
} from '@plether/perps-aa-client'
import {
  isAddressEqual,
  type Address,
  type Hex,
} from 'viem'
import { clearDepositAuthorization } from './authorizationStore'
import {
  asSponsorRequestError,
  BundlerRequestError,
  SponsorRequestError,
  SponsoredPreflightError,
} from './errors'
import { acquireSponsoredOperationBrowserLane } from './laneLock'
import type { PerpsAaDeploymentManifest } from './manifest'
import {
  beginSponsoredOperationTracking,
  trackSponsoredOperationPreflightFailure,
} from './operationTracker'
import { reconcilePimlicoUserOperation } from './operationReconciler'
import {
  DEFAULT_SPONSORED_OPERATION_LANE,
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
  lane?: string
  onStatus?: (status: SponsoredExecutionStatus) => void
}

export interface ExecuteSponsoredPerpsActionResult {
  userOperationHash: Hex
  receipt: ManagedUserOperationReceipt
  transactionHash: Hex
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
  timeoutMs?: number
  pollIntervalMs?: number
}): Promise<ManagedUserOperationReceipt> {
  const startedAt = Date.now()
  const timeoutMs = input.timeoutMs ?? 120_000
  const pollIntervalMs = input.pollIntervalMs ?? 1_500

  while (Date.now() - startedAt < timeoutMs) {
    input.signal.throwIfAborted()
    try {
      const outcome = await reconcilePimlicoUserOperation({
        runtime: input.runtime,
        userOperationHash: input.userOperationHash,
      })
      if (outcome.transactionHash) {
        input.onTransactionHash(outcome.transactionHash)
      }
      if (outcome.kind === 'confirmed') {
        return outcome.receipt
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
  let releaseBrowserLane: (() => void) | undefined
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
      await useSponsoredOperationStore.persist.rehydrate()
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

    activeTracker.signal.throwIfAborted()
    status('awaiting-signature')
    const signedOperation =
      await input.runtime.smartAccount.signUserOperation(operation)
    activeTracker.signal.throwIfAborted()

    // The EntryPoint hash excludes the account signature, but includes all
    // nonce, gas, factory and managed-paymaster fields. Persist it before the
    // first network submission so an ambiguous response can only be reconciled,
    // never retried as a fresh owner-EOA transaction.
    const localUserOperationHash =
      input.runtime.smartAccount.getUserOperationHash(signedOperation)
    activeTracker.onUserOperationHash(localUserOperationHash)

    status('submitting')
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
    const receipt = await waitForPimlicoOutcome({
      runtime: input.runtime,
      userOperationHash: localUserOperationHash,
      signal: activeTracker.signal,
      onTransactionHash: activeTracker.onTransactionHash,
    })
    status('confirmed')

    if (input.authorizationTokenToClearOnConfirmation) {
      try {
        clearDepositAuthorization({
          chainId: input.manifest.chainId,
          ownerAddress: input.ownerAddress,
          accountAddress: input.runtime.smartAccount.accountAddress,
          token: input.authorizationTokenToClearOnConfirmation,
        })
      } catch {
        // The operation is already authoritatively confirmed. Local cleanup
        // must never downgrade it to an ambiguous state or invite a retry.
      }
    }

    return {
      userOperationHash: localUserOperationHash,
      receipt,
      transactionHash: receipt.receipt.transactionHash,
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
    releaseBrowserLane?.()
  }
}
