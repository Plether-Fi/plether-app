import type {
  PerpsActionPlan,
  SponsoredExecutionResult,
  SponsoredExecutionStatus,
} from '@plether/perps-aa-client'
import { isAddressEqual, type Address } from 'viem'
import { createPletherBundlerAdapter } from './adapters/bundler'
import { createPletherSponsorAdapter } from './adapters/sponsor'
import { clearDepositAuthorization } from './authorizationStore'
import { sendSponsoredActionWithRestart } from './client'
import { SponsorRequestError } from './errors'
import { acquireSponsoredOperationBrowserLane } from './laneLock'
import type { PerpsAaDeploymentManifest } from './manifest'
import { beginSponsoredOperationTracking } from './operationTracker'
import {
  DEFAULT_SPONSORED_OPERATION_LANE,
  useSponsoredOperationStore,
} from './operationStore'
import type { PerpsAaSmartAccountRuntime } from './runtimeContext'
import {
  extractUserOperationTransactionHash,
  type UserOperationReceiptV08,
} from './types'

export interface ExecuteSponsoredPerpsActionInput {
  manifest: PerpsAaDeploymentManifest
  ownerAddress: Address
  action: PerpsActionPlan
  runtime: PerpsAaSmartAccountRuntime
  authorizationTokenToClearOnConfirmation?: Address
  lane?: string
  fetcher?: typeof fetch
  onStatus?: (status: SponsoredExecutionStatus) => void
}

export interface ExecuteSponsoredPerpsActionResult
  extends SponsoredExecutionResult<UserOperationReceiptV08> {
  transactionHash?: `0x${string}`
}

export async function executeSponsoredPerpsAction(
  input: ExecuteSponsoredPerpsActionInput
): Promise<ExecuteSponsoredPerpsActionResult> {
  if (!input.manifest.sponsorshipEnabled) {
    throw new SponsorRequestError({
      reason: 'SPONSOR_UNAVAILABLE',
      message: 'Gas sponsorship is disabled by the deployment manifest',
      retryable: true,
    })
  }
  const factoryMatches =
    (
      input.runtime.factoryAddress === null &&
      input.manifest.smartAccountFactory === null
    ) ||
    (
      input.runtime.factoryAddress !== null &&
      input.manifest.smartAccountFactory !== null &&
      isAddressEqual(
        input.runtime.factoryAddress,
        input.manifest.smartAccountFactory
      )
    )
  if (
    input.runtime.chainId !== input.manifest.chainId ||
    !isAddressEqual(input.runtime.ownerAddress, input.ownerAddress) ||
    !factoryMatches ||
    !isAddressEqual(
      input.runtime.smartAccount.entryPoint,
      input.manifest.entryPoint
    )
  ) {
    throw new SponsorRequestError({
      reason: 'ACCOUNT_NOT_TRUSTED',
      message: 'The smart-account adapter owner, chain, factory, or EntryPoint does not match the reviewed manifest',
      retryable: false,
    })
  }
  if (
    !isAddressEqual(
      input.runtime.implementationAddress,
      input.manifest.smartAccountImplementation
    ) ||
    input.runtime.accountRuntimeCodeHash.toLowerCase() !==
      input.manifest.accountRuntimeCodeHash.toLowerCase()
  ) {
    throw new SponsorRequestError({
      reason: 'ACCOUNT_NOT_TRUSTED',
      message: 'The smart-account runtime does not match the reviewed manifest',
      retryable: false,
    })
  }

  const lane = input.lane ?? DEFAULT_SPONSORED_OPERATION_LANE
  const releaseBrowserLane = await acquireSponsoredOperationBrowserLane({
    chainId: input.manifest.chainId,
    accountAddress: input.runtime.smartAccount.accountAddress,
    lane,
  })
  let tracker: ReturnType<typeof beginSponsoredOperationTracking> | undefined

  try {
    await useSponsoredOperationStore.persist.rehydrate()
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

    const sponsor = createPletherSponsorAdapter({
      rpcUrl: input.manifest.sponsorServiceRpcUrl,
      manifestVersion: input.manifest.version,
      policyId: input.manifest.policyId,
      expectedPaymaster: input.manifest.paymaster,
      getSignal: () => activeTracker.signal,
      fetcher: input.fetcher,
    })
    const bundler = createPletherBundlerAdapter({
      rpcUrl: input.manifest.bundlerRpcUrl,
      getSignal: () => activeTracker.signal,
      fetcher: input.fetcher,
      expectedSender: input.runtime.smartAccount.accountAddress,
      expectedPaymaster: input.manifest.paymaster,
      onUserOperationHash: activeTracker.onUserOperationHash,
      onTransactionHash: activeTracker.onTransactionHash,
    })

    const result = await sendSponsoredActionWithRestart({
      chainId: input.manifest.chainId,
      action: input.action,
      account: input.runtime.smartAccount,
      sponsor,
      bundler,
      waitForReceipt: true,
      onStatus: (status) => {
        activeTracker.onStatus(status)
        input.onStatus?.(status)
      },
      onEstimationRestart: activeTracker.onEstimationRestart,
    })

    const executionResult = {
      ...result,
      transactionHash: result.receipt
        ? extractUserOperationTransactionHash(result.receipt)
        : undefined,
    }
    if (input.authorizationTokenToClearOnConfirmation) {
      clearDepositAuthorization({
        chainId: input.manifest.chainId,
        ownerAddress: input.ownerAddress,
        accountAddress: input.runtime.smartAccount.accountAddress,
        token: input.authorizationTokenToClearOnConfirmation,
      })
    }
    return executionResult
  } catch (error) {
    tracker?.fail(error)
    throw error
  } finally {
    tracker?.release()
    releaseBrowserLane()
  }
}
