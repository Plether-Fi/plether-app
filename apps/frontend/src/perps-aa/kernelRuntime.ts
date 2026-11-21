import type { PerpsActionPlan } from '@plether-fi/perps-aa-client'
import { bytesToHex, isAddressEqual, keccak256, type Address, type Hex, type PublicClient, type WalletClient } from 'viem'
import { trackPerpsKernelOutcome } from '../analytics/perps'
import { deadlineNow, refreshDeadlineClock } from './deadlineClock'
import { BundlerRequestError, SponsorRequestError } from './errors'
import type { ExecuteSponsoredPerpsActionInput, ExecuteSponsoredPerpsActionResult } from './execution'
import { readGasPaymentPreference, type GasPaymentMode } from './gasPaymentPreference'
import { createKernelTradingAccount, encodeKernelCalls, verifyKernelTradingAccountAtBlock, type VerifiedKernelTradingAccount } from './kernelAccount'
import { kernelSepoliaCanaryPaymaster } from './kernelDeploymentPins'
import {
  getActiveKernelAttempt, getKernelAttempt, refreshKernelAttempt, saveKernelSignedAttempt, submitKernelAttempt, withKernelOperationLane,
  type KernelAttemptScope, type KernelOperationAttempt,
} from './kernelOperationJournal'
import { assertKernelPreparedOperation, fetchKernelManifest, KernelRpcError, prepareKernelCancellation, prepareKernelOperation, type KernelPreparedCancellation, type KernelPreparedOperation } from './kernelRpc'
import type { KernelPerpsAaDeploymentManifest } from './manifest'
import { currentReadiness, readinessBlocker, readinessMessage, refreshReadiness } from './readiness'
import { readReviewedActionState } from './reviewedActionState'
import type { KernelPerpsAaSmartAccountRuntime } from './runtimeContext'
import { hasKernelOrderSubmissionTime } from './kernelOrderDeadline'

export interface KernelActionQuote {
  readonly mode: GasPaymentMode
  readonly maxNetworkGasWei: bigint
  readonly expiresAt: number
}

export interface KernelRuntime {
  scope: KernelAttemptScope
  verifyAccount: () => Promise<VerifiedKernelTradingAccount>
  quoteAction(action: PerpsActionPlan, signal?: AbortSignal, clientOrderId?: Hex): Promise<KernelActionQuote>
  execute(input: ExecuteSponsoredPerpsActionInput): Promise<ExecuteSponsoredPerpsActionResult>
  quoteCancellation: (original: KernelPreparedOperation, originalSignature: Hex) => Promise<KernelPreparedCancellation>
  signPrepared: (prepared: KernelPreparedOperation) => Promise<Hex>
}

interface ReviewedKernelPreparation {
  accountKey: string
  preferenceRevision: string
  action: PerpsActionPlan['kind']
  clientOrderId?: Hex
  reviewedState: string
  prepared: KernelPreparedOperation
}

const reviewedPreparations = new WeakMap<KernelActionQuote, ReviewedKernelPreparation>()
const reviewedCancellations = new WeakMap<KernelPreparedOperation, { accountKey: string; hash: Hex }>()
const newPreparationId = () => bytesToHex(globalThis.crypto.getRandomValues(new Uint8Array(32)))

function requireFresh(prepared: KernelPreparedOperation, now = deadlineNow()) {
  if (prepared.expiresAt * 1_000 <= now) throw new Error('The gas quote expired. Review a fresh quote to continue.')
}

async function waitForCanonicalAttempt(attempt: KernelOperationAttempt) {
  const timeout = performance.now() + 75_000
  while (performance.now() < timeout) {
    let resolved: KernelOperationAttempt | undefined
    try {
      const saved = await getKernelAttempt(attempt.scope, attempt.prepared.userOperationHash)
      if (saved) resolved = await refreshKernelAttempt(saved)
    } catch {
      // Loss of status connectivity leaves the signed operation unresolved.
    }
    if (resolved && !['sending', 'submitted'].includes(resolved.phase)) return resolved
    await new Promise<void>(resolve => globalThis.setTimeout(resolve, 2_000))
  }
  throw new BundlerRequestError({
    message: 'Confirm the pending Trading Account operation in Activity before submitting another action.',
    terminalStatus: 'receipt-timeout', retryable: false,
  })
}

export async function createKernelRuntime({ manifest, ownerAddress, walletClient, publicClient }: {
  manifest: KernelPerpsAaDeploymentManifest
  ownerAddress: Address
  walletClient: WalletClient
  publicClient: PublicClient
}): Promise<KernelPerpsAaSmartAccountRuntime> {
  const { profile, readiness } = manifest.kernel
  if (publicClient.chain?.id !== profile.chainId || walletClient.chain?.id !== profile.chainId ||
      !walletClient.account || !isAddressEqual(walletClient.account.address, ownerAddress)) {
    throw new Error('The connected wallet does not match this Kernel deployment')
  }
  const verifyAt = async ({ number, hash }: { number: bigint; hash: Hex }, expectedAddress: Address) => {
    const verified = await verifyKernelTradingAccountAtBlock(publicClient, profile, ownerAddress, number, expectedAddress)
    const block = await publicClient.getBlock({ blockNumber: number })
    if (block.hash !== hash) throw new Error('The Kernel account verification block changed. Check the Trading Account again.')
    return verified
  }
  const initialBlock = await publicClient.getBlock({ blockTag: 'safe' })
  const account = await createKernelTradingAccount(publicClient, profile, {
    address: ownerAddress,
    signMessage: ({ message }) => walletClient.signMessage({ account: ownerAddress, message }),
  }, initialBlock.number)
  await verifyAt(initialBlock, account.address)
  const scope = { deployment: profile.deployment, chainId: profile.chainId, entryPoint: profile.entryPoint, sender: account.address }
  const preferenceScope = { deployment: scope.deployment, chainId: scope.chainId, ownerAddress, sender: scope.sender }
  const accountKey = JSON.stringify([profile, manifest.kernel.paymaster, ownerAddress.toLowerCase(), account.address.toLowerCase()])

  const verifyAccount = async () => verifyAt(await publicClient.getBlock({ blockTag: 'safe' }), scope.sender)
  const factoryAtSafe = async () => (await verifyAccount()).deployed ? undefined : account.factoryArgs
  const assertConnectedOwner = async () => {
    const [addresses, chainId] = await Promise.all([walletClient.getAddresses(), walletClient.getChainId()])
    const selected = addresses.at(0)
    if (chainId !== scope.chainId || !selected || !isAddressEqual(selected, ownerAddress)) {
      throw new Error('Reconnect the Trading Account owner on Arbitrum Sepolia before approving')
    }
  }
  const requireNonce = async (expectedNonce: bigint) => {
    if (await account.getNonce() !== expectedNonce) throw new Error('The Trading Account nonce changed. Review a fresh quote to continue.')
  }
  const sign = async (prepared: KernelPreparedOperation, beforePrompt?: () => void) => {
    assertKernelPreparedOperation(prepared)
    await Promise.all([assertConnectedOwner(), requireNonce(prepared.operation.nonce)])
    requireFresh(prepared)
    beforePrompt?.()
    return account.signUserOperation(prepared.operation)
  }

  const kernel: KernelRuntime = {
    scope,
    verifyAccount,
    quoteAction: async (action, signal, clientOrderId) => {
      if (!isAddressEqual(action.account, scope.sender)) throw new Error('The action belongs to a different Trading Account')
      const { mode, revision } = readGasPaymentPreference(preferenceScope)
      if (!readiness[mode]) {
        const error = new KernelRpcError('PAYMENT_MODE_UNAVAILABLE', mode === 'sponsored'
          ? 'Plether gas sponsorship is unavailable. Open Trading Account activity to change the gas setting.'
          : 'Paying with ETH is unavailable for this Trading Account.', true)
        trackPerpsKernelOutcome({ mode, outcome: 'quote_failed', error })
        throw error
      }
      if (await getActiveKernelAttempt(scope)) throw new Error('Resolve the pending operation in Trading Account activity before getting a new gas quote')
      const callData = encodeKernelCalls(action.calls)
      const [factory, expectedNonce] = await Promise.all([factoryAtSafe(), account.getNonce()])
      const reviewedState = await readReviewedActionState(publicClient, manifest, scope.sender, { action: action.kind, clientOrderId })
      signal?.throwIfAborted()
      let prepared: KernelPreparedOperation
      try {
        prepared = await prepareKernelOperation(manifest.kernel, { mode, sender: scope.sender, callData, preparationId: newPreparationId(), expectedNonce, ...factory }, signal)
      } catch (error) {
        if (!signal?.aborted) trackPerpsKernelOutcome({ mode, outcome: 'quote_failed', error })
        throw error
      }
      const current = readGasPaymentPreference(preferenceScope)
      if (mode !== current.mode || revision !== current.revision) throw new Error('The gas setting changed. Review a fresh quote to continue.')
      const quote = Object.freeze({ mode, maxNetworkGasWei: prepared.maxNetworkGasWei, expiresAt: prepared.expiresAt })
      reviewedPreparations.set(quote, { accountKey, preferenceRevision: revision, action: action.kind, clientOrderId, reviewedState, prepared })
      trackPerpsKernelOutcome({ mode, outcome: 'quote_ready' })
      return quote
    },
    execute: async input => {
      const { action, kernelQuote, onStatus } = input
      const review = kernelQuote && reviewedPreparations.get(kernelQuote)
      if (!isAddressEqual(input.ownerAddress, ownerAddress) || !isAddressEqual(action.account, scope.sender) ||
          review?.accountKey !== accountKey || review.action !== action.kind ||
          review.clientOrderId !== input.orderRequestV3?.clientOrderId ||
          review.prepared.operation.callData.toLowerCase() !== encodeKernelCalls(action.calls)) {
        throw new Error('Review the gas fee for this Trading Account action before approving')
      }
      const { prepared } = review
      const requirePreference = () => {
        const current = readGasPaymentPreference(preferenceScope)
        if (current.mode !== prepared.mode || current.revision !== review.preferenceRevision) {
          throw new Error('The gas setting changed. Review a fresh quote to continue.')
        }
      }
      const requireReviewedState = async () => {
        const current = await readReviewedActionState(publicClient, manifest, scope.sender, { action: action.kind, clientOrderId: review.clientOrderId })
        if (current !== review.reviewedState) throw new Error('The reviewed position or protection changed. Review a new transaction.')
      }
      const requireOrderSubmissionTime = (now: number) => {
        if (!hasKernelOrderSubmissionTime(input.orderRequestV3?.submitBy, now)) {
          throw new Error('The order deadline is too close. Review a fresh order before approving.')
        }
      }
      const requireModeReady = async () => {
        const current = await fetchKernelManifest()
        if (current.profile.deployment !== scope.deployment || !current.readiness[prepared.mode]) {
          throw new KernelRpcError('PAYMENT_MODE_UNAVAILABLE', 'The reviewed gas payment mode is unavailable. Review the gas fee again.', true)
        }
        if (prepared.mode === 'sponsored') {
          const { paymaster } = current
          if (!paymaster || !kernelSepoliaCanaryPaymaster || !prepared.operation.paymaster ||
              !isAddressEqual(paymaster, kernelSepoliaCanaryPaymaster.address) || !isAddressEqual(paymaster, prepared.operation.paymaster)) {
            throw new Error('The sponsorship contract changed. Review a fresh quote to continue.')
          }
          const code = await publicClient.getBytecode({ address: paymaster, blockTag: 'safe' })
          if (!code || keccak256(code) !== kernelSepoliaCanaryPaymaster.codeHash) {
            throw new Error('The sponsorship contract does not match this Kernel deployment.')
          }
        }
      }
      const attempt = await withKernelOperationLane(scope, async () => {
        await refreshReadiness()
        const readinessAction = input.orderRequestV3 ? (input.orderRequestV3.isClose ? 'close' : 'open') : input.protectionIntent ? 'protection' : 'deposit'
        const blocker = readinessBlocker(currentReadiness(), readinessAction, undefined, true)
        if (blocker) throw new SponsorRequestError({ reason: blocker.reason, message: readinessMessage(blocker.reason), retryable: true })
        await requireReviewedState()
        requirePreference()
        await requireModeReady()
        const signature = await sign(prepared, () => {
          requirePreference()
          requireOrderSubmissionTime(deadlineNow())
          onStatus?.('awaiting-signature')
        })
        await Promise.all([requireReviewedState(), requireNonce(prepared.operation.nonce)])
        const now = (await refreshDeadlineClock()).now()
        requirePreference()
        requireFresh(prepared, now)
        requireOrderSubmissionTime(now)
        onStatus?.('journaling')
        const stored = await saveKernelSignedAttempt({ scope, ownerAddress, action: action.kind, prepared, signature,
          orderRequestV3: input.orderRequestV3, protectionIntent: input.protectionIntent,
          authorizationToken: input.authorizationTokenToClearOnConfirmation,
          authorizationNonce: input.authorizationNonceToClearOnConfirmation,
        })
        onStatus?.('submitting')
        try {
          await submitKernelAttempt(stored)
        } catch (error) {
          trackPerpsKernelOutcome({ mode: prepared.mode, outcome: 'submission_unknown', error })
          throw new BundlerRequestError({ message: 'Check Trading Account activity for the signed operation before trying again.',
            terminalStatus: 'submission-unknown', retryable: false, cause: error })
        }
        return { ...stored, phase: 'submitted' as const }
      })
      onStatus?.('confirming')
      const settled = await waitForCanonicalAttempt(attempt)
      if (settled.phase === 'included' && settled.receipt && prepared.operation.factory !== undefined) {
        const { blockNumber, blockHash } = settled.receipt.receipt
        try {
          const verified = await verifyAt({ number: blockNumber, hash: blockHash }, scope.sender)
          if (!verified.deployed) throw new Error('The expected Kernel account was not deployed')
        } catch (error) {
          throw new BundlerRequestError({ reason: 'ACCOUNT_NOT_TRUSTED', retryable: false, cause: error,
            message: 'The operation was included. Plether could not verify the Trading Account deployment. Check Activity before another action.' })
        }
      }
      if (settled.phase !== 'included' || !settled.receipt?.success || settled.receipt.receipt.status !== 'success') {
        throw new BundlerRequestError({
          message: settled.phase === 'included'
            ? 'The Trading Account operation reverted onchain. See its gas charge in Activity.'
            : 'The Trading Account operation closed without executing this action. Check Activity for the outcome.',
          terminalStatus: settled.phase === 'included' ? 'execution-reverted' : settled.phase === 'sponsorship_expired' ? 'expired' : 'replaced',
          retryable: false,
        })
      }
      const result = { userOperationHash: prepared.userOperationHash, receipt: settled.receipt, transactionHash: settled.receipt.receipt.transactionHash }
      try {
        input.onIncluded?.(result)
      } catch {
        // The canonical receipt is already saved even if an optional UI consumer fails.
      }
      onStatus?.('confirmed')
      return result
    },
    quoteCancellation: async (original, originalSignature) => {
      if (!isAddressEqual(original.operation.sender, scope.sender)) throw new Error('The pending operation belongs to a different Trading Account')
      await requireNonce(original.operation.nonce)
      const factory = await factoryAtSafe()
      const response = await prepareKernelCancellation(manifest.kernel, original, { preparationId: newPreparationId(), originalSignature, ...factory })
      const quote = Object.freeze({ ...response, operation: Object.freeze(response.operation) })
      reviewedCancellations.set(quote, { accountKey, hash: quote.userOperationHash })
      return quote
    },
    signPrepared: async prepared => {
      const review = reviewedCancellations.get(prepared)
      if (review?.accountKey !== accountKey || review.hash !== prepared.userOperationHash) {
        throw new Error('Review the cancellation fee before approving')
      }
      const signature = await sign(prepared)
      await requireNonce(prepared.operation.nonce)
      return signature
    },
  }
  return {
    chainId: scope.chainId, ownerAddress, factoryAddress: profile.factory,
    accountVersion: profile.accountVersion, accountIndex: '0', manifestVersion: manifest.version,
    smartAccount: { accountAddress: scope.sender, entryPoint: scope.entryPoint },
    gasPayment: { scope: preferenceScope, readiness }, kernel,
    readReviewedActionState: input => readReviewedActionState(publicClient, manifest, scope.sender, input),
  }
}
