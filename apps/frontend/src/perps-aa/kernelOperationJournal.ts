import type { PerpsActionKind } from '@plether-fi/perps-aa-client'
import { createStore, get, update } from 'idb-keyval'
import { isAddressEqual, recoverMessageAddress, zeroAddress, type Address, type Hex, type TransactionReceipt } from 'viem'
import type { UserOperationReceipt } from 'viem/account-abstraction'
import { arbitrumSepolia } from 'viem/chains'
import { trackPerpsKernelOutcome } from '../analytics/perps'
import type { PersistedPerpsOrderRequestV3 } from '../contracts/perpsOrderV3'
import type { PersistedProtectionIntent } from '../contracts/positionProtection'
import { clearDepositAuthorization } from './authorizationStore'
import { deadlineNow, refreshDeadlineClock } from './deadlineClock'
import { encodeKernelCalls, normalizeKernelSignature } from './kernelAccount'
import { kernelSepoliaCanaryProfile } from './kernelDeploymentPins'
import { hasKernelOrderSubmissionTime } from './kernelOrderDeadline'
import type { KernelBundledInclusionReader } from './kernelBundledInclusion'
import {
  assertKernelPreparedOperation,
  getKernelOperationStatus,
  sendKernelCancellation,
  sendKernelOperation,
  type KernelActionPreparation,
  type KernelPreparedCancellation,
  type KernelPreparedOperation,
} from './kernelRpc'
import { assertKernelActionPreparation, type KernelWalletOutcomeReader, type KernelWalletSubmission } from './kernelWalletPayment'
import {
  acquireSponsoredOperationBrowserLane,
  acquireSponsoredOperationBrowserRecoveryLock,
} from './laneLock'
import { SponsoredOperationLockedError } from './operationLockError'

export interface KernelAttemptScope {
  deployment: string
  chainId: number
  entryPoint: Address
  sender: Address
}

export type KernelAttemptPhase =
  | 'signed'
  | 'sending'
  | 'submitted'
  | 'included'
  | 'nonce_consumed'
  | 'sponsorship_expired'
  | 'cancelled'
  | 'discarded'
  | 'wallet_failed'
  | 'wallet_replaced'

interface SignedKernelAttemptInput {
  scope: KernelAttemptScope
  ownerAddress: Address
  action: PerpsActionKind
  prepared: KernelActionPreparation
  signature: Hex
  walletSubmission?: KernelWalletSubmission
  orderRequestV3?: PersistedPerpsOrderRequestV3
  protectionIntent?: PersistedProtectionIntent
  authorizationToken?: Address
  authorizationNonce?: Hex
}

export interface KernelOperationAttempt extends SignedKernelAttemptInput {
  version: 1
  nonceKey: 0n
  expectedPaymaster: Address
  phase: KernelAttemptPhase
  createdAt: number
  updatedAt: number
  observedReceipt?: UserOperationReceipt<'0.9'>
  receipt?: UserOperationReceipt<'0.9'>
  outerReceipt?: TransactionReceipt
  cancellation?: {
    prepared: KernelPreparedCancellation
    signature: Hex
    phase: 'signed' | 'sending' | 'submitted'
  }
}

export type KernelObservationReorgCheck = (receipt: UserOperationReceipt<'0.9'>) => Promise<boolean>

function sameObservation(left: KernelOperationAttempt['observedReceipt'], right: KernelOperationAttempt['observedReceipt']) {
  return left?.receipt.blockHash === right?.receipt.blockHash &&
    left?.receipt.blockNumber === right?.receipt.blockNumber &&
    left?.receipt.transactionHash === right?.receipt.transactionHash
}

const journal = createStore('plether-perps-kernel-operations', 'attempts')
const historyLimit = 100
const attemptPhases: readonly KernelAttemptPhase[] = [
  'signed', 'sending', 'submitted', 'included', 'nonce_consumed', 'sponsorship_expired', 'cancelled', 'discarded', 'wallet_failed', 'wallet_replaced',
]
const updates = new EventTarget()

let crossTabUpdates: BroadcastChannel | undefined

function updateChannel(): BroadcastChannel | undefined {
  if (typeof window === 'undefined' || typeof BroadcastChannel === 'undefined') return undefined
  try {
    crossTabUpdates ??= new BroadcastChannel('plether-perps-kernel-operation-changes')
  } catch {
    return undefined
  }
  return crossTabUpdates
}

export function kernelAttemptScopeKey(scope: KernelAttemptScope): string {
  return `${scope.deployment}:${scope.chainId.toString()}:${scope.entryPoint.toLowerCase()}:${scope.sender.toLowerCase()}`
}

export function isKernelAttemptPending(attempt: KernelOperationAttempt): boolean {
  return attempt.phase === 'signed' || attempt.phase === 'sending' || attempt.phase === 'submitted'
}

interface KernelFollowUp {
  mode: KernelPreparedOperation['mode'] | undefined
  nonce?: bigint
}

export function kernelAttemptBlocksFollowUp(attempt: KernelOperationAttempt, next?: KernelFollowUp): boolean {
  if (!isKernelAttemptPending(attempt)) return false
  return !(import.meta.env.VITE_PERPS_KERNEL_HOMEPAGE_OPT_IN === 'true' &&
    attempt.scope.deployment === kernelSepoliaCanaryProfile.deployment &&
    attempt.scope.chainId === kernelSepoliaCanaryProfile.chainId && next?.mode === attempt.prepared.mode &&
    attempt.observedReceipt?.success &&
    (next.nonce === undefined || next.nonce > attempt.prepared.operation.nonce))
}

export function subscribeKernelAttemptUpdates(scope: KernelAttemptScope, notify: () => void): () => void {
  const key = kernelAttemptScopeKey(scope)
  const channel = updateChannel()
  const onMessage = (event: MessageEvent<unknown>) => {
    if (event.data === key) notify()
  }
  updates.addEventListener(key, notify)
  channel?.addEventListener('message', onMessage)
  return () => {
    updates.removeEventListener(key, notify)
    channel?.removeEventListener('message', onMessage)
  }
}

function readAttemptList(key: string, stored: unknown): KernelOperationAttempt[] {
  if (stored === undefined) return []
  if (!Array.isArray(stored)) throw new Error('The Trading Account journal could not be read safely')
  const attempts = stored as (Omit<KernelOperationAttempt, 'version'> & { version: unknown })[]
  if (attempts.some(attempt =>
    attempt.version !== 1 || kernelAttemptScopeKey(attempt.scope) !== key ||
    !attemptPhases.includes(attempt.phase)
  )) {
    throw new Error('The Trading Account journal could not be read safely')
  }
  return attempts as KernelOperationAttempt[]
}

export async function listKernelAttempts(scope: KernelAttemptScope): Promise<KernelOperationAttempt[]> {
  const key = kernelAttemptScopeKey(scope)
  return readAttemptList(key, await get<KernelOperationAttempt[]>(key, journal))
}

export async function getActiveKernelAttempt(scope: KernelAttemptScope, next?: KernelFollowUp): Promise<KernelOperationAttempt | undefined> {
  return (await listKernelAttempts(scope)).find(attempt => kernelAttemptBlocksFollowUp(attempt, next))
}

export async function getKernelAttempt(scope: KernelAttemptScope, hash: Hex, preparationId?: Hex): Promise<KernelOperationAttempt | undefined> {
  return (await listKernelAttempts(scope)).find(attempt => attempt.prepared.userOperationHash === hash &&
    (preparationId === undefined || attempt.prepared.preparationId === preparationId))
}

async function editJournal<T>(scope: KernelAttemptScope, edit: (attempts: KernelOperationAttempt[]) => {
  attempts: KernelOperationAttempt[]
  result: T
}): Promise<T> {
  const key = kernelAttemptScopeKey(scope)
  // The IndexedDB read and write share a transaction. Keep the lane fenced
  // even if the initiating tab closes or a stale tab writes new activity.
  let editResult: { result: T } | undefined
  await update<KernelOperationAttempt[]>(key, current => {
    const { attempts, result } = edit(readAttemptList(key, current))
    editResult = { result }
    return attempts
  }, journal)
  if (!editResult) throw new Error('The Trading Account journal could not be saved')
  updates.dispatchEvent(new Event(key))
  try {
    updateChannel()?.postMessage(key)
  } catch {
    // The committed journal remains the source of truth if live updates fail.
  }
  return editResult.result
}

function acquireKernelLane(scope: KernelAttemptScope) {
  return acquireSponsoredOperationBrowserLane({
    accountAddress: scope.sender,
    chainId: scope.chainId,
    lane: `kernel:${scope.deployment}:${scope.entryPoint.toLowerCase()}:0`,
  })
}

/** The same browser and durable lane covers both gas payers. */
export async function withKernelOperationLane<T>(scope: KernelAttemptScope, action: () => Promise<T>, next?: KernelFollowUp): Promise<T> {
  const release = await acquireKernelLane(scope)
  try {
    const active = await getActiveKernelAttempt(scope, next)
    if (active) throw new SponsoredOperationLockedError(active.prepared.preparationId)
    return await action()
  } finally {
    await release()
  }
}

async function assertSignedForScope(input: SignedKernelAttemptInput): Promise<void> {
  const { prepared, scope, signature } = input
  assertKernelActionPreparation(prepared)
  if (scope.chainId !== arbitrumSepolia.id || !isAddressEqual(prepared.entryPoint, scope.entryPoint) ||
      !isAddressEqual(prepared.operation.sender, scope.sender) ||
      prepared.operation.nonce >> 64n !== 0n ||
      normalizeKernelSignature(signature) !== signature.toLowerCase()) {
    throw new Error('The signed operation does not match this Trading Account')
  }
  const signer = await recoverMessageAddress({ message: { raw: prepared.userOperationHash }, signature })
  if (!isAddressEqual(signer, input.ownerAddress)) {
    throw new Error('The signed operation does not match the Trading Account owner')
  }
}

/** Called after wallet signing and before any network submission. */
export async function saveKernelSignedAttempt(input: SignedKernelAttemptInput): Promise<KernelOperationAttempt> {
  await assertSignedForScope(input)
  if ((input.prepared.transport === 'wallet') !== (input.walletSubmission !== undefined)) {
    throw new Error('The wallet submission does not match this Trading Account preparation')
  }
  const now = Date.now()
  const attempt: KernelOperationAttempt = {
    ...input,
    version: 1,
    nonceKey: 0n,
    expectedPaymaster: input.prepared.operation.paymaster ?? zeroAddress,
    phase: input.walletSubmission ? 'sending' : 'signed',
    createdAt: now,
    updatedAt: now,
  }
  return editJournal(input.scope, attempts => {
    const active = attempts.find(existing => kernelAttemptBlocksFollowUp(existing, {
      mode: input.prepared.mode, nonce: input.prepared.operation.nonce,
    }))
    if (active) throw new SponsoredOperationLockedError(active.prepared.preparationId)
    if (attempts.some(existing => existing.prepared.preparationId === input.prepared.preparationId)) {
      throw new Error('This Trading Account preparation was already used')
    }
    const recent = attempts.slice(0, historyLimit - 1)
    const olderPending = attempts.slice(historyLimit - 1).filter(isKernelAttemptPending)
    return { attempts: [attempt, ...recent, ...olderPending], result: attempt }
  })
}

export async function recordKernelWalletResponse(attempt: KernelOperationAttempt, transactionHash?: Hex): Promise<void> {
  await editAttempt(attempt, current => {
    if (current.phase !== 'sending' || current.prepared.transport !== 'wallet' || !current.walletSubmission) return current
    return transactionHash
      ? { ...current, phase: 'submitted', walletSubmission: { ...current.walletSubmission, transactionHash }, updatedAt: Date.now() }
      : { ...current, phase: 'discarded', updatedAt: Date.now() }
  })
}

function editAttempt(attempt: KernelOperationAttempt, edit: (current: KernelOperationAttempt) => KernelOperationAttempt) {
  return editJournal(attempt.scope, attempts => {
    const current = attempts.find(item => item.prepared.preparationId === attempt.prepared.preparationId)
    if (current?.prepared.userOperationHash !== attempt.prepared.userOperationHash ||
        current.signature !== attempt.signature || current.prepared.mode !== attempt.prepared.mode ||
        current.prepared.transport !== attempt.prepared.transport) {
      throw new Error('This signed Trading Account attempt is unavailable')
    }
    const next = edit(current)
    return {
      attempts: attempts.map(item => item === current ? next : item),
      result: { attempt: next, changed: next !== current },
    }
  })
}

function reconcileLocator(attempt: KernelOperationAttempt) {
  if (attempt.prepared.transport === 'wallet') throw new Error('This operation was submitted from the Owner Wallet')
  return {
    preparationId: attempt.prepared.preparationId,
    userOperationHash: attempt.prepared.userOperationHash,
    recoveryToken: attempt.prepared.recoveryToken,
    sender: attempt.scope.sender,
    entryPoint: attempt.scope.entryPoint,
    nonce: attempt.prepared.operation.nonce,
    paymaster: attempt.expectedPaymaster,
    cancellationHash: attempt.cancellation?.prepared.userOperationHash,
  }
}

/**
 * Called under the browser lane for the first send or an explicit exact retry.
 * A failure after `sending` is durable leaves an unresolved attempt.
 */
export async function submitKernelAttempt(attempt: KernelOperationAttempt): Promise<Hex> {
  if (attempt.prepared.transport === 'wallet') throw new Error('Check the pending transaction in your wallet')
  await assertSignedForScope(attempt)
  const { attempt: current } = await editAttempt(attempt, existing => {
    if (!isKernelAttemptPending(existing)) throw new Error('This Trading Account attempt is already resolved')
    if (!kernelAttemptCanRetry(existing)) throw new Error('The original gas quote expired')
    return { ...existing, phase: 'sending', updatedAt: Date.now() }
  })
  if (current.prepared.transport === 'wallet') throw new Error('Check the pending transaction in your wallet')
  const hash = await sendKernelOperation(current.prepared, current.signature)
  if (hash.toLowerCase() !== current.prepared.userOperationHash.toLowerCase()) {
    throw new Error('The Trading Account gateway returned a different operation hash')
  }
  await editAttempt(current, existing => isKernelAttemptPending(existing)
    ? { ...existing, phase: 'submitted', updatedAt: Date.now() }
    : existing)
  return hash
}

function knownServerTime(): number | undefined {
  try {
    return deadlineNow()
  } catch {
    return undefined
  }
}

export function kernelAttemptCanRetry(attempt: KernelOperationAttempt, now = knownServerTime()): boolean {
  const orderDeadline = attempt.orderRequestV3?.submitBy
  return attempt.prepared.transport !== 'wallet' && now !== undefined && isKernelAttemptPending(attempt) && attempt.observedReceipt === undefined && attempt.cancellation === undefined &&
    now < attempt.prepared.expiresAt * 1_000 &&
    hasKernelOrderSubmissionTime(orderDeadline, now)
}

/** Manual recovery preserves the originally signed fields and payer. */
export async function retryKernelAttempt(attempt: KernelOperationAttempt): Promise<Hex> {
  const release = await acquireKernelLane(attempt.scope)
  try {
    await refreshDeadlineClock()
    return await submitKernelAttempt(attempt)
  } finally {
    await release()
  }
}

export function kernelAttemptCanDiscard(attempt: KernelOperationAttempt, now = knownServerTime()): boolean {
  return now !== undefined && attempt.phase === 'signed' && !kernelAttemptCanRetry(attempt, now)
}

export async function discardUnsentKernelAttempt(attempt: KernelOperationAttempt): Promise<void> {
  const release = await acquireKernelLane(attempt.scope)
  try {
    await refreshDeadlineClock()
    await editAttempt(attempt, current => {
      if (!kernelAttemptCanDiscard(current)) {
        throw new Error('Only an expired and unsent Trading Account attempt can be discarded')
      }
      return { ...current, phase: 'discarded', updatedAt: Date.now() }
    })
  } finally {
    await release()
  }
}

export function kernelAttemptCanCancel(attempt: KernelOperationAttempt): boolean {
  return attempt.prepared.transport !== 'wallet' && attempt.prepared.mode === 'eth' && attempt.observedReceipt === undefined && attempt.cancellation === undefined &&
    (attempt.phase === 'sending' || attempt.phase === 'submitted')
}

export function kernelCancellationCanRetry(attempt: KernelOperationAttempt, now = knownServerTime()): boolean {
  const cancellation = attempt.cancellation
  return isKernelAttemptPending(attempt) && attempt.observedReceipt === undefined && cancellation !== undefined &&
    (cancellation.phase !== 'signed' || now !== undefined && now < cancellation.prepared.expiresAt * 1_000)
}

export function kernelCancellationCanDiscard(attempt: KernelOperationAttempt, now = knownServerTime()): boolean {
  const cancellation = attempt.cancellation
  return now !== undefined && isKernelAttemptPending(attempt) && cancellation?.phase === 'signed' &&
    now >= cancellation.prepared.expiresAt * 1_000
}

export async function discardUnsentKernelCancellation(attempt: KernelOperationAttempt): Promise<void> {
  const release = await acquireKernelLane(attempt.scope)
  try {
    await refreshDeadlineClock()
    await editAttempt(attempt, current => {
      if (!kernelCancellationCanDiscard(current) ||
          current.cancellation?.prepared.userOperationHash !== attempt.cancellation?.prepared.userOperationHash) {
        throw new Error('Only an expired and unsent cancellation can be discarded')
      }
      const next = { ...current, updatedAt: Date.now() }
      delete next.cancellation
      return next
    })
  } finally {
    await release()
  }
}

function assertCancellationBinding(attempt: KernelOperationAttempt, cancellation: KernelPreparedOperation & { cancelsUserOperationHash: Hex }) {
  assertKernelPreparedOperation(cancellation)
  if (attempt.prepared.transport === 'wallet' || cancellation.mode !== 'eth' || cancellation.operation.paymaster !== undefined ||
      cancellation.cancelsUserOperationHash !== attempt.prepared.userOperationHash ||
      !isAddressEqual(cancellation.entryPoint, attempt.scope.entryPoint) ||
      !isAddressEqual(cancellation.operation.sender, attempt.scope.sender) ||
      cancellation.operation.nonce !== attempt.prepared.operation.nonce ||
      cancellation.operation.callData.toLowerCase() !== encodeKernelCalls([{ to: attempt.scope.sender }]).toLowerCase() ||
      cancellation.userOperationHash === attempt.prepared.userOperationHash) {
    throw new Error('The cancellation quote does not match the pending Trading Account operation')
  }
}

async function sendSavedCancellation(attempt: KernelOperationAttempt): Promise<Hex> {
  const saved = attempt.cancellation
  if (!saved) throw new Error('No signed cancellation is available')
  assertCancellationBinding(attempt, saved.prepared)
  await assertSignedForScope({ ...attempt, ...saved })
  const { attempt: current } = await editAttempt(attempt, existing => {
    const cancellation = existing.cancellation
    if (cancellation?.prepared.userOperationHash !== saved.prepared.userOperationHash ||
        cancellation.signature !== saved.signature || !kernelCancellationCanRetry(existing)) {
      throw new Error('The original cancellation quote is unavailable or expired')
    }
    return { ...existing, cancellation: { ...cancellation, phase: 'sending' }, updatedAt: Date.now() }
  })
  if (current.prepared.transport === 'wallet') throw new Error('Cancel the pending transaction in your wallet')
  const hash = await sendKernelCancellation(current.prepared, saved.prepared, saved.signature)
  if (hash !== saved.prepared.userOperationHash) throw new Error('The gateway returned a different cancellation hash')
  await editAttempt(current, existing => isKernelAttemptPending(existing) && existing.cancellation
    ? { ...existing, cancellation: { ...existing.cancellation, phase: 'submitted' }, updatedAt: Date.now() }
    : existing)
  return hash
}

/** The wallet prompt runs only after explicit approval of the displayed cancellation quote. */
export async function signAndSubmitKernelCancellation(
  attempt: KernelOperationAttempt,
  prepared: KernelPreparedCancellation,
  sign: (prepared: KernelPreparedCancellation) => Promise<Hex>,
): Promise<Hex> {
  const release = await acquireKernelLane(attempt.scope)
  try {
    const current = await getKernelAttempt(attempt.scope, attempt.prepared.userOperationHash)
    assertCancellationBinding(attempt, prepared)
    if (!current || !kernelAttemptCanCancel(current) || deadlineNow() >= prepared.expiresAt * 1_000) {
      throw new Error('This cancellation quote is unavailable or expired')
    }
    const signature = await sign(prepared)
    await assertSignedForScope({ ...current, prepared, signature })
    await refreshDeadlineClock()
    const { attempt: saved } = await editAttempt(current, existing => {
      if (!kernelAttemptCanCancel(existing) || deadlineNow() >= prepared.expiresAt * 1_000) {
        throw new Error('This cancellation quote is unavailable or expired')
      }
      return { ...existing, cancellation: { prepared, signature, phase: 'signed' }, updatedAt: Date.now() }
    })
    return await sendSavedCancellation(saved)
  } finally {
    await release()
  }
}

export async function retryKernelCancellation(attempt: KernelOperationAttempt): Promise<Hex> {
  const release = await acquireKernelLane(attempt.scope)
  try {
    if (attempt.cancellation?.phase === 'signed') await refreshDeadlineClock()
    return await sendSavedCancellation(attempt)
  } finally {
    await release()
  }
}

/** An ambiguous transport response, timeout, or elapsed quote leaves the lane held. */
export async function reconcileKernelAttempt(attempt: KernelOperationAttempt, isObservationReorged?: KernelObservationReorgCheck, readWalletOutcome?: KernelWalletOutcomeReader, readBundledInclusion?: KernelBundledInclusionReader): Promise<KernelOperationAttempt> {
  const current = await getKernelAttempt(attempt.scope, attempt.prepared.userOperationHash, attempt.prepared.preparationId)
  if (!current) throw new Error('The pending Trading Account operation is unavailable')
  if (!isKernelAttemptPending(current) || current.phase === 'signed') return current
  if (current.prepared.transport === 'wallet') {
    if (!readWalletOutcome) return current
    const status = await readWalletOutcome(current)
    if (status.state === 'pending') return current
    const { attempt: resolved, changed } = await editAttempt(current, existing => isKernelAttemptPending(existing)
      ? { ...existing, phase: status.state, updatedAt: Date.now(),
          outerReceipt: 'outerReceipt' in status ? status.outerReceipt : undefined,
          receipt: status.state === 'included' ? status.receipt : undefined }
      : existing)
    if (changed) reportKernelOutcome(resolved)
    return resolved
  }
  assertKernelPreparedOperation(current.prepared)
  if (!isAddressEqual(current.expectedPaymaster, current.prepared.operation.paymaster ?? zeroAddress)) {
    throw new Error('The saved gas payer no longer matches the Trading Account operation')
  }
  const saveObservation = async (next: KernelOperationAttempt['observedReceipt']) => {
    const { attempt: updated } = await editAttempt(current, existing => {
      if (!isKernelAttemptPending(existing) || !sameObservation(existing.observedReceipt, current.observedReceipt)) return existing
      return { ...existing, observedReceipt: next, updatedAt: Date.now() }
    })
    return updated
  }
  if (!current.observedReceipt && readBundledInclusion) {
    const inclusion = await readBundledInclusion(current).catch(() => undefined)
    if (inclusion) return await saveObservation(inclusion)
  }
  const locator = reconcileLocator(current)
  const status = await getKernelOperationStatus(locator)
  if (status.state === 'pending' || status.state === 'observed') {
    const next = status.state === 'observed' ? status.receipt : undefined
    const observed = current.observedReceipt
    const changed = next
      ? !sameObservation(observed, next)
      : observed !== undefined && await isObservationReorged?.(observed)
    if (!changed) return (await getKernelAttempt(current.scope, current.prepared.userOperationHash)) ?? current
    return await saveObservation(next)
  }
  if (status.state === 'sponsorship_expired' && current.prepared.mode !== 'sponsored') {
    throw new Error('The Trading Account gateway returned an incompatible expiry')
  }
  const { attempt: resolved, changed } = await editAttempt(current, existing => {
    if (!isKernelAttemptPending(existing) || reconcileLocator(existing).cancellationHash !== locator.cancellationHash) return existing
    return { ...existing, phase: status.state, observedReceipt: undefined, ...(status.receipt ? { receipt: status.receipt } : {}), updatedAt: Date.now() }
  })
  if (!changed) return resolved
  reportKernelOutcome(resolved)
  return resolved
}

function reportKernelOutcome(resolved: KernelOperationAttempt) {
  const { phase } = resolved
  if (phase === 'signed' || phase === 'sending' || phase === 'submitted' || phase === 'discarded') return
  if (resolved.phase === 'included' && resolved.receipt?.success &&
      resolved.authorizationToken && resolved.authorizationNonce) {
    try {
      clearDepositAuthorization({
        chainId: resolved.scope.chainId,
        ownerAddress: resolved.ownerAddress,
        accountAddress: resolved.scope.sender,
        token: resolved.authorizationToken,
        expectedNonce: resolved.authorizationNonce,
      })
    } catch {
      // The canonical outcome and unlocked lane survive a local cleanup failure.
    }
  }
  trackPerpsKernelOutcome({
    mode: resolved.prepared.mode,
    outcome: phase === 'wallet_failed' || (phase === 'included' && !resolved.receipt?.success) ? 'execution_reverted'
      : phase === 'wallet_replaced' ? 'cancelled' : phase,
    maximum: resolved.phase === 'cancelled' ? resolved.cancellation?.prepared.maxNetworkGasWei : resolved.prepared.maxNetworkGasWei,
    actual: resolved.outerReceipt ? resolved.outerReceipt.gasUsed * resolved.outerReceipt.effectiveGasPrice : resolved.receipt?.actualGasCost,
  })
}

/** One tab per attempt polls at a time. Polling cannot send an operation. */
export async function refreshKernelAttempt(attempt: KernelOperationAttempt, isObservationReorged?: KernelObservationReorgCheck, readWalletOutcome?: KernelWalletOutcomeReader, readBundledInclusion?: KernelBundledInclusionReader): Promise<KernelOperationAttempt> {
  if (!isKernelAttemptPending(attempt) || attempt.phase === 'signed') return attempt
  const release = await acquireSponsoredOperationBrowserRecoveryLock({
    chainId: attempt.scope.chainId,
    accountAddress: attempt.scope.sender,
    operationId: attempt.prepared.preparationId,
  })
  try {
    return await reconcileKernelAttempt(attempt, isObservationReorged, readWalletOutcome, readBundledInclusion)
  } finally {
    await release()
  }
}
