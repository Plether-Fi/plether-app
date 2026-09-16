import { useEffect, useRef, useState } from 'react'
import { AccountDeploymentConfirmation } from './AccountDeploymentConfirmation'
import { trackPerpsPreparationRecovery } from '../analytics/perps'
import type { PerpsAaDeploymentManifestV2 } from './manifest'
import { clearDepositAuthorization } from './authorizationStore'
import { reconcileUserOperation } from './operationReconciler'
import { resumeSponsoredPerpsAction } from './execution'
import { acquireSponsoredOperationBrowserLane } from './laneLock'
import { hasSponsoredOperationSignal, isSponsoredOperationTerminal, restoreSponsoredOperationLane, useSponsoredOperationStore, type SponsoredOperation } from './operationStore'
import { usePerpsAaRuntime, type PerpsAaSmartAccountRuntime } from './runtimeContext'
import { useConfirmationStatus, useDeploymentConfirmationDetails } from './useAccountDeploymentConfirmation'
import { confirmationPending } from './deploymentConfirmation'
import { PreparationRecoveryError, recoveryMessage, recoveryReason, type WalletRecoveryResult } from './walletRecovery'
import type { PreparationStatusV1 } from './preparedOperation'
import { sponsoredOperationActionLabel } from '../utils/sponsoredOperation'
import { Button } from '../components/ui/Button'

function normalizedAddress(value?: string) { return value?.toLowerCase() }

function recoveryFailure(attemptId: string, outcome: 'verification-failed' | 'status-failed' | 'resume-failed' | 'discard-failed', cause: unknown) {
  const reason = recoveryReason(cause) ?? 'RECOVERY_UNKNOWN'
  trackPerpsPreparationRecovery({ attemptId, outcome, reason })
  return recoveryMessage(reason)
}

/** Mounted only in the visible recovery view. Status changes never sign or submit. */
export function PreparedOperationRecovery({ operation, fallbackManifest }: { operation: SponsoredOperation; fallbackManifest?: PerpsAaDeploymentManifestV2 }) {
  const base = usePerpsAaRuntime()
  const manifest = operation.nativePreparation?.manifest ?? fallbackManifest
  const [resolved, setResolved] = useState<{ base: PerpsAaSmartAccountRuntime; manifest: PerpsAaDeploymentManifestV2; runtime: PerpsAaSmartAccountRuntime }>()
  const [unavailable, setUnavailable] = useState(false)
  useEffect(() => {
    if (!base?.getPreparedOperationRuntime || !manifest) return
    let stopped = false
    void base.getPreparedOperationRuntime(manifest).then(runtime => {
      if (!stopped) { setResolved({ base, manifest, runtime }); setUnavailable(false) }
    }).catch(() => { if (!stopped) setUnavailable(true) })
    return () => { stopped = true }
  }, [base, manifest])
  const runtime = base?.getPreparedOperationRuntime
    ? resolved?.base === base && resolved.manifest === manifest ? resolved.runtime : undefined
    : base
  if (!runtime) return <p>{unavailable ? 'The saved account or deployment could not be loaded. Your attempt is retained.' : 'Loading saved transaction recovery…'}</p>
  if (runtime.chainId !== operation.chainId || normalizedAddress(runtime.ownerAddress) !== normalizedAddress(operation.ownerAddress)
    || normalizedAddress(runtime.smartAccount.accountAddress) !== normalizedAddress(operation.accountAddress)) {
    return <p>Connect the owner wallet for this saved Trading Account and network to recover it.</p>
  }
  return <PreparedRecoveryView key={`${operation.id}:${String(runtime.chainId)}:${runtime.smartAccount.accountAddress}:${JSON.stringify(manifest)}`}
    runtime={runtime} operation={operation} fallbackManifest={fallbackManifest} />
}

function PreparedRecoveryView({ runtime, operation, fallbackManifest }: {
  runtime: PerpsAaSmartAccountRuntime; operation: SponsoredOperation; fallbackManifest?: PerpsAaDeploymentManifestV2
}) {
  const [checkedStatus, setStatus] = useState<PreparationStatusV1>()
  const [statusError, setStatusError] = useState<string>()
  const [error, setError] = useState<string>()
  const [walletStatus, setWalletStatus] = useState<WalletRecoveryResult>()
  const [walletRequired, setWalletRequired] = useState(false)
  const [verified, setVerified] = useState(false)
  const [working, setWorking] = useState(false)
  const [checkingStatus, setCheckingStatus] = useState(false)
  const [statusCheckRevision, setStatusCheckRevision] = useState(0)
  const request = operation.nativePreparation
  const recoveryManifest = request?.manifest ?? fallbackManifest
  const accountConfirmation = useConfirmationStatus(runtime.deploymentConfirmation)
  const confirmationDetails = useDeploymentConfirmationDetails(runtime.deploymentConfirmation)
  const accountWaiting = confirmationPending(accountConfirmation) || (accountConfirmation === 'idle' && operation.reason === 'ACCOUNT_DEPLOYMENT_PENDING' && !operation.preparedOperation)
  // Identity verification can reject before a preparation row exists. Keep the
  // original cause instead of replacing it with a status-lookup 403. This is
  // advisory only: it never proves that a reservation can be discarded.
  const deferStatusCheck = !!request && !operation.preparedOperation && !operation.userOperationHash
    && (accountWaiting || (operation.status === 'preparation-pending' && operation.reason === 'ACCOUNT_DEPLOYMENT_PENDING'))
  const status = walletStatus ? ('phase' in walletStatus ? walletStatus : undefined) : deferStatusCheck ? undefined : checkedStatus
  const visibleError = error ?? (deferStatusCheck ? undefined : statusError)

  useEffect(() => {
    if (operation.reason === 'ACCOUNT_DEPLOYMENT_PENDING' && !operation.preparedOperation) {
      runtime.deploymentConfirmation?.notePending?.(operation.confirmationWaitingSince)
    }
    if (accountWaiting) return runtime.deploymentConfirmation?.start()
  }, [runtime, accountWaiting, operation.reason, operation.preparedOperation, operation.confirmationWaitingSince])

  useEffect(() => {
    useSponsoredOperationStore.getState().recordConfirmationTiming(operation.id,
      confirmationDetails.waitingSince, confirmationDetails.lastSuccessfulCheckAt)
  }, [operation.id, confirmationDetails.waitingSince, confirmationDetails.lastSuccessfulCheckAt])

  const recordedConfirmation = useRef<string | undefined>(undefined)
  useEffect(() => {
    if (accountConfirmation === 'idle' || recordedConfirmation.current === accountConfirmation) return
    recordedConfirmation.current = accountConfirmation
    trackPerpsPreparationRecovery({ attemptId: operation.id, outcome: accountConfirmation,
      durationMs: confirmationDetails.waitingSince ? (confirmationDetails.lastCheckedAt ?? confirmationDetails.waitingSince) - confirmationDetails.waitingSince : undefined,
    })
  }, [operation.id, accountConfirmation, confirmationDetails])
  useEffect(() => {
    if (walletStatus) trackPerpsPreparationRecovery({ attemptId: operation.id,
      outcome: 'recoveryState' in walletStatus ? walletStatus.recoveryState : 'verified', reason: walletStatus.reason })
  }, [operation.id, walletStatus])

  useEffect(() => {
    if (!recoveryManifest || (deferStatusCheck && !verified)) return
    const lifecycle = { stopped: false }
    const isStopped = () => lifecycle.stopped
    let inFlight = false
    let timer: ReturnType<typeof setTimeout> | undefined
    const refresh = async () => {
      if (isStopped() || inFlight || document.visibilityState === 'hidden') return
      inFlight = true
      setCheckingStatus(true)
      clearTimeout(timer)
      let delay = 15_000
      let resolved = false
      try {
        if (verified && runtime.preparationRecovery) {
          const result = await runtime.preparationRecovery.status(operation.id)
          if (!isStopped()) { setWalletStatus(result); setStatusError(undefined); setWalletRequired(false) }
          resolved = 'recoveryState' in result && result.recoveryState === 'retired'
          return
        }
        const hash = operation.userOperationHash ?? operation.preparedOperation?.expectedHash
        const result = await runtime.smartAccount.getPreparationStatus?.(hash ? { userOperationHash: hash } : { preparationId: operation.id })
        if (!result) throw new Error('Preparation status is unavailable')
        if (!isStopped()) { setStatus(result); setStatusError(undefined) }
        resolved = (result.phase === 'resolved' || result.phase === 'settled') && result.freshReviewAllowed
        if (resolved && !isStopped() && !runtime.preparationRecovery) useSponsoredOperationStore.getState().markPreparationResolved(operation.id)
      } catch (cause) {
        delay = 60_000
        if (!isStopped()) {
          const reason = recoveryReason(cause)
          setStatus(undefined); setWalletStatus(undefined)
          setStatusError(recoveryFailure(operation.id, 'status-failed', cause))
          if (reason === 'RECOVERY_VERIFICATION_REQUIRED' || reason === 'PREPARATION_NOT_AUTHORIZED') {
            setWalletRequired(true); setVerified(false); setWalletStatus(undefined)
          }
        }
      } finally {
        inFlight = false
        if (!isStopped()) setCheckingStatus(false)
        if (!isStopped() && !resolved) timer = setTimeout(() => { void refresh() }, delay)
      }
    }
    const onFocus = () => { void refresh() }
    void refresh()
    window.addEventListener('focus', onFocus)
    document.addEventListener('visibilitychange', onFocus)
    return () => {
      lifecycle.stopped = true; setCheckingStatus(false); clearTimeout(timer)
      window.removeEventListener('focus', onFocus); document.removeEventListener('visibilitychange', onFocus)
    }
  }, [runtime, recoveryManifest, operation.id, operation.status, operation.userOperationHash, operation.preparedOperation?.expectedHash, deferStatusCheck, verified, statusCheckRevision])

  if (!recoveryManifest) return null
  if (operation.preparationResolved && operation.status === 'cancelled') return <p>
    Saved attempt discarded. Review your current position before preparing a new action.
  </p>
  if (!request && !runtime.preparationRecovery) return <div className="text-sm text-content-secondary">
    <p>This older record has no recoverable transaction payload. Check account activity before reviewing a new transaction.</p>
    {status?.reason === 'SAFE_EXPIRY_WAIT' || status?.reason === 'ASSISTANCE_RESERVATION_PENDING'
      ? <p>Waiting for sponsorship reservation to clear. Authorization expiry and safe reconciliation are separate.</p> : null}
    {visibleError && <p>{visibleError}</p>}
  </div>
  if (isSponsoredOperationTerminal(operation.status) && !runtime.preparationRecovery) return <p className="text-sm text-content-secondary">
    {status?.phase === 'resolved' || status?.phase === 'settled' ? 'The original sponsorship reservation is resolved.'
      : 'The saved preparation is retained until its sponsorship reservation is safely resolved.'}
  </p>
  const waiting = status?.reason === 'ASSISTANCE_RESERVATION_PENDING' || status?.reason === 'SAFE_EXPIRY_WAIT'
  const active = working || hasSponsoredOperationSignal(operation.id)
  // An interrupted wallet request is not proof of rejection. Only safe expiry
  // or confirmed rejection permits abandoning the original unsigned payload.
  const legacyFreshAllowed = !accountWaiting && status?.freshReviewAllowed && (operation.walletPreparationOutcome === 'declined'
    || status.phase === 'resolved' || status.phase === 'settled' || !operation.preparedOperation)
  const walletRecovery = runtime.preparationRecovery
  const freshAllowed = walletRecovery ? walletStatus?.canRetire === true : legacyFreshAllowed
  const resumeBlocked = isSponsoredOperationTerminal(operation.status) || walletRequired || (walletStatus && 'recoveryState' in walletStatus && walletStatus.recoveryState !== 'missing')
  const safelyResolved = status?.phase === 'resolved'
  // An unavailable status is not permission to retry a response-lost request.
  // Account-confirmation retries are the explicit pre-preparation exception.
  const canOfferResume = !resumeBlocked && (status?.recoverable === true
    || (deferStatusCheck && accountConfirmation === 'ready')
    || (walletStatus && 'recoveryState' in walletStatus && walletStatus.recoveryState === 'missing' && !operation.preparedOperation))
  return <div className="space-y-2 text-sm text-content-secondary" aria-live="polite">
    {accountWaiting && <AccountDeploymentConfirmation monitor={runtime.deploymentConfirmation} />}
    {deferStatusCheck && accountConfirmation === 'ready' && <p>Trading Account confirmed. Resume this saved attempt to continue.</p>}
    {walletStatus && 'recoveryState' in walletStatus && <>
      <p>{recoveryMessage(walletStatus.reason)}</p>
      {walletStatus.operationHashes.map(hash => {
        const outcome = walletStatus.operationOutcomes?.find(item => item.hash === hash)
        const label = outcome?.state === 'settled' ? outcome.executionSuccess ? 'Confirmed' : 'Execution reverted'
          : outcome?.state === 'expired' || outcome?.state === 'cancelled' ? 'Safely resolved'
          : outcome?.state === 'submitted' ? 'Submitted; awaiting safe confirmation' : 'Sponsorship outstanding'
        return <p key={hash} className="break-all">{label}: <code>{hash}</code></p>
      })}
    </>}
    {walletRecovery && !verified && (!operation.userOperationHash || walletRequired
      || !isSponsoredOperationTerminal(operation.status) || operation.status === 'outcome-unknown') && <div>
      <p>{operation.userOperationHash
        ? 'Verify ownership with a gas-free wallet message to restore status checks for this signed attempt. This does not submit it again.'
        : 'Verify ownership with a gas-free wallet message to check or discard this saved attempt.'}</p>
      <Button type="button" variant="secondary" size="sm" disabled={working} onClick={() => {
        setWorking(true); setError(undefined)
        void walletRecovery.verify(operation.id).then(() => {
          // Poll the verified session separately. A failed status read must not
          // discard a successful proof or require another wallet signature.
          setWalletStatus(undefined); setStatus(undefined)
          setVerified(true); setWalletRequired(false); setStatusError(undefined)
        }).catch((cause: unknown) => { setError(recoveryFailure(operation.id, 'verification-failed', cause)) })
          .finally(() => { setWorking(false) })
      }}>Verify wallet to recover</Button>
    </div>}
    {waiting && <><p>Waiting for sponsorship reservation to clear</p><p>Authorization expiry and safe reconciliation are separate. Clearance depends on verified chain progress.</p></>}
    {status?.phase === 'expiry-awaiting-reconciliation' && !waiting && <p>The authorization expired. Checking safe chain evidence before resolving the operation.</p>}
    {safelyResolved && <p>{status.authorizationState === 'expired'
      ? 'The unused sponsorship has safely expired.' : 'The original sponsorship has been safely resolved.'} {operation.userOperationHash
        ? 'Checking the signed transaction against the chain before unlocking a fresh action.'
        : 'Discard this saved attempt to unlock a fresh action.'}
      {operation.action === 'cancel-protection' && ' Discarding does not cancel TP/SL onchain.'}</p>}
    {!safelyResolved && status?.reason === 'PREPARATION_UNUSABLE' && <p>This preparation cannot be resumed. Check recovery to discard it when available.</p>}
    {status?.reason === 'INTENT_ALREADY_COMMITTED' && <p>This intent was already committed. Check order activity before reviewing a new action.</p>}
    {status?.phase === 'included' && <p>Included onchain. Waiting for safe confirmation.</p>}
    {status?.phase === 'submitted' && <p>This operation was submitted. Check its existing outcome before continuing.</p>}
    {visibleError && <p role="alert">{visibleError}</p>}
    {!deferStatusCheck && <Button type="button" variant="secondary" size="sm" disabled={working || checkingStatus} onClick={() => {
      setError(undefined)
      setStatusCheckRevision(value => value + 1)
    }}>{checkingStatus ? 'Checking recovery…' : 'Check recovery again'}</Button>}
    {status?.recoverable && <p>Resume asks your wallet to sign the saved transaction again.</p>}
    {!operation.userOperationHash && <div className="flex flex-col gap-2 sm:flex-row sm:flex-wrap sm:gap-3">
      {canOfferResume && <Button type="button" variant="primary" size="sm" disabled={!request || active || checkingStatus || accountWaiting} onClick={() => {
        setWorking(true); setError(undefined)
        void resumeSponsoredPerpsAction(operation, runtime).catch((cause: unknown) => {
          const reason = recoveryReason(cause)
          setError(recoveryFailure(operation.id, 'resume-failed', cause))
          if (reason === 'RECOVERY_VERIFICATION_REQUIRED' || reason === 'PREPARATION_NOT_AUTHORIZED') {
            setWalletRequired(true); setVerified(false); setWalletStatus(undefined)
          }
        }).finally(() => { setWorking(false) })
      }}>Resume {sponsoredOperationActionLabel(operation.action).toLowerCase() || 'transaction'}</Button>}
      <Button type="button" variant={freshAllowed ? 'primary' : 'secondary'} size="sm" disabled={active || checkingStatus || !(freshAllowed === true || (walletStatus !== undefined && 'recoveryState' in walletStatus && walletStatus.recoveryState === 'retired'))} onClick={() => {
        setWorking(true); setError(undefined)
        void (async () => {
          const release = await acquireSponsoredOperationBrowserLane({ chainId: operation.chainId, accountAddress: operation.accountAddress, lane: operation.lane })
          try {
            restoreSponsoredOperationLane(operation)
            await useSponsoredOperationStore.persist.rehydrate()
            const current = useSponsoredOperationStore.getState().operations.find(item => item.id === operation.id)
            if (!current || current.userOperationHash || hasSponsoredOperationSignal(current.id)) throw new Error('The operation changed; check recovery again')
            if (runtime.preparationRecovery) {
              const result = await runtime.preparationRecovery.retire(operation.id)
              setWalletStatus(result)
              if (!('recoveryState' in result) || result.recoveryState !== 'retired') {
                throw new PreparationRecoveryError(result.reason)
              }
              const store = useSponsoredOperationStore.getState()
              store.markPreparationResolved(current.id)
              store.transition(current.id, 'cancelled')
              return
            }
            const native = runtime
            const checked = await native.smartAccount.getPreparationStatus?.({ preparationId: operation.id })
            if (!checked?.freshReviewAllowed || !(current.walletPreparationOutcome === 'declined' || checked.phase === 'resolved' || checked.phase === 'settled' || !current.preparedOperation)) throw new Error('A fresh review is not available yet')
            if (checked.phase === 'settled') {
              const hash = current.preparedOperation?.expectedHash
              if (!hash || hash !== checked.userOperationHash) throw new Error('The original payload is unavailable. Review account activity before continuing.')
              const outcome = await reconcileUserOperation({ runtime: native, userOperationHash: hash })
              if (outcome.kind !== 'confirmed' && !(outcome.kind === 'terminal' && outcome.terminalStatus === 'execution-reverted')) {
                throw new Error('Safe chain confirmation is still pending')
              }
              if (!outcome.transactionHash) throw new Error('The verified transaction locator is unavailable')
              const store = useSponsoredOperationStore.getState()
              if (!store.recordUserOperationHash(current.id, hash)) throw new Error('The verified recovery outcome could not be saved')
              store.recordTransactionHash(current.id, outcome.transactionHash)
              if (outcome.kind === 'confirmed') {
                if (current.authorizationToken && current.authorizationNonce) {
                  try {
                    clearDepositAuthorization({ chainId: current.chainId, ownerAddress: current.ownerAddress, accountAddress: current.accountAddress,
                      token: current.authorizationToken, expectedNonce: current.authorizationNonce })
                  } catch { /* Safe confirmation remains authoritative if local cleanup fails. */ }
                }
                store.transition(current.id, 'confirmed')
              } else store.failOperation({ id: current.id, status: 'execution-reverted', retryable: false })
            } else useSponsoredOperationStore.getState().transition(operation.id, 'cancelled')
          } finally { await release() }
        })().catch((cause: unknown) => {
          setError(recoveryFailure(operation.id, 'discard-failed', cause))
          const reason = recoveryReason(cause)
          if (reason === 'RECOVERY_VERIFICATION_REQUIRED' || reason === 'PREPARATION_NOT_AUTHORIZED') {
            setWalletRequired(true); setVerified(false); setWalletStatus(undefined)
          }
        }).finally(() => { setWorking(false) })
      }}>Discard saved transaction</Button>
    </div>}
  </div>
}
