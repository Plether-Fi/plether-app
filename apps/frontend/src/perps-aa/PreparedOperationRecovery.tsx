import { useEffect, useState } from 'react'
import type { PerpsAaDeploymentManifestV2 } from './manifest'
import { clearDepositAuthorization } from './authorizationStore'
import { reconcileUserOperation } from './operationReconciler'
import { resumeSponsoredPerpsAction } from './execution'
import { acquireSponsoredOperationBrowserLane } from './laneLock'
import { hasSponsoredOperationSignal, isSponsoredOperationTerminal, restoreSponsoredOperationLane, useSponsoredOperationStore, type SponsoredOperation } from './operationStore'
import { usePerpsAaRuntime } from './runtimeContext'
import type { PreparationStatusV1 } from './preparedOperation'
import { sponsoredOperationActionLabel } from '../utils/sponsoredOperation'
import { Button } from '../components/ui/Button'

/** Mounted only in the visible recovery view. Status changes never sign or submit. */
export function PreparedOperationRecovery({ operation, fallbackManifest }: { operation: SponsoredOperation; fallbackManifest?: PerpsAaDeploymentManifestV2 }) {
  const runtime = usePerpsAaRuntime()
  const [status, setStatus] = useState<PreparationStatusV1>()
  const [error, setError] = useState<string>()
  const [working, setWorking] = useState(false)
  const request = operation.nativePreparation
  const recoveryManifest = request?.manifest ?? fallbackManifest

  useEffect(() => {
    if (!runtime || !recoveryManifest) return
    const lifecycle = { stopped: false }
    const isStopped = () => lifecycle.stopped
    let inFlight = false
    let timer: ReturnType<typeof setTimeout> | undefined
    const refresh = async () => {
      if (isStopped() || inFlight || document.visibilityState === 'hidden') return
      inFlight = true
      clearTimeout(timer)
      let delay = 15_000
      let resolved = false
      try {
        const native = runtime.getPreparedOperationRuntime ? await runtime.getPreparedOperationRuntime(recoveryManifest) : runtime
        const result = await native.smartAccount.getPreparationStatus?.({ preparationId: operation.id })
        if (!result) throw new Error('Preparation status is unavailable')
        if (!isStopped()) { setStatus(result); setError(undefined) }
        resolved = (result.phase === 'resolved' || result.phase === 'settled') && result.freshReviewAllowed
        if (resolved && !isStopped()) useSponsoredOperationStore.getState().markPreparationResolved(operation.id)
      } catch (cause) {
        delay = 60_000
        if (!isStopped()) { setStatus(undefined); setError(cause instanceof Error ? cause.message : 'Unable to check preparation status') }
      } finally {
        inFlight = false
        if (!isStopped() && !resolved) timer = setTimeout(() => { void refresh() }, delay)
      }
    }
    const onFocus = () => { void refresh() }
    void refresh()
    window.addEventListener('focus', onFocus)
    document.addEventListener('visibilitychange', onFocus)
    return () => { lifecycle.stopped = true; clearTimeout(timer); window.removeEventListener('focus', onFocus); document.removeEventListener('visibilitychange', onFocus) }
  }, [runtime, recoveryManifest, operation.id, operation.status])

  if (!runtime || !recoveryManifest) return null
  if (!request) return <div className="text-sm text-content-secondary">
    <p>This older record has no recoverable transaction payload. Check account activity before reviewing a new transaction.</p>
    {status?.reason === 'SAFE_EXPIRY_WAIT' || status?.reason === 'ASSISTANCE_RESERVATION_PENDING'
      ? <p>Waiting for sponsorship reservation to clear. Authorization expiry and safe reconciliation are separate.</p> : null}
    {error && <p>{error}</p>}
  </div>
  if (isSponsoredOperationTerminal(operation.status)) return <p className="text-sm text-content-secondary">
    {status?.phase === 'resolved' || status?.phase === 'settled' ? 'The original sponsorship reservation is resolved.'
      : 'The saved preparation is retained until its sponsorship reservation is safely resolved.'}
  </p>
  const waiting = status?.reason === 'ASSISTANCE_RESERVATION_PENDING' || status?.reason === 'SAFE_EXPIRY_WAIT'
  const active = working || hasSponsoredOperationSignal(operation.id)
  // An interrupted wallet request is not proof of rejection. Only safe expiry
  // or confirmed rejection permits abandoning the original unsigned payload.
  const freshAllowed = status?.freshReviewAllowed && (operation.walletPreparationOutcome === 'declined'
    || status.phase === 'resolved' || status.phase === 'settled' || !operation.preparedOperation)
  return <div className="space-y-2 text-sm text-content-secondary" aria-live="polite">
    {waiting && <><p>Waiting for sponsorship reservation to clear</p><p>Authorization expiry and safe reconciliation are separate. Clearance depends on verified chain progress.</p></>}
    {status?.phase === 'expiry-awaiting-reconciliation' && !waiting && <p>The authorization expired. Checking safe chain evidence before resolving the operation.</p>}
    {status?.reason === 'PREPARATION_UNUSABLE' && <p>This preparation cannot be resumed. Review current account state before preparing another transaction.</p>}
    {status?.reason === 'INTENT_ALREADY_COMMITTED' && <p>This intent was already committed. Check order activity before reviewing a new action.</p>}
    {status?.phase === 'included' && <p>Included onchain. Waiting for safe confirmation.</p>}
    {status?.phase === 'submitted' && <p>This operation was submitted. Check its existing outcome before continuing.</p>}
    {error && <p role="alert">{error}</p>}
    {status?.recoverable && <p>Resume asks your wallet to sign the saved transaction again.</p>}
    <div className="flex flex-col gap-2 sm:flex-row sm:flex-wrap sm:gap-3">
      <Button type="button" variant="primary" size="sm" disabled={active || (operation.preparedOperation ? !status?.recoverable : waiting)} onClick={() => {
        setWorking(true); setError(undefined)
        void resumeSponsoredPerpsAction(operation, runtime).catch((cause: unknown) => {
          setError(cause instanceof Error ? cause.message : 'Resume could not complete')
        }).finally(() => { setWorking(false) })
      }}>Resume {sponsoredOperationActionLabel(operation.action).toLowerCase() || 'transaction'}</Button>
      <Button type="button" variant="secondary" size="sm" disabled={active || !freshAllowed} onClick={() => {
        setWorking(true)
        void (async () => {
          const release = await acquireSponsoredOperationBrowserLane({ chainId: operation.chainId, accountAddress: operation.accountAddress, lane: operation.lane })
          try {
            restoreSponsoredOperationLane(operation)
            await useSponsoredOperationStore.persist.rehydrate()
            const current = useSponsoredOperationStore.getState().operations.find(item => item.id === operation.id)
            if (!current || current.userOperationHash || isSponsoredOperationTerminal(current.status) || hasSponsoredOperationSignal(current.id)) throw new Error('The operation changed; check recovery again')
            const native = runtime.getPreparedOperationRuntime ? await runtime.getPreparedOperationRuntime(recoveryManifest) : runtime
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
        })().catch((cause: unknown) => { setError(cause instanceof Error ? cause.message : 'Unable to discard the saved transaction') }).finally(() => { setWorking(false) })
      }}>Discard saved transaction</Button>
    </div>
  </div>
}
