import { useEffect, useState } from 'react'
import type { SponsoredOperation } from './operationStore'
import type { PerpsAaDeploymentManifestV2 } from './manifest'
import { usePerpsAaRuntime, type PerpsAaSmartAccountRuntime } from './runtimeContext'
import { useConfirmationStatus, useDeploymentConfirmationDetails } from './useAccountDeploymentConfirmation'
import { confirmationPending } from './deploymentConfirmation'
import { reportRecoveryDiagnostic } from './recoveryDiagnostics'
import { useSponsoredOperationStore } from './operationStore'

const normalized = (address?: string) => address?.toLowerCase()

export function useSavedOperationRuntime(operation?: SponsoredOperation, fallbackManifest?: PerpsAaDeploymentManifestV2) {
  const base = usePerpsAaRuntime()
  const manifest = operation?.nativePreparation?.manifest ?? fallbackManifest
  const [resolved, setResolved] = useState<{ base: PerpsAaSmartAccountRuntime; manifest: PerpsAaDeploymentManifestV2; runtime?: PerpsAaSmartAccountRuntime }>()
  useEffect(() => {
    if (!base?.getPreparedOperationRuntime || !manifest) return
    let stopped = false
    void base.getPreparedOperationRuntime(manifest).then(runtime => {
      if (!stopped) setResolved({ base, manifest, runtime })
    }).catch(() => { if (!stopped) setResolved({ base, manifest }) })
    return () => { stopped = true }
  }, [base, manifest])
  const result = base?.getPreparedOperationRuntime
    ? resolved?.base === base && resolved.manifest === manifest ? resolved.runtime : undefined
    : base
  const matches = operation && result?.chainId === operation.chainId
    && normalized(result.ownerAddress) === normalized(operation.ownerAddress)
    && normalized(result.smartAccount.accountAddress) === normalized(operation.accountAddress)
  const unavailable = result ? !matches : Boolean(resolved && resolved.base === base && resolved.manifest === manifest && !resolved.runtime)
  return { runtime: matches ? result : undefined, unavailable }
}

/** The ticket observes the saved deployment even while Activity is closed. */
export function useSavedOperationConfirmation(operation?: SponsoredOperation) {
  const { runtime, unavailable } = useSavedOperationRuntime(operation)
  const monitor = runtime?.deploymentConfirmation
  const status = useConfirmationStatus(monitor)
  const details = useDeploymentConfirmationDetails(monitor)
  useEffect(() => {
    if (operation?.reason === 'ACCOUNT_DEPLOYMENT_PENDING' && !operation.preparedOperation) {
      monitor?.notePending?.(operation.confirmationWaitingSince)
    }
    if (monitor && (confirmationPending(monitor.getSnapshot()))) return monitor.start()
  }, [monitor, operation?.id, operation?.reason, operation?.preparedOperation, operation?.confirmationWaitingSince])
  useEffect(() => {
    if (!operation?.id || status === 'idle' || !details.lastCheckedAt) return
    useSponsoredOperationStore.getState().recordConfirmationTiming(operation.id, details.waitingSince, details.lastSuccessfulCheckAt)
    reportRecoveryDiagnostic({ operationKey: operation.id, attemptId: operation.id,
      stage: status === 'ready' ? 'account_confirmation_ready'
        : status === 'waiting' ? 'account_confirmation_waiting' : 'account_confirmation_unavailable',
      safeBlockNumber: details.safeBlockNumber ? BigInt(details.safeBlockNumber) : undefined,
      safeBlockTimestamp: details.safeBlockTimestamp, lastSuccessfulCheckAt: details.lastSuccessfulCheckAt })
  }, [operation?.id, status, details])
  return unavailable ? 'check-unavailable' : status
}
