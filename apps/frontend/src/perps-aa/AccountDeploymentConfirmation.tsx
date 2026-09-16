import { useEffect } from 'react'
import { usePerpsAaRuntime } from './runtimeContext'
import { useConfirmationStatus, useDeploymentConfirmationDetails } from './useAccountDeploymentConfirmation'

import { confirmationPending, type DeploymentConfirmationMonitor } from './deploymentConfirmation'

export function AccountDeploymentConfirmation({ monitor: suppliedMonitor }: { monitor?: DeploymentConfirmationMonitor } = {}) {
  const baseMonitor = usePerpsAaRuntime()?.deploymentConfirmation
  const monitor = suppliedMonitor ?? baseMonitor
  const status = useConfirmationStatus(monitor)
  const details = useDeploymentConfirmationDetails(monitor)
  const minutes = details.waitingSince ? Math.floor(((details.lastCheckedAt ?? details.waitingSince) - details.waitingSince) / 60_000) : 0
  useEffect(() => {
    if (confirmationPending(status)) return monitor?.start()
  }, [monitor, status])

  if (status === 'idle') return null
  return (
    <div role="status" aria-live="polite" className="border-b border-brand-orange/30 bg-surface-panel px-4 py-3 text-sm text-content-primary">
      {confirmationPending(status) ? (
        <>
          <strong>Trading Account awaiting confirmation.</strong>{' '}
          {status === 'check-unavailable'
            ? 'We cannot check network confirmation right now. Your saved attempt is retained, and we’ll keep checking.'
            : 'The network needs to confirm your new account before the next action. This can take several minutes.'}
          {minutes > 0 && <> Waiting {minutes} {minutes === 1 ? 'minute' : 'minutes'}.</>}
          {minutes >= 5 && <> Confirmation is still pending; elapsed time alone does not make the account ready.</>}
          {details.lastSuccessfulCheckAt && <> Last successful check: {new Date(details.lastSuccessfulCheckAt).toLocaleTimeString()}.</>}
          We’re checking automatically; no transaction will be sent automatically.
        </>
      ) : (
        <><strong>Trading Account confirmed.</strong> You can now retry your action.</>
      )}
    </div>
  )
}
