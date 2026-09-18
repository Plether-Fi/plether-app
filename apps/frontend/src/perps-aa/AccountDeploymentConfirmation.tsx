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
          <strong>Trading Account awaiting confirmation.</strong>
          <span>{status === 'check-unavailable'
            ? ' We cannot check network confirmation right now. Your saved attempt is retained, and we’ll keep checking.'
            : ' The network needs to confirm your new account before the next action. This can take several minutes.'}</span>
          {minutes > 0 && <span>{` Waiting ${String(minutes)} ${minutes === 1 ? 'minute' : 'minutes'}.`}</span>}
          {minutes >= 5 && <span> Confirmation is still pending; elapsed time alone does not make the account ready.</span>}
          {details.lastSuccessfulCheckAt && <span>{` Last successful check: ${new Date(details.lastSuccessfulCheckAt).toLocaleTimeString()}.`}</span>}
          <span> We’re checking automatically; no transaction will be sent automatically.</span>
        </>
      ) : (
        <><strong>Trading Account confirmed.</strong><span> You can now retry your action.</span></>
      )}
    </div>
  )
}
