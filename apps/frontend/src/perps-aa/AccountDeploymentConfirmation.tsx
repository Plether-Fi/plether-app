import { useEffect } from 'react'
import { usePerpsAaRuntime } from './runtimeContext'
import { useAccountDeploymentConfirmation } from './useAccountDeploymentConfirmation'

export function AccountDeploymentConfirmation() {
  const monitor = usePerpsAaRuntime()?.deploymentConfirmation
  const status = useAccountDeploymentConfirmation()
  useEffect(() => {
    if (status === 'waiting') return monitor?.start()
  }, [monitor, status])

  if (status === 'idle') return null
  return (
    <div role="status" aria-live="polite" className="border-b border-brand-orange/30 bg-surface-panel px-4 py-3 text-sm text-content-primary">
      {status === 'waiting' ? (
        <>
          <strong>Trading Account awaiting confirmation.</strong>{' '}
          The network needs to confirm your new account before the next action. This can take several minutes.
          We’re checking automatically; no transaction will be sent automatically.
        </>
      ) : (
        <><strong>Trading Account confirmed.</strong> You can now retry your action.</>
      )}
    </div>
  )
}
