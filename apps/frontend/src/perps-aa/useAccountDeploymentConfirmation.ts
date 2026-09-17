import { useSyncExternalStore } from 'react'
import type { DeploymentConfirmationDetails, DeploymentConfirmationMonitor } from './deploymentConfirmation'
import { usePerpsAaRuntime } from './runtimeContext'

const subscribeIdle = () => () => { /* No runtime to subscribe to. */ }
const idle = () => 'idle' as const

export function useAccountDeploymentConfirmation() {
  const monitor = usePerpsAaRuntime()?.deploymentConfirmation
  return useConfirmationStatus(monitor)
}

export function useConfirmationStatus(monitor?: DeploymentConfirmationMonitor) {
  return useSyncExternalStore(monitor?.subscribe ?? subscribeIdle, monitor?.getSnapshot ?? idle, idle)
}

const idleDetails: DeploymentConfirmationDetails = { status: 'idle' }
const getIdleDetails = () => idleDetails
export function useDeploymentConfirmationDetails(monitor?: DeploymentConfirmationMonitor) {
  return useSyncExternalStore(monitor?.subscribe ?? subscribeIdle, monitor?.getDetails ?? getIdleDetails, getIdleDetails)
}
