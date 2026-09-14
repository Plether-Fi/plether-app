import { useSyncExternalStore } from 'react'
import { usePerpsAaRuntime } from './runtimeContext'

const subscribeIdle = () => () => { /* No runtime to subscribe to. */ }
const idle = () => 'idle' as const

export function useAccountDeploymentConfirmation() {
  const monitor = usePerpsAaRuntime()?.deploymentConfirmation
  return useSyncExternalStore(monitor?.subscribe ?? subscribeIdle, monitor?.getSnapshot ?? idle, idle)
}
