import { useCallback, useEffect, useState } from 'react'
import {
  isKernelAttemptPending,
  kernelAttemptScopeKey,
  listKernelAttempts,
  refreshKernelAttempt,
  subscribeKernelAttemptUpdates,
  type KernelAttemptScope,
  type KernelObservationReorgCheck,
  type KernelOperationAttempt,
} from './kernelOperationJournal'

const emptyAttempts: KernelOperationAttempt[] = []

export function useKernelAttempts(scope: KernelAttemptScope | undefined, recover = false, isObservationReorged?: KernelObservationReorgCheck) {
  const { deployment, chainId, entryPoint, sender } = scope ?? {}
  const [snapshot, setSnapshot] = useState<{
    key: string
    attempts: KernelOperationAttempt[]
    error?: string
  }>()
  const [refreshCount, setRefreshCount] = useState(0)
  const refresh = useCallback(() => {
    setRefreshCount(count => count + 1)
  }, [])

  useEffect(() => {
    if (deployment === undefined || chainId === undefined || entryPoint === undefined || sender === undefined) {
      return
    }
    const currentScope = { deployment, chainId, entryPoint, sender }
    const key = kernelAttemptScopeKey(currentScope)
    let stopped = false
    let recovering = false
    let readSequence = 0
    let lastPolledHash: string | undefined

    const check = async (attempts: KernelOperationAttempt[]) => {
      if (!recover || recovering || stopped || document.visibilityState === 'hidden') return
      const pending = attempts.filter(attempt => isKernelAttemptPending(attempt) && attempt.phase !== 'signed')
      if (pending.length === 0) return
      const previous = pending.findIndex(attempt => attempt.prepared.userOperationHash === lastPolledHash)
      const attempt = pending[(previous + 1) % pending.length]
      if (!attempt) return
      lastPolledHash = attempt.prepared.userOperationHash
      recovering = true
      try {
        await refreshKernelAttempt(attempt, isObservationReorged)
      } catch {
        // A network error or concurrent poll never changes the saved attempt.
      } finally {
        recovering = false
      }
    }

    const load = async () => {
      const started = ++readSequence
      try {
        const attempts = await listKernelAttempts(currentScope)
        if (!stopped && started === readSequence) {
          setSnapshot({ key, attempts })
          void check(attempts)
        }
      } catch {
        if (!stopped && started === readSequence) {
          setSnapshot(previous => ({
            key,
            attempts: previous?.key === key ? previous.attempts : emptyAttempts,
            error: 'Trading Account activity could not be read from this browser.',
          }))
        }
      }
    }

    const update = () => {
      void load()
    }
    const unsubscribe = subscribeKernelAttemptUpdates(currentScope, update)
    const interval = recover ? window.setInterval(update, 10_000) : undefined
    document.addEventListener('visibilitychange', update)
    window.addEventListener('online', update)
    update()

    return () => {
      stopped = true
      unsubscribe()
      if (interval !== undefined) window.clearInterval(interval)
      document.removeEventListener('visibilitychange', update)
      window.removeEventListener('online', update)
    }
  }, [deployment, chainId, entryPoint, sender, recover, refreshCount, isObservationReorged])

  const active = snapshot && scope && snapshot.key === kernelAttemptScopeKey(scope) ? snapshot : undefined
  return { attempts: active?.attempts ?? emptyAttempts, error: active?.error, refresh }
}

export function KernelOperationRecovery({ scope, isObservationReorged }: {
  scope: KernelAttemptScope | undefined
  isObservationReorged?: KernelObservationReorgCheck
}) {
  useKernelAttempts(scope, true, isObservationReorged)
  return null
}
