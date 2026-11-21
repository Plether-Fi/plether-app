import { useCallback, useEffect, useState } from 'react'
import {
  isKernelAttemptPending,
  kernelAttemptScopeKey,
  listKernelAttempts,
  refreshKernelAttempt,
  subscribeKernelAttemptUpdates,
  type KernelAttemptScope,
  type KernelOperationAttempt,
} from './kernelOperationJournal'

const emptyAttempts: KernelOperationAttempt[] = []

export function useKernelAttempts(scope: KernelAttemptScope | undefined, recover = false) {
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

    const check = async (attempts: KernelOperationAttempt[]) => {
      if (!recover || recovering || stopped || document.visibilityState === 'hidden') return
      const attempt = attempts.find(isKernelAttemptPending)
      if (!attempt) return
      recovering = true
      try {
        await refreshKernelAttempt(attempt)
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
  }, [deployment, chainId, entryPoint, sender, recover, refreshCount])

  const active = snapshot && scope && snapshot.key === kernelAttemptScopeKey(scope) ? snapshot : undefined
  return { attempts: active?.attempts ?? emptyAttempts, error: active?.error, refresh }
}

export function KernelOperationRecovery({ scope }: { scope: KernelAttemptScope | undefined }) {
  useKernelAttempts(scope, true)
  return null
}
