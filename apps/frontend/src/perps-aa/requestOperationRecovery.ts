type RecoveryCheckResult = 'checked' | 'unavailable'
type RecoveryCheckHandler = (id: string) => Promise<RecoveryCheckResult> | undefined

const handlers = new Set<RecoveryCheckHandler>()

/** Tab-local requests use the existing coordinator and its browser locks. */
export function registerOperationRecoveryCheck(handler: RecoveryCheckHandler): () => void {
  handlers.add(handler)
  return () => { handlers.delete(handler) }
}

/** A completed check is not proof of success; only the operation store carries outcomes. */
export async function requestOperationRecovery(id: string): Promise<RecoveryCheckResult> {
  for (const handler of handlers) {
    const check = handler(id)
    if (!check) continue
    let timer: ReturnType<typeof setTimeout> | undefined
    try {
      return await Promise.race([
        check,
        new Promise<RecoveryCheckResult>(resolve => {
          // Stop the visible spinner without cancelling the check or releasing its lock.
          timer = setTimeout(() => { resolve('unavailable') }, 20_000)
        }),
      ])
    } catch {
      return 'unavailable'
    } finally { clearTimeout(timer) }
  }
  return 'unavailable'
}
