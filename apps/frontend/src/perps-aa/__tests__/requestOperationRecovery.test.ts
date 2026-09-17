import { afterEach, describe, expect, it, vi } from 'vitest'
import { registerOperationRecoveryCheck, requestOperationRecovery } from '../requestOperationRecovery'

describe('visible recovery requests', () => {
  afterEach(() => { vi.useRealTimers() })
  it('ends the visible wait without cancelling an unfinished coordinator check', async () => {
    vi.useFakeTimers()
    let complete!: (result: 'checked') => void
    const check = new Promise<'checked'>(resolve => { complete = resolve })
    const unregister = registerOperationRecoveryCheck(id => id === 'attempt' ? check : undefined)
    try {
      const request = requestOperationRecovery('attempt')
      await vi.advanceTimersByTimeAsync(20_000)
      expect(await request).toBe('unavailable')
      complete('checked')
      expect(await check).toBe('checked')
      expect(await requestOperationRecovery('different-attempt')).toBe('unavailable')
    } finally { unregister() }
  })
  it('reports a failed check without manufacturing a transaction outcome', async () => {
    const unregister = registerOperationRecoveryCheck(() => Promise.reject(new Error('offline')))
    try { expect(await requestOperationRecovery('attempt')).toBe('unavailable') }
    finally { unregister() }
  })
})
