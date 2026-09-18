import { afterEach, describe, expect, it, vi } from 'vitest'
import { isModuleLoadError, retryModuleLoad } from './lazyWithRetry'

describe('bounded module loading recovery', () => {
  afterEach(() => vi.useRealTimers())

  it('retries a transient fetch once without reloading or touching recovery storage', async () => {
    vi.useFakeTimers()
    const reload = vi.spyOn(window.location, 'reload').mockImplementation(() => {})
    const clear = vi.spyOn(Storage.prototype, 'clear')
    const module = { default: () => null }
    const load = vi.fn().mockRejectedValueOnce(new TypeError('Failed to fetch dynamically imported module: https://example.com/chunk.js')).mockResolvedValue(module)
    const pending = retryModuleLoad(load)
    await vi.advanceTimersByTimeAsync(500)
    await expect(pending).resolves.toBe(module)
    expect(load).toHaveBeenCalledTimes(2)
    expect(reload).not.toHaveBeenCalled()
    expect(clear).not.toHaveBeenCalled()
    vi.restoreAllMocks()
  })

  it('surfaces persistent failures for manual recovery instead of looping', async () => {
    vi.useFakeTimers()
    const error = new TypeError('Importing a module script failed.')
    const load = vi.fn().mockRejectedValue(error)
    const rejection = expect(retryModuleLoad(load)).rejects.toBe(error)
    await vi.advanceTimersByTimeAsync(500)
    await rejection
    expect(load).toHaveBeenCalledTimes(2)
  })

  it('does not retry module evaluation errors or unrelated transaction/network errors', async () => {
    for (const error of [new Error('Transaction failed'), new TypeError('Failed to fetch'), new ReferenceError('notDefined is not defined')]) {
      const load = vi.fn().mockRejectedValue(error)
      await expect(retryModuleLoad(load)).rejects.toBe(error)
      expect(load).toHaveBeenCalledOnce()
    }
    expect(isModuleLoadError(new TypeError('error loading dynamically imported module: https://example.com/chunk.js'))).toBe(true)
  })
})
