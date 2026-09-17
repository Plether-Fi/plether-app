import { afterEach, describe, expect, it, vi } from 'vitest'
import { createDeploymentConfirmationGate } from '../deploymentConfirmation'
import { SponsorRequestError, sponsorReasonMessage } from '../errors'

const pending = () => new SponsorRequestError({ reason: 'ACCOUNT_DEPLOYMENT_PENDING', retryable: true, message: 'pending' })

describe('deployment confirmation preparation gate', () => {
  afterEach(() => { vi.useRealTimers() })

  it('persists waiting across reloads, isolates deployments, and clears only after a safe read', async () => {
    const values = new Map<string, string>()
    const storage = () => ({
      getItem: (key: string) => values.get(key) ?? null,
      setItem: (key: string, value: string) => { values.set(key, value) },
      removeItem: (key: string) => { values.delete(key) },
    })
    const scope = '421614:factory:entrypoint:manifest:account-a'
    const run = createDeploymentConfirmationGate(async () => false, undefined, { scope, storage })
    await expect(run(async () => { throw pending() })).rejects.toMatchObject({ reason: 'ACCOUNT_DEPLOYMENT_PENDING' })
    const reloaded = createDeploymentConfirmationGate(async () => false, undefined, { scope, storage })
    expect(reloaded.getSnapshot()).toBe('waiting')
    const other = createDeploymentConfirmationGate(async () => false, undefined, { scope: `${scope}-other`, storage })
    expect(other.getSnapshot()).toBe('idle')
    const prepare = vi.fn()
    await expect(reloaded(prepare)).rejects.toMatchObject({ reason: 'ACCOUNT_DEPLOYMENT_PENDING' })
    expect(prepare).not.toHaveBeenCalled()
    const confirmed = createDeploymentConfirmationGate(async () => true, undefined, { scope, storage })
    await confirmed(async () => 'manually retried')
    expect(values.size).toBe(0)
    expect(createDeploymentConfirmationGate(async () => false, undefined, { scope, storage }).getSnapshot()).toBe('idle')
  })

  it('polls only read-only evidence, not actions, and stops polling on cleanup', async () => {
    vi.useFakeTimers()
    const check = vi.fn().mockResolvedValue(false)
    const run = createDeploymentConfirmationGate(check, () => Date.now())
    const prepare = vi.fn().mockRejectedValue(pending())
    const changed = vi.fn()
    const unsubscribe = run.subscribe(changed)
    await expect(run(prepare)).rejects.toMatchObject({ reason: 'ACCOUNT_DEPLOYMENT_PENDING' })
    const stop = run.start()
    await vi.advanceTimersByTimeAsync(15_000)
    expect(run.getSnapshot()).toBe('waiting')
    check.mockRejectedValueOnce(new Error('provider down'))
    await vi.advanceTimersByTimeAsync(15_000)
    expect(run.getSnapshot()).toBe('check-unavailable')
    check.mockResolvedValue(true)
    await vi.advanceTimersByTimeAsync(15_000)
    expect(run.getSnapshot()).toBe('ready')
    expect(prepare).toHaveBeenCalledTimes(1)
    expect(changed).toHaveBeenCalledTimes(4)
    stop()
    unsubscribe()
    await vi.advanceTimersByTimeAsync(60_000)
    expect(check).toHaveBeenCalledTimes(3)
    expect(vi.getTimerCount()).toBe(0)
  })

  it('survives denied persistence and still gates repeated actions', async () => {
    const run = createDeploymentConfirmationGate(async () => false, undefined, {
      scope: 'account', storage: () => { throw new Error('storage denied') },
    })
    const prepare = vi.fn().mockRejectedValue(pending())
    await expect(run(prepare)).rejects.toMatchObject({ reason: 'ACCOUNT_DEPLOYMENT_PENDING' })
    expect(run.getSnapshot()).toBe('waiting')
    await expect(run(prepare)).rejects.toMatchObject({ reason: 'ACCOUNT_DEPLOYMENT_PENDING' })
    expect(prepare).toHaveBeenCalledTimes(1)
  })

  it('does not let an older safe read erase a newer backend pending decision', async () => {
    let now = 0
    let confirm!: (value: boolean) => void
    let rejectSecond!: (error: unknown) => void
    const run = createDeploymentConfirmationGate(() => new Promise<boolean>(yes => { confirm = yes }), () => now)
    // Both actions started before either backend response marked the account pending.
    const delayed = expect(run(() => new Promise<string>((_, no) => { rejectSecond = no }))).rejects.toThrow()
    await expect(run(async () => { throw pending() })).rejects.toThrow()
    now = 15_000
    const stop = run.start()
    rejectSecond(pending())
    await delayed
    confirm(true)
    await Promise.resolve()
    await Promise.resolve()
    expect(run.getSnapshot()).toBe('waiting')
    stop()
  })

  it('adds no reads to ordinary preparation and preserves unrelated errors', async () => {
    const check = vi.fn()
    const run = createDeploymentConfirmationGate(check)
    await expect(run(async () => 'exact operation')).resolves.toBe('exact operation')
    const denied = new Error('denied')
    await expect(run(async () => { throw denied })).rejects.toBe(denied)
    expect(check).not.toHaveBeenCalled()
  })

  it('suppresses preparation retries until safe code is visible, without automatically preparing', async () => {
    let now = 0
    const check = vi.fn().mockResolvedValue(false)
    const run = createDeploymentConfirmationGate(check, () => now)
    const prepare = vi.fn().mockRejectedValueOnce({ cause: { data: { reason: 'ACCOUNT_DEPLOYMENT_PENDING' } } }).mockResolvedValue('fresh operation')
    await expect(run(prepare)).rejects.toMatchObject({ reason: 'ACCOUNT_DEPLOYMENT_PENDING' })
    await expect(run(prepare)).rejects.toMatchObject({ reason: 'ACCOUNT_DEPLOYMENT_PENDING' })
    expect(prepare).toHaveBeenCalledTimes(1)
    expect(check).not.toHaveBeenCalled()
    now = 15_000
    await expect(run(prepare)).rejects.toMatchObject({ reason: 'ACCOUNT_DEPLOYMENT_PENDING' })
    await expect(run(prepare)).rejects.toMatchObject({ reason: 'ACCOUNT_DEPLOYMENT_PENDING' })
    expect(check).toHaveBeenCalledTimes(1)
    check.mockResolvedValue(true)
    now = 30_000
    expect(prepare).toHaveBeenCalledTimes(1)
    await expect(run(prepare)).resolves.toBe('fresh operation')
    expect(prepare).toHaveBeenCalledTimes(2)
    await run(prepare)
    expect(check).toHaveBeenCalledTimes(2)
  })

  it('coalesces safe checks and retains waiting when the provider fails', async () => {
    let now = 0
    let reject!: (error: Error) => void
    const check = vi.fn(() => new Promise<boolean>((_, no) => { reject = no }))
    const run = createDeploymentConfirmationGate(check, () => now)
    const prepare = vi.fn().mockRejectedValue(pending())
    await expect(run(prepare)).rejects.toMatchObject({ reason: 'ACCOUNT_DEPLOYMENT_PENDING' })
    now = 15_000
    const first = expect(run(prepare)).rejects.toMatchObject({ reason: 'ACCOUNT_DEPLOYMENT_PENDING' })
    const second = expect(run(prepare)).rejects.toMatchObject({ reason: 'ACCOUNT_DEPLOYMENT_PENDING' })
    reject(new Error('RPC unavailable'))
    await Promise.all([first, second])
    expect(check).toHaveBeenCalledTimes(1)
    expect(prepare).toHaveBeenCalledTimes(1)
  })

  it('keeps account/runtime gates isolated and explains the pending state', async () => {
    const first = createDeploymentConfirmationGate(vi.fn())
    const second = createDeploymentConfirmationGate(vi.fn())
    await expect(first(async () => { throw pending() })).rejects.toMatchObject({ reason: 'ACCOUNT_DEPLOYMENT_PENDING' })
    await expect(second(async () => 'other account')).resolves.toBe('other account')
    expect(sponsorReasonMessage(pending())).toContain('awaiting safe confirmation')
    expect(sponsorReasonMessage(pending())).toContain('has not been sent')
  })
  it('pauses hidden-page checks, refreshes on focus, and retains elapsed time after reload', async () => {
    vi.useFakeTimers()
    const values = new Map<string, string>()
    const persistence = { scope: 'scope', storage: () => ({ getItem: (key: string) => values.get(key) ?? null,
      setItem: (key: string, value: string) => { values.set(key, value) }, removeItem: (key: string) => { values.delete(key) } }) }
    const check = vi.fn(async () => false)
    const run = createDeploymentConfirmationGate(check, () => Date.now(), persistence)
    await expect(run(async () => { throw pending() })).rejects.toThrow()
    const started = run.getDetails().waitingSince
    const stop = run.start()
    vi.spyOn(document, 'visibilityState', 'get').mockReturnValue('hidden')
    await vi.advanceTimersByTimeAsync(300_000)
    expect(check).not.toHaveBeenCalled()
    vi.restoreAllMocks()
    window.dispatchEvent(new Event('focus'))
    await vi.advanceTimersByTimeAsync(0)
    expect(check).toHaveBeenCalledTimes(1)
    expect(run.getDetails().lastSuccessfulCheckAt).toBeDefined()
    const restored = createDeploymentConfirmationGate(check, () => Date.now(), persistence)
    expect(restored.getDetails().waitingSince).toBe(started)
    stop()
  })

  it('migrates the legacy wait once without resurrecting it after confirmation', async () => {
    vi.useFakeTimers()
    const values = new Map<string, string>([['plether:deployment-confirmation:v1:legacy', 'waiting']])
    const persistence = { scope: 'paymaster-scope', legacyScope: 'legacy', storage: () => ({ getItem: (key: string) => values.get(key) ?? null,
      setItem: (key: string, value: string) => { values.set(key, value) }, removeItem: (key: string) => { values.delete(key) } }) }
    const check = vi.fn(async () => true)
    const run = createDeploymentConfirmationGate(check, () => Date.now(), persistence)
    expect(run.getSnapshot()).toBe('waiting')
    expect(values.has('plether:deployment-confirmation:v1:legacy')).toBe(false)
    const stop = run.start()
    await vi.advanceTimersByTimeAsync(0)
    expect(run.getSnapshot()).toBe('ready')
    await vi.advanceTimersByTimeAsync(60_000)
    expect(check).toHaveBeenCalledTimes(1)
    expect(createDeploymentConfirmationGate(check, () => Date.now(), persistence).getSnapshot()).toBe('idle')
    stop()
  })

  it('ignores an in-flight confirmation after the saved scope unmounts', async () => {
    vi.useFakeTimers()
    let complete!: (ready: boolean) => void
    const run = createDeploymentConfirmationGate(() => new Promise<boolean>(resolve => { complete = resolve }), () => Date.now())
    await expect(run(async () => { throw pending() })).rejects.toThrow()
    const stop = run.start()
    await vi.advanceTimersByTimeAsync(15_000)
    stop()
    complete(true)
    await vi.advanceTimersByTimeAsync(0)
    expect(run.getSnapshot()).toBe('waiting')
  })

})
