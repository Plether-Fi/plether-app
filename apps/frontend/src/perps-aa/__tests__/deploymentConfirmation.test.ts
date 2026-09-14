import { describe, expect, it, vi } from 'vitest'
import { createDeploymentConfirmationGate } from '../deploymentConfirmation'
import { SponsorRequestError, sponsorReasonMessage } from '../errors'

const pending = () => new SponsorRequestError({ reason: 'ACCOUNT_DEPLOYMENT_PENDING', retryable: true, message: 'pending' })

describe('deployment confirmation preparation gate', () => {
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
})
