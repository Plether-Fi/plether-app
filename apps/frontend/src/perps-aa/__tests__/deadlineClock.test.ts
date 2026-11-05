import { afterEach, describe, expect, it, vi } from 'vitest'
import { deadlineNow, refreshDeadlineClock } from '../deadlineClock'
import { requireDeadlineHeadroom } from '../deadline'

afterEach(() => { vi.restoreAllMocks(); vi.unstubAllGlobals() })
describe('authoritative order deadline clock', () => {
  it.each([-86_400_000, 86_400_000])('ignores a device wall-clock offset of %s ms', async offset => {
    vi.spyOn(Date, 'now').mockReturnValue(2_000_000_000_000 + offset)
    vi.spyOn(performance, 'now').mockReturnValue(500)
    vi.stubGlobal('fetch', vi.fn(async () => new Response('{}', { headers: { Date: new Date(2_000_000_000_000).toUTCString() } })))
    await refreshDeadlineClock()
    expect(deadlineNow()).toBe(2_000_000_001_000)
    expect(() => requireDeadlineHeadroom(2_000_000_120n, '2000000060', 'signing')).not.toThrow()
  })
  it.each([0,15,30,60,119,121])('uses refreshed server time after a %s-second wallet delay', async delay => {
    vi.spyOn(performance, 'now').mockReturnValue(500) // may pause during suspension
    const epoch = 2_000_000_000_000
    vi.stubGlobal('fetch', vi.fn(async () => new Response('{}', { headers: { Date: new Date(epoch + delay * 1000).toUTCString() } })))
    await refreshDeadlineClock()
    const submit = () => requireDeadlineHeadroom(2_000_000_300n, '2000000060', 'submission')
    if (delay < 30) expect(submit).not.toThrow()
    else expect(submit).toThrow('Wallet approval finished too late')
  })
  it('rejects a timing response without Date instead of consulting the device clock', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response('{}')))
    await expect(refreshDeadlineClock()).rejects.toThrow('Response time unavailable')
  })
  it('keeps the exact inclusive 45/30-second boundaries', () => {
    expect(() => requireDeadlineHeadroom(1000n, '1000', 'signing', 955000)).not.toThrow()
    expect(() => requireDeadlineHeadroom(1000n, '1000', 'signing', 955001)).toThrow()
    expect(() => requireDeadlineHeadroom(1000n, '1000', 'submission', 970000)).not.toThrow()
    expect(() => requireDeadlineHeadroom(1000n, '1000', 'submission', 970001)).toThrow()
    expect(() => requireDeadlineHeadroom(970n, '1100', 'submission', 941000)).toThrow()
  })
})
