import { afterEach, describe, expect, it, vi } from 'vitest'
import { responseClock } from '../responseClock'

afterEach(() => { vi.restoreAllMocks() })
describe('response clock', () => {
  it('includes timestamp precision, response age and transport time, then advances monotonically', () => {
    let elapsed = 2_000
    vi.spyOn(performance, 'now').mockImplementation(() => elapsed)
    const clock = responseClock(new Response('', { headers: { Date: new Date(100_000).toUTCString(), Age: '2' } }), 500)
    expect(clock.now()).toBe(104_500)
    vi.spyOn(Date, 'now').mockReturnValue(0)
    elapsed += 5_000
    expect(clock.now()).toBe(109_500)
  })
  it.each([{}, { Date: 'invalid' }, { Date: new Date(0).toUTCString(), Age: '-1' },
    { Date: new Date(0).toUTCString(), Age: '9007199254740992' }])('rejects unverifiable timestamps %j', headers => {
    expect(() => responseClock(new Response('', { headers }), performance.now())).toThrow('Response time unavailable')
  })
})
