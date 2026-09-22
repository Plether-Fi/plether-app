import { afterEach, describe, expect, it, vi } from 'vitest'
import { currentReadiness, refreshReadiness, parseReadiness, readinessBlocker, readinessChecks, readinessWorkers, readinessMessage, type ReadinessSnapshot } from '../readiness'
import { requireDeadlineHeadroom } from '../deadline'
import { sanitizeAnalyticsProperties, sanitizeFrontendLogAttributes } from '../../analytics/client'

const sample = (): ReadinessSnapshot => ({ version: 1, observedAt: 100_000, expiresAt: 115_000, enforcementEnabled: true,
  actions: { deposit: [{ component: 'sponsorship', status: 'ready', reason: 'READY' }], open: [{ component: 'keeper', status: 'blocked', reason: 'KEEPER_INSUFFICIENT_FUNDS' }], close: [{ component: 'keeper', status: 'unknown', reason: 'WORKER_HEARTBEAT_STALE' }], protection: [{ component: 'oracle', status: 'unknown', reason: 'ORACLE_UNAVAILABLE' }] } })
afterEach(() => { vi.restoreAllMocks(); vi.unstubAllGlobals() })
describe('trading readiness', () => {
  it.each([-86_400_000, -6_000, 6_000, 86_400_000])('uses response time for a device offset of %i ms and still expires', async offset => {
    let elapsed = 20_000
    vi.spyOn(performance, 'now').mockImplementation(() => elapsed)
    vi.spyOn(Date, 'now').mockReturnValue(100_000 + offset)
    const fetcher = vi.fn(async () => new Response(JSON.stringify(sample()), { headers: { Date: new Date(100_000).toUTCString() } }))
    vi.stubGlobal('fetch', fetcher)
    await refreshReadiness(true)
    const snapshot = currentReadiness()
    expect(snapshot).toEqual(sample())
    expect(readinessChecks(snapshot, 'deposit')[0].status).toBe('ready')
    expect(readinessBlocker(snapshot, 'open')?.reason).toBe('KEEPER_INSUFFICIENT_FUNDS')
    elapsed += 14_000
    vi.mocked(Date.now).mockReturnValue(0)
    expect(readinessChecks(snapshot, 'deposit')[0].status).toBe('unknown')
    expect(readinessBlocker(snapshot, 'open')).toBeUndefined()
    await refreshReadiness()
    expect(fetcher).toHaveBeenCalledTimes(2)
  })
  it('does not turn a stale response into a fresh blocker', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify(sample()), { headers: { Date: new Date(120_000).toUTCString() } })))
    await refreshReadiness(true)
    expect(readinessBlocker(currentReadiness(), 'open')).toBeUndefined()
    expect(readinessChecks(currentReadiness(), 'deposit')[0].status).toBe('unknown')
  })
  it('fails to unknown when response time is missing', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify(sample()))))
    await refreshReadiness(true)
    expect(currentReadiness()).toBeUndefined()
  })
  it('explains verified historical gas failure without blaming funding or current readiness', () => {
    expect(readinessMessage('USER_OPERATION_OUT_OF_GAS')).toContain('ran out of execution gas')
    expect(readinessMessage('USER_OPERATION_OUT_OF_GAS')).toContain('separately completed transfer')
    expect(readinessMessage('USER_OPERATION_REVERTED')).toContain('could not be verified')
    expect(readinessMessage('AUTHORIZATION_EXPIRED')).toContain('expired and was safely reconciled')
    expect(readinessMessage('PREPARATION_UNUSABLE')).toContain('can no longer be signed')
  })
  it('blocks only fresh confirmed blockers with enforcement on', () => {
    expect(readinessBlocker(sample(), 'open', 110_000)?.reason).toBe('KEEPER_INSUFFICIENT_FUNDS')
    expect(readinessBlocker(sample(), 'close', 110_000)).toBeUndefined()
    expect(readinessBlocker(sample(), 'open', 115_000)).toBeUndefined()
    expect(readinessBlocker({ ...sample(), enforcementEnabled: false }, 'open', 110_000)).toBeUndefined()
    expect(readinessChecks(undefined, 'close')[0].status).toBe('unknown')
  })
  it('does not let an open blocker disable deposits or invent exit permission', () => {
    expect(readinessChecks(sample(), 'deposit', 110_000)[0].status).toBe('ready')
    expect(readinessChecks(sample(), 'close', 110_000)[0].status).toBe('unknown')
  })
  it('rejects malformed, future-dated and overlong snapshots', () => {
    expect(parseReadiness(sample(), 110_000)).toEqual(sample())
    expect(() => parseReadiness({ ...sample(), expiresAt: 116_000 }, 110_000)).toThrow()
    expect(() => parseReadiness(sample(), 90_000)).toThrow()
    expect(() => parseReadiness({ ...sample(), actions: {} }, 110_000)).toThrow()
  })
  it('shows background-worker funding without making it an unrelated action blocker', () => {
    const v = { ...sample(), workers: [{ component: 'lp_settlement', status: 'blocked', reason: 'WORKER_INSUFFICIENT_FUNDS' }] }
    const parsed = parseReadiness(v,110_000)
    expect(parsed.workers?.[0].status).toBe('blocked')
    expect(readinessWorkers(parsed,115_000)).toEqual([])
    expect(readinessBlocker(parsed,'deposit',110_000)).toBeUndefined()
    expect(readinessBlocker(parsed,'close',110_000)).toBeUndefined()
    expect(() => parseReadiness({...v,workers:[...v.workers,...v.workers]},110_000)).toThrow()
    expect(() => parseReadiness({...v,workers:[{component:'address',status:'blocked',reason:'SECRET'}]},110_000)).toThrow()
  })
})
describe('deadline safety', () => {
  it('rounds fractional seconds conservatively and uses the earlier deadline', () => {
    expect(() => requireDeadlineHeadroom(120n, '200', 'signing', 100_001)).toThrow()
    expect(() => requireDeadlineHeadroom(200n, '120', 'signing', 100_001)).toThrow()
    expect(() => requireDeadlineHeadroom(110n, '200', 'submission', 100_001)).toThrow()
    expect(() => requireDeadlineHeadroom(200n, '110', 'submission', 100_001)).toThrow()
  })
  it('requires 20 seconds before signing and ten before submission', () => {
    expect(() => requireDeadlineHeadroom(150n, '120', 'signing', 100_000)).not.toThrow()
    expect(() => requireDeadlineHeadroom(150n, '119', 'signing', 100_000)).toThrow()
    expect(() => requireDeadlineHeadroom(110n, '150', 'submission', 100_000)).not.toThrow()
    expect(() => requireDeadlineHeadroom(109n, '150', 'submission', 100_000)).toThrow()
  })
  it('retains recovery for signed operations rather than declaring safe expiry', () => {
    try { requireDeadlineHeadroom(109n, undefined, 'submission', 100_000) } catch (error) {
      expect(error).toMatchObject({ reason: 'DEADLINE_TOO_CLOSE', terminalStatus: 'signed-not-submitted', retryable: false })
      return
    }
    throw new Error('Expected deadline rejection')
  })
})
describe('opaque diagnostic references', () => {
  it('accepts UUIDs but drops wallet-derived references and sensitive values', () => {
    const attempt_id = '12345678-1234-4123-8123-123456789abc'
    expect(sanitizeAnalyticsProperties({ attempt_id, stage: 'signing' })).toEqual({ attempt_id, stage: 'signing' })
    expect(sanitizeFrontendLogAttributes({ attempt_id, wallet_address: '0xdead', raw: 'secret' })).toEqual({ attempt_id })
    expect(sanitizeAnalyticsProperties({ attempt_id: '0xdead' })).toEqual({})
  })
})
