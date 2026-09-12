import { describe, expect, it } from 'vitest'
import { parseReadiness, readinessBlocker, readinessChecks, readinessWorkers, type ReadinessSnapshot } from '../readiness'
import { requireDeadlineHeadroom } from '../deadline'
import { sanitizeAnalyticsProperties, sanitizeFrontendLogAttributes } from '../../analytics/client'

const sample = (): ReadinessSnapshot => ({ version: 1, observedAt: 100_000, expiresAt: 115_000, enforcementEnabled: true,
  actions: { deposit: [{ component: 'sponsorship', status: 'ready', reason: 'READY' }], open: [{ component: 'keeper', status: 'blocked', reason: 'KEEPER_INSUFFICIENT_FUNDS' }], close: [{ component: 'keeper', status: 'unknown', reason: 'WORKER_HEARTBEAT_STALE' }], protection: [{ component: 'oracle', status: 'unknown', reason: 'ORACLE_UNAVAILABLE' }] } })
describe('trading readiness', () => {
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
  it('requires 20 seconds before signing and ten before submission', () => {
    expect(() => requireDeadlineHeadroom(150n, '120', 'signing', 100_000)).not.toThrow()
    expect(() => requireDeadlineHeadroom(150n, '119', 'signing', 100_000)).toThrow()
    expect(() => requireDeadlineHeadroom(110n, '150', 'submission', 100_000)).not.toThrow()
    expect(() => requireDeadlineHeadroom(109n, '150', 'submission', 100_000)).toThrow()
  })
  it('retains recovery for signed operations rather than declaring safe expiry', () => {
    try { requireDeadlineHeadroom(109n, undefined, 'submission', 100_000) } catch (error) {
      expect(error).toMatchObject({ reason: 'DEADLINE_TOO_CLOSE', terminalStatus: 'receipt-timeout', retryable: false })
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
