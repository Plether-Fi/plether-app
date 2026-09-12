import { describe, expect, it, vi } from 'vitest'
import { createReadinessReporter } from './readiness'
import { sanitizeAnalyticsProperties } from './client'

const funding = { component: 'keeper', status: 'blocked', reason: 'KEEPER_INSUFFICIENT_FUNDS' }
const oracle = { component: 'oracle', status: 'unknown', reason: 'ORACLE_UNAVAILABLE' }
const healthy = { component: 'keeper', status: 'ready', reason: 'READY' }

describe('readiness incident telemetry', () => {
  it('captures distinct failures immediately while deduplicating shared action evidence', () => {
    const emit = vi.fn()
    const report = createReadinessReporter(emit, () => 0)
    report([funding, funding, oracle, oracle])
    expect(emit).toHaveBeenCalledTimes(2)
    expect(emit).toHaveBeenCalledWith('started', expect.objectContaining({ reason_code: funding.reason, outcome: 'blocked', occurrence_count: 1 }))
    expect(emit).toHaveBeenCalledWith('started', expect.objectContaining({ reason_code: oracle.reason, outcome: 'unknown', occurrence_count: 1 }))
  })
  it('summarizes recurring observations once per minute and flushes the remaining count on recovery', () => {
    let now = 0
    const emit = vi.fn()
    const report = createReadinessReporter(emit, () => now)
    report([funding])
    for (now = 10_000; now <= 60_000; now += 10_000) report([funding])
    expect(emit).toHaveBeenCalledTimes(2)
    expect(emit).toHaveBeenLastCalledWith('summary', expect.objectContaining({ occurrence_count: 6, duration_ms: 60_000 }))
    report([funding])
    now = 80_000
    report([healthy])
    expect(emit).toHaveBeenLastCalledWith('resolved', expect.objectContaining({ occurrence_count: 1, duration_ms: 80_000, outcome: 'ready' }))
    report([healthy])
    expect(emit).toHaveBeenCalledTimes(3)
  })
  it('records a new incident after recovery and treats low reserves as a warning rather than a blocker', () => {
    const emit = vi.fn()
    const report = createReadinessReporter(emit)
    report([funding]); report([healthy]); report([funding])
    expect(emit.mock.calls.filter(([phase]) => phase === 'started')).toHaveLength(2)
    report([{ component: 'funding', status: 'ready', reason: 'FUNDING_LOW' }])
    expect(emit).toHaveBeenLastCalledWith('started', expect.objectContaining({ outcome: 'ready', reason_code: 'FUNDING_LOW' }))
  })
  it('projects unknown labels to fixed categories, dropping nested secrets and arbitrary values', () => {
    const emit = vi.fn()
    const report = createReadinessReporter(emit)
    report([{ component: 'SECRET_COMPONENT', status: 'SECRET_STATUS', reason: 'SECRET_PROVIDER_CREDENTIAL' }])
    const properties = emit.mock.calls[0][1] as Record<string, string | number>
    expect(JSON.stringify(properties)).not.toContain('SECRET')
    expect(sanitizeAnalyticsProperties(properties)).toEqual(properties)
  })
  it('never throws exporter errors into readiness or authorization', () => {
    const report = createReadinessReporter(() => { throw new Error('offline') })
    expect(() => report([funding])).not.toThrow()
    expect(() => report([healthy])).not.toThrow()
  })
  it('does not report recovery when the next snapshot is unknown or missing evidence', () => {
    const emit = vi.fn()
    const report = createReadinessReporter(emit)
    report([funding])
    report([{ component: 'readiness', status: 'unknown', reason: 'READINESS_UNAVAILABLE' }])
    report([])
    report([{ component: 'keeper', status: 'unknown', reason: 'FUNDING_UNVERIFIED' }])
    expect(emit.mock.calls.filter(([phase]) => phase === 'resolved')).toHaveLength(1)
    expect(emit.mock.calls.find(([phase]) => phase === 'resolved')?.[1]).toMatchObject({ component: 'readiness' })
    report([healthy])
    expect(emit.mock.calls.filter(([phase, properties]) => phase === 'resolved' && properties.component === 'keeper')).toHaveLength(2)
  })
})
