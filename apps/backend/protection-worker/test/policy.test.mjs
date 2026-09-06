import { describe, it } from 'node:test'
import assert from 'node:assert/strict'
import { admittedPayload, retryDecision, triggerLeg } from '../policy.mjs'

describe('protection trigger and retry policy', () => {
  const long = { status: 2, side: 0, armedAt: 100n, armedBlock: 10n, takeProfitTriggerPrice: 68_000_000n, stopLossTriggerPrice: 92_000_000n }
  it('triggers raw-price OCO in both directions, including exact thresholds', () => {
    assert.equal(triggerLeg(long, 68_000_000n, 101n, 11n), 1)
    assert.equal(triggerLeg(long, 92_000_000n, 101n, 11n), 2)
    assert.equal(triggerLeg(long, 80_000_000n, 101n, 11n), 0)
    const short = { ...long, side: 1, takeProfitTriggerPrice: 92_000_000n, stopLossTriggerPrice: 68_000_000n }
    assert.equal(triggerLeg(short, 92_000_000n, 101n, 11n), 1)
    assert.equal(triggerLeg(short, 68_000_000n, 101n, 11n), 2)
  })
  it('never triggers from an arming tick, disabled leg or terminal record', () => {
    assert.equal(triggerLeg(long, 68_000_000n, 100n, 11n), 0)
    assert.equal(triggerLeg(long, 68_000_000n, 101n, 10n), 0)
    assert.equal(triggerLeg({ ...long, takeProfitTriggerPrice: 0n }, 68_000_000n, 101n, 11n), 0)
    for (const status of [0, 1, 3, 4, 5, 6, 7, 8]) assert.equal(triggerLeg({ ...long, status }, 68_000_000n, 101n, 11n), 0)
  })
  const retry = { protection: { status: 8 }, outcome: { status: 3, reason: 2 }, pendingCount: 0n, oracleAvailable: true, queueSize: 45n, maxOrderAge: 60n, keeperBatchSize: 5, keeperPollSeconds: 5 }
  it('retries only expiry within a conservatively projected FIFO window', () => {
    assert.equal(retryDecision(retry), 'retry')
    assert.equal(retryDecision({ ...retry, queueSize: 46n }), 'queue-congested')
    assert.equal(retryDecision({ ...retry, pendingCount: 1n }), 'pending-orders')
    assert.equal(retryDecision({ ...retry, oracleAvailable: false }), 'oracle-unavailable')
    for (const reason of [0, 1, 3, 4, 5, 6, 7, 8, 9]) assert.equal(retryDecision({ ...retry, outcome: { status: 3, reason } }), 'operator-required')
  })
  it('accepts only complete, fresh, admitted current payloads', () => {
    const row = { source: 'backend_hermes_latest_v2', min_publish_time: '990', max_publish_time: '991', update_data: ['0x1234'] }
    assert.equal(admittedPayload(row, 1000, 15), true)
    for (const change of [{ source: 'backend_hermes_latest' }, { min_publish_time: '980' }, { max_publish_time: '1001' }, { update_data: [] }, { update_data: ['0x123'] }]) assert.equal(admittedPayload({ ...row, ...change }, 1000, 15), false)
  })
})
