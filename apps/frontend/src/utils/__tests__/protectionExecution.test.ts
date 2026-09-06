import { describe, expect, it } from 'vitest'
import type { PositionProtection } from '../../contracts/positionProtection'
import { currentProtectionObservation, parseProtectionExecutionReport } from '../protectionExecution'

const observation = {
  protectionId: '7', linkedOrderId: '19', account: '0x1111111111111111111111111111111111111111',
  protectionStatus: 8, reason: 'queue-congested', checkedBlock: '120', checkedBlockHash: '0x' + 'ab'.repeat(32),
  checkedAt: '2026-09-06T15:00:00Z', ageSeconds: 5, outcomeReason: 2,
}
const protection = { protectionId: 7n, linkedOrderId: 19n, account: observation.account, status: 8 } as PositionProtection
describe('protection execution reports', () => {
  it('accepts a missing observation without inventing worker activity', () => {
    expect(parseProtectionExecutionReport({ observation: null }).observation).toBeNull()
  })
  it('matches the exact account, protection, close attempt and contract state', () => {
    const report = parseProtectionExecutionReport({ observation })
    expect(currentProtectionObservation(report, protection, report.receivedAt)).toEqual(observation)
    for (const changes of [{ protectionId: 8n }, { linkedOrderId: 20n }, { status: 3 }, { account: '0x2222222222222222222222222222222222222222' as const }]) {
      expect(currentProtectionObservation(report, { ...protection, ...changes }, report.receivedAt)).toBeUndefined()
    }
  })
  it('expires server-aged reports even when the query is cached and the client clock differs', () => {
    const report = parseProtectionExecutionReport({ observation })
    expect(currentProtectionObservation(report, protection, report.receivedAt + 54_999)).toBeDefined()
    expect(currentProtectionObservation(report, protection, report.receivedAt + 55_000)).toBeUndefined()
    const stale = parseProtectionExecutionReport({ observation: { ...observation, ageSeconds: 61 } })
    expect(currentProtectionObservation(stale, protection, stale.receivedAt - 100_000)).toBeUndefined()
  })
  it.each([
    { reason: 'unexpected' }, { ageSeconds: -1 }, { ageSeconds: Infinity }, { protectionStatus: '8' },
    { checkedBlockHash: '0x123' }, { account: 'wrong' }, { protectionId: '07' }, { linkedOrderId: '-1' },
    { checkedAt: 'invalid' }, { transactionHash: 'javascript:alert(1)' }, { transactionAction: {} },
    { transactionHash: '0x' + 'ab'.repeat(32) },
  ])('rejects malformed execution data: %j', changes => {
    expect(() => parseProtectionExecutionReport({ observation: { ...observation, ...changes } })).toThrow('temporarily unavailable')
  })
  it('does not show trigger-monitoring copy on a latched record', () => {
    const report = parseProtectionExecutionReport({ observation: { ...observation, reason: 'monitoring' } })
    expect(currentProtectionObservation(report, protection, report.receivedAt)).toBeUndefined()
  })
})
