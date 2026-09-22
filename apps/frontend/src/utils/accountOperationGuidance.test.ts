import { describe, expect, it } from 'vitest'
import { accountOperationGuidance } from './accountOperationGuidance'
import type { SponsoredOperation } from '../perps-aa/operationStore'

const now = 2_000_000_000_000
const operation = { status: 'preparation-pending', reason: 'ACCOUNT_DEPLOYMENT_PENDING', createdAt: now - 20 * 60_000,
  statusTimestamps: {}, orderRequestV3: { submitBy: String(now / 1000 + 60) },
} as SponsoredOperation
describe('account operation guidance', () => {
  it('uses live confirmation despite the saved historical error', () => {
    expect(accountOperationGuidance(operation, now, 'ready').action).toBe('Resume order')
    expect(accountOperationGuidance(operation, now, 'check-unavailable').title).toContain('Unable to check')
  })
  it('requires fresh review at the signing threshold instead of resuming', () => {
    expect(accountOperationGuidance(operation, now + 41_000, 'ready').action).toBe('Review order again')
    expect(accountOperationGuidance({ ...operation, reason: 'INVALID_ORDER_DEADLINE' }, now, 'ready').action).toBe('Review order again')
  })
  it('never offers fresh order review for an uncertain signed submission', () => {
    expect(accountOperationGuidance({ ...operation, userOperationHash: '0xab', status: 'outcome-unknown' }, now + 100_000, 'ready').action).toBe('Check transaction status')
  })
  it('uses submission progress after a successful resume instead of the historical confirmation error', () => {
    expect(accountOperationGuidance({ ...operation, status: 'confirming', userOperationHash: '0xab',
      statusTimestamps: { confirming: now } }, now, 'ready').title).toBe('Waiting for transaction confirmation')
    expect(accountOperationGuidance({ ...operation, status: 'requesting-sponsorship',
      statusTimestamps: { 'requesting-sponsorship': now } }, now, 'ready').action).toBe('View pending transaction')
  })
})
