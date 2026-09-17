import { describe, expect, it } from 'vitest'
import { sponsoredOperationDisplayStatus, sponsoredOperationStatusLabel } from '../../utils/sponsoredOperation'
import type { SponsoredOperation } from '../../perps-aa/operationStore'

describe('sponsoredOperationStatusLabel', () => {
  it.each(['UNKNOWN', 'PREPARATION_UNUSABLE', 'SPONSOR_UNAVAILABLE', undefined])('shows recovery for a historical native refusal with reason %s', reason => {
    const saved = { status: 'sponsorship-refused', reason, nativePreparation: { version: 1 } } as SponsoredOperation
    expect(sponsoredOperationStatusLabel(sponsoredOperationDisplayStatus(saved))).toBe('Preparation recovery')
    expect(saved.status).toBe('sponsorship-refused')
  })
  it('preserves an explicit sponsorship denial', () => {
    const saved = { status: 'sponsorship-refused', reason: 'SPONSOR_BUDGET_EXCEEDED', nativePreparation: { version: 1 } } as SponsoredOperation
    expect(sponsoredOperationDisplayStatus(saved)).toBe('sponsorship-refused')
  })
  it('groups preparation phases into one user-facing state', () => {
    expect(sponsoredOperationStatusLabel('building')).toBe(
      'Preparing sponsored transaction'
    )
    expect(sponsoredOperationStatusLabel('requesting-sponsorship')).toBe(
      'Preparing sponsored transaction'
    )
    expect(sponsoredOperationStatusLabel('journaling')).toBe(
      'Saving recovery record'
    )
  })

  it('does not label a reverted receipt as confirmed', () => {
    expect(sponsoredOperationStatusLabel('execution-reverted')).toBe(
      'Failed onchain'
    )
  })
})
