import { describe, expect, it } from 'vitest'
import { sponsoredOperationStatusLabel } from '../../utils/sponsoredOperation'

describe('sponsoredOperationStatusLabel', () => {
  it('groups preparation phases into one user-facing state', () => {
    expect(sponsoredOperationStatusLabel('building')).toBe(
      'Preparing sponsored transaction'
    )
    expect(sponsoredOperationStatusLabel('requesting-sponsorship')).toBe(
      'Preparing sponsored transaction'
    )
  })

  it('does not label a reverted receipt as confirmed', () => {
    expect(sponsoredOperationStatusLabel('execution-reverted')).toBe(
      'Failed onchain'
    )
  })
})
