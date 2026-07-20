import { describe, expect, it } from 'vitest'
import { getPerpsMarketSchedule } from '../perpsMarketSchedule'

describe('perps market schedule', () => {
  it('counts down to the deployed Friday FAD start', () => {
    expect(getPerpsMarketSchedule(new Date('2026-07-17T20:50:00Z'), 'open')).toEqual({
      currentDuration: '40m',
      nextPhase: 'close-only',
      nextDuration: '1d 23h 45m',
    })
  })

  it('counts down through the close-only FAD window', () => {
    expect(getPerpsMarketSchedule(new Date('2026-07-17T21:30:00Z'), 'close-only')).toEqual({
      currentDuration: '1d 23h 45m',
      nextPhase: 'open',
      nextDuration: '5d 0h 15m',
    })
  })

  it('starts the next open period at the deployed Sunday FAD end', () => {
    expect(getPerpsMarketSchedule(new Date('2026-07-19T21:15:00Z'), 'open')).toEqual({
      currentDuration: '5d 0h 15m',
      nextPhase: 'close-only',
      nextDuration: '1d 23h 45m',
    })
  })
})
