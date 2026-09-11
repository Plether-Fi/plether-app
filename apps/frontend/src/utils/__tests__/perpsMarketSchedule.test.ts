import { describe, expect, it } from 'vitest'
import { formatPerpsMarketDuration, getPerpsMarketSchedule } from '../perpsMarketSchedule'

describe('perps market schedule', () => {
  it('counts down to Friday 16:30 New York during daylight time', () => {
    expect(getPerpsMarketSchedule(new Date('2026-07-17T19:50:00Z'), 'open')).toEqual({
      currentDuration: '40m',
      nextPhase: 'close-only',
      nextDuration: '2d 0h 45m',
    })
  })

  it('counts down through the close-only FAD window', () => {
    expect(getPerpsMarketSchedule(new Date('2026-07-17T20:30:00Z'), 'close-only')).toEqual({
      currentDuration: '2d 0h 45m',
      nextPhase: 'open',
      nextDuration: '4d 23h 15m',
    })
  })

  it('starts the next open period at the deployed Sunday FAD end', () => {
    expect(getPerpsMarketSchedule(new Date('2026-07-19T21:15:00Z'), 'open')).toEqual({
      currentDuration: '4d 23h 15m',
      nextPhase: 'close-only',
      nextDuration: '2d 0h 45m',
    })
  })

  it('uses standard time for both winter boundaries', () => {
    expect(getPerpsMarketSchedule(new Date('2026-01-16T20:50:00Z'), 'open')).toEqual({
      currentDuration: '40m',
      nextPhase: 'close-only',
      nextDuration: '2d 0h 45m',
    })
    expect(getPerpsMarketSchedule(new Date('2026-01-18T22:00:00Z'), 'close-only').currentDuration)
      .toBe('15m')
    expect(getPerpsMarketSchedule(new Date('2026-01-18T22:15:00Z'), 'open').currentDuration)
      .toBe('4d 23h 15m')
  })

  it.each([
    ['2026-03-06T21:30:00Z', '1d 23h 45m'],
    ['2026-10-30T20:30:00Z', '2d 1h 45m'],
    ['2027-03-12T21:30:00Z', '1d 23h 45m'],
    ['2027-11-05T20:30:00Z', '2d 1h 45m'],
    ['2028-03-10T21:30:00Z', '1d 23h 45m'],
    ['2028-11-03T20:30:00Z', '2d 1h 45m'],
  ])('uses different Friday and Sunday offsets over the DST weekend %s', (timestamp, duration) => {
    const now = new Date(timestamp)
    expect(getPerpsMarketSchedule(now, 'close-only')).toEqual({
      currentDuration: duration,
      nextPhase: 'open',
      nextDuration: '4d 23h 15m',
    })
    expect(getPerpsMarketSchedule(new Date(now.getTime() - 60_000), 'open')).toEqual({
      currentDuration: '1m',
      nextPhase: 'close-only',
      nextDuration: duration,
    })
  })

  it.each([
    ['2026-03-08T21:15:00Z'],
    ['2026-11-01T22:15:00Z'],
    ['2027-01-03T22:15:00Z'],
  ])('starts a new open interval at %s', (timestamp) => {
    expect(getPerpsMarketSchedule(new Date(timestamp), 'open')).toEqual({
      currentDuration: '4d 23h 15m',
      nextPhase: 'close-only',
      nextDuration: '2d 0h 45m',
    })
  })

  it.each([
    ['2026-07-17T20:30:00Z', 'open'],
    ['2026-07-18T12:00:00Z', 'open'],
    ['2026-07-19T21:15:00Z', 'close-only'],
    ['2026-07-20T12:00:00Z', 'close-only'],
    ['2026-07-20T12:00:00Z', 'closed'],
    ['2026-07-18T12:00:00Z', 'closed'],
    ['2026-07-18T12:00:00Z', 'degraded'],
    ['2026-07-20T12:00:00Z', 'paused'],
  ] as const)('does not invent a countdown for %s in state %s', (timestamp, phase) => {
    expect(getPerpsMarketSchedule(new Date(timestamp), phase)).toMatchObject({
      currentDuration: undefined,
      nextDuration: undefined,
    })
  })

  it('shows less than one minute immediately before each transition', () => {
    expect(getPerpsMarketSchedule(new Date('2026-07-17T20:29:59Z'), 'open').currentDuration)
      .toBe('<1m')
    expect(getPerpsMarketSchedule(new Date('2026-07-19T21:14:59Z'), 'close-only').currentDuration)
      .toBe('<1m')
  })

  it('handles the same instant in a different caller timezone', () => {
    expect(getPerpsMarketSchedule(new Date('2026-07-17T22:00:00+02:00'), 'open').currentDuration)
      .toBe('30m')
  })

  it('does not show a countdown for an invalid date', () => {
    expect(getPerpsMarketSchedule(new Date(NaN), 'open').currentDuration).toBeUndefined()
  })

  it('formats remaining time without rounding past the deadline', () => {
    expect(formatPerpsMarketDuration(0)).toBe('0m')
    expect(formatPerpsMarketDuration(-1)).toBe('0m')
    expect(formatPerpsMarketDuration(59_999)).toBe('<1m')
    expect(formatPerpsMarketDuration(119_999)).toBe('1m')
  })
})
