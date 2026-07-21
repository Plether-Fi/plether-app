import { describe, expect, it } from 'vitest'
import { classifyInsightsRoute } from './routes'

describe('Insights route analytics', () => {
  it.each([
    ['/', 'leaderboard'],
    ['/methodology', 'methodology'],
    ['/methodology/', 'methodology'],
    ['/competitions/testnet/wallets/0xabc', 'wallet'],
    ['/wallets/0xabc', null],
    ['/unknown', null],
  ])('classifies %s without returning route parameters', (pathname, expected) => {
    expect(classifyInsightsRoute(pathname)).toBe(expected)
  })
})
