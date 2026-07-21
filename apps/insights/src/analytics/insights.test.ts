import { beforeEach, describe, expect, it, vi } from 'vitest'
import {
  trackInsightsPageViewed,
  trackLeaderboardPageRequested,
  trackLeaderboardSearchSubmitted,
  trackOutboundLinkOpened,
  trackWalletProfileOpened,
} from './insights'

const captureAnalyticsEvent = vi.hoisted(() => vi.fn())

vi.mock('./client', () => ({ captureAnalyticsEvent }))

describe('Insights analytics events', () => {
  beforeEach(() => {
    captureAnalyticsEvent.mockClear()
  })

  it('captures only coarse interaction metadata', () => {
    trackInsightsPageViewed('wallet')
    trackLeaderboardSearchSubmitted('wallet_address')
    trackLeaderboardPageRequested()
    trackWalletProfileOpened()
    trackOutboundLinkOpened()

    expect(captureAnalyticsEvent.mock.calls).toEqual([
      ['insights page viewed', { page: 'wallet', surface: 'insights' }],
      ['insights leaderboard searched', { search_kind: 'wallet_address', surface: 'insights' }],
      ['insights leaderboard page requested', { surface: 'insights' }],
      ['insights wallet profile opened', { source: 'leaderboard', surface: 'insights' }],
      ['insights outbound link opened', { destination: 'plether_app', surface: 'insights' }],
    ])
  })
})
