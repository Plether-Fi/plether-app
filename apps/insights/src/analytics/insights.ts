import { captureAnalyticsEvent } from './client'

export type InsightsPage = 'leaderboard' | 'methodology' | 'wallet'
export type SearchKind = 'alias' | 'empty' | 'wallet_address'

const BASE_PROPERTIES = {
  surface: 'insights',
} as const

export function trackInsightsPageViewed(page: InsightsPage): void {
  captureAnalyticsEvent('insights page viewed', {
    ...BASE_PROPERTIES,
    page,
  })
}

export function trackLeaderboardSearchSubmitted(searchKind: SearchKind): void {
  captureAnalyticsEvent('insights leaderboard searched', {
    ...BASE_PROPERTIES,
    search_kind: searchKind,
  })
}

export function trackLeaderboardPageRequested(): void {
  captureAnalyticsEvent('insights leaderboard page requested', BASE_PROPERTIES)
}

export function trackWalletProfileOpened(): void {
  captureAnalyticsEvent('insights wallet profile opened', {
    ...BASE_PROPERTIES,
    source: 'leaderboard',
  })
}

export function trackOutboundLinkOpened(): void {
  captureAnalyticsEvent('insights outbound link opened', {
    ...BASE_PROPERTIES,
    destination: 'plether_app',
  })
}
