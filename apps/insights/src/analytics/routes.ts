import type { InsightsPage } from './insights'

export function classifyInsightsRoute(pathname: string): InsightsPage | null {
  if (pathname === '/') return 'leaderboard'
  if (pathname === '/methodology' || pathname === '/methodology/') return 'methodology'
  if (/^\/competitions\/[^/]+\/wallets\/[^/]+\/?$/.test(pathname)) return 'wallet'
  return null
}
