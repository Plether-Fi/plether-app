export const TIERS = ['Iron', 'Bronze', 'Silver', 'Gold', 'Platinum', 'Diamond'] as const
export type Tier = typeof TIERS[number]

export const TIER_REQUIREMENTS: Record<Tier, string> = {
  Iron: 'Trade in a completed week',
  Bronze: 'Finish in profit',
  Silver: 'Top 50% of profitable traders',
  Gold: 'Top 20% of profitable traders',
  Platinum: 'Top 10% of profitable traders',
  Diamond: 'Top 5% of profitable traders',
}

export const DIVISIONS_START = '2026-10-12T00:00:00Z'
export const FIRST_WEEK_START = '2026-10-11T21:00:00Z'
export const FIRST_PUBLICATION = '2026-10-19T00:00:00Z'
const DAY_MS = 86_400_000

export function tradingWeek(now: number): { startsAt: string; closesAt: string; publishesAt: string } {
  const first = Date.parse(FIRST_WEEK_START)
  const start = first + Math.max(0, Math.floor((now - first) / (7 * DAY_MS))) * 7 * DAY_MS
  return {
    startsAt: new Date(start).toISOString(),
    closesAt: new Date(start + 5 * DAY_MS).toISOString(),
    publishesAt: new Date(start + 7 * DAY_MS + 3 * 3_600_000).toISOString(),
  }
}

// Executions belong to Monday–Friday FX sessions, starting at 21:00 UTC
// on the previous calendar day. Weekend executions never create active days.
export function activeSessionDay(timestamp: string, startsAt: string, closesAt: string): string | null {
  const time = Date.parse(timestamp)
  if (!Number.isFinite(time) || time < Date.parse(startsAt) || time >= Date.parse(closesAt)) return null
  const shifted = new Date(time + 3 * 3_600_000)
  if (shifted.getUTCDay() === 0 || shifted.getUTCDay() === 6) return null
  return shifted.toISOString().slice(0, 10)
}
