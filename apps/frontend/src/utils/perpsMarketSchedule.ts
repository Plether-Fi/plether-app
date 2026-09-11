export type PerpsMarketPhase = 'open' | 'close-only' | 'closed' | 'degraded' | 'paused'

const FRIDAY = 5
const DAY_MS = 86_400_000
// Match MarketCalendarLib: Friday 16:30 and Sunday 17:15 New York time,
// using the post-2007 US DST rules independently at each boundary.
const FAD_LEAD_MS = 30 * 60_000
const FAD_LAG_MS = 15 * 60_000

export function formatPerpsMarketDuration(ms: number): string {
  if (ms > 0 && ms < 60_000) return '<1m'
  const totalMinutes = Math.max(0, Math.floor(ms / 60_000))
  const days = Math.floor(totalMinutes / (24 * 60))
  const hours = Math.floor((totalMinutes % (24 * 60)) / 60)
  const minutes = totalMinutes % 60
  const parts: string[] = []

  if (days > 0) parts.push(`${days.toString()}d`)
  if (hours > 0 || days > 0) parts.push(`${hours.toString()}h`)
  if (minutes > 0 || parts.length === 0) parts.push(`${minutes.toString()}m`)

  return parts.join(' ')
}

function newYorkMarketBoundary(utcDay: number): number {
  const year = new Date(utcDay).getUTCFullYear()
  const secondSundayInMarch = 8 + (7 - new Date(Date.UTC(year, 2, 1)).getUTCDay()) % 7
  const firstSundayInNovember = 1 + (7 - new Date(Date.UTC(year, 10, 1)).getUTCDay()) % 7
  const dstStart = Date.UTC(year, 2, secondSundayInMarch, 7)
  const dstEnd = Date.UTC(year, 10, firstSundayInNovember, 6)
  const daylightBoundary = utcDay + 21 * 60 * 60_000
  const isDaylightTime = daylightBoundary >= dstStart && daylightBoundary < dstEnd
  return utcDay + (isDaylightTime ? 21 : 22) * 60 * 60_000
}

export function getPerpsMarketSchedule(now: Date, currentPhase: PerpsMarketPhase) {
  const timestamp = now.getTime()
  const today = Date.UTC(now.getUTCFullYear(), now.getUTCMonth(), now.getUTCDate())
  let friday = today - ((now.getUTCDay() - FRIDAY + 7) % 7) * DAY_MS
  if (timestamp < newYorkMarketBoundary(friday) - FAD_LEAD_MS) friday -= 7 * DAY_MS

  const reopensAt = newYorkMarketBoundary(friday + 2 * DAY_MS) + FAD_LAG_MS
  const closesAt = newYorkMarketBoundary(friday + 7 * DAY_MS) - FAD_LEAD_MS
  const scheduledPhase = timestamp < reopensAt ? 'close-only' : 'open'
  const nextPhase = scheduledPhase === 'open' ? 'close-only' as const : 'open' as const

  // Onchain state is authoritative. Overrides, recovery, and delayed polling
  // have no predictable weekly countdown; never roll a stale phase forward a week.
  if (currentPhase !== scheduledPhase || !Number.isFinite(timestamp)) {
    return { currentDuration: undefined, nextPhase, nextDuration: undefined }
  }

  const endsAt = scheduledPhase === 'close-only' ? reopensAt : closesAt
  const nextEndsAt = scheduledPhase === 'close-only'
    ? closesAt
    : newYorkMarketBoundary(friday + 9 * DAY_MS) + FAD_LAG_MS
  return {
    currentDuration: formatPerpsMarketDuration(endsAt - timestamp),
    nextPhase,
    nextDuration: formatPerpsMarketDuration(nextEndsAt - endsAt),
  }
}
