export type PerpsMarketPhase = 'open' | 'close-only' | 'closed' | 'degraded' | 'paused'

const FRIDAY = 5
const SUNDAY = 0
// Keep these UTC boundaries aligned with plether-core's MarketCalendarLib.
const FRIDAY_FAD_START = { hour: 21, minute: 30 } as const
const SUNDAY_FAD_END = { hour: 21, minute: 15 } as const

export function formatPerpsMarketDuration(ms: number): string {
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

function utcBoundary(date: Date, targetDay: number, hour: number, minute: number): Date {
  const boundary = new Date(Date.UTC(
    date.getUTCFullYear(),
    date.getUTCMonth(),
    date.getUTCDate(),
    hour,
    minute,
    0,
    0
  ))
  const daysUntilTarget = (targetDay - date.getUTCDay() + 7) % 7
  boundary.setUTCDate(boundary.getUTCDate() + daysUntilTarget)

  if (boundary.getTime() <= date.getTime()) {
    boundary.setUTCDate(boundary.getUTCDate() + 7)
  }

  return boundary
}

function previousUtcBoundary(date: Date, targetDay: number, hour: number, minute: number): Date {
  const boundary = utcBoundary(date, targetDay, hour, minute)
  boundary.setUTCDate(boundary.getUTCDate() - 7)
  return boundary
}

export function getPerpsMarketSchedule(now: Date, currentPhase: PerpsMarketPhase) {
  if (currentPhase === 'open') {
    const endsAt = utcBoundary(now, FRIDAY, FRIDAY_FAD_START.hour, FRIDAY_FAD_START.minute)
    return {
      currentDuration: formatPerpsMarketDuration(endsAt.getTime() - now.getTime()),
      nextPhase: 'close-only' as const,
      nextDuration: formatPerpsMarketDuration(
        utcBoundary(endsAt, SUNDAY, SUNDAY_FAD_END.hour, SUNDAY_FAD_END.minute).getTime()
          - endsAt.getTime()
      ),
    }
  }

  if (currentPhase === 'close-only') {
    const endsAt = utcBoundary(now, SUNDAY, SUNDAY_FAD_END.hour, SUNDAY_FAD_END.minute)
    return {
      currentDuration: formatPerpsMarketDuration(endsAt.getTime() - now.getTime()),
      nextPhase: 'open' as const,
      nextDuration: formatPerpsMarketDuration(
        utcBoundary(endsAt, FRIDAY, FRIDAY_FAD_START.hour, FRIDAY_FAD_START.minute).getTime()
          - endsAt.getTime()
      ),
    }
  }

  if (currentPhase === 'closed') {
    const endsAt = utcBoundary(now, SUNDAY, SUNDAY_FAD_END.hour, SUNDAY_FAD_END.minute)
    return {
      currentDuration: formatPerpsMarketDuration(endsAt.getTime() - now.getTime()),
      nextPhase: 'open' as const,
      nextDuration: formatPerpsMarketDuration(
        utcBoundary(endsAt, FRIDAY, FRIDAY_FAD_START.hour, FRIDAY_FAD_START.minute).getTime()
          - endsAt.getTime()
      ),
    }
  }

  return {
    currentDuration: undefined,
    nextPhase: 'open' as const,
    nextDuration: formatPerpsMarketDuration(
      utcBoundary(now, FRIDAY, FRIDAY_FAD_START.hour, FRIDAY_FAD_START.minute).getTime()
        - previousUtcBoundary(now, SUNDAY, SUNDAY_FAD_END.hour, SUNDAY_FAD_END.minute).getTime()
    ),
  }
}
