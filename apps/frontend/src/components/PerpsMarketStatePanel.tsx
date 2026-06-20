import { useEffect, useMemo, useState } from 'react'

export type PerpsMarketPhase = 'open' | 'close-only' | 'closed' | 'degraded' | 'paused'

export interface PerpsMarketStatePanelProps {
  currentPhase?: PerpsMarketPhase
  currentDuration?: string
  nextPhase?: PerpsMarketPhase
  nextDuration?: string
  now?: Date
}

const FRIDAY = 5
const SUNDAY = 0

function phaseToneClass(phase: PerpsMarketPhase): string {
  switch (phase) {
    case 'open':
      return 'text-cyber-neon-green'
    case 'close-only':
    case 'degraded':
      return 'text-yellow-300'
    case 'closed':
    case 'paused':
      return 'text-cyber-electric-fuchsia'
  }
}

function PhaseText({ phase }: { phase: PerpsMarketPhase }) {
  return <span className={`font-medium ${phaseToneClass(phase)}`}>{phase}</span>
}

function formatDuration(ms: number): string {
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

function utcBoundary(date: Date, targetDay: number, hour: number): Date {
  const boundary = new Date(Date.UTC(
    date.getUTCFullYear(),
    date.getUTCMonth(),
    date.getUTCDate(),
    hour,
    0,
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

function previousUtcBoundary(date: Date, targetDay: number, hour: number): Date {
  const boundary = utcBoundary(date, targetDay, hour)
  boundary.setUTCDate(boundary.getUTCDate() - 7)
  return boundary
}

function weeklySchedule(now: Date, currentPhase: PerpsMarketPhase) {
  if (currentPhase === 'open') {
    const endsAt = utcBoundary(now, FRIDAY, 19)
    return {
      currentDuration: formatDuration(endsAt.getTime() - now.getTime()),
      nextPhase: 'close-only' as const,
      nextDuration: formatDuration(utcBoundary(endsAt, SUNDAY, 22).getTime() - endsAt.getTime()),
    }
  }

  if (currentPhase === 'close-only') {
    const endsAt = utcBoundary(now, SUNDAY, 22)
    return {
      currentDuration: formatDuration(endsAt.getTime() - now.getTime()),
      nextPhase: 'open' as const,
      nextDuration: formatDuration(utcBoundary(endsAt, FRIDAY, 19).getTime() - endsAt.getTime()),
    }
  }

  if (currentPhase === 'closed') {
    const endsAt = utcBoundary(now, SUNDAY, 22)
    return {
      currentDuration: formatDuration(endsAt.getTime() - now.getTime()),
      nextPhase: 'open' as const,
      nextDuration: formatDuration(utcBoundary(endsAt, FRIDAY, 19).getTime() - endsAt.getTime()),
    }
  }

  return {
    currentDuration: undefined,
    nextPhase: 'open' as const,
    nextDuration: formatDuration(utcBoundary(now, FRIDAY, 19).getTime() - previousUtcBoundary(now, SUNDAY, 22).getTime()),
  }
}

export function PerpsMarketStatePanel({
  currentPhase = 'open',
  currentDuration,
  nextPhase,
  nextDuration,
  now,
}: PerpsMarketStatePanelProps) {
  const [clock, setClock] = useState(() => new Date())

  useEffect(() => {
    if (now) return undefined

    const interval = window.setInterval(() => {
      setClock(new Date())
    }, 30_000)

    return () => {
      window.clearInterval(interval)
    }
  }, [now])

  const currentClock = now ?? clock
  const schedule = useMemo(() => weeklySchedule(currentClock, currentPhase), [currentClock, currentPhase])
  const displayedCurrentDuration = currentDuration ?? schedule.currentDuration
  const displayedNextPhase = nextPhase ?? schedule.nextPhase
  const displayedNextDuration = nextDuration ?? schedule.nextDuration

  return (
    <section className="border border-cyber-border-glow/20 bg-cyber-bg px-4 py-3">
      <div className="flex items-start gap-3">
        <span className={`mt-1.5 h-2 w-2 shrink-0 rounded-full bg-current ${phaseToneClass(currentPhase)}`} />
        <p className="text-xs leading-5 text-cyber-text-secondary">
          <span>Market is </span>
          <PhaseText phase={currentPhase} />
          {displayedCurrentDuration ? (
            <>
              <span> for another </span>
              <span className="font-medium text-cyber-text-primary">{displayedCurrentDuration}</span>
            </>
          ) : null}
          {displayedNextDuration ? (
            <>
              <span>. Then </span>
              <PhaseText phase={displayedNextPhase} />
              <span> for </span>
              <span className="font-medium text-cyber-text-primary">{displayedNextDuration}</span>
            </>
          ) : null}
          <span>.</span>
        </p>
      </div>
    </section>
  )
}
