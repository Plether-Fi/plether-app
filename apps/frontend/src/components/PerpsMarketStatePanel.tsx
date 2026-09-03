import { useEffect, useMemo, useState } from 'react'
import {
  getPerpsMarketSchedule,
  type PerpsMarketPhase,
} from '../utils/perpsMarketSchedule'

export interface PerpsMarketStatePanelProps {
  currentPhase?: PerpsMarketPhase
  currentDuration?: string
  nextPhase?: PerpsMarketPhase
  nextDuration?: string
  now?: Date
}

function phaseToneClass(phase: PerpsMarketPhase): string {
  switch (phase) {
    case 'open':
      return 'text-positive'
    case 'close-only':
    case 'degraded':
      return 'text-brand-peach'
    case 'closed':
    case 'paused':
      return 'text-brand-orange'
  }
}

function PhaseText({ phase }: { phase: PerpsMarketPhase }) {
  return <span className={`font-medium ${phaseToneClass(phase)}`}>{phase}</span>
}

export function PerpsMarketStatePanel({
  currentPhase = 'open',
  currentDuration,
  nextPhase,
  nextDuration,
  now,
}: PerpsMarketStatePanelProps) {
  const [liveClock, setLiveClock] = useState(() => new Date())

  useEffect(() => {
    if (now) return

    const interval = window.setInterval(() => {
      setLiveClock(new Date())
    }, 30_000)

    return () => {
      window.clearInterval(interval)
    }
  }, [now])

  const clock = now ?? liveClock
  const schedule = useMemo(() => getPerpsMarketSchedule(clock, currentPhase), [clock, currentPhase])
  const displayedCurrentDuration = currentDuration ?? schedule.currentDuration
  const displayedNextPhase = nextPhase ?? schedule.nextPhase
  const displayedNextDuration = nextDuration ?? schedule.nextDuration

  return (
    <section className="border border-brand-border/30 bg-app-bg px-3 py-2 sm:px-4 sm:py-3">
      <div className="flex items-start gap-2 sm:gap-3">
        <span className={`mt-1 h-2 w-2 shrink-0 rounded-full bg-current sm:mt-1.5 ${phaseToneClass(currentPhase)}`} />
        <p className="text-[11px] leading-4 text-content-secondary sm:text-xs sm:leading-5">
          <span>Market is </span>
          <PhaseText phase={currentPhase} />
          {displayedCurrentDuration ? (
            <>
              <span> for another </span>
              <span className="font-medium text-content-primary">{displayedCurrentDuration}</span>
            </>
          ) : null}
          {displayedNextDuration ? (
            <>
              <span>. Then </span>
              <PhaseText phase={displayedNextPhase} />
              <span> for </span>
              <span className="font-medium text-content-primary">{displayedNextDuration}</span>
            </>
          ) : null}
          <span>.</span>
        </p>
      </div>
    </section>
  )
}
