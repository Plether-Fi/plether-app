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
