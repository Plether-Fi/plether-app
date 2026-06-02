export type PerpsMarketPhase = 'open' | 'close-only' | 'closed' | 'degraded' | 'paused'

export interface PerpsMarketStatePanelProps {
  currentPhase?: PerpsMarketPhase
  currentDuration?: string
  nextPhase?: PerpsMarketPhase
  nextDuration?: string
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
  currentDuration = '1d 12h 35m',
  nextPhase = 'close-only',
  nextDuration = '3h',
}: PerpsMarketStatePanelProps) {
  return (
    <section className="border border-cyber-border-glow/20 bg-cyber-bg/45 px-4 py-3">
      <div className="flex items-start gap-3">
        <span className={`mt-1.5 h-2 w-2 shrink-0 rounded-full bg-current ${phaseToneClass(currentPhase)}`} />
        <p className="text-xs leading-5 text-cyber-text-secondary">
          <span>Market is </span>
          <PhaseText phase={currentPhase} />
          <span> for another </span>
          <span className="font-medium text-cyber-text-primary">{currentDuration}</span>
          <span>. Then </span>
          <PhaseText phase={nextPhase} />
          <span> for </span>
          <span className="font-medium text-cyber-text-primary">{nextDuration}</span>
          <span>.</span>
        </p>
      </div>
    </section>
  )
}
