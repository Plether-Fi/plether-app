import { memo } from 'react'
import { Confetti } from '@neoconfetti/react'

const CONFETTI_COLORS = [
  'var(--color-brand-peach)',
  'var(--color-brand-orange)',
  'var(--color-brand-yellow)',
  'var(--color-positive)',
  'var(--color-content-primary)',
] satisfies string[]

const ConfettiCannon = memo(function ConfettiCannon({
  particleCount,
  force,
  stageWidth,
}: {
  particleCount: number
  force: number
  stageWidth: number
}) {
  return (
    <Confetti
      colors={CONFETTI_COLORS}
      destroyAfterDone
      duration={2_900}
      force={force}
      particleCount={particleCount}
      particleShape="rectangles"
      particleSize={7}
      stageHeight={230}
      stageWidth={stageWidth}
    />
  )
})

export function PerpsFinalizationConfetti() {
  return (
    <div
      className="perps-confetti-burst pointer-events-none absolute inset-0 z-0 overflow-hidden"
      data-finalization-confetti
      aria-hidden="true"
    >
      <div className="perps-confetti-cannon perps-confetti-cannon-left">
        <ConfettiCannon particleCount={34} force={0.42} stageWidth={380} />
      </div>
      <div className="perps-confetti-cannon perps-confetti-cannon-middle">
        <ConfettiCannon particleCount={30} force={0.37} stageWidth={420} />
      </div>
      <div className="perps-confetti-cannon perps-confetti-cannon-right">
        <ConfettiCannon particleCount={27} force={0.34} stageWidth={340} />
      </div>
    </div>
  )
}
