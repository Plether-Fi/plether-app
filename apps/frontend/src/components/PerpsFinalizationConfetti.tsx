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
  stageHeight = 230,
}: {
  particleCount: number
  force: number
  stageWidth: number
  stageHeight?: number
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
      stageHeight={stageHeight}
      stageWidth={stageWidth}
    />
  )
})

export function PerpsFinalizationConfetti({
  origin,
}: {
  origin?: {
    x: number
    y: number
    stageWidth: number
    stageHeight: number
    direction: 'up' | 'down'
  }
}) {
  return (
    <div
      className="perps-confetti-burst pointer-events-none absolute inset-0 z-0 overflow-hidden"
      data-finalization-confetti
      aria-hidden="true"
    >
      {origin ? (
        <div
          className="perps-confetti-cannon"
          data-confetti-origin
          style={{
            left: origin.x,
            top: origin.y,
            transform: origin.direction === 'up' ? 'rotate(180deg)' : undefined,
          }}
        >
          <ConfettiCannon
            particleCount={91}
            force={0.42}
            stageWidth={origin.stageWidth}
            stageHeight={origin.stageHeight}
          />
        </div>
      ) : (
        <>
          <div className="perps-confetti-cannon perps-confetti-cannon-left">
            <ConfettiCannon particleCount={34} force={0.42} stageWidth={380} />
          </div>
          <div className="perps-confetti-cannon perps-confetti-cannon-middle">
            <ConfettiCannon particleCount={30} force={0.37} stageWidth={420} />
          </div>
          <div className="perps-confetti-cannon perps-confetti-cannon-right">
            <ConfettiCannon particleCount={27} force={0.34} stageWidth={340} />
          </div>
        </>
      )}
    </div>
  )
}
