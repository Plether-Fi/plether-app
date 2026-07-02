import { Alert } from './ui'
import { PositionCard } from './PositionCard'
import { HEALTH_FACTOR_WARNING } from '../config/constants'
import type { LeveragePosition } from '../types'

export interface PositionsSectionProps {
  positions: LeveragePosition[]
  isLoading: boolean
  isClosing: boolean
  onAdjust: (position: LeveragePosition) => void
  onClose: (position: LeveragePosition) => void
}

export function PositionsSection({
  positions,
  isLoading,
  isClosing,
  onAdjust,
  onClose,
}: PositionsSectionProps) {
  const hasLowHealth = positions.some((p) => p.healthFactor > 0 && p.healthFactor < HEALTH_FACTOR_WARNING)

  return (
    <div className="mb-12">
      <h2 className="text-xl font-semibold text-content-primary mb-4">Open Leveraged Positions</h2>

      {isLoading ? (
        <div className="bg-surface-panel border border-brand-border/30 p-8 text-center">
          <div className="text-content-secondary">Loading positions...</div>
        </div>
      ) : positions.length === 0 ? (
        <div className="bg-surface-panel border border-brand-border/30 p-8 text-center">
          <div className="text-content-secondary mb-2">No open positions</div>
          <p className="text-content-secondary/60 text-sm">
            Open a leveraged position using the Leverage tab below
          </p>
        </div>
      ) : (
        <>
          {hasLowHealth && (
            <Alert variant="warning" title="Low Health Factor Warning" className="mb-6">
              One or more positions have low health factors and may be at risk of liquidation.
            </Alert>
          )}

          <div className="space-y-4">
            {positions.map((position) => (
              <PositionCard
                key={position.id}
                position={position}
                onAdjust={() => { onAdjust(position) }}
                onClose={() => { onClose(position) }}
                isClosing={isClosing}
              />
            ))}
          </div>
        </>
      )}
    </div>
  )
}
