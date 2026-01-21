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
      <h2 className="text-xl font-semibold text-cyber-text-primary mb-4">Open Positions</h2>

      {isLoading ? (
        <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 p-8 text-center">
          <div className="text-cyber-text-secondary">Loading positions...</div>
        </div>
      ) : positions.length === 0 ? (
        <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 p-8 text-center">
          <div className="text-cyber-text-secondary mb-2">No open positions</div>
          <p className="text-cyber-text-secondary/60 text-sm">
            Open a leveraged position using the Leverage tab below
          </p>
        </div>
      ) : (
        <>
          {hasLowHealth && (
            <Alert variant="warning" title="Low Health Factor Warning" className="mb-6 shadow-lg shadow-cyber-warning-text/10">
              One or more positions have low health factors and may be at risk of liquidation.
            </Alert>
          )}

          <div className="space-y-4">
            {positions.map((position) => (
              <PositionCard
                key={position.id}
                position={position}
                onAdjust={() => onAdjust(position)}
                onClose={() => onClose(position)}
                isClosing={isClosing}
              />
            ))}
          </div>
        </>
      )}
    </div>
  )
}
