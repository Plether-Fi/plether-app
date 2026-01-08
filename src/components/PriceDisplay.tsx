import { Skeleton } from './ui'
import type { ProtocolStatus } from '../config/constants'

interface PriceDisplayProps {
  price?: bigint
  status?: ProtocolStatus
  isLoading?: boolean
  variant?: 'compact' | 'detailed'
}

const mockPrice = 103.45
const mockStatus: ProtocolStatus = 'Active'

export function PriceDisplay({
  isLoading = false,
  variant = 'compact',
}: PriceDisplayProps) {
  const price = mockPrice
  const status = mockStatus

  const getStatusStyles = (s: ProtocolStatus) => {
    switch (s) {
      case 'Active':
        return 'bg-cyber-neon-green/20 text-cyber-neon-green border-cyber-neon-green/30 shadow-cyber-neon-green/10'
      case 'Paused':
        return 'bg-cyber-warning-bg text-cyber-warning-text border-cyber-warning-text/30 shadow-cyber-warning-text/10'
      case 'Settled':
        return 'bg-cyber-bright-blue/20 text-cyber-bright-blue border-cyber-bright-blue/30 shadow-cyber-bright-blue/10'
    }
  }

  if (variant === 'compact') {
    return (
      <div className="flex items-center gap-4">
        <div className="flex items-center gap-2">
          <span className="text-cyber-text-secondary text-sm">DXY</span>
          {isLoading ? (
            <Skeleton width={60} height={20} />
          ) : (
            <span className="text-cyber-text-primary font-semibold">${price.toFixed(2)}</span>
          )}
        </div>
        <span className={`px-2 py-0.5 text-xs font-medium border shadow-sm ${getStatusStyles(status)}`}>
          {status}
        </span>
      </div>
    )
  }

  return (
    <div className="bg-cyber-surface-dark  border border-cyber-border-glow/30 p-4 shadow-lg shadow-cyber-border-glow/10">
      <div className="flex items-center justify-between mb-3">
        <h3 className="text-cyber-text-secondary text-sm">DXY Index Price</h3>
        <span className={`px-2 py-0.5 text-xs font-medium border shadow-sm ${getStatusStyles(status)}`}>
          {status}
        </span>
      </div>
      {isLoading ? (
        <Skeleton width={120} height={36} />
      ) : (
        <div className="flex items-baseline gap-2">
          <span className="text-3xl font-bold text-cyber-text-primary">${price.toFixed(2)}</span>
        </div>
      )}
      <p className="text-xs text-cyber-text-secondary mt-2">
        Updated from BasketOracle
      </p>
    </div>
  )
}
