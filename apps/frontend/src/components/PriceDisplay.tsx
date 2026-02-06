import { Skeleton, TokenLabel, Tooltip } from './ui'
import { useProtocolStatus } from '../api'
import type { ProtocolStatus } from '../config/constants'
import { ORACLE_STALE_SECONDS } from '../config/constants'

interface PriceDisplayProps {
  variant?: 'compact' | 'detailed'
}

function formatTimeAgo(unixSeconds: number): string {
  const seconds = Math.floor(Date.now() / 1000 - unixSeconds)
  if (seconds < 60) return `${String(seconds)}s ago`
  const minutes = Math.floor(seconds / 60)
  if (minutes < 60) return `${String(minutes)}m ago`
  const hours = Math.floor(minutes / 60)
  return `${String(hours)}h ago`
}

export function PriceDisplay({
  variant = 'compact',
}: PriceDisplayProps) {
  const { data: protocolData, isLoading } = useProtocolStatus()

  const bullPriceStr = protocolData?.data.prices.bull ?? '0'
  const bullPrice = BigInt(bullPriceStr)
  const priceUnknown = bullPrice === 0n
  const price = bullPrice > 0n ? Number(bullPrice) / 1e8 : 0

  const updatedAt = protocolData?.data.oracle.updatedAt ?? 0
  const isStale = updatedAt > 0 && (Date.now() / 1000 - updatedAt) > ORACLE_STALE_SECONDS

  const apiStatus = protocolData?.data.status
  const baseStatus: ProtocolStatus = apiStatus === 'ACTIVE'
    ? 'Active'
    : apiStatus === 'PAUSED'
      ? 'Paused'
      : apiStatus === 'LIQUIDATED'
        ? 'Settled'
        : 'Active'

  const status: ProtocolStatus = isStale && baseStatus === 'Active' ? 'Stale' : baseStatus

  const getStatusStyles = (s: ProtocolStatus) => {
    switch (s) {
      case 'Active':
        return 'bg-cyber-neon-green/20 text-cyber-neon-green border-cyber-neon-green/30 shadow-cyber-neon-green/10'
      case 'Stale':
        return 'bg-cyber-warning-bg text-cyber-warning-text border-cyber-warning-text/30 shadow-cyber-warning-text/10'
      case 'Paused':
        return 'bg-cyber-warning-bg text-cyber-warning-text border-cyber-warning-text/30 shadow-cyber-warning-text/10'
      case 'Settled':
        return 'bg-cyber-bright-blue/20 text-cyber-bright-blue border-cyber-bright-blue/30 shadow-cyber-bright-blue/10'
    }
  }

  const statusBadge = (
    <Tooltip
      content={isStale ? `Oracle last updated ${formatTimeAgo(updatedAt)}` : ''}
      position="bottom"
    >
      <span className={`px-2 py-0.5 text-xs font-medium border shadow-sm ${getStatusStyles(status)}`}>
        {status}
      </span>
    </Tooltip>
  )

  if (variant === 'compact') {
    return (
      <div className="flex items-center gap-4">
        <div className="flex items-center gap-2">
          <TokenLabel token="plDXY-BULL" />
          {isLoading ? (
            <Skeleton width={60} height={20} />
          ) : priceUnknown ? (
            <span className="text-cyber-text-secondary font-semibold">--</span>
          ) : (
            <span className="text-cyber-text-primary font-semibold">{price.toFixed(4)} USDC</span>
          )}
        </div>
        {isStale ? statusBadge : (
          <span className={`px-2 py-0.5 text-xs font-medium border shadow-sm ${getStatusStyles(status)}`}>
            {status}
          </span>
        )}
      </div>
    )
  }

  return (
    <div className="bg-cyber-surface-dark  border border-cyber-border-glow/30 p-4 shadow-lg shadow-cyber-border-glow/10">
      <div className="flex items-center justify-between mb-3">
        <h3 className="text-cyber-text-secondary text-sm">plDXY Index Price</h3>
        {isStale ? statusBadge : (
          <span className={`px-2 py-0.5 text-xs font-medium border shadow-sm ${getStatusStyles(status)}`}>
            {status}
          </span>
        )}
      </div>
      {isLoading ? (
        <Skeleton width={120} height={36} />
      ) : priceUnknown ? (
        <div className="flex items-baseline gap-2">
          <span className="text-3xl font-bold text-cyber-text-secondary">--</span>
        </div>
      ) : (
        <div className="flex items-baseline gap-2">
          <span className="text-3xl font-bold text-cyber-text-primary">{price.toFixed(2)} USDC</span>
        </div>
      )}
      <p className="text-xs mt-2 text-cyber-text-secondary">
        Updated from Backend API
      </p>
    </div>
  )
}
