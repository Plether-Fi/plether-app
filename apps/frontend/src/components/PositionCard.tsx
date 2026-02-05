import { TokenIcon } from './ui'
import { formatUsd, formatPercent } from '../utils/formatters'
import { HEALTH_FACTOR_WARNING, HEALTH_FACTOR_DANGER } from '../config/constants'
import type { LeveragePosition } from '../types'

export interface PositionCardProps {
  position: LeveragePosition
  onAdjust: () => void
  onClose: () => void
  isClosing?: boolean
}

export function PositionCard({ position, onAdjust, onClose, isClosing }: PositionCardProps) {
  const sideColor = position.side === 'BEAR' ? 'text-cyber-electric-fuchsia' : 'text-cyber-neon-green'
  const pnlColor = position.pnl >= 0n ? 'text-cyber-neon-green' : 'text-cyber-electric-fuchsia'
  const healthColor = position.healthFactor >= HEALTH_FACTOR_WARNING
    ? 'text-cyber-neon-green'
    : position.healthFactor >= HEALTH_FACTOR_DANGER
      ? 'text-cyber-warning-text'
      : 'text-cyber-electric-fuchsia'

  return (
    <div className="bg-cyber-surface-dark p-4 border border-cyber-border-glow/30 hover:border-cyber-bright-blue/50 transition-all shadow-md">
      <div className="flex flex-col md:flex-row md:items-center justify-between gap-4">
        <div className="flex items-center gap-4">
          <TokenIcon side={position.side} />
          <div>
            <div className="flex items-center gap-2">
              <span className={`font-semibold ${sideColor}`}>plDXY-{position.side}</span>
              <span className="px-1.5 py-0.5 bg-cyber-surface-light text-xs text-cyber-text-secondary font-medium border border-cyber-border-glow/30">
                {position.leverage}x
              </span>
            </div>
            <div className="text-xs text-cyber-text-secondary mt-1">
              Size: {formatUsd(position.size)} USDC | Equity: {formatUsd(position.collateral)} USDC
            </div>
          </div>
        </div>

        <div className="flex flex-wrap items-center gap-6 lg:gap-12 flex-1 md:justify-end">
          <div className="flex flex-col">
            <span className="text-xs text-cyber-text-secondary mb-1">PnL</span>
            <span className={`text-sm font-semibold ${pnlColor}`}>
              {formatUsd(position.pnl)} ({position.pnlPercentage > 0 ? '+' : ''}{formatPercent(position.pnlPercentage)})
            </span>
          </div>
          <div className="flex flex-col">
            <span className="text-xs text-cyber-text-secondary mb-1">Liq. Price</span>
            <span className="text-sm font-semibold text-cyber-text-primary">
              {(Number(position.liquidationPrice) / 1e6).toFixed(2)} USDC
            </span>
          </div>
          <div className="flex flex-col">
            <div className="flex items-center gap-1 text-xs text-cyber-text-secondary mb-1">
              Health
              <span className="material-symbols-outlined text-[10px] text-cyber-text-secondary">help</span>
            </div>
            <span className={`text-sm font-semibold ${healthColor}`}>
              {position.healthFactor.toFixed(2)}
            </span>
          </div>

          <div className="flex items-center gap-2 mt-2 md:mt-0">
            <button
              onClick={onAdjust}
              className="px-3 py-1.5 text-sm border border-cyber-border-glow/30 text-cyber-text-secondary hover:bg-cyber-surface-light hover:text-cyber-bright-blue transition-colors"
            >
              Adjust
            </button>
            <button
              onClick={onClose}
              disabled={isClosing}
              className="px-3 py-1.5 text-sm bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/80 text-cyber-text-primary transition-colors shadow-md shadow-cyber-electric-fuchsia/20 disabled:opacity-50 disabled:cursor-not-allowed"
            >
              {isClosing ? 'Closing...' : 'Close'}
            </button>
          </div>
        </div>
      </div>
    </div>
  )
}
