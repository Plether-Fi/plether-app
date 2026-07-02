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
  const sideColor = position.side === 'BEAR' ? 'text-brand-orange' : 'text-positive'
  const pnlColor = position.pnl >= 0n ? 'text-positive' : 'text-brand-orange'
  const healthColor = position.healthFactor >= HEALTH_FACTOR_WARNING
    ? 'text-positive'
    : position.healthFactor >= HEALTH_FACTOR_DANGER
      ? 'text-warning'
      : 'text-brand-orange'

  return (
    <div className="bg-surface-panel p-4 border border-brand-border/30 hover:border-[#FFAB96]/50 transition-all">
      <div className="flex flex-col md:flex-row md:items-center justify-between gap-4">
        <div className="flex items-center gap-4">
          <TokenIcon side={position.side} />
          <div>
            <div className="flex items-center gap-2">
              <span className={`font-semibold ${sideColor}`}>plDXY-{position.side}</span>
              <span className="px-1.5 py-0.5 bg-surface-muted text-xs text-content-secondary font-medium border border-brand-border/30">
                {position.leverage}x
              </span>
            </div>
            <div className="text-xs text-content-secondary mt-1">
              Size: {formatUsd(position.size)} USDC | Equity: {formatUsd(position.collateral)} USDC
            </div>
          </div>
        </div>

        <div className="flex flex-wrap items-center gap-6 lg:gap-12 flex-1 md:justify-end">
          <div className="flex flex-col">
            <span className="text-xs text-content-secondary mb-1">PnL</span>
            <span className={`text-sm font-semibold ${pnlColor}`}>
              {formatUsd(position.pnl)} ({position.pnlPercentage > 0 ? '+' : ''}{formatPercent(position.pnlPercentage)})
            </span>
          </div>
          <div className="flex flex-col">
            <span className="text-xs text-content-secondary mb-1">Liq. Price</span>
            <span className="text-sm font-semibold text-content-primary">
              {(Number(position.liquidationPrice) / 1e6).toFixed(2)} USDC
            </span>
          </div>
          <div className="flex flex-col">
            <div className="flex items-center gap-1 text-xs text-content-secondary mb-1">
              Health
              <span className="material-symbols-outlined text-[10px] text-content-secondary">help</span>
            </div>
            <span className={`text-sm font-semibold ${healthColor}`}>
              {position.healthFactor.toFixed(2)}
            </span>
          </div>

          <div className="flex items-center gap-2 mt-2 md:mt-0">
            <button
              onClick={onAdjust}
              className="border border-brand-border/30 px-3 py-1.5 text-sm text-content-secondary transition-colors hover:bg-[#3B212D] hover:text-[#FFAB96] hover:underline hover:underline-offset-4"
            >
              Adjust
            </button>
            <button
              onClick={onClose}
              disabled={isClosing}
              className="px-3 py-1.5 text-sm bg-brand-orange text-content-primary enabled:hover:bg-[#FF572D] enabled:hover:underline enabled:hover:underline-offset-4 transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
            >
              {isClosing ? 'Closing...' : 'Close'}
            </button>
          </div>
        </div>
      </div>
    </div>
  )
}
