import { InfoTooltip, TokenIcon } from './ui'
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
    <div className="min-w-0 border border-brand-border/30 bg-surface-panel p-4 transition-all hover:border-[#FFAB96]/50">
      <div className="flex min-w-0 flex-col justify-between gap-4 md:flex-row md:items-center">
        <div className="flex min-w-0 items-start gap-3 sm:items-center sm:gap-4">
          <span className="shrink-0">
            <TokenIcon side={position.side} />
          </span>
          <div className="min-w-0">
            <div className="flex min-w-0 flex-wrap items-center gap-2">
              <span className={`break-words font-semibold ${sideColor}`}>plDXY-{position.side}</span>
              <span className="shrink-0 border border-brand-border/30 bg-surface-muted px-1.5 py-0.5 text-xs font-medium text-content-secondary">
                {position.leverage}x
              </span>
            </div>
            <div className="mt-1 break-words text-xs text-content-secondary [overflow-wrap:anywhere]">
              Size: {formatUsd(position.size)} USDC | Equity: {formatUsd(position.collateral)} USDC
            </div>
          </div>
        </div>

        <div className="grid w-full min-w-0 grid-cols-2 gap-x-4 gap-y-4 sm:grid-cols-3 md:flex md:flex-1 md:flex-wrap md:items-center md:justify-end md:gap-6 lg:gap-12">
          <div className="flex min-w-0 flex-col">
            <span className="text-xs text-content-secondary mb-1">PnL</span>
            <span className={`break-words text-sm font-semibold [overflow-wrap:anywhere] ${pnlColor}`}>
              {formatUsd(position.pnl)} ({position.pnlPercentage > 0 ? '+' : ''}{formatPercent(position.pnlPercentage)})
            </span>
          </div>
          <div className="flex min-w-0 flex-col">
            <span className="text-xs text-content-secondary mb-1">Liq. Price</span>
            <span className="break-words text-sm font-semibold text-content-primary [overflow-wrap:anywhere]">
              {(Number(position.liquidationPrice) / 1e6).toFixed(2)} USDC
            </span>
          </div>
          <div className="flex min-w-0 flex-col">
            <div className="flex items-center gap-1 text-xs text-content-secondary mb-1">
              Health
              <InfoTooltip content="Ratio of collateral value to debt. Lower values increase liquidation risk." />
            </div>
            <span className={`text-sm font-semibold ${healthColor}`}>
              {position.healthFactor.toFixed(2)}
            </span>
          </div>

          <div className="col-span-2 grid w-full grid-cols-2 gap-2 sm:col-span-3 md:mt-0 md:flex md:w-auto md:items-center">
            <button
              onClick={onAdjust}
              className="min-h-11 min-w-0 border border-brand-border/30 px-3 py-2 text-sm text-content-secondary transition-colors hover:bg-[#3B212D] hover:text-[#FFAB96] hover:underline hover:underline-offset-4"
            >
              Adjust
            </button>
            <button
              onClick={onClose}
              disabled={isClosing}
              className="min-h-11 min-w-0 bg-brand-orange px-3 py-2 text-sm text-content-primary transition-colors enabled:hover:bg-[#FF572D] enabled:hover:underline enabled:hover:underline-offset-4 disabled:cursor-not-allowed disabled:opacity-50"
            >
              {isClosing ? 'Closing...' : 'Close'}
            </button>
          </div>
        </div>
      </div>
    </div>
  )
}
