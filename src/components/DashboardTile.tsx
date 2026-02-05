import { Skeleton, TokenIcon, TokenLabel } from './ui'
import { formatAmount, formatUsd } from '../utils/formatters'

type TileVariant = 'BULL' | 'USDC' | 'BEAR'

interface DashboardTileProps {
  variant: TileVariant
  title: string
  balance: bigint
  balanceDecimals: number
  balanceToken: string
  secondaryValue: bigint
  secondaryLabel: string
  secondaryDecimals: number
  secondaryToken: string
  apy?: number
  isLoading?: boolean
}

const variantStyles: Record<TileVariant, { glow: string; text: string; border: string; divider: string }> = {
  BULL: {
    glow: 'shadow-[0_0_8px_rgba(0,255,153,0.1)] hover:shadow-[0_0_20px_rgba(0,255,153,0.4)]',
    text: 'text-cyber-neon-green',
    border: 'border-cyber-neon-green/50',
    divider: 'border-cyber-neon-green/30',
  },
  USDC: {
    glow: 'shadow-[0_0_8px_rgba(0,200,255,0.1)] hover:shadow-[0_0_20px_rgba(0,200,255,0.4)]',
    text: 'text-cyber-bright-blue',
    border: 'border-cyber-bright-blue/50',
    divider: 'border-cyber-bright-blue/30',
  },
  BEAR: {
    glow: 'shadow-[0_0_8px_rgba(255,0,204,0.1)] hover:shadow-[0_0_20px_rgba(255,0,204,0.4)]',
    text: 'text-cyber-electric-fuchsia',
    border: 'border-cyber-electric-fuchsia/50',
    divider: 'border-cyber-electric-fuchsia/30',
  },
}

function TileSkeleton() {
  return (
    <div className="bg-cyber-surface-dark p-5 border border-cyber-border-glow/50 shadow-md h-full">
      <div className="flex items-center justify-between mb-2">
        <Skeleton className="h-5 w-32" />
        <Skeleton className="h-8 w-8 rounded-full" />
      </div>
      <div className="space-y-3">
        <div className="pb-3 border-b border-cyber-border-glow/50">
          <Skeleton className="h-6 w-32" />
        </div>
        <div className="flex justify-between items-end">
          <div>
            <Skeleton className="h-3 w-20 mb-1" />
            <Skeleton className="h-6 w-28" />
          </div>
          <div className="text-right">
            <Skeleton className="h-3 w-8 mb-1 ml-auto" />
            <Skeleton className="h-6 w-12 ml-auto" />
          </div>
        </div>
      </div>
    </div>
  )
}

export function DashboardTile({
  variant,
  title,
  balance,
  balanceDecimals,
  balanceToken,
  secondaryValue,
  secondaryLabel,
  secondaryDecimals,
  secondaryToken,
  apy,
  isLoading,
}: DashboardTileProps) {
  const styles = variantStyles[variant]

  if (isLoading) {
    return <TileSkeleton />
  }

  return (
    <div
      className={`bg-cyber-surface-dark p-5 border ${styles.border} shadow-md ${styles.glow} h-full transition-shadow duration-200`}
    >
        <div className="flex items-center justify-between mb-2">
          <h3 className={`font-semibold ${styles.text}`}>{title}</h3>
          <TokenIcon side={variant} size="sm" />
        </div>

        <div className="space-y-3">
          <div className={`pb-3 border-b ${styles.divider}`}>
            <p className="text-2xl font-bold text-cyber-text-primary flex items-center gap-2">
              {balanceDecimals === 6 ? formatUsd(balance) : formatAmount(balance, balanceDecimals, 2, 2)}
              <TokenLabel token={balanceToken} />
            </p>
          </div>

          <div className="flex justify-between items-end">
            <div>
              <p className="text-xs text-cyber-text-secondary uppercase tracking-wider">{secondaryLabel}</p>
              <p className="text-lg font-semibold text-cyber-text-primary flex items-center gap-2">
                {secondaryDecimals === 6 ? formatUsd(secondaryValue) : formatAmount(secondaryValue, secondaryDecimals, 2, 2)}
                <TokenLabel token={secondaryToken} />
              </p>
            </div>
            <div className="text-right">
              <p className="text-xs text-cyber-text-secondary uppercase tracking-wider">Current APY</p>
              <p className={`text-lg font-semibold ${styles.text}`}>{(apy ?? 0).toFixed(2)}%</p>
            </div>
          </div>
        </div>
    </div>
  )
}
