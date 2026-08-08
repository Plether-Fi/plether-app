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

const variantStyles: Record<TileVariant, { text: string; border: string; divider: string }> = {
  BULL: {
    text: 'text-positive',
    border: 'border-positive/50',
    divider: 'border-positive/30',
  },
  USDC: {
    text: 'text-brand-peach',
    border: 'border-brand-peach/50',
    divider: 'border-brand-peach/30',
  },
  BEAR: {
    text: 'text-brand-orange',
    border: 'border-brand-orange/50',
    divider: 'border-brand-orange/30',
  },
}

function TileSkeleton() {
  return (
    <div className="h-full min-w-0 border border-brand-border/50 bg-surface-panel p-4 sm:p-5">
      <div className="flex items-center justify-between mb-2">
        <Skeleton className="h-5 w-32 max-w-[75%]" />
        <Skeleton className="h-8 w-8 shrink-0 rounded-full" />
      </div>
      <div className="space-y-3">
        <div className="pb-3 border-b border-brand-border/50">
          <Skeleton className="h-6 w-32 max-w-full" />
        </div>
        <div className="grid grid-cols-1 gap-3 sm:grid-cols-2">
          <div className="min-w-0">
            <Skeleton className="mb-1 h-3 w-20 max-w-full" />
            <Skeleton className="h-6 w-28 max-w-full" />
          </div>
          <div className="min-w-0 sm:text-right">
            <Skeleton className="mb-1 h-3 w-8 max-w-full sm:ml-auto" />
            <Skeleton className="h-6 w-12 max-w-full sm:ml-auto" />
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
      className={`h-full min-w-0 border bg-surface-panel p-4 transition-colors duration-200 sm:p-5 ${styles.border}`}
    >
      <div className="mb-2 flex min-w-0 items-center justify-between gap-3">
        <h3 className={`min-w-0 break-words font-semibold ${styles.text}`}>{title}</h3>
        <span className="shrink-0">
          <TokenIcon side={variant} size="sm" />
        </span>
      </div>

      <div className="min-w-0 space-y-3">
        <div className={`border-b pb-3 ${styles.divider}`}>
          <p className="flex min-w-0 flex-wrap items-center gap-x-2 gap-y-1 break-words text-xl font-bold text-content-primary [overflow-wrap:anywhere] sm:text-2xl">
            {balanceDecimals === 6 ? formatUsd(balance) : formatAmount(balance, balanceDecimals, 2, 2)}
            <TokenLabel token={balanceToken} />
          </p>
        </div>

        <div className="grid min-w-0 grid-cols-1 gap-3 sm:grid-cols-2 sm:items-end">
          <div className="min-w-0">
            <p className="break-words text-xs uppercase tracking-wider text-content-secondary">{secondaryLabel}</p>
            <p className="flex min-w-0 flex-wrap items-center gap-x-2 gap-y-1 break-words text-lg font-semibold text-content-primary [overflow-wrap:anywhere]">
              {secondaryDecimals === 6 ? formatUsd(secondaryValue) : formatAmount(secondaryValue, secondaryDecimals, 2, 2)}
              <TokenLabel token={secondaryToken} />
            </p>
          </div>
          <div className="min-w-0 sm:text-right">
            <p className="break-words text-xs uppercase tracking-wider text-content-secondary">Current 7d APY</p>
            <p className={`break-words text-lg font-semibold ${styles.text}`}>{(apy ?? 0).toFixed(2)}%</p>
          </div>
        </div>
      </div>
    </div>
  )
}
