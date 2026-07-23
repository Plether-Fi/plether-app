import { type ChangeEvent } from 'react'
import { formatAmount, formatUsd } from '../utils/formatters'
import { TokenLabel } from './ui'

interface TokenInputProps {
  value: string
  onChange: (value: string) => void
  token: {
    symbol: string
    decimals: number
  }
  balance?: bigint
  balanceLabel?: string
  label?: string
  disabled?: boolean
  error?: string
  compact?: boolean
}

export function TokenInput({
  value,
  onChange,
  token,
  balance,
  balanceLabel = 'Balance:',
  label,
  disabled,
  error,
  compact = false,
}: TokenInputProps) {
  const handleChange = (e: ChangeEvent<HTMLInputElement>) => {
    const newValue = e.target.value
    if (newValue === '' || /^\d*\.?\d*$/.test(newValue)) {
      onChange(newValue)
    }
  }

  const handleMax = () => {
    if (balance) {
      const formatted = formatAmount(balance, token.decimals, token.decimals)
      onChange(formatted.replace(/,/g, ''))
    }
  }

  return (
    <div className="w-full">
      {label && (
        <label className="block text-sm font-medium text-content-secondary mb-1.5">
          {label}
        </label>
      )}
      <div
        className={`
          grid w-full grid-cols-[minmax(0,1fr)_auto] items-center bg-app-bg
          border transition-colors focus-within:border-[#FFAB96]
          ${error ? 'border-brand-orange' : 'border-brand-border/30'}
          ${disabled ? 'opacity-50' : ''}
        `}
      >
        <input
          type="text"
          inputMode="decimal"
          value={value}
          onChange={handleChange}
          disabled={disabled}
          placeholder="0.00"
          className={`
            min-w-0 w-full bg-transparent text-content-primary
            placeholder-content-secondary/50 focus:outline-none
            disabled:opacity-50 disabled:cursor-not-allowed
            transition-all
            ${compact ? 'px-3 py-2.5 text-base' : 'px-4 py-4 text-lg sm:text-xl'}
          `}
        />
        <div className="flex shrink-0 items-center gap-1.5 pr-3 sm:gap-2">
          {balance !== undefined && (
            <button
              type="button"
              onClick={handleMax}
              disabled={disabled}
              className="min-h-11 bg-content-secondary/10 px-2 py-1 text-xs font-semibold text-content-secondary transition-colors hover:bg-[#3B212D] hover:text-content-primary hover:underline hover:underline-offset-4 disabled:cursor-not-allowed disabled:opacity-50 disabled:hover:no-underline"
            >
              MAX
            </button>
          )}
          <TokenLabel token={token.symbol} />
        </div>
      </div>

      {balance !== undefined && (
        <div className="mt-2 flex flex-wrap justify-between gap-x-3 gap-y-1 text-sm">
          <span className="text-content-secondary">{balanceLabel}</span>
          <span className="flex min-w-0 flex-wrap items-center justify-end gap-1.5 text-right text-content-primary">
            {token.symbol === 'USDC' ? formatUsd(balance) : formatAmount(balance, token.decimals)}
            <TokenLabel token={token.symbol} />
          </span>
        </div>
      )}

      {error && <p className="mt-1 text-sm text-brand-orange">{error}</p>}
    </div>
  )
}
