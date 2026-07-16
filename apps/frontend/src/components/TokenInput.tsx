import { type ChangeEvent, useId } from 'react'
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
  const inputId = useId()
  const errorId = `${inputId}-error`

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
        <label htmlFor={inputId} className="block text-sm font-medium text-content-secondary mb-1.5">
          {label}
        </label>
      )}
      <div className="relative">
        <input
          type="text"
          id={inputId}
          inputMode="decimal"
          value={value}
          onChange={handleChange}
          disabled={disabled}
          aria-invalid={error ? true : undefined}
          aria-describedby={error ? errorId : undefined}
          placeholder="0.00"
          className={`
            w-full bg-app-bg border text-content-primary
            placeholder-content-secondary/50 focus:outline-none
            focus:border-[#FFAB96]
            disabled:opacity-50 disabled:cursor-not-allowed
            transition-all
            ${compact ? 'px-3 py-2.5 pr-24 text-base' : 'px-4 py-4 pr-32 text-xl'}
            ${error ? 'border-brand-orange' : 'border-brand-border/30'}
          `}
        />
        <div className="absolute right-3 top-1/2 -translate-y-1/2 flex items-center gap-2">
          {balance !== undefined && (
            <button
              type="button"
              onClick={handleMax}
              disabled={disabled}
              className="bg-content-secondary/10 px-2 py-1 text-xs font-semibold text-content-secondary transition-colors hover:bg-[#3B212D] hover:text-content-primary hover:underline hover:underline-offset-4 disabled:cursor-not-allowed disabled:opacity-50 disabled:hover:no-underline"
            >
              MAX
            </button>
          )}
          <TokenLabel token={token.symbol} />
        </div>
      </div>

      {balance !== undefined && (
        <div className="flex justify-between mt-2 text-sm">
          <span className="text-content-secondary">{balanceLabel}</span>
          <span className="text-content-primary flex items-center gap-1.5">
            {token.symbol === 'USDC' ? formatUsd(balance) : formatAmount(balance, token.decimals)}
            <TokenLabel token={token.symbol} />
          </span>
        </div>
      )}

      {error && <p id={errorId} className="mt-1 text-sm text-brand-orange">{error}</p>}
    </div>
  )
}
