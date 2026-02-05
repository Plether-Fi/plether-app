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
        <label className="block text-sm font-medium text-cyber-text-secondary mb-1.5">
          {label}
        </label>
      )}
      <div className="relative">
        <input
          type="text"
          inputMode="decimal"
          value={value}
          onChange={handleChange}
          disabled={disabled}
          placeholder="0.00"
          className={`
            w-full bg-cyber-surface-light border text-cyber-text-primary
            placeholder-cyber-text-secondary/50 focus:outline-none focus:ring-1
            focus:ring-cyber-bright-blue focus:border-cyber-bright-blue
            disabled:opacity-50 disabled:cursor-not-allowed
            transition-all shadow-sm shadow-cyber-border-glow/10
            ${compact ? 'px-3 py-2.5 pr-24 text-base' : 'px-4 py-4 pr-32 text-xl'}
            ${error ? 'border-cyber-electric-fuchsia' : 'border-cyber-border-glow/30'}
          `}
        />
        <div className="absolute right-3 top-1/2 -translate-y-1/2 flex items-center gap-2">
          {balance !== undefined && (
            <button
              onClick={handleMax}
              disabled={disabled}
              className="text-xs font-semibold text-cyber-text-secondary hover:text-cyber-text-primary px-2 py-1 bg-cyber-text-secondary/10 transition-colors cursor-pointer disabled:opacity-50 disabled:cursor-not-allowed"
            >
              MAX
            </button>
          )}
          <TokenLabel token={token.symbol} />
        </div>
      </div>

      {balance !== undefined && (
        <div className="flex justify-between mt-2 text-sm">
          <span className="text-cyber-text-secondary">{balanceLabel}</span>
          <span className="text-cyber-text-primary flex items-center gap-1.5">
            {token.symbol === 'USDC' ? formatUsd(balance) : formatAmount(balance, token.decimals)}
            <TokenLabel token={token.symbol} />
          </span>
        </div>
      )}

      {error && <p className="mt-1 text-sm text-cyber-electric-fuchsia">{error}</p>}
    </div>
  )
}
