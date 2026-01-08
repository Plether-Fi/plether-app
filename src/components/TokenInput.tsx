import { type ChangeEvent } from 'react'
import { formatAmount } from '../utils/formatters'

interface TokenInputProps {
  value: string
  onChange: (value: string) => void
  token: {
    symbol: string
    decimals: number
  }
  balance?: bigint
  label?: string
  disabled?: boolean
  error?: string
}

export function TokenInput({
  value,
  onChange,
  token,
  balance,
  label,
  disabled,
  error,
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
            w-full px-4 py-4 pr-32 bg-cyber-surface-light border rounded-xl text-cyber-text-primary text-xl
            placeholder-cyber-text-secondary/50 focus:outline-none focus:ring-1
            focus:ring-cyber-bright-blue focus:border-cyber-bright-blue
            disabled:opacity-50 disabled:cursor-not-allowed
            transition-all shadow-sm shadow-cyber-border-glow/10
            ${error ? 'border-cyber-electric-fuchsia' : 'border-cyber-border-glow/30'}
          `}
        />
        <div className="absolute right-3 top-1/2 -translate-y-1/2 flex items-center gap-2">
          {balance !== undefined && (
            <button
              onClick={handleMax}
              disabled={disabled}
              className="text-xs font-semibold text-cyber-neon-green hover:text-cyber-neon-green/80 px-2 py-1 rounded bg-cyber-neon-green/10 transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
            >
              MAX
            </button>
          )}
          <span className="text-cyber-text-secondary font-medium">{token.symbol}</span>
        </div>
      </div>

      {balance !== undefined && (
        <div className="flex justify-between mt-2 text-sm">
          <span className="text-cyber-text-secondary">Balance:</span>
          <span className="text-cyber-text-primary">
            {formatAmount(balance, token.decimals)} {token.symbol}
          </span>
        </div>
      )}

      {error && <p className="mt-1 text-sm text-cyber-electric-fuchsia">{error}</p>}
    </div>
  )
}
