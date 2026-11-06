import { type ChangeEvent, useId } from 'react'
import { formatUnits } from 'viem'
import { formatAmount, formatUsd, parseAmount } from '../utils/formatters'
import { TokenLabel } from './ui'
import { PRESET_BUTTON_CLASS, PRESET_BUTTON_SELECTED_CLASS, PRESET_BUTTON_UNSELECTED_CLASS } from './ui/presetButtonStyles'

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
  showAmountShortcuts?: boolean
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
  showAmountShortcuts = false,
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
          id={inputId}
          inputMode="decimal"
          value={value}
          onChange={handleChange}
          disabled={disabled}
          aria-invalid={error ? true : undefined}
          aria-describedby={error ? errorId : undefined}
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
          {!showAmountShortcuts && balance !== undefined && (
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

      {showAmountShortcuts && (
        <div role="group" aria-label={`${label ?? token.symbol} shortcuts`} className="mt-2 grid grid-cols-4 gap-1.5 whitespace-nowrap text-xs font-medium text-content-secondary">
          {[25, 50, 75, 100].map((percentage) => {
            const presetAmount = (balance ?? 0n) * BigInt(percentage) / 100n
            const isMax = percentage === 100
            const isSelected = isMax && presetAmount > 0n && parseAmount(value, token.decimals) === presetAmount

            return (
              <button
                key={percentage}
                type="button"
                title={isMax ? 'Use the maximum available amount' : `Use ${percentage.toString()}% of the available amount`}
                aria-pressed={isMax ? isSelected : undefined}
                disabled={disabled === true || presetAmount <= 0n}
                className={`${PRESET_BUTTON_CLASS} cursor-pointer ${isSelected ? PRESET_BUTTON_SELECTED_CLASS : PRESET_BUTTON_UNSELECTED_CLASS} disabled:cursor-not-allowed disabled:opacity-50`}
                onClick={() => { onChange(formatUnits(presetAmount, token.decimals)) }}
              >
                {isMax ? 'Max' : `${percentage.toString()}%`}
              </button>
            )
          })}
        </div>
      )}

      {balance !== undefined && (
        <div className="mt-2 flex flex-wrap justify-between gap-x-3 gap-y-1 text-sm">
          <span className="text-content-secondary">{balanceLabel}</span>
          <span className="flex min-w-0 flex-wrap items-center justify-end gap-1.5 text-right text-content-primary">
            {token.symbol === 'USDC' ? formatUsd(balance) : formatAmount(balance, token.decimals)}
            <TokenLabel token={token.symbol} />
          </span>
        </div>
      )}

      {error && <p id={errorId} className="mt-1 text-sm text-brand-orange">{error}</p>}
    </div>
  )
}
