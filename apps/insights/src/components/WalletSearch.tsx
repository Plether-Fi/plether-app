import { useId, useState, type SyntheticEvent } from 'react'
import { useNavigate } from 'react-router-dom'
import { isWalletAddress } from '../utils/format'

export function WalletSearch({
  compact = false,
  className = '',
}: {
  compact?: boolean
  className?: string
}) {
  const navigate = useNavigate()
  const inputId = useId()
  const errorId = useId()
  const [value, setValue] = useState('')
  const [error, setError] = useState<string | null>(null)

  function handleSubmit(event: SyntheticEvent<HTMLFormElement>) {
    event.preventDefault()
    const trimmed = value.trim()
    if (!isWalletAddress(trimmed)) {
      setError('Enter a complete 0x Trading Account address (42 characters).')
      return
    }
    setError(null)
    setValue('')
    void navigate(`/wallets/${trimmed}`)
  }

  return (
    <form
      onSubmit={handleSubmit}
      role="search"
      aria-label="Inspect Trading Account"
      className={className}
      noValidate
    >
      <label htmlFor={inputId} className="sr-only">
        Inspect Trading Account address
      </label>
      <div className="flex items-stretch">
        <input
          id={inputId}
          type="search"
          value={value}
          onChange={(event) => {
            setValue(event.target.value)
            if (error) setError(null)
          }}
          placeholder={compact ? 'Trading Account 0x…' : 'Inspect Trading Account (0x…)'}
          aria-invalid={error !== null}
          aria-describedby={error ? errorId : undefined}
          className={`min-w-0 flex-1 border border-brand-border/35 bg-app-bg font-mono text-content-primary placeholder:font-sans placeholder:text-content-tertiary ${
            compact ? 'px-3 py-1.5 text-xs' : 'px-4 py-2.5 text-sm'
          }`}
        />
        <button
          type="submit"
          className={`shrink-0 border border-l-0 border-brand-border/35 bg-surface-raised font-semibold text-brand-peach transition-colors hover:border-brand-orange hover:bg-brand-orange hover:text-content-primary ${
            compact ? 'px-3 py-1.5 text-xs' : 'px-4 py-2.5 text-sm'
          }`}
        >
          Inspect
        </button>
      </div>
      {error ? (
        <p id={errorId} role="alert" className="mt-1.5 text-xs text-brand-peach">
          {error}
        </p>
      ) : null}
    </form>
  )
}
