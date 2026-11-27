import { useId, useState, type SyntheticEvent } from 'react'
import { useNavigate } from 'react-router-dom'
import { isTxHash, isWalletAddress } from '../utils/format'

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
    if (isWalletAddress(trimmed)) {
      setError(null)
      setValue('')
      void navigate(`/wallets/${trimmed}`)
      return
    }
    if (isTxHash(trimmed)) {
      setError(null)
      setValue('')
      void navigate(`/tx/${trimmed.toLowerCase()}`)
      return
    }
    setError('Enter a complete 0x Trading Account address (42 characters) or transaction hash (66 characters).')
  }

  return (
    <form
      onSubmit={handleSubmit}
      role="search"
      aria-label="Inspect Trading Account or transaction"
      className={className}
      noValidate
    >
      <label htmlFor={inputId} className="sr-only">
        Inspect Trading Account address or transaction hash
      </label>
      <div className="flex min-w-0 items-stretch">
        <input
          id={inputId}
          type="search"
          value={value}
          onChange={(event) => {
            setValue(event.target.value)
            if (error) setError(null)
          }}
          placeholder={compact ? '0x… account / tx' : '0x address or tx hash'}
          aria-invalid={error !== null}
          aria-describedby={error ? errorId : undefined}
          className={`w-full min-w-0 flex-1 border border-brand-border/35 bg-app-bg font-mono text-content-primary placeholder:font-sans placeholder:text-xs placeholder:text-content-tertiary sm:placeholder:text-sm ${
            compact ? 'px-3 py-1.5 text-base sm:text-xs' : 'px-3 py-2 text-base sm:px-4 sm:py-2.5 sm:text-sm'
          }`}
        />
        <button
          type="submit"
          className={`shrink-0 border border-l-0 border-brand-border/35 bg-surface-raised font-semibold text-brand-peach transition-colors hover:border-brand-orange hover:bg-brand-orange hover:text-content-primary ${
            compact ? 'px-3 py-1.5 text-xs' : 'px-3.5 py-2 text-sm sm:px-4 sm:py-2.5'
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
