import { useEffect, useId, useRef, useState, type SyntheticEvent } from 'react'
import { useNavigate } from 'react-router-dom'
import { isTxHash, isWalletAddress } from '../utils/format'

/**
 * Compact header search bar that routes complete 0x addresses straight to
 * /wallets/:address, 66-char tx hashes to /tx/:txHash, and Discord handles or
 * partial queries to /accounts?q=...
 *
 * Focusable from anywhere via `/` or `Cmd/Ctrl + K`.
 */
export function GlobalSearch({ className = '' }: { className?: string }) {
  const navigate = useNavigate()
  const inputId = useId()
  const inputRef = useRef<HTMLInputElement>(null)
  const [value, setValue] = useState('')

  useEffect(() => {
    function onKeyDown(event: KeyboardEvent) {
      const target = event.target as HTMLElement | null
      const inEditable =
        target != null &&
        (target.tagName === 'INPUT' ||
          target.tagName === 'TEXTAREA' ||
          target.tagName === 'SELECT' ||
          target.isContentEditable)

      if ((event.metaKey || event.ctrlKey) && event.key.toLowerCase() === 'k') {
        event.preventDefault()
        inputRef.current?.focus()
        inputRef.current?.select()
        return
      }

      if (event.key === '/' && !event.metaKey && !event.ctrlKey && !event.altKey && !inEditable) {
        event.preventDefault()
        inputRef.current?.focus()
      }
    }

    document.addEventListener('keydown', onKeyDown)
    return () => {
      document.removeEventListener('keydown', onKeyDown)
    }
  }, [])

  function handleSubmit(event: SyntheticEvent<HTMLFormElement>) {
    event.preventDefault()
    const trimmed = value.trim()
    if (!trimmed) return

    if (isWalletAddress(trimmed)) {
      setValue('')
      inputRef.current?.blur()
      void navigate(`/wallets/${trimmed}`)
      return
    }
    if (isTxHash(trimmed)) {
      setValue('')
      inputRef.current?.blur()
      void navigate(`/tx/${trimmed.toLowerCase()}`)
      return
    }

    const query = trimmed.replace(/^@+/, '')
    setValue('')
    inputRef.current?.blur()
    void navigate(`/accounts?q=${encodeURIComponent(query)}`)
  }

  return (
    <form
      onSubmit={handleSubmit}
      role="search"
      aria-label="Search accounts, Discord handles, or transactions"
      className={`relative flex items-center ${className}`}
    >
      <label htmlFor={inputId} className="sr-only">
        Search account, @discord handle, or tx hash
      </label>
      <input
        ref={inputRef}
        id={inputId}
        type="search"
        value={value}
        onChange={(event) => {
          setValue(event.target.value)
        }}
        onKeyDown={(event) => {
          if (event.key === 'Escape') {
            setValue('')
            inputRef.current?.blur()
          }
        }}
        placeholder="Search 0x, @, tx"
        className="w-full border border-brand-border/35 bg-app-bg py-1.5 pl-2.5 pr-7 font-mono text-xs text-content-primary transition-colors placeholder:font-sans placeholder:text-content-tertiary focus:border-brand-peach/70 focus:outline-none"
      />
      <kbd
        aria-hidden="true"
        className="pointer-events-none absolute right-2 border border-brand-border/30 bg-surface-panel px-1 py-0.5 font-mono text-[10px] leading-none text-content-tertiary"
      >
        /
      </kbd>
    </form>
  )
}
