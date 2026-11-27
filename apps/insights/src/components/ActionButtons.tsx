import { useEffect, useRef, useState } from 'react'
import { explorerAddressUrl, explorerTxUrl, resolvePerpsChain } from '../utils/chain'

const BASE_BUTTON_CLASS =
  'group inline-flex h-6 shrink-0 cursor-pointer items-center overflow-hidden whitespace-nowrap border font-mono text-[11px] font-semibold leading-none transition-colors duration-200 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-brand-peach'

const IDLE_TONE_CLASS =
  'border-brand-border/35 bg-app-bg/75 px-1.5 text-content-secondary hover:border-brand-peach/65 hover:bg-brand-peach/10 hover:text-brand-peach focus-visible:border-brand-peach/65 focus-visible:bg-brand-peach/10 focus-visible:text-brand-peach'

const COPIED_TONE_CLASS =
  'border-positive/60 bg-positive/10 px-1.5 text-positive'

export interface ArbiscanButtonProps {
  kind: 'tx' | 'address'
  value: string
  chainId?: number | null
  /** Optional accessible label override. Defaults to "View <value> on Arbiscan". */
  label?: string
  className?: string
}

/**
 * Compact square arrow button that smoothly expands on hover / focus-visible to
 * reveal "Arbiscan". Use everywhere an external Arbiscan link is needed, and
 * ensure each distinct external URL appears at most once per page.
 */
export function ArbiscanButton({
  kind,
  value,
  chainId,
  label,
  className,
}: ArbiscanButtonProps) {
  const chain = resolvePerpsChain(chainId)
  const href =
    kind === 'tx'
      ? explorerTxUrl(value, chainId)
      : explorerAddressUrl(value, chainId)
  const ariaLabel = label ?? `View ${value} on ${chain.explorerName}`

  return (
    <a
      href={href}
      target="_blank"
      rel="noreferrer"
      aria-label={ariaLabel}
      title={ariaLabel}
      className={[BASE_BUTTON_CLASS, IDLE_TONE_CLASS, className].filter(Boolean).join(' ')}
    >
      <span
        aria-hidden="true"
        className="max-w-0 overflow-hidden opacity-0 transition-all duration-200 ease-out group-hover:mr-1 group-hover:max-w-[5.5rem] group-hover:opacity-100 group-focus-visible:mr-1 group-focus-visible:max-w-[5.5rem] group-focus-visible:opacity-100"
      >
        {chain.explorerName}
      </span>
      <svg
        aria-hidden="true"
        viewBox="0 0 12 12"
        className="h-3 w-3 shrink-0 transition-transform duration-200 group-hover:translate-x-[0.5px] group-hover:-translate-y-[0.5px]"
        fill="none"
        stroke="currentColor"
        strokeWidth="1.6"
        strokeLinecap="square"
      >
        <path d="M3.5 8.5L8.5 3.5M4.25 3.5H8.5V7.75" />
      </svg>
    </a>
  )
}

export interface CopyButtonProps {
  /** Exact text copied to the clipboard when clicked. */
  value: string
  /** Label revealed on hover / focus-visible (default: "Copy"). */
  hoverLabel?: string
  /** Label shown briefly after copying (default: "Copied"). */
  copiedLabel?: string
  /** Optional accessible label override (default: "Copy <value>"). */
  ariaLabel?: string
  className?: string
}

/**
 * Compact square copy button matching ArbiscanButton: rests as a crisp copy
 * icon, smoothly expands on hover / focus-visible to show "Copy", and turns
 * green with a checkmark + "Copied" briefly after activation.
 */
export function CopyButton({
  value,
  hoverLabel = 'Copy',
  copiedLabel = 'Copied',
  ariaLabel,
  className,
}: CopyButtonProps) {
  const [copied, setCopied] = useState(false)
  const timerRef = useRef<ReturnType<typeof setTimeout> | null>(null)

  useEffect(() => {
    return () => {
      if (timerRef.current !== null) {
        clearTimeout(timerRef.current)
      }
    }
  }, [])

  function handleCopy() {
    if ('clipboard' in navigator && typeof navigator.clipboard.writeText === 'function') {
      void navigator.clipboard.writeText(value).catch(() => undefined)
    }
    setCopied(true)
    if (timerRef.current !== null) {
      clearTimeout(timerRef.current)
    }
    timerRef.current = setTimeout(() => {
      setCopied(false)
      timerRef.current = null
    }, 1800)
  }

  const label = ariaLabel ?? `${hoverLabel} ${value}`

  return (
    <button
      type="button"
      onClick={handleCopy}
      aria-label={copied ? copiedLabel : label}
      title={copied ? copiedLabel : label}
      className={[
        BASE_BUTTON_CLASS,
        copied ? COPIED_TONE_CLASS : IDLE_TONE_CLASS,
        className,
      ]
        .filter(Boolean)
        .join(' ')}
    >
      {copied ? (
        <svg
          aria-hidden="true"
          viewBox="0 0 12 12"
          className="h-3 w-3 shrink-0"
          fill="none"
          stroke="currentColor"
          strokeWidth="1.8"
          strokeLinecap="square"
        >
          <path d="M2.25 6.25L4.75 8.75L9.75 3.25" />
        </svg>
      ) : (
        <svg
          aria-hidden="true"
          viewBox="0 0 12 12"
          className="h-3 w-3 shrink-0"
          fill="none"
          stroke="currentColor"
          strokeWidth="1.4"
          strokeLinecap="square"
        >
          <rect x="4" y="4" width="6" height="6" />
          <path d="M8 4V2H2V8H4" />
        </svg>
      )}
      <span
        aria-hidden="true"
        className={
          copied
            ? 'ml-1 max-w-[6.5rem] overflow-hidden opacity-100 transition-all duration-200 ease-out'
            : 'max-w-0 overflow-hidden opacity-0 transition-all duration-200 ease-out group-hover:ml-1 group-hover:max-w-[6.5rem] group-hover:opacity-100 group-focus-visible:ml-1 group-focus-visible:max-w-[6.5rem] group-focus-visible:opacity-100'
        }
      >
        {copied ? copiedLabel : hoverLabel}
      </span>
    </button>
  )
}
