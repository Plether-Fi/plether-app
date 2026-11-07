import { useEffect, useRef, useState } from 'react'
import { isPasskeyOriginSupported, localhostPasskeyUrl } from './passkeyOrigin'

interface LoginOption {
  label: string
  onClick: (() => void) | (() => Promise<string | undefined>)
  disabled?: boolean
  kind?: 'passkey-login' | 'passkey-link'
  icon?: 'google'
}

export function EmailLoginOption({ onClick, label = 'Log in with Email', options = [] }: {
  onClick: () => void
  label?: string
  options?: LoginOption[]
}) {
  const [error, setError] = useState<string | null>(null)
  const [pending, setPending] = useState(false)
  const attempt = useRef(0)
  useEffect(() => () => { attempt.current++ }, [])
  const selectOption = async (option: LoginOption) => {
    if (pending) return
    setError(null)
    const current = ++attempt.current
    if (option.kind !== 'passkey-login') {
      await option.onClick()
      return
    }
    setPending(true)
    try {
      const result = await option.onClick()
      if (attempt.current === current && typeof result === 'string') setError(result)
    } finally {
      if (attempt.current === current) setPending(false)
    }
  }
  const passkeySupported = isPasskeyOriginSupported()
  const hasPasskey = options.some(option => option.kind)
  const localhostUrl = localhostPasskeyUrl()
  const passkeyDisabled = (option: LoginOption) => pending || option.disabled === true || (option.kind !== undefined && !passkeySupported)
  return (
    <span className="ph-no-capture ph-no-record" style={{ display: 'block', width: 'min(312px, calc(100vw - 64px))' }}>
      <button
        type="button"
        disabled={pending}
        onClick={() => { setError(null); onClick() }}
        style={{
          display: 'flex', alignItems: 'center', gap: 12, width: '100%', minHeight: 56,
          padding: '14px 16px', border: '1px solid rgba(255, 171, 150, 0.3)',
          borderRadius: 'var(--privy-border-radius-md)', background: '#3B212D', color: '#FFF5F9',
          textAlign: 'left', fontSize: 16, fontWeight: 500, cursor: 'pointer',
        }}
      >
        <svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.7" aria-hidden="true">
          <rect x="3" y="5" width="18" height="14" rx="2" />
          <path d="m3 6 9 7 9-7" />
        </svg>
        {label}
      </button>
      {options.map(option => (
        <button key={option.label} type="button" onClick={() => { void selectOption(option) }} disabled={passkeyDisabled(option)}
          style={{ display: 'flex', alignItems: 'center', gap: 12, width: '100%', minHeight: 56, marginTop: 8,
            padding: '12px 16px', border: '1px solid rgba(255, 171, 150, 0.3)',
            borderRadius: 'var(--privy-border-radius-md)', background: '#3B212D', color: '#FFF5F9',
            textAlign: 'left', fontSize: 16, cursor: passkeyDisabled(option) ? 'default' : 'pointer',
            opacity: passkeyDisabled(option) ? 0.65 : 1 }}>
          {option.icon === 'google' ? <img src="/google.svg" alt="" aria-hidden="true" width="24" height="24" /> : null}
          {option.label}
        </button>
      ))}
      {error ? <span role="alert" style={{ display: 'block', marginTop: 12, padding: 12,
        border: '1px solid rgba(255, 87, 45, 0.5)', background: 'rgba(255, 87, 45, 0.1)',
        color: '#FFAB96', fontSize: 14, lineHeight: 1.5 }}>{error}</span> : null}
      {hasPasskey && !passkeySupported ? (
        <span style={{ display: 'block', marginTop: 10, color: '#D8CBD0', fontSize: 12, lineHeight: 1.5 }}>
          Passkeys need an HTTPS domain or localhost.{localhostUrl ? <> <a href={localhostUrl}
              style={{ color: '#FFF5F9', textDecoration: 'underline' }}>Open localhost to use passkeys</a>.</> : null}
        </span>
      ) : null}
      <span style={{ display: 'flex', alignItems: 'center', gap: 12, margin: '20px 0 4px', color: '#D8CBD0', fontSize: 11 }}>
        <span style={{ height: 1, flex: 1, background: 'rgba(255, 171, 150, 0.2)' }} />
        OR CONNECT A WALLET
        <span style={{ height: 1, flex: 1, background: 'rgba(255, 171, 150, 0.2)' }} />
      </span>
    </span>
  )
}
