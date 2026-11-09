import type { PasskeyWithMetadata } from '@privy-io/react-auth'
import { useId, useState, type CSSProperties, type ReactNode } from 'react'
import { currentBrowserDevice, matchesBrowserDevice, passkeyDeviceLabel } from './passkeyDevice'
import { MAX_PASSKEY_NAME_LENGTH, type PasskeyNames } from './passkeyNames'

const nameButton: CSSProperties = {
  minHeight: 44, padding: '8px 12px', border: '1px solid var(--privy-color-foreground-4)',
  background: 'transparent', color: 'var(--privy-color-foreground)', fontSize: 12, cursor: 'pointer',
}

function PasskeyName({ name, fallback, disabled, onRename }: {
  name: string; fallback: string; disabled: boolean; onRename: (name: string) => Promise<string | null>
}) {
  const [editing, setEditing] = useState(false)
  const [draft, setDraft] = useState('')
  const [error, setError] = useState<string | null>(null)
  const hintId = useId()
  const [saving, setSaving] = useState(false)
  const save = async () => {
    if (saving || disabled) return
    setSaving(true)
    try {
      const failure = await onRename(draft)
      setError(failure)
      if (!failure) setEditing(false)
    } catch { setError('Could not save the name. Try again.')
    } finally { setSaving(false) }
  }
  return editing ? <form onSubmit={event => {
    event.preventDefault()
    void save()
  }}>
    <label style={{ display: 'block', fontSize: 12 }}>
      Passkey name
      <input autoFocus value={draft} maxLength={MAX_PASSKEY_NAME_LENGTH} disabled={disabled || saving}
        aria-describedby={hintId} onChange={event => { setDraft(event.target.value); setError(null) }}
        style={{ display: 'block', width: '100%', boxSizing: 'border-box', marginTop: 4, padding: 8,
          border: '1px solid var(--privy-color-foreground-4)', borderRadius: 0,
          background: 'var(--privy-color-background)', color: 'var(--privy-color-foreground)', fontSize: 16 }} />
    </label>
    <p id={hintId} style={{ margin: '6px 0', fontSize: 12, color: 'var(--privy-color-foreground-2)' }}>Name syncs across your devices. Leave empty to use the default name.</p>
    <div style={{ display: 'flex', gap: 8 }}>
      <button type="submit" disabled={disabled || saving} style={{ ...nameButton, background: 'var(--privy-color-accent)', color: '#fff' }}>{saving ? 'Saving…' : 'Save'}</button>
      <button type="button" disabled={disabled || saving} style={nameButton} onClick={() => { setEditing(false); setError(null) }}>Cancel</button>
    </div>
    {error ? <p role="alert" style={{ margin: '6px 0 0', fontSize: 12, color: 'var(--privy-color-error)' }}>{error}</p> : null}
  </form> : <div style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
    <div style={{ fontSize: 14, fontWeight: 500 }}>{name || fallback}</div>
    <button type="button" disabled={disabled} aria-label="Rename passkey" title="Rename passkey"
      className="inline-flex h-7 w-7 shrink-0 cursor-pointer items-center justify-center border border-brand-border/30 bg-app-bg text-content-secondary transition-colors hover:border-[#FFAB96] hover:text-[#FFAB96] disabled:cursor-default disabled:opacity-50"
      onClick={() => { setDraft(name || fallback); setError(null); setEditing(true) }}>
      <span className="material-symbols-outlined !text-[16px] !leading-none" aria-hidden="true">edit</span>
    </button>
  </div>
}

export function RegisteredPasskeys({ passkeys, mfaEnabled, action, renderMfaAction, disabled = false, names = {}, onRename }: {
  passkeys: PasskeyWithMetadata[]
  mfaEnabled: boolean
  action?: ReactNode
  renderMfaAction?: (passkey: PasskeyWithMetadata) => ReactNode
  disabled?: boolean
  names?: PasskeyNames
  onRename?: (credentialId: string, name: string) => Promise<string | null>
}) {
  const currentDevice = currentBrowserDevice(navigator.userAgent)
  return (
    <section aria-label="Registered passkeys" style={{ marginBottom: 16 }}>
      <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'space-between', flexWrap: 'wrap', gap: 8, marginBottom: 12 }}>
        <h3 style={{ margin: 0, fontSize: '1.125rem', lineHeight: '1.875rem', fontWeight: 600 }}>Registered passkeys</h3>
        {action}
      </div>
      {passkeys.length === 0 ? (
        <p style={{ margin: 0, fontSize: 14, color: 'var(--privy-color-foreground-2)' }}>No passkeys registered yet.</p>
      ) : (
        <ul aria-label="Registered passkeys" style={{ listStyle: 'none', margin: 0, padding: 0, maxHeight: 280, overflowY: 'auto' }}>
          {passkeys.map((passkey, index) => {
            const device = passkeyDeviceLabel(passkey)
            const matchesCurrent = matchesBrowserDevice(passkey, currentDevice)
            const authenticator = passkey.authenticatorName?.trim() ?? ''
            const label = authenticator || `Passkey ${String(index + 1)}`
            const customName = Object.hasOwn(names, passkey.credentialId) ? names[passkey.credentialId] : ''
            const enabled = mfaEnabled && passkey.enrolledInMfa
            const added = passkey.firstVerifiedAt
            const hasDate = added instanceof Date && !Number.isNaN(added.getTime())
            return (
              <li key={passkey.credentialId} style={{
                display: 'flex', flexWrap: 'wrap', alignItems: 'center', gap: 12, padding: 12,
                marginTop: index === 0 ? 0 : 8,
                border: `1px solid var(${matchesCurrent ? '--privy-color-accent' : '--privy-color-foreground-4'})`,
                background: matchesCurrent ? 'var(--privy-color-background-2)' : undefined,
              }}>
                <div style={{ flex: '1 1 180px', minWidth: 0, overflowWrap: 'anywhere' }}>
                  {onRename ? <PasskeyName name={customName} fallback={label} disabled={disabled}
                    onRename={name => onRename(passkey.credentialId, name)} />
                    : <div style={{ fontSize: 14, fontWeight: 500 }}>{customName || label}</div>}
                  {authenticator ? <div style={{ marginTop: 4, fontSize: 12, color: 'var(--privy-color-foreground-2)' }}>Authenticator: {authenticator}</div> : null}
                  {matchesCurrent ? <span style={{ display: 'inline-block', marginTop: 4, fontSize: 12, color: 'var(--privy-color-accent-light)' }}>
                    Registration matches this browser
                  </span> : null}
                  <div style={{ marginTop: 4, fontSize: 12, color: 'var(--privy-color-foreground-2)' }}>
                    {device ? `Registered on ${device}` : 'Registration device unavailable'}
                  </div>
                  {hasDate ? <div style={{ marginTop: 4, fontSize: 12, color: 'var(--privy-color-foreground-2)' }}>
                    Added <time dateTime={added.toISOString()}>{added.toLocaleString(undefined, {
                      year: 'numeric', month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit',
                    })}</time>
                  </div> : null}
                </div>
                <div style={{ flex: '1 1 180px' }}><span style={{ fontSize: 12, color: enabled ? 'var(--privy-color-success)' : 'var(--privy-color-foreground-2)' }}>
                  {enabled ? 'Wallet MFA enabled' : 'Not enabled for wallet MFA'}
                </span><div>{renderMfaAction?.(passkey)}</div></div>
              </li>
            )
          })}
        </ul>
      )}
    </section>
  )
}
