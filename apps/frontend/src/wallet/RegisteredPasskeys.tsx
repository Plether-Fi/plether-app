import type { PasskeyWithMetadata } from '@privy-io/react-auth'
import type { ReactNode } from 'react'
import { currentBrowserDevice, matchesBrowserDevice, passkeyDeviceLabel } from './passkeyDevice'

export function RegisteredPasskeys({ passkeys, mfaEnabled, action }: {
  passkeys: PasskeyWithMetadata[]
  mfaEnabled: boolean
  action?: ReactNode
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
            const label = device || authenticator || `Passkey ${String(index + 1)}`
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
                  <div style={{ fontSize: 14, fontWeight: 500 }}>{label}</div>
                  {matchesCurrent ? <span style={{ display: 'inline-block', marginTop: 4, fontSize: 12, color: 'var(--privy-color-accent-light)' }}>
                    Matches this device
                  </span> : null}
                  {device && authenticator ? <div style={{ marginTop: 4, fontSize: 12, color: 'var(--privy-color-foreground-2)' }}>{authenticator}</div> : null}
                  {!device ? <div style={{ marginTop: 4, fontSize: 12, color: 'var(--privy-color-foreground-2)' }}>Registration device unavailable</div> : null}
                  {hasDate ? <div style={{ marginTop: 4, fontSize: 12, color: 'var(--privy-color-foreground-2)' }}>
                    Added <time dateTime={added.toISOString()}>{added.toLocaleString(undefined, {
                      year: 'numeric', month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit',
                    })}</time>
                  </div> : null}
                </div>
                <span style={{ fontSize: 12, color: enabled ? 'var(--privy-color-success)' : 'var(--privy-color-foreground-2)' }}>
                  {enabled ? 'Wallet MFA enabled' : 'Not enabled for wallet MFA'}
                </span>
              </li>
            )
          })}
        </ul>
      )}
    </section>
  )
}
