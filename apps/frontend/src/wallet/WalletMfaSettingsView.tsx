import type { PasskeyWithMetadata } from '@privy-io/react-auth'
import type { CSSProperties } from 'react'
import { RegisteredPasskeys } from './RegisteredPasskeys'
import { isPasskeyOriginSupported, localhostPasskeyUrl } from './passkeyOrigin'
import type { PasskeyNames } from './passkeyNames'

const buttonStyle: CSSProperties = {
  width: '100%', padding: '12px 16px', border: '1px solid var(--privy-color-foreground-4)',
  borderRadius: 0, background: 'var(--privy-color-background-2)',
  color: 'var(--privy-color-foreground)', fontSize: 14, textAlign: 'left', cursor: 'pointer',
}

export interface WalletMfaSettingsViewProps {
  passkeys: PasskeyWithMetadata[]
  mfaEnabled: boolean
  labels: {
    names: PasskeyNames
    saving: boolean
    error: string | null
    retry: () => void
    rename: (credentialId: string, name: string) => Promise<string | null>
  }
  updating: boolean
  selectedKey: string | null
  updatedKey: string | null
  error: string | null
  onEnablePasskey: (credentialId: string) => void
  onOpenSettings: () => void
  onAddPasskey?: () => void
}

export function WalletMfaSettingsView({ passkeys, mfaEnabled, labels, updating, selectedKey, updatedKey, error,
  onEnablePasskey, onOpenSettings, onAddPasskey }: WalletMfaSettingsViewProps) {
  const canUpdate = mfaEnabled && passkeys.some(key => !key.enrolledInMfa)
  const passkeySupported = isPasskeyOriginSupported()
  const localhostUrl = localhostPasskeyUrl()
  return (
    <div style={{ marginTop: 16, paddingTop: 16, borderTop: '1px solid var(--privy-color-foreground-4)' }}>
      {labels.error ? <p role="alert" style={{ margin: '0 0 12px', fontSize: 12 }}>
        Could not load passkey names. <button type="button" style={buttonStyle} onClick={labels.retry}>Retry names</button>
      </p> : null}
      <RegisteredPasskeys passkeys={passkeys} names={labels.names} onRename={labels.rename}
        mfaEnabled={mfaEnabled} disabled={updating || labels.saving}
        renderMfaAction={key => <>
          {canUpdate && !key.enrolledInMfa && updatedKey !== key.credentialId ? <button type="button"
            disabled={updating} onClick={() => { onEnablePasskey(key.credentialId) }}
            style={{ ...buttonStyle, width: 'auto', marginTop: 8, minHeight: 44 }}>
            {updating && selectedKey === key.credentialId ? 'Updating wallet MFA…' : 'Enable for wallet MFA'}
          </button> : null}
          {selectedKey === key.credentialId && error ? <p role="alert" style={{ margin: '8px 0 0', color: 'var(--privy-color-error)', fontSize: 14 }}>{error}</p> : null}
          {updatedKey === key.credentialId ? <p role="status" style={{ margin: '8px 0 0', fontSize: 14 }}>Wallet MFA enabled for this passkey. Existing MFA keys remain enabled.</p> : null}
        </>}
        action={onAddPasskey ? (
        <button type="button" disabled={updating || !passkeySupported} onClick={onAddPasskey} style={{
          display: 'inline-flex', alignItems: 'center', gap: 4, minHeight: 44, padding: 0,
          border: 0, background: 'transparent', color: 'var(--privy-color-foreground)',
          fontSize: '0.875rem', lineHeight: '1.375rem', fontWeight: 400, cursor: updating || !passkeySupported ? 'default' : 'pointer',
          opacity: updating || !passkeySupported ? 0.65 : 1,
        }}>
          <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="3" strokeLinecap="round" strokeLinejoin="round" aria-hidden="true">
            <path d="M12 4.5v15M4.5 12h15" />
          </svg>
          Add passkey
        </button>
      ) : undefined} />
      {onAddPasskey && !passkeySupported ? <p style={{ margin: '0 0 12px', fontSize: 12, color: 'var(--privy-color-foreground-2)' }}>
          Passkeys need an HTTPS domain or localhost.{localhostUrl ? <> <a href={localhostUrl}
            style={{ color: 'var(--privy-color-foreground)', textDecoration: 'underline' }}>Open localhost to use passkeys</a>.</> : null}
      </p> : null}
      <button type="button" onClick={onOpenSettings} disabled={updating} style={buttonStyle}>
        Wallet MFA settings
      </button>
      {error && selectedKey === null ? <p role="alert" style={{ marginTop: 12, color: 'var(--privy-color-error)', fontSize: 14 }}>{error}</p> : null}
    </div>
  )
}
