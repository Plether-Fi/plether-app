import { useLayoutEffect, useRef, useState, type CSSProperties } from 'react'
import { useActiveWallet, useMfa, useMfaEnrollment, usePrivy, useUser, type User } from '@privy-io/react-auth'

const buttonStyle: CSSProperties = {
  width: '100%', padding: '12px 16px', border: '1px solid var(--privy-color-foreground-4)',
  borderRadius: 0, background: 'var(--privy-color-background-2)',
  color: 'var(--privy-color-foreground)', fontSize: 14, textAlign: 'left', cursor: 'pointer',
}

export function WalletMfaSettings() {
  const { ready, authenticated, user } = usePrivy()
  const { wallet } = useActiveWallet()
  if (!ready || !authenticated || !user || wallet?.walletClientType !== 'privy') return null
  return <WalletMfaActions key={`${user.id}:${wallet.address.toLowerCase()}`} user={user} wallet={wallet} />
}

function WalletMfaActions({ user, wallet }: {
  user: User
  wallet: NonNullable<ReturnType<typeof useActiveWallet>['wallet']>
}) {
  const { promptMfa } = useMfa()
  const { refreshUser } = useUser()
  const { showMfaEnrollmentModal, initEnrollmentWithPasskey, submitEnrollmentWithPasskey } = useMfaEnrollment()
  const [error, setError] = useState<string | null>(null)
  const [updating, setUpdating] = useState(false)
  const [updated, setUpdated] = useState(false)
  const context = useRef({ user, wallet })
  const attempt = useRef(0)
  const pending = useRef(false)
  useLayoutEffect(() => { context.current = { user, wallet } })
  // The keyed child unmounts on logout, owner changes, or modal dismissal.
  useLayoutEffect(() => () => { attempt.current += 1 }, [])

  const passkeys = user.linkedAccounts.filter(account => account.type === 'passkey')
  const canUpdate = user.mfaMethods.includes('passkey') && passkeys.some(key => !key.enrolledInMfa)

  const openSettings = () => {
    setError(null)
    // The SDK owns enrollment, verification, and dismissal. Keep the account
    // dialog underneath, just as for Privy's other wallet management screens.
    void showMfaEnrollmentModal().catch(() => {
      setError('Could not open wallet MFA settings. Try again.')
    })
  }

  const enableAdditionalPasskeys = async () => {
    if (pending.current || !canUpdate) return
    const id = ++attempt.current
    const originalIds = passkeys.map(key => key.credentialId)
    const enrolledIds = passkeys.filter(key => key.enrolledInMfa).map(key => key.credentialId)
    const currentPasskeys = (latestUser?: User) => {
      const current = context.current
      if (attempt.current !== id || current.user.id !== user.id || current.wallet.walletClientType !== 'privy' ||
        current.wallet.address.toLowerCase() !== wallet.address.toLowerCase()) return null
      const enrollmentUser = latestUser ?? current.user
      if (enrollmentUser.id !== user.id) return null
      const keys = enrollmentUser.linkedAccounts.filter(account => account.type === 'passkey')
      if (!enrollmentUser.mfaMethods.includes('passkey') ||
        originalIds.some(keyId => !keys.some(key => key.credentialId === keyId)) ||
        keys.some(key => !originalIds.includes(key.credentialId) && !key.enrolledInMfa) ||
        enrolledIds.some(keyId => !keys.some(key => key.credentialId === keyId && key.enrolledInMfa))) {
        throw new Error('Passkeys changed during verification')
      }
      // Enrollment replaces the credential list. Preserve every enrolled key,
      // including keys enrolled elsewhere while verification was in progress.
      return keys.filter(key => originalIds.includes(key.credentialId) || key.enrolledInMfa)
    }

    pending.current = true
    setUpdating(true)
    setUpdated(false)
    setError(null)
    try {
      await promptMfa()
      if (!currentPasskeys()) return
      await initEnrollmentWithPasskey()
      if (!currentPasskeys()) return
      // Read the server's current enrollment immediately before replacing it.
      // SDK 3.51.2 may return undefined from its optional client despite its
      // non-null public return type. Never fall back to cached enrollment.
      const latestUser = await refreshUser() as User | undefined
      if (!latestUser) throw new Error('Cannot refresh MFA enrollment')
      if (!currentPasskeys()) return
      const keys = currentPasskeys(latestUser)
      if (!keys) return
      await submitEnrollmentWithPasskey({ credentialIds: [...new Set(keys.map(key => key.credentialId))] }, { removeForLogin: false })
      if (currentPasskeys()) setUpdated(true)
    } catch {
      if (attempt.current === id) setError('Could not complete the wallet MFA update. Verify with an existing MFA method and try again.')
    } finally {
      pending.current = false
      if (attempt.current === id) setUpdating(false)
    }
  }

  return (
    <div style={{ marginTop: 16, paddingTop: 16, borderTop: '1px solid var(--privy-color-foreground-4)' }}>
      <button type="button" onClick={openSettings} disabled={updating} style={buttonStyle}>
        Wallet MFA settings
      </button>
      {canUpdate && !updated ? <>
        <button type="button" onClick={() => { void enableAdditionalPasskeys() }} disabled={updating}
          style={{ ...buttonStyle, marginTop: 12 }}>
          {updating ? 'Updating wallet MFA…' : 'Enable additional passkeys for wallet MFA'}
        </button>
        <p style={{ marginTop: 12, color: 'var(--privy-color-foreground-2)', fontSize: 14 }}>
          Verify with an existing wallet MFA method to enable all linked passkeys. Your current MFA passkeys and login methods will be kept.
        </p>
      </> : null}
      {updated ? <p role="status" style={{ marginTop: 12, fontSize: 14 }}>Linked passkeys are enabled for wallet MFA. Your existing passkeys are still enabled.</p> : null}
      {error ? <p role="alert" style={{ marginTop: 12, color: 'var(--privy-color-error)', fontSize: 14 }}>{error}</p> : null}
    </div>
  )
}
