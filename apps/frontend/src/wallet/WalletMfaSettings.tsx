import { useLayoutEffect, useRef, useState } from 'react'
import { useActiveWallet, useMfa, useMfaEnrollment, usePrivy, useUser, type User } from '@privy-io/react-auth'
import { usePasskeyNames } from './passkeyNames'
import { WalletMfaSettingsView } from './WalletMfaSettingsView'

export function WalletMfaSettings({ onAddPasskey }: { onAddPasskey?: () => void }) {
  const { ready, authenticated, user } = usePrivy()
  const { wallet } = useActiveWallet()
  if (!ready || !authenticated || !user || wallet?.walletClientType !== 'privy') return null
  return <WalletMfaActions key={`${user.id}:${wallet.address.toLowerCase()}`} user={user} wallet={wallet} onAddPasskey={onAddPasskey} />
}

function WalletMfaActions({ user, wallet, onAddPasskey }: {
  user: User
  wallet: NonNullable<ReturnType<typeof useActiveWallet>['wallet']>
  onAddPasskey?: () => void
}) {
  const { getAccessToken } = usePrivy()
  const labels = usePasskeyNames(user.id, getAccessToken)
  const { promptMfa } = useMfa()
  const { refreshUser } = useUser()
  const { showMfaEnrollmentModal, initEnrollmentWithPasskey, submitEnrollmentWithPasskey } = useMfaEnrollment()
  const [error, setError] = useState<string | null>(null)
  const [updating, setUpdating] = useState(false)
  const [selectedKey, setSelectedKey] = useState<string | null>(null)
  const [updatedKey, setUpdatedKey] = useState<string | null>(null)
  const context = useRef({ user, wallet })
  const attempt = useRef(0)
  const pending = useRef(false)
  useLayoutEffect(() => { context.current = { user, wallet } })
  // The keyed child unmounts on logout, owner changes, or modal dismissal.
  useLayoutEffect(() => () => { attempt.current += 1 }, [])

  const passkeys = user.linkedAccounts.filter(account => account.type === 'passkey')
  const canUpdate = user.mfaMethods.includes('passkey') && passkeys.some(key => !key.enrolledInMfa)

  const openSettings = () => {
    setSelectedKey(null)
    setUpdatedKey(null)
    setError(null)
    // The SDK owns enrollment, verification, and dismissal. Keep the account
    // dialog underneath, just as for Privy's other wallet management screens.
    void showMfaEnrollmentModal().catch(() => {
      setError('Could not open wallet MFA settings. Try again.')
    })
  }

  const enablePasskey = async (credentialId: string) => {
    if (pending.current || !canUpdate || !passkeys.some(key => key.credentialId === credentialId && !key.enrolledInMfa)) return
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
      return keys.filter(key => key.credentialId === credentialId || key.enrolledInMfa)
    }

    pending.current = true
    setSelectedKey(credentialId)
    setUpdating(true)
    setUpdatedKey(null)
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
      if (currentPasskeys()) setUpdatedKey(credentialId)
    } catch {
      if (attempt.current === id) setError('Could not complete the wallet MFA update. Verify with an existing MFA method and try again.')
    } finally {
      pending.current = false
      if (attempt.current === id) setUpdating(false)
    }
  }

  return <WalletMfaSettingsView passkeys={passkeys} mfaEnabled={user.mfaMethods.includes('passkey')} labels={labels}
    updating={updating} selectedKey={selectedKey} updatedKey={updatedKey} error={error}
    onEnablePasskey={credentialId => { void enablePasskey(credentialId) }} onOpenSettings={openSettings} onAddPasskey={onAddPasskey} />
}
