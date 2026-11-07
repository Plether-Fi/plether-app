import { useState } from 'react'
import { useActiveWallet, useMfaEnrollment, usePrivy } from '@privy-io/react-auth'

export function WalletMfaSettings() {
  const { ready, authenticated, user } = usePrivy()
  const { wallet } = useActiveWallet()
  const { showMfaEnrollmentModal } = useMfaEnrollment()
  const [error, setError] = useState<string | null>(null)

  if (!ready || !authenticated || !user || wallet?.walletClientType !== 'privy') return null

  const openSettings = () => {
    setError(null)
    // The SDK owns enrollment, verification, and dismissal. Keep the account
    // dialog underneath, just as for Privy's other wallet management screens.
    void showMfaEnrollmentModal().catch(() => {
      setError('Could not open wallet MFA settings. Try again.')
    })
  }

  return (
    <div style={{ marginTop: 16, paddingTop: 16, borderTop: '1px solid var(--privy-color-foreground-4)' }}>
      <button type="button" onClick={openSettings} style={{ width: '100%', padding: '12px 16px',
        border: '1px solid var(--privy-color-foreground-4)', borderRadius: 0,
        background: 'var(--privy-color-background-2)', color: 'var(--privy-color-foreground)',
        fontSize: 14, textAlign: 'left', cursor: 'pointer' }}>
        Wallet MFA settings
      </button>
      {error ? <p role="alert" style={{ marginTop: 12, color: 'var(--privy-color-error)', fontSize: 14 }}>{error}</p> : null}
    </div>
  )
}
