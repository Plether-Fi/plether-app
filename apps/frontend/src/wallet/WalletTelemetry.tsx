import { useEffect, useRef, useState } from 'react'
import { captureAnalyticsEvent } from '../analytics/client'
import { useWalletActions } from './useWalletActions'

// Record bounded wallet outcomes without user IDs, addresses, login methods,
// tokens, provider errors, or signing payloads.
export function WalletTelemetry() {
  const { configured, ready, signerReady, wallets, error } = useWalletActions()
  const [startedAt] = useState(() => performance.now())
  const reportedReady = useRef(false)
  const previousState = useRef<string | undefined>(undefined)
  const connectionState = signerReady ? 'connected' : wallets.length > 0 ? 'selection_required' : 'disconnected'

  useEffect(() => {
    if (!configured || !ready || reportedReady.current) return
    reportedReady.current = true
    captureAnalyticsEvent('wallet_provider_ready', {
      component: 'wallet_provider',
      duration_ms: Math.round(performance.now() - startedAt),
      outcome: 'success',
    })
  }, [configured, ready, startedAt])

  useEffect(() => {
    if (!configured || !ready || previousState.current === connectionState) return
    previousState.current = connectionState
    captureAnalyticsEvent('wallet_connection_state_changed', {
      component: 'wallet_provider',
      connected_state: connectionState,
    })
  }, [configured, ready, connectionState])

  useEffect(() => {
    if (!configured || !error) return
    captureAnalyticsEvent('wallet_action_failed', {
      component: 'wallet_provider',
      outcome: 'failure',
      error_category: 'wallet_action',
    })
  }, [configured, error])

  return null
}
