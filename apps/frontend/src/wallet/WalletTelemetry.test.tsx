import { render } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { WalletTelemetry } from './WalletTelemetry'

const state = vi.hoisted(() => ({
  configured: true,
  ready: false,
  signerReady: false,
  wallets: [] as Array<{ address: string }>,
  error: null as string | null,
  capture: vi.fn(),
}))
vi.mock('./useWalletActions', () => ({ useWalletActions: () => state }))
vi.mock('../analytics/client', () => ({ captureAnalyticsEvent: state.capture }))

describe('wallet telemetry privacy and readiness', () => {
  beforeEach(() => {
    state.configured = true
    state.ready = false
    state.signerReady = false
    state.wallets = []
    state.error = null
    state.capture.mockClear()
  })

  it('reports initialization once, then bounded connection states without owner data', () => {
    const view = render(<WalletTelemetry />)
    expect(state.capture).not.toHaveBeenCalled()
    state.ready = true
    view.rerender(<WalletTelemetry />)
    expect(state.capture).toHaveBeenCalledWith('wallet_provider_ready', expect.objectContaining({ outcome: 'success', duration_ms: expect.any(Number) }))
    state.wallets = [{ address: '0x1111111111111111111111111111111111111111' }]
    view.rerender(<WalletTelemetry />)
    expect(state.capture).toHaveBeenLastCalledWith('wallet_connection_state_changed', {
      component: 'wallet_provider', connected_state: 'selection_required',
    })
    state.signerReady = true
    view.rerender(<WalletTelemetry />)
    expect(state.capture).toHaveBeenLastCalledWith('wallet_connection_state_changed', {
      component: 'wallet_provider', connected_state: 'connected',
    })
    expect(state.capture.mock.calls.filter(([name]) => name === 'wallet_provider_ready')).toHaveLength(1)
    expect(JSON.stringify(state.capture.mock.calls)).not.toContain(state.wallets[0].address)
  })

  it('never sends provider errors and remains quiet when unconfigured', () => {
    state.configured = false
    state.ready = true
    state.error = 'sensitive SDK error: user@example.com'
    const view = render(<WalletTelemetry />)
    expect(state.capture).not.toHaveBeenCalled()
    state.configured = true
    view.rerender(<WalletTelemetry />)
    expect(state.capture).toHaveBeenLastCalledWith('wallet_action_failed', {
      component: 'wallet_provider', outcome: 'failure', error_category: 'wallet_action',
    })
    expect(JSON.stringify(state.capture.mock.calls)).not.toContain(state.error)
  })
})
