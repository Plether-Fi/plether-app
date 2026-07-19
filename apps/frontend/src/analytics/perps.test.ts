import { beforeEach, describe, expect, it, vi } from 'vitest'
import {
  trackPerpsMarginLifecycle,
  trackPerpsOrderLifecycle,
} from './perps'

const analyticsMock = vi.hoisted(() => ({
  captureAnalyticsEvent: vi.fn(),
  captureFrontendLog: vi.fn(),
}))

vi.mock('./client', () => ({
  captureAnalyticsEvent: analyticsMock.captureAnalyticsEvent,
  captureFrontendLog: analyticsMock.captureFrontendLog,
}))

describe('perps structured logs', () => {
  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('logs order failures with stable messages and structured context', () => {
    trackPerpsOrderLifecycle('commit_failed', {
      error_category: 'user_rejected',
      chain_state: 'correct_chain',
    })

    expect(analyticsMock.captureFrontendLog).toHaveBeenCalledWith(
      'warn',
      'perps order lifecycle failed',
      {
        surface: 'perps',
        error_category: 'user_rejected',
        chain_state: 'correct_chain',
        component: 'perps_trade_ticket',
        operation: 'order_commit',
        outcome: 'failure',
      }
    )
  })

  it('uses error severity for non-user-rejected failures', () => {
    trackPerpsOrderLifecycle('reveal_failed', {
      error_category: 'timeout_or_expired',
    })

    expect(analyticsMock.captureFrontendLog).toHaveBeenCalledWith(
      'error',
      'perps order lifecycle failed',
      expect.objectContaining({
        operation: 'order_reveal',
        error_category: 'timeout_or_expired',
      })
    )
  })

  it('logs margin failures but not successful lifecycle events', () => {
    trackPerpsMarginLifecycle('deposit_succeeded')
    expect(analyticsMock.captureFrontendLog).not.toHaveBeenCalled()

    trackPerpsMarginLifecycle('withdraw_failed', {
      error_category: 'unknown',
    })

    expect(analyticsMock.captureFrontendLog).toHaveBeenCalledWith(
      'error',
      'perps margin lifecycle failed',
      expect.objectContaining({
        operation: 'margin_withdraw',
        outcome: 'failure',
      })
    )
  })
})
