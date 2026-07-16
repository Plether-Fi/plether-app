import { describe, expect, it, vi, beforeEach } from 'vitest'
import {
  captureAnalyticsEvent,
  resetAnalyticsForTests,
  sanitizeAnalyticsProperties,
} from './client'

const posthogMock = vi.hoisted(() => ({
  capture: vi.fn(),
  init: vi.fn(),
  startSessionRecording: vi.fn(),
}))

vi.mock('posthog-js/dist/module.full.no-external', () => ({
  default: posthogMock,
}))

describe('analytics client', () => {
  beforeEach(() => {
    resetAnalyticsForTests()
    vi.clearAllMocks()
  })

  it('keeps only allow-listed properties', () => {
    expect(sanitizeAnalyticsProperties({
      surface: 'perps',
      direction: 'long',
      wallet_address: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
      wallet_family: 'MetaMask',
      wallet_version: '12.0.0',
      manifest_version: 'perps-aa-arbitrum-sepolia-v1',
      exact_amount: '1000',
      arbitrary: 'value',
    })).toEqual({
      surface: 'perps',
      direction: 'long',
      wallet_family: 'MetaMask',
      wallet_version: '12.0.0',
      manifest_version: 'perps-aa-arbitrum-sepolia-v1',
    })
  })

  it('redacts address-like, tx-hash-like, and email-like values', () => {
    expect(sanitizeAnalyticsProperties({
      surface: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
      button_id: '0x1111111111111111111111111111111111111111111111111111111111111111',
      validation_reason: 'user@example.com',
    })).toEqual({
      surface: '[redacted]',
      button_id: '[redacted]',
      validation_reason: '[redacted]',
    })
  })

  it('drops null, undefined, and non-finite numeric values', () => {
    expect(sanitizeAnalyticsProperties({
      surface: 'perps',
      duration_ms: Number.NaN,
      modal_id: undefined,
      close_reason: null,
    })).toEqual({
      surface: 'perps',
    })
  })

  it('does not capture when analytics is disabled', () => {
    captureAnalyticsEvent('perps button clicked', {
      button_id: 'review_trade',
      surface: 'perps',
    })

    expect(posthogMock.capture).not.toHaveBeenCalled()
  })
})
