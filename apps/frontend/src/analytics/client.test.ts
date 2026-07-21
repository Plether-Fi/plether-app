import { describe, expect, it, vi, beforeEach } from 'vitest'
import {
  captureAnalyticsEvent,
  captureFrontendLog,
  createAnalyticsConfig,
  resetAnalyticsForTests,
  sanitizeAnalyticsProperties,
  sanitizeFrontendLogAttributes,
  sanitizeFrontendLogRecord,
} from './client'

const posthogMock = vi.hoisted(() => ({
  capture: vi.fn(),
  captureLog: vi.fn(),
  init: vi.fn(),
  startSessionRecording: vi.fn(),
}))

vi.mock('posthog-js', () => ({
  default: posthogMock,
}))

describe('analytics client', () => {
  beforeEach(() => {
    vi.unstubAllEnvs()
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

  it('configures privacy-safe structured logs with deployment metadata', () => {
    expect(createAnalyticsConfig(0.05, 'sepolia')).toEqual(expect.objectContaining({
      logs: expect.objectContaining({
        serviceName: 'plether-web',
        environment: 'sepolia',
        serviceVersion: expect.any(String),
        captureConsoleLogs: false,
        maxLogsPerInterval: 100,
        beforeSend: sanitizeFrontendLogRecord,
      }),
    }))
  })

  it('allow-lists structured log attributes and redacts embedded sensitive values', () => {
    expect(sanitizeFrontendLogAttributes({
      component: 'perps_trade_ticket',
      operation: 'order_commit',
      outcome: 'failure',
      error_category: 'failed for user@example.com',
      http_status: 503,
      wallet_address: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
      details: { rpc: 'payload' },
    })).toEqual({
      component: 'perps_trade_ticket',
      operation: 'order_commit',
      outcome: 'failure',
      error_category: 'failed for [redacted]',
      http_status: 503,
    })
  })

  it('sanitizes log records before they enter the PostHog buffer', () => {
    expect(sanitizeFrontendLogRecord({
      body: 'request failed for 0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
      level: 'error',
      attributes: {
        component: 'api_client',
        authorization: 'Bearer secret',
      },
    })).toEqual({
      body: 'request failed for [redacted]',
      level: 'error',
      attributes: {
        component: 'api_client',
      },
    })
  })

  it('does not capture structured logs when analytics is disabled', () => {
    captureFrontendLog('error', 'frontend failed', { component: 'react_root' })
    expect(posthogMock.captureLog).not.toHaveBeenCalled()
  })
})
