import { describe, expect, it, vi, beforeEach } from 'vitest'
import {
  captureAnalyticsEvent,
  captureFrontendLog,
  captureReactException,
  createAnalyticsConfig,
  initAnalytics,
  resetAnalyticsForTests,
  sanitizeAnalyticsProperties,
  sanitizeFrontendLogAttributes,
  sanitizeFrontendLogRecord,
} from './client'

const posthogMock = vi.hoisted(() => ({
  capture: vi.fn(),
  captureLog: vi.fn(),
  captureException: vi.fn(),
  init: vi.fn(),
  startSessionRecording: vi.fn(),
}))

vi.mock('posthog-js', () => ({
  default: posthogMock,
}))

describe('analytics client', () => {
  it('queues sanitized React exceptions while loading and includes component stack and release', async () => {
    resetAnalyticsForTests()
    vi.stubEnv('VITE_POSTHOG_KEY', 'phc_test')
    const initialization = initAnalytics()
    const error = new Error('Cannot read properties of undefined')
    captureReactException(error, { componentStack: '\n    at Funding (/src/Funding.tsx:10:1)' }, 'uncaught')
    await initialization
    expect(posthogMock.captureException).toHaveBeenCalledWith(expect.any(Error), expect.objectContaining({
      error_category: 'uncaught', component_stack: expect.stringContaining('Funding'), build_commit: expect.any(String),
    }))
    expect(posthogMock.captureException.mock.lastCall?.[0]).not.toBe(error)
  })
  it('never throws from exception reporting, including malformed error getters', () => {
    const error = new Error('hidden')
    Object.defineProperty(error, 'message', { get() { throw new Error('getter') } })
    expect(() => captureReactException(error, {}, 'uncaught')).not.toThrow()
  })
  it('omits credentials and signed payloads from replay network metadata', () => {
    const mask = createAnalyticsConfig(0.05, 'sepolia').session_recording?.maskCapturedNetworkRequestFn
    const input = { name: '/api/aa/rpc?token=secret', entryType: 'resource', startTime: 0, duration: 1,
      requestHeaders: { 'X-Plether-AA-Recovery': 'secret' }, responseHeaders: { 'X-Plether-AA-Recovery': 'secret' },
      requestBody: 'signed operation', responseBody: 'credential' }
    const result = mask?.(input)
    expect(result).toEqual({ name: '/api/aa/rpc', entryType: 'resource', startTime: 0, duration: 1 })
  })
  it('keeps exception metadata but drops SDK URL and arbitrary context properties', () => {
    const before = createAnalyticsConfig(0.05, 'sepolia').before_send
    if (typeof before !== 'function') throw new Error('missing exception privacy filter')
    const result = before({ uuid: 'test', event: '$exception', properties: {
      $exception_list: [{ type: 'TypeError', value: 'Cannot read properties of undefined' }],
      $session_id: 'session', build_commit: 'release', component_stack: 'at Funding',
      $current_url: 'https://rpc.example/secret', $referrer: 'private', request: 'signed operation',
    } })
    expect(result?.properties).toEqual({
      $exception_list: [{ type: 'TypeError', value: 'Cannot read properties of undefined' }],
      $session_id: 'session', build_commit: 'release', component_stack: 'at Funding',
    })
  })
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
      manifest_version: 'perps-aa-arbitrum-sepolia-v2',
      exact_amount: '1000',
      arbitrary: 'value',
    })).toEqual({
      surface: 'perps',
      direction: 'long',
      wallet_family: 'MetaMask',
      wallet_version: '12.0.0',
      manifest_version: 'perps-aa-arbitrum-sepolia-v2',
    })
  })

  it('retains bounded preparation diagnostics while dropping raw error context', () => {
    expect(sanitizeAnalyticsProperties({ error_code: 'undecoded_revert', stage: 'commit_simulation',
      contract_function: 'commitOrder', error_message: 'user@example.com', revert_data: '0x12345678',
      wallet_address: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B', rpc_payload: 'signed payload',
    })).toEqual({ error_code: 'undecoded_revert', stage: 'commit_simulation', contract_function: 'commitOrder' })
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

  it('flushes captures queued while the analytics bundle loads', async () => {
    vi.stubEnv('VITE_POSTHOG_KEY', 'phc_test')

    const initialization = initAnalytics()
    captureAnalyticsEvent('perps button clicked', {
      button_id: 'review_trade',
      surface: 'perps',
    })
    await initialization

    expect(posthogMock.init).toHaveBeenCalledOnce()
    expect(posthogMock.capture).toHaveBeenCalledWith('perps button clicked', {
      button_id: 'review_trade',
      surface: 'perps',
    })
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
      timeout_ms: 180_000,
      wallet_address: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
      details: { rpc: 'payload' },
    })).toEqual({
      component: 'perps_trade_ticket',
      operation: 'order_commit',
      outcome: 'failure',
      error_category: 'failed for [redacted]',
      http_status: 503,
      timeout_ms: 180_000,
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

  it('keeps exporter exceptions out of the transaction lifecycle', async () => {
    vi.stubEnv('VITE_POSTHOG_KEY', 'phc_test')
    await initAnalytics()
    posthogMock.capture.mockImplementationOnce(() => { throw new Error('export unavailable') })
    posthogMock.captureLog.mockImplementationOnce(() => { throw new Error('export unavailable') })
    expect(() => captureAnalyticsEvent('perps sponsored operation', { stage: 'submitting' })).not.toThrow()
    expect(() => captureFrontendLog('error', 'Sponsored operation failed', { stage: 'submitting' })).not.toThrow()
  })

  it('adds the Sepolia diagnostic filter only to the explicitly selected profile', async () => {
    vi.stubEnv('VITE_POSTHOG_KEY', 'phc_test')
    vi.stubEnv('VITE_DEPLOYMENT_ENV', 'sepolia')
    await initAnalytics()
    captureAnalyticsEvent('perps sponsored operation', { stage: 'submitting' })
    expect(posthogMock.capture).toHaveBeenCalledWith('perps sponsored operation', { stage: 'submitting', deployment_name: 'sepolia' })
  })
})
