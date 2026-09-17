import { afterEach, expect, it, vi } from 'vitest'
import { createAnalyticsConfig } from './client'
import { sanitizedReactException } from './reactErrors'

afterEach(() => { vi.unstubAllGlobals(); vi.restoreAllMocks() })

it('sends an authenticated, sanitized exception through the real SDK transport', async () => {
  // No live project, no network: intercept the final fetch body rather than
  // mocking captureException (which cannot detect before_send regressions).
  const fetchMock = vi.fn().mockResolvedValue(new Response('{}', { status: 200 }))
  vi.stubGlobal('fetch', fetchMock)
  const { PostHog } = await import('posthog-js')
  const sdk = new PostHog()
  sdk.init('phc_test_transport_only', {
    ...createAnalyticsConfig(0, 'sepolia'),
    api_host: 'https://telemetry.invalid', api_transport: 'fetch',
    request_batching: false, disable_compression: true,
    advanced_disable_flags: true, disable_external_dependency_loading: true,
    capture_performance: false,
    opt_out_useragent_filter: true, // Happy DOM is correctly classified as a bot.
  })
  try {
    sdk.register({ private_context: 'do-not-export' })
    const original = new TypeError('Cannot read properties of undefined https://rpc.example/private-key')
    original.stack = `TypeError: ignored\n    at Trading (${location.origin}/assets/trading-test.js:42:7)`
    const safe = sanitizedReactException(original)
    sdk.captureException(safe.error, { support_reference: 'ui-transport-test', build_commit: 'test-build' })
    await vi.waitFor(() => expect(fetchMock).toHaveBeenCalled())
    const wire = fetchMock.mock.calls.map(call => String((call[1] as RequestInit).body)).join('\n')
    expect(wire).toContain('$exception')
    expect(wire).toContain('phc_test_transport_only')
    expect(wire).toContain('ui-transport-test')
    expect(wire).toContain('TypeError')
    expect(wire).toContain('trading-test.js')
    expect(wire).not.toMatch(/private-key|rpc\.example|private_context|do-not-export/)
  } finally {
    sdk.opt_out_capturing()
  }
})
