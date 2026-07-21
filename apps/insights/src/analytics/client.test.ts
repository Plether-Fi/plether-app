import { beforeEach, describe, expect, it, vi } from 'vitest'
import {
  captureAnalyticsEvent,
  createAnalyticsConfig,
  resetAnalyticsForTests,
  sanitizeAnalyticsProperties,
  sanitizeAnalyticsUrl,
  sanitizeCapturedEvent,
} from './client'

const posthogMock = vi.hoisted(() => ({
  capture: vi.fn(),
  init: vi.fn(),
  startSessionRecording: vi.fn(),
}))

vi.mock('posthog-js', () => ({
  default: posthogMock,
}))

describe('Insights analytics client', () => {
  beforeEach(() => {
    vi.unstubAllEnvs()
    resetAnalyticsForTests()
    vi.clearAllMocks()
  })

  it('keeps only coarse allow-listed properties', () => {
    expect(sanitizeAnalyticsProperties({
      destination: 'plether_app',
      page: 'wallet',
      search_kind: 'wallet_address',
      source: 'leaderboard',
      surface: 'insights',
      wallet_address: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
      pnl: '123',
      arbitrary: 'value',
    })).toEqual({
      destination: 'plether_app',
      page: 'wallet',
      search_kind: 'wallet_address',
      source: 'leaderboard',
      surface: 'insights',
    })
  })

  it('removes URL queries and redacts wallet addresses in routes', () => {
    expect(sanitizeAnalyticsUrl(
      'https://insights.plether.com/competitions/testnet/wallets/0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B?source=user@example.com',
    )).toBe(
      'https://insights.plether.com/competitions/testnet/wallets/[redacted]',
    )
  })

  it('sanitizes SDK URL properties without modifying the required project token', () => {
    const event = sanitizeCapturedEvent({
      event: 'insights page viewed',
      properties: {
        token: 'phc_public_project_token',
        $current_url: 'https://insights.plether.com/wallets/0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B?ref=private',
        $pathname: '/wallets/0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
        $referrer: 'https://example.com/private-path',
        surface: 'insights',
      },
      $set: { email: 'user@example.com' },
    })

    expect(event?.properties).toEqual({
      token: 'phc_public_project_token',
      $current_url: 'https://insights.plether.com/wallets/[redacted]',
      $pathname: '/wallets/[redacted]',
      surface: 'insights',
    })
    expect(event?.$set).toBeUndefined()
  })

  it('disables automatic collection and fully masks sampled replays', () => {
    expect(createAnalyticsConfig(0.05)).toEqual(expect.objectContaining({
      api_host: 'https://eu.i.posthog.com',
      autocapture: false,
      capture_pageview: false,
      capture_pageleave: false,
      capture_exceptions: false,
      disable_persistence: true,
      person_profiles: 'never',
      before_send: sanitizeCapturedEvent,
      mask_all_text: true,
      mask_all_element_attributes: true,
      session_recording: expect.objectContaining({
        maskAllInputs: true,
        maskTextSelector: '*',
        sampleRate: 0.05,
      }),
    }))
  })

  it('does not capture when analytics is disabled', () => {
    captureAnalyticsEvent('insights page viewed', {
      page: 'leaderboard',
      surface: 'insights',
    })

    expect(posthogMock.capture).not.toHaveBeenCalled()
  })
})
