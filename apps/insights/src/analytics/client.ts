import posthog from 'posthog-js'
import type { BeforeSendFn, PostHogConfig, Properties } from 'posthog-js'

export type AnalyticsPropertyValue = string | number | boolean | null | undefined
export type AnalyticsProperties = Record<string, AnalyticsPropertyValue>

const DEFAULT_POSTHOG_HOST = 'https://eu.i.posthog.com'
const DEFAULT_REPLAY_SAMPLE_RATE = 0.05

const ALLOWED_PROPERTY_KEYS = new Set([
  'destination',
  'page',
  'search_kind',
  'source',
  'surface',
])

const FORBIDDEN_PROPERTY_PATTERNS = [
  /address/i,
  /alias/i,
  /amount/i,
  /balance/i,
  /email/i,
  /equity/i,
  /hash/i,
  /name/i,
  /pnl/i,
  /query/i,
  /search_term/i,
  /tx/i,
  /volume/i,
  /wallet/i,
]

const EMBEDDED_TX_HASH_PATTERN = /0x[a-fA-F0-9]{64}/g
const EMBEDDED_ADDRESS_PATTERN = /0x[a-fA-F0-9]{40}/g
const EMBEDDED_EMAIL_PATTERN = /[^\s@]+@[^\s@]+\.[^\s@]+/g
const POSTHOG_CREDENTIAL_PATTERN = /\bph[ctx]_[A-Za-z0-9_-]+\b/g
const BEARER_TOKEN_PATTERN = /\bBearer\s+\S+/gi
const URL_PROPERTY_KEYS = new Set([
  '$current_url',
  '$initial_current_url',
  '$pathname',
])
const DROPPED_REFERRER_PROPERTY_KEYS = new Set([
  '$initial_referrer',
  '$initial_referring_domain',
  '$referrer',
  '$referring_domain',
])

let initialized = false

function envString(name: string): string | undefined {
  const env = import.meta.env as Record<string, unknown>
  const value = env[name]
  return typeof value === 'string' && value.trim() !== '' ? value.trim() : undefined
}

function parseReplaySampleRate(value: string | undefined): number {
  if (value === undefined) return DEFAULT_REPLAY_SAMPLE_RATE
  const parsed = Number(value)
  if (!Number.isFinite(parsed)) return DEFAULT_REPLAY_SAMPLE_RATE
  return Math.min(1, Math.max(0, parsed))
}

function shouldDropProperty(key: string): boolean {
  if (!ALLOWED_PROPERTY_KEYS.has(key)) return true
  return FORBIDDEN_PROPERTY_PATTERNS.some((pattern) => pattern.test(key))
}

function sanitizeStringValue(value: string): string {
  return value
    .replace(EMBEDDED_TX_HASH_PATTERN, '[redacted]')
    .replace(EMBEDDED_ADDRESS_PATTERN, '[redacted]')
    .replace(EMBEDDED_EMAIL_PATTERN, '[redacted]')
    .replace(POSTHOG_CREDENTIAL_PATTERN, '[redacted]')
    .replace(BEARER_TOKEN_PATTERN, 'Bearer [redacted]')
}

export function sanitizeAnalyticsUrl(value: string): string {
  return sanitizeStringValue(value.split(/[?#]/, 1)[0] ?? '')
}

export function sanitizeAnalyticsProperties(properties?: AnalyticsProperties): Properties {
  if (!properties) return {}

  const sanitized: Properties = {}
  for (const [key, value] of Object.entries(properties)) {
    if (shouldDropProperty(key) || value === undefined || value === null) continue

    if (typeof value === 'string') {
      sanitized[key] = sanitizeStringValue(value)
    } else if (typeof value === 'number') {
      if (Number.isFinite(value)) sanitized[key] = value
    } else if (typeof value === 'boolean') {
      sanitized[key] = value
    }
  }

  return sanitized
}

export const sanitizeCapturedEvent: BeforeSendFn = (event) => {
  if (!event?.properties) return event

  const properties: Properties = {}
  for (const [key, value] of Object.entries(event.properties)) {
    const safeValue: unknown = value
    if (DROPPED_REFERRER_PROPERTY_KEYS.has(key)) continue

    if (URL_PROPERTY_KEYS.has(key) && typeof safeValue === 'string') {
      properties[key] = sanitizeAnalyticsUrl(safeValue)
    } else if (key !== 'token' && typeof safeValue === 'string') {
      properties[key] = sanitizeStringValue(safeValue)
    } else {
      properties[key] = safeValue
    }
  }

  return {
    ...event,
    properties,
    $set: undefined,
    $set_once: undefined,
  }
}

export function createAnalyticsConfig(replaySampleRate: number): Partial<PostHogConfig> {
  return {
    api_host: envString('VITE_POSTHOG_HOST') ?? DEFAULT_POSTHOG_HOST,
    defaults: '2026-05-30',
    disable_persistence: true,
    person_profiles: 'never',
    capture_pageview: false,
    capture_pageleave: false,
    autocapture: false,
    capture_dead_clicks: false,
    capture_exceptions: false,
    capture_heatmaps: false,
    capture_performance: false,
    before_send: sanitizeCapturedEvent,
    get_current_url: sanitizeAnalyticsUrl,
    disable_session_recording: true,
    enable_recording_console_log: false,
    mask_all_text: true,
    mask_all_element_attributes: true,
    session_recording: {
      maskAllInputs: true,
      maskTextSelector: '*',
      sampleRate: replaySampleRate,
      maskCapturedNetworkRequestFn: (request) => {
        if (request.name) request.name = sanitizeAnalyticsUrl(request.name)
        return request
      },
    },
  }
}

export function initAnalytics(): void {
  if (initialized || import.meta.env.MODE === 'test') return

  const token = envString('VITE_POSTHOG_KEY')
  if (!token) return

  const replaySampleRate = parseReplaySampleRate(
    envString('VITE_POSTHOG_REPLAY_SAMPLE_RATE'),
  )
  posthog.init(token, createAnalyticsConfig(replaySampleRate))
  initialized = true

  if (replaySampleRate > 0 && Math.random() < replaySampleRate) {
    posthog.startSessionRecording({ sampling: true, linked_flag: true })
  }
}

export function captureAnalyticsEvent(
  eventName: string,
  properties?: AnalyticsProperties,
): void {
  if (!initialized) return
  posthog.capture(eventName, {
    ...sanitizeAnalyticsProperties(properties),
    $geoip_disable: true,
  })
}

export function resetAnalyticsForTests(): void {
  initialized = false
}
