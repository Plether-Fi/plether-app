import posthog from 'posthog-js'
import type {
  CaptureLogOptions,
  LogAttributes,
  LogSeverityLevel,
  PostHogConfig,
  Properties,
} from 'posthog-js'
import { BUILD_COMMIT } from '../config/buildInfo'

export type AnalyticsPropertyValue = string | number | boolean | null | undefined
export type AnalyticsProperties = Record<string, AnalyticsPropertyValue>

const DEFAULT_POSTHOG_HOST = 'https://eu.i.posthog.com'
const DEFAULT_REPLAY_SAMPLE_RATE = 0.05
const DEFAULT_LOG_SERVICE_NAME = 'plether-web'
const DEFAULT_LOG_ENVIRONMENT = import.meta.env.DEV ? 'development' : 'production'
const MAX_LOG_BODY_LENGTH = 256
const MAX_LOG_ATTRIBUTE_STRING_LENGTH = 128

const ALLOWED_PROPERTY_KEYS = new Set([
  'account_mode',
  'action_kind',
  'button_id',
  'chain_state',
  'close_reason',
  'connected_state',
  'direction',
  'duration_ms',
  'error_category',
  'lifecycle_state',
  'manifest_version',
  'market_phase',
  'modal_id',
  'reason_code',
  'reduce_only',
  'retry_count',
  'size_bucket',
  'sponsorship_accepted',
  'sponsorship_status',
  'surface',
  'terminal_outcome',
  'validation_reason',
  'wallet_family',
  'wallet_version',
])

const ALLOWED_LOG_ATTRIBUTE_KEYS = new Set([
  ...ALLOWED_PROPERTY_KEYS,
  'component',
  'http_status',
  'operation',
  'outcome',
])

const FORBIDDEN_PROPERTY_PATTERNS = [
  /address/i,
  /amount/i,
  /balance/i,
  /email/i,
  /hash/i,
  /order_?id/i,
  /permit/i,
  /raw/i,
  /rpc/i,
  /signature/i,
  /tx/i,
  /wallet.*address/i,
]

const ADDRESS_PATTERN = /^0x[a-fA-F0-9]{40}$/
const TX_HASH_PATTERN = /^0x[a-fA-F0-9]{64}$/
const EMAIL_PATTERN = /^[^\s@]+@[^\s@]+\.[^\s@]+$/
const EMBEDDED_TX_HASH_PATTERN = /0x[a-fA-F0-9]{64}/g
const EMBEDDED_ADDRESS_PATTERN = /0x[a-fA-F0-9]{40}/g
const EMBEDDED_EMAIL_PATTERN = /[^\s@]+@[^\s@]+\.[^\s@]+/g
const POSTHOG_CREDENTIAL_PATTERN = /\bph[ctx]_[A-Za-z0-9_-]+\b/g
const BEARER_TOKEN_PATTERN = /\bBearer\s+\S+/gi

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
  if (
    ADDRESS_PATTERN.test(value) ||
    TX_HASH_PATTERN.test(value) ||
    EMAIL_PATTERN.test(value)
  ) {
    return '[redacted]'
  }
  return value
    .replace(EMBEDDED_TX_HASH_PATTERN, '[redacted]')
    .replace(EMBEDDED_ADDRESS_PATTERN, '[redacted]')
    .replace(EMBEDDED_EMAIL_PATTERN, '[redacted]')
    .replace(POSTHOG_CREDENTIAL_PATTERN, '[redacted]')
    .replace(BEARER_TOKEN_PATTERN, 'Bearer [redacted]')
}

export function sanitizeFrontendLogBody(body: string): string {
  return sanitizeStringValue(body.trim()).slice(0, MAX_LOG_BODY_LENGTH)
}

export function sanitizeFrontendLogAttributes(
  attributes?: Record<string, unknown>
): LogAttributes {
  if (!attributes) return {}

  const sanitized: LogAttributes = {}
  for (const [key, value] of Object.entries(attributes)) {
    if (!ALLOWED_LOG_ATTRIBUTE_KEYS.has(key) || value === undefined || value === null) continue

    if (typeof value === 'string') {
      sanitized[key] = sanitizeStringValue(value).slice(0, MAX_LOG_ATTRIBUTE_STRING_LENGTH)
    } else if (typeof value === 'number') {
      if (Number.isFinite(value)) sanitized[key] = value
    } else if (typeof value === 'boolean') {
      sanitized[key] = value
    }
  }

  return sanitized
}

export function sanitizeFrontendLogRecord(record: CaptureLogOptions): CaptureLogOptions | null {
  const body = sanitizeFrontendLogBody(record.body)
  if (!body) return null

  return {
    ...record,
    body,
    attributes: sanitizeFrontendLogAttributes(record.attributes),
  }
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

export function createAnalyticsConfig(
  replaySampleRate: number,
  logEnvironment: string
): Partial<PostHogConfig> {
  return {
    api_host: envString('VITE_POSTHOG_HOST') ?? DEFAULT_POSTHOG_HOST,
    defaults: '2026-05-30',
    disable_persistence: true,
    person_profiles: 'never',
    capture_pageview: false,
    capture_pageleave: false,
    autocapture: false,
    capture_dead_clicks: false,
    logs: {
      serviceName: DEFAULT_LOG_SERVICE_NAME,
      environment: logEnvironment,
      serviceVersion: BUILD_COMMIT,
      captureConsoleLogs: false,
      maxBufferSize: 100,
      maxLogsPerInterval: 100,
      beforeSend: sanitizeFrontendLogRecord,
    },
    disable_session_recording: true,
    enable_recording_console_log: false,
    mask_all_text: true,
    mask_all_element_attributes: true,
    session_recording: {
      maskAllInputs: true,
      maskTextSelector: '*',
      sampleRate: replaySampleRate,
      maskCapturedNetworkRequestFn: (request) => {
        if (request.name) request.name = request.name.split('?')[0]
        return request
      },
    },
  }
}

export function initAnalytics(): void {
  if (initialized || import.meta.env.MODE === 'test') return

  const token = envString('VITE_POSTHOG_KEY')
  if (!token) return

  const replaySampleRate = parseReplaySampleRate(envString('VITE_POSTHOG_REPLAY_SAMPLE_RATE'))
  const logEnvironment = envString('VITE_DEPLOYMENT_ENV') ?? DEFAULT_LOG_ENVIRONMENT

  posthog.init(token, createAnalyticsConfig(replaySampleRate, logEnvironment))

  initialized = true

  if (replaySampleRate > 0 && Math.random() < replaySampleRate) {
    posthog.startSessionRecording({ sampling: true, linked_flag: true })
  }
}

export function isAnalyticsEnabled(): boolean {
  return initialized
}

export function captureAnalyticsEvent(eventName: string, properties?: AnalyticsProperties): void {
  if (!initialized) return
  posthog.capture(eventName, sanitizeAnalyticsProperties(properties))
}

export function captureFrontendLog(
  level: LogSeverityLevel,
  body: string,
  attributes?: Record<string, unknown>
): void {
  if (!initialized) return

  const record = sanitizeFrontendLogRecord({
    body,
    level,
    attributes: sanitizeFrontendLogAttributes(attributes),
  })
  if (!record) return
  posthog.captureLog(record)
}

export function resetAnalyticsForTests(): void {
  initialized = false
}
