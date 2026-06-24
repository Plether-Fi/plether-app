import {
  captureAnalyticsEvent,
  type AnalyticsProperties,
  type AnalyticsPropertyValue,
} from './client'

export type PerpsDirection = 'long' | 'short'
export type PerpsConnectedState = 'connected' | 'disconnected'
export type PerpsChainState = 'correct_chain' | 'wrong_chain' | 'unknown'
export type PerpsCloseReason = 'backdrop' | 'close_button' | 'escape' | 'state_change'
export type PerpsOrderLifecycleEvent =
  | 'commit_started'
  | 'commit_pending'
  | 'commit_succeeded'
  | 'commit_failed'
  | 'reveal_started'
  | 'reveal_succeeded'
  | 'reveal_failed'
  | 'executed'
  | 'failed'
export type PerpsMarginLifecycleEvent =
  | 'deposit_opened'
  | 'deposit_submitted'
  | 'deposit_succeeded'
  | 'deposit_failed'
  | 'withdraw_opened'
  | 'withdraw_submitted'
  | 'withdraw_succeeded'
  | 'withdraw_failed'

export type PerpsAnalyticsProperties = AnalyticsProperties

function compactProperties(properties: PerpsAnalyticsProperties = {}): PerpsAnalyticsProperties {
  return Object.fromEntries(
    Object.entries(properties).filter(([, value]) => value !== undefined && value !== null)
  ) as PerpsAnalyticsProperties
}

export function trackPerpsPageViewed(properties?: PerpsAnalyticsProperties): void {
  captureAnalyticsEvent('perps page viewed', compactProperties({
    surface: 'perps',
    ...properties,
  }))
}

export function trackPerpsButtonClicked(
  buttonId: string,
  properties?: PerpsAnalyticsProperties
): void {
  captureAnalyticsEvent('perps button clicked', compactProperties({
    surface: 'perps',
    button_id: buttonId,
    ...properties,
  }))
}

export function trackPerpsModalOpened(
  modalId: string,
  properties?: PerpsAnalyticsProperties
): void {
  captureAnalyticsEvent('perps modal opened', compactProperties({
    surface: 'perps',
    modal_id: modalId,
    ...properties,
  }))
}

export function trackPerpsModalClosed(
  modalId: string,
  properties?: PerpsAnalyticsProperties
): void {
  captureAnalyticsEvent('perps modal closed', compactProperties({
    surface: 'perps',
    modal_id: modalId,
    ...properties,
  }))
}

export function trackPerpsOrderLifecycle(
  event: PerpsOrderLifecycleEvent,
  properties?: PerpsAnalyticsProperties
): void {
  captureAnalyticsEvent(`perps order ${event}`, compactProperties({
    surface: 'perps',
    ...properties,
  }))
}

export function trackPerpsMarginLifecycle(
  event: PerpsMarginLifecycleEvent,
  properties?: PerpsAnalyticsProperties
): void {
  captureAnalyticsEvent(`perps margin ${event}`, compactProperties({
    surface: 'perps',
    ...properties,
  }))
}

export function trackPerpsValidationBlocked(
  reason: string,
  properties?: PerpsAnalyticsProperties
): void {
  captureAnalyticsEvent('perps validation blocked', compactProperties({
    surface: 'perps',
    validation_reason: reason,
    ...properties,
  }))
}

export function perpsConnectedState(isConnected: boolean): PerpsConnectedState {
  return isConnected ? 'connected' : 'disconnected'
}

export function perpsChainState(isConnected: boolean, isCorrectChain: boolean): PerpsChainState {
  if (!isConnected) return 'unknown'
  return isCorrectChain ? 'correct_chain' : 'wrong_chain'
}

export function perpsSizeBucket(value: number): AnalyticsPropertyValue {
  if (!Number.isFinite(value) || value <= 0) return 'zero'
  if (value < 100) return 'lt_100'
  if (value < 1_000) return '100_999'
  if (value < 10_000) return '1k_10k'
  if (value < 100_000) return '10k_100k'
  return 'gte_100k'
}

export function perpsErrorCategory(error: unknown): string {
  const message = error instanceof Error ? error.message : String(error)
  const normalized = message.toLowerCase()
  if (normalized.includes('user rejected') || normalized.includes('rejected')) return 'user_rejected'
  if (normalized.includes('network') || normalized.includes('chain')) return 'network_or_chain'
  if (normalized.includes('allowance') || normalized.includes('approve')) return 'allowance'
  if (normalized.includes('insufficient') || normalized.includes('exceeds')) return 'insufficient_balance'
  if (normalized.includes('pyth') || normalized.includes('hermes') || normalized.includes('price')) return 'oracle'
  if (normalized.includes('timeout') || normalized.includes('expired')) return 'timeout_or_expired'
  return 'unknown'
}
