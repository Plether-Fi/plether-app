// Slippage settings
export const SLIPPAGE_PRESETS = [0.5, 1.0] as const
export const MAX_SLIPPAGE = 1.0 // 1% protocol max
export const DEFAULT_SLIPPAGE = 0.5

// Price impact settings
export const PRICE_IMPACT_PRESETS = [1.0, 2.0, 3.0] as const
export const MAX_PRICE_IMPACT = 5.0 // 5% max allowed
export const DEFAULT_MAX_PRICE_IMPACT = 2.0 // 2% default threshold

// Token decimals
export const USDC_DECIMALS = 6
export const TOKEN_DECIMALS = 18

// Health factor thresholds
export const HEALTH_FACTOR_WARNING = 1.5
export const HEALTH_FACTOR_DANGER = 1.2

// Protocol status
export type ProtocolStatus = 'Active' | 'Paused' | 'Settled' | 'Stale'

// Oracle staleness threshold (seconds)
export const ORACLE_STALE_SECONDS = 86400

// Local storage keys
export const STORAGE_KEYS = {
  SETTINGS: 'plether_settings',
  SLIPPAGE: 'plether_slippage',
  PENDING_TXS: 'plether_pending_txs',
} as const
