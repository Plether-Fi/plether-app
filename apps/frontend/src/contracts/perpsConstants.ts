export const PERPS_SIDE = {
  LONG: 0,
  SHORT: 1,
} as const

export type PerpsSide = typeof PERPS_SIDE[keyof typeof PERPS_SIDE]

export const PERPS_PROTOCOL_PHASE = {
  CONFIGURING: 0,
  ACTIVE: 1,
  DEGRADED: 2,
} as const

export type PerpsProtocolPhase = typeof PERPS_PROTOCOL_PHASE[keyof typeof PERPS_PROTOCOL_PHASE]

export const PERPS_PROTOCOL_PHASE_LABELS = {
  [PERPS_PROTOCOL_PHASE.CONFIGURING]: 'Configuring',
  [PERPS_PROTOCOL_PHASE.ACTIVE]: 'Active',
  [PERPS_PROTOCOL_PHASE.DEGRADED]: 'Degraded',
} as const satisfies Record<PerpsProtocolPhase, string>

export const PERPS_DECIMALS = {
  USDC: 6,
  PRICE: 8,
  POSITION_SIZE: 18,
} as const

export const PERPS_POSITION_SIZE_TO_USDC_SCALE =
  10n ** BigInt(PERPS_DECIMALS.POSITION_SIZE + PERPS_DECIMALS.PRICE - PERPS_DECIMALS.USDC)

// CfdEngine requires every open/increase size delta to use this canonical
// synthetic-token quantum. Close orders intentionally remain exact so an
// account can always fully exit an existing position.
export const PERPS_POSITION_SIZE_QUANTUM =
  100n * 10n ** BigInt(PERPS_DECIMALS.POSITION_SIZE)
