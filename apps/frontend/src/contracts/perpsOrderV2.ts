import { bytesToHex, type Address, type Hex } from 'viem'

export const PERPS_CLIENT_ORDER_ID_RESERVED_PREFIX =
  '0x504c455448455221' as const

export const PERPS_LIFECYCLE_STATUS = {
  NONE: 0,
  PENDING: 1,
  EXECUTED: 2,
  FAILED: 3,
} as const

export const PERPS_CLIENT_INTENT_RESOLUTION = {
  UNUSED: 0,
  EXACT_REPLAY: 1,
  CONFLICT: 2,
} as const

export const PERPS_EXECUTION_MODE = {
  NONE: 0,
  LIVE: 1,
  FAD: 2,
  FROZEN: 3,
} as const

export const PERPS_EXECUTION_MODE_MASK = {
  LIVE: 1,
  FAD: 2,
  FROZEN: 4,
  ALL: 7,
} as const

export const PERPS_TERMINAL_REASON = {
  NONE: 0,
  EXECUTED: 1,
  EXPIRED: 2,
  SLIPPAGE: 3,
  CONFIG_MISMATCH: 4,
  EXECUTION_MODE_DISALLOWED: 5,
  RISK_OFF: 6,
  PLANNER_REJECTED: 7,
  CONSTRAINT_VIOLATION: 8,
  ACCOUNT_LIQUIDATED: 9,
} as const

export const PERPS_PENDING_REASON = {
  NONE: 0,
  CLOSE_ONLY: 1,
  SAME_BLOCK: 2,
  MEV_BOUNDARY: 3,
  HISTORICAL_PRICE_UNAVAILABLE: 4,
  INSUFFICIENT_GAS: 5,
  MARK_PRICE_OUT_OF_ORDER: 6,
  ENGINE_FAILURE: 7,
  RECEIPT_FAILURE: 8,
  CLEANUP_LIMIT: 9,
} as const

export const PERPS_FAILED_CONSTRAINT = {
  NONE: 0,
  EXECUTION_BOUNTY: 1,
  EXECUTION_NOTIONAL: 2,
  GROSS_ACCOUNT_DEBIT: 3,
  ACTION_CHARGE: 4,
  EXPLICIT_FEES: 5,
  POST_POSITION_SIZE: 6,
  POST_SETTLEMENT_BALANCE: 7,
  POST_POSITION_EQUITY: 8,
  POST_LEVERAGE: 9,
} as const

export type PerpsLifecycleStatus =
  typeof PERPS_LIFECYCLE_STATUS[keyof typeof PERPS_LIFECYCLE_STATUS]
export type PerpsExecutionMode =
  typeof PERPS_EXECUTION_MODE[keyof typeof PERPS_EXECUTION_MODE]
export type PerpsTerminalReason =
  typeof PERPS_TERMINAL_REASON[keyof typeof PERPS_TERMINAL_REASON]
export type PerpsPendingReason =
  typeof PERPS_PENDING_REASON[keyof typeof PERPS_PENDING_REASON]
export type PerpsFailedConstraint =
  typeof PERPS_FAILED_CONSTRAINT[keyof typeof PERPS_FAILED_CONSTRAINT]

export interface PerpsExecutionBounds {
  validUntil: bigint
  allowedExecutionModes: number
  expectedConfigHash: Hex
  maxExecutionBountyUsdc: bigint
  maxExecutionNotionalUsdc: bigint
  maxGrossAccountDebitUsdc: bigint
  maxActionChargeUsdc: bigint
  maxExplicitFeesUsdc: bigint
  maxPostPositionSize: bigint
  minPostSettlementBalanceUsdc: bigint
  minPostPositionEquityUsdc: bigint
  maxPostLeverageBps: number
}

export interface PerpsOrderRequestV2 {
  clientOrderId: Hex
  side: number
  sizeDelta: bigint
  marginDelta: bigint
  targetPrice: bigint
  isClose: boolean
  bounds: PerpsExecutionBounds
}

export interface PerpsExecutionAssessment {
  mode: PerpsExecutionMode
  executionNotionalUsdc: bigint
  grossAccountDebitUsdc: bigint
  actionChargeAssessedUsdc: bigint
  actionChargeCollectedUsdc: bigint
  explicitFeesUsdc: bigint
  preSettlementBalanceUsdc: bigint
  postSettlementBalanceUsdc: bigint
  realizedPnlUsdc: bigint
  vpiUsdc: bigint
  carryUsdc: bigint
  executionFeeUsdc: bigint
  frozenSpreadUsdc: bigint
  preTraderClaimUsdc: bigint
  postTraderClaimUsdc: bigint
  postPositionSize: bigint
  postPositionMarginUsdc: bigint
  postPositionEquityUsdc: bigint
  postLeverageBps: bigint
}

export interface PerpsExecutionProtectionSummary {
  validUntil: bigint
  executionMode: PerpsExecutionMode
  executionBountyUsdc: bigint
  /** Current-price assessment retained as the display baseline for bounded variances. */
  reviewedGrossAccountDebitUsdc?: bigint
  reviewedActionChargeUsdc?: bigint
  reviewedExplicitFeesUsdc?: bigint
  reviewedPostSettlementBalanceUsdc?: bigint
  reviewedPostPositionEquityUsdc?: bigint
  maxGrossAccountDebitUsdc: bigint
  maxActionChargeUsdc: bigint
  maxExplicitFeesUsdc: bigint
  maxPostLeverageBps: number
  minPostSettlementBalanceUsdc: bigint
  minPostPositionEquityUsdc: bigint
}

export interface PreparedPerpsOrderV2 {
  account: Address
  manifestVersion: string
  orderRouter: Address
  orderLifecycleBook: Address
  request: PerpsOrderRequestV2
  executionBountyUsdc: bigint
  reviewedBlockNumber: bigint
  reviewedBlockHash: Hex
  reviewedPrice: bigint
  protection: PerpsExecutionProtectionSummary
}

/** JSON-safe immutable request persisted before any UserOperation signature. */
export interface PersistedPerpsOrderRequestV2 {
  version: 2
  account: Address
  clientOrderId: Hex
  side: number
  sizeDelta: string
  marginDelta: string
  targetPrice: string
  isClose: boolean
  validUntil: string
  allowedExecutionModes: number
  expectedConfigHash: Hex
  maxExecutionBountyUsdc: string
  maxExecutionNotionalUsdc: string
  maxGrossAccountDebitUsdc: string
  maxActionChargeUsdc: string
  maxExplicitFeesUsdc: string
  maxPostPositionSize: string
  minPostSettlementBalanceUsdc: string
  minPostPositionEquityUsdc: string
  maxPostLeverageBps: number
}

const UINT256_MAX = (1n << 256n) - 1n
const UINT32_MAX = 0xffff_ffff
const ZERO_CLIENT_ORDER_ID = `0x${'0'.repeat(64)}`

export function isPublicPerpsClientOrderId(clientOrderId: Hex): boolean {
  return clientOrderId.toLowerCase() !== ZERO_CLIENT_ORDER_ID &&
    !clientOrderId.toLowerCase().startsWith(
      PERPS_CLIENT_ORDER_ID_RESERVED_PREFIX
    )
}

export function generatePerpsClientOrderId(
  fillRandom: (bytes: Uint8Array) => Uint8Array = (bytes) =>
    globalThis.crypto.getRandomValues(bytes)
): Hex {
  for (let attempt = 0; attempt < 128; attempt += 1) {
    const clientOrderId = bytesToHex(fillRandom(new Uint8Array(32)))
    if (isPublicPerpsClientOrderId(clientOrderId)) return clientOrderId
  }
  throw new Error('Unable to generate a valid public client order ID')
}

export function executionModeMask(mode: PerpsExecutionMode): number {
  if (mode < PERPS_EXECUTION_MODE.LIVE || mode > PERPS_EXECUTION_MODE.FROZEN) {
    throw new Error('The policy evaluator returned an invalid execution mode')
  }
  return 1 << (mode - 1)
}

export function executionModeFromPinnedMask(mask: number): PerpsExecutionMode {
  if (mask === PERPS_EXECUTION_MODE_MASK.LIVE) return PERPS_EXECUTION_MODE.LIVE
  if (mask === PERPS_EXECUTION_MODE_MASK.FAD) return PERPS_EXECUTION_MODE.FAD
  if (mask === PERPS_EXECUTION_MODE_MASK.FROZEN) return PERPS_EXECUTION_MODE.FROZEN
  throw new Error('The persisted order does not pin exactly one execution regime')
}

export function permissivePerpsExecutionBounds(input: {
  validUntil: bigint
  expectedConfigHash: Hex
  executionBountyUsdc: bigint
}): PerpsExecutionBounds {
  return {
    validUntil: input.validUntil,
    allowedExecutionModes: PERPS_EXECUTION_MODE_MASK.ALL,
    expectedConfigHash: input.expectedConfigHash,
    maxExecutionBountyUsdc: input.executionBountyUsdc,
    maxExecutionNotionalUsdc: UINT256_MAX,
    maxGrossAccountDebitUsdc: UINT256_MAX,
    maxActionChargeUsdc: UINT256_MAX,
    maxExplicitFeesUsdc: UINT256_MAX,
    maxPostPositionSize: UINT256_MAX,
    minPostSettlementBalanceUsdc: 0n,
    minPostPositionEquityUsdc: 0n,
    maxPostLeverageBps: UINT32_MAX,
  }
}

function maximum(values: bigint[]): bigint {
  return values.reduce((result, value) => value > result ? value : result, 0n)
}

function minimum(values: bigint[]): bigint {
  if (values.length === 0) throw new Error('No policy assessments were returned')
  return values.slice(1).reduce(
    (result, value) => value < result ? value : result,
    values[0]
  )
}

export function derivePerpsExecutionBounds(input: {
  validUntil: bigint
  expectedConfigHash: Hex
  executionBountyUsdc: bigint
  selectedMaxLeverageBps: number
  assessments: PerpsExecutionAssessment[]
}): PerpsExecutionBounds {
  if (input.assessments.length === 0) {
    throw new Error('Execution protections could not be assessed')
  }
  const mode = input.assessments[0].mode
  if (input.assessments.some((assessment) => assessment.mode !== mode)) {
    throw new Error('The execution regime changed during final review')
  }
  if (input.assessments.some((assessment) => assessment.postPositionEquityUsdc < 0n)) {
    throw new Error('The reviewed order can produce negative position equity')
  }
  const assessedLeverage = maximum(
    input.assessments.map((assessment) => assessment.postLeverageBps)
  )
  if (
    assessedLeverage > BigInt(input.selectedMaxLeverageBps) ||
    assessedLeverage > BigInt(UINT32_MAX)
  ) {
    throw new Error('The reviewed order exceeds the selected maximum leverage')
  }

  return {
    validUntil: input.validUntil,
    allowedExecutionModes: executionModeMask(mode),
    expectedConfigHash: input.expectedConfigHash,
    maxExecutionBountyUsdc: input.executionBountyUsdc,
    maxExecutionNotionalUsdc: maximum(
      input.assessments.map((assessment) => assessment.executionNotionalUsdc)
    ),
    maxGrossAccountDebitUsdc: maximum([
      input.executionBountyUsdc,
      ...input.assessments.map((assessment) => assessment.grossAccountDebitUsdc),
    ]),
    maxActionChargeUsdc: maximum(
      input.assessments.map((assessment) => assessment.actionChargeAssessedUsdc)
    ),
    maxExplicitFeesUsdc: maximum(
      input.assessments.map((assessment) => assessment.explicitFeesUsdc)
    ),
    maxPostPositionSize: maximum(
      input.assessments.map((assessment) => assessment.postPositionSize)
    ),
    minPostSettlementBalanceUsdc: minimum(
      input.assessments.map((assessment) => assessment.postSettlementBalanceUsdc)
    ),
    minPostPositionEquityUsdc: minimum(
      input.assessments.map((assessment) => assessment.postPositionEquityUsdc)
    ),
    maxPostLeverageBps: Number(assessedLeverage),
  }
}

export function persistPerpsOrderRequestV2(
  account: Address,
  request: PerpsOrderRequestV2
): PersistedPerpsOrderRequestV2 {
  return {
    version: 2,
    account,
    clientOrderId: request.clientOrderId,
    side: request.side,
    sizeDelta: request.sizeDelta.toString(),
    marginDelta: request.marginDelta.toString(),
    targetPrice: request.targetPrice.toString(),
    isClose: request.isClose,
    validUntil: request.bounds.validUntil.toString(),
    allowedExecutionModes: request.bounds.allowedExecutionModes,
    expectedConfigHash: request.bounds.expectedConfigHash,
    maxExecutionBountyUsdc: request.bounds.maxExecutionBountyUsdc.toString(),
    maxExecutionNotionalUsdc: request.bounds.maxExecutionNotionalUsdc.toString(),
    maxGrossAccountDebitUsdc: request.bounds.maxGrossAccountDebitUsdc.toString(),
    maxActionChargeUsdc: request.bounds.maxActionChargeUsdc.toString(),
    maxExplicitFeesUsdc: request.bounds.maxExplicitFeesUsdc.toString(),
    maxPostPositionSize: request.bounds.maxPostPositionSize.toString(),
    minPostSettlementBalanceUsdc:
      request.bounds.minPostSettlementBalanceUsdc.toString(),
    minPostPositionEquityUsdc:
      request.bounds.minPostPositionEquityUsdc.toString(),
    maxPostLeverageBps: request.bounds.maxPostLeverageBps,
  }
}

export function restorePerpsOrderRequestV2(
  persisted: PersistedPerpsOrderRequestV2
): PerpsOrderRequestV2 {
  return {
    clientOrderId: persisted.clientOrderId,
    side: persisted.side,
    sizeDelta: BigInt(persisted.sizeDelta),
    marginDelta: BigInt(persisted.marginDelta),
    targetPrice: BigInt(persisted.targetPrice),
    isClose: persisted.isClose,
    bounds: {
      validUntil: BigInt(persisted.validUntil),
      allowedExecutionModes: persisted.allowedExecutionModes,
      expectedConfigHash: persisted.expectedConfigHash,
      maxExecutionBountyUsdc: BigInt(persisted.maxExecutionBountyUsdc),
      maxExecutionNotionalUsdc: BigInt(persisted.maxExecutionNotionalUsdc),
      maxGrossAccountDebitUsdc: BigInt(persisted.maxGrossAccountDebitUsdc),
      maxActionChargeUsdc: BigInt(persisted.maxActionChargeUsdc),
      maxExplicitFeesUsdc: BigInt(persisted.maxExplicitFeesUsdc),
      maxPostPositionSize: BigInt(persisted.maxPostPositionSize),
      minPostSettlementBalanceUsdc: BigInt(
        persisted.minPostSettlementBalanceUsdc
      ),
      minPostPositionEquityUsdc: BigInt(
        persisted.minPostPositionEquityUsdc
      ),
      maxPostLeverageBps: persisted.maxPostLeverageBps,
    },
  }
}

export const PERPS_TERMINAL_REASON_LABELS: Record<number, string> = {
  [PERPS_TERMINAL_REASON.EXPIRED]: 'Expired',
  [PERPS_TERMINAL_REASON.SLIPPAGE]: 'Slippage',
  [PERPS_TERMINAL_REASON.CONFIG_MISMATCH]: 'Config mismatch',
  [PERPS_TERMINAL_REASON.EXECUTION_MODE_DISALLOWED]: 'Mode disallowed',
  [PERPS_TERMINAL_REASON.RISK_OFF]: 'Risk off',
  [PERPS_TERMINAL_REASON.PLANNER_REJECTED]: 'Planner rejected',
  [PERPS_TERMINAL_REASON.CONSTRAINT_VIOLATION]: 'Constraint violation',
  [PERPS_TERMINAL_REASON.ACCOUNT_LIQUIDATED]: 'Account liquidated',
}

export const PERPS_EXECUTION_MODE_LABELS: Record<number, string> = {
  [PERPS_EXECUTION_MODE.LIVE]: 'Live',
  [PERPS_EXECUTION_MODE.FAD]: 'FAD',
  [PERPS_EXECUTION_MODE.FROZEN]: 'Frozen',
}
