import { isAddress, type Address } from 'viem'

function tupleValue(value: unknown, index: number, name: string): unknown {
  if (!value || typeof value !== 'object') return undefined
  if (name in value) return (value as Record<string, unknown>)[name]
  return Array.isArray(value) ? value[index] : undefined
}

function bigintField(value: unknown, index: number, name: string): bigint | undefined {
  const field = tupleValue(value, index, name)
  return typeof field === 'bigint' ? field : undefined
}

function numberField(value: unknown, index: number, name: string): number | undefined {
  const field = tupleValue(value, index, name)
  return typeof field === 'number' ? field : undefined
}

function booleanField(value: unknown, index: number, name: string): boolean | undefined {
  const field = tupleValue(value, index, name)
  return typeof field === 'boolean' ? field : undefined
}

function addressField(value: unknown, index: number, name: string): Address | undefined {
  const field = tupleValue(value, index, name)
  return typeof field === 'string' && isAddress(field) ? field : undefined
}

function hasEveryField(value: Record<string, unknown>): boolean {
  return Object.values(value).every((field) => field !== undefined)
}

export interface PoolLiquidityView {
  totalAssetsUsdc: bigint
  freeUsdc: bigint
  withdrawalReservedUsdc: bigint
  pendingRecapitalizationUsdc: bigint
  pendingTradingRevenueUsdc: bigint
  seniorPrincipalUsdc: bigint
  juniorPrincipalUsdc: bigint
  seniorHighWaterMarkUsdc: bigint
  currentTerminalDeficitUsdc: bigint
  markFresh: boolean
  oracleFrozen: boolean
  degradedMode: boolean
}

export function decodePoolLiquidityView(value: unknown): PoolLiquidityView | undefined {
  const decoded = {
    totalAssetsUsdc: bigintField(value, 0, 'totalAssetsUsdc'),
    freeUsdc: bigintField(value, 1, 'freeUsdc'),
    withdrawalReservedUsdc: bigintField(value, 2, 'withdrawalReservedUsdc'),
    pendingRecapitalizationUsdc: bigintField(value, 3, 'pendingRecapitalizationUsdc'),
    pendingTradingRevenueUsdc: bigintField(value, 4, 'pendingTradingRevenueUsdc'),
    seniorPrincipalUsdc: bigintField(value, 5, 'seniorPrincipalUsdc'),
    juniorPrincipalUsdc: bigintField(value, 6, 'juniorPrincipalUsdc'),
    seniorHighWaterMarkUsdc: bigintField(value, 7, 'seniorHighWaterMarkUsdc'),
    currentTerminalDeficitUsdc: bigintField(value, 8, 'currentTerminalDeficitUsdc'),
    markFresh: booleanField(value, 9, 'markFresh'),
    oracleFrozen: booleanField(value, 10, 'oracleFrozen'),
    degradedMode: booleanField(value, 11, 'degradedMode'),
  }
  return hasEveryField(decoded) ? decoded as PoolLiquidityView : undefined
}

export interface TrancheView {
  totalAssetsUsdc: bigint
  totalShares: bigint
  effectiveTotalShares: bigint
  pendingMaintenanceFeeShares: bigint
  maintenanceFeeAprBps: bigint
  maintenanceFeeRecipient: Address
  sharePrice: bigint
  maxWithdrawUsdc: bigint
  frozenLpFeeBps: bigint
  depositEnabled: boolean
  withdrawEnabled: boolean
  oracleFrozen: boolean
}

export function decodeTrancheView(value: unknown): TrancheView | undefined {
  const decoded = {
    totalAssetsUsdc: bigintField(value, 0, 'totalAssetsUsdc'),
    totalShares: bigintField(value, 1, 'totalShares'),
    effectiveTotalShares: bigintField(value, 2, 'effectiveTotalShares'),
    pendingMaintenanceFeeShares: bigintField(value, 3, 'pendingMaintenanceFeeShares'),
    maintenanceFeeAprBps: bigintField(value, 4, 'maintenanceFeeAprBps'),
    maintenanceFeeRecipient: addressField(value, 5, 'maintenanceFeeRecipient'),
    sharePrice: bigintField(value, 6, 'sharePrice'),
    maxWithdrawUsdc: bigintField(value, 7, 'maxWithdrawUsdc'),
    frozenLpFeeBps: bigintField(value, 8, 'frozenLpFeeBps'),
    depositEnabled: booleanField(value, 9, 'depositEnabled'),
    withdrawEnabled: booleanField(value, 10, 'withdrawEnabled'),
    oracleFrozen: booleanField(value, 11, 'oracleFrozen'),
  }
  return hasEveryField(decoded) ? decoded as TrancheView : undefined
}

export interface TrancheQueueView {
  vault: Address
  currentEpoch: bigint
  cutoffEpoch: bigint
  nextRequestEpoch: bigint
  nextRequestCutoffTime: bigint
  depositHeadEpoch: bigint
  depositHeadAssets: bigint
  redeemHeadEpoch: bigint
  redeemHeadShares: bigint
  depositBacklog: boolean
  redeemBacklog: boolean
  settlementLive: boolean
  poolPaused: boolean
  lpEpochSettlementPaused: boolean
}

export function decodeTrancheQueueView(value: unknown): TrancheQueueView | undefined {
  const decoded = {
    vault: addressField(value, 0, 'vault'),
    currentEpoch: bigintField(value, 1, 'currentEpoch'),
    cutoffEpoch: bigintField(value, 2, 'cutoffEpoch'),
    nextRequestEpoch: bigintField(value, 3, 'nextRequestEpoch'),
    nextRequestCutoffTime: bigintField(value, 4, 'nextRequestCutoffTime'),
    depositHeadEpoch: bigintField(value, 5, 'depositHeadEpoch'),
    depositHeadAssets: bigintField(value, 6, 'depositHeadAssets'),
    redeemHeadEpoch: bigintField(value, 7, 'redeemHeadEpoch'),
    redeemHeadShares: bigintField(value, 8, 'redeemHeadShares'),
    depositBacklog: booleanField(value, 9, 'depositBacklog'),
    redeemBacklog: booleanField(value, 10, 'redeemBacklog'),
    settlementLive: booleanField(value, 11, 'settlementLive'),
    poolPaused: booleanField(value, 12, 'poolPaused'),
    lpEpochSettlementPaused: booleanField(value, 13, 'lpEpochSettlementPaused'),
  }
  return hasEveryField(decoded) ? decoded as TrancheQueueView : undefined
}

export interface ProtocolStatusView {
  phase: number
  lastMarkPrice: bigint
  lastMarkTime: bigint
  oracleFrozen: boolean
  fadWindow: boolean
  tradingActive: boolean
  withdrawalLive: boolean
  lpEpochSettlementPaused: boolean
}

export function decodeProtocolStatusView(value: unknown): ProtocolStatusView | undefined {
  const decoded = {
    // viem intentionally decodes Solidity integers up to uint48 as numbers.
    phase: numberField(value, 0, 'phase'),
    lastMarkPrice: bigintField(value, 1, 'lastMarkPrice'),
    lastMarkTime: bigintField(value, 2, 'lastMarkTime'),
    oracleFrozen: booleanField(value, 3, 'oracleFrozen'),
    fadWindow: booleanField(value, 4, 'fadWindow'),
    tradingActive: booleanField(value, 5, 'tradingActive'),
    withdrawalLive: booleanField(value, 6, 'withdrawalLive'),
    lpEpochSettlementPaused: booleanField(value, 7, 'lpEpochSettlementPaused'),
  }
  return hasEveryField(decoded) ? decoded as ProtocolStatusView : undefined
}

export interface PendingTrancheState {
  seniorPrincipalUsdc: bigint
  juniorPrincipalUsdc: bigint
  maxSeniorWithdrawUsdc: bigint
  maxJuniorWithdrawUsdc: bigint
}

export function decodePendingTrancheState(value: unknown): PendingTrancheState | undefined {
  const decoded = {
    seniorPrincipalUsdc: bigintField(value, 0, 'seniorPrincipalUsdc'),
    juniorPrincipalUsdc: bigintField(value, 1, 'juniorPrincipalUsdc'),
    maxSeniorWithdrawUsdc: bigintField(value, 2, 'maxSeniorWithdrawUsdc'),
    maxJuniorWithdrawUsdc: bigintField(value, 3, 'maxJuniorWithdrawUsdc'),
  }
  return hasEveryField(decoded) ? decoded as PendingTrancheState : undefined
}
