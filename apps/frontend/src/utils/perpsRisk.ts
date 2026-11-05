import { formatUnits } from 'viem'

/** v1.2.3 PositionRiskAccountingLib.buildExactPriceRiskState, source ffe45937.
 * Supply position margin after projecting carry; claims cannot pay carry.
 */
export interface PerpsPriceRiskInputs {
  size: bigint
  side: number
  entryCostAssetAtoms: bigint
  positionMarginAssets: bigint
  traderClaimBalanceAssets: bigint
  capPrice: bigint
  maintenanceMarginBps: bigint
}

export type LiquidationThreshold =
  | { status: 'boundary'; price: bigint }
  | { status: 'out-of-range' }
  | { status: 'unavailable' }

export function calculatePriceRisk(input: PerpsPriceRiskInputs, rawPrice: bigint) {
  const price = rawPrice > input.capPrice ? input.capPrice : rawPrice
  const notionalAssets = input.size / 10n ** 20n * price
  const pnlAssets = input.side === 0
    ? input.entryCostAssetAtoms - notionalAssets
    : notionalAssets - input.entryCostAssetAtoms
  const equityAssets = input.positionMarginAssets + input.traderClaimBalanceAssets + pnlAssets
  const maintenanceMarginAssets = notionalAssets * input.maintenanceMarginBps / 10_000n
  return { pnlAssets, equityAssets, notionalAssets, maintenanceMarginAssets, liquidatable: equityAssets <= maintenanceMarginAssets }
}

export function findLiquidationThreshold(input: PerpsPriceRiskInputs | undefined): LiquidationThreshold {
  if (!input || input.size <= 0n || input.size % (10n ** 20n) !== 0n ||
    (input.side !== 0 && input.side !== 1) || input.capPrice <= 0n ||
    input.maintenanceMarginBps < 0n || input.maintenanceMarginBps > 10_000n ||
    input.entryCostAssetAtoms < 0n || input.positionMarginAssets < 0n || input.traderClaimBalanceAssets < 0n) {
    return { status: 'unavailable' }
  }
  const at = (price: bigint) => calculatePriceRisk(input, price).liquidatable
  const long = input.side === 0
  if (!(long ? at(input.capPrice) : at(0n))) return { status: 'out-of-range' }
  if (long ? at(0n) : at(input.capPrice)) return { status: 'boundary', price: long ? 0n : input.capPrice }
  let low = 0n
  let high = input.capPrice
  while (low < high) {
    const mid = (low + high + (long ? 0n : 1n)) / 2n
    if (long) {
      if (at(mid)) high = mid
      else low = mid + 1n
    } else {
      if (at(mid)) low = mid
      else high = mid - 1n
    }
  }
  return { status: 'boundary', price: low }
}

/** Unlike live-mark formatting, both endpoints are valid liquidation prices. */
export function liquidationDisplayPrice(rawPrice: bigint | undefined, capPrice: bigint): bigint | undefined {
  return rawPrice === undefined || rawPrice < 0n || rawPrice > capPrice ? undefined : capPrice - rawPrice
}

export function formatLiquidationPrice(rawPrice: bigint | undefined, capPrice: bigint, decimals = 4): string {
  const display = liquidationDisplayPrice(rawPrice, capPrice)
  if (display === undefined) return 'Unavailable'
  return Number(formatUnits(display, 8)).toFixed(decimals)
}

/** MarginClearinghouseAccountingLib.projectCarryLoss: active margin, then free funds. */
export function projectCarry(positionMarginAssets: bigint, freeAssets: bigint, carryAssets: bigint) {
  const fromMargin = carryAssets < positionMarginAssets ? carryAssets : positionMarginAssets
  const residual = carryAssets - fromMargin
  const fromFree = residual < freeAssets ? residual : freeAssets
  return {
    positionMarginAssets: positionMarginAssets - fromMargin,
    freeSettlementAssets: freeAssets - fromFree,
    uncoveredCarryAssets: residual - fromFree,
  }
}
