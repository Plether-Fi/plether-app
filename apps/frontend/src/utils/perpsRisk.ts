import { formatUnits } from 'viem'

/** v1.2.3 PositionRiskAccountingLib.buildExactPriceRiskState, source ffe45937.
 * Supply position margin after projecting carry; claims cannot pay carry.
 */
export interface PerpsPriceRiskInputs {
  size: bigint
  side: number
  entryCostUsdcAtoms: bigint
  positionMarginUsdc: bigint
  traderClaimBalanceUsdc: bigint
  capPrice: bigint
  maintenanceMarginBps: bigint
}

export type LiquidationThreshold =
  | { status: 'boundary'; price: bigint }
  | { status: 'out-of-range' }
  | { status: 'unavailable' }

export function calculatePriceRisk(input: PerpsPriceRiskInputs, rawPrice: bigint) {
  const price = rawPrice > input.capPrice ? input.capPrice : rawPrice
  const notionalUsdc = input.size / 10n ** 20n * price
  const pnlUsdc = input.side === 0
    ? input.entryCostUsdcAtoms - notionalUsdc
    : notionalUsdc - input.entryCostUsdcAtoms
  const equityUsdc = input.positionMarginUsdc + input.traderClaimBalanceUsdc + pnlUsdc
  const maintenanceMarginUsdc = notionalUsdc * input.maintenanceMarginBps / 10_000n
  return { pnlUsdc, equityUsdc, notionalUsdc, maintenanceMarginUsdc, liquidatable: equityUsdc <= maintenanceMarginUsdc }
}

export function findLiquidationThreshold(input: PerpsPriceRiskInputs | undefined): LiquidationThreshold {
  if (!input || input.size <= 0n || input.size % (10n ** 20n) !== 0n ||
    (input.side !== 0 && input.side !== 1) || input.capPrice <= 0n ||
    input.maintenanceMarginBps < 0n || input.maintenanceMarginBps > 10_000n ||
    input.entryCostUsdcAtoms < 0n || input.positionMarginUsdc < 0n || input.traderClaimBalanceUsdc < 0n) {
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
export function projectCarry(positionMarginUsdc: bigint, freeUsdc: bigint, carryUsdc: bigint) {
  const fromMargin = carryUsdc < positionMarginUsdc ? carryUsdc : positionMarginUsdc
  const residual = carryUsdc - fromMargin
  const fromFree = residual < freeUsdc ? residual : freeUsdc
  return {
    positionMarginUsdc: positionMarginUsdc - fromMargin,
    freeSettlementUsdc: freeUsdc - fromFree,
    uncoveredCarryUsdc: residual - fromFree,
  }
}
