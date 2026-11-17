import {
  PERPS_POSITION_SIZE_QUANTUM,
  PERPS_POSITION_SIZE_TO_ASSETS_SCALE,
} from '../contracts/perpsConstants'

const WAD = 10n ** 18n
const BPS = 10_000n

export type PerpsPoolCapacityConstraint = 'directional' | 'backing'

export interface PerpsPoolCapacity {
  longSize: bigint
  shortSize: bigint
  longLimitingConstraint: PerpsPoolCapacityConstraint
  shortLimitingConstraint: PerpsPoolCapacityConstraint
}

export interface PerpsPoolCapacityInputs {
  rawPrice: bigint | undefined
  capPrice: bigint | undefined
  poolAssets: bigint | undefined
  totalTraderClaimBalanceAssets: bigint | undefined
  effectiveSolvencyAssets: bigint | undefined
  longOpenInterest: bigint | undefined
  shortOpenInterest: bigint | undefined
  longMaxProfitAssets: bigint | undefined
  shortMaxProfitAssets: bigint | undefined
  maxSkewRatio: bigint | undefined
  settlementBufferBps: bigint | undefined
}

/** Estimates additional pool exposure from current backing and directional limits.
 * The two sides are alternative trades against the same snapshot, not additive
 * budgets. Trade-specific VPI, fees, margin and market-state gates are excluded;
 * the canonical order quote remains authoritative for executable size. */
export function calculatePerpsPoolCapacity({
  rawPrice,
  capPrice,
  poolAssets,
  totalTraderClaimBalanceAssets,
  effectiveSolvencyAssets,
  longOpenInterest,
  shortOpenInterest,
  longMaxProfitAssets,
  shortMaxProfitAssets,
  maxSkewRatio,
  settlementBufferBps,
}: PerpsPoolCapacityInputs): PerpsPoolCapacity | undefined {
  if (
    rawPrice === undefined || capPrice === undefined || poolAssets === undefined ||
    totalTraderClaimBalanceAssets === undefined || effectiveSolvencyAssets === undefined ||
    longOpenInterest === undefined || shortOpenInterest === undefined ||
    longMaxProfitAssets === undefined || shortMaxProfitAssets === undefined ||
    maxSkewRatio === undefined || settlementBufferBps === undefined
  ) return undefined

  if (rawPrice <= 0n || capPrice <= 0n || [
    poolAssets, totalTraderClaimBalanceAssets, effectiveSolvencyAssets,
    longOpenInterest, shortOpenInterest, longMaxProfitAssets, shortMaxProfitAssets,
    maxSkewRatio, settlementBufferBps,
  ].some(value => value < 0n)) return undefined

  const netDepthAssets = poolAssets > totalTraderClaimBalanceAssets
    ? poolAssets - totalTraderClaimBalanceAssets : 0n
  // Inverts liability + ceil(liability * buffer / BPS) <= effective assets.
  const maximumLiabilityAssets = effectiveSolvencyAssets * BPS / (BPS + settlementBufferBps)
  if (
    netDepthAssets === 0n || maximumLiabilityAssets < longMaxProfitAssets ||
    maximumLiabilityAssets < shortMaxProfitAssets
  ) return {
    longSize: 0n,
    shortSize: 0n,
    longLimitingConstraint: 'backing',
    shortLimitingConstraint: 'backing',
  }

  const price = rawPrice < capPrice ? rawPrice : capPrice
  const maximumSkewAssets = netDepthAssets * maxSkewRatio / WAD
  const longNotionalAssets = longOpenInterest * price / PERPS_POSITION_SIZE_TO_ASSETS_SCALE
  const shortNotionalAssets = shortOpenInterest * price / PERPS_POSITION_SIZE_TO_ASSETS_SCALE

  function sideCapacity(selectedNotional: bigint, opposingNotional: bigint, liability: bigint, profitPerLot: bigint) {
    const directionalRoom = maximumSkewAssets + opposingNotional - selectedNotional
    // One canonical 100-unit lot has exactly `price` settlement-token atoms of
    // notional and either `price` or `capPrice-price` atoms of maximum profit.
    const directionalLots = directionalRoom > 0n ? directionalRoom / price : 0n
    const backingLots = profitPerLot === 0n
      ? directionalLots : (maximumLiabilityAssets - liability) / profitPerLot
    const limitingConstraint: PerpsPoolCapacityConstraint = profitPerLot > 0n && backingLots <= directionalLots
      ? 'backing' : 'directional'
    return {
      size: (backingLots < directionalLots ? backingLots : directionalLots) * PERPS_POSITION_SIZE_QUANTUM,
      limitingConstraint,
    }
  }

  const long = sideCapacity(longNotionalAssets, shortNotionalAssets, longMaxProfitAssets, price)
  const short = sideCapacity(shortNotionalAssets, longNotionalAssets, shortMaxProfitAssets, capPrice - price)
  return {
    longSize: long.size,
    shortSize: short.size,
    longLimitingConstraint: long.limitingConstraint,
    shortLimitingConstraint: short.limitingConstraint,
  }
}
