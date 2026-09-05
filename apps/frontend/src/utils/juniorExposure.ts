import { oraclePriceToDisplayDxyPrice, sizeDeltaToNotionalUsdc } from './perps'

export interface JuniorExposureInputs {
  totalAssetsUsdc?: bigint
  freeUsdc?: bigint
  juniorPrincipalUsdc?: bigint
  seniorPrincipalUsdc?: bigint
  seniorRateBps?: bigint
  longOpenInterest?: bigint
  shortOpenInterest?: bigint
  markPrice?: bigint
}

function ratio(numerator: bigint | undefined, denominator: bigint | undefined) {
  return numerator === undefined || denominator === undefined || denominator <= 0n
    ? undefined
    : Number(numerator) / Number(denominator)
}

export function calculateJuniorExposure(pool: JuniorExposureInputs) {
  const junior = pool.juniorPrincipalUsdc
  const senior = pool.seniorPrincipalUsdc
  const capital = junior === undefined || senior === undefined ? undefined : junior + senior
  const displayPrice = oraclePriceToDisplayDxyPrice(pool.markPrice)
  const usablePrice = displayPrice !== undefined && displayPrice > 0n ? displayPrice : undefined
  const long = sizeDeltaToNotionalUsdc(pool.longOpenInterest, usablePrice)
  const short = sizeDeltaToNotionalUsdc(pool.shortOpenInterest, usablePrice)
  const netPositionSize = pool.longOpenInterest === undefined || pool.shortOpenInterest === undefined
    ? undefined : pool.longOpenInterest - pool.shortOpenInterest
  const net = long === undefined || short === undefined ? undefined : long - short
  const gross = long === undefined || short === undefined ? undefined : long + short
  const unavailableCash = pool.totalAssetsUsdc === undefined || pool.freeUsdc === undefined
    ? undefined
    : pool.totalAssetsUsdc > pool.freeUsdc ? pool.totalAssetsUsdc - pool.freeUsdc : 0n
  const seniorToJunior = ratio(senior, junior)

  return {
    long, short, net, netPositionSize,
    juniorShare: ratio(junior, capital),
    capitalMultiple: ratio(capital, junior),
    grossExposureMultiple: ratio(gross, capital),
    // A +1% displayed dollar-index move changes pool PnL by -1% of net exposure.
    juniorLossPercentForOnePercentRise: ratio(net, junior),
    unavailableCashShare: ratio(unavailableCash, pool.totalAssetsUsdc),
    couponDragPercent: seniorToJunior === undefined || pool.seniorRateBps === undefined
      ? undefined : seniorToJunior * Number(pool.seniorRateBps) / 100,
  }
}
