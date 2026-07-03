import type { BasketComponentPrice, BasketLatest } from '../api'

const BASKET_WEIGHT_SCALE = 10_000n
const BPS_SCALE = 10_000n

function parseInteger(value: string | number | undefined): bigint | undefined {
  if (value === undefined) return undefined

  try {
    return BigInt(value)
  } catch {
    return undefined
  }
}

function absolute(value: bigint): bigint {
  return value < 0n ? -value : value
}

function componentBasketContribution(component: BasketComponentPrice): bigint | undefined {
  const price = parseInteger(component.price)
  const basePrice = parseInteger(component.basePrice)
  const weightBps = BigInt(component.weightBps)

  if (
    price === undefined ||
    basePrice === undefined ||
    price <= 0n ||
    basePrice <= 0n ||
    weightBps <= 0n
  ) {
    return undefined
  }

  return (price * weightBps * BASKET_WEIGHT_SCALE) / basePrice
}

function componentConfidenceContribution(component: BasketComponentPrice): bigint | undefined {
  const rawPriceValue = parseInteger(component.rawPrice)
  const confidence = parseInteger(component.confidence)
  const basketContribution = componentBasketContribution(component)

  if (
    rawPriceValue === undefined ||
    confidence === undefined ||
    basketContribution === undefined ||
    confidence < 0n
  ) {
    return undefined
  }

  const rawPrice = absolute(rawPriceValue)
  if (rawPrice <= 0n) return undefined

  return (basketContribution * confidence) / rawPrice
}

export function calculateRawBasketOracleConfidenceSpreadPercent(
  latestBasket: BasketLatest | undefined
): number | undefined {
  const basketPrice = parseInteger(latestBasket?.basketPrice)

  if (
    basketPrice === undefined ||
    basketPrice <= 0n ||
    !latestBasket?.components.length
  ) {
    return undefined
  }

  let rawBasketConfidence = 0n
  for (const component of latestBasket.components) {
    const contribution = componentConfidenceContribution(component)
    if (contribution === undefined) return undefined
    rawBasketConfidence += contribution
  }

  const spreadPercent = (Number(rawBasketConfidence) / Number(basketPrice)) * 100
  return Number.isFinite(spreadPercent) ? spreadPercent : undefined
}

export function calculateAdverseOracleConfidenceSpreadPercent(
  latestBasket: BasketLatest | undefined,
  adverseConfidenceMultiplierBps: string | undefined
): number | undefined {
  const rawSpreadPercent = calculateRawBasketOracleConfidenceSpreadPercent(latestBasket)
  const multiplierBps = parseInteger(adverseConfidenceMultiplierBps)

  if (
    rawSpreadPercent === undefined ||
    multiplierBps === undefined ||
    multiplierBps < 0n
  ) {
    return undefined
  }

  const spreadPercent = (rawSpreadPercent * Number(multiplierBps)) / Number(BPS_SCALE)
  return Number.isFinite(spreadPercent) ? spreadPercent : undefined
}

export function formatOracleConfidenceSpreadPercent(spreadPercent: number | undefined): string | undefined {
  if (spreadPercent === undefined) return undefined
  return `~${spreadPercent.toFixed(4)}%`
}

export function formatAdverseConfidenceMultiplier(
  adverseConfidenceMultiplierBps: string | undefined
): string | undefined {
  const multiplierBps = parseInteger(adverseConfidenceMultiplierBps)
  if (multiplierBps === undefined || multiplierBps < 0n) return undefined

  const multiplier = Number(multiplierBps) / Number(BPS_SCALE)
  if (!Number.isFinite(multiplier)) return undefined

  return `${multiplier.toLocaleString('en-US', {
    maximumFractionDigits: 4,
    minimumFractionDigits: 0,
  })}x (${multiplierBps.toString()} bps)`
}

export function formatAdverseOracleConfidenceSpread(
  latestBasket: BasketLatest | undefined,
  adverseConfidenceMultiplierBps: string | undefined
): string | undefined {
  const spreadPercent = calculateAdverseOracleConfidenceSpreadPercent(
    latestBasket,
    adverseConfidenceMultiplierBps
  )

  return formatOracleConfidenceSpreadPercent(spreadPercent)
}
