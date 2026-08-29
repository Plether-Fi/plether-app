import { formatPerpsUsdc, type PerpsDirection } from './perps'

export function getOpenCapacityUnavailableMessage({
  direction,
  isOpeningFromZero,
  minimumDxyExposureUsdc,
}: {
  direction: PerpsDirection
  isOpeningFromZero: boolean
  minimumDxyExposureUsdc: bigint
}): string {
  const selectedDirectionLabel = direction === 'long' ? 'Long' : 'Short'
  const opposingDirectionLabel = direction === 'long' ? 'Short' : 'Long'
  const minimumLabel = isOpeningFromZero ? 'minimum position size' : 'minimum increase size'
  const alternativeAction = isOpeningFromZero
    ? `You can open a ${opposingDirectionLabel} plDXY Perp position instead, which helps rebalance the market.`
    : `You can reduce or close your current ${selectedDirectionLabel} position. After closing it, you can open a ${opposingDirectionLabel} plDXY Perp position, which helps rebalance the market.`

  return `${selectedDirectionLabel} plDXY Perp positions are temporarily unavailable because there is not enough remaining ${selectedDirectionLabel} capacity to fit the ${minimumLabel} of ${formatPerpsUsdc(minimumDxyExposureUsdc)} USDC. Opening more ${selectedDirectionLabel} exposure would worsen the market imbalance. ${alternativeAction}`
}
