import { notionalUsdcToSizeDelta } from './perps'

export interface ResolvePerpsSizeDeltaInput {
  isReducingCurrentPosition: boolean
  currentPositionSize?: bigint
  notionalUsdc: bigint
  maxNotionalUsdc: bigint
  oraclePrice: bigint
}

export function resolvePerpsSizeDelta({
  isReducingCurrentPosition,
  currentPositionSize,
  notionalUsdc,
  maxNotionalUsdc,
  oraclePrice,
}: ResolvePerpsSizeDeltaInput): bigint {
  if (
    isReducingCurrentPosition &&
    currentPositionSize !== undefined &&
    currentPositionSize > 0n &&
    maxNotionalUsdc > 0n &&
    notionalUsdc >= maxNotionalUsdc
  ) {
    return currentPositionSize
  }

  return notionalUsdcToSizeDelta(notionalUsdc, oraclePrice)
}
