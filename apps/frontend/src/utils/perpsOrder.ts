import { notionalAssetsToQuantizedSizeDelta } from './perps'

export interface ResolvePerpsSizeDeltaInput {
  isReducingCurrentPosition: boolean
  currentPositionSize?: bigint
  notionalAssets: bigint
  maxNotionalAssets: bigint
  oraclePrice: bigint
}

export function resolvePerpsSizeDelta({
  isReducingCurrentPosition,
  currentPositionSize,
  notionalAssets,
  maxNotionalAssets,
  oraclePrice,
}: ResolvePerpsSizeDeltaInput): bigint {
  if (
    isReducingCurrentPosition &&
    currentPositionSize !== undefined &&
    currentPositionSize > 0n &&
    maxNotionalAssets > 0n &&
    notionalAssets >= maxNotionalAssets
  ) {
    return currentPositionSize
  }

  return notionalAssetsToQuantizedSizeDelta(notionalAssets, oraclePrice)
}
