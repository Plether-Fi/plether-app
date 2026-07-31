const WAD = 10n ** 18n

export type PerpsDirectionalLimitSide = 'long' | 'short' | 'balanced'

export interface PerpsDirectionalLimitMetrics {
  usagePercent: number
  side: PerpsDirectionalLimitSide
  netExposureUsdc: bigint
  limitUsdc: bigint
}

export function calculatePerpsDirectionalLimit({
  longOpenInterestUsdc,
  shortOpenInterestUsdc,
  poolAssetsUsdc,
  maxSkewRatio,
}: {
  longOpenInterestUsdc: bigint | undefined
  shortOpenInterestUsdc: bigint | undefined
  poolAssetsUsdc: bigint | undefined
  maxSkewRatio: bigint | undefined
}): PerpsDirectionalLimitMetrics | undefined {
  if (
    longOpenInterestUsdc === undefined ||
    shortOpenInterestUsdc === undefined ||
    poolAssetsUsdc === undefined ||
    maxSkewRatio === undefined
  ) {
    return undefined
  }

  const limitUsdc = (poolAssetsUsdc * maxSkewRatio) / WAD
  if (limitUsdc <= 0n) return undefined

  const signedExposureUsdc = longOpenInterestUsdc - shortOpenInterestUsdc
  const netExposureUsdc = signedExposureUsdc < 0n ? -signedExposureUsdc : signedExposureUsdc
  const usageBasisPoints = (netExposureUsdc * 10_000n + limitUsdc / 2n) / limitUsdc

  return {
    usagePercent: Number(usageBasisPoints) / 100,
    side: signedExposureUsdc > 0n ? 'long' : signedExposureUsdc < 0n ? 'short' : 'balanced',
    netExposureUsdc,
    limitUsdc,
  }
}
