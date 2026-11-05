const WAD = 10n ** 18n

export type PerpsDirectionalLimitSide = 'long' | 'short' | 'balanced'

export interface PerpsDirectionalLimitMetrics {
  usagePercent: number
  side: PerpsDirectionalLimitSide
  netExposureAssets: bigint
  limitAssets: bigint
}

export function calculatePerpsDirectionalLimit({
  longOpenInterestAssets,
  shortOpenInterestAssets,
  poolAssets,
  maxSkewRatio,
}: {
  longOpenInterestAssets: bigint | undefined
  shortOpenInterestAssets: bigint | undefined
  poolAssets: bigint | undefined
  maxSkewRatio: bigint | undefined
}): PerpsDirectionalLimitMetrics | undefined {
  if (
    longOpenInterestAssets === undefined ||
    shortOpenInterestAssets === undefined ||
    poolAssets === undefined ||
    maxSkewRatio === undefined
  ) {
    return undefined
  }

  const limitAssets = (poolAssets * maxSkewRatio) / WAD
  if (limitAssets <= 0n) return undefined

  const signedExposureAssets = longOpenInterestAssets - shortOpenInterestAssets
  const netExposureAssets = signedExposureAssets < 0n ? -signedExposureAssets : signedExposureAssets
  const usageBasisPoints = (netExposureAssets * 10_000n + limitAssets / 2n) / limitAssets

  return {
    usagePercent: Number(usageBasisPoints) / 100,
    side: signedExposureAssets > 0n ? 'long' : signedExposureAssets < 0n ? 'short' : 'balanced',
    netExposureAssets,
    limitAssets,
  }
}
