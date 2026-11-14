function percentage(amount: bigint, total: bigint): number {
  return total === 0n ? 0 : Number((amount * 10_000n + total / 2n) / total) / 100
}

export function calculateSeniorDepositUsage({
  pendingSeniorPrincipalAssets,
  pendingJuniorPrincipalAssets,
  seniorHighWaterMarkAssets,
  reservedSeniorDepositAssets,
  maxSeniorExposureAssets,
  maxSeniorShareBps,
}: {
  pendingSeniorPrincipalAssets?: bigint
  pendingJuniorPrincipalAssets?: bigint
  seniorHighWaterMarkAssets?: bigint
  reservedSeniorDepositAssets?: bigint
  maxSeniorExposureAssets?: bigint
  maxSeniorShareBps?: bigint
}) {
  if (pendingSeniorPrincipalAssets === undefined
    || seniorHighWaterMarkAssets === undefined
    || reservedSeniorDepositAssets === undefined) return undefined

  // Admission protects the high-water mark even when current Senior capital is impaired.
  const protectedExposureAssets = pendingSeniorPrincipalAssets > seniorHighWaterMarkAssets
    ? pendingSeniorPrincipalAssets
    : seniorHighWaterMarkAssets
  const committedExposureAssets = protectedExposureAssets + reservedSeniorDepositAssets
  const hasFixedAbsoluteLimit = maxSeniorExposureAssets !== undefined
    && maxSeniorExposureAssets < 2n ** 255n

  return {
    protectedExposureAssets,
    committedExposureAssets,
    absoluteUsagePercent: hasFixedAbsoluteLimit && maxSeniorExposureAssets > 0n
      ? percentage(committedExposureAssets, maxSeniorExposureAssets)
      : undefined,
    absoluteLimitExceeded: maxSeniorExposureAssets !== undefined
      && committedExposureAssets > maxSeniorExposureAssets,
    shareUsagePercent: pendingJuniorPrincipalAssets === undefined || maxSeniorShareBps === undefined
      || maxSeniorShareBps === 0n || maxSeniorShareBps >= 10_000n
      ? undefined
      : percentage(committedExposureAssets * 10_000n,
        (committedExposureAssets + pendingJuniorPrincipalAssets) * maxSeniorShareBps),
    shareLimitExceeded: pendingJuniorPrincipalAssets !== undefined && maxSeniorShareBps !== undefined
      && committedExposureAssets * 10_000n > maxSeniorShareBps * (committedExposureAssets + pendingJuniorPrincipalAssets),
  }
}
