export type PerpsSeniorCapitalStatus = 'at-high-water-mark' | 'not-impaired' | 'impaired'

export interface PerpsPoolCapitalMetrics {
  isEmpty: boolean
  isJuniorExhausted: boolean
  juniorSharePercent: number
  seniorSharePercent: number
  seniorStatus: PerpsSeniorCapitalStatus
  seniorImpairmentAssets: bigint
}

export function calculatePerpsPoolCapital({
  juniorPrincipalAssets,
  seniorPrincipalAssets,
  seniorHighWaterMarkAssets,
}: {
  juniorPrincipalAssets: bigint | undefined
  seniorPrincipalAssets: bigint | undefined
  seniorHighWaterMarkAssets: bigint | undefined
}): PerpsPoolCapitalMetrics | undefined {
  if (
    juniorPrincipalAssets === undefined ||
    seniorPrincipalAssets === undefined ||
    seniorHighWaterMarkAssets === undefined
  ) {
    return undefined
  }

  const totalPrincipalAssets = juniorPrincipalAssets + seniorPrincipalAssets
  const isEmpty = totalPrincipalAssets === 0n
  const juniorShareBasisPoints = isEmpty
    ? 0n
    : (juniorPrincipalAssets * 10_000n + totalPrincipalAssets / 2n) / totalPrincipalAssets
  const juniorSharePercent = Number(juniorShareBasisPoints) / 100
  const seniorImpairmentAssets = seniorHighWaterMarkAssets > seniorPrincipalAssets
    ? seniorHighWaterMarkAssets - seniorPrincipalAssets
    : 0n
  const seniorStatus: PerpsSeniorCapitalStatus = seniorImpairmentAssets > 0n
    ? 'impaired'
    : seniorPrincipalAssets === seniorHighWaterMarkAssets
      ? 'at-high-water-mark'
      : 'not-impaired'

  return {
    isEmpty,
    isJuniorExhausted: juniorPrincipalAssets === 0n,
    juniorSharePercent,
    seniorSharePercent: isEmpty ? 0 : 100 - juniorSharePercent,
    seniorStatus,
    seniorImpairmentAssets,
  }
}
