export type PerpsSeniorCapitalStatus = 'at-high-water-mark' | 'not-impaired' | 'impaired'

export interface PerpsPoolCapitalMetrics {
  isEmpty: boolean
  isJuniorExhausted: boolean
  juniorSharePercent: number
  seniorSharePercent: number
  seniorStatus: PerpsSeniorCapitalStatus
  seniorImpairmentUsdc: bigint
}

export function calculatePerpsPoolCapital({
  juniorPrincipalUsdc,
  seniorPrincipalUsdc,
  seniorHighWaterMarkUsdc,
}: {
  juniorPrincipalUsdc: bigint | undefined
  seniorPrincipalUsdc: bigint | undefined
  seniorHighWaterMarkUsdc: bigint | undefined
}): PerpsPoolCapitalMetrics | undefined {
  if (
    juniorPrincipalUsdc === undefined ||
    seniorPrincipalUsdc === undefined ||
    seniorHighWaterMarkUsdc === undefined
  ) {
    return undefined
  }

  const totalPrincipalUsdc = juniorPrincipalUsdc + seniorPrincipalUsdc
  const isEmpty = totalPrincipalUsdc === 0n
  const juniorShareBasisPoints = isEmpty
    ? 0n
    : (juniorPrincipalUsdc * 10_000n + totalPrincipalUsdc / 2n) / totalPrincipalUsdc
  const juniorSharePercent = Number(juniorShareBasisPoints) / 100
  const seniorImpairmentUsdc = seniorHighWaterMarkUsdc > seniorPrincipalUsdc
    ? seniorHighWaterMarkUsdc - seniorPrincipalUsdc
    : 0n
  const seniorStatus: PerpsSeniorCapitalStatus = seniorImpairmentUsdc > 0n
    ? 'impaired'
    : seniorPrincipalUsdc === seniorHighWaterMarkUsdc
      ? 'at-high-water-mark'
      : 'not-impaired'

  return {
    isEmpty,
    isJuniorExhausted: juniorPrincipalUsdc === 0n,
    juniorSharePercent,
    seniorSharePercent: isEmpty ? 0 : 100 - juniorSharePercent,
    seniorStatus,
    seniorImpairmentUsdc,
  }
}
