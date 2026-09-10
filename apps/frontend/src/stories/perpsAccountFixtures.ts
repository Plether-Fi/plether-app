import type { PerpsPosition } from '../hooks/usePerpsAccount'
import type { PerpsIdentityContextValue } from '../perps-aa'
import { calculatePriceRisk, findLiquidationThreshold, type PerpsPriceRiskInputs } from '../utils/perpsRisk'

export const accountStoryIdentity: PerpsIdentityContextValue = {
  status: 'ready', ownerAddress: '0x9B2F4e0E78E36D97f91c80D5B1aED422d3C2e741',
  accountAddress: '0x62A9c44fAbC68B6dE62059E827cE972bD09E6c18', chainId: 421614,
  isAaManifestConfigured: false, sponsorshipEnabled: false, manifest: null,
  identity: null, proposedIdentity: null, changedIdentityFields: [], error: null,
  confirmIdentityAfterContinuityCheck: () => false, reloadIdentity: () => undefined,
}

export function accountPositionFixture(overrides: Partial<PerpsPriceRiskInputs> = {}, mark = 98_300_000n): PerpsPosition {
  const input = { size: 2_000n * 10n ** 18n, side: 0, entryCostUsdcAtoms: 2_014_250_000n,
    positionMarginUsdc: 400_000_000n, traderClaimBalanceUsdc: 0n, capPrice: 200_000_000n, maintenanceMarginBps: 10n, ...overrides }
  const risk = calculatePriceRisk(input, mark)
  const threshold = findLiquidationThreshold(input)
  return { exists: true, side: input.side, direction: input.side === 0 ? 'long' : 'short', size: input.size,
    entryPrice: input.entryCostUsdcAtoms / (input.size / 10n ** 20n), marginUsdc: input.positionMarginUsdc,
    unrealizedPnlUsdc: risk.pnlUsdc, maintenanceMarginUsdc: risk.maintenanceMarginUsdc, liquidatable: risk.liquidatable,
    estimatedNotionalUsdc: risk.notionalUsdc, entryNotionalUsdc: input.entryCostUsdcAtoms,
    dxyExposureUsdc: input.size / 10n ** 20n * (input.capPrice - mark), displayDxyPrice: input.capPrice - mark,
    liquidationThreshold: threshold, liquidationPrice: threshold.status === 'boundary' ? threshold.price : undefined,
    capPrice: input.capPrice, riskStatus: 'ready', positionEquityUsdc: risk.equityUsdc, pendingCarryUsdc: 1_250_000n }
}
