import type { VaultHistory, VaultHistoryPoint } from '../../api'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../../contracts/perpsAddresses'

/** Illustrative hourly data, anchored to today so the rolling chart stays populated. */
export function createVaultAssetHistory(): VaultHistory {
  const end = Math.floor((Date.now() / 1000 - 120) / 3600) * 3600
  const start = end - 7 * 24 * 3600
  const series = (tranche: 'senior' | 'junior'): VaultHistoryPoint[] => (
    Array.from({ length: 169 }, (_, index) => {
      const progress = index / 168
      const wave = Math.sin(progress * Math.PI * 6) * Math.sin(progress * Math.PI)
      const total = tranche === 'senior'
        ? 56_000_000 + progress * 14_000_000 + wave * 1_200_000
        : 38_000_000 + progress * 12_000_000 + wave * 1_800_000
      const locked = tranche === 'senior'
        ? Math.max(0, (1 - progress) * 9_000_000 + wave * 2_000_000)
        : 12_000_000 + progress * (23_333_333.333334 - 12_000_000) + wave * 2_500_000
      return {
        timestamp: start + index * 3600,
        blockNumber: String(307_400_000 + index * 14_400),
        markFresh: true,
        sharePrice: '1000000000000000000',
        totalAssets: String(Math.round(total * 1e6)),
        lockedAssets: String(Math.round(locked * 1e6)),
        totalSupply: String(Math.round(total * 1e9)),
      }
    })
  )
  return {
    range: '7d',
    intervalSeconds: 3600,
    deployment: {
      chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
      housePool: PERPS_ARBITRUM_SEPOLIA.housePool,
      seniorVault: PERPS_ARBITRUM_SEPOLIA.seniorVault,
      juniorVault: PERPS_ARBITRUM_SEPOLIA.juniorVault,
    },
    coverage: { start, end, complete: true },
    senior: { apy7d: null, return7d: null, points: series('senior') },
    junior: { apy7d: null, return7d: null, points: series('junior') },
  }
}
