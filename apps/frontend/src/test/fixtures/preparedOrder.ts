import type { PreparedPerpsOrderV3 } from '../../contracts/perpsOrderV3'
const account = '0x0000000000000000000000000000000000000001'
export function prepared(seconds = 60, margin = 20_000_000n): PreparedPerpsOrderV3 {
  return {
    account, manifestVersion: 'test', orderRouter: account, orderLifecycleBook: account,
    reviewedBlockNumber: 1n, reviewedBlockHash: '0x12', reviewedPrice: 100_000_000n,
    executionBountyAssets: 100n,
    protection: { submitBy: BigInt(Math.floor(Date.now() / 1000) + seconds), executionWindowSeconds: 60, executionMode: 1, executionBountyAssets: 100n },
    request: { clientOrderId: '0x12', side: 0, sizeDelta: 100n, marginDelta: margin, targetPrice: 100_000_000n, isClose: false,
      bounds: { submitBy: BigInt(Math.floor(Date.now() / 1000) + seconds), executionWindowSeconds: 60, allowedExecutionModes: 1, expectedConfigHash: '0x12',
        maxExecutionBountyAssets: 100n, maxExecutionNotionalAssets: 100n, maxGrossAccountDebitAssets: 100n, maxActionChargeAssets: 100n,
        maxExplicitFeesAssets: 100n, maxPostPositionSize: 100n, minPostSettlementBalanceAssets: 0n, minPostPositionEquityAssets: 0n, maxPostLeverageBps: 50_000 } },
  }
}
