import type { PreparedPerpsOrderV3 } from '../../contracts/perpsOrderV3'
const account = '0x0000000000000000000000000000000000000001'
export function prepared(seconds = 60, margin = 20_000_000n): PreparedPerpsOrderV3 {
  return {
    account, manifestVersion: 'test', orderRouter: account, orderLifecycleBook: account,
    reviewedBlockNumber: 1n, reviewedBlockHash: '0x12', reviewedPrice: 100_000_000n,
    executionBountyUsdc: 100n,
    protection: { submitBy: BigInt(Math.floor(Date.now() / 1000) + seconds), executionWindowSeconds: 60, executionMode: 1, executionBountyUsdc: 100n },
    request: { clientOrderId: '0x12', side: 0, sizeDelta: 100n, marginDelta: margin, targetPrice: 100_000_000n, isClose: false,
      bounds: { submitBy: BigInt(Math.floor(Date.now() / 1000) + seconds), executionWindowSeconds: 60, allowedExecutionModes: 1, expectedConfigHash: '0x12',
        maxExecutionBountyUsdc: 100n, maxExecutionNotionalUsdc: 100n, maxGrossAccountDebitUsdc: 100n, maxActionChargeUsdc: 100n,
        maxExplicitFeesUsdc: 100n, maxPostPositionSize: 100n, minPostSettlementBalanceUsdc: 0n, minPostPositionEquityUsdc: 0n, maxPostLeverageBps: 50_000 } },
  }
}
