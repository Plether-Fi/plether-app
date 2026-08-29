import { describe, expect, it } from 'vitest'
import { PERPS_HOUSE_POOL_ABI, PERPS_PUBLIC_LENS_ABI } from './abis/Perps'
import { TRANCHE_VAULT_READ_ABI } from './abis/TrancheVault'
import {
  decodePoolLiquidityView,
  decodeProtocolStatusView,
  decodeTrancheQueueView,
  decodeTrancheView,
} from './vaultViewAdapters'

function outputComponents(abi: readonly unknown[], functionName: string) {
  const entry = abi.find((candidate) => {
    const item = candidate as { type?: string; name?: string }
    return item.type === 'function' && item.name === functionName
  }) as { outputs?: readonly { components?: readonly { name: string; type: string }[] }[] }
  return entry.outputs?.[0]?.components?.map(({ name, type }) => [name, type])
}

describe('v1.2.0 vault ABI regression boundary', () => {
  it('keeps TrancheView in the exact released order', () => {
    const expected = [
      ['totalAssetsUsdc', 'uint256'],
      ['totalShares', 'uint256'],
      ['effectiveTotalShares', 'uint256'],
      ['pendingMaintenanceFeeShares', 'uint256'],
      ['maintenanceFeeAprBps', 'uint256'],
      ['maintenanceFeeRecipient', 'address'],
      ['sharePrice', 'uint256'],
      ['maxWithdrawUsdc', 'uint256'],
      ['frozenLpFeeBps', 'uint256'],
      ['depositEnabled', 'bool'],
      ['withdrawEnabled', 'bool'],
      ['oracleFrozen', 'bool'],
    ]
    expect(outputComponents(PERPS_PUBLIC_LENS_ABI, 'getSeniorTranche')).toEqual(expected)
    expect(outputComponents(PERPS_PUBLIC_LENS_ABI, 'getJuniorTranche')).toEqual(expected)
  })

  it('keeps queue, LP status, and protocol status settlement holds appended', () => {
    expect(outputComponents(PERPS_PUBLIC_LENS_ABI, 'getTrancheQueues')).toEqual([
      ['vault', 'address'],
      ['currentEpoch', 'uint256'],
      ['cutoffEpoch', 'uint256'],
      ['nextRequestEpoch', 'uint256'],
      ['nextRequestCutoffTime', 'uint256'],
      ['depositHeadEpoch', 'uint256'],
      ['depositHeadAssets', 'uint256'],
      ['redeemHeadEpoch', 'uint256'],
      ['redeemHeadShares', 'uint256'],
      ['depositBacklog', 'bool'],
      ['redeemBacklog', 'bool'],
      ['settlementLive', 'bool'],
      ['poolPaused', 'bool'],
      ['lpEpochSettlementPaused', 'bool'],
    ])
    expect(outputComponents(PERPS_PUBLIC_LENS_ABI, 'getLpStatus')?.at(-1)).toEqual([
      'lpEpochSettlementPaused',
      'bool',
    ])
    expect(outputComponents(PERPS_PUBLIC_LENS_ABI, 'getProtocolStatus')?.at(-1)).toEqual([
      'lpEpochSettlementPaused',
      'bool',
    ])
  })

  it('keeps request-state and liquidity tuples exact and exposes vault estimates and cooldowns', () => {
    expect(outputComponents(PERPS_PUBLIC_LENS_ABI, 'getLpRequestState')).toEqual([
      ['vault', 'address'],
      ['requestId', 'uint256'],
      ['controller', 'address'],
      ['pendingDepositAssets', 'uint256'],
      ['pendingDepositSharesEstimate', 'uint256'],
      ['claimableDepositAssets', 'uint256'],
      ['claimableDepositShares', 'uint256'],
      ['pendingRedeemShares', 'uint256'],
      ['pendingRedeemAssetsEstimate', 'uint256'],
      ['claimableRedeemShares', 'uint256'],
      ['claimableRedeemAssets', 'uint256'],
      ['refundableDepositAssets', 'uint256'],
      ['refundableRedeemShares', 'uint256'],
      ['redeemRefundPending', 'bool'],
    ])
    expect(outputComponents(PERPS_HOUSE_POOL_ABI, 'getPoolLiquidityView')).toEqual([
      ['totalAssetsUsdc', 'uint256'],
      ['freeUsdc', 'uint256'],
      ['withdrawalReservedUsdc', 'uint256'],
      ['pendingRecapitalizationUsdc', 'uint256'],
      ['pendingTradingRevenueUsdc', 'uint256'],
      ['seniorPrincipalUsdc', 'uint256'],
      ['juniorPrincipalUsdc', 'uint256'],
      ['seniorHighWaterMarkUsdc', 'uint256'],
      ['currentTerminalDeficitUsdc', 'uint256'],
      ['markFresh', 'bool'],
      ['oracleFrozen', 'bool'],
      ['degradedMode', 'bool'],
    ])
    expect(TRANCHE_VAULT_READ_ABI.some((entry) => entry.name === 'estimateMintAssets')).toBe(true)
    expect(TRANCHE_VAULT_READ_ABI.some((entry) => entry.name === 'DEPOSIT_COOLDOWN')).toBe(true)
    expect(TRANCHE_VAULT_READ_ABI.some((entry) => entry.name === 'lastDepositTime')).toBe(true)
  })
})

describe('typed vault tuple adapters', () => {
  it('decodes exact v1.2.0 positional tuples', () => {
    const tranche = decodeTrancheView([
      1n, 2n, 3n, 4n, 500n,
      '0x0000000000000000000000000000000000000001',
      6n, 7n, 8n, true, false, true,
    ])
    expect(tranche?.effectiveTotalShares).toBe(3n)
    expect(tranche?.maintenanceFeeRecipient).toBe('0x0000000000000000000000000000000000000001')

    const queue = decodeTrancheQueueView([
      '0x0000000000000000000000000000000000000002',
      1n, 2n, 3n, 4n, 5n, 6n, 7n, 8n, false, true, false, true, true,
    ])
    expect(queue?.lpEpochSettlementPaused).toBe(true)

    const status = decodeProtocolStatusView([0, 1n, 2n, false, false, true, false, true])
    expect(status?.lpEpochSettlementPaused).toBe(true)

    const liquidity = decodePoolLiquidityView([
      1n, 2n, 3n, 4n, 5n, 6n, 7n, 8n, 9n, true, false, false,
    ])
    expect(liquidity?.currentTerminalDeficitUsdc).toBe(9n)
  })

  it('fails closed instead of shifting a legacy tuple', () => {
    expect(decodeTrancheView([1n, 2n, 3n, 4n, 5n, true, true, false])).toBeUndefined()
    expect(decodePoolLiquidityView([1n, 2n, 3n, 4n, 5n, 6n, 7n, 8n, true, false, false])).toBeUndefined()
  })
})
