import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address, Hex, PublicClient } from 'viem'
import rawManifest from '../../../public/perps-aa-manifest.json'
import { parsePerpsAaManifest } from '../../perps-aa/manifest'
import type { PerpsExecutionAssessment } from '../perpsOrderV2'
import { preparePerpsOrderV2 } from '../preparePerpsOrderV2'
import { verifyPerpsV2DeploymentBindings } from '../verifyPerpsV2Bindings'

vi.mock('../verifyPerpsV2Bindings', () => ({
  verifyPerpsV2DeploymentBindings: vi.fn(),
}))

const manifest = parsePerpsAaManifest(rawManifest)
const account = '0x00000000000000000000000000000000000000A1' as Address
const configHash = `0x${'11'.repeat(32)}` as Hex
const block = {
  number: 302_300_000n,
  timestamp: 2_000_000_000n,
  hash: `0x${'22'.repeat(32)}` as Hex,
}

function assessment(
  price: bigint,
  marginDelta: bigint
): PerpsExecutionAssessment {
  const marginIncrease = marginDelta - 1_000_000_000n
  const requiredEquity = price === 100_100_000n
    ? 1_001_000_000n
    : price === 100_050_000n
      ? 1_000_500_000n
      : 1_000_000_000n
  const postPositionEquityUsdc = 999_500_000n + marginIncrease
  return {
    mode: 1,
    executionNotionalUsdc: 5_000_000_000n,
    grossAccountDebitUsdc: 10_000n,
    actionChargeAssessedUsdc: 0n,
    actionChargeCollectedUsdc: 0n,
    explicitFeesUsdc: 0n,
    preSettlementBalanceUsdc: 2_000_000_000n,
    postSettlementBalanceUsdc: 1_998_490_000n,
    realizedPnlUsdc: 0n,
    vpiUsdc: 0n,
    carryUsdc: 0n,
    executionFeeUsdc: 0n,
    frozenSpreadUsdc: 0n,
    preTraderClaimUsdc: 0n,
    postTraderClaimUsdc: 0n,
    postPositionSize: 50n * 10n ** 20n,
    postPositionMarginUsdc: marginDelta,
    postPositionEquityUsdc,
    postLeverageBps: postPositionEquityUsdc >= requiredEquity
      ? 50_000n
      : 50_001n,
  }
}

describe('preparePerpsOrderV2 leverage margin', () => {
  beforeEach(() => {
    vi.mocked(verifyPerpsV2DeploymentBindings).mockResolvedValue({
      blockNumber: block.number,
      positionProtectionBook: manifest.positionProtectionBook,
    })
  })

  it('reassesses and submits the exact reviewed margin buffer', async () => {
    const assessedMargins: bigint[] = []
    let freeBuyingPowerUsdc = 10_000_000_000n
    const readContract = vi.fn(async (request: {
      functionName: string
      args?: readonly unknown[]
    }) => {
      switch (request.functionName) {
        case 'maxOrderAge': return 60n
        case 'currentExecutionConfigHash': return configHash
        case 'openOrderExecutionBountyBps': return 1n
        case 'minOpenOrderExecutionBountyUsdc': return 10_000n
        case 'maxOpenOrderExecutionBountyUsdc': return 200_000n
        case 'closeOrderExecutionBountyUsdc': return 200_000n
        case 'lastMarkPrice': return 100_000_000n
        case 'CAP_PRICE': return 200_000_000n
        case 'totalAssets': return 1_000_000_000_000n
        case 'getLatestPrice': return 100_000_000n
        case 'activePositionProtectionId': return 0n
        case 'getPendingOrders': return []
        case 'maxPendingOrders': return 8n
        case 'getFreeBuyingPowerUsdc': return freeBuyingPowerUsdc
        case 'assessOrder': {
          const order = request.args?.[1] as { marginDelta: bigint }
          const price = request.args?.[3] as bigint
          assessedMargins.push(order.marginDelta)
          return assessment(price, order.marginDelta)
        }
        default: throw new Error(`Unexpected read ${request.functionName}`)
      }
    })
    const simulateContract = vi.fn(async () => ({ request: {} }))
    const client = {
      getBlock: vi.fn(async () => block),
      readContract,
      simulateContract,
    } as unknown as PublicClient

    const prepared = await preparePerpsOrderV2(client, manifest, {
      account,
      direction: 'short',
      side: 1,
      sizeDelta: 50n * 10n ** 20n,
      marginDelta: 1_000_000_000n,
      slippagePercent: 0.1,
      isClose: false,
      selectedMaxLeverageBps: 50_000,
      clientOrderId: `0x${'33'.repeat(32)}`,
    })

    expect(prepared.request.marginDelta).toBe(1_001_500_000n)
    expect(prepared.request.bounds.maxExecutionNotionalUsdc).toBe(
      (1n << 256n) - 1n
    )
    // Order 12 exceeded its point-in-time assessment by just 0.0001 USDC.
    // The normal web request must not convert that movement into a terminal
    // ExecutionNotional constraint failure.
    expect(prepared.request.bounds.maxExecutionNotionalUsdc).toBeGreaterThan(
      4_937_517_050n
    )
    expect(assessedMargins).toEqual([
      1_000_000_000n,
      1_000_000_000n,
      1_000_000_000n,
      1_001_500_000n,
      1_001_500_000n,
      1_001_500_000n,
      1_001_500_000n,
      1_001_500_000n,
      1_001_500_000n,
    ])
    expect(simulateContract).toHaveBeenCalledWith(expect.objectContaining({
      functionName: 'commitOrder',
      args: [expect.objectContaining({ marginDelta: 1_001_500_000n })],
      blockNumber: block.number,
    }))

    freeBuyingPowerUsdc = 1_000_000_000n
    await expect(preparePerpsOrderV2(client, manifest, {
      account,
      direction: 'short',
      side: 1,
      sizeDelta: 50n * 10n ** 20n,
      marginDelta: 1_000_000_000n,
      slippagePercent: 0.1,
      isClose: false,
      selectedMaxLeverageBps: 50_000,
      clientOrderId: `0x${'44'.repeat(32)}`,
    })).rejects.toMatchObject({
      name: 'PerpsOrderFundingShortfallError',
      shortfallUsdc: 1_700_000n,
    })
    expect(simulateContract).toHaveBeenCalledTimes(1)
  })
})
