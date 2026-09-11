import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address, Hex, PublicClient } from 'viem'
import rawManifest from '../../../public/perps-aa-manifest.json'
import { parsePerpsAaManifest } from '../../perps-aa/manifest'
import type { PerpsExecutionAssessment } from '../perpsOrderV2'
import { PerpsOrderReviewError, preparePerpsOrderV2, reviewPerpsOrderV2 } from '../preparePerpsOrderV2'
import { verifyPerpsV2DeploymentBindings } from '../verifyPerpsV2Bindings'

vi.mock('../verifyPerpsV2Bindings', () => ({
  verifyPerpsV2DeploymentBindings: vi.fn(),
  verifyProtectionDeployment: vi.fn(),
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
      block,
      blockNumber: block.number,
      positionProtectionBook: manifest.positionProtectionBook,
    })
  })

  it('reviews both protection rewards separately and simulates the exact atomic protected open', async () => {
    let exists = false
    let available = 10_000_000_000n
    const params = { takeProfitTriggerPrice: 110_000_000n, stopLossTriggerPrice: 90_000_000n }
    const input = { account, direction: 'short' as const, side: 1 as const, sizeDelta: 50n * 10n ** 20n, marginDelta: 1_000_000_000n, slippagePercent: 0.1, isClose: false, selectedMaxLeverageBps: 50_000, clientOrderId: `0x${'33'.repeat(32)}` as Hex }
    const simulateContract = vi.fn(async () => ({ request: {} }))
    const values: Record<string, unknown> = { maxOrderAge: 60n, currentExecutionConfigHash: configHash, openOrderExecutionBountyBps: 1n, minOpenOrderExecutionBountyUsdc: 10_000n, maxOpenOrderExecutionBountyUsdc: 200_000n, closeOrderExecutionBountyUsdc: 200_000n, lastMarkPrice: 100_000_000n, CAP_PRICE: 200_000_000n, totalAssets: 1_000_000_000_000n, getLatestPrice: 100_000_000n, activePositionProtectionId: 0n, getPendingOrders: [], maxPendingOrders: 8n, positionProtectionTriggerBountyUsdc: 200_000n }
    const client = { getBlock: vi.fn(async () => block), simulateContract, readContract: vi.fn(async ({ functionName, args }: { functionName: string; args?: readonly unknown[] }) => {
      if (functionName === 'getPosition') return { exists }
      if (functionName === 'getFreeBuyingPowerUsdc') return available
      if (functionName === 'assessOrder') return assessment(args?.[3] as bigint, (args?.[1] as { marginDelta: bigint }).marginDelta)
      if (functionName in values) return values[functionName]
      throw new Error(`Unexpected read ${functionName}`)
    }) } as unknown as PublicClient
    const plain = await reviewPerpsOrderV2(client, manifest, input)
    const protectedReview = await reviewPerpsOrderV2(client, manifest, { ...input, positionProtection: params })
    expect(protectedReview.reviewSummary.requiredFundingUsdc - plain.reviewSummary.requiredFundingUsdc).toBe(400_000n)
    expect(protectedReview.preparedOrder.request.bounds).toEqual(plain.preparedOrder.request.bounds)
    const prepared = await preparePerpsOrderV2(client, manifest, { ...input, positionProtection: params })
    expect(simulateContract).toHaveBeenCalledWith(expect.objectContaining({ address: manifest.positionProtectionBook, functionName: 'commitOpenOrderWithProtection', args: [prepared.request, params], blockNumber: block.number }))
    available = protectedReview.reviewSummary.requiredFundingUsdc - 1n
    await expect(preparePerpsOrderV2(client, manifest, { ...input, positionProtection: params })).rejects.toMatchObject({ shortfallUsdc: 1n })
    available = 10_000_000_000n
    exists = true
    await expect(preparePerpsOrderV2(client, manifest, { ...input, positionProtection: params })).rejects.toThrow('no position')
    expect(simulateContract).toHaveBeenCalledTimes(1)
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
    expect(client.getBlock).not.toHaveBeenCalled()
    expect(prepared.reviewedBlockHash).toBe(block.hash)
    expect(prepared.protection.validUntil).toBe(block.timestamp + 60n)
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


describe('reviewed leverage validation', () => {
  const input = {
    account, direction: 'short' as const, side: 1 as const,
    sizeDelta: 50n * 10n ** 20n, marginDelta: 0n,
    slippagePercent: 0.1, isClose: true, selectedMaxLeverageBps: 50_000,
  }

  function reviewClient(overrides: (price: bigint) => Partial<PerpsExecutionAssessment>) {
    const values: Record<string, unknown> = {
      maxOrderAge: 60n, currentExecutionConfigHash: configHash,
      openOrderExecutionBountyBps: 1n, minOpenOrderExecutionBountyUsdc: 10_000n,
      maxOpenOrderExecutionBountyUsdc: 200_000n, closeOrderExecutionBountyUsdc: 200_000n,
      lastMarkPrice: 100_000_000n, CAP_PRICE: 200_000_000n,
      totalAssets: 1_000_000_000_000n, getLatestPrice: 100_000_000n,
      activePositionProtectionId: 0n, getPendingOrders: [], maxPendingOrders: 8n,
      getFreeBuyingPowerUsdc: 10_000_000_000n,
    }
    const readContract = vi.fn(async ({ functionName, args }: { functionName: string; args?: readonly unknown[] }) => {
      if (functionName === 'assessOrder') {
        const price = args?.[3] as bigint
        return { ...assessment(price, 2_000_000_000n), ...overrides(price) }
      }
      if (functionName in values) return values[functionName]
      throw new Error(`Unexpected read ${functionName}`)
    })
    const simulateContract = vi.fn(async () => ({ request: {} }))
    const client = { getBlock: vi.fn(async () => block), readContract, simulateContract } as unknown as PublicClient
    return { client, readContract, simulateContract }
  }

  beforeEach(() => {
    vi.mocked(verifyPerpsV2DeploymentBindings).mockResolvedValue({
      block,
      block, blockNumber: block.number, positionProtectionBook: manifest.positionProtectionBook,
    })
  })

  it('allows a reduction above the opening slider limit without adding margin', async () => {
    const { client, simulateContract } = reviewClient(() => ({ postLeverageBps: 50_300n }))
    const prepared = await preparePerpsOrderV2(client, manifest, input)
    expect(prepared.reviewSummary?.worstPostLeverageBps).toBe(50_300n)
    expect(prepared.request.marginDelta).toBe(0n)
    expect(simulateContract).toHaveBeenCalledWith(expect.objectContaining({
      functionName: 'commitOrder', args: [prepared.request], blockNumber: block.number,
    }))
  })

  it('reviews a full close with no remaining leverage independently of the opening slider', async () => {
    const { client } = reviewClient(() => ({ postPositionSize: 0n, postPositionEquityUsdc: 0n, postLeverageBps: 0n }))
    const prepared = await preparePerpsOrderV2(client, manifest, { ...input, selectedMaxLeverageBps: 0 })
    expect(prepared.reviewSummary?.currentAssessment.postPositionSize).toBe(0n)
    expect(prepared.reviewSummary?.worstPostLeverageBps).toBe(0n)
  })

  it('retains the exact worst reviewed leverage when an opening exceeds its selected limit', async () => {
    const { client, readContract, simulateContract } = reviewClient(price => ({
      postLeverageBps: price === 100_000_000n ? 49_700n : 50_001n,
    }))
    const error = await preparePerpsOrderV2(client, manifest, {
      ...input, isClose: false, marginDelta: 2_000_000_000n,
    }).catch((error: unknown) => error)
    expect(error).toBeInstanceOf(PerpsOrderReviewError)
    expect(error).toMatchObject({
      message: 'Highest reviewed leverage is 5.0001x, above your selected 5x limit.',
      reviewSummary: {
        worstPostLeverageBps: 50_001n, reviewedBlockNumber: block.number,
        reviewedBlockHash: block.hash, currentAssessment: { postLeverageBps: 49_700n },
      },
    })
    for (const [request] of readContract.mock.calls) {
      expect(request).toMatchObject({ blockNumber: block.number })
    }
    expect(simulateContract).not.toHaveBeenCalled()
  })

  it.each([
    ['negative position equity', () => ({ postPositionEquityUsdc: -1n })],
    ['execution regime changed', (price: bigint) => ({ mode: price === 100_000_000n ? 1 : 2 })],
    ['maximum leverage', () => ({ postLeverageBps: 0x1_0000_0000n })],
  ])('still rejects a reduction with %s', async (message, overrides) => {
    const { client, simulateContract } = reviewClient(overrides)
    await expect(preparePerpsOrderV2(client, manifest, input)).rejects.toThrow(message)
    expect(simulateContract).not.toHaveBeenCalled()
  })

  it('still requires the close commit simulation to succeed', async () => {
    const { client, simulateContract } = reviewClient(() => ({ postLeverageBps: 50_300n }))
    simulateContract.mockRejectedValueOnce(new Error('Protocol rejected close'))
    await expect(preparePerpsOrderV2(client, manifest, input)).rejects.toThrow('Protocol rejected close')
  })
})

describe('Max opening review', () => {
  const quantum = 100n * 10n ** 18n
  const input = {
    account, direction: 'short' as const, side: 1,
    sizeDelta: 329n * quantum, marginDelta: 996_969_697n,
    slippagePercent: 0.1, isClose: false, selectedMaxLeverageBps: 330_000,
    maxSize: true,
  }

  function maxClient({ size = 7_900n * quantum, available = 1_000_000_000n, failure,
    leverage = 330_000n, quoteValid = true, assessmentFailure,
  }: {
    size?: bigint; available?: bigint; failure?: unknown; leverage?: bigint;
    quoteValid?: boolean; assessmentFailure?: unknown;
  } = {}) {
    const values: Record<string, unknown> = {
      maxOrderAge: 60n, currentExecutionConfigHash: configHash,
      openOrderExecutionBountyBps: 1n, minOpenOrderExecutionBountyUsdc: 10_000n,
      maxOpenOrderExecutionBountyUsdc: 200_000n, closeOrderExecutionBountyUsdc: 200_000n,
      lastMarkPrice: 100_000_000n, CAP_PRICE: 200_000_000n,
      totalAssets: 1_000_000_000_000n, getLatestPrice: 100_000_000n,
      activePositionProtectionId: 0n, getPendingOrders: [], maxPendingOrders: 8n,
      getFreeBuyingPowerUsdc: available,
      positionProtectionTriggerBountyUsdc: 200_000n, getPosition: { exists: false },
    }
    const readContract = vi.fn(async ({ functionName, args }: { functionName: string; args?: readonly unknown[] }) => {
      if (functionName === 'quoteMaxOpen') {
        if (failure) throw failure
        return { maxSizeDelta: size, preview: { valid: quoteValid, invalidReason: quoteValid ? 0 : 9 }, limitingReason: 9 }
      }
      if (functionName === 'assessOrder') {
        if (assessmentFailure) throw assessmentFailure
        const { sizeDelta, marginDelta } = args?.[1] as { sizeDelta: bigint; marginDelta: bigint }
        return { ...assessment(args?.[3] as bigint, marginDelta),
          postPositionSize: sizeDelta, postLeverageBps: leverage, postPositionEquityUsdc: 999_000_000n,
        } satisfies PerpsExecutionAssessment
      }
      if (functionName in values) return values[functionName]
      throw new Error(`Unexpected read ${functionName}`)
    })
    const simulateContract = vi.fn(async () => ({ request: {} }))
    const client = { getBlock: vi.fn(async () => block), readContract, simulateContract } as unknown as PublicClient
    return { client, readContract, simulateContract }
  }

  beforeEach(() => {
    vi.mocked(verifyPerpsV2DeploymentBindings).mockResolvedValue({
      block, blockNumber: block.number, positionProtectionBook: manifest.positionProtectionBook,
    })
  })

  it('uses the exact lens maximum beyond the earlier ticket size, without probing smaller candidates', async () => {
    const { client, readContract, simulateContract } = maxClient()
    const prepared = await preparePerpsOrderV2(client, manifest, input)
    expect(prepared.request.sizeDelta).toBe(7_900n * quantum)
    expect(prepared.request.marginDelta).toBe(999_800_000n)
    expect(prepared.reviewSummary?.requiredFundingUsdc).toBe(1_000_000_000n)
    const quotes = readContract.mock.calls.filter(([request]) => request.functionName === 'quoteMaxOpen')
    expect(quotes).toHaveLength(1)
    expect(quotes[0][0]).toMatchObject({
      args: [account, 1, 999_800_000n, 100_000_000n, block.timestamp], blockNumber: block.number,
    })
    const assessments = readContract.mock.calls.filter(([request]) => request.functionName === 'assessOrder')
    expect(assessments).toHaveLength(6)
    for (const [request] of assessments) expect(request.args?.[1]).toMatchObject({ sizeDelta: prepared.request.sizeDelta, marginDelta: prepared.request.marginDelta })
    for (const [request] of readContract.mock.calls) expect(request).toMatchObject({ blockNumber: block.number })
    expect(simulateContract).toHaveBeenCalledExactlyOnceWith(expect.objectContaining({ args: [prepared.request], blockNumber: block.number }))
  })

  it('refreshes a smaller lens maximum and reserves both attached protection rewards', async () => {
    const { client, readContract } = maxClient({ size: 200n * quantum })
    const prepared = await preparePerpsOrderV2(client, manifest, {
      ...input, positionProtection: { takeProfitTriggerPrice: 110_000_000n, stopLossTriggerPrice: 90_000_000n },
    })
    expect(prepared.request.sizeDelta).toBe(200n * quantum)
    expect(prepared.request.marginDelta).toBe(999_400_000n)
    expect(prepared.reviewSummary?.requiredFundingUsdc).toBe(1_000_000_000n)
    expect(readContract).toHaveBeenCalledWith(expect.objectContaining({ functionName: 'quoteMaxOpen', args: [account, 1, 999_400_000n, 100_000_000n, block.timestamp] }))
  })

  it('does not quote when the account cannot cover the router reward reserve', async () => {
    const { client, readContract, simulateContract } = maxClient({ available: 200_000n })
    await expect(preparePerpsOrderV2(client, manifest, input)).rejects.toThrow('after execution rewards')
    expect(readContract.mock.calls.some(([request]) => request.functionName === 'quoteMaxOpen')).toBe(false)
    expect(simulateContract).not.toHaveBeenCalled()
  })

  it.each([{ size: 0n, quoteValid: false }, { size: quantum, quoteValid: false }])('rejects zero capacity or an invalid quote: %s', async (options) => {
    const { client, readContract, simulateContract } = maxClient(options)
    await expect(preparePerpsOrderV2(client, manifest, input)).rejects.toThrow()
    expect(readContract.mock.calls.filter(([request]) => request.functionName === 'quoteMaxOpen')).toHaveLength(1)
    expect(readContract.mock.calls.some(([request]) => request.functionName === 'assessOrder')).toBe(false)
    expect(simulateContract).not.toHaveBeenCalled()
  })

  it.each([new Error('RPC connection lost'), { errorName: 'CfdEngineLens__QuoteSearchLimitExceeded' }])('propagates quote failures without falling back to client sizing', async (failure) => {
    const { client, readContract, simulateContract } = maxClient({ failure })
    await expect(preparePerpsOrderV2(client, manifest, input)).rejects.toBe(failure)
    expect(readContract.mock.calls.filter(([request]) => request.functionName === 'quoteMaxOpen')).toHaveLength(1)
    expect(readContract.mock.calls.some(([request]) => request.functionName === 'assessOrder')).toBe(false)
    expect(simulateContract).not.toHaveBeenCalled()
  })

  it('retains the selected leverage limit and fixed quote budget without resizing', async () => {
    const { client, readContract, simulateContract } = maxClient({ leverage: 330_001n })
    await expect(preparePerpsOrderV2(client, manifest, input)).rejects.toMatchObject({ reason: 'leverage' })
    expect(readContract.mock.calls.filter(([request]) => request.functionName === 'quoteMaxOpen')).toHaveLength(1)
    expect(simulateContract).not.toHaveBeenCalled()
  })

  it('retains router rejection without retrying another size', async () => {
    const failure = new Error('Router rejected the quoted size')
    const { client, readContract, simulateContract } = maxClient({ assessmentFailure: failure })
    await expect(preparePerpsOrderV2(client, manifest, input)).rejects.toBe(failure)
    expect(readContract.mock.calls.filter(([request]) => request.functionName === 'quoteMaxOpen')).toHaveLength(1)
    expect(simulateContract).not.toHaveBeenCalled()
  })

  it('does not use the maximum lens for a close or manually entered size', async () => {
    const { client, readContract } = maxClient({ leverage: 50_000n })
    await preparePerpsOrderV2(client, manifest, { ...input, maxSize: undefined })
    await preparePerpsOrderV2(client, manifest, { ...input, isClose: true, marginDelta: 0n })
    expect(readContract.mock.calls.some(([request]) => request.functionName === 'quoteMaxOpen')).toBe(false)
  })

  it('discards a quote delivered after cancellation', async () => {
    const controller = new AbortController()
    const { client, readContract, simulateContract } = maxClient()
    const read = readContract.getMockImplementation()!
    readContract.mockImplementation(async (request) => {
      const result = await read(request)
      if (request.functionName === 'quoteMaxOpen') controller.abort()
      return result
    })
    await expect(preparePerpsOrderV2(client, manifest, { ...input, signal: controller.signal })).rejects.toMatchObject({ name: 'AbortError' })
    expect(readContract.mock.calls.some(([request]) => request.functionName === 'assessOrder')).toBe(false)
    expect(simulateContract).not.toHaveBeenCalled()
  })
})
