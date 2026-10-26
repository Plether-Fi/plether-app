vi.mock('../perpsAddresses', async () => (await import('../../test/perpsReleaseFixture')).fixturePerpsAddresses)
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { type Address, type Hex, type PublicClient } from 'viem'
import rawManifest from './fixtures/perps-canonical-aa.json'
import { parsePerpsAaManifest } from '../../perps-aa/manifest'
import type { PerpsClosePreview, PerpsExecutionAssessment } from '../perpsOrderV3'
import { PerpsOrderReviewError, preparePerpsOrderV3, reviewPerpsOrderV3 } from '../preparePerpsOrderV3'
import { verifyClosePreviewDeployment, verifyPerpsV3DeploymentBindings } from '../verifyPerpsV3Bindings'
import { getPreparationFailureProperties } from '../../utils/perpsPreparationDiagnostics'
import { relaxedProtectionCloseLimits } from '../positionProtection'

vi.mock('../verifyPerpsV3Bindings', () => ({
  verifyPerpsV3DeploymentBindings: vi.fn(),
  verifyClosePreviewDeployment: vi.fn(async () => '0x4a1d87466d272be9ebb643cd386883d94f0eb59b'),
  verifyProtectionDeployment: vi.fn(),
}))

const manifest = parsePerpsAaManifest({ ...rawManifest, orderInterfaceVersion: 3 })
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
  const postPositionEquityAssets = 999_500_000n + marginIncrease
  return {
    mode: 1,
    executionNotionalAssets: 5_000_000_000n,
    grossAccountDebitAssets: 10_000n,
    actionChargeAssessedAssets: 0n,
    actionChargeCollectedAssets: 0n,
    explicitFeesAssets: 0n,
    preSettlementBalanceAssets: 2_000_000_000n,
    postSettlementBalanceAssets: 1_998_490_000n,
    realizedPnlAssets: 0n,
    vpiAssets: 0n,
    carryAssets: 0n,
    executionFeeAssets: 0n,
    frozenSpreadAssets: 0n,
    preTraderClaimAssets: 0n,
    postTraderClaimAssets: 0n,
    postPositionSize: 50n * 10n ** 20n,
    postPositionMarginAssets: marginDelta,
    postPositionEquityAssets,
    postLeverageBps: postPositionEquityAssets >= requiredEquity
      ? 50_000n
      : 50_001n,
  }
}

function closePreview(result: PerpsExecutionAssessment, carry = 123_456n): PerpsClosePreview {
  return {
    replayOrderId: 0n,
    commitmentCarryAssets: carry,
    commitment: { carryCollectedAssets: carry, carryOutstandingAssets: 0n, bountyFromFreeAssets: 0n,
      settlementBeforeAssets: 2_000_000_000n, settlementAfterAssets: 2_000_000_000n, freeSettlementAfterAssets: 2_000_000_000n },
    assessment: result,
  }
}

describe('preparePerpsOrderV3 leverage margin', () => {
  beforeEach(() => {
    vi.mocked(verifyPerpsV3DeploymentBindings).mockResolvedValue({
      block,
      blockNumber: block.number,
      positionProtectionBook: manifest.positionProtectionBook,
    })
  })

  it('reviews the protection trigger reward separately and simulates the exact atomic protected open', async () => {
    let exists = false
    let available = 10_000_000_000n
    const params = { takeProfitTriggerPrice: 110_000_000n, stopLossTriggerPrice: 90_000_000n, closeLimits: relaxedProtectionCloseLimits() }
    const input = { account, direction: 'short' as const, side: 1 as const, sizeDelta: 50n * 10n ** 20n, marginDelta: 1_000_000_000n, slippagePercent: 0.1, isClose: false, selectedMaxLeverageBps: 50_000, clientOrderId: `0x${'33'.repeat(32)}` as Hex }
    const simulateContract = vi.fn(async () => ({ request: {} }))
    const values: Record<string, unknown> = { maxExecutionWindowSeconds: 60n, currentExecutionConfigHash: configHash, openOrderExecutionBountyBps: 1n, minOpenOrderExecutionBountyAssets: 10_000n, maxOpenOrderExecutionBountyAssets: 200_000n, closeOrderExecutionBountyAssets: 200_000n, lastMarkPrice: 100_000_000n, CAP_PRICE: 200_000_000n, totalAssets: 1_000_000_000_000n, getLatestPrice: 100_000_000n, activePositionProtectionId: 0n, getPendingOrders: [], maxPendingOrders: 8n, positionProtectionTriggerBountyAssets: 200_000n }
    const client = { getBlock: vi.fn(async () => block), simulateContract, readContract: vi.fn(async ({ functionName, args }: { functionName: string; args?: readonly unknown[] }) => {
      if (functionName === 'getPosition') return { exists }
      if (functionName === 'getFreeBuyingPowerAssets') return available
      if (functionName === 'assessOrder') return assessment(args?.[3] as bigint, (args?.[1] as { marginDelta: bigint }).marginDelta)
      if (functionName in values) return values[functionName]
      throw new Error(`Unexpected read ${functionName}`)
    }) } as unknown as PublicClient
    const plain = await reviewPerpsOrderV3(client, manifest, input)
    const protectedReview = await reviewPerpsOrderV3(client, manifest, { ...input, positionProtection: params })
    expect(protectedReview.reviewSummary.requiredFundingAssets - plain.reviewSummary.requiredFundingAssets).toBe(200_000n)
    expect(protectedReview.preparedOrder.request.bounds).toEqual(plain.preparedOrder.request.bounds)
    const prepared = await preparePerpsOrderV3(client, manifest, { ...input, positionProtection: params })
    expect(simulateContract).toHaveBeenCalledWith(expect.objectContaining({ address: manifest.positionProtectionBook, functionName: 'commitOpenOrderWithProtection', args: [prepared.request, params], blockNumber: block.number }))
    available = protectedReview.reviewSummary.requiredFundingAssets - 1n
    await expect(preparePerpsOrderV3(client, manifest, { ...input, positionProtection: params })).rejects.toMatchObject({ shortfallAssets: 1n })
    available = 10_000_000_000n
    exists = true
    await expect(preparePerpsOrderV3(client, manifest, { ...input, positionProtection: params })).rejects.toThrow('no position')
    expect(simulateContract).toHaveBeenCalledTimes(1)
  })

  it('reassesses and submits the exact reviewed margin buffer', async () => {
    const assessedMargins: bigint[] = []
    let freeBuyingPowerAssets = 10_000_000_000n
    const readContract = vi.fn(async (request: {
      functionName: string
      args?: readonly unknown[]
    }) => {
      switch (request.functionName) {
        case 'maxExecutionWindowSeconds': return 60n
        case 'currentExecutionConfigHash': return configHash
        case 'openOrderExecutionBountyBps': return 1n
        case 'minOpenOrderExecutionBountyAssets': return 10_000n
        case 'maxOpenOrderExecutionBountyAssets': return 200_000n
        case 'closeOrderExecutionBountyAssets': return 200_000n
        case 'lastMarkPrice': return 100_000_000n
        case 'CAP_PRICE': return 200_000_000n
        case 'totalAssets': return 1_000_000_000_000n
        case 'getLatestPrice': return 100_000_000n
        case 'activePositionProtectionId': return 0n
        case 'getPendingOrders': return []
        case 'maxPendingOrders': return 8n
        case 'getFreeBuyingPowerAssets': return freeBuyingPowerAssets
        case 'getPosition': return { exists: false, side: 0 }
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

    const prepared = await preparePerpsOrderV3(client, manifest, {
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
    expect(prepared.protection.submitBy).toBe(block.timestamp + 120n)
    expect(prepared.request.bounds.executionWindowSeconds).toBe(60)
    expect(prepared.request.bounds.maxExecutionNotionalAssets).toBe(
      (1n << 256n) - 1n
    )
    // Order 12 exceeded its point-in-time assessment by just 0.0001 settlement token.
    // The normal web request must not convert that movement into a terminal
    // ExecutionNotional constraint failure.
    expect(prepared.request.bounds.maxExecutionNotionalAssets).toBeGreaterThan(
      4_937_517_050n
    )
    for (const [request] of readContract.mock.calls.filter(([request]) => request.functionName === 'assessOrder')) {
      expect(request.args).toHaveLength(7)
      expect(request.args?.[5]).toMatchObject({ expectedConfigHash: configHash })
      expect(typeof request.args?.[6]).toBe('bigint')
    }
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

    freeBuyingPowerAssets = 1_000_000_000n
    await expect(preparePerpsOrderV3(client, manifest, {
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
      shortfallAssets: 1_702_000n,
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
      maxExecutionWindowSeconds: 60n, currentExecutionConfigHash: configHash,
      openOrderExecutionBountyBps: 1n, minOpenOrderExecutionBountyAssets: 10_000n,
      maxOpenOrderExecutionBountyAssets: 200_000n, closeOrderExecutionBountyAssets: 200_000n,
      lastMarkPrice: 100_000_000n, CAP_PRICE: 200_000_000n,
      totalAssets: 1_000_000_000_000n, getLatestPrice: 100_000_000n,
      activePositionProtectionId: 0n, getPendingOrders: [], maxPendingOrders: 8n,
      getFreeBuyingPowerAssets: 10_000_000_000n,
      getPosition: { exists: false, side: 0 },
    }
    const readContract = vi.fn(async ({ functionName, args }: { functionName: string; args?: readonly unknown[] }) => {
      if (functionName === 'assessOrder' || functionName === 'previewClose') {
        const price = args?.[functionName === 'previewClose' ? 4 : 3] as bigint
        const result = { ...assessment(price, 2_000_000_000n), ...overrides(price) }
        return functionName === 'previewClose' ? closePreview(result) : result
      }
      if (functionName in values) return values[functionName]
      throw new Error(`Unexpected read ${functionName}`)
    })
    const simulateContract = vi.fn(async () => ({ request: {} }))
    const client = { getBlock: vi.fn(async () => block), readContract, simulateContract } as unknown as PublicClient
    return { client, readContract, simulateContract }
  }

  beforeEach(() => {
    vi.mocked(verifyPerpsV3DeploymentBindings).mockResolvedValue({
      block, blockNumber: block.number, positionProtectionBook: manifest.positionProtectionBook,
    })
  })

  it.each([
    ['getLatestPrice', 'context_read'],
    ['previewClose', 'order_assessment'],
    ['commitOrder', 'commit_simulation'],
  ])('identifies the failing %s call without changing the thrown error', async (functionName, stage) => {
    const { client, readContract, simulateContract } = reviewClient(() => ({}))
    const failure = new Error('execution reverted with private request parameters')
    const originalRead = readContract.getMockImplementation()!
    readContract.mockImplementation(async request => {
      if (request.functionName === functionName) throw failure
      return originalRead(request)
    })
    if (functionName === 'commitOrder') simulateContract.mockRejectedValueOnce(failure)
    const error = await preparePerpsOrderV3(client, manifest, input).catch((error: unknown) => error)
    expect(error).toBe(failure)
    expect(getPreparationFailureProperties(new Error('Normalized review error', { cause: error }))).toEqual({
      error_code: 'undecoded_revert', stage, contract_function: functionName,
      ...(functionName === 'previewClose' ? { assessment_point: 'current' } : {}),
    })
  })

  it('separates deployment verification failures from review validation', async () => {
    const { client, readContract } = reviewClient(() => ({}))
    const failure = new Error('HTTP request failed at a private endpoint')
    vi.mocked(verifyPerpsV3DeploymentBindings).mockRejectedValueOnce(failure)
    const error = await preparePerpsOrderV3(client, manifest, input).catch((error: unknown) => error)
    expect(error).toBe(failure)
    expect(readContract).not.toHaveBeenCalled()
    expect(getPreparationFailureProperties(error)).toEqual({ error_code: 'network_failure', stage: 'deployment_verification' })
  })

  it('uses request-taking close previews and keeps commitment carry outside execution bounds', async () => {
    const { client, readContract, simulateContract } = reviewClient(() => ({ postPositionSize: 0n, postPositionEquityAssets: 0n, postLeverageBps: 0n }))
    const prepared = await preparePerpsOrderV3(client, manifest, input)
    const calls = readContract.mock.calls.map(([request]) => request).filter(request => request.functionName === 'previewClose')
    expect(calls).toHaveLength(6)
    expect(new Set(calls.map(call => call.args?.[4])).size).toBe(3)
    for (const call of calls) {
      expect(call).toMatchObject({ address: '0x4a1d87466d272be9ebb643cd386883d94f0eb59b', blockNumber: block.number })
      expect(call.args).toHaveLength(5)
      expect(call.args?.[0]).toBe(manifest.cfdEngine)
      expect(call.args?.[1]).toBe(account)
      expect(call.args?.[2]).toMatchObject({ marginDelta: 0n, isClose: true })
      expect(call.args?.[3]).toBe(manifest.orderRouter)
    }
    expect(readContract.mock.calls.some(([request]) => request.functionName === 'assessOrder')).toBe(false)
    expect(prepared.reviewSummary).toMatchObject({ commitmentCarryAssets: 123_456n, executionBountyAssets: 0n, requiredFundingAssets: 0n })
    expect(prepared.request.bounds.maxExecutionBountyAssets).toBe(0n)
    expect(prepared.request.bounds.maxGrossAccountDebitAssets).toBe((1n << 256n) - 1n)
    expect(simulateContract).toHaveBeenCalledOnce()
  })

  it('keeps commitment carry out of the free-settlement funding gate', async () => {
    const { client, readContract } = reviewClient(() => ({ postPositionSize: 0n, postPositionEquityAssets: 0n, postLeverageBps: 0n }))
    const original = readContract.getMockImplementation()!
    readContract.mockImplementation(async request => {
      if (request.functionName === 'getFreeBuyingPowerAssets') return 200_000n
      const result = await original(request)
      return request.functionName === 'previewClose' ? { ...(result as object), commitmentCarryAssets: 1_250_000n } : result
    })
    // The lens has already paid carry from position margin. Adding it to the
    // local funding gate would incorrectly block this zero-bounty close.
    const prepared = await preparePerpsOrderV3(client, manifest, input)
    expect(prepared.reviewSummary).toMatchObject({ commitmentCarryAssets: 1_250_000n, availableFundingAssets: 200_000n, requiredFundingAssets: 0n })
  })

  it('keeps frozen-mode bits, signed economics and deferred claims in the nested assessment', async () => {
    const { client } = reviewClient(() => ({ mode: 3, realizedPnlAssets: -1_000_000n, vpiAssets: -50_000n, postTraderClaimAssets: 7_000_000n, postPositionSize: 0n, postPositionEquityAssets: 0n, postLeverageBps: 0n }))
    const prepared = await preparePerpsOrderV3(client, manifest, input)
    expect(prepared.request.bounds.allowedExecutionModes).toBe(4)
    expect(prepared.reviewSummary?.currentAssessment).toMatchObject({ mode: 3, realizedPnlAssets: -1_000_000n, vpiAssets: -50_000n, postTraderClaimAssets: 7_000_000n })
  })

  it('does not let a missing close lens block opens', async () => {
    const { client } = reviewClient(() => ({ postLeverageBps: 0n }))
    vi.mocked(verifyClosePreviewDeployment).mockRejectedValueOnce(new Error('Close lens unavailable'))
    await expect(preparePerpsOrderV3(client, manifest, { ...input, isClose: false, marginDelta: 2_000_000_000n })).resolves.toBeDefined()
    await expect(preparePerpsOrderV3(client, manifest, input)).rejects.toThrow('Close lens unavailable')
  })

  it('rejects inconsistent commitment carry across close samples without fallback', async () => {
    const { client, readContract, simulateContract } = reviewClient(() => ({}))
    const original = readContract.getMockImplementation()!
    let samples = 0
    readContract.mockImplementation(async request => {
      const result = await original(request)
      if (request.functionName === 'previewClose' && ++samples === 2) return { ...(result as object), commitmentCarryAssets: 1n }
      return result
    })
    await expect(preparePerpsOrderV3(client, manifest, input)).rejects.toThrow('Close review changed')
    expect(simulateContract).not.toHaveBeenCalled()
    expect(readContract.mock.calls.some(([request]) => request.functionName === 'assessOrder')).toBe(false)
  })

  it('does not interpret an exact replay as a fresh zero-valued close quote', async () => {
    const { client, readContract, simulateContract } = reviewClient(() => ({}))
    const original = readContract.getMockImplementation()!
    readContract.mockImplementation(async request => request.functionName === 'previewClose'
      ? { ...closePreview(assessment(100_000_000n, 2_000_000_000n)), replayOrderId: 42n, assessment: { mode: 0 } }
      : original(request))
    await expect(preparePerpsOrderV3(client, manifest, input)).rejects.toThrow('Close order #42 already exists')
    expect(simulateContract).not.toHaveBeenCalled()
  })

  it('allows an ordinary full close with zero free settlement and no subsidy batch', async () => {
    const { client, readContract, simulateContract } = reviewClient(() => ({ postPositionSize: 0n, postPositionEquityAssets: 0n, postLeverageBps: 0n }))
    const original = readContract.getMockImplementation()!
    readContract.mockImplementation(async call => call.functionName === 'getFreeBuyingPowerAssets' ? 0n : original(call))
    const prepared = await preparePerpsOrderV3(client, manifest, input)
    expect(prepared.sponsoredClose).toBeUndefined()
    expect(prepared.reviewSummary.requiredFundingAssets).toBe(0n)
    expect(simulateContract).toHaveBeenCalledExactlyOnceWith(expect.objectContaining({ functionName: 'commitOrder', args: [prepared.request] }))
  })

  it('allows a reduction above the opening slider limit without adding margin', async () => {
    const { client, simulateContract } = reviewClient(() => ({ postLeverageBps: 50_300n }))
    const prepared = await preparePerpsOrderV3(client, manifest, input)
    expect(prepared.reviewSummary?.worstPostLeverageBps).toBe(50_300n)
    expect(prepared.request.marginDelta).toBe(0n)
    expect(simulateContract).toHaveBeenCalledWith(expect.objectContaining({
      functionName: 'commitOrder', args: [prepared.request], blockNumber: block.number,
    }))
  })

  it('reviews a full close with no remaining leverage independently of the opening slider', async () => {
    const { client } = reviewClient(() => ({ postPositionSize: 0n, postPositionEquityAssets: 0n, postLeverageBps: 0n }))
    const prepared = await preparePerpsOrderV3(client, manifest, { ...input, selectedMaxLeverageBps: 0 })
    expect(prepared.reviewSummary?.currentAssessment.postPositionSize).toBe(0n)
    expect(prepared.reviewSummary?.worstPostLeverageBps).toBe(0n)
  })

  it('retains the exact worst reviewed leverage when an opening exceeds its selected limit', async () => {
    const { client, readContract, simulateContract } = reviewClient(price => ({
      postLeverageBps: price === 100_000_000n ? 49_700n : 50_001n,
    }))
    const error = await preparePerpsOrderV3(client, manifest, {
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
    ['negative position equity', () => ({ postPositionEquityAssets: -1n })],
    ['execution regime changed', (price: bigint) => ({ mode: price === 100_000_000n ? 1 : 2 })],
    ['maximum leverage', () => ({ postLeverageBps: 0x1_0000_0000n })],
  ])('still rejects a reduction with %s', async (message, overrides) => {
    const { client, simulateContract } = reviewClient(overrides)
    await expect(preparePerpsOrderV3(client, manifest, input)).rejects.toThrow(message)
    expect(simulateContract).not.toHaveBeenCalled()
  })

  it('still requires the close commit simulation to succeed', async () => {
    const { client, simulateContract } = reviewClient(() => ({ postLeverageBps: 50_300n }))
    simulateContract.mockRejectedValueOnce(new Error('Protocol rejected close'))
    await expect(preparePerpsOrderV3(client, manifest, input)).rejects.toThrow('Protocol rejected close')
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
    leverage = 330_000n, quoteValid = true, assessmentFailure, position = { exists: false, side: 0 },
  }: {
    size?: bigint; available?: bigint; failure?: unknown; leverage?: bigint | ((size: bigint, price: bigint) => bigint);
    quoteValid?: boolean; assessmentFailure?: unknown; position?: { exists: boolean; side: number };
  } = {}) {
    const values: Record<string, unknown> = {
      maxExecutionWindowSeconds: 60n, currentExecutionConfigHash: configHash,
      openOrderExecutionBountyBps: 1n, minOpenOrderExecutionBountyAssets: 10_000n,
      maxOpenOrderExecutionBountyAssets: 200_000n, closeOrderExecutionBountyAssets: 200_000n,
      lastMarkPrice: 100_000_000n, CAP_PRICE: 200_000_000n,
      totalAssets: 1_000_000_000_000n, getLatestPrice: 100_000_000n,
      activePositionProtectionId: 0n, getPendingOrders: [], maxPendingOrders: 8n,
      getFreeBuyingPowerAssets: available,
      positionProtectionTriggerBountyAssets: 200_000n, getPosition: position,
    }
    const readContract = vi.fn(async ({ functionName, args }: { functionName: string; args?: readonly unknown[] }) => {
      if (functionName === 'quoteMaxOpen' || functionName === 'previewOpen') {
        if (failure) throw failure
        const previewSize = functionName === 'quoteMaxOpen' ? size : args?.[2] as bigint
        const notionalBps = previewSize * 100_000_000n / 10n ** 20n * 10_000n
        const preview = { valid: quoteValid, invalidReason: quoteValid ? 0 : 9, postSize: previewSize,
          postEquityAssets: typeof leverage === 'function' ? 999_000_000n : (notionalBps + leverage - 1n) / leverage }
        return functionName === 'previewOpen' ? preview : { maxSizeDelta: size, preview, limitingReason: 9 }
      }
      if (functionName === 'previewClose') return closePreview(assessment(args?.[4] as bigint, 2_000_000_000n), 0n)
      if (functionName === 'assessOrder') {
        if (assessmentFailure) throw assessmentFailure
        const { sizeDelta, marginDelta } = args?.[1] as { sizeDelta: bigint; marginDelta: bigint }
        return { ...assessment(args?.[3] as bigint, marginDelta),
          postPositionSize: sizeDelta,
          postLeverageBps: typeof leverage === 'function' ? leverage(sizeDelta, args?.[3] as bigint) : leverage,
          postPositionEquityAssets: 999_000_000n,
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
    vi.mocked(verifyPerpsV3DeploymentBindings).mockResolvedValue({
      block, blockNumber: block.number, positionProtectionBook: manifest.positionProtectionBook,
    })
  })

  it.each([false, true])('rejects a stale opposite-direction open before quoting or simulation (max=%s)', async maxSize => {
    const { client, readContract, simulateContract } = maxClient({ position: { exists: true, side: 0 } })
    const error = await preparePerpsOrderV3(client, manifest, { ...input, maxSize }).catch((cause: unknown) => cause)
    expect(error).toMatchObject({ name: 'PerpsOrderPositionConflictError', message: expect.stringMatching(/opposing/i) })
    expect(getPreparationFailureProperties(error)).toEqual({ stage: 'preflight', contract_function: 'getPosition', error_code: 'MUST_CLOSE_OPPOSING' })
    expect(readContract).toHaveBeenCalledWith(expect.objectContaining({ functionName: 'getPosition', args: [account], blockNumber: block.number }))
    expect(readContract.mock.calls.some(([request]) => ['quoteMaxOpen', 'assessOrder'].includes(request.functionName))).toBe(false)
    expect(simulateContract).not.toHaveBeenCalled()
  })

  it('allows increasing an existing same-direction position', async () => {
    const { client, simulateContract } = maxClient({ position: { exists: true, side: 1 } })
    const prepared = await preparePerpsOrderV3(client, manifest, input)
    expect(prepared.request.side).toBe(1)
    expect(prepared.request.isClose).toBe(false)
    expect(simulateContract).toHaveBeenCalledOnce()
  })

  it('uses the exact lens maximum beyond the earlier ticket size, without probing smaller candidates', async () => {
    const { client, readContract, simulateContract } = maxClient()
    const prepared = await preparePerpsOrderV3(client, manifest, input)
    expect(prepared.request.sizeDelta).toBe(7_900n * quantum)
    expect(prepared.request.marginDelta).toBe(999_798_000n)
    expect(prepared.reviewSummary?.requiredFundingAssets).toBe(1_000_000_000n)
    const quotes = readContract.mock.calls.filter(([request]) => request.functionName === 'quoteMaxOpen')
    expect(quotes).toHaveLength(1)
    expect(quotes[0][0]).toMatchObject({
      args: [account, 1, 999_798_000n, 100_000_000n], blockNumber: block.number,
    })
    const assessments = readContract.mock.calls.filter(([request]) => request.functionName === 'assessOrder')
    expect(assessments).toHaveLength(6)
    for (const [request] of assessments) expect(request.args?.[1]).toMatchObject({ sizeDelta: prepared.request.sizeDelta, marginDelta: prepared.request.marginDelta })
    for (const [request] of readContract.mock.calls) expect(request).toMatchObject({ blockNumber: block.number })
    expect(simulateContract).toHaveBeenCalledExactlyOnceWith(expect.objectContaining({ args: [prepared.request], blockNumber: block.number }))
  })

  it('refreshes a smaller lens maximum and reserves the attached protection trigger reward', async () => {
    const { client, readContract } = maxClient({ size: 200n * quantum })
    const prepared = await preparePerpsOrderV3(client, manifest, {
      ...input, positionProtection: { takeProfitTriggerPrice: 110_000_000n, stopLossTriggerPrice: 90_000_000n, closeLimits: relaxedProtectionCloseLimits() },
    })
    expect(prepared.request.sizeDelta).toBe(200n * quantum)
    expect(prepared.request.marginDelta).toBe(999_598_000n)
    expect(prepared.reviewSummary?.requiredFundingAssets).toBe(1_000_000_000n)
    expect(readContract).toHaveBeenCalledWith(expect.objectContaining({ functionName: 'quoteMaxOpen', args: [account, 1, 999_598_000n, 100_000_000n] }))
  })

  it('does not quote when the account cannot cover the router reward reserve', async () => {
    const { client, readContract, simulateContract } = maxClient({ available: 200_000n })
    await expect(preparePerpsOrderV3(client, manifest, input)).rejects.toThrow('after execution rewards')
    expect(readContract.mock.calls.some(([request]) => request.functionName === 'quoteMaxOpen')).toBe(false)
    expect(simulateContract).not.toHaveBeenCalled()
  })

  it.each([{ size: 0n, quoteValid: false }, { size: quantum, quoteValid: false }])('rejects zero capacity or an invalid quote: %s', async (options) => {
    const { client, readContract, simulateContract } = maxClient(options)
    await expect(preparePerpsOrderV3(client, manifest, input)).rejects.toThrow()
    expect(readContract.mock.calls.filter(([request]) => request.functionName === 'quoteMaxOpen')).toHaveLength(1)
    expect(readContract.mock.calls.some(([request]) => request.functionName === 'assessOrder')).toBe(false)
    expect(simulateContract).not.toHaveBeenCalled()
  })

  it.each([new Error('RPC connection lost'), { errorName: 'CfdEngineLens__QuoteSearchLimitExceeded' }])('propagates quote failures without falling back to client sizing', async (failure) => {
    const { client, readContract, simulateContract } = maxClient({ failure })
    await expect(preparePerpsOrderV3(client, manifest, input)).rejects.toBe(failure)
    expect(readContract.mock.calls.filter(([request]) => request.functionName === 'quoteMaxOpen')).toHaveLength(1)
    expect(readContract.mock.calls.some(([request]) => request.functionName === 'assessOrder')).toBe(false)
    expect(simulateContract).not.toHaveBeenCalled()
  })

  it('reports no capacity when no smaller size fits the selected leverage', async () => {
    const { client, readContract, simulateContract } = maxClient({ leverage: 400_000n })
    await expect(preparePerpsOrderV3(client, manifest, input)).rejects.toThrow('Available margin cannot fund an opening order at the selected leverage.')
    expect(readContract.mock.calls.filter(([request]) => request.functionName === 'quoteMaxOpen')).toHaveLength(1)
    expect(simulateContract).not.toHaveBeenCalled()
  })

  it('resizes Max to fit selected leverage at the worst reviewed price with its fixed budget', async () => {
    const { client, readContract, simulateContract } = maxClient({
      leverage: (size, price) => (size * price / 10n ** 20n) * 10_000n / 999_000_000n,
    })
    const read = readContract.getMockImplementation()!
    readContract.mockImplementation(async request => {
      if (request.functionName === 'assessOrder' && (request.args?.[1] as { sizeDelta: bigint }).sizeDelta > 100n * quantum) {
        throw new Error('Unrestricted maximum fails entry margin at adverse prices')
      }
      return read(request)
    })
    const prepared = await preparePerpsOrderV3(client, manifest, { ...input, slippagePercent: 2, selectedMaxLeverageBps: 100_000 })
    expect(prepared.request.sizeDelta).toBe(97n * quantum)
    expect(prepared.request.marginDelta).toBe(999_798_000n)
    expect(prepared.reviewSummary?.worstPostLeverageBps).toBeLessThanOrEqual(100_000n)
    for (const [request] of readContract.mock.calls) expect(request).toMatchObject({ blockNumber: block.number })
    expect(simulateContract).toHaveBeenCalledExactlyOnceWith(expect.objectContaining({ args: [prepared.request] }))
  })

  it('retains router rejection without retrying another size', async () => {
    const failure = new Error('Router rejected the quoted size')
    const { client, readContract, simulateContract } = maxClient({ assessmentFailure: failure })
    await expect(preparePerpsOrderV3(client, manifest, input)).rejects.toBe(failure)
    expect(readContract.mock.calls.filter(([request]) => request.functionName === 'quoteMaxOpen')).toHaveLength(1)
    expect(simulateContract).not.toHaveBeenCalled()
  })

  it('does not use the maximum lens for a close or manually entered size', async () => {
    const { client, readContract } = maxClient({ leverage: 50_000n })
    await preparePerpsOrderV3(client, manifest, { ...input, maxSize: undefined })
    await preparePerpsOrderV3(client, manifest, { ...input, isClose: true, marginDelta: 0n })
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
    await expect(preparePerpsOrderV3(client, manifest, { ...input, signal: controller.signal })).rejects.toMatchObject({ name: 'AbortError' })
    expect(readContract.mock.calls.some(([request]) => request.functionName === 'assessOrder')).toBe(false)
    expect(simulateContract).not.toHaveBeenCalled()
  })
})

vi.mock('../../perps-aa/deadlineClock', () => ({ deadlineNow: () => Date.now(), refreshDeadlineClock: async () => ({ now: () => Date.now() }), observeDeadlineResponse: () => {} }))
