import { describe, expect, it, vi } from 'vitest'
import { createPublicClient, custom, encodeErrorResult, encodeFunctionData, encodeFunctionResult, type Hex, type PublicClient } from 'viem'
import { PERPS_MAX_OPEN_QUOTER_ABI, PERPS_MAX_OPEN_QUOTER_BYTECODE } from '../generated/perpsMaxOpenQuoter'
import { PERPS_ARBITRUM_SEPOLIA } from '../perpsAddresses'
import { quotePerpsMaxOpen, quotePerpsMaxOpenWithMargin, type PerpsMaxOpenQuote } from '../perpsMaxOpenQuote'
import { getPerpsContractErrorCode, getPerpsErrorMessage } from '../../utils/perpsErrors'

const input = {
  account: '0x00000000000000000000000000000000000000a1' as const, side: 1,
  availableAssets: 100_000_000n, protectionRewardsAssets: 400_000n,
  oraclePrice: 98_000_000n, selectedMaxLeverageBps: 100_000,
}
const quote: PerpsMaxOpenQuote = {
  maxSizeDelta: 900n * 10n ** 18n, limitingReason: 9,
  preview: {
    valid: true, invalidReason: 0, failureCategory: 0, executionPrice: input.oraclePrice,
    sizeDelta: 900n * 10n ** 18n, notionalAssets: 882_000_000n, marginDeltaAssets: 99_400_000n,
    vpiAssets: 200_000n, executionFeeAssets: 352_800n, tradeCostAssets: 552_800n,
    poolRebatePayoutAssets: 0n, pendingCarryAssets: 0n, initialMarginRequirementAssets: 17_640_000n,
    maintenanceMarginAssets: 8_820_000n, postSize: 900n * 10n ** 18n,
    postMarginAssets: 98_847_200n, postEntryPrice: input.oraclePrice, postVpiAccrued: 200_000n,
    postUnrealizedPnlAssets: 0n, postEquityAssets: 98_847_200n, postHealthBps: 112_071n,
    postLiquidatable: false, hasLiquidationPrice: true, liquidationPrice: 108_000_000n,
  },
}

function rpcClient(result: Hex) {
  const request = vi.fn(async () => result)
  return { client: createPublicClient({ transport: custom({ request }, { retryCount: 0 }) }), request }
}

describe('single-RPC Max quote', () => {
  it('uses one deployless eth_call including budget, config and leverage, without a block/config preflight', async () => {
    const { client, request } = rpcClient(encodeFunctionResult({
      abi: PERPS_MAX_OPEN_QUOTER_ABI, functionName: 'quoteAvailable', result: [99_400_000n, quote],
    }))
    expect(await quotePerpsMaxOpen(client, input)).toEqual({ marginDelta: 99_400_000n, quote })
    expect(request).toHaveBeenCalledOnce()
    const call = request.mock.calls[0]![0] as unknown as { method: string; params: [{ to?: string; data: Hex }, string] }
    expect(call.method).toBe('eth_call')
    expect(call.params[0].to).toBeUndefined()
    expect(call.params[1]).toBe('latest')
    expect(call.params[0].data).toContain(PERPS_MAX_OPEN_QUOTER_BYTECODE.slice(2))
    expect(call.params[0].data).toContain(encodeFunctionData({
      abi: PERPS_MAX_OPEN_QUOTER_ABI, functionName: 'quoteAvailable',
      args: [PERPS_ARBITRUM_SEPOLIA.cfdEngineLens, PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        PERPS_ARBITRUM_SEPOLIA.orderRouter, input.account, input.side, input.availableAssets,
        input.protectionRewardsAssets, input.oraclePrice, BigInt(input.selectedMaxLeverageBps)],
    }).slice(2))
  })

  it('uses the verified block and fixed margin for review in one call', async () => {
    const { client, request } = rpcClient(encodeFunctionResult({
      abi: PERPS_MAX_OPEN_QUOTER_ABI, functionName: 'quoteWithMargin', result: quote,
    }))
    expect(await quotePerpsMaxOpenWithMargin(client, { ...input, marginDelta: 99_400_000n, blockNumber: 42n })).toEqual(quote)
    expect(request).toHaveBeenCalledOnce()
    expect(request.mock.lastCall).toEqual(expect.arrayContaining([expect.objectContaining({ method: 'eth_call', params: [expect.objectContaining({ data: expect.any(String) }), '0x2a'] })]))
  })

  it('returns no quote when execution and protection rewards exhaust the budget', async () => {
    const { client } = rpcClient(encodeFunctionResult({
      abi: PERPS_MAX_OPEN_QUOTER_ABI, functionName: 'quoteAvailable',
      result: [0n, { ...quote, maxSizeDelta: 0n }],
    }))
    expect(await quotePerpsMaxOpen(client, input)).toEqual({ marginDelta: 0n, quote: undefined })
  })

  it.each([0, -1, NaN, Infinity, 1.5, Number.MAX_SAFE_INTEGER + 1])('rejects invalid leverage %s before making a request', async selectedMaxLeverageBps => {
    const { client, request } = rpcClient('0x')
    await expect(quotePerpsMaxOpen(client, { ...input, selectedMaxLeverageBps })).rejects.toThrow('Selected maximum leverage is invalid')
    expect(request).not.toHaveBeenCalled()
  })

  it('rejects an invalid side without making a request', async () => {
    const { client, request } = rpcClient('0x')
    await expect(quotePerpsMaxOpen(client, { ...input, side: 2 })).rejects.toThrow('Position side is invalid')
    expect(request).not.toHaveBeenCalled()
  })

  it.each(['before', 'after'])('discards cancellation %s the RPC without falling back', async when => {
    const controller = new AbortController()
    const readContract = vi.fn(async () => {
      controller.abort()
      return [99_400_000n, quote]
    })
    if (when === 'before') controller.abort()
    const client = { readContract } as unknown as PublicClient
    await expect(quotePerpsMaxOpen(client, { ...input, signal: controller.signal })).rejects.toMatchObject({ name: 'AbortError' })
    expect(readContract).toHaveBeenCalledTimes(when === 'before' ? 0 : 1)
  })

  it('propagates a provider failure without a second request or an approximate quote', async () => {
    const failure = new Error('RPC unavailable')
    const readContract = vi.fn().mockRejectedValue(failure)
    const client = { readContract } as unknown as PublicClient
    await expect(quotePerpsMaxOpen(client, input)).rejects.toBe(failure)
    expect(readContract).toHaveBeenCalledOnce()
  })

  it('preserves invalid intermediate planner diagnostics through a real viem revert', async () => {
    const data = encodeErrorResult({ abi: PERPS_MAX_OPEN_QUOTER_ABI,
      errorName: 'PerpsMaxOpenQuoter__InvalidPreview', args: [9] })
    const request = vi.fn(async () => { throw { code: 3, message: 'execution reverted', data } })
    const client = createPublicClient({ transport: custom({ request }, { retryCount: 0 }) })
    const error = await quotePerpsMaxOpen(client, input).catch((cause: unknown) => cause)
    expect(request).toHaveBeenCalledOnce()
    expect(getPerpsContractErrorCode(error)).toBe('PerpsMaxOpenQuoter__InvalidPreview')
    expect(getPerpsErrorMessage(error, 'review')).toBe(getPerpsErrorMessage({ errorName: 'OrderRouter__PredictableOpenInvalid', args: [9] }, 'review'))
  })
})
