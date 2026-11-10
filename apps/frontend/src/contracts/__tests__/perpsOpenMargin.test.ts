import { describe, expect, it, vi } from 'vitest'
import type { PublicClient } from 'viem'
import { openPreviewLeverageBps, quotePerpsOpenMargin, type PerpsOpenMarginPreview } from '../perpsOpenMargin'

const input = {
  account: '0x00000000000000000000000000000000000000a1' as const,
  side: 0, sizeDelta: 1_000n * 10n ** 18n, marginBudget: 500_000_000n,
  oraclePrice: 100_000_000n, selectedMaxLeverageBps: 100_000,
}

function planner({ existingSize = 0n, existingEquity = 0n, reserve = 10_000_000n, fee = 500_000n,
  cap = 200_000_000n, invalidReason = 0,
} = {}) {
  const readContract = vi.fn(async ({ args }: { args: readonly unknown[] }) => {
    const size = args[2] as bigint
    const margin = args[3] as bigint
    const price = (args[4] as bigint) < cap ? args[4] as bigint : cap
    const valid = margin >= reserve + fee && invalidReason === 0
    return { valid, invalidReason: invalidReason || (valid ? 0 : 9),
      sizeDelta: size, marginDeltaAssets: margin, executionPrice: price,
      postSize: size + existingSize,
      postEquityAssets: valid ? existingEquity + margin - reserve - fee : 0n,
      postMarginAssets: valid ? margin - reserve - fee : 0n,
    } as PerpsOpenMarginPreview
  })
  const getBlockNumber = vi.fn(async () => 42n)
  return { readContract, getBlockNumber, client: { readContract, getBlockNumber } as unknown as PublicClient }
}

describe('funded opening margin', () => {
  it.each([0, 1])('funds reserves and fees for manual and percentage sizes on side %s', async side => {
    for (const percentage of [10n, 25n, 50n, 75n, 100n]) {
      const sizeDelta = input.sizeDelta * percentage / 100n
      const { client, readContract, getBlockNumber } = planner()
      const preview = await quotePerpsOpenMargin(client, { ...input, side, sizeDelta })
      const positionEquity = sizeDelta * input.oraclePrice / 10n ** 20n / 10n
      expect(preview.valid).toBe(true)
      expect(preview.marginDeltaAssets).toBe(positionEquity + 10_500_000n)
      expect(openPreviewLeverageBps(preview, preview.executionPrice)).toBe(100_000n)
      expect(getBlockNumber).toHaveBeenCalledOnce()
      for (const [request] of readContract.mock.calls) expect(request).toMatchObject({ blockNumber: 42n })
      // The returned input was actually rechecked by the planner.
      expect(readContract).toHaveBeenCalledWith(expect.objectContaining({
        args: [input.account, side, sizeDelta, preview.marginDeltaAssets, input.oraclePrice],
      }))
    }
  })

  it('requires reserve funding even when existing claims already cover the leverage target', async () => {
    const { client } = planner({ existingEquity: 1_000_000_000n })
    const preview = await quotePerpsOpenMargin(client, input)
    expect(preview.valid).toBe(true)
    expect(preview.marginDeltaAssets).toBe(10_500_000n)
  })

  it('uses total post-position size and equity when increasing a position', async () => {
    const { client } = planner({ existingSize: 2_000n * 10n ** 18n, existingEquity: 150_000_000n })
    const preview = await quotePerpsOpenMargin(client, input)
    expect(preview.marginDeltaAssets).toBe(160_500_000n)
    expect(openPreviewLeverageBps(preview, preview.executionPrice)).toBe(100_000n)
  })

  it('uses the canonical capped price and rounds required equity upward', async () => {
    const { client, getBlockNumber } = planner({ cap: 80_000_001n })
    const preview = await quotePerpsOpenMargin(client, { ...input, selectedMaxLeverageBps: 30_000, blockNumber: 123n })
    expect(preview.marginDeltaAssets).toBe(277_166_670n)
    expect(openPreviewLeverageBps(preview, preview.executionPrice)).toBeLessThanOrEqual(30_000n)
    expect(getBlockNumber).not.toHaveBeenCalled()
  })

  it('retains an unfunded reserve diagnostic when even the available budget cannot cover it', async () => {
    const { client } = planner()
    const preview = await quotePerpsOpenMargin(client, { ...input, marginBudget: 1_000_000n })
    expect(preview).toMatchObject({ valid: false, invalidReason: 9, marginDeltaAssets: 1_000_000n })
  })

  it('returns a funded but over-leveraged budget for the caller to reject', async () => {
    const { client } = planner()
    const preview = await quotePerpsOpenMargin(client, { ...input, marginBudget: 100_000_000n })
    expect(preview.valid).toBe(true)
    expect(openPreviewLeverageBps(preview, preview.executionPrice)).toBeGreaterThan(100_000n)
    expect(preview.marginDeltaAssets).toBe(100_000_000n)
  })

  it.each([1, 2, 3, 4, 7, 8, 11])('preserves non-funding planner rejection %s', async invalidReason => {
    const { client, readContract } = planner({ invalidReason })
    expect(await quotePerpsOpenMargin(client, input)).toMatchObject({ valid: false, invalidReason })
    expect(readContract).toHaveBeenCalledOnce()
  })

  it('propagates RPC failures and cancellation without an approximate margin', async () => {
    const { client, readContract } = planner()
    const error = new Error('RPC unavailable')
    readContract.mockRejectedValueOnce(error)
    await expect(quotePerpsOpenMargin(client, input)).rejects.toBe(error)
    const controller = new AbortController()
    controller.abort()
    await expect(quotePerpsOpenMargin(client, { ...input, signal: controller.signal })).rejects.toMatchObject({ name: 'AbortError' })
    expect(readContract).toHaveBeenCalledOnce()
  })
})
