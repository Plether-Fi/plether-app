import { describe, expect, it, vi } from 'vitest'
import { constrainPerpsMaxOpen } from '../perpsMaxOpen'
import { PERPS_POSITION_SIZE_QUANTUM as quantum } from '../perpsConstants'

describe('constrainPerpsMaxOpen', () => {
  it('returns the exact highest lot within the selected leverage', async () => {
    const assess = vi.fn(async (size: bigint) => ({ leverageBps: size / quantum * 1000n, value: size }))
    const result = await constrainPerpsMaxOpen({ maxSizeDelta: 100n * quantum, selectedMaxLeverageBps: 33_000, assess })
    expect(result?.sizeDelta).toBe(33n * quantum)
    expect(result?.value).toBe(33n * quantum)
    expect(assess.mock.calls.some(([size]) => size === 34n * quantum)).toBe(true)
  })

  it('does not interpret rejected smaller sizes as zero capacity', async () => {
    const failure = new Error('Planner rejected this size')
    await expect(constrainPerpsMaxOpen({ maxSizeDelta: 100n * quantum, selectedMaxLeverageBps: 10_000,
      assess: async size => {
        if (size < 100n * quantum) throw failure
        return { leverageBps: 100_000n, value: size }
      },
    })).rejects.toBe(failure)
  })

  it('discards an assessment returned after cancellation', async () => {
    const controller = new AbortController()
    await expect(constrainPerpsMaxOpen({ maxSizeDelta: 100n * quantum, selectedMaxLeverageBps: 10_000,
      signal: controller.signal, assess: async size => {
        controller.abort()
        return { leverageBps: 100_000n, value: size }
      },
    })).rejects.toMatchObject({ name: 'AbortError' })
  })
})
