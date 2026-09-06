import { renderHook } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { useProtectionConfiguration } from '../useProtectionConfiguration'

const mocks = vi.hoisted(() => ({ read: vi.fn(), enabled: true }))
vi.mock('wagmi', () => ({ useReadContracts: mocks.read }))
vi.mock('../../contracts/positionProtection', async importOriginal => ({
  ...await importOriginal<typeof import('../../contracts/positionProtection')>(),
  get PROTECTION_RELEASE_ENABLED() { return mocks.enabled },
}))

describe('v1.2.2 protection configuration', () => {
  beforeEach(() => { mocks.enabled = true; mocks.read.mockReset() })
  it('enables TP/SL from live reserves without querying the removed contract flag', () => {
    mocks.read.mockReturnValue({ data: [{ result: 200_000n }, { result: 300_000n }] })
    const { result } = renderHook(useProtectionConfiguration)
    expect(result.current).toEqual({ enabled: true, triggerBountyUsdc: 200_000n, executionBountyUsdc: 300_000n })
    expect(mocks.read).toHaveBeenCalledWith(expect.objectContaining({
      contracts: [
        expect.objectContaining({ functionName: 'positionProtectionTriggerBountyUsdc' }),
        expect.objectContaining({ functionName: 'closeOrderExecutionBountyUsdc' }),
      ],
    }))
  })
  it.each([undefined, [{ result: 200_000n }, { status: 'failure' }]])('does not enable TP/SL with missing reserves: %s', data => {
    mocks.read.mockReturnValue({ data })
    expect(renderHook(useProtectionConfiguration).result.current.enabled).toBe(false)
  })
  it('still respects the frontend release flag', () => {
    mocks.enabled = false
    mocks.read.mockReturnValue({ data: [{ result: 200_000n }, { result: 300_000n }] })
    expect(renderHook(useProtectionConfiguration).result.current.enabled).toBe(false)
    expect(mocks.read).toHaveBeenCalledWith(expect.objectContaining({ query: expect.objectContaining({ enabled: false }) }))
  })
})
