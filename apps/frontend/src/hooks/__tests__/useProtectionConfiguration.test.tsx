import { renderHook } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { useProtectionConfiguration } from '../useProtectionConfiguration'

const mocks = vi.hoisted(() => ({ read: vi.fn(), enabled: true, deployed: true }))
vi.mock('wagmi', () => ({ useReadContracts: mocks.read }))
vi.mock('../../contracts/positionProtection', async importOriginal => ({
  ...await importOriginal<typeof import('../../contracts/positionProtection')>(),
  get PROTECTION_RELEASE_ENABLED() { return mocks.enabled },
}))
vi.mock('../../contracts/perpsAddresses', async importOriginal => ({
  ...await importOriginal<typeof import('../../contracts/perpsAddresses')>(),
  get PERPS_RELEASE_IS_DEPLOYED() { return mocks.deployed },
}))

describe('canonical protection configuration', () => {
  beforeEach(() => { mocks.enabled = true; mocks.deployed = true; mocks.read.mockReset() })

  it('reserves only the live trigger reward without querying a close execution bounty', () => {
    mocks.read.mockReturnValue({ data: [{ result: 200_000n }] })
    const { result } = renderHook(useProtectionConfiguration)
    expect(result.current).toEqual({ enabled: true, triggerBountyAssets: 200_000n, executionBountyAssets: 0n })
    expect(mocks.read).toHaveBeenCalledWith(expect.objectContaining({
      contracts: [expect.objectContaining({ functionName: 'positionProtectionTriggerBountyAssets' })],
      query: expect.objectContaining({ enabled: true }),
    }))
  })

  it.each([undefined, [{ status: 'failure' }], [{ result: '200000' }]])('does not enable TP/SL with an unavailable trigger reward: %s', data => {
    mocks.read.mockReturnValue({ data })
    expect(renderHook(useProtectionConfiguration).result.current.enabled).toBe(false)
  })

  it('accepts a confirmed zero trigger reward', () => {
    mocks.read.mockReturnValue({ data: [{ result: 0n }] })
    expect(renderHook(useProtectionConfiguration).result.current).toEqual({ enabled: true, triggerBountyAssets: 0n, executionBountyAssets: 0n })
  })

  it.each(['enabled', 'deployed'] as const)('disables reads and TP/SL when %s is false, even with cached configuration', gate => {
    mocks[gate] = false
    mocks.read.mockReturnValue({ data: [{ result: 200_000n }] })
    expect(renderHook(useProtectionConfiguration).result.current.enabled).toBe(false)
    expect(mocks.read).toHaveBeenCalledWith(expect.objectContaining({ query: expect.objectContaining({ enabled: false }) }))
  })
})
