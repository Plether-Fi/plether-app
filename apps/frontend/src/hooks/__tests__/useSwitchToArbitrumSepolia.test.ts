import { act, renderHook } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { useSwitchToArbitrumSepolia } from '../useSwitchToArbitrumSepolia'

const mocks = vi.hoisted(() => ({ switchChain: vi.fn(), switchAppKit: vi.fn(), open: vi.fn() }))
vi.mock('wagmi', () => ({ useSwitchChain: () => ({ switchChainAsync: mocks.switchChain, isPending: false }) }))
vi.mock('../../contracts/perpsAddresses', () => ({ PERPS_CHAIN: { id: 42161, name: 'Arbitrum One' } }))
vi.mock('../../config/wagmi', () => ({ switchAppKitToArbitrumSepolia: mocks.switchAppKit, openAppKit: mocks.open }))

describe('switching to the active perps network', () => {
  afterEach(() => vi.restoreAllMocks())
  beforeEach(() => {
    vi.resetAllMocks()
    vi.spyOn(console, 'warn').mockImplementation(() => undefined)
  })

  it('uses the reviewed mainnet chain and clears the pending state after success', async () => {
    mocks.switchChain.mockResolvedValue(undefined)
    const { result } = renderHook(useSwitchToArbitrumSepolia)
    await act(async () => { expect(await result.current.switchToArbitrumSepolia()).toBe(true) })
    expect(mocks.switchChain).toHaveBeenCalledWith({ chainId: 42161 })
    expect(mocks.switchAppKit).not.toHaveBeenCalled()
    expect(result.current.isSwitching).toBe(false)
  })

  it('clears the pending state after an AppKit fallback succeeds', async () => {
    mocks.switchChain.mockRejectedValue(new Error('Unsupported method'))
    mocks.switchAppKit.mockResolvedValue(undefined)
    const { result } = renderHook(useSwitchToArbitrumSepolia)
    await act(async () => { expect(await result.current.switchToArbitrumSepolia()).toBe(true) })
    expect(mocks.switchAppKit).toHaveBeenCalledOnce()
    expect(result.current.isSwitching).toBe(false)
  })

  it('names the active network when both switch methods fail', async () => {
    mocks.switchChain.mockRejectedValue(new Error('User rejected request'))
    mocks.switchAppKit.mockRejectedValue(new Error('User rejected request'))
    mocks.open.mockResolvedValue(undefined)
    const { result } = renderHook(useSwitchToArbitrumSepolia)
    await act(async () => { expect(await result.current.switchToArbitrumSepolia()).toBe(false) })
    expect(result.current.switchError).toContain('Arbitrum One')
    expect(result.current.switchError).not.toContain('Sepolia')
    expect(result.current.isSwitching).toBe(false)
  })
})
