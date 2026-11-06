import { act, renderHook } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'

const switchChain = vi.hoisted(() => vi.fn())
vi.mock('wagmi', () => ({ useSwitchChain: () => ({ switchChainAsync: switchChain, isPending: false }) }))

import { useSwitchToArbitrumSepolia } from '../useSwitchToArbitrumSepolia'

describe('switch to Arbitrum Sepolia', () => {
  beforeEach(() => { vi.clearAllMocks() })

  it('switches through the active wagmi connector', async () => {
    switchChain.mockResolvedValue({ id: 421614 })
    const { result } = renderHook(() => useSwitchToArbitrumSepolia())
    await act(async () => { expect(await result.current.switchToArbitrumSepolia()).toBe(true) })
    expect(switchChain).toHaveBeenCalledWith({ chainId: 421614 })
    expect(result.current.switchError).toBeNull()
    expect(result.current.isSwitching).toBe(false)
  })

  it('surfaces a declined request and allows the user to retry', async () => {
    switchChain.mockRejectedValue(new Error('User rejected the request'))
    const { result } = renderHook(() => useSwitchToArbitrumSepolia())
    await act(async () => { expect(await result.current.switchToArbitrumSepolia()).toBe(false) })
    expect(result.current.switchError).toMatch(/rejected/)
    expect(result.current.isSwitching).toBe(false)
    act(() => { result.current.clearSwitchError() })
    expect(result.current.switchError).toBeNull()
  })
})
