import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook, act, waitFor } from '@testing-library/react'
import { Result } from 'better-result'
import { useTransactionStore } from '../../stores/transactionStore'
import type { SwapError } from '../useTrading'

const mockWriteContractAsync = vi.fn()
const mockReset = vi.fn()

const mockUseAccount = vi.fn()
const mockUseWriteContract = vi.fn()
const mockUseWaitForTransactionReceipt = vi.fn()

vi.mock('wagmi', () => ({
  useAccount: () => mockUseAccount(),
  useWriteContract: () => mockUseWriteContract(),
  useWaitForTransactionReceipt: () => mockUseWaitForTransactionReceipt(),
}))

import { useZapSwap } from '../useTrading'

describe('useZapSwap', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    useTransactionStore.setState({ transactions: [] })

    mockUseAccount.mockReturnValue({
      chainId: 11155111,
    })

    mockUseWriteContract.mockReturnValue({
      writeContractAsync: mockWriteContractAsync,
      data: undefined,
      isPending: false,
      error: null,
      reset: mockReset,
    })

    mockUseWaitForTransactionReceipt.mockReturnValue({
      isLoading: false,
      isSuccess: false,
      isError: false,
    })
  })

  describe('zapSell', () => {
    it('returns Result.ok with hash when zapSell succeeds', async () => {
      mockWriteContractAsync.mockResolvedValue('0xhash')

      const { result } = renderHook(() => useZapSwap())
      let zapResult: Result<`0x${string}`, SwapError> | undefined

      await act(async () => {
        zapResult = await result.current.zapSell(
          100000000000000000000n, // 100 BULL (18 decimals)
          85000000n, // min 85 USDC out (6 decimals)
          1800000000n // deadline
        )
      })

      expect(zapResult).toBeDefined()
      expect(Result.isOk(zapResult!)).toBe(true)
      expect((zapResult as { value: string }).value).toBe('0xhash')
    })

    it('calls writeContractAsync with correct arguments for zapSell', async () => {
      mockWriteContractAsync.mockResolvedValue('0xhash')

      const { result } = renderHook(() => useZapSwap())
      const bullAmount = 100000000000000000000n
      const minUsdcOut = 85000000n
      const deadline = 1800000000n
      let zapResult: Result<`0x${string}`, SwapError> | undefined

      await act(async () => {
        zapResult = await result.current.zapSell(bullAmount, minUsdcOut, deadline)
      })

      expect(zapResult).toBeDefined()
      expect(Result.isOk(zapResult!)).toBe(true)

      expect(mockWriteContractAsync).toHaveBeenCalledTimes(1)
      const callArgs = mockWriteContractAsync.mock.calls[0][0]
      expect(callArgs.functionName).toBe('zapBurn')
      expect(callArgs.args[0]).toBe(bullAmount)
      expect(callArgs.args[1]).toBe(minUsdcOut)
      expect(callArgs.args[2]).toBe(deadline)
    })

    it('returns Result.err when writeContractAsync rejects', async () => {
      mockWriteContractAsync.mockRejectedValue(new Error('User rejected'))

      const { result } = renderHook(() => useZapSwap())
      let zapResult: Result<`0x${string}`, SwapError> | undefined

      await act(async () => {
        zapResult = await result.current.zapSell(100000000000000000000n, 85000000n, 1800000000n)
      })

      expect(zapResult).toBeDefined()
      expect(Result.isError(zapResult!)).toBe(true)
    })

    it('exposes isSuccess when receipt confirms', async () => {
      mockWriteContractAsync.mockResolvedValue('0xzapsell123hash')

      const { result, rerender } = renderHook(() => useZapSwap())

      await act(async () => {
        await result.current.zapSell(100000000000000000000n, 85000000n, 1800000000n)
      })

      mockUseWaitForTransactionReceipt.mockReturnValue({
        isLoading: false,
        isSuccess: true,
        isError: false,
      })
      rerender()

      await waitFor(() => {
        expect(result.current.isSuccess).toBe(true)
      })
    })

    it('returns Result.err when chainId is not available', async () => {
      mockUseAccount.mockReturnValue({
        chainId: undefined,
      })

      const { result } = renderHook(() => useZapSwap())
      let zapResult: Result<`0x${string}`, SwapError> | undefined

      await act(async () => {
        zapResult = await result.current.zapSell(100000000000000000000n, 85000000n, 1800000000n)
      })

      expect(zapResult).toBeDefined()
      expect(Result.isError(zapResult!)).toBe(true)

      expect(mockWriteContractAsync).not.toHaveBeenCalled()
    })
  })

  describe('zapBuy', () => {
    it('calls writeContractAsync with correct arguments for zapBuy and returns Result.ok', async () => {
      mockWriteContractAsync.mockResolvedValue('0xhash')

      const { result } = renderHook(() => useZapSwap())
      const usdcAmount = 100000000n // 100 USDC
      const minBullOut = 95000000000000000000n // min 95 BULL
      const maxSlippageBps = 100n
      const deadline = 1800000000n
      let zapResult: Result<`0x${string}`, SwapError> | undefined

      await act(async () => {
        zapResult = await result.current.zapBuy(usdcAmount, minBullOut, maxSlippageBps, deadline)
      })

      expect(zapResult).toBeDefined()
      expect(Result.isOk(zapResult!)).toBe(true)
      expect((zapResult as { value: string }).value).toBe('0xhash')

      expect(mockWriteContractAsync).toHaveBeenCalledTimes(1)
      const callArgs = mockWriteContractAsync.mock.calls[0][0]
      expect(callArgs.functionName).toBe('zapMint')
      expect(callArgs.args[0]).toBe(usdcAmount)
      expect(callArgs.args[1]).toBe(minBullOut)
      expect(callArgs.args[2]).toBe(maxSlippageBps)
      expect(callArgs.args[3]).toBe(deadline)
    })
  })
})
