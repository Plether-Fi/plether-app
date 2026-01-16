import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook, act, waitFor } from '@testing-library/react'
import { Result } from 'better-result'
import { useTransactionStore } from '../../stores/transactionStore'
import type { MintError } from '../usePlethCore'

const mockWriteContract = vi.fn()
const mockReset = vi.fn()

const mockUseWriteContract = vi.fn()
const mockUseWaitForTransactionReceipt = vi.fn()

vi.mock('wagmi', () => ({
  useAccount: vi.fn(() => ({
    chainId: 11155111,
    address: '0x1234567890abcdef1234567890abcdef12345678',
    isConnected: true,
  })),
  useWriteContract: () => mockUseWriteContract(),
  useWaitForTransactionReceipt: () => mockUseWaitForTransactionReceipt(),
  useReadContract: vi.fn(() => ({
    data: undefined,
    isLoading: false,
    error: null,
    refetch: vi.fn(),
  })),
}))

import { useMint } from '../usePlethCore'

describe('useMint', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    useTransactionStore.setState({ pendingTransactions: [] })

    mockUseWriteContract.mockReturnValue({
      writeContract: mockWriteContract,
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

  it('adds a pending transaction when mint is called', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xhash')
    })

    const { result } = renderHook(() => useMint())

    await act(async () => {
      await result.current.mint(1000000000000000000n)
    })

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions).toHaveLength(1)
    expect(pendingTransactions[0].type).toBe('mint')
    expect(pendingTransactions[0].description).toBe('Minting DXY-BEAR + DXY-BULL')
  })

  it('calls writeContract with correct arguments', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xhash')
    })

    const { result } = renderHook(() => useMint())
    const pairAmount = 50000000000000000000n

    await act(async () => {
      await result.current.mint(pairAmount)
    })

    expect(mockWriteContract).toHaveBeenCalledTimes(1)
    const callArgs = mockWriteContract.mock.calls[0][0]
    expect(callArgs.functionName).toBe('mint')
    expect(callArgs.args[0]).toBe(pairAmount)
  })

  it('returns Result.ok with hash when writeContract succeeds', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xabc123hash')
    })

    const { result } = renderHook(() => useMint())
    let mintResult: Result<`0x${string}`, MintError> | undefined

    await act(async () => {
      mintResult = await result.current.mint(1000000000000000000n)
    })

    expect(mintResult).toBeDefined()
    expect(Result.isOk(mintResult!)).toBe(true)
    if (Result.isOk(mintResult!)) {
      expect(mintResult.value).toBe('0xabc123hash')
    }

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions[0].status).toBe('confirming')
    expect(pendingTransactions[0].hash).toBe('0xabc123hash')
  })

  it('returns Result.err when writeContract errors', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onError(new Error('User rejected'))
    })

    const { result } = renderHook(() => useMint())
    let mintResult: Result<`0x${string}`, MintError> | undefined

    await act(async () => {
      mintResult = await result.current.mint(1000000000000000000n)
    })

    expect(mintResult).toBeDefined()
    expect(Result.isError(mintResult!)).toBe(true)

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions[0].status).toBe('failed')
  })

  it('updates transaction to success when isSuccess becomes true', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xabc123hash')
    })

    const { result, rerender } = renderHook(() => useMint())

    await act(async () => {
      await result.current.mint(1000000000000000000n)
    })

    expect(useTransactionStore.getState().pendingTransactions[0].status).toBe('confirming')

    mockUseWaitForTransactionReceipt.mockReturnValue({
      isLoading: false,
      isSuccess: true,
      isError: false,
    })
    rerender()

    await waitFor(() => {
      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions[0].status).toBe('success')
    })
  })

  it('updates transaction to failed when isError becomes true', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xabc123hash')
    })

    const { result, rerender } = renderHook(() => useMint())

    await act(async () => {
      await result.current.mint(1000000000000000000n)
    })

    expect(useTransactionStore.getState().pendingTransactions[0].status).toBe('confirming')

    mockUseWaitForTransactionReceipt.mockReturnValue({
      isLoading: false,
      isSuccess: false,
      isError: true,
    })
    rerender()

    await waitFor(() => {
      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions[0].status).toBe('failed')
    })
  })

  it('returns Result.err when writeContract throws an exception', async () => {
    mockWriteContract.mockImplementation(() => {
      throw new Error('Network error')
    })

    const { result } = renderHook(() => useMint())
    let mintResult: Result<`0x${string}`, MintError> | undefined

    await act(async () => {
      mintResult = await result.current.mint(1000000000000000000n)
    })

    expect(mintResult).toBeDefined()
    expect(Result.isError(mintResult!)).toBe(true)

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions[0].status).toBe('failed')
  })
})
