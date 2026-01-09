import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook, act, waitFor } from '@testing-library/react'
import { useMint } from '../usePlethCore'
import { useTransactionStore } from '../../stores/transactionStore'

const mockWriteContract = vi.fn()
const mockReset = vi.fn()

let mockIsSuccess = false
let mockIsError = false
let mockHash: string | undefined = undefined

vi.mock('wagmi', () => ({
  useAccount: vi.fn(() => ({
    chainId: 11155111,
    address: '0x1234567890abcdef1234567890abcdef12345678',
    isConnected: true,
  })),
  useWriteContract: vi.fn(() => ({
    writeContract: mockWriteContract,
    data: mockHash,
    isPending: false,
    error: null,
    reset: mockReset,
  })),
  useWaitForTransactionReceipt: vi.fn(() => ({
    isLoading: false,
    isSuccess: mockIsSuccess,
    isError: mockIsError,
  })),
  useReadContract: vi.fn(() => ({
    data: undefined,
    isLoading: false,
    error: null,
    refetch: vi.fn(),
  })),
}))

describe('useMint', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mockIsSuccess = false
    mockIsError = false
    mockHash = undefined
    useTransactionStore.setState({ pendingTransactions: [] })
  })

  it('adds a pending transaction when mint is called', async () => {
    const { result } = renderHook(() => useMint())

    await act(async () => {
      await result.current.mint(1000000000000000000n)
    })

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions).toHaveLength(1)
    expect(pendingTransactions[0].type).toBe('mint')
    expect(pendingTransactions[0].status).toBe('pending')
    expect(pendingTransactions[0].description).toBe('Minting DXY-BEAR + DXY-BULL')
  })

  it('calls writeContract with correct arguments', async () => {
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

  it('updates transaction to confirming when writeContract succeeds', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xabc123hash')
    })

    const { result } = renderHook(() => useMint())

    await act(async () => {
      await result.current.mint(1000000000000000000n)
    })

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions[0].status).toBe('confirming')
    expect(pendingTransactions[0].hash).toBe('0xabc123hash')
  })

  it('updates transaction to failed when writeContract errors', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onError(new Error('User rejected'))
    })

    const { result } = renderHook(() => useMint())

    await act(async () => {
      await result.current.mint(1000000000000000000n)
    })

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

    mockIsSuccess = true
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

    mockIsError = true
    rerender()

    await waitFor(() => {
      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions[0].status).toBe('failed')
    })
  })

  it('handles writeContract throwing an exception', async () => {
    mockWriteContract.mockImplementation(() => {
      throw new Error('Network error')
    })

    const { result } = renderHook(() => useMint())

    await act(async () => {
      await result.current.mint(1000000000000000000n)
    })

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions[0].status).toBe('failed')
  })
})
