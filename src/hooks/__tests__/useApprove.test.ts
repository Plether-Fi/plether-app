import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook, act, waitFor } from '@testing-library/react'
import { useTransactionStore } from '../../stores/transactionStore'

const mockWriteContract = vi.fn()
const mockReset = vi.fn()

const mockUseWriteContract = vi.fn()
const mockUseWaitForTransactionReceipt = vi.fn()

vi.mock('wagmi', () => ({
  useWriteContract: () => mockUseWriteContract(),
  useWaitForTransactionReceipt: () => mockUseWaitForTransactionReceipt(),
}))

import { useApprove } from '../useApprove'

const TOKEN_ADDRESS = '0x1111111111111111111111111111111111111111' as const
const SPENDER_ADDRESS = '0x2222222222222222222222222222222222222222' as const

describe('useApprove', () => {
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

  it('adds a pending transaction when approve is called', async () => {
    const { result } = renderHook(() => useApprove(TOKEN_ADDRESS, SPENDER_ADDRESS))

    await act(async () => {
      await result.current.approve(100000000n)
    })

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions).toHaveLength(1)
    expect(pendingTransactions[0].type).toBe('approve')
    expect(pendingTransactions[0].status).toBe('pending')
    expect(pendingTransactions[0].description).toBe('Approving token spend')
  })

  it('calls writeContract with correct arguments', async () => {
    const { result } = renderHook(() => useApprove(TOKEN_ADDRESS, SPENDER_ADDRESS))
    const amount = 100000000n

    await act(async () => {
      await result.current.approve(amount)
    })

    expect(mockWriteContract).toHaveBeenCalledTimes(1)
    const callArgs = mockWriteContract.mock.calls[0][0]
    expect(callArgs.address).toBe(TOKEN_ADDRESS)
    expect(callArgs.functionName).toBe('approve')
    expect(callArgs.args[0]).toBe(SPENDER_ADDRESS)
    expect(callArgs.args[1]).toBe(amount)
  })

  it('updates transaction to confirming when writeContract succeeds', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xapprove123hash')
    })

    const { result } = renderHook(() => useApprove(TOKEN_ADDRESS, SPENDER_ADDRESS))

    await act(async () => {
      await result.current.approve(100000000n)
    })

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions[0].status).toBe('confirming')
    expect(pendingTransactions[0].hash).toBe('0xapprove123hash')
  })

  it('updates transaction to failed when writeContract errors', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onError(new Error('User rejected'))
    })

    const { result } = renderHook(() => useApprove(TOKEN_ADDRESS, SPENDER_ADDRESS))

    await act(async () => {
      await result.current.approve(100000000n)
    })

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions[0].status).toBe('failed')
  })

  it('updates transaction to success when isSuccess becomes true', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xapprove123hash')
    })

    const { result, rerender } = renderHook(() => useApprove(TOKEN_ADDRESS, SPENDER_ADDRESS))

    await act(async () => {
      await result.current.approve(100000000n)
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
      callbacks.onSuccess('0xapprove123hash')
    })

    const { result, rerender } = renderHook(() => useApprove(TOKEN_ADDRESS, SPENDER_ADDRESS))

    await act(async () => {
      await result.current.approve(100000000n)
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

  it('handles writeContract throwing an exception', async () => {
    mockWriteContract.mockImplementation(() => {
      throw new Error('Network error')
    })

    const { result } = renderHook(() => useApprove(TOKEN_ADDRESS, SPENDER_ADDRESS))

    await act(async () => {
      await result.current.approve(100000000n)
    })

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions[0].status).toBe('failed')
  })

  it('uses the correct token and spender addresses from hook params', async () => {
    const customToken = '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' as const
    const customSpender = '0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' as const

    const { result } = renderHook(() => useApprove(customToken, customSpender))

    await act(async () => {
      await result.current.approve(500n)
    })

    const callArgs = mockWriteContract.mock.calls[0][0]
    expect(callArgs.address).toBe(customToken)
    expect(callArgs.args[0]).toBe(customSpender)
  })
})
