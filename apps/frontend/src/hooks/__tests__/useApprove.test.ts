import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook, act } from '@testing-library/react'
import { Result } from 'better-result'
import { useTransactionStore } from '../../stores/transactionStore'
import type { ApproveError } from '../useApprove'

const mockWriteContractAsync = vi.fn()
const mockReset = vi.fn()
const mockWaitForTransactionReceipt = vi.fn()

const mockUseWriteContract = vi.fn()

vi.mock('wagmi', () => ({
  useWriteContract: () => mockUseWriteContract(),
  usePublicClient: () => ({
    waitForTransactionReceipt: mockWaitForTransactionReceipt,
  }),
}))

import { useApprove } from '../useApprove'

const TOKEN_ADDRESS = '0x1111111111111111111111111111111111111111' as const
const SPENDER_ADDRESS = '0x2222222222222222222222222222222222222222' as const

describe('useApprove', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ transactions: [] })

    mockUseWriteContract.mockReturnValue({
      writeContractAsync: mockWriteContractAsync,
      isPending: false,
      error: null,
      reset: mockReset,
    })

    mockWaitForTransactionReceipt.mockResolvedValue({
      status: 'success',
    })
  })

  it('adds a pending transaction when approve is called', async () => {
    mockWriteContractAsync.mockResolvedValue('0xhash')

    const { result } = renderHook(() => useApprove(TOKEN_ADDRESS, SPENDER_ADDRESS))

    await act(async () => {
      await result.current.approve(100000000n)
    })

    const { transactions } = useTransactionStore.getState()
    expect(transactions).toHaveLength(1)
    expect(transactions[0].type).toBe('approve')
    expect(transactions[0].title).toBe('Approving USDC')
  })

  it('calls writeContractAsync with correct arguments', async () => {
    mockWriteContractAsync.mockResolvedValue('0xhash')

    const { result } = renderHook(() => useApprove(TOKEN_ADDRESS, SPENDER_ADDRESS))
    const amount = 100000000n

    await act(async () => {
      await result.current.approve(amount)
    })

    expect(mockWriteContractAsync).toHaveBeenCalledTimes(1)
    const callArgs = mockWriteContractAsync.mock.calls[0][0]
    expect(callArgs.address).toBe(TOKEN_ADDRESS)
    expect(callArgs.functionName).toBe('approve')
    expect(callArgs.args[0]).toBe(SPENDER_ADDRESS)
    expect(callArgs.args[1]).toBe(amount)
  })

  it('returns Result.ok with hash when transaction succeeds', async () => {
    mockWriteContractAsync.mockResolvedValue('0xapprove123hash')
    mockWaitForTransactionReceipt.mockResolvedValue({ status: 'success' })

    const { result } = renderHook(() => useApprove(TOKEN_ADDRESS, SPENDER_ADDRESS))
    let approveResult: Result<`0x${string}`, ApproveError> | undefined

    await act(async () => {
      approveResult = await result.current.approve(100000000n)
    })

    expect(approveResult).toBeDefined()
    expect(Result.isOk(approveResult!)).toBe(true)
    if (Result.isOk(approveResult!)) {
      expect(approveResult.value).toBe('0xapprove123hash')
    }

    const { transactions } = useTransactionStore.getState()
    expect(transactions[0].status).toBe('success')
    expect(transactions[0].hash).toBe('0xapprove123hash')
  })

  it('returns Result.err when writeContractAsync rejects', async () => {
    mockWriteContractAsync.mockRejectedValue(new Error('User rejected'))

    const { result } = renderHook(() => useApprove(TOKEN_ADDRESS, SPENDER_ADDRESS))
    let approveResult: Result<`0x${string}`, ApproveError> | undefined

    await act(async () => {
      approveResult = await result.current.approve(100000000n)
    })

    expect(approveResult).toBeDefined()
    expect(Result.isError(approveResult!)).toBe(true)

    const { transactions } = useTransactionStore.getState()
    expect(transactions[0].status).toBe('failed')
  })

  it('updates transaction to failed when receipt shows reverted', async () => {
    mockWriteContractAsync.mockResolvedValue('0xrevertedhash')
    mockWaitForTransactionReceipt.mockResolvedValue({ status: 'reverted' })

    const { result } = renderHook(() => useApprove(TOKEN_ADDRESS, SPENDER_ADDRESS))
    let approveResult: Result<`0x${string}`, ApproveError> | undefined

    await act(async () => {
      approveResult = await result.current.approve(100000000n)
    })

    expect(Result.isError(approveResult!)).toBe(true)

    const { transactions } = useTransactionStore.getState()
    expect(transactions[0].status).toBe('failed')
  })

  it('uses the correct token and spender addresses from hook params', async () => {
    mockWriteContractAsync.mockResolvedValue('0xhash')

    const customToken = '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' as const
    const customSpender = '0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' as const

    const { result } = renderHook(() => useApprove(customToken, customSpender))

    await act(async () => {
      await result.current.approve(500n)
    })

    const callArgs = mockWriteContractAsync.mock.calls[0][0]
    expect(callArgs.address).toBe(customToken)
    expect(callArgs.args[0]).toBe(customSpender)
  })

  it('sets isSuccess true after successful transaction', async () => {
    mockWriteContractAsync.mockResolvedValue('0xhash')
    mockWaitForTransactionReceipt.mockResolvedValue({ status: 'success' })

    const { result } = renderHook(() => useApprove(TOKEN_ADDRESS, SPENDER_ADDRESS))

    expect(result.current.isSuccess).toBe(false)

    await act(async () => {
      await result.current.approve(100000000n)
    })

    expect(result.current.isSuccess).toBe(true)
  })

})
