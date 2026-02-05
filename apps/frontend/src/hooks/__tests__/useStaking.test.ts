import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook, act, waitFor } from '@testing-library/react'
import { Result } from 'better-result'
import { useTransactionStore } from '../../stores/transactionStore'
import type { StakingError } from '../useStaking'

const mockWriteContractAsync = vi.fn()
const mockReset = vi.fn()
const mockSignTypedDataAsync = vi.fn()

const mockUseAccount = vi.fn()
const mockUseReadContract = vi.fn()
const mockUseWriteContract = vi.fn()
const mockUseWaitForTransactionReceipt = vi.fn()
const mockUseSignTypedData = vi.fn()

vi.mock('wagmi', () => ({
  useAccount: () => mockUseAccount(),
  useReadContract: () => mockUseReadContract(),
  useWriteContract: () => mockUseWriteContract(),
  useWaitForTransactionReceipt: () => mockUseWaitForTransactionReceipt(),
  useSignTypedData: () => mockUseSignTypedData(),
}))

import {
  useStakedBalance,
  useStakingInfo,
  usePreviewDeposit,
  usePreviewRedeem,
  useStake,
  useUnstake,
  useStakeWithPermit,
} from '../useStaking'

const MOCK_CHAIN_ID = 1
const MOCK_ADDRESS = '0x1234567890123456789012345678901234567890' as const

describe('useStakedBalance', () => {
  beforeEach(() => {
    vi.resetAllMocks()

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
    })

    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
      refetch: vi.fn(),
    })
  })

  it('returns shares and assets when available', () => {
    let readCallCount = 0
    mockUseReadContract.mockImplementation(() => {
      readCallCount++
      if (readCallCount === 1) {
        return { data: 1000000000000000000n, isLoading: false, refetch: vi.fn() }
      }
      return { data: 1100000000000000000n, isLoading: false, refetch: vi.fn() }
    })

    const { result } = renderHook(() => useStakedBalance('BEAR'))

    expect(result.current.shares).toBe(1000000000000000000n)
    expect(result.current.assets).toBe(1100000000000000000n)
  })

  it('returns 0n for shares when no balance', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => useStakedBalance('BULL'))

    expect(result.current.shares).toBe(0n)
  })

  it('returns loading state', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: true,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => useStakedBalance('BEAR'))

    expect(result.current.isLoading).toBe(true)
  })

  it('provides refetch function', () => {
    const mockRefetch = vi.fn()
    mockUseReadContract.mockReturnValue({
      data: 1000000000000000000n,
      isLoading: false,
      refetch: mockRefetch,
    })

    const { result } = renderHook(() => useStakedBalance('BEAR'))
    result.current.refetch()

    expect(mockRefetch).toHaveBeenCalled()
  })
})

describe('useStakingInfo', () => {
  beforeEach(() => {
    vi.resetAllMocks()

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
    })

    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
      refetch: vi.fn(),
    })
  })

  it('returns totalAssets when available', () => {
    mockUseReadContract.mockReturnValue({
      data: 10000000000000000000000n,
      isLoading: false,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => useStakingInfo('BEAR'))

    expect(result.current.totalAssets).toBe(10000000000000000000000n)
  })

  it('returns 0n when no data', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => useStakingInfo('BULL'))

    expect(result.current.totalAssets).toBe(0n)
  })
})

describe('usePreviewDeposit', () => {
  beforeEach(() => {
    vi.resetAllMocks()

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
    })

    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })
  })

  it('returns shares preview when available', () => {
    mockUseReadContract.mockReturnValue({
      data: 950000000000000000n,
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => usePreviewDeposit('BEAR', 1000000000000000000n))

    expect(result.current.shares).toBe(950000000000000000n)
  })

  it('returns 0n when no preview data', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => usePreviewDeposit('BULL', 1000000000000000000n))

    expect(result.current.shares).toBe(0n)
  })

  it('returns loading state', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: true,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => usePreviewDeposit('BEAR', 1000000000000000000n))

    expect(result.current.isLoading).toBe(true)
  })
})

describe('usePreviewRedeem', () => {
  beforeEach(() => {
    vi.resetAllMocks()

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
    })

    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })
  })

  it('returns assets preview when available', () => {
    mockUseReadContract.mockReturnValue({
      data: 1050000000000000000n,
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => usePreviewRedeem('BEAR', 1000000000000000000n))

    expect(result.current.assets).toBe(1050000000000000000n)
  })

  it('returns 0n when no preview data', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => usePreviewRedeem('BULL', 1000000000000000000n))

    expect(result.current.assets).toBe(0n)
  })
})

describe('useStake', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ transactions: [] })

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
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

  it('calls writeContractAsync with correct arguments', async () => {
    mockWriteContractAsync.mockResolvedValue('0xhash')

    const { result } = renderHook(() => useStake('BEAR'))

    await act(async () => {
      await result.current.stake(1000000000000000000n)
    })

    expect(mockWriteContractAsync).toHaveBeenCalledTimes(1)
    const callArgs = mockWriteContractAsync.mock.calls[0][0]
    expect(callArgs.functionName).toBe('deposit')
    expect(callArgs.args[0]).toBe(1000000000000000000n)
  })

  it('returns Result.ok with hash on success', async () => {
    mockWriteContractAsync.mockResolvedValue('0xstakehash')

    const { result } = renderHook(() => useStake('BULL'))
    let stakeResult: Result<`0x${string}`, StakingError> | undefined

    await act(async () => {
      stakeResult = await result.current.stake(1000000000000000000n)
    })

    expect(stakeResult).toBeDefined()
    expect(Result.isOk(stakeResult!)).toBe(true)
    if (Result.isOk(stakeResult!)) {
      expect(stakeResult.value).toBe('0xstakehash')
    }
  })

  it('returns NotConnectedError when address is missing', async () => {
    mockUseAccount.mockReturnValue({
      address: undefined,
      chainId: MOCK_CHAIN_ID,
    })

    const { result } = renderHook(() => useStake('BEAR'))
    let stakeResult: Result<`0x${string}`, StakingError> | undefined

    await act(async () => {
      stakeResult = await result.current.stake(1000000000000000000n)
    })

    expect(stakeResult).toBeDefined()
    expect(Result.isError(stakeResult!)).toBe(true)
    if (Result.isError(stakeResult!)) {
      expect(stakeResult.error._tag).toBe('NotConnectedError')
    }
  })

  it('returns Result.err when writeContractAsync fails', async () => {
    mockWriteContractAsync.mockRejectedValue(new Error('User rejected'))

    const { result } = renderHook(() => useStake('BEAR'))
    let stakeResult: Result<`0x${string}`, StakingError> | undefined

    await act(async () => {
      stakeResult = await result.current.stake(1000000000000000000n)
    })

    expect(stakeResult).toBeDefined()
    expect(Result.isError(stakeResult!)).toBe(true)
  })

  it('exposes isSuccess when receipt confirms', async () => {
    mockWriteContractAsync.mockResolvedValue('0xhash')

    const { result, rerender } = renderHook(() => useStake('BEAR'))

    await act(async () => {
      await result.current.stake(1000000000000000000n)
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
})

describe('useUnstake', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ transactions: [] })

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
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

  it('calls writeContractAsync with correct arguments', async () => {
    mockWriteContractAsync.mockResolvedValue('0xhash')

    const { result } = renderHook(() => useUnstake('BULL'))

    await act(async () => {
      await result.current.unstake(1000000000000000000n)
    })

    expect(mockWriteContractAsync).toHaveBeenCalledTimes(1)
    const callArgs = mockWriteContractAsync.mock.calls[0][0]
    expect(callArgs.functionName).toBe('redeem')
    expect(callArgs.args[0]).toBe(1000000000000000000n)
  })

  it('returns Result.ok with hash on success', async () => {
    mockWriteContractAsync.mockResolvedValue('0xunstakehash')

    const { result } = renderHook(() => useUnstake('BEAR'))
    let unstakeResult: Result<`0x${string}`, StakingError> | undefined

    await act(async () => {
      unstakeResult = await result.current.unstake(1000000000000000000n)
    })

    expect(unstakeResult).toBeDefined()
    expect(Result.isOk(unstakeResult!)).toBe(true)
    if (Result.isOk(unstakeResult!)) {
      expect(unstakeResult.value).toBe('0xunstakehash')
    }
  })

  it('returns NotConnectedError when address is missing', async () => {
    mockUseAccount.mockReturnValue({
      address: undefined,
      chainId: MOCK_CHAIN_ID,
    })

    const { result } = renderHook(() => useUnstake('BEAR'))
    let unstakeResult: Result<`0x${string}`, StakingError> | undefined

    await act(async () => {
      unstakeResult = await result.current.unstake(1000000000000000000n)
    })

    expect(unstakeResult).toBeDefined()
    expect(Result.isError(unstakeResult!)).toBe(true)
    if (Result.isError(unstakeResult!)) {
      expect(unstakeResult.error._tag).toBe('NotConnectedError')
    }
  })

  it('returns Result.err when writeContractAsync fails', async () => {
    mockWriteContractAsync.mockRejectedValue(new Error('Insufficient balance'))

    const { result } = renderHook(() => useUnstake('BULL'))
    let unstakeResult: Result<`0x${string}`, StakingError> | undefined

    await act(async () => {
      unstakeResult = await result.current.unstake(1000000000000000000n)
    })

    expect(unstakeResult).toBeDefined()
    expect(Result.isError(unstakeResult!)).toBe(true)
  })
})

describe('useStakeWithPermit', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ transactions: [] })

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
    })

    mockUseReadContract.mockReturnValue({
      data: 0n,
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

    mockUseSignTypedData.mockReturnValue({
      signTypedDataAsync: mockSignTypedDataAsync,
    })
  })

  it('returns NotConnectedError when address is missing', async () => {
    mockUseAccount.mockReturnValue({
      address: undefined,
      chainId: MOCK_CHAIN_ID,
    })

    const { result } = renderHook(() => useStakeWithPermit('BEAR'))
    let stakeResult: Result<`0x${string}`, StakingError> | undefined

    await act(async () => {
      stakeResult = await result.current.stakeWithPermit(1000000000000000000n)
    })

    expect(stakeResult).toBeDefined()
    expect(Result.isError(stakeResult!)).toBe(true)
    if (Result.isError(stakeResult!)) {
      expect(stakeResult.error._tag).toBe('NotConnectedError')
    }
  })

  it('executes with permit signature and returns Result.ok on success', async () => {
    let readCallCount = 0
    mockUseReadContract.mockImplementation(() => {
      readCallCount++
      if (readCallCount === 1) {
        return { data: 0n }
      }
      return { data: 'plDXY-BEAR' }
    })

    mockSignTypedDataAsync.mockResolvedValue(
      '0x' + '1'.repeat(64) + '2'.repeat(64) + '1b'
    )

    mockWriteContractAsync.mockResolvedValue('0xstakepermithash')

    const { result } = renderHook(() => useStakeWithPermit('BEAR'))
    let stakeResult: Result<`0x${string}`, StakingError> | undefined

    await act(async () => {
      stakeResult = await result.current.stakeWithPermit(1000000000000000000n)
    })

    expect(stakeResult).toBeDefined()
    expect(Result.isOk(stakeResult!)).toBe(true)
    expect(mockSignTypedDataAsync).toHaveBeenCalled()
  })

  it('handles signature rejection', async () => {
    let readCallCount = 0
    mockUseReadContract.mockImplementation(() => {
      readCallCount++
      if (readCallCount === 1) {
        return { data: 0n }
      }
      return { data: 'plDXY-BULL' }
    })

    mockSignTypedDataAsync.mockRejectedValue(new Error('User rejected'))

    const { result } = renderHook(() => useStakeWithPermit('BULL'))
    let stakeResult: Result<`0x${string}`, StakingError> | undefined

    await act(async () => {
      stakeResult = await result.current.stakeWithPermit(1000000000000000000n)
    })

    expect(stakeResult).toBeDefined()
    expect(Result.isError(stakeResult!)).toBe(true)
  })

  it('provides reset function that clears permit state', async () => {
    let readCallCount = 0
    mockUseReadContract.mockImplementation(() => {
      readCallCount++
      if (readCallCount === 1) {
        return { data: 0n }
      }
      return { data: 'plDXY-BEAR' }
    })

    mockSignTypedDataAsync.mockRejectedValue(new Error('User rejected'))

    const { result } = renderHook(() => useStakeWithPermit('BEAR'))

    await act(async () => {
      await result.current.stakeWithPermit(1000000000000000000n)
    })

    expect(result.current.error).toBeDefined()

    act(() => {
      result.current.reset()
    })

    expect(mockReset).toHaveBeenCalled()
  })
})
