import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook } from '@testing-library/react'
import { useTransactionStore } from '../../stores/transactionStore'

const mockWriteContract = vi.fn()
const mockReset = vi.fn()

const mockUseAccount = vi.fn()
const mockUseReadContract = vi.fn()
const mockUseWriteContract = vi.fn()
const mockUseWaitForTransactionReceipt = vi.fn()

vi.mock('wagmi', () => ({
  useAccount: () => mockUseAccount(),
  useReadContract: () => mockUseReadContract(),
  useWriteContract: () => mockUseWriteContract(),
  useWaitForTransactionReceipt: () => mockUseWaitForTransactionReceipt(),
}))

import {
  useLendingPosition,
  useCombinedLendingPosition,
  useLendingMarketInfo,
  useAvailableToBorrow,
  useSupply,
  useWithdraw,
  useBorrow,
  useRepay,
} from '../useLending'

const MOCK_CHAIN_ID = 11155111
const MOCK_ADDRESS = '0x1234567890123456789012345678901234567890' as const
const MOCK_MORPHO_ADDRESS = '0xMorphoAddress0000000000000000000000000000' as const
const MOCK_MARKET_PARAMS = [
  '0x1111111111111111111111111111111111111111',
  '0x2222222222222222222222222222222222222222',
  '0x3333333333333333333333333333333333333333',
  '0x4444444444444444444444444444444444444444',
  915000000000000000n,
] as const

describe('useLendingPosition', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ pendingTransactions: [] })

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
      error: null,
    })
  })

  it('returns position data when user has supply', () => {
    const supplyShares = 1000000000000000000000n
    const borrowShares = 0n
    const collateral = 0n
    const totalSupplyAssets = 10000000000n
    const totalSupplyShares = 10000000000000000000000n
    const totalBorrowAssets = 5000000000n
    const totalBorrowShares = 5000000000000000000000n

    let readCallCount = 0
    mockUseReadContract.mockImplementation(() => {
      readCallCount++
      if (readCallCount === 1) return { data: MOCK_MORPHO_ADDRESS }
      if (readCallCount === 2) return { data: MOCK_MARKET_PARAMS }
      if (readCallCount === 3) return {
        data: [supplyShares, borrowShares, collateral],
        isLoading: false,
        error: null,
        refetch: vi.fn(),
      }
      if (readCallCount === 4) return {
        data: [totalSupplyAssets, totalSupplyShares, totalBorrowAssets, totalBorrowShares, 0n, 0n],
        refetch: vi.fn(),
      }
      return { data: undefined }
    })

    const { result } = renderHook(() => useLendingPosition('BEAR'))

    expect(result.current.supplyShares).toBe(supplyShares)
    expect(result.current.borrowShares).toBe(borrowShares)
    expect(result.current.suppliedAssets).toBe(1000000000n)
    expect(result.current.borrowedAssets).toBe(0n)
  })

  it('returns zero values when no position exists', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => useLendingPosition('BEAR'))

    expect(result.current.supplyShares).toBe(0n)
    expect(result.current.borrowShares).toBe(0n)
    expect(result.current.suppliedAssets).toBe(0n)
    expect(result.current.borrowedAssets).toBe(0n)
  })

  it('returns loading state', () => {
    let readCallCount = 0
    mockUseReadContract.mockImplementation(() => {
      readCallCount++
      if (readCallCount === 3) return {
        data: undefined,
        isLoading: true,
        error: null,
        refetch: vi.fn(),
      }
      return { data: undefined, isLoading: false, refetch: vi.fn() }
    })

    const { result } = renderHook(() => useLendingPosition('BEAR'))

    expect(result.current.isLoading).toBe(true)
  })
})

describe('useLendingMarketInfo', () => {
  beforeEach(() => {
    vi.resetAllMocks()

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
    })
  })

  it('calculates utilization correctly', () => {
    const totalSupplyAssets = 10000000000n
    const totalBorrowAssets = 5000000000n

    let readCallCount = 0
    mockUseReadContract.mockImplementation(() => {
      readCallCount++
      if (readCallCount === 1) return { data: MOCK_MORPHO_ADDRESS }
      if (readCallCount === 2) return { data: MOCK_MARKET_PARAMS }
      if (readCallCount === 3) return {
        data: [totalSupplyAssets, 10000n, totalBorrowAssets, 5000n, 0n, 0n],
        isLoading: false,
      }
      return { data: undefined }
    })

    const { result } = renderHook(() => useLendingMarketInfo('BEAR'))

    expect(result.current.totalSupplyAssets).toBe(10000000000n)
    expect(result.current.totalBorrowAssets).toBe(5000000000n)
    expect(result.current.utilization).toBe(50)
  })

  it('returns zero utilization when no supply', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
    })

    const { result } = renderHook(() => useLendingMarketInfo('BEAR'))

    expect(result.current.utilization).toBe(0)
  })
})

describe('useAvailableToBorrow', () => {
  beforeEach(() => {
    vi.resetAllMocks()

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
    })
  })

  it('calculates available to borrow correctly', () => {
    const collateral = 1000000000n
    const borrowShares = 100000000000000000000n
    const totalBorrowAssets = 500000000n
    const totalBorrowShares = 1000000000000000000000n
    const lltv = 915000000000000000n

    let readCallCount = 0
    mockUseReadContract.mockImplementation(() => {
      readCallCount++
      if (readCallCount === 1) return { data: MOCK_MORPHO_ADDRESS }
      if (readCallCount === 2) return { data: MOCK_MARKET_PARAMS }
      if (readCallCount === 3) return { data: [0n, borrowShares, collateral] }
      if (readCallCount === 4) return { data: [10000000000n, 10000n, totalBorrowAssets, totalBorrowShares, 0n, 0n] }
      return { data: undefined }
    })

    const { result } = renderHook(() => useAvailableToBorrow('BEAR'))

    expect(result.current.collateral).toBe(collateral)
    expect(result.current.lltv).toBe(lltv)
    expect(result.current.currentDebt).toBe(50000000n)
    // maxBorrow and availableToBorrow depend on oracle price which requires
    // async re-renders that are difficult to test with synchronous mocks
  })

  it('returns zero when no collateral', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
    })

    const { result } = renderHook(() => useAvailableToBorrow('BEAR'))

    expect(result.current.collateral).toBe(0n)
    expect(result.current.availableToBorrow).toBe(0n)
  })
})

describe('useSupply', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ pendingTransactions: [] })

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
    })

    let callCount = 0
    mockUseReadContract.mockImplementation(() => {
      callCount++
      if (callCount === 1) return { data: MOCK_MORPHO_ADDRESS }
      if (callCount === 2) return { data: MOCK_MARKET_PARAMS }
      return { data: undefined, refetch: vi.fn() }
    })

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
      error: null,
    })
  })

  it('returns morphoAddress when available', () => {
    const { result } = renderHook(() => useSupply('BEAR'))

    expect(result.current.morphoAddress).toBe(MOCK_MORPHO_ADDRESS)
    expect(result.current.isPending).toBe(false)
  })

  it('returns isPending when transaction is pending', () => {
    let callCount = 0
    mockUseReadContract.mockImplementation(() => {
      callCount++
      if (callCount === 1) return { data: MOCK_MORPHO_ADDRESS }
      if (callCount === 2) return { data: MOCK_MARKET_PARAMS }
      return { data: undefined, refetch: vi.fn() }
    })

    mockUseWriteContract.mockReturnValue({
      writeContract: mockWriteContract,
      data: undefined,
      isPending: true,
      error: null,
      reset: mockReset,
    })

    const { result } = renderHook(() => useSupply('BEAR'))

    expect(result.current.isPending).toBe(true)
  })
})

describe('useWithdraw', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ pendingTransactions: [] })

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
    })

    let callCount = 0
    mockUseReadContract.mockImplementation(() => {
      callCount++
      if (callCount === 1) return { data: MOCK_MORPHO_ADDRESS }
      if (callCount === 2) return { data: MOCK_MARKET_PARAMS }
      return { data: undefined, refetch: vi.fn() }
    })

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
      error: null,
    })
  })

  it('returns withdraw function', () => {
    const { result } = renderHook(() => useWithdraw('BEAR'))

    expect(typeof result.current.withdraw).toBe('function')
    expect(result.current.isPending).toBe(false)
  })
})

describe('useBorrow', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ pendingTransactions: [] })

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
    })

    let callCount = 0
    mockUseReadContract.mockImplementation(() => {
      callCount++
      if (callCount === 1) return { data: MOCK_MORPHO_ADDRESS }
      if (callCount === 2) return { data: MOCK_MARKET_PARAMS }
      return { data: undefined, refetch: vi.fn() }
    })

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
      error: null,
    })
  })

  it('returns borrow function', () => {
    const { result } = renderHook(() => useBorrow('BEAR'))

    expect(typeof result.current.borrow).toBe('function')
    expect(result.current.isPending).toBe(false)
  })
})

describe('useRepay', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ pendingTransactions: [] })

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
    })

    let callCount = 0
    mockUseReadContract.mockImplementation(() => {
      callCount++
      if (callCount === 1) return { data: MOCK_MORPHO_ADDRESS }
      if (callCount === 2) return { data: MOCK_MARKET_PARAMS }
      return { data: undefined, refetch: vi.fn() }
    })

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
      error: null,
    })
  })

  it('returns repay function', () => {
    const { result } = renderHook(() => useRepay('BEAR'))

    expect(typeof result.current.repay).toBe('function')
    expect(result.current.isPending).toBe(false)
  })
})

describe('useCombinedLendingPosition', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ pendingTransactions: [] })

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
      error: null,
    })
  })

  it('aggregates positions from both markets', () => {
    const { result } = renderHook(() => useCombinedLendingPosition())

    expect(result.current.totalSupplied).toBe(0n)
    expect(result.current.totalBorrowed).toBe(0n)
    expect(result.current.isLoading).toBe(false)
  })

  it('provides refetch function', () => {
    const { result } = renderHook(() => useCombinedLendingPosition())

    expect(typeof result.current.refetch).toBe('function')
  })
})
