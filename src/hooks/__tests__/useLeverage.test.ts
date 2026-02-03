import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook, act, waitFor } from '@testing-library/react'
import { Result } from 'better-result'
import { useTransactionStore } from '../../stores/transactionStore'
import type { LeverageError } from '../useLeverage'

const mockWriteContract = vi.fn()
const mockWriteContractAsync = vi.fn()
const mockReset = vi.fn()

const mockUseAccount = vi.fn()
const mockUseReadContract = vi.fn()
const mockUseWriteContract = vi.fn()
const mockUseWaitForTransactionReceipt = vi.fn()
const mockWaitForTransactionReceipt = vi.fn()

vi.mock('wagmi', () => ({
  useAccount: () => mockUseAccount(),
  useReadContract: () => mockUseReadContract(),
  useWriteContract: () => mockUseWriteContract(),
  useWaitForTransactionReceipt: () => mockUseWaitForTransactionReceipt(),
  usePublicClient: () => ({
    waitForTransactionReceipt: mockWaitForTransactionReceipt,
  }),
}))

import {
  useLeveragePosition,
  usePreviewOpenLeverage,
  useOpenLeverage,
  useCloseLeverage,
  useAdjustCollateral,
} from '../useLeverage'

const MOCK_CHAIN_ID = 1
const MOCK_ADDRESS = '0x1234567890123456789012345678901234567890' as const

const MOCK_MORPHO_ADDRESS = '0xMorphoAddress0000000000000000000000000000' as const
const MOCK_MARKET_PARAMS = [
  '0x1111111111111111111111111111111111111111',
  '0x2222222222222222222222222222222222222222',
  '0x3333333333333333333333333333333333333333',
  '0x4444444444444444444444444444444444444444',
  800000000000000000n,
] as const
const MOCK_ORACLE_ROUND_DATA = [1n, 100000000n, 0n, 0n, 1n] as const // price = 1.00 (8 decimals)
const MOCK_CAP = 200000000n // CAP = 2.00 (8 decimals)

describe('useLeveragePosition', () => {
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

  it('returns position data when available', () => {
    // Morpho position: [supplyShares, borrowShares, collateral]
    // Collateral: 200e21 shares (200 tokens with 1000x offset)
    const morphoPosition = [0n, 0n, 200000000000000000000000n]
    const debt = 100000000n // 100 USDC (6 decimals)

    let readCallCount = 0
    mockUseReadContract.mockImplementation(() => {
      readCallCount++
      if (readCallCount === 1) return { data: MOCK_MORPHO_ADDRESS } // MORPHO()
      if (readCallCount === 2) return { data: MOCK_MARKET_PARAMS } // marketParams()
      if (readCallCount === 3) return { data: morphoPosition, isLoading: false, error: null, refetch: vi.fn() } // position()
      if (readCallCount === 4) return { data: debt, isLoading: false } // getActualDebt()
      if (readCallCount === 5) return { data: MOCK_ORACLE_ROUND_DATA } // latestRoundData()
      if (readCallCount === 6) return { data: MOCK_CAP } // CAP()
      return { data: undefined }
    })

    const { result } = renderHook(() => useLeveragePosition('BEAR'))

    expect(result.current.collateral).toBe(200000000000000000000000n)
    expect(result.current.debt).toBe(100000000n)
    // BEAR price = CAP - oracle = 2.00 - 1.00 = 1.00
    // collateralUsdc = 200e21 * 1e8 / 10^23 = 200e6 = $200
    // equity = 200e6 - 100e6 = 100e6 = $100
    // leverage = 200e6 * 100 / 100e6 = 200 (2x)
    expect(result.current.leverage).toBe(200n)
  })

  it('returns default values when no position exists', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => useLeveragePosition('BULL'))

    expect(result.current.collateral).toBe(0n)
    expect(result.current.debt).toBe(0n)
    expect(result.current.leverage).toBe(0n)
    expect(result.current.healthFactor).toBe(0)
    expect(result.current.liquidationPrice).toBe(0n)
    expect(result.current.hasPosition).toBe(false)
  })

  it('returns hasPosition true when collateral > 0', () => {
    // Morpho position with collateral
    const morphoPosition = [0n, 0n, 200000000000000000000000n]

    let readCallCount = 0
    mockUseReadContract.mockImplementation(() => {
      readCallCount++
      if (readCallCount === 1) return { data: MOCK_MORPHO_ADDRESS }
      if (readCallCount === 2) return { data: MOCK_MARKET_PARAMS }
      if (readCallCount === 3) return { data: morphoPosition, isLoading: false, error: null, refetch: vi.fn() }
      if (readCallCount === 4) return { data: 0n, isLoading: false }
      if (readCallCount === 5) return { data: MOCK_ORACLE_ROUND_DATA }
      if (readCallCount === 6) return { data: MOCK_CAP }
      return { data: undefined }
    })

    const { result } = renderHook(() => useLeveragePosition('BEAR'))

    expect(result.current.hasPosition).toBe(true)
  })

  it('returns loading state', () => {
    let readCallCount = 0
    mockUseReadContract.mockImplementation(() => {
      readCallCount++
      if (readCallCount === 3) return { data: undefined, isLoading: true, error: null, refetch: vi.fn() }
      if (readCallCount === 4) return { data: undefined, isLoading: true }
      return { data: undefined }
    })

    const { result } = renderHook(() => useLeveragePosition('BEAR'))

    expect(result.current.isLoading).toBe(true)
  })
})

describe('usePreviewOpenLeverage', () => {
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

  it('returns preview data when available', () => {
    const previewData = [
      2000000000000000000n,
      3000000000000000000n,
      1500000000000000000n,
      1000000000000000000n,
    ]

    mockUseReadContract.mockReturnValue({
      data: previewData,
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() =>
      usePreviewOpenLeverage('BEAR', 1000000000000000000n, 2000000000000000000n)
    )

    expect(result.current.loanAmount).toBe(2000000000000000000n)
    expect(result.current.totalUSDC).toBe(3000000000000000000n)
    expect(result.current.expectedCollateralTokens).toBe(1500000000000000000n)
    expect(result.current.expectedDebt).toBe(1000000000000000000n)
  })

  it('returns default values when no data', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() =>
      usePreviewOpenLeverage('BULL', 1000000000000000000n, 2000000000000000000n)
    )

    expect(result.current.loanAmount).toBe(0n)
    expect(result.current.totalUSDC).toBe(0n)
    expect(result.current.expectedCollateralTokens).toBe(0n)
    expect(result.current.expectedDebt).toBe(0n)
  })
})

describe('useOpenLeverage', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ transactions: [] })

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
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
    })

  })

  it('adds pending transaction when openPosition is called', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xhash')
    })

    const { result } = renderHook(() => useOpenLeverage('BEAR'))
    const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)

    await act(async () => {
      await result.current.openPosition(1000000000000000000n, 2000000000000000000n, 100n, deadline)
    })

    const { transactions } = useTransactionStore.getState()
    expect(transactions).toHaveLength(1)
    expect(transactions[0].type).toBe('leverage')
    expect(transactions[0].title).toBe('Opening BEAR leverage position')
  })

  it('returns Result.ok with hash on success', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xleveragehash')
    })

    const { result } = renderHook(() => useOpenLeverage('BULL'))
    const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)
    let leverageResult: Result<`0x${string}`, LeverageError> | undefined

    await act(async () => {
      leverageResult = await result.current.openPosition(
        1000000000000000000n,
        2000000000000000000n,
        100n,
        deadline
      )
    })

    expect(leverageResult).toBeDefined()
    expect(Result.isOk(leverageResult!)).toBe(true)
    if (Result.isOk(leverageResult!)) {
      expect(leverageResult.value).toBe('0xleveragehash')
    }
  })

  it('returns NotConnectedError when chainId is missing', async () => {
    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: undefined,
    })

    const { result } = renderHook(() => useOpenLeverage('BEAR'))
    const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)
    let leverageResult: Result<`0x${string}`, LeverageError> | undefined

    await act(async () => {
      leverageResult = await result.current.openPosition(
        1000000000000000000n,
        2000000000000000000n,
        100n,
        deadline
      )
    })

    expect(leverageResult).toBeDefined()
    expect(Result.isError(leverageResult!)).toBe(true)
    if (Result.isError(leverageResult!)) {
      expect(leverageResult.error._tag).toBe('NotConnectedError')
    }
  })

  it('returns Result.err when writeContract fails', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onError(new Error('User rejected'))
    })

    const { result } = renderHook(() => useOpenLeverage('BEAR'))
    const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)
    let leverageResult: Result<`0x${string}`, LeverageError> | undefined

    await act(async () => {
      leverageResult = await result.current.openPosition(
        1000000000000000000n,
        2000000000000000000n,
        100n,
        deadline
      )
    })

    expect(leverageResult).toBeDefined()
    expect(Result.isError(leverageResult!)).toBe(true)

    const { transactions } = useTransactionStore.getState()
    expect(transactions[0].status).toBe('failed')
  })

  it('updates transaction to success when receipt confirms', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xhash')
    })

    const { result, rerender } = renderHook(() => useOpenLeverage('BEAR'))
    const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)

    await act(async () => {
      await result.current.openPosition(1000000000000000000n, 2000000000000000000n, 100n, deadline)
    })

    mockUseWaitForTransactionReceipt.mockReturnValue({
      isLoading: false,
      isSuccess: true,
      isError: false,
    })
    rerender()

    await waitFor(() => {
      const { transactions } = useTransactionStore.getState()
      expect(transactions[0].status).toBe('success')
    })
  })
})

describe('useCloseLeverage', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ transactions: [] })

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
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
    })

  })

  it('adds pending transaction for close position', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xhash')
    })

    const { result } = renderHook(() => useCloseLeverage('BULL'))
    const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)

    await act(async () => {
      await result.current.closePosition(
        1000000000000000000n,
        100n,
        deadline
      )
    })

    const { transactions } = useTransactionStore.getState()
    expect(transactions).toHaveLength(1)
    expect(transactions[0].type).toBe('leverage')
    expect(transactions[0].title).toBe('Closing BULL leverage position')
  })

  it('returns Result.ok on success', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xclosehash')
    })

    const { result } = renderHook(() => useCloseLeverage('BEAR'))
    const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)
    let closeResult: Result<`0x${string}`, LeverageError> | undefined

    await act(async () => {
      closeResult = await result.current.closePosition(
        1000000000000000000n,
        100n,
        deadline
      )
    })

    expect(closeResult).toBeDefined()
    expect(Result.isOk(closeResult!)).toBe(true)
  })
})

describe('useAdjustCollateral', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ transactions: [] })

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
    })

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

  it('adds collateral and creates transaction', async () => {
    mockWriteContractAsync.mockResolvedValue('0xaddcollateralhash')

    const { result } = renderHook(() => useAdjustCollateral('BEAR'))
    let adjustResult: Result<`0x${string}`, LeverageError> | undefined

    await act(async () => {
      adjustResult = await result.current.addCollateral(500000000000000000n)
    })

    expect(adjustResult).toBeDefined()
    expect(Result.isOk(adjustResult!)).toBe(true)

    const { transactions } = useTransactionStore.getState()
    expect(transactions[0].title).toBe('Adding collateral')
  })

  it('removes collateral and creates transaction', async () => {
    mockWriteContractAsync.mockResolvedValue('0xremovecollateralhash')

    const { result } = renderHook(() => useAdjustCollateral('BULL'))
    let adjustResult: Result<`0x${string}`, LeverageError> | undefined

    await act(async () => {
      adjustResult = await result.current.removeCollateral(250000000000000000n)
    })

    expect(adjustResult).toBeDefined()
    expect(Result.isOk(adjustResult!)).toBe(true)

    const { transactions } = useTransactionStore.getState()
    expect(transactions[0].title).toBe('Removing collateral')
  })

  it('returns NotConnectedError when chainId is missing for addCollateral', async () => {
    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: undefined,
    })

    const { result } = renderHook(() => useAdjustCollateral('BEAR'))
    let adjustResult: Result<`0x${string}`, LeverageError> | undefined

    await act(async () => {
      adjustResult = await result.current.addCollateral(500000000000000000n)
    })

    expect(adjustResult).toBeDefined()
    expect(Result.isError(adjustResult!)).toBe(true)
    if (Result.isError(adjustResult!)) {
      expect(adjustResult.error._tag).toBe('NotConnectedError')
    }
  })

  it('returns NotConnectedError when chainId is missing for removeCollateral', async () => {
    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: undefined,
    })

    const { result } = renderHook(() => useAdjustCollateral('BULL'))
    let adjustResult: Result<`0x${string}`, LeverageError> | undefined

    await act(async () => {
      adjustResult = await result.current.removeCollateral(250000000000000000n)
    })

    expect(adjustResult).toBeDefined()
    expect(Result.isError(adjustResult!)).toBe(true)
    if (Result.isError(adjustResult!)) {
      expect(adjustResult.error._tag).toBe('NotConnectedError')
    }
  })

  it('handles writeContractAsync failure for addCollateral', async () => {
    mockWriteContractAsync.mockRejectedValue(new Error('Insufficient funds'))

    const { result } = renderHook(() => useAdjustCollateral('BEAR'))
    let adjustResult: Result<`0x${string}`, LeverageError> | undefined

    await act(async () => {
      adjustResult = await result.current.addCollateral(500000000000000000n)
    })

    expect(adjustResult).toBeDefined()
    expect(Result.isError(adjustResult!)).toBe(true)

    const { transactions } = useTransactionStore.getState()
    expect(transactions[0].status).toBe('failed')
  })

  it('handles transaction revert', async () => {
    mockWriteContractAsync.mockResolvedValue('0xrevertedhash')
    mockWaitForTransactionReceipt.mockResolvedValue({
      status: 'reverted',
    })

    const { result } = renderHook(() => useAdjustCollateral('BEAR'))
    let adjustResult: Result<`0x${string}`, LeverageError> | undefined

    await act(async () => {
      adjustResult = await result.current.addCollateral(500000000000000000n)
    })

    expect(adjustResult).toBeDefined()
    expect(Result.isError(adjustResult!)).toBe(true)

    const { transactions } = useTransactionStore.getState()
    expect(transactions[0].status).toBe('failed')
  })
})
