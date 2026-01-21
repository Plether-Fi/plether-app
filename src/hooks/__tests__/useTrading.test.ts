import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook, act, waitFor } from '@testing-library/react'
import { Result } from 'better-result'
import { useTransactionStore } from '../../stores/transactionStore'
import type { SwapError } from '../useTrading'

const mockWriteContract = vi.fn()
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
  useCurveQuote,
  useCurveSwap,
  useZapQuote,
  useZapSwap,
  useZapBuyWithPermit,
  useZapSellWithPermit,
} from '../useTrading'

const MOCK_CHAIN_ID = 1
const MOCK_ADDRESS = '0x1234567890123456789012345678901234567890' as const

describe('useCurveQuote', () => {
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

  it('returns amountOut when quote succeeds', () => {
    mockUseReadContract.mockReturnValue({
      data: 1000000000000000000n,
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => useCurveQuote('USDC', 1000000n))

    expect(result.current.amountOut).toBe(1000000000000000000n)
    expect(result.current.isLoading).toBe(false)
  })

  it('returns 0n when no data available', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => useCurveQuote('USDC', 1000000n))

    expect(result.current.amountOut).toBe(0n)
  })

  it('returns loading state while fetching', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: true,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => useCurveQuote('BEAR', 1000000000000000000n))

    expect(result.current.isLoading).toBe(true)
  })

  it('calculates positive price impact when actual rate is worse than spot rate', () => {
    let callCount = 0
    mockUseReadContract.mockImplementation(() => {
      callCount++
      if (callCount === 1) {
        return { data: 900000000000000000n, isLoading: false, error: null, refetch: vi.fn() }
      }
      return { data: 1000000000000000000n, isLoading: false, error: null, refetch: vi.fn() }
    })

    const { result } = renderHook(() => useCurveQuote('USDC', 1000000n))
    expect(result.current.priceImpact).toBeGreaterThanOrEqual(0)
  })
})

describe('useCurveSwap', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ pendingTransactions: [] })

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

  it('adds a pending transaction when swap is called', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xhash')
    })

    const { result } = renderHook(() => useCurveSwap())

    await act(async () => {
      await result.current.swap('USDC', 1000000n, 900000000000000000n)
    })

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions).toHaveLength(1)
    expect(pendingTransactions[0].type).toBe('swap')
    expect(pendingTransactions[0].description).toBe('Swapping USDC for DXY-BEAR')
  })

  it('returns Result.ok with hash when swap succeeds', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xswaphash123')
    })

    const { result } = renderHook(() => useCurveSwap())
    let swapResult: Result<`0x${string}`, SwapError> | undefined

    await act(async () => {
      swapResult = await result.current.swap('USDC', 1000000n, 900000000000000000n)
    })

    expect(swapResult).toBeDefined()
    expect(Result.isOk(swapResult!)).toBe(true)
    if (Result.isOk(swapResult!)) {
      expect(swapResult.value).toBe('0xswaphash123')
    }
  })

  it('returns Result.err when user rejects transaction', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onError(new Error('User rejected'))
    })

    const { result } = renderHook(() => useCurveSwap())
    let swapResult: Result<`0x${string}`, SwapError> | undefined

    await act(async () => {
      swapResult = await result.current.swap('USDC', 1000000n, 900000000000000000n)
    })

    expect(swapResult).toBeDefined()
    expect(Result.isError(swapResult!)).toBe(true)

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions[0].status).toBe('failed')
  })

  it('returns NotConnectedError when address is missing', async () => {
    mockUseAccount.mockReturnValue({
      address: undefined,
      chainId: MOCK_CHAIN_ID,
    })

    const { result } = renderHook(() => useCurveSwap())
    let swapResult: Result<`0x${string}`, SwapError> | undefined

    await act(async () => {
      swapResult = await result.current.swap('USDC', 1000000n, 900000000000000000n)
    })

    expect(swapResult).toBeDefined()
    expect(Result.isError(swapResult!)).toBe(true)
    if (Result.isError(swapResult!)) {
      expect(swapResult.error._tag).toBe('NotConnectedError')
    }
  })

  it('updates transaction status to confirming on success', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xhash')
    })

    const { result } = renderHook(() => useCurveSwap())

    await act(async () => {
      await result.current.swap('BEAR', 1000000000000000000n, 900000n)
    })

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions[0].status).toBe('confirming')
    expect(pendingTransactions[0].hash).toBe('0xhash')
    expect(pendingTransactions[0].description).toBe('Swapping DXY-BEAR for USDC')
  })

  it('updates transaction to success when receipt confirms', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xhash')
    })

    const { result, rerender } = renderHook(() => useCurveSwap())

    await act(async () => {
      await result.current.swap('USDC', 1000000n, 900000000000000000n)
    })

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
})

describe('useZapQuote', () => {
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

  it('returns amountOut for buy direction', () => {
    mockUseReadContract.mockReturnValue({
      data: [0n, 0n, 0n, 2000000000000000000n],
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => useZapQuote('buy', 1000000n))

    expect(result.current.amountOut).toBe(2000000000000000000n)
  })

  it('returns amountOut for sell direction', () => {
    mockUseReadContract.mockReturnValue({
      data: [0n, 0n, 500000n],
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => useZapQuote('sell', 1000000000000000000n))

    expect(result.current.amountOut).toBe(500000n)
  })

  it('returns 0n when no data available', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => useZapQuote('buy', 1000000n))

    expect(result.current.amountOut).toBe(0n)
  })
})

describe('useZapSwap', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ pendingTransactions: [] })

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

  it('executes zapBuy and returns Result.ok on success', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xzapbuyhash')
    })

    const { result } = renderHook(() => useZapSwap())
    let zapResult: Result<`0x${string}`, SwapError> | undefined

    await act(async () => {
      const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)
      zapResult = await result.current.zapBuy(1000000n, 900000000000000000n, 100n, deadline)
    })

    expect(zapResult).toBeDefined()
    expect(Result.isOk(zapResult!)).toBe(true)

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions[0].description).toBe('Swapping USDC for DXY-BULL')
  })

  it('executes zapSell and returns Result.ok on success', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xzapsellhash')
    })

    const { result } = renderHook(() => useZapSwap())
    let zapResult: Result<`0x${string}`, SwapError> | undefined

    await act(async () => {
      const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)
      zapResult = await result.current.zapSell(1000000000000000000n, 900000n, deadline)
    })

    expect(zapResult).toBeDefined()
    expect(Result.isOk(zapResult!)).toBe(true)

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions[0].description).toBe('Swapping DXY-BULL for USDC')
  })

  it('returns NotConnectedError when chainId is missing', async () => {
    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: undefined,
    })

    const { result } = renderHook(() => useZapSwap())
    let zapResult: Result<`0x${string}`, SwapError> | undefined

    await act(async () => {
      const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)
      zapResult = await result.current.zapBuy(1000000n, 900000000000000000n, 100n, deadline)
    })

    expect(zapResult).toBeDefined()
    expect(Result.isError(zapResult!)).toBe(true)
    if (Result.isError(zapResult!)) {
      expect(zapResult.error._tag).toBe('NotConnectedError')
    }
  })

  it('returns Result.err when writeContract fails', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onError(new Error('Insufficient balance'))
    })

    const { result } = renderHook(() => useZapSwap())
    let zapResult: Result<`0x${string}`, SwapError> | undefined

    await act(async () => {
      const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)
      zapResult = await result.current.zapBuy(1000000n, 900000000000000000n, 100n, deadline)
    })

    expect(zapResult).toBeDefined()
    expect(Result.isError(zapResult!)).toBe(true)

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions[0].status).toBe('failed')
  })
})

describe('useZapBuyWithPermit', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ pendingTransactions: [] })

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
    })

    mockUseReadContract.mockReturnValue({
      data: 0n,
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

    mockUseSignTypedData.mockReturnValue({
      signTypedDataAsync: mockSignTypedDataAsync,
    })
  })

  it('returns NotConnectedError when address is missing', async () => {
    mockUseAccount.mockReturnValue({
      address: undefined,
      chainId: MOCK_CHAIN_ID,
    })

    const { result } = renderHook(() => useZapBuyWithPermit())
    let zapResult: Result<`0x${string}`, SwapError> | undefined

    await act(async () => {
      zapResult = await result.current.zapBuyWithPermit(1000000n, 900000000000000000n, 100n)
    })

    expect(zapResult).toBeDefined()
    expect(Result.isError(zapResult!)).toBe(true)
    if (Result.isError(zapResult!)) {
      expect(zapResult.error._tag).toBe('NotConnectedError')
    }
  })

  it('executes with permit signature and returns Result.ok on success', async () => {
    let readCallCount = 0
    mockUseReadContract.mockImplementation(() => {
      readCallCount++
      if (readCallCount === 1) {
        return { data: 0n }
      }
      return { data: 'USD Coin' }
    })

    mockSignTypedDataAsync.mockResolvedValue(
      '0x' + '1'.repeat(64) + '2'.repeat(64) + '1b'
    )

    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xpermithash')
    })

    const { result } = renderHook(() => useZapBuyWithPermit())
    let zapResult: Result<`0x${string}`, SwapError> | undefined

    await act(async () => {
      zapResult = await result.current.zapBuyWithPermit(1000000n, 900000000000000000n, 100n)
    })

    expect(zapResult).toBeDefined()
    expect(Result.isOk(zapResult!)).toBe(true)
    expect(mockSignTypedDataAsync).toHaveBeenCalled()
  })
})

describe('useZapSellWithPermit', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.setState({ pendingTransactions: [] })

    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: MOCK_CHAIN_ID,
    })

    mockUseReadContract.mockReturnValue({
      data: 0n,
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

    mockUseSignTypedData.mockReturnValue({
      signTypedDataAsync: mockSignTypedDataAsync,
    })
  })

  it('returns NotConnectedError when chainId is missing', async () => {
    mockUseAccount.mockReturnValue({
      address: MOCK_ADDRESS,
      chainId: undefined,
    })

    const { result } = renderHook(() => useZapSellWithPermit())
    let zapResult: Result<`0x${string}`, SwapError> | undefined

    await act(async () => {
      zapResult = await result.current.zapSellWithPermit(1000000000000000000n, 900000n)
    })

    expect(zapResult).toBeDefined()
    expect(Result.isError(zapResult!)).toBe(true)
    if (Result.isError(zapResult!)) {
      expect(zapResult.error._tag).toBe('NotConnectedError')
    }
  })

  it('adds transaction with correct description', async () => {
    let readCallCount = 0
    mockUseReadContract.mockImplementation(() => {
      readCallCount++
      if (readCallCount === 1) {
        return { data: 0n }
      }
      return { data: 'DXY-BULL' }
    })

    mockSignTypedDataAsync.mockResolvedValue(
      '0x' + '1'.repeat(64) + '2'.repeat(64) + '1b'
    )

    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xsellpermithash')
    })

    const { result } = renderHook(() => useZapSellWithPermit())

    await act(async () => {
      await result.current.zapSellWithPermit(1000000000000000000n, 900000n)
    })

    const { pendingTransactions } = useTransactionStore.getState()
    expect(pendingTransactions[0].description).toBe('Swapping DXY-BULL for USDC')
  })
})
