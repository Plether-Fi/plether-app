import { describe, it, expect, vi, beforeEach, type Mock } from 'vitest'
import { renderHook, act } from '@testing-library/react'
import { type ReactNode } from 'react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { Result } from 'better-result'
import type { MintError, BurnError } from '../usePlethCore'

// Mock wagmi before importing hooks
const mockWriteContract = vi.fn()
const mockUseWriteContract = vi.fn()
const mockUseWaitForTransactionReceipt = vi.fn()
const mockUseAccount = vi.fn()
const mockUseReadContract = vi.fn()

vi.mock('wagmi', () => ({
  useAccount: () => mockUseAccount(),
  useWriteContract: () => mockUseWriteContract(),
  useWaitForTransactionReceipt: () => mockUseWaitForTransactionReceipt(),
  useReadContract: () => mockUseReadContract(),
}))

// Mock the transaction store
const mockAddTransaction = vi.fn()
const mockUpdateTransaction = vi.fn()

vi.mock('../../stores/transactionStore', () => ({
  useTransactionStore: (selector: (state: { addTransaction: Mock; updateTransaction: Mock }) => unknown) =>
    selector({ addTransaction: mockAddTransaction, updateTransaction: mockUpdateTransaction }),
}))

// Mock crypto.randomUUID
vi.stubGlobal('crypto', {
  randomUUID: () => 'test-uuid-123',
})

// Import hooks after mocking
import { useMint, useBurn, usePreviewMint, usePreviewBurn } from '../usePlethCore'

function createWrapper() {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  })
  return function Wrapper({ children }: { children: ReactNode }) {
    return <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
  }
}

describe('useMint', () => {
  beforeEach(() => {
    vi.clearAllMocks()

    // Default mock implementations
    mockUseAccount.mockReturnValue({ chainId: 11155111 })
    mockUseWriteContract.mockReturnValue({
      writeContract: mockWriteContract,
      data: undefined,
      isPending: false,
      error: null,
      reset: vi.fn(),
    })
    mockUseWaitForTransactionReceipt.mockReturnValue({
      isLoading: false,
      isSuccess: false,
      isError: false,
      error: null,
    })
  })

  it('returns initial state correctly', () => {
    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })

    expect(result.current.isPending).toBe(false)
    expect(result.current.isConfirming).toBe(false)
    expect(result.current.isSuccess).toBe(false)
    expect(result.current.error).toBeNull()
    expect(result.current.hash).toBeUndefined()
    expect(typeof result.current.mint).toBe('function')
  })

  it('returns Result.err when chainId is undefined', async () => {
    mockUseAccount.mockReturnValue({ chainId: undefined })

    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })
    let mintResult: Result<`0x${string}`, MintError> | undefined

    await act(async () => {
      mintResult = await result.current.mint(1000n)
    })

    expect(mintResult).toBeDefined()
    expect(Result.isError(mintResult!)).toBe(true)
    expect(mockWriteContract).not.toHaveBeenCalled()
    expect(mockAddTransaction).not.toHaveBeenCalled()
  })

  it('calls writeContract with correct parameters and returns Result.ok', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xhash')
    })

    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })
    let mintResult: Result<`0x${string}`, MintError> | undefined

    await act(async () => {
      mintResult = await result.current.mint(1000n)
    })

    expect(mintResult).toBeDefined()
    expect(Result.isOk(mintResult!)).toBe(true)
    expect((mintResult as { value: string }).value).toBe('0xhash')

    expect(mockAddTransaction).toHaveBeenCalledWith({
      id: 'test-uuid-123',
      type: 'mint',
      status: 'pending',
      hash: undefined,
      title: 'Minting plDXY-BEAR + plDXY-BULL',
      steps: [{ label: 'Mint pairs', status: 'pending' }],
    })

    expect(mockWriteContract).toHaveBeenCalledWith(
      expect.objectContaining({
        functionName: 'mint',
        args: [1000n],
      }),
      expect.objectContaining({
        onSuccess: expect.any(Function),
        onError: expect.any(Function),
      })
    )
  })

  it('updates transaction on success callback and returns Result.ok', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xabcd1234')
    })

    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })
    let mintResult: Result<`0x${string}`, MintError> | undefined

    await act(async () => {
      mintResult = await result.current.mint(1000n)
    })

    expect(mintResult).toBeDefined()
    expect(Result.isOk(mintResult!)).toBe(true)
    expect((mintResult as { value: string }).value).toBe('0xabcd1234')

    expect(mockUpdateTransaction).toHaveBeenCalledWith('test-uuid-123', {
      hash: '0xabcd1234',
      status: 'confirming',
    })
  })

  it('updates transaction on error callback and returns Result.err', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onError(new Error('User rejected'))
    })

    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })
    let mintResult: Result<`0x${string}`, MintError> | undefined

    await act(async () => {
      mintResult = await result.current.mint(1000n)
    })

    expect(mintResult).toBeDefined()
    expect(Result.isError(mintResult!)).toBe(true)

    expect(mockUpdateTransaction).toHaveBeenCalledWith('test-uuid-123', {
      status: 'failed',
      errorMessage: expect.any(String),
    })
  })

  it('reflects isPending state from useWriteContract', () => {
    mockUseWriteContract.mockReturnValue({
      writeContract: mockWriteContract,
      data: undefined,
      isPending: true,
      error: null,
      reset: vi.fn(),
    })

    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })

    expect(result.current.isPending).toBe(true)
  })

  it('reflects isConfirming state from useWaitForTransactionReceipt', () => {
    mockUseWriteContract.mockReturnValue({
      writeContract: mockWriteContract,
      data: '0xhash',
      isPending: false,
      error: null,
      reset: vi.fn(),
    })
    mockUseWaitForTransactionReceipt.mockReturnValue({
      isLoading: true,
      isSuccess: false,
      isError: false,
      error: null,
    })

    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })

    expect(result.current.isConfirming).toBe(true)
    expect(result.current.hash).toBe('0xhash')
  })

  it('reflects isSuccess state from useWaitForTransactionReceipt', () => {
    mockUseWriteContract.mockReturnValue({
      writeContract: mockWriteContract,
      data: '0xhash',
      isPending: false,
      error: null,
      reset: vi.fn(),
    })
    mockUseWaitForTransactionReceipt.mockReturnValue({
      isLoading: false,
      isSuccess: true,
      isError: false,
      error: null,
    })

    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })

    expect(result.current.isSuccess).toBe(true)
  })
})

describe('useBurn', () => {
  beforeEach(() => {
    vi.clearAllMocks()

    mockUseAccount.mockReturnValue({ chainId: 11155111 })
    mockUseWriteContract.mockReturnValue({
      writeContract: mockWriteContract,
      data: undefined,
      isPending: false,
      error: null,
      reset: vi.fn(),
    })
    mockUseWaitForTransactionReceipt.mockReturnValue({
      isLoading: false,
      isSuccess: false,
      isError: false,
      error: null,
    })
  })

  it('returns initial state correctly', () => {
    const { result } = renderHook(() => useBurn(), { wrapper: createWrapper() })

    expect(result.current.isPending).toBe(false)
    expect(result.current.isConfirming).toBe(false)
    expect(result.current.isSuccess).toBe(false)
    expect(result.current.error).toBeNull()
    expect(result.current.hash).toBeUndefined()
    expect(typeof result.current.burn).toBe('function')
  })

  it('returns Result.err when chainId is undefined', async () => {
    mockUseAccount.mockReturnValue({ chainId: undefined })

    const { result } = renderHook(() => useBurn(), { wrapper: createWrapper() })
    let burnResult: Result<`0x${string}`, BurnError> | undefined

    await act(async () => {
      burnResult = await result.current.burn(1000n)
    })

    expect(burnResult).toBeDefined()
    expect(Result.isError(burnResult!)).toBe(true)
    expect(mockWriteContract).not.toHaveBeenCalled()
    expect(mockAddTransaction).not.toHaveBeenCalled()
  })

  it('calls writeContract with correct parameters and returns Result.ok', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xhash')
    })

    const { result } = renderHook(() => useBurn(), { wrapper: createWrapper() })
    let burnResult: Result<`0x${string}`, BurnError> | undefined

    await act(async () => {
      burnResult = await result.current.burn(1000n)
    })

    expect(burnResult).toBeDefined()
    expect(Result.isOk(burnResult!)).toBe(true)
    expect((burnResult as { value: string }).value).toBe('0xhash')

    expect(mockAddTransaction).toHaveBeenCalledWith({
      id: 'test-uuid-123',
      type: 'burn',
      status: 'pending',
      hash: undefined,
      title: 'Redeeming plDXY-BEAR + plDXY-BULL for USDC',
      steps: [{ label: 'Redeem pairs', status: 'pending' }],
    })

    expect(mockWriteContract).toHaveBeenCalledWith(
      expect.objectContaining({
        functionName: 'burn',
        args: [1000n],
      }),
      expect.objectContaining({
        onSuccess: expect.any(Function),
        onError: expect.any(Function),
      })
    )
  })

  it('updates transaction on success callback and returns Result.ok', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onSuccess('0xabcd1234')
    })

    const { result } = renderHook(() => useBurn(), { wrapper: createWrapper() })
    let burnResult: Result<`0x${string}`, BurnError> | undefined

    await act(async () => {
      burnResult = await result.current.burn(1000n)
    })

    expect(burnResult).toBeDefined()
    expect(Result.isOk(burnResult!)).toBe(true)
    expect((burnResult as { value: string }).value).toBe('0xabcd1234')

    expect(mockUpdateTransaction).toHaveBeenCalledWith('test-uuid-123', {
      hash: '0xabcd1234',
      status: 'confirming',
    })
  })

  it('updates transaction on error callback and returns Result.err', async () => {
    mockWriteContract.mockImplementation((_, callbacks) => {
      callbacks.onError(new Error('User rejected'))
    })

    const { result } = renderHook(() => useBurn(), { wrapper: createWrapper() })
    let burnResult: Result<`0x${string}`, BurnError> | undefined

    await act(async () => {
      burnResult = await result.current.burn(1000n)
    })

    expect(burnResult).toBeDefined()
    expect(Result.isError(burnResult!)).toBe(true)

    expect(mockUpdateTransaction).toHaveBeenCalledWith('test-uuid-123', {
      status: 'failed',
      errorMessage: expect.any(String),
    })
  })
})

describe('usePreviewMint', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mockUseAccount.mockReturnValue({ chainId: 11155111 })
  })

  it('returns loading state initially', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: true,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => usePreviewMint(1000n), { wrapper: createWrapper() })

    expect(result.current.isLoading).toBe(true)
    expect(result.current.usdcRequired).toBe(0n)
  })

  it('returns data when loaded', () => {
    mockUseReadContract.mockReturnValue({
      data: [200000000n, 180000000n, 20000000n],
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => usePreviewMint(1000n), { wrapper: createWrapper() })

    expect(result.current.isLoading).toBe(false)
    expect(result.current.usdcRequired).toBe(200000000n)
    expect(result.current.error).toBeNull()
  })

  it('returns 0 when no chainId', () => {
    mockUseAccount.mockReturnValue({ chainId: undefined })
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => usePreviewMint(1000n), { wrapper: createWrapper() })

    expect(result.current.usdcRequired).toBe(0n)
  })
})

describe('usePreviewBurn', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mockUseAccount.mockReturnValue({ chainId: 11155111 })
  })

  it('returns loading state initially', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: true,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => usePreviewBurn(1000n), { wrapper: createWrapper() })

    expect(result.current.isLoading).toBe(true)
    expect(result.current.usdcToReturn).toBe(0n)
  })

  it('returns data when loaded', () => {
    mockUseReadContract.mockReturnValue({
      data: [195000000n, 175000000n],
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => usePreviewBurn(1000n), { wrapper: createWrapper() })

    expect(result.current.isLoading).toBe(false)
    expect(result.current.usdcToReturn).toBe(195000000n)
    expect(result.current.error).toBeNull()
  })
})
