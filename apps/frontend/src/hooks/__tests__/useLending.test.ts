import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook } from '@testing-library/react'
import { encodeAbiParameters, keccak256, type Address } from 'viem'
import { useTransactionStore } from '../../stores/transactionStore'
import { getAddresses } from '../../contracts/addresses'

const mockWriteContract = vi.fn()
const mockReset = vi.fn()

const mockUseAccount = vi.fn()
const mockUseReadContract = vi.fn()
const mockUseWriteContract = vi.fn()
const mockUseWaitForTransactionReceipt = vi.fn()

vi.mock('wagmi', () => ({
  useAccount: () => mockUseAccount(),
  useReadContract: (parameters: unknown) => mockUseReadContract(parameters),
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
const MOCK_MORPHO_ADDRESS = '0x5555555555555555555555555555555555555555' as const
type MockMarketParams = readonly [Address, Address, Address, Address, bigint]
const MOCK_MARKET_PARAMS = [
  '0x1111111111111111111111111111111111111111',
  '0x2222222222222222222222222222222222222222',
  '0x3333333333333333333333333333333333333333',
  '0x4444444444444444444444444444444444444444',
  915000000000000000n,
] as const

function mockLendingMarketReads(markets: {
  router: Address
  params: MockMarketParams
  position: readonly [bigint, bigint, bigint]
  market: readonly [bigint, bigint, bigint, bigint, bigint, bigint]
  oraclePrice?: bigint
}[]) {
  const byId = new Map(markets.map((market) => [keccak256(encodeAbiParameters(
    [{ type: 'address' }, { type: 'address' }, { type: 'address' }, { type: 'address' }, { type: 'uint256' }],
    market.params,
  )), market]))
  mockUseReadContract.mockImplementation(({ address, functionName, args }: {
    address?: Address; functionName: string; args?: readonly unknown[]
  }) => {
    const configured = markets.find(({ router }) => router === address)
    const indexed = byId.get(args?.[0] as `0x${string}`)
    let data: unknown
    if (functionName === 'MORPHO' && configured) data = MOCK_MORPHO_ADDRESS
    if (functionName === 'marketParams' && configured) data = configured.params
    if (address === MOCK_MORPHO_ADDRESS && functionName === 'position' && args?.[1] === MOCK_ADDRESS) data = indexed?.position
    if (address === MOCK_MORPHO_ADDRESS && functionName === 'market') data = indexed?.market
    if (functionName === 'price') data = markets.find(({ params }) => params[2] === address)?.oraclePrice
    if (data === undefined) throw new Error(`Unexpected lending read: ${address} ${functionName}`)
    return { data, isLoading: false, error: null, refetch: vi.fn() }
  })
}

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

  it.each([
    { borrowShares: 500n * 10n ** 18n, debt: 250_000_000n, available: 665_000_000n },
    { borrowShares: 1_830n * 10n ** 18n, debt: 915_000_000n, available: 0n },
    { borrowShares: 2_000n * 10n ** 18n, debt: 1_000_000_000n, available: 0n },
  ])('values collateral and clamps borrowing capacity for $debt debt', ({ borrowShares, debt, available }) => {
    // 800 collateral tokens at $1.25 back $915 at 91.5% LLTV.
    const collateral = 800n * 10n ** 21n
    mockLendingMarketReads([{
      router: getAddresses(MOCK_CHAIN_ID).LEVERAGE_ROUTER,
      params: MOCK_MARKET_PARAMS,
      position: [0n, borrowShares, collateral],
      market: [4_000_000_000n, 1_000n * 10n ** 18n, 2_000_000_000n, 4_000n * 10n ** 18n, 0n, 0n],
      oraclePrice: 1_250_000_000_000_000_000_000_000n,
    }])

    const { result } = renderHook(() => useAvailableToBorrow('BEAR'))

    expect(result.current.collateral).toBe(collateral)
    expect(result.current.collateralUsd).toBe(1_000_000_000n)
    expect(result.current.lltv).toBe(915_000_000_000_000_000n)
    expect(result.current.maxBorrow).toBe(915_000_000n)
    expect(result.current.currentDebt).toBe(debt)
    expect(result.current.availableToBorrow).toBe(available)
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
    const addresses = getAddresses(MOCK_CHAIN_ID)
    mockLendingMarketReads([
      {
        router: addresses.LEVERAGE_ROUTER,
        params: MOCK_MARKET_PARAMS,
        position: [200n * 10n ** 18n, 50n * 10n ** 18n, 300n * 10n ** 21n],
        market: [2_000_000_000n, 1_000n * 10n ** 18n, 1_000_000_000n, 500n * 10n ** 18n, 0n, 0n],
      },
      {
        router: addresses.BULL_LEVERAGE_ROUTER,
        params: [MOCK_MARKET_PARAMS[0], '0x6666666666666666666666666666666666666666',
          MOCK_MARKET_PARAMS[2], MOCK_MARKET_PARAMS[3], MOCK_MARKET_PARAMS[4]],
        position: [150n * 10n ** 18n, 125n * 10n ** 18n, 500n * 10n ** 21n],
        market: [3_000_000_000n, 1_000n * 10n ** 18n, 1_200_000_000n, 1_000n * 10n ** 18n, 0n, 0n],
      },
    ])
    const { result } = renderHook(() => useCombinedLendingPosition())

    expect(result.current.bearPosition.suppliedAssets).toBe(400_000_000n)
    expect(result.current.bearPosition.borrowedAssets).toBe(100_000_000n)
    expect(result.current.bullPosition.suppliedAssets).toBe(450_000_000n)
    expect(result.current.bullPosition.borrowedAssets).toBe(150_000_000n)
    expect(result.current.totalSupplied).toBe(850_000_000n)
    expect(result.current.totalBorrowed).toBe(250_000_000n)
    expect(result.current.isLoading).toBe(false)
  })

  it('provides refetch function', () => {
    const { result } = renderHook(() => useCombinedLendingPosition())

    expect(typeof result.current.refetch).toBe('function')
  })
})
