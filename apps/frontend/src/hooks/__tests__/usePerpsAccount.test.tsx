import { act, renderHook } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { usePerpsAccount } from '../usePerpsAccount'

const ACCOUNT = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'

type ContractResult =
  | { status: 'success'; result: unknown }
  | { status: 'failure'; error: Error; result?: undefined }

const mocks = vi.hoisted(() => ({
  primaryData: [] as ContractResult[],
  pendingDetailsLoading: false,
  refetch: vi.fn(),
  useReadContracts: vi.fn(),
}))

vi.mock('wagmi', () => ({
  useAccount: () => ({
    address: ACCOUNT,
    isConnected: true,
  }),
  useReadContracts: mocks.useReadContracts,
}))

function success(result: unknown): ContractResult {
  return { status: 'success', result }
}

function failure(message: string): ContractResult {
  return { status: 'failure', error: new Error(message) }
}

function primaryData({
  hasOpenPosition = true,
  positionResult = success({
    exists: true,
    side: 0,
    size: 2_000_000_000_000_000_000_000n,
    entryPrice: 97_500_000n,
    marginUsdc: 400_000_000n,
    unrealizedPnlUsdc: 25_000_000n,
    maintenanceMarginUsdc: 100_000_000n,
    liquidatable: false,
  }),
}: {
  hasOpenPosition?: boolean
  positionResult?: ContractResult
} = {}): ContractResult[] {
  return [
    success({
      equityUsdc: 2_000_000_000n,
      withdrawableUsdc: 1_500_000_000n,
      pendingOrderMarginUsdc: 0n,
      pendingExecutionBountyUsdc: 200_000n,
      hasOpenPosition,
      liquidatable: false,
    }),
    positionResult,
    success([{
      orderId: 42n,
      side: 0,
      sizeDelta: 500_000_000_000_000_000_000n,
      marginDeltaUsdc: 0n,
      acceptablePrice: 98_000_000n,
      isReduceOnly: true,
      status: 0,
    }]),
    success(5_000_000_000n),
    success(5_000_000_000n),
    success(1_500_000_000n),
    success(10n),
    success(300n),
    failure('ledger snapshot unavailable'),
    failure('risk params unavailable'),
    failure('cap price unavailable'),
    success(false),
    failure('engine position unavailable'),
  ]
}

describe('usePerpsAccount', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mocks.primaryData = primaryData()
    mocks.pendingDetailsLoading = true
    mocks.useReadContracts.mockImplementation((parameters: {
      contracts?: { functionName?: string }[]
    }) => {
      const isPrimaryAccountBatch = parameters.contracts?.[0]?.functionName === 'getTraderAccount'

      return isPrimaryAccountBatch
        ? {
            data: mocks.primaryData,
            isLoading: false,
            error: undefined,
            refetch: mocks.refetch,
          }
        : {
            data: undefined,
            isLoading: mocks.pendingDetailsLoading,
            error: undefined,
            refetch: vi.fn(),
          }
    })
  })

  it('keeps position data visible while pending-order details load', () => {
    const { result } = renderHook(() => usePerpsAccount(98_000_000n))

    expect(result.current.isLoading).toBe(false)
    expect(result.current.isPendingOrderDetailsLoading).toBe(true)
    expect(result.current.position).toEqual(expect.objectContaining({
      exists: true,
      size: 2_000_000_000_000_000_000_000n,
      marginUsdc: 400_000_000n,
    }))
  })

  it('retains the last valid position across a transient getPosition subcall failure', () => {
    const { result, rerender } = renderHook(() => usePerpsAccount(98_000_000n))

    expect(result.current.position?.size).toBe(2_000_000_000_000_000_000_000n)

    act(() => {
      mocks.primaryData = primaryData({
        positionResult: success({
          exists: true,
          side: 0,
          size: 1_250_000_000_000_000_000_000n,
          entryPrice: 97_500_000n,
          marginUsdc: 250_000_000n,
          unrealizedPnlUsdc: 15_000_000n,
          maintenanceMarginUsdc: 62_500_000n,
          liquidatable: false,
        }),
      })
      rerender()
    })

    expect(result.current.position?.size).toBe(1_250_000_000_000_000_000_000n)
    const reducedPositionNotional = result.current.display.positionNotional

    act(() => {
      mocks.primaryData = primaryData({
        positionResult: failure('getPosition temporarily failed'),
      })
      rerender()
    })

    expect(result.current.hasOpenPosition).toBe(true)
    expect(result.current.position).toEqual(expect.objectContaining({
      exists: true,
      size: 1_250_000_000_000_000_000_000n,
      marginUsdc: 250_000_000n,
    }))
    expect(result.current.display.positionNotional).toBe(reducedPositionNotional)
  })

  it('does not retain an old position when the account view confirms it is closed', () => {
    const { result, rerender } = renderHook(() => usePerpsAccount(98_000_000n))

    expect(result.current.position?.exists).toBe(true)

    act(() => {
      mocks.primaryData = primaryData({
        hasOpenPosition: false,
        positionResult: failure('getPosition temporarily failed'),
      })
      rerender()
    })

    expect(result.current.hasOpenPosition).toBe(false)
    expect(result.current.position).toBeUndefined()
  })
})
