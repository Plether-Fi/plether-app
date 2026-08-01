import { act, renderHook } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { usePerpsMarket } from '../usePerpsMarket'

const mocks = vi.hoisted(() => ({
  refetchDynamic: vi.fn(),
  refetchRiskParams: vi.fn(),
  refetchConfiguration: vi.fn(),
  refetchLatestBasket: vi.fn(),
  refetchBasketHistory: vi.fn(),
  refetchMarketStats: vi.fn(),
  useReadContracts: vi.fn(),
}))

vi.mock('wagmi', () => ({
  useReadContracts: mocks.useReadContracts,
}))

vi.mock('../../api', () => ({
  usePerpsBasketLatest: () => ({
    data: undefined,
    refetch: mocks.refetchLatestBasket,
  }),
  usePerpsBasketHistory: () => ({
    data: undefined,
    isLoading: false,
    refetch: mocks.refetchBasketHistory,
  }),
  usePerpsMarketStats: () => ({
    data: undefined,
    isLoading: false,
    refetch: mocks.refetchMarketStats,
  }),
}))

describe('usePerpsMarket', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mocks.useReadContracts.mockImplementation((parameters: {
      contracts?: { functionName?: string }[]
    }) => {
      const firstFunctionName = parameters.contracts?.[0]?.functionName
      const refetch = firstFunctionName === 'getProtocolStatus'
        ? mocks.refetchDynamic
        : firstFunctionName === 'riskParams'
          ? mocks.refetchRiskParams
          : mocks.refetchConfiguration
      const data = firstFunctionName === 'riskParams'
        ? [{
            status: 'success',
            result: {
              maxSkewRatio: 500_000_000_000_000_000n,
              maintMarginBps: 500n,
              fadMarginBps: 1_000n,
              baseCarryBps: 25n,
              minBountyUsdc: 100_000n,
              bountyBps: 50n,
            },
          }]
        : firstFunctionName === 'executionFeeBps'
          ? [
              { status: 'success', result: 15n },
              { status: 'success', result: 1_000_000n },
            ]
          : undefined

      return {
        data,
        isLoading: false,
        error: undefined,
        refetch,
      }
    })
  })

  it('polls dynamic market state frequently and revalidates configuration slowly', () => {
    const { result } = renderHook(() => usePerpsMarket())
    const calls = mocks.useReadContracts.mock.calls.map(([parameters]) => parameters)
    const dynamicCall = calls.find((call) => call.contracts?.[0]?.functionName === 'getProtocolStatus')
    const riskParamsCall = calls.find((call) => call.contracts?.[0]?.functionName === 'riskParams')
    const configurationCall = calls.find((call) => call.contracts?.[0]?.functionName === 'executionFeeBps')

    expect(dynamicCall?.contracts.map((contract: { functionName: string }) => contract.functionName)).toEqual([
      'getProtocolStatus',
      'getPoolLiquidityView',
      'sides',
      'sides',
    ])
    expect(dynamicCall?.query).toMatchObject({ refetchInterval: 15_000 })
    expect(riskParamsCall?.query).toMatchObject({
      staleTime: 300_000,
      refetchInterval: 300_000,
      gcTime: 1_800_000,
    })
    expect(configurationCall?.contracts.map((contract: { functionName: string }) => contract.functionName)).toEqual([
      'executionFeeBps',
      'minOpenNotionalUsdc',
    ])
    expect(configurationCall?.query).toMatchObject({
      staleTime: 300_000,
      refetchInterval: 300_000,
      gcTime: 1_800_000,
    })
    expect(result.current.raw.executionFeeBps).toBe(15n)
    expect(result.current.raw.minOpenNotionalUsdc).toBe(1_000_000n)
    expect(result.current.raw.maintenanceMarginBps).toBe(500n)

    act(() => {
      void result.current.refetchDynamic()
    })

    expect(mocks.refetchDynamic).toHaveBeenCalledOnce()
    expect(mocks.refetchRiskParams).not.toHaveBeenCalled()
    expect(mocks.refetchConfiguration).not.toHaveBeenCalled()

    act(() => {
      result.current.refetch()
    })

    expect(mocks.refetchDynamic).toHaveBeenCalledTimes(2)
    expect(mocks.refetchRiskParams).toHaveBeenCalledOnce()
    expect(mocks.refetchConfiguration).toHaveBeenCalledOnce()
    expect(mocks.refetchLatestBasket).toHaveBeenCalledOnce()
    expect(mocks.refetchBasketHistory).toHaveBeenCalledOnce()
    expect(mocks.refetchMarketStats).toHaveBeenCalledOnce()
  })
})
