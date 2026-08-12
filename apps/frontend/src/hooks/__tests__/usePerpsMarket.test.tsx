import { act, renderHook } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { perpsBasketHistoryQueryPolicy } from '../../api/hooks'
import { usePerpsMarket } from '../usePerpsMarket'

const mocks = vi.hoisted(() => ({
  refetchDynamic: vi.fn(),
  refetchRiskParams: vi.fn(),
  refetchConfiguration: vi.fn(),
  refetchLatestBasket: vi.fn(),
  refetchBasketHistory: vi.fn(),
  refetchBasketComponentHistory: vi.fn(),
  refetchMarketStats: vi.fn(),
  usePerpsBasketLatest: vi.fn(),
  usePerpsBasketHistory: vi.fn(),
  usePerpsMarketStats: vi.fn(),
  useReadContracts: vi.fn(),
}))

vi.mock('wagmi', () => ({
  useReadContracts: mocks.useReadContracts,
}))

vi.mock('../../api', () => ({
  usePerpsBasketLatest: mocks.usePerpsBasketLatest,
  usePerpsBasketHistory: mocks.usePerpsBasketHistory,
  usePerpsMarketStats: mocks.usePerpsMarketStats,
}))

describe('usePerpsMarket', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mocks.usePerpsBasketLatest.mockReturnValue({
      data: undefined,
      isLoading: false,
      isError: false,
      refetch: mocks.refetchLatestBasket,
    })
    mocks.usePerpsBasketHistory.mockImplementation((
      _range: string,
      _intervalSeconds: number,
      includeComponents = false
    ) => ({
      data: undefined,
      isLoading: false,
      refetch: includeComponents
        ? mocks.refetchBasketComponentHistory
        : mocks.refetchBasketHistory,
    }))
    mocks.usePerpsMarketStats.mockReturnValue({
      data: undefined,
      isLoading: false,
      refetch: mocks.refetchMarketStats,
    })
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
        ? [
            {
              status: 'success',
              result: {
                maxSkewRatio: 500_000_000_000_000_000n,
                maintMarginBps: 500n,
                fadMarginBps: 1_000n,
                baseCarryBps: 25n,
                minBountyUsdc: 100_000n,
                bountyBps: 50n,
              },
            },
            { status: 'success', result: 15n },
          ]
        : firstFunctionName === 'minOpenNotionalUsdc'
          ? [
              { status: 'success', result: 1_000_000n },
              { status: 'success', result: 10n },
              { status: 'success', result: 300n },
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

  it('polls dynamic market state but refreshes timelocked config only on lifecycle boundaries', () => {
    const { result } = renderHook(() => usePerpsMarket())
    const calls = mocks.useReadContracts.mock.calls.map(([parameters]) => parameters)
    const dynamicCall = calls.find((call) => call.contracts?.[0]?.functionName === 'getProtocolStatus')
    const riskParamsCall = calls.find((call) => call.contracts?.[0]?.functionName === 'riskParams')
    const configurationCall = calls.find((call) => call.contracts?.[0]?.functionName === 'minOpenNotionalUsdc')

    expect(dynamicCall?.contracts.map((contract: { functionName: string }) => contract.functionName)).toEqual([
      'getProtocolStatus',
      'getPoolLiquidityView',
      'sides',
      'sides',
    ])
    expect(dynamicCall?.query).toMatchObject({ refetchInterval: 15_000 })
    expect(riskParamsCall?.contracts.map((contract: { functionName: string }) => contract.functionName)).toEqual([
      'riskParams',
      'executionFeeBps',
    ])
    expect(riskParamsCall?.query).toMatchObject({
      staleTime: 300_000,
      gcTime: Number.POSITIVE_INFINITY,
      refetchOnWindowFocus: true,
      refetchOnReconnect: true,
    })
    expect(riskParamsCall?.query).not.toHaveProperty('refetchInterval')
    expect(configurationCall?.contracts.map((contract: { functionName: string }) => contract.functionName)).toEqual([
      'minOpenNotionalUsdc',
      'maxPendingOrders',
      'maxOrderAge',
    ])
    expect(configurationCall?.query).toMatchObject({
      staleTime: 300_000,
      gcTime: Number.POSITIVE_INFINITY,
      refetchOnWindowFocus: true,
      refetchOnReconnect: true,
    })
    expect(configurationCall?.query).not.toHaveProperty('refetchInterval')
    expect(mocks.usePerpsBasketHistory).toHaveBeenCalledWith('24h', 3_600, true)
    expect(mocks.usePerpsBasketHistory).toHaveBeenCalledTimes(1)
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
    expect(mocks.refetchBasketHistory).not.toHaveBeenCalled()
    expect(mocks.refetchBasketComponentHistory).toHaveBeenCalledOnce()
    expect(mocks.refetchMarketStats).toHaveBeenCalledOnce()
  })

  it('uses a five-minute component-history cadence without an immediate error retry', () => {
    expect(perpsBasketHistoryQueryPolicy('24h', 3_600, true)).toEqual({
      staleTimeMs: 300_000,
      refetchIntervalMs: 300_000,
      errorRefetchIntervalMs: 300_000,
      retryTransientFailure: false,
    })
    expect(perpsBasketHistoryQueryPolicy('7d', 300, false)).toEqual({
      staleTimeMs: 60_000,
      refetchIntervalMs: 60_000,
      errorRefetchIntervalMs: 120_000,
      retryTransientFailure: true,
    })
  })

  it('reuses component history for the headline and per-component changes', () => {
    const historicalTimestamp = 100_000
    const latestTimestamp = historicalTimestamp + 24 * 60 * 60
    const historicalComponent = {
      symbol: 'EUR/USD',
      feedSymbol: 'EUR/USD',
      feedId: '0xfeed',
      price: '100000000',
      rawPrice: '100000',
      confidence: '1',
      exponent: -5,
      publishTime: historicalTimestamp,
      inverted: false,
      weightBps: 10_000,
      basePrice: '100000000',
    }

    mocks.usePerpsBasketLatest.mockReturnValue({
      data: {
        data: {
          timestamp: latestTimestamp,
          basketPrice: '96000000',
          components: [{
            ...historicalComponent,
            price: '101000000',
            publishTime: latestTimestamp,
          }],
          generatedAt: latestTimestamp,
          source: 'database',
        },
      },
      isLoading: false,
      isError: false,
      refetch: mocks.refetchLatestBasket,
    })
    mocks.usePerpsBasketHistory.mockImplementation((
      _range: string,
      _intervalSeconds: number,
      includeComponents = false
    ) => ({
      data: {
        data: {
          points: includeComponents
            ? [{
                timestamp: historicalTimestamp,
                basketPrice: '98000000',
                components: [historicalComponent],
              }]
            : [{
                timestamp: historicalTimestamp,
                basketPrice: '98000000',
              }],
        },
      },
      isLoading: false,
      refetch: includeComponents
        ? mocks.refetchBasketComponentHistory
        : mocks.refetchBasketHistory,
    }))

    const { result } = renderHook(() => usePerpsMarket())

    expect(result.current.priceChange24h).toBe('+1.96%')
    expect(result.current.basketComponentPriceChanges['0xfeed']).toBeCloseTo(0.01)
  })

  it('uses market stats rather than component history as authoritative 24h volume', () => {
    mocks.usePerpsMarketStats.mockReturnValue({
      data: { data: { volume24hUsdc: '123000000' } },
      isLoading: false,
      refetch: mocks.refetchMarketStats,
    })
    mocks.usePerpsBasketHistory.mockReturnValue({
      data: {
        data: {
          points: [{
            timestamp: 100_000,
            basketPrice: '98000000',
            volumeUsdc: '999999999999',
            components: [],
          }],
        },
      },
      isLoading: false,
      refetch: mocks.refetchBasketComponentHistory,
    })

    const { result } = renderHook(() => usePerpsMarket())

    expect(result.current.volume24h).toBe('123')
  })
})
