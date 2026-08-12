import { act, renderHook } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { usePerpsAccount } from '../usePerpsAccount'

const ACCOUNT = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'

type ContractResult =
  | { status: 'success'; result: unknown }
  | { status: 'failure'; error: Error; result?: undefined }

const mocks = vi.hoisted(() => ({
  primaryData: [] as ContractResult[],
  configurationData: [] as ContractResult[],
  immutableData: [] as ContractResult[],
  riskParamsData: [] as ContractResult[],
  pendingDetailsLoading: false,
  refetchDynamic: vi.fn(),
  refetchConfiguration: vi.fn(),
  refetchRiskParams: vi.fn(),
  refetchImmutable: vi.fn(),
  useReadContracts: vi.fn(),
}))

vi.mock('wagmi', () => ({
  useReadContracts: mocks.useReadContracts,
}))

vi.mock('../../perps-aa', () => ({
  usePerpsIdentity: () => ({
    status: 'ready',
    ownerAddress: ACCOUNT,
    accountAddress: ACCOUNT,
    chainId: 421614,
    isAaManifestConfigured: true,
    sponsorshipEnabled: true,
    manifest: null,
    identity: null,
    proposedIdentity: null,
    changedIdentityFields: [],
    error: null,
    confirmIdentityAfterContinuityCheck: () => false,
    reloadIdentity: () => undefined,
  }),
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
    success(5_000_000_000n),
    success(1_500_000_000n),
    failure('ledger snapshot unavailable'),
    success(false),
    failure('engine position unavailable'),
  ]
}

describe('usePerpsAccount', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mocks.primaryData = primaryData()
    mocks.configurationData = [
      success(1_000_000n),
      success(10n),
      success(300n),
    ]
    mocks.immutableData = [failure('cap price unavailable')]
    mocks.riskParamsData = [failure('risk params unavailable'), success(15n)]
    mocks.pendingDetailsLoading = true
    mocks.useReadContracts.mockImplementation((parameters: {
      contracts?: { functionName?: string }[]
    }) => {
      const firstFunctionName = parameters.contracts?.[0]?.functionName

      if (firstFunctionName === 'getTraderAccount') {
        return {
          data: mocks.primaryData,
          isLoading: false,
          error: undefined,
          refetch: mocks.refetchDynamic,
        }
      }

      if (firstFunctionName === 'riskParams') {
        return {
          data: mocks.riskParamsData,
          isLoading: false,
          error: undefined,
          refetch: mocks.refetchRiskParams,
        }
      }

      if (firstFunctionName === 'minOpenNotionalUsdc') {
        return {
          data: mocks.configurationData,
          isLoading: false,
          error: undefined,
          refetch: mocks.refetchConfiguration,
        }
      }

      if (firstFunctionName === 'CAP_PRICE') {
        return {
          data: mocks.immutableData,
          isLoading: false,
          error: undefined,
          refetch: mocks.refetchImmutable,
        }
      }

      return {
        data: undefined,
        isLoading: mocks.pendingDetailsLoading,
        error: undefined,
        refetch: vi.fn(),
      }
    })
  })

  it('polls dynamic account state but refreshes timelocked config only on lifecycle boundaries', async () => {
    const { result } = renderHook(() => usePerpsAccount(98_000_000n))
    const calls = mocks.useReadContracts.mock.calls.map(([parameters]) => parameters)
    const dynamicCall = calls.find((call) => call.contracts?.[0]?.functionName === 'getTraderAccount')
    const riskParamsCall = calls.find((call) => call.contracts?.[0]?.functionName === 'riskParams')
    const configurationCall = calls.find((call) => call.contracts?.[0]?.functionName === 'minOpenNotionalUsdc')
    const immutableCall = calls.find((call) => call.contracts?.[0]?.functionName === 'CAP_PRICE')

    expect(dynamicCall?.contracts.map((contract: { functionName: string }) => contract.functionName)).toEqual([
      'getTraderAccount',
      'getPosition',
      'getPendingOrders',
      'balanceOf',
      'balanceOf',
      'allowance',
      'getFreeBuyingPowerUsdc',
      'getAccountLedgerSnapshot',
      'isFadWindow',
      'positions',
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
    expect(immutableCall?.query).toMatchObject({
      staleTime: Number.POSITIVE_INFINITY,
      gcTime: Number.POSITIVE_INFINITY,
      refetchOnWindowFocus: false,
      refetchOnReconnect: false,
    })
    expect(result.current.maxPendingOrders).toBe(10n)
    expect(result.current.maxOrderAge).toBe(300n)

    await act(async () => {
      await result.current.refetchDynamic()
    })

    expect(mocks.refetchDynamic).toHaveBeenCalledOnce()
    expect(mocks.refetchRiskParams).not.toHaveBeenCalled()
    expect(mocks.refetchConfiguration).not.toHaveBeenCalled()

    await act(async () => {
      await result.current.refetch()
    })

    expect(mocks.refetchDynamic).toHaveBeenCalledTimes(2)
    expect(mocks.refetchRiskParams).toHaveBeenCalledOnce()
    expect(mocks.refetchConfiguration).toHaveBeenCalledOnce()
    expect(mocks.refetchImmutable).not.toHaveBeenCalled()
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
