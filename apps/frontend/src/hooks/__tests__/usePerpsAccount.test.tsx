import { act, renderHook } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { usePerpsAccount } from '../usePerpsAccount'

const ACCOUNT = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'

type ContractResult =
  | { status: 'success'; result: unknown }
  | { status: 'failure'; error: Error; result?: undefined }

const mocks = vi.hoisted(() => ({
  invalidated: false,
  invalidateSnapshot: vi.fn(),
  dynamicError: undefined as Error | undefined,
  account: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
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

vi.mock('../usePerpsSnapshotInvalidated', () => ({ usePerpsSnapshotInvalidated: () => mocks.invalidated, useInvalidatePerpsSnapshot: () => mocks.invalidateSnapshot }))

vi.mock('wagmi', () => ({
  useReadContracts: mocks.useReadContracts,
}))

vi.mock('../../perps-aa', () => ({
  usePerpsIdentity: () => ({
    status: 'ready',
    ownerAddress: ACCOUNT,
    accountAddress: mocks.account,
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
  enginePositionResult = failure('engine position unavailable'),
}: {
  hasOpenPosition?: boolean
  positionResult?: ContractResult
  enginePositionResult?: ContractResult
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
    enginePositionResult,
    success({ active: false }),
  ]
}

// Arbitrum Sepolia block 306432516: the reported 192,000 plDXY position.
function screenshotData(): ContractResult[] {
  const data = primaryData({
    positionResult: success({
      exists: true,
      side: 0,
      size: 192_000n * 10n ** 18n,
      entryPrice: 99_223_895n,
      marginUsdc: 38_373_563_531n,
      unrealizedPnlUsdc: -317_528_915n,
      maintenanceMarginUsdc: 190_826_610n,
      liquidatable: false,
    }),
    enginePositionResult: success([
      192_000n * 10n ** 18n, 38_373_563_531n, 99_223_895n,
      190_509_879_085n, 0, 1_788_797_525n, 25_857_538n,
    ]),
  })
  data[7] = success({
    liquidationReachableSettlementUsdc: 99_872_022_645n,
    unrealizedPnlUsdc: -317_528_915n,
    netEquityUsdc: 38_056_034_616n,
  })
  data.push(
    success([152_136_315_554n, 5_287_315_765_581n, 1_788_797_525n]),
    success(0n),
    success([0n, 0n, 10n, 20n, 300n, 500n]),
    success(60_003_610_699_152n),
    success(1_788_797_607n),
    success(5_301_042_300_334n),
    success(1_788_797_581n),
    success(9_277_623_571_807n),
    success(0n), success(1_788_797_607n), success(0n),
  )
  return data
}

function riskData(side = 0): ContractResult[] {
  const data = screenshotData()
  data[0] = success({ hasOpenPosition: true, equityUsdc: 250_000_000n, withdrawableUsdc: 750_000_000n, liquidatable: false })
  data[1] = success({ exists: true, side, size: 10_000n * 10n ** 18n, entryPrice: 100_000_000n,
    marginUsdc: 250_000_000n, unrealizedPnlUsdc: 0n, maintenanceMarginUsdc: 10_000_000n, liquidatable: false })
  data[7] = success({ settlementBalanceUsdc: 1_000_000_000n, freeSettlementUsdc: 750_000_000n,
    traderClaimBalanceUsdc: 0n, netEquityUsdc: 250_000_000n, liquidationReachableSettlementUsdc: 1_000_000_000n })
  data[9] = success({ side, vpiAccrued: 0n })
  data[11] = success({ borrowBaseUsdc: 0n, lastCarryIndex: 0n })
  data[12] = success(0n)
  data[22] = success(10_000_000_000n)
  data[23] = success(100_000_000n)
  data[24] = success(0n)
  return data
}

describe('usePerpsAccount', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mocks.invalidated = false
    mocks.dynamicError = undefined
    mocks.account = ACCOUNT
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
          error: mocks.dynamicError,
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
      'getActivePositionProtection',
      'positionCarryState',
      'unsettledCarryUsdc',
      'riskParams',
      'totalAssets',
      'getCurrentBlockTimestamp',
      'sideCarryIndex',
      'sideCarryTimestamp',
      'sideBorrowBaseUsdc',
      'sideCarryIndex',
      'sideCarryTimestamp',
      'sideBorrowBaseUsdc',
      'positionEntryCostUsdcAtoms',
      'lastMarkPrice',
      'vpiRebateReserveUsdc',
    ])
    expect(dynamicCall?.batchSize).toBe(0)
    expect(dynamicCall?.contracts.slice(16, 22).map((contract: { args: bigint[] }) => contract.args))
      .toEqual([[0n], [0n], [0n], [1n], [1n], [1n]])
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

    expect(mocks.invalidateSnapshot).toHaveBeenCalledOnce()
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

  it('shows actual indexed carry instead of the screenshot’s 61,498.46 USDC of collateral', () => {
    mocks.primaryData = screenshotData()

    const { result } = renderHook(() => usePerpsAccount(98_000_000n))

    expect(result.current.position?.pendingCarryUsdc).toBe(3_057n)
  })

  it('adds checkpointed unpaid carry and uses object-shaped getter results', () => {
    mocks.primaryData = screenshotData()
    mocks.primaryData[9] = success({ side: 0, vpiAccrued: -40_000_000n })
    mocks.primaryData[11] = success({ borrowBaseUsdc: 152_136_315_554n, lastCarryIndex: 5_287_315_765_581n })
    mocks.primaryData[12] = success(7_000_000n)
    mocks.primaryData[13] = success({ baseCarryBps: 500n })

    const { result } = renderHook(() => usePerpsAccount(98_000_000n))

    expect(result.current.position?.pendingCarryUsdc).toBe(7_003_057n)
  })

  it('selects the new side’s carry inputs when the position direction changes', () => {
    mocks.primaryData = screenshotData()
    const { result, rerender } = renderHook(() => usePerpsAccount(98_000_000n))
    expect(result.current.position?.pendingCarryUsdc).toBe(3_057n)

    mocks.primaryData = screenshotData()
    mocks.primaryData[9] = success({ side: 1 })
    mocks.primaryData[19] = success(5_301_042_300_334n)
    mocks.primaryData[20] = success(1_788_797_581n)
    mocks.primaryData[21] = success(9_277_623_571_807n)
    mocks.primaryData[16] = failure('other side unavailable')
    rerender()

    expect(result.current.position?.pendingCarryUsdc).toBe(3_057n)
  })

  it.each([9, 11, 12, 13, 14, 15, 16, 17, 18])(
    'leaves carry unavailable when required dynamic read %i fails', (index) => {
      mocks.primaryData = screenshotData()
      const { result, rerender } = renderHook(() => usePerpsAccount(98_000_000n))
      expect(result.current.position?.pendingCarryUsdc).toBe(3_057n)

      mocks.primaryData = screenshotData()
      mocks.primaryData[index] = failure('carry input unavailable')
      rerender()

      expect(result.current.position?.pendingCarryUsdc).toBeUndefined()
    }
  )

  it('does not depend on the collateral snapshot or cached risk configuration for carry', () => {
    mocks.primaryData = screenshotData()
    mocks.primaryData[7] = failure('ledger unavailable')
    mocks.riskParamsData = [success({ baseCarryBps: 9_000n }), success(15n)]

    const { result } = renderHook(() => usePerpsAccount(98_000_000n))

    expect(result.current.position?.pendingCarryUsdc).toBe(3_057n)
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

  it('exposes the engine position signed VPI balance', () => {
    mocks.primaryData = primaryData({
      enginePositionResult: success({
        size: 2_000_000_000_000_000_000_000n,
        margin: 400_000_000n,
        entryPrice: 97_500_000n,
        maxProfitUsdc: 1_000_000_000n,
        side: 0,
        lastUpdateTime: 1_700_000_000n,
        vpiAccrued: -40_000_000n,
      }),
    })

    const { result } = renderHook(() => usePerpsAccount(98_000_000n))

    expect(result.current.position?.vpiAccrued).toBe(-40_000_000n)
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
  it.each([[0, 102_397_603n], [1, 97_597_597n]])('uses canonical price collateral for side %s', (side, price) => {
    mocks.primaryData = riskData(Number(side))
    mocks.immutableData = [success(200_000_000n)]
    const { result } = renderHook(() => usePerpsAccount(150_000_000n))
    expect(result.current.position?.liquidationThreshold).toEqual({ status: 'boundary', price })
    expect(result.current.position?.estimatedNotionalUsdc).toBe(10_000_000_000n)
    expect(result.current.position?.displayDxyPrice).toBe(100_000_000n)
    expect(result.current.positionEquityUsdc).toBe(250_000_000n)
    expect(result.current.settlementBalanceUsdc).toBe(1_000_000_000n)
    expect(result.current.snapshotStatus).toBe('ready')
  })

  it('does not move the boundary for free funds, carry, or reserve-backed negative VPI', () => {
    mocks.primaryData = riskData()
    mocks.immutableData = [success(200_000_000n)]
    mocks.primaryData[7] = success({ settlementBalanceUsdc: 2_000_000_000n, freeSettlementUsdc: 1_750_000_000n,
      traderClaimBalanceUsdc: 0n, netEquityUsdc: 250_000_000n })
    mocks.primaryData[12] = success(20_000_000n)
    mocks.primaryData[9] = success({ side: 0n, vpiAccrued: -10_000_000n })
    mocks.primaryData[24] = success(10_000_000n)
    const { result } = renderHook(() => usePerpsAccount())
    expect(result.current.position?.liquidationPrice).toBe(102_397_603n)
    expect(result.current.position?.uncoveredCarryUsdc).toBe(0n)
    expect(result.current.position?.vpiReserveUnderfunded).toBe(false)
  })

  it('preserves signed equity and reports non-price liquidation reasons separately', () => {
    mocks.primaryData = riskData()
    mocks.immutableData = [success(200_000_000n)]
    mocks.primaryData[7] = success({ settlementBalanceUsdc: 250_000_000n, freeSettlementUsdc: 0n,
      traderClaimBalanceUsdc: 0n, netEquityUsdc: -5n })
    mocks.primaryData[0] = success({ hasOpenPosition: true, equityUsdc: 0n, liquidatable: true })
    mocks.primaryData[12] = success(7n)
    mocks.primaryData[9] = success({ side: 0n, vpiAccrued: -10n })
    const { result } = renderHook(() => usePerpsAccount())
    expect(result.current.positionEquityUsdc).toBe(-5n)
    expect(result.current.liquidatable).toBe(true)
    expect(result.current.position?.uncoveredCarryUsdc).toBe(7n)
    expect(result.current.position?.vpiReserveUnderfunded).toBe(true)
  })

  it.each([0, 1, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 22, 23, 24])('does not present missing read %i as no liquidation risk', index => {
    mocks.primaryData = riskData()
    mocks.immutableData = [success(200_000_000n)]
    const { result, rerender } = renderHook(() => usePerpsAccount())
    expect(result.current.position?.liquidationPrice).toBeDefined()
    mocks.primaryData = [...mocks.primaryData]
    mocks.primaryData[index] = failure('temporarily unavailable')
    rerender()
    expect(result.current.snapshotStatus).toBe('unavailable')
    expect(result.current.position?.liquidationThreshold).toEqual({ status: 'unavailable' })
    expect(result.current.position?.liquidationPrice).toBeUndefined()
  })

  it('treats a missing engine mark sentinel as unavailable, without excluding zero price boundaries', () => {
    mocks.primaryData = riskData()
    mocks.immutableData = [success(200_000_000n)]
    mocks.primaryData[23] = success(0n)
    const { result } = renderHook(() => usePerpsAccount())
    expect(result.current.snapshotStatus).toBe('unavailable')
    expect(result.current.position?.liquidationThreshold).toEqual({ status: 'unavailable' })
    expect(result.current.positionEquityUsdc).toBeUndefined()
  })

  it.each(['invalidated', 'failed'] as const)('hides cached thresholds during %s refreshes and restores them after success', state => {
    mocks.primaryData = riskData()
    mocks.immutableData = [success(200_000_000n)]
    const { result, rerender } = renderHook(() => usePerpsAccount())
    if (state === 'invalidated') mocks.invalidated = true
    else mocks.dynamicError = new Error('RPC unavailable')
    rerender()
    expect(result.current.position?.liquidationPrice).toBeUndefined()
    expect(result.current.position?.riskStatus).toBe('unavailable')
    expect(result.current.display.availableToTrade).toBe('--')
    mocks.invalidated = false
    mocks.dynamicError = undefined
    rerender()
    expect(result.current.position?.liquidationPrice).toBe(102_397_603n)
  })

  it('uses current FAD risk parameters despite stale cached configuration', () => {
    mocks.primaryData = riskData()
    mocks.immutableData = [success(200_000_000n)]
    mocks.primaryData[8] = success(true)
    mocks.riskParamsData = [success({ maintMarginBps: 999n, fadMarginBps: 999n })]
    const { result } = renderHook(() => usePerpsAccount())
    expect(result.current.position?.liquidationPrice).toBeLessThan(100_000_000n)
  })

  it('does not retain another account’s cached position', () => {
    mocks.primaryData = riskData()
    mocks.immutableData = [success(200_000_000n)]
    const { result, rerender } = renderHook(() => usePerpsAccount())
    mocks.account = '0x2222222222222222222222222222222222222222'
    mocks.primaryData = []
    rerender()
    expect(result.current.position).toBeUndefined()
  })

})
