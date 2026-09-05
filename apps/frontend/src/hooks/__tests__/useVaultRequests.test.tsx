import { renderHook, waitFor } from '@testing-library/react'
import { Result } from 'better-result'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { perpsApi } from '../../api/client'
import { useVaultRequests } from '../useVaultRequests'

const CONTROLLER = '0x1111111111111111111111111111111111111111' as const

const mocks = vi.hoisted(() => ({
  refetch: vi.fn(),
  requestId: 0n,
  requestState: [
    '0x0000000000000000000000000000000000000000',
    0n,
    '0x0000000000000000000000000000000000000000',
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    false,
  ] as readonly unknown[],
  cooldownState: [
    '0x0000000000000000000000000000000000000000',
    0n,
    '0x0000000000000000000000000000000000000000',
    0n,
    0n,
    0n,
    0n,
  ] as readonly unknown[],
}))

vi.mock('wagmi', () => ({
  useReadContracts: (args: unknown) => {
    expect(args).toMatchObject({ allowFailure: false })
    const contracts = (args as {
      contracts: readonly {
        functionName: 'getLpDepositCooldownState' | 'getLpRequestState'
        args: readonly [boolean, bigint, `0x${string}`]
      }[]
    }).contracts
    return {
      data: contracts.map((contract) => {
        if (contract.functionName === 'getLpDepositCooldownState') {
          return {
            status: 'success' as const,
            result: contract.args[1] === mocks.requestId
              ? mocks.cooldownState
              : [
                  contract.args[0]
                    ? '0x0000000000000000000000000000000000000001'
                    : '0x0000000000000000000000000000000000000002',
                  contract.args[1],
                  contract.args[2],
                  0n,
                  0n,
                  0n,
                  0n,
                ],
          }
        }
        return {
          status: 'success' as const,
          result: contract.args[1] === mocks.requestId
            ? mocks.requestState
            : [
                contract.args[0]
                  ? '0x0000000000000000000000000000000000000001'
                  : '0x0000000000000000000000000000000000000002',
                contract.args[1],
                contract.args[2],
                0n,
                0n,
                0n,
                0n,
                0n,
                0n,
                0n,
                0n,
                0n,
                0n,
                false,
              ],
        }
      }).map(({ result }) => result),
      isLoading: false,
      refetch: mocks.refetch,
    }
  },
}))

function requestIdResponse(
  requestIds: string[] = [],
  tranche: 'senior' | 'junior' = 'junior',
  nextCursor: string | null = null,
  stale = false,
) {
  return Result.ok({
    data: {
      tranche,
      account: CONTROLLER,
      requestIds,
      nextCursor,
      confirmedThroughBlock: 302_300_000,
      stale,
    },
    meta: {
      cached: false,
      blockNumber: 302_300_000,
      chainId: 421_614,
    },
  })
}

describe('useVaultRequests', () => {
  beforeEach(() => {
    vi.restoreAllMocks()
    window.localStorage.clear()
    vi.spyOn(perpsApi, 'getPerpsVaultRequestIds')
      .mockImplementation(async (tranche) => requestIdResponse([], tranche))
    mocks.requestId = 0n
    mocks.requestState = [
      '0x0000000000000000000000000000000000000000',
      0n,
      CONTROLLER,
      0n,
      0n,
      0n,
      0n,
      0n,
      0n,
      0n,
      0n,
      0n,
      0n,
      false,
    ]
    mocks.cooldownState = [
      '0x0000000000000000000000000000000000000000',
      0n,
      CONTROLLER,
      0n,
      0n,
      0n,
      0n,
    ]
  })

  afterEach(() => {
    vi.restoreAllMocks()
  })

  it('reads nearby deposit and redemption state from the public lens', async () => {
    mocks.requestId = 501n
    mocks.requestState = [
      '0x0000000000000000000000000000000000000001',
      501n,
      CONTROLLER,
      25_000_000n,
      12_000_000_000n,
      0n,
      0n,
      5_000_000_000n,
      10_000_000n,
      0n,
      0n,
      0n,
      0n,
      false,
    ]

    const { result } = renderHook(() => useVaultRequests({
      controller: CONTROLLER,
      isSenior: true,
      currentEpoch: 500n,
    }))

    await waitFor(() => {
      expect(result.current.depositRequests).toHaveLength(1)
      expect(result.current.redeemRequests).toHaveLength(1)
    })
    expect(result.current.depositRequests[0]).toMatchObject({
      requestId: 501n,
      pendingAssets: 25_000_000n,
      matured: false,
    })
    expect(result.current.redeemRequests[0]).toMatchObject({
      requestId: 501n,
      pendingShares: 5_000_000_000n,
      pendingAssetsEstimate: 10_000_000n,
      matured: false,
    })
  })

  it('restores an older claimable request discovered through the backend index', async () => {
    mocks.requestId = 300n
    mocks.requestState = [
      '0x0000000000000000000000000000000000000002',
      300n,
      CONTROLLER,
      0n,
      0n,
      0n,
      0n,
      0n,
      0n,
      3_000_000_000n,
      6_000_000n,
      0n,
      0n,
      false,
    ]
    vi.mocked(perpsApi.getPerpsVaultRequestIds)
      .mockResolvedValue(requestIdResponse(['300']))

    const { result } = renderHook(() => useVaultRequests({
      controller: CONTROLLER,
      isSenior: false,
      currentEpoch: 500n,
    }))

    await waitFor(() => {
      expect(result.current.redeemRequests[0]?.requestId).toBe(300n)
    })
    expect(result.current.redeemRequests[0]).toMatchObject({
      claimableShares: 3_000_000_000n,
      claimableAssets: 6_000_000n,
      matured: true,
    })
    expect(perpsApi.getPerpsVaultRequestIds).toHaveBeenCalledWith(
      'junior',
      CONTROLLER,
      undefined,
      250,
      expect.any(AbortSignal),
    )
  })

  it('exposes settlement-aged cooldown state for direct redemption', async () => {
    mocks.requestId = 400n
    mocks.requestState = [
      '0x0000000000000000000000000000000000000002',
      400n,
      CONTROLLER,
      0n,
      0n,
      12_000_000n,
      6_000_000_000n,
      0n,
      0n,
      0n,
      0n,
      0n,
      0n,
      false,
    ]
    mocks.cooldownState = [
      '0x0000000000000000000000000000000000000002',
      400n,
      CONTROLLER,
      1_800_000_000n,
      1_800_003_600n,
      6_000_000_000n,
      6_000_000_000n,
    ]
    vi.mocked(perpsApi.getPerpsVaultRequestIds)
      .mockResolvedValue(requestIdResponse(['400']))

    const { result } = renderHook(() => useVaultRequests({
      controller: CONTROLLER,
      isSenior: false,
      currentEpoch: 500n,
    }))

    await waitFor(() => {
      expect(result.current.depositRequests[0]?.requestId).toBe(400n)
    })
    expect(result.current.depositRequests[0]).toMatchObject({
      activationTimestamp: 1_800_000_000,
      cooldownEndsAt: 1_800_003_600n,
      directRedeemableShares: 6_000_000_000n,
    })
  })

  it('paginates strictly and exposes stale confirmed discovery data', async () => {
    vi.mocked(perpsApi.getPerpsVaultRequestIds)
      .mockResolvedValueOnce(requestIdResponse(['400', '300'], 'junior', '300', true))
      .mockResolvedValueOnce(requestIdResponse(['200'], 'junior'))

    const { result } = renderHook(() => useVaultRequests({
      controller: CONTROLLER,
      isSenior: false,
      currentEpoch: 500n,
    }))

    await waitFor(() => expect(result.current.isLoading).toBe(false))
    expect(result.current.discoveryError).toBe(false)
    expect(result.current.discoveryStale).toBe(true)
    expect(perpsApi.getPerpsVaultRequestIds).toHaveBeenNthCalledWith(
      2,
      'junior',
      CONTROLLER,
      '300',
      250,
      expect.any(AbortSignal),
    )
  })
})
