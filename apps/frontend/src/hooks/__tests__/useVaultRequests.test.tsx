import { renderHook, waitFor } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { useVaultRequests } from '../useVaultRequests'

const CONTROLLER = '0x1111111111111111111111111111111111111111' as const

const mocks = vi.hoisted(() => ({
  fetch: vi.fn(),
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
}))

vi.mock('wagmi', () => ({
  useReadContracts: (args: unknown) => {
    const contracts = (args as {
      contracts: readonly { args: readonly [boolean, bigint, `0x${string}`] }[]
    }).contracts
    return {
      data: contracts.map((contract) => ({
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
      })),
      isLoading: false,
      refetch: mocks.refetch,
    }
  },
}))

function successfulFetch(result: unknown[] = []) {
  mocks.fetch.mockResolvedValue({
    ok: true,
    status: 200,
    json: async () => ({ status: result.length > 0 ? '1' : '0', result }),
  })
}

describe('useVaultRequests', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    window.localStorage.clear()
    vi.stubGlobal('fetch', mocks.fetch)
    successfulFetch()
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

  it('restores an older claimable request discovered through event history', async () => {
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
    successfulFetch([{
      topics: [
        `0x${'0'.repeat(64)}`,
        `0x${'0'.repeat(24)}${CONTROLLER.slice(2)}`,
        `0x${'0'.repeat(64)}`,
        `0x${mocks.requestId.toString(16).padStart(64, '0')}`,
      ],
    }])

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
    expect(mocks.fetch).toHaveBeenCalledTimes(3)
  })
})
