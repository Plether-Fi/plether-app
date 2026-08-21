import { renderHook, waitFor } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { PERPS_ARBITRUM_SEPOLIA } from '../../contracts/perpsAddresses'
import { usePendingVaultDeposits } from '../usePendingVaultDeposits'

const OWNER = '0x1111111111111111111111111111111111111111' as const

const mocks = vi.hoisted(() => ({
  epochId: 0n,
  epochResult: [0n, 0n, 0n, 0n, false] as readonly [bigint, bigint, bigint, bigint, boolean],
  fetch: vi.fn(),
  pendingAssets: 0n,
  readContractsArgs: vi.fn(),
  refetch: vi.fn(),
}))

vi.mock('wagmi', () => ({
  useReadContracts: (args: unknown) => {
    mocks.readContractsArgs(args)
    const contracts = (args as {
      contracts: readonly {
        functionName: string
        args: readonly unknown[]
      }[]
    }).contracts

    return {
      data: contracts.map((contract) => {
        const epochId = contract.functionName === 'pendingDepositAssets'
          ? contract.args[1]
          : contract.args[0]
        if (epochId !== mocks.epochId) {
          return {
            status: 'success' as const,
            result: contract.functionName === 'pendingDepositAssets'
              ? 0n
              : [0n, 0n, 0n, 0n, false],
          }
        }
        return {
          status: 'success' as const,
          result: contract.functionName === 'pendingDepositAssets'
            ? mocks.pendingAssets
            : mocks.epochResult,
        }
      }),
      isLoading: false,
      refetch: mocks.refetch,
    }
  },
}))

function successfulFetch(result: unknown[] = []) {
  mocks.fetch.mockResolvedValue({
    ok: true,
    status: 200,
    json: async () => ({ status: '1', result }),
  })
}

describe('usePendingVaultDeposits', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    window.localStorage.clear()
    vi.stubGlobal('fetch', mocks.fetch)
    successfulFetch()
    mocks.epochId = 0n
    mocks.pendingAssets = 0n
    mocks.epochResult = [0n, 0n, 0n, 0n, false]
  })

  it('finds a newly queued request from nearby onchain epochs before explorer indexing', async () => {
    const currentEpoch = BigInt(Math.floor(Date.now() / 3_600_000))
    mocks.epochId = currentEpoch + 2n
    mocks.pendingAssets = 25_000_000n
    mocks.epochResult = [100_000_000n, 0n, 0n, 0n, false]

    const { result } = renderHook(() => usePendingVaultDeposits({
      owner: OWNER,
      vaultAddress: PERPS_ARBITRUM_SEPOLIA.seniorVault,
      currentEpoch,
    }))

    await waitFor(() => {
      expect(result.current.deposits).toHaveLength(1)
    })
    expect(result.current.deposits[0]).toMatchObject({
      epochId: currentEpoch + 2n,
      assets: 25_000_000n,
      status: 'waiting',
      finalized: false,
    })
    expect(mocks.fetch).toHaveBeenCalledWith(
      expect.stringContaining(`topic2=0x000000000000000000000000${OWNER.slice(2)}`),
      expect.objectContaining({ signal: expect.any(AbortSignal) })
    )
  })

  it('calculates the wallet share claim from finalized batch accounting', async () => {
    const currentEpoch = BigInt(Math.floor(Date.now() / 3_600_000))
    mocks.epochId = currentEpoch - 2n
    mocks.pendingAssets = 25_000_000n
    mocks.epochResult = [100_000_000n, 50_000_000_000n, 20_000_000n, 10_000_000_000n, true]

    const { result } = renderHook(() => usePendingVaultDeposits({
      owner: OWNER,
      vaultAddress: PERPS_ARBITRUM_SEPOLIA.juniorVault,
      currentEpoch,
    }))

    await waitFor(() => {
      expect(result.current.deposits[0]?.status).toBe('claimable')
    })
    expect(result.current.deposits[0].claimableShares).toBe(12_500_000_000n)
  })

  it('restores an older unclaimed request from DepositRequested event history', async () => {
    const currentEpoch = BigInt(Math.floor(Date.now() / 3_600_000))
    const oldEpoch = currentEpoch - 100n
    mocks.epochId = oldEpoch
    mocks.pendingAssets = 10_000_000n
    mocks.epochResult = [10_000_000n, 0n, 0n, 0n, false]
    successfulFetch([{
      topics: [
        '0xeeda014808729eb8163550725955953141f4e8a7353951eb22f8c02dc2dcb813',
        `0x${'0'.repeat(64)}`,
        `0x${'0'.repeat(24)}${OWNER.slice(2)}`,
        `0x${oldEpoch.toString(16).padStart(64, '0')}`,
      ],
    }])

    const { result } = renderHook(() => usePendingVaultDeposits({
      owner: OWNER,
      vaultAddress: PERPS_ARBITRUM_SEPOLIA.seniorVault,
      currentEpoch,
    }))

    await waitFor(() => {
      expect(result.current.deposits[0]?.epochId).toBe(oldEpoch)
    })
    expect(result.current.deposits[0].status).toBe('ready')
  })
})
