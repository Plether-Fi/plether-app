import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { act, renderHook, waitFor } from '@testing-library/react'
import { Result } from 'better-result'
import type { PropsWithChildren } from 'react'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { perpsApi, PlethApiError } from '../../api/client'
import type { ApiResponse, VaultActivity } from '../../api/types'
import { PERPS_ARBITRUM_SEPOLIA } from '../../contracts/perpsAddresses'
import { useVaultActivity } from '../useVaultActivity'

const HOLDER = '0x1111111111111111111111111111111111111111'
const TX_A = `0x${'aa'.repeat(32)}`
const TX_B = `0x${'bb'.repeat(32)}`

const response: ApiResponse<VaultActivity> = {
  data: {
    deployment: {
      chainId: 421_614,
      housePool: PERPS_ARBITRUM_SEPOLIA.housePool,
      seniorVault: PERPS_ARBITRUM_SEPOLIA.seniorVault,
      juniorVault: PERPS_ARBITRUM_SEPOLIA.juniorVault,
      deploymentBlock: 302_257_125,
    },
    coverage: {
      confirmedThroughBlock: 302_300_000,
      confirmedThroughHash: `0x${'cc'.repeat(32)}`,
      observedSafeHeadBlock: 302_300_000,
      observedSafeHeadHash: `0x${'cc'.repeat(32)}`,
      complete: true,
      stale: false,
      lagBlocks: 0,
      lagSeconds: 0,
      lastSuccessfulPoll: 1_700_000_000,
    },
    senior: {
      holders: [{ address: HOLDER, shareBalance: '10' }],
      holderCount: 1,
      holdersTruncated: false,
      activity: [{
        id: `${TX_A}-1`,
        tranche: 'senior',
        kind: 'deposit',
        account: HOLDER,
        requestId: '100',
        rawAssets: '50',
        rawShares: null,
        timestamp: 1_700_000_000,
        blockNumber: 302_300_000,
        transactionIndex: 0,
        logIndex: 1,
        transactionHash: TX_A,
      }],
      activityCount: 1,
      activityTruncated: false,
    },
    junior: {
      holders: [{ address: HOLDER, shareBalance: '20' }],
      holderCount: 1,
      holdersTruncated: false,
      activity: [{
        id: `${TX_B}-2`,
        tranche: 'junior',
        kind: 'withdraw',
        account: HOLDER,
        requestId: '101',
        rawAssets: null,
        rawShares: '10',
        timestamp: 1_700_000_001,
        blockNumber: 302_300_000,
        transactionIndex: 1,
        logIndex: 2,
        transactionHash: TX_B,
      }],
      activityCount: 1,
      activityTruncated: false,
    },
  },
  meta: {
    cached: false,
    blockNumber: 302_300_000,
    chainId: 421_614,
  },
}

function wrapper(client: QueryClient) {
  return function Wrapper({ children }: PropsWithChildren) {
    return <QueryClientProvider client={client}>{children}</QueryClientProvider>
  }
}

afterEach(() => {
  vi.restoreAllMocks()
})

describe('useVaultActivity', () => {
  it('derives live NAV from backend shares and retains it after a refresh failure', async () => {
    const refreshFailure = Result.err(
      new PlethApiError('NETWORK_ERROR', 'Alchemy-backed activity is unavailable', 503),
    )
    const getActivity = vi.spyOn(perpsApi, 'getPerpsVaultActivity')
      .mockResolvedValueOnce(Result.ok(response))
      .mockResolvedValue(refreshFailure)
    const client = new QueryClient({
      defaultOptions: { queries: { retry: false } },
    })
    const { result } = renderHook(() => useVaultActivity({
      seniorTotalAssets: 1_000n,
      seniorEffectiveSupply: 100n,
      juniorTotalAssets: 500n,
      juniorEffectiveSupply: 100n,
    }), { wrapper: wrapper(client) })

    await waitFor(() => expect(result.current.holders).toHaveLength(1))
    expect(result.current.holders[0]).toMatchObject({
      address: HOLDER,
      seniorNavUsdc: 100n,
      juniorNavUsdc: 100n,
      currentNavUsdc: 200n,
    })
    expect(result.current.activity.map(({ amountUsdc, amountIsEstimate }) => ({
      amountUsdc,
      amountIsEstimate,
    }))).toEqual([
      { amountUsdc: 50n, amountIsEstimate: true },
      { amountUsdc: 50n, amountIsEstimate: false },
    ])

    await act(async () => {
      await result.current.refetch()
    })
    await waitFor(() => expect(result.current.isStale).toBe(true))
    expect(result.current.isError).toBe(false)
    expect(result.current.holders[0]?.currentNavUsdc).toBe(200n)
    expect(getActivity).toHaveBeenCalledTimes(3)
    client.clear()
  })
})
