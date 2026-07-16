import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { renderHook } from '@testing-library/react'
import type { ReactNode } from 'react'
import { describe, expect, it, vi } from 'vitest'
import { usePerpsTrading } from '../usePerpsTrading'

const publicClientMocks = vi.hoisted(() => ({
  readContract: vi.fn(),
  simulateContract: vi.fn(),
}))

vi.mock('wagmi', () => ({
  usePublicClient: () => ({
    readContract: publicClientMocks.readContract,
    simulateContract: publicClientMocks.simulateContract,
  }),
  useSignTypedData: () => ({
    signTypedDataAsync: vi.fn(),
  }),
}))

vi.mock('../../perps-aa', async (importOriginal) => {
  const actual = await importOriginal<typeof import('../../perps-aa')>()
  const ownerAddress = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
  return {
    ...actual,
    usePerpsAaRuntime: () => undefined,
    usePerpsIdentity: () => ({
      status: 'blocked',
      ownerAddress,
      accountAddress: undefined,
      chainId: 421614,
      isAaManifestConfigured: true,
      sponsorshipEnabled: false,
      manifest: null,
      identity: null,
      proposedIdentity: null,
      changedIdentityFields: [],
      error: {
        code: 'SPONSORSHIP_MANIFEST_REQUIRED',
        message: 'Perps is sponsorship-only on testnet.',
      },
      confirmIdentityAfterContinuityCheck: () => false,
      reloadIdentity: () => undefined,
    }),
  }
})

function wrapper({ children }: { children: ReactNode }) {
  return (
    <QueryClientProvider client={new QueryClient()}>
      {children}
    </QueryClientProvider>
  )
}

describe('usePerpsTrading sponsorship-only mode', () => {
  it('never exposes a direct owner-wallet approval path', async () => {
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.approveUsdcForMargin(25_000_000n))
      .rejects.toThrow('Direct USDC approvals are disabled')
  })

  it('blocks order submission until the Trading Account is ready', async () => {
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.commitOrder({
      direction: 'long',
      notionalUsdc: 1_000_000_000n,
      sizeDelta: 1_000_000_000_000_000_000n,
      marginUsdc: 200_000_000n,
      oraclePrice: 98_300_000n,
      slippagePercent: 0.1,
      isClose: false,
    })).rejects.toThrow(
      'Confirm the Plether Trading Account before committing an order'
    )

    expect(publicClientMocks.simulateContract).not.toHaveBeenCalled()
  })

  it('keeps manual finalization and cleanup keeper-only', async () => {
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.executeOrder(42n))
      .rejects.toThrow('Order finalization is keeper-operated')
    await expect(result.current.cleanupExpiredOrder(42n))
      .rejects.toThrow('Expired-order cleanup is keeper-operated')
  })
})
