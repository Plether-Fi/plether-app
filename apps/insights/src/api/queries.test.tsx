import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { act, renderHook, waitFor } from '@testing-library/react'
import type { PropsWithChildren } from 'react'
import { afterEach, describe, expect, it, vi } from 'vitest'
import * as client from './client'
import type {
  KeeperResponse,
  KeepersResponse,
  ParameterChangesResponse,
  ProtocolWalletResponse,
  ProtocolWalletsResponse,
  TrancheHistoryResponse,
} from './types'
import {
  useKeeper,
  useKeepers,
  useParameterChanges,
  useProtocolWallet,
  useProtocolWallets,
  useTrancheHistory,
} from './queries'

vi.mock('./client', async (importOriginal) => {
  const actual = await importOriginal<typeof import('./client')>()
  return {
    ...actual,
    getKeeper: vi.fn(),
    getKeepers: vi.fn(),
    getParameterChanges: vi.fn(),
    getProtocolWallet: vi.fn(),
    getProtocolWallets: vi.fn(),
    getTrancheHistory: vi.fn(),
  }
})

afterEach(() => {
  vi.clearAllMocks()
})

describe('protocol cursor query hooks', () => {
  it('paginates keeper addresses with keepers.nextCursor', async () => {
    const getKeepers = vi.mocked(client.getKeepers)
    getKeepers
      .mockResolvedValueOnce(keepersPage('keepers-next'))
      .mockResolvedValueOnce(keepersPage(null))

    const { result } = renderHook(
      () => useKeepers('release-1', '24h', 20),
      { wrapper: queryWrapper() },
    )

    await waitFor(() => expect(result.current.hasNextPage).toBe(true))
    await act(async () => {
      await result.current.fetchNextPage()
    })

    expect(getKeepers).toHaveBeenNthCalledWith(
      1,
      'release-1',
      expect.objectContaining({ window: '24h', limit: 20, cursor: undefined }),
    )
    expect(getKeepers).toHaveBeenNthCalledWith(
      2,
      'release-1',
      expect.objectContaining({ window: '24h', limit: 20, cursor: 'keepers-next' }),
    )
    await waitFor(() => expect(result.current.hasNextPage).toBe(false))
  })

  it('paginates tranche history with history.nextCursor', async () => {
    const getTrancheHistory = vi.mocked(client.getTrancheHistory)
    getTrancheHistory
      .mockResolvedValueOnce(trancheHistoryPage('combined-next'))
      .mockResolvedValueOnce(trancheHistoryPage(null))

    const { result } = renderHook(
      () => useTrancheHistory('release-1', 'senior', 25),
      { wrapper: queryWrapper() },
    )

    await waitFor(() => expect(result.current.hasNextPage).toBe(true))
    await act(async () => {
      await result.current.fetchNextPage()
    })

    expect(getTrancheHistory).toHaveBeenNthCalledWith(
      1,
      'release-1',
      'senior',
      expect.objectContaining({ limit: 25, cursor: undefined }),
    )
    expect(getTrancheHistory).toHaveBeenNthCalledWith(
      2,
      'release-1',
      'senior',
      expect.objectContaining({ limit: 25, cursor: 'combined-next' }),
    )
    await waitFor(() => expect(result.current.hasNextPage).toBe(false))
  })

  it('paginates a keeper action feed with keeper.nextCursor', async () => {
    const getKeeper = vi.mocked(client.getKeeper)
    getKeeper
      .mockResolvedValueOnce(keeperPage('keeper-next'))
      .mockResolvedValueOnce(keeperPage(null))

    const { result } = renderHook(
      () => useKeeper('release-1', '0xABC', '30d', 40),
      { wrapper: queryWrapper() },
    )

    await waitFor(() => expect(result.current.hasNextPage).toBe(true))
    await act(async () => {
      await result.current.fetchNextPage()
    })

    expect(getKeeper).toHaveBeenNthCalledWith(
      1,
      'release-1',
      '0xABC',
      expect.objectContaining({ window: '30d', limit: 40, cursor: undefined }),
    )
    expect(getKeeper).toHaveBeenNthCalledWith(
      2,
      'release-1',
      '0xABC',
      expect.objectContaining({ window: '30d', limit: 40, cursor: 'keeper-next' }),
    )
    await waitFor(() => expect(result.current.hasNextPage).toBe(false))
  })

  it('paginates governance history with parameterChanges.nextCursor', async () => {
    const getParameterChanges = vi.mocked(client.getParameterChanges)
    getParameterChanges
      .mockResolvedValueOnce(parameterChangesPage('parameter-next'))
      .mockResolvedValueOnce(parameterChangesPage(null))

    const { result } = renderHook(
      () => useParameterChanges('release-1', 75),
      { wrapper: queryWrapper() },
    )

    await waitFor(() => expect(result.current.hasNextPage).toBe(true))
    await act(async () => {
      await result.current.fetchNextPage()
    })

    expect(getParameterChanges).toHaveBeenNthCalledWith(
      1,
      'release-1',
      expect.objectContaining({ limit: 75, cursor: undefined }),
    )
    expect(getParameterChanges).toHaveBeenNthCalledWith(
      2,
      'release-1',
      expect.objectContaining({ limit: 75, cursor: 'parameter-next' }),
    )
    await waitFor(() => expect(result.current.hasNextPage).toBe(false))
  })

  it('paginates operational wallet addresses and detail activity independently', async () => {
    const getProtocolWallets = vi.mocked(client.getProtocolWallets)
    const getProtocolWallet = vi.mocked(client.getProtocolWallet)
    getProtocolWallets
      .mockResolvedValueOnce(protocolWalletsPage('wallets-next'))
      .mockResolvedValueOnce(protocolWalletsPage(null))
    getProtocolWallet
      .mockResolvedValueOnce(protocolWalletPage('wallet-next'))
      .mockResolvedValueOnce(protocolWalletPage(null))

    const list = renderHook(
      () => useProtocolWallets('release-1', '24h', 20),
      { wrapper: queryWrapper() },
    )
    await waitFor(() => expect(list.result.current.hasNextPage).toBe(true))
    await act(async () => {
      await list.result.current.fetchNextPage()
    })
    expect(getProtocolWallets).toHaveBeenNthCalledWith(
      2,
      'release-1',
      expect.objectContaining({ window: '24h', limit: 20, cursor: 'wallets-next' }),
    )

    const detail = renderHook(
      () => useProtocolWallet('release-1', '0xABC', '30d', 40),
      { wrapper: queryWrapper() },
    )
    await waitFor(() => expect(detail.result.current.hasNextPage).toBe(true))
    await act(async () => {
      await detail.result.current.fetchNextPage()
    })
    expect(getProtocolWallet).toHaveBeenNthCalledWith(
      2,
      'release-1',
      '0xABC',
      expect.objectContaining({ window: '30d', limit: 40, cursor: 'wallet-next' }),
    )
  })
})

function queryWrapper() {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: {
        retry: false,
      },
    },
  })

  return function QueryWrapper({ children }: PropsWithChildren) {
    return <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
  }
}

function trancheHistoryPage(nextCursor: string | null): TrancheHistoryResponse {
  return {
    history: {
      nextCursor,
      nextCursors: {
        combined: nextCursor,
        actions: nextCursor === null ? null : 'actions-next',
        checkpoints: nextCursor === null ? null : 'checkpoints-next',
      },
      pagination: {
        actionsComplete: nextCursor === null,
        checkpointsComplete: nextCursor === null,
      },
    },
  } as TrancheHistoryResponse
}

function keeperPage(nextCursor: string | null): KeeperResponse {
  return {
    keeper: { nextCursor },
  } as KeeperResponse
}

function keepersPage(nextCursor: string | null): KeepersResponse {
  return {
    keepers: { nextCursor },
  } as KeepersResponse
}

function parameterChangesPage(nextCursor: string | null): ParameterChangesResponse {
  return {
    parameterChanges: { nextCursor },
  } as ParameterChangesResponse
}

function protocolWalletsPage(nextCursor: string | null): ProtocolWalletsResponse {
  return {
    wallets: { nextCursor },
  } as ProtocolWalletsResponse
}

function protocolWalletPage(nextCursor: string | null): ProtocolWalletResponse {
  return {
    wallet: { nextCursor },
  } as ProtocolWalletResponse
}
