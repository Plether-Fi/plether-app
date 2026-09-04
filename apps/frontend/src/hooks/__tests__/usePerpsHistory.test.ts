import { createElement, type ReactNode } from 'react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { act, renderHook, waitFor } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { usePerpsHistory, waitForPerpsOrderTerminal } from '../usePerpsHistory'

const identityMocks = vi.hoisted(() => ({
  ownerAddress: undefined as string | undefined,
  accountAddress: undefined as string | undefined,
}))

vi.mock('../../perps-aa', () => ({
  usePerpsIdentity: () => identityMocks,
}))

function createQueryWrapper() {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: {
        gcTime: 0,
        retry: false,
      },
    },
  })

  return function QueryWrapper({ children }: { children: ReactNode }) {
    return createElement(QueryClientProvider, { client: queryClient }, children)
  }
}

beforeEach(() => {
  identityMocks.ownerAddress = undefined
  identityMocks.accountAddress = undefined
})

afterEach(() => {
  vi.unstubAllGlobals()
})

describe('waitForPerpsOrderTerminal', () => {
  it('preserves canonical V2 receipt evidence and signed VPI from the backend', async () => {
    const fetchMock = vi.fn(async () => new Response(JSON.stringify({
      data: {
        timedOut: false,
        order: {
          orderId: '9202',
          account: '0x10cf39340e1a5307e45f1de989ce7b21915ef377',
          clientOrderId: `0x${'12'.repeat(32)}`,
          side: 1,
          commitTxHash: '0x54237f181c19e86acfd661fd217e219fd6570227dc5f0b9815589a9d278f6104',
          commitTimestamp: 1_785_437_833,
          terminalTxHash: '0xebbbf75e5b32d516e9e0398d9a7b1647a1dcf434b385c0e90b123b815957eaed',
          terminalBlockNumber: '190002345',
          terminalBlockHash: '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
          terminalTimestamp: 1_785_437_841,
          terminalStatus: 'Executed',
          terminalReason: 'Executed',
          executionMode: 'Live',
          receiptHash: `0x${'34'.repeat(32)}`,
          receiptEconomics: {
            executionBountyUsdc: '200000',
            vpiUsdc: '182822887',
            frozenSpreadUsdc: '0',
          },
          executionPrice: '98391251',
          executionOraclePrice: '98391482',
          oracleMinPublishTime: '1785437834',
          oracleMaxPublishTime: '1785437834',
          oracleDerivationVersion: 1,
          executionEconomicsVersion: 2,
          activityType: 'Close',
          activityPrice: '98391251',
          activityVpiUsdc: '-182822887',
        },
      },
    }), {
      status: 200,
      headers: { 'Content-Type': 'application/json' },
    }))
    vi.stubGlobal('fetch', fetchMock)

    const result = await waitForPerpsOrderTerminal({
      accountAddress: '0x10cf39340e1a5307e45f1de989ce7b21915ef377',
      orderId: 9202n,
    })

    expect(result).toEqual({
      timedOut: false,
      order: expect.objectContaining({
        orderId: 9202n,
        clientOrderId: `0x${'12'.repeat(32)}`,
        receiptHash: `0x${'34'.repeat(32)}`,
        terminalReason: 'Executed',
        terminalBlockNumberRaw: 190_002_345n,
        terminalBlockHash:
          '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
        executionPriceRaw: 98_391_251n,
        executionOraclePriceRaw: 98_391_482n,
        executionOracleFrozen: false,
        oracleMinPublishTimeRaw: 1_785_437_834n,
        oracleMaxPublishTimeRaw: 1_785_437_834n,
        oracleDerivationVersion: 1,
        vpiUsdcRaw: 182_822_887n,
        receiptEconomics: expect.objectContaining({
          executionBountyUsdc: '200000',
        }),
        executionEconomicsVersion: 2,
        activityPriceRaw: 98_391_251n,
        activityVpiUsdcRaw: -182_822_887n,
      }),
    })
    expect(fetchMock).toHaveBeenCalledOnce()
    expect(String(fetchMock.mock.calls[0]?.[0])).toContain('/perps/orders/9202/wait')
  })

  it('rejects V1 history rows without a client intent identity', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify({
      data: {
        timedOut: false,
        order: {
          orderId: '9202',
          account: '0x10cf39340e1a5307e45f1de989ce7b21915ef377',
          terminalStatus: 'Executed',
          terminalTxHash:
            '0xebbbf75e5b32d516e9e0398d9a7b1647a1dcf434b385c0e90b123b815957eaed',
        },
      },
    }), {
      status: 200,
      headers: { 'Content-Type': 'application/json' },
    })))

    await expect(waitForPerpsOrderTerminal({ orderId: 9202n })).resolves.toEqual({
      timedOut: false,
      order: undefined,
    })
  })

  it('exposes the sequenced orders height and resets it with the account history', async () => {
    const accountAddress = '0x10cf39340e1a5307e45f1de989ce7b21915ef377'
    identityMocks.ownerAddress = accountAddress
    identityMocks.accountAddress = accountAddress
    const fetchMock = vi.fn(async (input: string | URL | Request) => {
      const url = String(input)
      if (url.includes('/orders')) {
        return new Response(JSON.stringify({
          data: {
            orders: [],
            indexedThroughBlock: '190002400',
          },
        }), {
          status: 200,
          headers: { 'Content-Type': 'application/json' },
        })
      }
      return new Response(JSON.stringify({
        data: { activity: [] },
      }), {
        status: 200,
        headers: { 'Content-Type': 'application/json' },
      })
    })
    vi.stubGlobal('fetch', fetchMock)

    const { result, rerender } = renderHook(() => usePerpsHistory(), {
      wrapper: createQueryWrapper(),
    })

    await waitFor(() => {
      expect(result.current.ordersIndexedThroughBlockRaw).toBe(190_002_400n)
    })

    identityMocks.ownerAddress = undefined
    identityMocks.accountAddress = undefined
    rerender()

    await waitFor(() => {
      expect(result.current.ordersIndexedThroughBlockRaw).toBeUndefined()
      expect(result.current.orderHistory).toEqual([])
    })
  })

  it('cancels the old account request and keeps the new indexed snapshot', async () => {
    const firstAccountAddress = '0x10cf39340e1a5307e45f1de989ce7b21915ef377'
    const secondAccountAddress = '0x20cf39340e1a5307e45f1de989ce7b21915ef388'
    identityMocks.ownerAddress = firstAccountAddress
    identityMocks.accountAddress = firstAccountAddress
    let firstRequestSignal: AbortSignal | undefined
    const fetchMock = vi.fn(async (
      input: string | URL | Request,
      init?: RequestInit
    ) => {
      const url = String(input)
      if (url.includes(firstAccountAddress)) {
        firstRequestSignal = init?.signal as AbortSignal | undefined
        return await new Promise<Response>((_resolve, reject) => {
          firstRequestSignal?.addEventListener('abort', () => {
            reject(new DOMException('The operation was aborted', 'AbortError'))
          }, { once: true })
        })
      }
      return new Response(JSON.stringify({
        data: {
          orders: [],
          indexedThroughBlock: '190002500',
        },
      }), {
        status: 200,
        headers: { 'Content-Type': 'application/json' },
      })
    })
    vi.stubGlobal('fetch', fetchMock)

    const { result, rerender } = renderHook(() => usePerpsHistory(), {
      wrapper: createQueryWrapper(),
    })
    await waitFor(() => {
      expect(firstRequestSignal).toBeDefined()
    })

    identityMocks.ownerAddress = secondAccountAddress
    identityMocks.accountAddress = secondAccountAddress
    rerender()

    await waitFor(() => {
      expect(firstRequestSignal?.aborted).toBe(true)
      expect(result.current.ordersIndexedThroughBlockRaw).toBe(190_002_500n)
    })
  })

  it('loads activity only while the transaction-history consumer enables it', async () => {
    const accountAddress = '0x10cf39340e1a5307e45f1de989ce7b21915ef377'
    identityMocks.ownerAddress = accountAddress
    identityMocks.accountAddress = accountAddress
    let orderRequestCount = 0
    let activityRequestCount = 0
    let activityRequestSignal: AbortSignal | undefined
    const fetchMock = vi.fn(async (
      input: string | URL | Request,
      init?: RequestInit
    ) => {
      const url = String(input)
      if (url.includes('/orders')) {
        orderRequestCount += 1
        return new Response(JSON.stringify({
          data: {
            orders: [],
            indexedThroughBlock: '190002500',
          },
        }), {
          status: 200,
          headers: { 'Content-Type': 'application/json' },
        })
      }

      activityRequestCount += 1
      activityRequestSignal = init?.signal as AbortSignal | undefined
      return await new Promise<Response>((_resolve, reject) => {
        activityRequestSignal?.addEventListener('abort', () => {
          reject(new DOMException('The operation was aborted', 'AbortError'))
        }, { once: true })
      })
    })
    vi.stubGlobal('fetch', fetchMock)

    const { result, rerender } = renderHook(
      ({ activityEnabled }) => usePerpsHistory({ activityEnabled }),
      {
        initialProps: { activityEnabled: false },
        wrapper: createQueryWrapper(),
      }
    )

    await waitFor(() => {
      expect(orderRequestCount).toBe(1)
    })
    expect(activityRequestCount).toBe(0)

    rerender({ activityEnabled: true })
    await waitFor(() => {
      expect(activityRequestCount).toBe(1)
    })

    rerender({ activityEnabled: false })
    await waitFor(() => {
      expect(activityRequestSignal?.aborted).toBe(true)
    })
    await act(async () => {
      await result.current.refetch()
    })

    expect(orderRequestCount).toBe(2)
    expect(activityRequestCount).toBe(1)
  })
})
