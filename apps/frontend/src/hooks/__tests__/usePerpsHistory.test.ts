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
            executionBountyAssets: '200000',
            vpiAssets: '182822887',
            frozenSpreadAssets: '0',
          },
          executionPrice: '98391251',
          executionOraclePrice: '98391482',
          oracleMinPublishTime: '1785437834',
          oracleMaxPublishTime: '1785437834',
          oracleDerivationVersion: 1,
          executionEconomicsVersion: 2,
          activityType: 'Close',
          activityPrice: '98391251',
          activityVpiAssets: '-182822887',
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
        vpiAssetsRaw: 182_822_887n,
        receiptEconomics: expect.objectContaining({
          executionBountyAssets: '200000',
        }),
        executionEconomicsVersion: 2,
        activityPriceRaw: 98_391_251n,
        activityVpiAssetsRaw: -182_822_887n,
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

describe('optional keeper execution progress', () => {
  const execution = { state: 'submitted', orderId: '42', orderRouter: `0x${'11'.repeat(20)}`,
    account: `0x${'22'.repeat(20)}`, clientOrderId: `0x${'33'.repeat(32)}`,
    commitTxHash: `0x${'44'.repeat(32)}`, transactionHash: `0x${'55'.repeat(32)}`, requiredConfirmations: 1 }
  it('round trips an optional progress cursor and keeps broadcast separate from terminal evidence', async () => {
    const controller = new AbortController()
    const fetchMock = vi.fn(async () => new Response(JSON.stringify({data:{timedOut:false,execution,executionVersion:'submitted:1'}})))
    vi.stubGlobal('fetch', fetchMock)
    const result = await waitForPerpsOrderTerminal({orderId:42n,accountAddress:execution.account,afterExecution:'none:0',signal:controller.signal})
    expect(result).toEqual({timedOut:false,order:undefined,execution:{...execution,orderId:42n},executionVersion:'submitted:1'})
    expect(String(fetchMock.mock.calls[0]?.[0])).toContain('afterExecution=none%3A0')
    expect(fetchMock.mock.calls[0]?.[1]).toEqual({signal:controller.signal})
  })
  it.each([{requiredConfirmations:-1},{requiredConfirmations:0},{requiredConfirmations:0.5},{requiredConfirmations:10001},
    {transactionHash:'0x12'},{clientOrderId:'0x12'},{account:'0x12'},{orderId:'-1'},{state:'executed'}])('rejects malformed advisory progress %j',async patch=>{
    vi.stubGlobal('fetch',vi.fn(async()=>new Response(JSON.stringify({data:{execution:{...execution,...patch},executionVersion:'bad:1'}}))))
    await expect(waitForPerpsOrderTerminal({orderId:42n})).resolves.toMatchObject({execution:null,order:undefined})
  })
  it('represents explicit clearing and remains compatible with an older backend',async()=>{
    const fetchMock=vi.fn().mockResolvedValueOnce(new Response(JSON.stringify({data:{execution:null,executionVersion:'none:2'}})))
      .mockResolvedValueOnce(new Response(JSON.stringify({data:{timedOut:true}})))
    vi.stubGlobal('fetch',fetchMock)
    await expect(waitForPerpsOrderTerminal({orderId:42n})).resolves.toMatchObject({execution:null,executionVersion:'none:2'})
    await expect(waitForPerpsOrderTerminal({orderId:42n})).resolves.toEqual({timedOut:true,order:undefined})
  })
})
