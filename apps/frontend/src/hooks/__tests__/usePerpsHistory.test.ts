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

beforeEach(() => {
  identityMocks.ownerAddress = undefined
  identityMocks.accountAddress = undefined
})

afterEach(() => {
  vi.unstubAllGlobals()
})

describe('waitForPerpsOrderTerminal', () => {
  it('preserves immutable execution evidence and signed VPI from the backend', async () => {
    const fetchMock = vi.fn(async () => new Response(JSON.stringify({
      data: {
        timedOut: false,
        order: {
          orderId: '9202',
          account: '0x10cf39340e1a5307e45f1de989ce7b21915ef377',
          side: 1,
          commitTxHash: '0x54237f181c19e86acfd661fd217e219fd6570227dc5f0b9815589a9d278f6104',
          commitTimestamp: 1_785_437_833,
          terminalTxHash: '0xebbbf75e5b32d516e9e0398d9a7b1647a1dcf434b385c0e90b123b815957eaed',
          terminalBlockNumber: '190002345',
          terminalBlockHash: '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
          terminalTimestamp: 1_785_437_841,
          terminalStatus: 'Executed',
          executionPrice: '98391251',
          executionOraclePrice: '98391482',
          executionOracleFrozen: false,
          oracleMinPublishTime: '1785437834',
          oracleMaxPublishTime: '1785437834',
          oracleDerivationVersion: 1,
          vpiUsdc: '182822887',
          executionEconomicsVersion: 1,
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
        executionEconomicsVersion: 1,
        activityPriceRaw: 98_391_251n,
        activityVpiUsdcRaw: -182_822_887n,
      }),
    })
    expect(fetchMock).toHaveBeenCalledOnce()
    expect(String(fetchMock.mock.calls[0]?.[0])).toContain('/perps/orders/9202/wait')
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

    const { result, rerender } = renderHook(() => usePerpsHistory())

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

  it('does not let an older history request overwrite a newer indexed snapshot', async () => {
    const accountAddress = '0x10cf39340e1a5307e45f1de989ce7b21915ef377'
    identityMocks.ownerAddress = accountAddress
    identityMocks.accountAddress = accountAddress
    let resolveFirstOrders: ((response: Response) => void) | undefined
    let orderRequestCount = 0
    const fetchMock = vi.fn(async (input: string | URL | Request) => {
      const url = String(input)
      if (!url.includes('/orders')) {
        return new Response(JSON.stringify({ data: { activity: [] } }), {
          status: 200,
          headers: { 'Content-Type': 'application/json' },
        })
      }

      orderRequestCount += 1
      if (orderRequestCount === 1) {
        return await new Promise<Response>((resolve) => {
          resolveFirstOrders = resolve
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

    const { result } = renderHook(() => usePerpsHistory())
    await waitFor(() => {
      expect(orderRequestCount).toBe(1)
    })

    await act(async () => {
      await result.current.refetch()
    })
    expect(result.current.ordersIndexedThroughBlockRaw).toBe(190_002_500n)

    await act(async () => {
      resolveFirstOrders?.(new Response(JSON.stringify({
        data: {
          orders: [],
          indexedThroughBlock: '190002450',
        },
      }), {
        status: 200,
        headers: { 'Content-Type': 'application/json' },
      }))
    })

    expect(result.current.ordersIndexedThroughBlockRaw).toBe(190_002_500n)
  })
})
