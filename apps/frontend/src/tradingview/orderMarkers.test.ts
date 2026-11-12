import { describe, expect, it } from 'vitest'
import type { PerpsOrderHistoryRow } from '../hooks/usePerpsHistory'
import { orderHistoryToMarkers } from './orderMarkers'

const baseOrder: PerpsOrderHistoryRow = {
  orderId: 1n,
  time: '12 Nov, 14:00',
  terminalTimestampSeconds: 1_800_000_120,
  market: 'plDXY Perp',
  side: 'Long',
  type: 'Open',
  price: '1.0161',
  size: '100',
  status: 'Executed',
  account: '0x1111111111111111111111111111111111111111',
  clientOrderId: `0x${'22'.repeat(32)}`,
  executionPriceRaw: 98_390_000n,
}

describe('orderHistoryToMarkers', () => {
  it('maps open and close executions to buy and sell at their actual time and display price', () => {
    const orders = [
      baseOrder,
      { ...baseOrder, orderId: 2n, side: 'Short', type: 'Close' },
      { ...baseOrder, orderId: 3n, side: 'Short', type: 'Open' },
      { ...baseOrder, orderId: 4n, side: 'Long', type: 'Close' },
    ]
    expect(orderHistoryToMarkers(orders)).toEqual(orders.map((order, index) => ({
      key: `${order.account}:${order.orderId.toString()}`,
      time: 1_800_000_120,
      price: 1.0161,
      side: index < 2 ? 'buy' : 'sell',
    })))
  })

  it('omits orders without a confirmed execution, timestamp, or price', () => {
    expect(orderHistoryToMarkers([
      { ...baseOrder, status: 'Committed' },
      { ...baseOrder, status: 'Failed: Slippage' },
      { ...baseOrder, type: 'Executed' },
      { ...baseOrder, terminalTimestampSeconds: undefined },
      { ...baseOrder, executionPriceRaw: undefined },
      { ...baseOrder, executionPriceRaw: 200_000_000n },
    ])).toEqual([])
  })
})
