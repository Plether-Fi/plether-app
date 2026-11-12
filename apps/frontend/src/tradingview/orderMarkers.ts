import type { PerpsOrderHistoryRow } from '../hooks/usePerpsHistory'
import { oraclePriceToDisplayDxyPrice } from '../utils/perps'

export interface OrderMarker {
  key: string
  time: number
  price: number
  side: 'buy' | 'sell'
}

export function orderHistoryToMarkers(orders: PerpsOrderHistoryRow[]): OrderMarker[] {
  return orders.flatMap((order) => {
    if (order.status !== 'Executed' ||
      (order.type !== 'Open' && order.type !== 'Close') ||
      (order.side !== 'Long' && order.side !== 'Short')) return []

    const time = order.terminalTimestampSeconds
    const rawPrice = order.executionPriceRaw ?? order.activityPriceRaw
    if (time === undefined || !Number.isSafeInteger(time) || time <= 0 || rawPrice === undefined) return []

    const displayPrice = oraclePriceToDisplayDxyPrice(rawPrice)
    if (displayPrice === undefined || displayPrice <= 0n) return []
    const price = Number(displayPrice) / 1e8
    if (!Number.isFinite(price)) return []

    const side = (order.type === 'Open') === (order.side === 'Long') ? 'buy' : 'sell'
    return [{ key: `${order.account}:${order.orderId.toString()}`, time, price, side }]
  })
}
