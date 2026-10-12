import { usePerpsHistory } from '../hooks/usePerpsHistory'
import type { SponsoredOperation } from '../perps-aa/operationStore'
import { Button } from './ui/Button'

/** Submission confirmation never stands in for trade execution. */
export function RecoveredOrderStatus({ operation }: { operation: SponsoredOperation }) {
  const history = usePerpsHistory()
  const matches = history.orderHistory.filter(order =>
    order.account.toLowerCase() === operation.accountAddress.toLowerCase()
    && (operation.orderRequestV3
      ? order.clientOrderId.toLowerCase() === operation.orderRequestV3.clientOrderId.toLowerCase()
      : operation.transactionHashVerified === true && operation.transactionHash !== undefined
        && order.commitTxHash?.toLowerCase() === operation.transactionHash.toLowerCase()))
  const order = matches.length === 1 ? matches[0] : undefined
  return <div className="space-y-2 border border-positive/30 bg-positive/5 p-3 text-sm">
    <p className="font-semibold text-content-primary">Order submitted</p>
    {order ? <>
      <p>Execution status: <strong>{order.status === 'Committed' ? 'Awaiting execution' : order.status}</strong></p>
      <p className="text-xs text-content-secondary">Order #{order.orderId.toString()} · {order.side} · {order.size}</p>
      {history.orderHistoryError && <p>The latest update is unavailable. Showing the last known status.</p>}
    </> : <p>{history.isOrderHistoryLoading ? 'Loading execution status…'
      : 'Execution status is not available yet. The order was submitted; this does not confirm that the trade executed.'}</p>}
    <Button type="button" variant="secondary" size="sm" onClick={() => { void history.refetch() }}>Refresh order status</Button>
  </div>
}
