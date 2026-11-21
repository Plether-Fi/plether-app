import { usePerpsHistory } from '../hooks/usePerpsHistory'
import type { SponsoredOperation } from '../perps-aa/operationStore'
import type { Address, Hex } from 'viem'
import { Button } from './ui/Button'

/** Submission confirmation never stands in for trade execution. */
export function RecoveredOrderStatus(props: { operation: SponsoredOperation } | {
  accountAddress: Address
  transactionHash: Hex
  clientOrderId?: Hex | undefined
}) {
  const history = usePerpsHistory()
  const account = ('operation' in props ? props.operation.accountAddress : props.accountAddress).toLowerCase()
  const clientOrderId = ('operation' in props ? props.operation.orderRequestV3?.clientOrderId : props.clientOrderId)?.toLowerCase()
  const transactionHash = 'operation' in props
    ? props.operation.transactionHashVerified === true ? props.operation.transactionHash?.toLowerCase() : undefined
    : props.transactionHash.toLowerCase()
  const matches = history.orderHistory.filter(order =>
    order.account.toLowerCase() === account
    && (clientOrderId !== undefined
      ? order.clientOrderId.toLowerCase() === clientOrderId
      : transactionHash !== undefined && order.commitTxHash?.toLowerCase() === transactionHash))
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
