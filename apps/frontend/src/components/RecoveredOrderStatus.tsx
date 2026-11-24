import { usePerpsHistory } from '../hooks/usePerpsHistory'
import type { SponsoredOperation } from '../perps-aa/operationStore'
import type { Address, Hex } from 'viem'
import { Button } from './ui/Button'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { usePerpsTrading } from '../hooks/usePerpsTrading'
import { PERPS_LIFECYCLE_STATUS } from '../contracts/perpsOrderV3'

/** Submission confirmation never stands in for trade execution. */
export function RecoveredOrderStatus(props: { operation: SponsoredOperation } | {
  accountAddress: Address
  transactionHash: Hex
  clientOrderId?: Hex | undefined
  liveOrderId?: bigint | undefined
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
  const liveOrderId = 'operation' in props ? undefined : props.liveOrderId
  const showLiveOutcome = liveOrderId !== undefined && clientOrderId !== undefined && (!order || order.status === 'Committed')
  return <div className="space-y-2 border border-positive/30 bg-positive/5 p-3 text-sm">
    <p className="font-semibold text-content-primary">Order submitted</p>
    {showLiveOutcome ? <LiveOrderOutcome orderId={liveOrderId} account={account} clientOrderId={clientOrderId} /> : order ? <>
      <p>Execution status: <strong>{order.status === 'Committed' ? 'Awaiting execution' : order.status}</strong></p>
      <p className="text-xs text-content-secondary">Order #{order.orderId.toString()} · {order.side} · {order.size}</p>
      {history.orderHistoryError && <p>The latest update is unavailable. Showing the last known status.</p>}
    </> : <p>{history.isOrderHistoryLoading ? 'Loading execution status…'
      : 'Execution status is not available yet. The order was submitted; this does not confirm that the trade executed.'}</p>}
    <Button type="button" variant="secondary" size="sm" onClick={() => { void history.refetch() }}>Refresh order status</Button>
  </div>
}

function LiveOrderOutcome({ orderId, account, clientOrderId }: { orderId: bigint; account: string; clientOrderId: string }) {
  const { readOrderLifecycleOutcome } = usePerpsTrading()
  const queryClient = useQueryClient()
  const queryKey = ['perps', 'live-order-outcome', account, clientOrderId, orderId.toString()] as const
  const { data } = useQuery({
    queryKey,
    queryFn: async () => {
      const previous = queryClient.getQueryData<'Executed' | 'Failed' | null>(queryKey) ?? null
      const outcome = await readOrderLifecycleOutcome(orderId)
      if (outcome?.account.toLowerCase() !== account || outcome.clientOrderId.toLowerCase() !== clientOrderId) return previous
      return outcome.status === PERPS_LIFECYCLE_STATUS.EXECUTED ? 'Executed' : 'Failed'
    },
    refetchInterval: query => (query.state.data ? false : 2_000),
    retry: false,
  })
  return <p>Order #{orderId.toString()} · <strong>{data ?? 'Awaiting execution'}</strong></p>
}
