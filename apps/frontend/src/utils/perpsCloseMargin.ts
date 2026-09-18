import { decodeEventLog, parseAbi, parseEventLogs, toEventSelector, type TransactionReceipt } from 'viem'
import { PERPS_ORDER_LIFECYCLE_BOOK_ABI } from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA } from '../contracts/perpsAddresses'
import type { PerpsOrderHistoryRow } from '../hooks/usePerpsHistory'

const marginEvents = parseAbi([
  'event MarginLocked(address indexed account, uint8 indexed bucket, uint256 amountUsdc)',
  'event MarginUnlocked(address indexed account, uint8 indexed bucket, uint256 amountUsdc)',
])

/** Net reduction of assigned position collateral in this exact execution interval. */
export function deriveCloseMarginReleased(
  transaction: Pick<TransactionReceipt, 'logs' | 'status' | 'transactionHash' | 'blockHash'>,
  order: PerpsOrderHistoryRow,
): bigint | undefined {
  if (order.type !== 'Close' || order.status !== 'Executed' || transaction.status !== 'success'
    || transaction.transactionHash.toLowerCase() !== order.revealTxHash?.toLowerCase()
    || (order.terminalBlockHash !== undefined && transaction.blockHash.toLowerCase() !== order.terminalBlockHash.toLowerCase())) return undefined

  const finals = parseEventLogs({
    abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI,
    eventName: 'OrderFinalized',
    logs: transaction.logs.filter(log => log.address.toLowerCase() === PERPS_ARBITRUM_SEPOLIA.orderLifecycleBook.toLowerCase()),
  }).sort((left, right) => left.logIndex - right.logIndex)
  const index = finals.findIndex(log => log.args.orderId === order.orderId)
  if (index < 0) return undefined
  const final = finals.at(index)
  if (!final || final.args.account.toLowerCase() !== order.account.toLowerCase()
    || final.args.receipt.status !== 2
    || (order.receiptHash !== undefined && final.args.receiptHash.toLowerCase() !== order.receiptHash.toLowerCase())) return undefined
  const previousIndex = finals[index - 1]?.logIndex ?? -1
  let released = 0n
  for (const log of transaction.logs) {
    if (log.address.toLowerCase() !== PERPS_ARBITRUM_SEPOLIA.marginClearinghouse.toLowerCase()
      || log.logIndex <= previousIndex || log.logIndex >= final.logIndex) continue
    // Other clearinghouse events do not change position margin.
    if (!marginEvents.some(event => toEventSelector(event) === log.topics[0])) continue
    try {
      const decoded = decodeEventLog({ abi: marginEvents, data: log.data, topics: log.topics })
      if (decoded.args.account.toLowerCase() !== order.account.toLowerCase() || decoded.args.bucket !== 0) continue
      released += decoded.eventName === 'MarginUnlocked' ? decoded.args.amountUsdc : -decoded.args.amountUsdc
    } catch {
      return undefined
    }
  }
  return released >= 0n ? released : undefined
}
