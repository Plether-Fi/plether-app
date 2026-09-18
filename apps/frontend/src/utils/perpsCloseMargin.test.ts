import { describe, expect, it } from 'vitest'
import { parseEventLogs, toHex, type RpcTransactionReceipt } from 'viem'
import { formatTransactionReceipt } from 'viem'
import waiver from '../../../../scripts/fixtures/insights-close-waiver.json'
import rebate from '../../../../scripts/fixtures/insights-close-rebate.json'
import { PERPS_ORDER_LIFECYCLE_BOOK_ABI } from '../contracts/abis'
import type { PerpsOrderHistoryRow } from '../hooks/usePerpsHistory'
import { deriveCloseMarginReleased } from './perpsCloseMargin'

function fixture(raw = waiver) {
  const transaction = formatTransactionReceipt(raw.receipt as RpcTransactionReceipt)
  const final = parseEventLogs({ abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI, logs: transaction.logs, eventName: 'OrderFinalized' })[0]
  const order: PerpsOrderHistoryRow = {
    orderId: final.args.orderId, account: final.args.account, clientOrderId: final.args.clientOrderId,
    receiptHash: final.args.receiptHash, revealTxHash: transaction.transactionHash, terminalBlockHash: transaction.blockHash,
    type: 'Close', status: 'Executed', time: '', market: '', side: '', price: '', size: '',
  }
  return { transaction, order, final }
}

describe('deriveCloseMarginReleased', () => {
  it.each([[waiver, 98_742_864_242n], [rebate, 77_473_770_650n]] as const)(
    'reads actual position collateral reductions from the receipt', (raw, expected) => {
      const { transaction, order } = fixture(raw)
      expect(deriveCloseMarginReleased(transaction, order)).toBe(expected)
    })

  it('does not include margin from an earlier execution in the same batch', () => {
    const { transaction, order, final } = fixture()
    const priorFinal = {
      ...transaction.logs.find(log => log.logIndex === final.logIndex)!, logIndex: 1,
      topics: [final.topics[0], toHex(order.orderId + 1n, { size: 32 }), ...final.topics.slice(2)] as typeof final.topics,
    }
    const earlierMargin = { ...transaction.logs[0], logIndex: 0 }
    expect(deriveCloseMarginReleased({ ...transaction, logs: [earlierMargin, priorFinal, ...transaction.logs] }, order))
      .toBe(98_742_864_242n)
  })

  it('rejects a receipt from the wrong transaction, account, block, or order', () => {
    const { transaction, order } = fixture()
    expect(deriveCloseMarginReleased(transaction, { ...order, revealTxHash: toHex(0, { size: 32 }) })).toBeUndefined()
    expect(deriveCloseMarginReleased(transaction, { ...order, account: toHex(0, { size: 20 }) })).toBeUndefined()
    expect(deriveCloseMarginReleased(transaction, { ...order, terminalBlockHash: toHex(0, { size: 32 }) })).toBeUndefined()
    expect(deriveCloseMarginReleased(transaction, { ...order, orderId: 999999n })).toBeUndefined()
    expect(deriveCloseMarginReleased({ ...transaction, status: 'reverted' }, order)).toBeUndefined()
  })

  it('does not turn malformed collateral evidence into zero', () => {
    const { transaction, order } = fixture()
    const logs = transaction.logs.map((log, index) => index === 0 ? { ...log, data: '0x' as const } : log)
    expect(deriveCloseMarginReleased({ ...transaction, logs }, order)).toBeUndefined()
  })
})
