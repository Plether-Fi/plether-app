import { keccak256, stringToHex } from 'viem'

export const PERPS_CONFIG_SCHEMA_HASH = keccak256(stringToHex('PletherExecutionConfigV3'))
export const PERPS_RECEIPT_TYPEHASH = keccak256(stringToHex(
  'PletherOrderReceiptV3(uint256 chainId,address book,address router,uint64 terminalBlock,uint64 terminalTime,OrderReceipt receipt)'
))

export const POSITION_PROTECTION_STATUS = {
  None: 0, PendingOpen: 1, Armed: 2, Triggered: 3, Executed: 4,
  Failed: 5, Cancelled: 6, Liquidated: 7, Latched: 8,
} as const

export const POSITION_PROTECTION_STATUS_LABELS: Record<number, string> = {
  0: 'No protection', 1: 'Pending open', 2: 'Armed', 3: 'Close queued',
  4: 'Executed', 5: 'Failed', 6: 'Cancelled', 7: 'Liquidated', 8: 'Waiting to retry',
}

export const BOUNTY_DISPOSITION = {
  None: 0, Paid: 1, Forfeited: 2, RefundedToAccount: 3, RetainedForProtectionRetry: 4,
} as const

export function positionProtectionMessage(id: bigint, status: number): string {
  if (status === POSITION_PROTECTION_STATUS.Latched) {
    return `Position protection #${id.toString()} is triggered and waiting for another close attempt. Its execution reward is retained; no additional reward is charged. The protection cannot be cancelled and discretionary orders remain locked.`
  }
  if (status === POSITION_PROTECTION_STATUS.Triggered) {
    return `Position protection #${id.toString()} is triggered. A market close is queued. Failed attempts can be retried until the protected position closes or no longer matches. Discretionary orders remain locked.`
  }
  if (status === POSITION_PROTECTION_STATUS.PendingOpen) {
    return `Position protection #${id.toString()} is attached to an opening order. It is waiting for the position to open and is not yet armed.`
  }
  if (status === POSITION_PROTECTION_STATUS.Armed) {
    return `Position protection #${id.toString()} is active. Cancel it before placing a discretionary order.`
  }
  if (status === POSITION_PROTECTION_STATUS.Executed) {
    return `Position protection #${id.toString()} completed its close execution.`
  }
  if (status === POSITION_PROTECTION_STATUS.Failed) {
    return `Position protection #${id.toString()} ended in failure. This protection is no longer active and cannot be retried.`
  }
  if (status === POSITION_PROTECTION_STATUS.Cancelled) {
    return `Position protection #${id.toString()} was cancelled. It no longer protects the position.`
  }
  if (status === POSITION_PROTECTION_STATUS.Liquidated) {
    return `Position protection #${id.toString()} ended because the protected position was liquidated.`
  }
  if (status === POSITION_PROTECTION_STATUS.None) return 'No SL/TP protection is attached.'
  return `Position protection #${id.toString()} is active. Discretionary orders remain locked until it resolves.`
}
