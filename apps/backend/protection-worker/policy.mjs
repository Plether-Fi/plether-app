export const STATUS = ['None', 'PendingOpen', 'Armed', 'Triggered', 'Executed', 'Failed', 'Cancelled', 'Liquidated', 'Latched']
export const LEG = ['None', 'TakeProfit', 'StopLoss']
export function json(value) { return JSON.stringify(value, (_, item) => typeof item === 'bigint' ? item.toString() : item) }
export function protectionSnapshot(value) { return { ...value, statusName: STATUS[value.status], triggeredLegName: LEG[value.triggeredLeg] } }

export function triggerLeg(protection, mark, publication, blockNumber) {
  if (protection.status !== 2 || mark <= 0n || publication <= protection.armedAt || blockNumber <= protection.armedBlock) return 0
  const tp = protection.takeProfitTriggerPrice
  const sl = protection.stopLossTriggerPrice
  if (tp > 0n && (protection.side === 0 ? mark <= tp : mark >= tp)) return 1
  if (sl > 0n && (protection.side === 0 ? mark >= sl : mark <= sl)) return 2
  return 0
}

export function retryDecision({ protection, outcome, pendingCount, oracleAvailable, queueSize, maxOrderAge, keeperBatchSize, keeperPollSeconds }) {
  if (protection.status !== 8) return 'inactive'
  if (outcome.reason !== 2 || outcome.status !== 3) return 'operator-required'
  if (pendingCount !== 0n) return 'pending-orders'
  if (!oracleAvailable) return 'oracle-unavailable'
  const arrivalSeconds = (queueSize + BigInt(keeperBatchSize) - 1n) / BigInt(keeperBatchSize) * BigInt(keeperPollSeconds)
  return arrivalSeconds <= maxOrderAge - 15n ? 'retry' : 'queue-congested'
}

export function admittedPayload(row, now, maxAge) {
  return row?.source === 'backend_hermes_latest_v2' &&
    Number(row.min_publish_time) > 0 && Number(row.max_publish_time) >= Number(row.min_publish_time) &&
    Number(row.max_publish_time) <= now && now - Number(row.min_publish_time) <= maxAge &&
    Array.isArray(row.update_data) && row.update_data.length > 0 &&
    row.update_data.every(value => typeof value === 'string' && /^0x(?:[0-9a-fA-F]{2})+$/.test(value))
}
