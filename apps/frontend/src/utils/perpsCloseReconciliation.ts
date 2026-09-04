import type { PerpsOrderReceiptEconomics } from '../hooks/usePerpsHistory'

export interface PerpsCloseReconciliation {
  executionNotionalUsdc: bigint
  executionBountyUsdc: bigint
  realizedPnlUsdc: bigint
  vpiUsdc: bigint
  carryUsdc: bigint
  executionFeeUsdc: bigint
  frozenSpreadAssessedUsdc: bigint
  frozenSpreadPaidUsdc: bigint
  frozenSpreadWaivedUsdc: bigint
  netCloseResultUsdc: bigint
  marginAccountChangeUsdc: bigint
  traderClaimChangeUsdc: bigint
  uncoveredLossUsdc: bigint
  postPositionSize: bigint
  postPositionMarginUsdc: bigint
  releasedPositionMarginUsdc?: bigint
}

export interface PerpsCloseReconciliationOptions {
  preExecutionPositionMarginUsdc?: bigint
}

function parseReceiptInteger(value: string | undefined): bigint | undefined {
  if (value === undefined || value === '' || !/^-?\d+$/.test(value)) return undefined
  try {
    return BigInt(value)
  } catch {
    return undefined
  }
}

function clamp(value: bigint, minimum: bigint, maximum: bigint): bigint {
  if (value < minimum) return minimum
  if (value > maximum) return maximum
  return value
}

/**
 * Builds an exact executed-close reconciliation from receipt evidence.
 * Returns undefined for legacy, incomplete, malformed, or inconsistent receipts.
 */
export function derivePerpsCloseReconciliation(
  receipt: PerpsOrderReceiptEconomics | undefined,
  options: PerpsCloseReconciliationOptions = {}
): PerpsCloseReconciliation | undefined {
  if (receipt === undefined) return undefined

  const executionNotionalUsdc = parseReceiptInteger(receipt.executionNotionalUsdc)
  const executionBountyUsdc = parseReceiptInteger(receipt.executionBountyUsdc)
  const realizedPnlUsdc = parseReceiptInteger(receipt.realizedPnlUsdc)
  const vpiUsdc = parseReceiptInteger(receipt.vpiUsdc)
  const carryUsdc = parseReceiptInteger(receipt.carryUsdc)
  const executionFeeUsdc = parseReceiptInteger(receipt.executionFeeUsdc)
  const frozenSpreadAssessedUsdc = parseReceiptInteger(receipt.frozenSpreadUsdc)
  const actionChargeAssessedUsdc = parseReceiptInteger(receipt.actionChargeAssessedUsdc)
  const actionChargeCollectedUsdc = parseReceiptInteger(receipt.actionChargeCollectedUsdc)
  const grossAccountDebitUsdc = parseReceiptInteger(receipt.grossAccountDebitUsdc)
  const preSettlementBalanceUsdc = parseReceiptInteger(receipt.preSettlementBalanceUsdc)
  const postSettlementBalanceUsdc = parseReceiptInteger(receipt.postSettlementBalanceUsdc)
  const preTraderClaimBalanceUsdc = parseReceiptInteger(receipt.preTraderClaimBalanceUsdc)
  const postTraderClaimBalanceUsdc = parseReceiptInteger(receipt.postTraderClaimBalanceUsdc)
  const postPositionSize = parseReceiptInteger(receipt.postPositionSize)
  const postPositionMarginUsdc = parseReceiptInteger(receipt.postPositionMarginUsdc)
  const postPositionEquityUsdc = parseReceiptInteger(receipt.postPositionEquityUsdc)
  const postLeverageBps = parseReceiptInteger(receipt.postLeverageBps)

  if (
    executionNotionalUsdc === undefined ||
    executionBountyUsdc === undefined ||
    realizedPnlUsdc === undefined ||
    vpiUsdc === undefined ||
    carryUsdc === undefined ||
    executionFeeUsdc === undefined ||
    frozenSpreadAssessedUsdc === undefined ||
    actionChargeAssessedUsdc === undefined ||
    actionChargeCollectedUsdc === undefined ||
    grossAccountDebitUsdc === undefined ||
    preSettlementBalanceUsdc === undefined ||
    postSettlementBalanceUsdc === undefined ||
    preTraderClaimBalanceUsdc === undefined ||
    postTraderClaimBalanceUsdc === undefined ||
    postPositionSize === undefined ||
    postPositionMarginUsdc === undefined ||
    postPositionEquityUsdc === undefined ||
    postLeverageBps === undefined
  ) {
    return undefined
  }

  const unsignedValues = [
    executionNotionalUsdc,
    executionBountyUsdc,
    executionFeeUsdc,
    frozenSpreadAssessedUsdc,
    actionChargeAssessedUsdc,
    actionChargeCollectedUsdc,
    grossAccountDebitUsdc,
    preSettlementBalanceUsdc,
    postSettlementBalanceUsdc,
    preTraderClaimBalanceUsdc,
    postTraderClaimBalanceUsdc,
    postPositionSize,
    postPositionMarginUsdc,
    postLeverageBps,
  ]
  if (unsignedValues.some((value) => value < 0n)) return undefined
  if (executionNotionalUsdc === 0n) return undefined
  if (executionBountyUsdc > grossAccountDebitUsdc) return undefined
  if (actionChargeAssessedUsdc < frozenSpreadAssessedUsdc) return undefined
  if (actionChargeCollectedUsdc > actionChargeAssessedUsdc) return undefined
  if (
    postPositionSize === 0n &&
    (postPositionMarginUsdc !== 0n || postPositionEquityUsdc !== 0n || postLeverageBps !== 0n)
  ) {
    return undefined
  }
  if (postPositionSize > 0n && postPositionEquityUsdc < 0n) return undefined

  const nonSpreadAssessedUsdc = actionChargeAssessedUsdc - frozenSpreadAssessedUsdc
  const frozenSpreadPaidUsdc = clamp(
    actionChargeCollectedUsdc - nonSpreadAssessedUsdc,
    0n,
    frozenSpreadAssessedUsdc
  )
  const frozenSpreadWaivedUsdc = frozenSpreadAssessedUsdc - frozenSpreadPaidUsdc
  const netCloseResultUsdc = realizedPnlUsdc
    - vpiUsdc
    - carryUsdc
    - executionFeeUsdc
    - frozenSpreadPaidUsdc
    - executionBountyUsdc
  const marginAccountChangeUsdc = postSettlementBalanceUsdc - preSettlementBalanceUsdc
  const traderClaimChangeUsdc = postTraderClaimBalanceUsdc - preTraderClaimBalanceUsdc
  const observedValueChangeUsdc = marginAccountChangeUsdc + traderClaimChangeUsdc
  const uncoveredLossUsdc = observedValueChangeUsdc - netCloseResultUsdc

  if (netCloseResultUsdc >= 0n) {
    if (
      observedValueChangeUsdc !== netCloseResultUsdc ||
      marginAccountChangeUsdc < 0n ||
      traderClaimChangeUsdc < 0n ||
      (marginAccountChangeUsdc > 0n && traderClaimChangeUsdc > 0n)
    ) {
      return undefined
    }
  } else {
    if (
      uncoveredLossUsdc < 0n ||
      marginAccountChangeUsdc > 0n ||
      traderClaimChangeUsdc > 0n ||
      (uncoveredLossUsdc > 0n && postPositionSize !== 0n)
    ) {
      return undefined
    }
  }

  const preExecutionPositionMarginUsdc = options.preExecutionPositionMarginUsdc
  const releasedPositionMarginUsdc =
    preExecutionPositionMarginUsdc !== undefined &&
    preExecutionPositionMarginUsdc >= 0n &&
    preExecutionPositionMarginUsdc >= postPositionMarginUsdc
      ? preExecutionPositionMarginUsdc - postPositionMarginUsdc
      : undefined

  return {
    executionNotionalUsdc,
    executionBountyUsdc,
    realizedPnlUsdc,
    vpiUsdc,
    carryUsdc,
    executionFeeUsdc,
    frozenSpreadAssessedUsdc,
    frozenSpreadPaidUsdc,
    frozenSpreadWaivedUsdc,
    netCloseResultUsdc,
    marginAccountChangeUsdc,
    traderClaimChangeUsdc,
    uncoveredLossUsdc,
    postPositionSize,
    postPositionMarginUsdc,
    releasedPositionMarginUsdc,
  }
}
