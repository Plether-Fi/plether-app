import type { PerpsOrderHistoryRow } from '../hooks/usePerpsHistory'
import { formatDisplayDxyPrice, formatPerpsUsdc, oraclePriceToDisplayDxyPrice, type PerpsDirection } from './perps'

const FAILURE_GUIDANCE: Partial<Record<string, [string, string]>> = {
  Expired: ['The order expired before execution.', 'Request a fresh preview before submitting a new order.'],
  Slippage: [
    'The execution price was outside your committed price limit. Your price protection prevented the trade from executing.',
    'Review a fresh preview and your price limit before submitting a new order. Market movement and the oracle confidence adjustment can affect the execution price.',
  ],
  ConfigMismatch: ['Protocol settings changed after you reviewed the order.', 'Request a fresh preview to review the updated settings before submitting again.'],
  ExecutionModeDisallowed: ['The market entered a state that no longer allowed this order to execute.', 'Check the current market restrictions before requesting a new preview.'],
  RiskOff: ['The protocol blocked this order to limit risk.', 'Check the market status and available trading actions before submitting another order.'],
  PlannerRejected: ['The order could not pass the execution checks for the account or market.', 'Refresh your account and review the order size, margin and market capacity in a fresh preview.'],
  ConstraintViolation: ['The execution result did not satisfy a financial limit approved with your order.', 'Review any failed check shown below, then request a fresh preview of the size, margin and fees before submitting again.'],
  AccountLiquidated: ['The account was liquidated before this order executed.', 'Review your account balance and liquidation history before creating a new order.'],
}

const REASON_ALIASES: Partial<Record<string, string>> = {
  'Config mismatch': 'ConfigMismatch',
  'Mode disallowed': 'ExecutionModeDisallowed',
  'Risk off': 'RiskOff',
  'Planner rejected': 'PlannerRejected',
  'Constraint violation': 'ConstraintViolation',
  'Account liquidated': 'AccountLiquidated',
}

export function getPerpsOrderFailureContext(
  order: PerpsOrderHistoryRow,
  { direction, isClose, limit }: { direction?: PerpsDirection; isClose?: boolean; limit?: number | null },
) {
  const reason = REASON_ALIASES[order.terminalReason ?? ''] ?? order.terminalReason
  const [defaultExplanation, nextStep] = FAILURE_GUIDANCE[reason ?? ''] ?? [
    'The order reached a final failed state. The available receipt does not explain the cause.',
    'Review Order History and the finalization transaction before creating a new order.',
  ]
  const liquidated = reason === 'AccountLiquidated'
  const outcome = liquidated
    ? 'This order did not execute. Liquidation changed the account separately; review its remaining collateral and positions.'
    : `This order did not ${isClose === true ? 'reduce or close your position' : 'change your position'}. Any opening margin reserved for this order was released to your trading balance.${isClose === true ? ' A failed close does not remove your existing exposure.' : ''}`
  const maximum = direction === undefined || isClose === undefined
    ? undefined
    : (direction === 'long') !== isClose
  const limitLabel = maximum === undefined ? 'Committed price limit' : maximum ? 'Maximum acceptable price' : 'Minimum acceptable price'
  const hasLimit = limit !== undefined && limit !== null && Number.isFinite(limit) && limit > 0
  const limitDisplay = limit === null ? 'No price limit' : hasLimit ? limit.toFixed(8) : 'Not available'
  const hasExecutionPrice = order.executionPriceRaw !== undefined && order.executionPriceRaw > 0n
  const attemptedPrice = hasExecutionPrice ? formatDisplayDxyPrice(order.executionPriceRaw, 8) : 'Not available'
  let explanation = defaultExplanation
  if (reason === 'Slippage' && hasExecutionPrice) {
    if (hasLimit) {
      const executionPrice = Number(oraclePriceToDisplayDxyPrice(order.executionPriceRaw))
      const limitPrice = Math.round(limit * 100_000_000)
      const difference = maximum === true ? executionPrice - limitPrice : limitPrice - executionPrice
      explanation = maximum !== undefined && difference > 0
        ? `The execution price of ${attemptedPrice} was ${maximum ? 'above' : 'below'} your ${maximum ? 'maximum' : 'minimum'} acceptable price of ${limitDisplay} by ${(difference / 100_000_000).toFixed(8)}. Your price protection prevented the trade from executing.`
        : `Price protection rejected this order. The recorded execution price was ${attemptedPrice}; your ${limitLabel.toLowerCase()} was ${limitDisplay}. Check the finalization transaction for details.`
    } else {
      explanation = `Price protection rejected the execution price of ${attemptedPrice}. The committed price limit is unavailable in this view. Your price protection prevented the trade from executing.`
    }
  } else if (reason === 'Slippage' && hasLimit) {
    explanation = `Your price protection prevented the trade from executing. Your ${limitLabel.toLowerCase()} was ${limitDisplay}; the attempted execution price is not available yet.`
  }
  const bounty = order.receiptEconomics?.executionBountyUsdc
  const fee = order.receiptEconomics?.executionFeeUsdc
  const formatCharge = (value: string | undefined) => value !== undefined && /^\d+$/.test(value)
    ? `${formatPerpsUsdc(BigInt(value), 6)} USDC`
    : 'Not yet available'

  return {
    explanation,
    outcome,
    nextStep: `This order is final and cannot be retried or finalized manually. ${nextStep}`,
    limitLabel,
    limit: limitDisplay,
    priceLabel: reason === 'Slippage' ? 'Attempted execution price' : 'Recorded price',
    attemptedPrice,
    reward: formatCharge(bounty),
    executionFee: formatCharge(fee),
    rewardExplanation: liquidated
      ? 'Liquidation forfeits reserved execution rewards to the protocol treasury.'
      : 'The execution reward pays for processing the order even when the trade fails.',
  }
}
