import { formatUnits } from 'viem'
import type { PreparedPerpsOrderV2 } from '../contracts/perpsOrderV2'
import { PERPS_EXECUTION_MODE_LABELS } from '../contracts/perpsOrderV2'

export interface PerpsReviewChange { label: string; before: string; after: string }
/** Show full precision here so a one-unit change cannot appear unchanged. */
export function perpsReviewChanges(before?: PreparedPerpsOrderV2, after?: PreparedPerpsOrderV2): PerpsReviewChange[] {
  if (!before || !after) return []
  const changes: PerpsReviewChange[] = []
  const add = (label: string, a: bigint | number | string | undefined, b: typeof a, format = (v: NonNullable<typeof a>) => String(v)) => {
    if (a !== b) changes.push({ label, before: a === undefined ? 'Unavailable' : format(a), after: b === undefined ? 'Unavailable' : format(b) })
  }
  const usdc = (value: bigint | string | number) => `${formatUnits(BigInt(value), 6)} USDC`
  const price = (value: bigint | string | number) => BigInt(value) === 0n ? 'No limit' : `${formatUnits(200_000_000n - BigInt(value), 8)} USDC`
  const leverage = (value: bigint | string | number) => `${formatUnits(BigInt(value), 4)}×`
  add('Order quantity', before.request.sizeDelta, after.request.sizeDelta, value => `${formatUnits(BigInt(value), 18)} plDXY`)
  add('Execution limit', before.request.targetPrice, after.request.targetPrice, price)
  add('Required margin', before.request.marginDelta, after.request.marginDelta, usdc)
  add('Required funding', before.reviewSummary?.requiredFundingUsdc, after.reviewSummary?.requiredFundingUsdc, usdc)
  add('Execution reward', before.executionBountyUsdc, after.executionBountyUsdc, usdc)
  add('Execution fee', before.reviewSummary?.currentAssessment.executionFeeUsdc, after.reviewSummary?.currentAssessment.executionFeeUsdc, usdc)
  add('VPI', before.reviewSummary?.currentAssessment.vpiUsdc, after.reviewSummary?.currentAssessment.vpiUsdc, usdc)
  add('Frozen spread', before.reviewSummary?.currentAssessment.frozenSpreadUsdc, after.reviewSummary?.currentAssessment.frozenSpreadUsdc, usdc)
  add('Carry', before.reviewSummary?.currentAssessment.carryUsdc, after.reviewSummary?.currentAssessment.carryUsdc, usdc)
  add('Explicit fees', before.reviewSummary?.currentAssessment.explicitFeesUsdc, after.reviewSummary?.currentAssessment.explicitFeesUsdc, usdc)
  add('Highest reviewed leverage', before.reviewSummary?.worstPostLeverageBps, after.reviewSummary?.worstPostLeverageBps, leverage)
  add('Execution conditions', before.protection.executionMode, after.protection.executionMode,
    value => PERPS_EXECUTION_MODE_LABELS[Number(value)])
  if (before.request.bounds.expectedConfigHash !== after.request.bounds.expectedConfigHash) {
    changes.push({ label: 'Execution configuration', before: 'Previously reviewed rules', after: 'Updated execution rules' })
  }
  add('Take profit', before.positionProtection?.params.takeProfitTriggerPrice, after.positionProtection?.params.takeProfitTriggerPrice, price)
  add('Stop loss', before.positionProtection?.params.stopLossTriggerPrice, after.positionProtection?.params.stopLossTriggerPrice, price)
  add('Protection trigger reward', before.positionProtection?.triggerBountyUsdc, after.positionProtection?.triggerBountyUsdc, usdc)
  add('Protection execution reward', before.positionProtection?.executionBountyUsdc, after.positionProtection?.executionBountyUsdc, usdc)
  return changes
}
