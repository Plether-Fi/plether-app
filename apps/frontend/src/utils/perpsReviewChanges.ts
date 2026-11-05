import { PERPS_SETTLEMENT_TOKEN_LABEL } from '../contracts/perpsAddresses'
import { formatUnits } from 'viem'
import type { PreparedPerpsOrderV3 } from '../contracts/perpsOrderV3'
import { PERPS_EXECUTION_MODE_LABELS } from '../contracts/perpsOrderV3'

export interface PerpsReviewChange { label: string; before: string; after: string }
/** Show full precision here so a one-unit change cannot appear unchanged. */
export function perpsReviewChanges(before?: PreparedPerpsOrderV3, after?: PreparedPerpsOrderV3): PerpsReviewChange[] {
  if (!before || !after) return []
  const changes: PerpsReviewChange[] = []
  const add = (label: string, a: bigint | number | string | undefined, b: typeof a, format = (v: NonNullable<typeof a>) => String(v)) => {
    if (a !== b) changes.push({ label, before: a === undefined ? 'Unavailable' : format(a), after: b === undefined ? 'Unavailable' : format(b) })
  }
  const assets = (value: bigint | string | number) => `${formatUnits(BigInt(value), 6)} ${PERPS_SETTLEMENT_TOKEN_LABEL}`
  const price = (value: bigint | string | number) => BigInt(value) === 0n ? 'No limit' : `${formatUnits(200_000_000n - BigInt(value), 8)} ${PERPS_SETTLEMENT_TOKEN_LABEL}`
  const leverage = (value: bigint | string | number) => `${formatUnits(BigInt(value), 4)}×`
  add('Order quantity', before.request.sizeDelta, after.request.sizeDelta, value => `${formatUnits(BigInt(value), 18)} plDXY`)
  add('Execution limit', before.request.targetPrice, after.request.targetPrice, price)
  add('Required margin', before.request.marginDelta, after.request.marginDelta, assets)
  add('Required funding', before.reviewSummary?.requiredFundingAssets, after.reviewSummary?.requiredFundingAssets, assets)
  add('Execution reward', before.executionBountyAssets, after.executionBountyAssets, assets)
  add('Maximum execution reward', before.request.bounds.maxExecutionBountyAssets, after.request.bounds.maxExecutionBountyAssets, assets)
  add('Execution fee', before.reviewSummary?.currentAssessment.executionFeeAssets, after.reviewSummary?.currentAssessment.executionFeeAssets, assets)
  add('VPI', before.reviewSummary?.currentAssessment.vpiAssets, after.reviewSummary?.currentAssessment.vpiAssets, assets)
  add('Frozen spread', before.reviewSummary?.currentAssessment.frozenSpreadAssets, after.reviewSummary?.currentAssessment.frozenSpreadAssets, assets)
  add(`Sponsored ${PERPS_SETTLEMENT_TOKEN_LABEL}`, before.sponsoredClose?.amountAssets ?? 0n, after.sponsoredClose?.amountAssets ?? 0n, assets)
  add('Deposit carry', before.sponsoredClose?.depositCarryAssets ?? 0n, after.sponsoredClose?.depositCarryAssets ?? 0n, assets)
  add('Carry', before.reviewSummary?.currentAssessment.carryAssets, after.reviewSummary?.currentAssessment.carryAssets, assets)
  add('Commitment carry', before.reviewSummary?.commitmentCarryAssets, after.reviewSummary?.commitmentCarryAssets, assets)
  add('Explicit fees', before.reviewSummary?.currentAssessment.explicitFeesAssets, after.reviewSummary?.currentAssessment.explicitFeesAssets, assets)
  add('Highest reviewed leverage', before.reviewSummary?.worstPostLeverageBps, after.reviewSummary?.worstPostLeverageBps, leverage)
  add('Execution conditions', before.protection.executionMode, after.protection.executionMode,
    value => PERPS_EXECUTION_MODE_LABELS[Number(value)])
  if (before.request.bounds.expectedConfigHash !== after.request.bounds.expectedConfigHash) {
    changes.push({ label: 'Execution configuration', before: 'Previously reviewed rules', after: 'Updated execution rules' })
  }
  add('Take profit', before.positionProtection?.params.takeProfitTriggerPrice, after.positionProtection?.params.takeProfitTriggerPrice, price)
  add('Stop loss', before.positionProtection?.params.stopLossTriggerPrice, after.positionProtection?.params.stopLossTriggerPrice, price)
  add('Protection trigger reward', before.positionProtection?.triggerBountyAssets, after.positionProtection?.triggerBountyAssets, assets)
  add('Protection execution reward', before.positionProtection?.executionBountyAssets, after.positionProtection?.executionBountyAssets, assets)
  return changes
}
