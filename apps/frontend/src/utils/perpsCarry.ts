const CARRY_INDEX_SCALE = 10n ** 18n

/** Mirrors PositionRiskAccountingLib's flat-carry integer rounding using one onchain snapshot. */
export function calculatePendingCarryAssets({
  unsettledCarryAssets,
  maxProfitAssets,
  lastCarryIndex,
  currentCarryIndex,
}: {
  unsettledCarryAssets?: bigint
  maxProfitAssets?: bigint
  lastCarryIndex?: bigint
  currentCarryIndex?: bigint
}): bigint | undefined {
  if (
    unsettledCarryAssets === undefined || maxProfitAssets === undefined ||
    lastCarryIndex === undefined || currentCarryIndex === undefined
  ) return undefined

  const indexDelta = currentCarryIndex > lastCarryIndex ? currentCarryIndex - lastCarryIndex : 0n
  return unsettledCarryAssets + maxProfitAssets * indexDelta / CARRY_INDEX_SCALE
}
