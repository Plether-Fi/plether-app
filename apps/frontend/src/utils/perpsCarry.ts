const CARRY_INDEX_SCALE = 10n ** 18n
const BPS = 10_000n
const SECONDS_PER_YEAR = 365n * 24n * 60n * 60n

/** Mirrors PositionRiskAccountingLib's integer rounding using one onchain snapshot. */
export function calculatePendingCarryAssets({
  unsettledCarryAssets,
  borrowBaseAssets,
  lastCarryIndex,
  sideCarryIndex,
  sideCarryTimestamp,
  sideBorrowBaseAssets,
  poolAssets,
  baseCarryBps,
  blockTimestamp,
}: {
  unsettledCarryAssets?: bigint
  borrowBaseAssets?: bigint
  lastCarryIndex?: bigint
  sideCarryIndex?: bigint
  sideCarryTimestamp?: bigint
  sideBorrowBaseAssets?: bigint
  poolAssets?: bigint
  baseCarryBps?: bigint
  blockTimestamp?: bigint
}): bigint | undefined {
  if (
    unsettledCarryAssets === undefined || borrowBaseAssets === undefined ||
    lastCarryIndex === undefined || sideCarryIndex === undefined ||
    sideCarryTimestamp === undefined || sideBorrowBaseAssets === undefined ||
    poolAssets === undefined || baseCarryBps === undefined || blockTimestamp === undefined
  ) return undefined

  const rawUtilizationBps = sideBorrowBaseAssets === 0n
    ? 0n
    : poolAssets === 0n ? BPS : sideBorrowBaseAssets * BPS / poolAssets
  const utilizationBps = rawUtilizationBps > BPS ? BPS : rawUtilizationBps
  const elapsed = blockTimestamp > sideCarryTimestamp ? blockTimestamp - sideCarryTimestamp : 0n
  const currentIndex = sideCarryIndex +
    baseCarryBps * utilizationBps * CARRY_INDEX_SCALE * elapsed / (SECONDS_PER_YEAR * BPS * BPS)
  const indexDelta = currentIndex > lastCarryIndex ? currentIndex - lastCarryIndex : 0n

  return unsettledCarryAssets + borrowBaseAssets * indexDelta / CARRY_INDEX_SCALE
}
