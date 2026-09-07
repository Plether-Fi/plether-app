const CARRY_INDEX_SCALE = 10n ** 18n
const BPS = 10_000n
const SECONDS_PER_YEAR = 365n * 24n * 60n * 60n

/** Mirrors PositionRiskAccountingLib's integer rounding using one onchain snapshot. */
export function calculatePendingCarryUsdc({
  unsettledCarryUsdc,
  borrowBaseUsdc,
  lastCarryIndex,
  sideCarryIndex,
  sideCarryTimestamp,
  sideBorrowBaseUsdc,
  poolAssetsUsdc,
  baseCarryBps,
  blockTimestamp,
}: {
  unsettledCarryUsdc?: bigint
  borrowBaseUsdc?: bigint
  lastCarryIndex?: bigint
  sideCarryIndex?: bigint
  sideCarryTimestamp?: bigint
  sideBorrowBaseUsdc?: bigint
  poolAssetsUsdc?: bigint
  baseCarryBps?: bigint
  blockTimestamp?: bigint
}): bigint | undefined {
  if (
    unsettledCarryUsdc === undefined || borrowBaseUsdc === undefined ||
    lastCarryIndex === undefined || sideCarryIndex === undefined ||
    sideCarryTimestamp === undefined || sideBorrowBaseUsdc === undefined ||
    poolAssetsUsdc === undefined || baseCarryBps === undefined || blockTimestamp === undefined
  ) return undefined

  const rawUtilizationBps = sideBorrowBaseUsdc === 0n
    ? 0n
    : poolAssetsUsdc === 0n ? BPS : sideBorrowBaseUsdc * BPS / poolAssetsUsdc
  const utilizationBps = rawUtilizationBps > BPS ? BPS : rawUtilizationBps
  const elapsed = blockTimestamp > sideCarryTimestamp ? blockTimestamp - sideCarryTimestamp : 0n
  const currentIndex = sideCarryIndex +
    baseCarryBps * utilizationBps * CARRY_INDEX_SCALE * elapsed / (SECONDS_PER_YEAR * BPS * BPS)
  const indexDelta = currentIndex > lastCarryIndex ? currentIndex - lastCarryIndex : 0n

  return unsettledCarryUsdc + borrowBaseUsdc * indexDelta / CARRY_INDEX_SCALE
}
