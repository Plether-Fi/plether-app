/** Keep router rewards outside the margin budget supplied to the engine lens. */
export function perpsMaxOpenMarginBudget(
  availableUsdc: bigint,
  maximumOpenBountyUsdc: bigint,
  protectionRewardsUsdc = 0n
): bigint {
  const rewards = maximumOpenBountyUsdc + protectionRewardsUsdc
  return availableUsdc > rewards ? availableUsdc - rewards : 0n
}
