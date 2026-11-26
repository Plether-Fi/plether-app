/** Preserve the OrderRouter's minimum time to accept an offchain order. */
export function hasKernelOrderSubmissionTime(submitBy: string | undefined, now: number): boolean {
  return submitBy === undefined || BigInt(Math.floor(now / 1_000)) + 30n < BigInt(submitBy)
}
