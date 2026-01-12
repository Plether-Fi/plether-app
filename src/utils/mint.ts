export function getMinBalance(bearBalance: bigint, bullBalance: bigint): bigint {
  return bearBalance < bullBalance ? bearBalance : bullBalance
}
