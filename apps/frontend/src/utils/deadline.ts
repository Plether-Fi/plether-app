const SECONDS_PER_MINUTE = 60

export function getDeadline(minutes = 30): bigint {
  return BigInt(Math.floor(Date.now() / 1000) + minutes * SECONDS_PER_MINUTE)
}
