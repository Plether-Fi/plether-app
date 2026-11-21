import assert from 'node:assert/strict'

// Nitro's L1 data component can dominate preVerificationGas on Arbitrum Sepolia.
export const gasCap = {
  callGasLimit: 3_000_000n,
  verificationGasLimit: 1_000_000n,
  preVerificationGas: 12_000_000n,
}

export function boundedGasEstimate(estimate) {
  return Object.fromEntries(Object.entries(gasCap).map(([field, cap]) => {
    const gas = BigInt(estimate[field])
    assert.ok(gas > 0n && gas <= cap, `Alto ${field} exceeds reviewed cap`)
    return [field, gas]
  }))
}
