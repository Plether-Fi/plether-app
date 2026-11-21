import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { test } from 'node:test'
import { aggregateGasCap, boundedGasEstimate, gasCap, maxFeeCap, maxPrefundCap,
  maxPriorityFeeCap, sponsorVerificationGasCap } from './nitro-sepolia-gas.mjs'

const { gas: limits } = JSON.parse(readFileSync(
  new URL('../../apps/backend/test/fixtures/kernel-deployment.json', import.meta.url), 'utf8',
))

test('recorded Nitro preVerificationGas fits the smoke cap', () => {
  const evidence = JSON.parse(readFileSync(new URL('../../docs/runbooks/evidence/kernel-nitro-safemode-2026-11-20.json', import.meta.url)))
  const recorded = evidence.operations.find(operation => operation.estimatedGas?.preVerification === String(limits.nitroFixturePreVerificationGas))
  assert.ok(recorded, 'Recorded Nitro SafeMode estimate is missing')
  const gas = boundedGasEstimate({
    callGasLimit: recorded.estimatedGas.call,
    verificationGasLimit: recorded.estimatedGas.verification,
    preVerificationGas: recorded.estimatedGas.preVerification,
  })
  assert.equal(gas.preVerificationGas, BigInt(limits.nitroFixturePreVerificationGas))
})

test('Nitro smoke and live gas tooling use the shared economic ceilings', () => {
  for (const field of Object.keys(gasCap)) assert.equal(gasCap[field], BigInt(limits[field]))
  assert.equal(aggregateGasCap, BigInt(limits.aggregateGas))
  assert.equal(sponsorVerificationGasCap, BigInt(limits.paymasterVerificationGasLimit))
  assert.equal(maxFeeCap, BigInt(limits.maxFeePerGas))
  assert.equal(maxPriorityFeeCap, BigInt(limits.maxPriorityFeePerGas))
  assert.equal(maxPrefundCap, BigInt(limits.ethMaxPrefundWei))
  assert.deepEqual(boundedGasEstimate(gasCap), gasCap)
  for (const [field, cap] of Object.entries(gasCap)) {
    assert.throws(() => boundedGasEstimate({ ...gasCap, [field]: cap + 1n }),
      { message: `Alto ${field} exceeds reviewed cap` })
  }
})
