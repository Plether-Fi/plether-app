import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { test } from 'node:test'
import { boundedGasEstimate, gasCap } from './nitro-sepolia-gas.mjs'

test('recorded Nitro preVerificationGas fits the smoke cap', () => {
  const evidence = JSON.parse(readFileSync(new URL('../../docs/runbooks/evidence/kernel-nitro-safemode-2026-11-20.json', import.meta.url)))
  const recorded = evidence.operations.find(operation => operation.estimatedGas?.preVerification === '5861566')
  assert.ok(recorded, 'Recorded Nitro SafeMode estimate is missing')
  const gas = boundedGasEstimate({
    callGasLimit: recorded.estimatedGas.call,
    verificationGasLimit: recorded.estimatedGas.verification,
    preVerificationGas: recorded.estimatedGas.preVerification,
  })
  assert.equal(gas.preVerificationGas, 5_861_566n)
  assert.ok(gasCap.preVerificationGas >= gas.preVerificationGas)
  assert.throws(() => boundedGasEstimate({ ...gas, preVerificationGas: gasCap.preVerificationGas + 1n }),
    /Alto preVerificationGas exceeds reviewed cap/u)
})
