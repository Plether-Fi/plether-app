import assert from 'node:assert/strict'
import { test } from 'node:test'
import { assertExactGasDebit, assertRecoveredEthSmokeOperation, validateSmokeRpcUrls } from './nitro-sepolia-smoke-guards.mjs'

const localUrls = {
  alto: 'http://127.0.0.1:18080/',
  nitro: 'http://127.0.0.1:18081/',
  secondary: 'https://arb-sepolia.g.alchemy.com/v2/test-credential',
}

test('smoke RPCs accept localhost tunnels and an independent HTTPS chain provider', () => {
  assert.deepEqual(validateSmokeRpcUrls(localUrls), localUrls)
})

test('direct private ALBs require exact reviewed hosts', () => {
  const urls = { ...localUrls, alto: 'http://internal-plether-kernel-alto-123.ap-southeast-1.elb.amazonaws.com/' }
  assert.throws(() => validateSmokeRpcUrls(urls), /exact reviewed KERNEL_SMOKE_EXPECTED_ALTO_ALB_HOST/u)
  assert.deepEqual(validateSmokeRpcUrls(urls, { alto: new URL(urls.alto).hostname }), urls)
})

test('smoke RPCs reject credential-bearing URLs and unreviewed public endpoints', () => {
  for (const changed of [
    { alto: 'https://bundler.example/rpc' },
    { nitro: 'https://sepolia-rollup.arbitrum.io/rpc' },
    { alto: 'http://user:pass@127.0.0.1:18080/' },
    { nitro: 'http://127.0.0.1:18081/?token=secret' },
    { secondary: 'http://arb-sepolia.g.alchemy.com/v2/key' },
  ]) assert.throws(() => validateSmokeRpcUrls({ ...localUrls, ...changed }))
})

test('payer debit must exactly equal EntryPoint gas charged', () => {
  assert.doesNotThrow(() => assertExactGasDebit(100n, 77n, 23n, 'test'))
  assert.throws(() => assertExactGasDebit(100n, 76n, 23n, 'test'), /payer debit differs/u)
})

const reviewed = {
  sender: '0x1111111111111111111111111111111111111111',
  callData: '0x1234',
  factory: '0x2222222222222222222222222222222222222222',
  factoryData: '0xabcd',
  estimate: { callGasLimit: '300000', verificationGasLimit: '400000', preVerificationGas: '500000' },
  gasCap: { callGasLimit: 3_000_000n, verificationGasLimit: 1_000_000n, preVerificationGas: 12_000_000n },
  maxFeeCap: 10_000_000_000n,
  maxPrefundCap: 50_000_000_000_000_000n,
  maxPrefundWei: '1200000000000000',
}
const included = {
  sender: reviewed.sender, nonce: 0n, callData: reviewed.callData,
  factory: reviewed.factory, factoryData: reviewed.factoryData,
  callGasLimit: 300_000n, verificationGasLimit: 400_000n, preVerificationGas: 500_000n,
  maxFeePerGas: 1_000_000_000n, maxPriorityFeePerGas: 1_000_000n,
}

test('recovery accepts only the exact reviewed ETH first operation', () => {
  assert.doesNotThrow(() => assertRecoveredEthSmokeOperation(included, reviewed))
  for (const changed of [
    { nonce: 1n }, { callData: '0x5678' }, { factoryData: '0x5678' },
    { paymaster: reviewed.factory }, { preVerificationGas: 500_001n },
    { maxFeePerGas: 2_000_000_000n },
  ]) assert.throws(() => assertRecoveredEthSmokeOperation({ ...included, ...changed }, reviewed))
  assert.throws(() => assertRecoveredEthSmokeOperation(included, { ...reviewed, maxPrefundCap: 1n }),
    /exceeds the 0.05 ETH smoke cap/u)
})
