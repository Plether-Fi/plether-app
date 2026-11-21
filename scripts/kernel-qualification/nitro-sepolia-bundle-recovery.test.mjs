import assert from 'node:assert/strict'
import { createRequire } from 'node:module'
import test from 'node:test'
import { recoverOperationFromBundle as recoverEthOperationFromBundle } from './nitro-sepolia-bundle-recovery.mjs'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const { encodeFunctionData } = require('viem')
const { entryPoint09Abi, getUserOperationHash, toPackedUserOperation } = require('viem/account-abstraction')
const entryPoint = '0x433709009B8330FDa32311DF1C2AFA402eD8D009'
const chainId = 421614
const receipt = { transactionHash: '0x' + 'ab'.repeat(32), blockHash: '0x' + 'cd'.repeat(32), blockNumber: 331412106n }
const sender = '0x3f4Be7F68bE2484be0813B3B79286Ab7d9076fE9'
const factory = '0xA3062A34599B9ef3D7D946B21770b31BDF4f3b0B'
const operation = {
  sender, factory, factoryData: '0xabcdef00', callData: '0x12345678', nonce: 0n,
  callGasLimit: 16323n, verificationGasLimit: 354471n, preVerificationGas: 52102n,
  maxFeePerGas: 24100000n, maxPriorityFeePerGas: 100000n, signature: '0xdeadbeef',
}
const sponsor = {
  ...operation, paymaster: factory, paymasterVerificationGasLimit: 1n, paymasterPostOpGasLimit: 2n,
  paymasterData: '0x1200', paymasterSignature: '0x3040',
}

function bundle(operations, target = operation) {
  const input = encodeFunctionData({
    abi: entryPoint09Abi, functionName: 'handleOps',
    args: [operations.map(op => toPackedUserOperation(op)), sender],
  })
  const tx = { hash: receipt.transactionHash, to: entryPoint, chainId, input,
    blockHash: receipt.blockHash, blockNumber: receipt.blockNumber }
  return { transactions: [tx, { ...tx }], receipt, entryPoint, chainId,
    operationHash: getUserOperationHash({ userOperation: target, chainId, entryPointAddress: entryPoint, entryPointVersion: '0.9' }) }
}

test('matches the exact v0.9 hash in a mixed bundle and restores factory and fee fields', () => {
  const found = recoverEthOperationFromBundle(bundle([sponsor, { ...operation, nonce: 1n }, operation]))
  assert.deepEqual(found, { ...operation, factory: factory.toLowerCase() })
  assert.equal(found.paymaster, undefined)
  assert.equal(found.paymasterData, undefined)
})

test('rejects an unmatched hash, ambiguous matching operations and a sponsored match', () => {
  assert.throws(() => recoverEthOperationFromBundle(bundle([{ ...operation, nonce: 1n }])), /exactly one matching ETH/)
  assert.throws(() => recoverEthOperationFromBundle(bundle([operation, { ...operation, signature: '0x0102' }])), /exactly one matching ETH/)
  assert.throws(() => recoverEthOperationFromBundle(bundle([sponsor], sponsor)), /exactly one matching ETH/)
})

test('rejects inconsistent calldata, wrong transaction metadata, and trailing bytes', () => {
  const wrongCalldata = bundle([operation])
  wrongCalldata.transactions[1].input += '00'
  assert.throws(() => recoverEthOperationFromBundle(wrongCalldata), /disagree on the included bundle calldata/)
  const wrongDestination = bundle([operation])
  wrongDestination.transactions[0].to = sender
  assert.throws(() => recoverEthOperationFromBundle(wrongDestination), /did not call the pinned EntryPoint/)
  const wrongBlock = bundle([operation])
  wrongBlock.transactions[1].blockNumber++
  assert.throws(() => recoverEthOperationFromBundle(wrongBlock), /block differs from receipt/)
  const trailingBytes = bundle([operation])
  for (const tx of trailingBytes.transactions) tx.input += '00'
  assert.throws(() => recoverEthOperationFromBundle(trailingBytes), /calldata is not canonical/)
})

test('recovers only the exact Plether paymaster and envelope in a sponsored bundle', () => {
  const expected = { ...operation, paymaster: factory.toLowerCase(), paymasterVerificationGasLimit: 100n,
    paymasterPostOpGasLimit: 0n, paymasterData: '0x' + '11'.repeat(157) }
  const verified = recoverEthOperationFromBundle({ ...bundle([operation, sponsor, expected], expected), paymaster: factory })
  assert.deepEqual(verified, { ...expected, factory: factory.toLowerCase() })
  assert.throws(() => recoverEthOperationFromBundle({ ...bundle([expected], expected), paymaster: sender }),
    /exactly one matching sponsored/)
})
