import assert from 'node:assert/strict'
import { test } from 'node:test'
import {
  completeMinedDeployment, deploymentFeeFields, fingerprintDeploymentRequest, readMinedDeployment,
  rebroadcastRecordedDeployment, signDeploymentTransaction,
} from './deployment-intent.mjs'

const hash = `0x${'11'.repeat(32)}`
const blockHash = `0x${'22'.repeat(32)}`
const deployer = '0x1111111111111111111111111111111111111111'
const contract = '0x2222222222222222222222222222222222222222'
const action = { to: contract, data: '0x12345678', value: 10n }

function fixture({ status = 'success', safe = 100n, secondValue = 10n, secondHash = blockHash, recorded = true } = {}) {
  const manifest = { expectedNonce: '8', chainId: 421614, binding: { deployer }, transactions: [],
    inFlight: { name: 'Deposit paymaster', hash, nonce: '8',
      ...(recorded ? { request: fingerprintDeploymentRequest(action) } : {}) } }
  function client(value, canonicalHash) {
    return {
      async getTransactionReceipt() { return { transactionHash: hash, blockHash, blockNumber: 100n, status } },
      async getTransaction() { return { hash, from: deployer, to: contract, input: action.data, value,
        chainId: manifest.chainId, nonce: 8, blockHash, blockNumber: 100n } },
      async getBlock() { return { hash: canonicalHash } },
    }
  }
  return { manifest, intent: manifest.inFlight,
    clients: { primary: client(10n, blockHash), secondary: client(secondValue, secondHash) },
    awaitSafeBlock: async () => ({ number: safe }),
    expectedRequest: async () => fingerprintDeploymentRequest(action) }
}

test('reconciles both durable and legacy known hashes without resending', async () => {
  for (const recorded of [true, false]) {
    const plan = fixture({ recorded })
    const updated = completeMinedDeployment(plan.manifest, await readMinedDeployment(plan))
    assert.equal(updated.inFlight, undefined)
    assert.equal(updated.expectedNonce, '9')
    assert.equal(updated.transactions[0].hash, hash)
  }
})

test('a canonical reverted transaction advances the nonce without completing the action', async () => {
  const plan = fixture({ status: 'reverted' })
  const updated = completeMinedDeployment(plan.manifest, await readMinedDeployment(plan))
  assert.equal(updated.expectedNonce, '9')
  assert.deepEqual(updated.transactions, [])
  assert.equal(updated.revertedTransactions[0].hash, hash)
})

test('keeps the intent on unsafe, noncanonical or mismatching responses', async () => {
  await assert.rejects(readMinedDeployment(fixture({ safe: 99n })), /not safe/)
  await assert.rejects(readMinedDeployment(fixture({ secondHash: hash })), /not canonical/)
  await assert.rejects(readMinedDeployment(fixture({ secondValue: 11n })), /disagree/)
  const wrongAction = fixture()
  wrongAction.expectedRequest = async () => fingerprintDeploymentRequest({ ...action, value: 11n })
  await assert.rejects(readMinedDeployment(wrongAction), /reviewed deployment action/)
  assert.equal(wrongAction.manifest.inFlight.hash, hash)
})

test('rebroadcasts only the exact recorded EIP-1559 transaction after the nonce check', async () => {
  const { manifest } = fixture()
  const fees = { gas: 70_000n, maxFeePerGas: 200_000_000n, maxPriorityFeePerGas: 100_000n }
  const serialized = '0x1234'
  const calls = []
  const signTransaction = async tx => {
    assert.deepEqual(tx, { ...action, ...fees, chainId: 421614, nonce: 8, type: 'eip1559' })
    calls.push('signed')
    return serialized
  }
  manifest.inFlight.fees = deploymentFeeFields(fees)
  manifest.inFlight.hash = (await signDeploymentTransaction({ request: action, chainId: manifest.chainId,
    nonce: 8, fees: manifest.inFlight.fees, signTransaction })).hash
  calls.length = 0
  const recovery = { manifest, request: action, signTransaction,
    assertCleanNonce: async nonce => { assert.equal(nonce, 8); calls.push('nonce checked') },
    sendRawTransaction: async raw => { assert.equal(raw, serialized); calls.push('sent'); return manifest.inFlight.hash },
  }
  assert.equal(await rebroadcastRecordedDeployment(recovery), manifest.inFlight.hash)
  assert.deepEqual(calls, ['signed', 'nonce checked', 'sent'])
  calls.length = 0
  await assert.rejects(rebroadcastRecordedDeployment({ ...recovery, assertCleanNonce: async () => { throw Error('nonce in use') } }),
    /nonce in use/u)
  await assert.rejects(rebroadcastRecordedDeployment({ ...recovery, signTransaction: async () => '0x5678' }),
    /differs from the original signed intent/u)
  await assert.rejects(rebroadcastRecordedDeployment({ ...recovery, request: { ...action, value: 11n } }),
    /differs from the intent/u)
  delete manifest.inFlight.fees
  await assert.rejects(rebroadcastRecordedDeployment(recovery), /journaled EIP-1559/u)
  assert.deepEqual(calls, ['signed'])
})
