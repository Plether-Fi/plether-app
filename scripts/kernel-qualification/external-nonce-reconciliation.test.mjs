import assert from 'node:assert/strict'
import test from 'node:test'
import {
  externalOrderRouter, externalOrderSelector, reconcileExternalNonces, verifyRecordedExternalNonces,
} from './external-nonce-reconciliation.mjs'

const deployer = '0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b'
const hash = nonce => `0x${nonce.toString(16).padStart(64, '0')}`
const blockHash = nonce => `0x${(nonce + 1000).toString(16).padStart(64, '0')}`
const baseManifest = () => ({
  binding: { deployer }, expectedNonce: '10', inFlight: { name: 'Stake Plether Staker', nonce: '10' },
  transactions: [],
})

function fixture({ nonces = [10, 11], tip = 12, pending = tip, secondaryTip = tip, to = externalOrderRouter } = {}) {
  const transactions = new Map(nonces.map(nonce => [hash(nonce), {
    hash: hash(nonce), from: deployer, to, input: `${externalOrderSelector}00`, value: 0n,
    nonce, blockNumber: BigInt(100 + nonce), blockHash: blockHash(nonce),
  }]))
  const receipts = new Map(nonces.map(nonce => [hash(nonce), {
    transactionHash: hash(nonce), status: 'success', blockNumber: BigInt(100 + nonce), blockHash: blockHash(nonce),
  }]))
  function client(latestNonce) {
    return {
      async getTransaction({ hash: requested }) { return transactions.get(requested) },
      async getTransactionReceipt({ hash: requested }) { return receipts.get(requested) },
      async getBlock({ blockNumber }) { return { number: blockNumber, hash: blockHash(Number(blockNumber) - 100) } },
      async getTransactionCount({ blockTag }) { return blockTag === 'pending' ? pending : latestNonce },
    }
  }
  return { clients: { primary: client(tip), secondary: client(secondaryTip) }, transactions, receipts }
}

test('reconciles the disputed safe nonce and incremental later orders without signing', async () => {
  const { clients } = fixture({ tip: 13, nonces: [10, 11, 12] })
  const manifest = baseManifest()
  const safeRequests = []
  const awaitSafeBlock = async (_, atLeast) => {
    safeRequests.push(atLeast)
    return { number: 110n, hash: blockHash(10) }
  }
  const first = await reconcileExternalNonces({ clients, manifest, hashes: [hash(10)], awaitSafeBlock })
  assert.equal(first.expectedNonce, '11')
  assert.equal(first.inFlight, undefined)
  assert.equal(first.externalTransactions[0].finalityAtReconciliation, 'safe')
  assert.deepEqual(safeRequests, [110n])
  assert.deepEqual(manifest, baseManifest(), 'A reconciliation result must not mutate the live manifest in memory')

  const second = await reconcileExternalNonces({ clients, manifest: first, hashes: [hash(11), hash(12)],
    awaitSafeBlock: () => { throw Error('Later orders must not wait for a moving safe head') } })
  assert.equal(second.expectedNonce, '13')
  assert.deepEqual(second.externalTransactions.map(record => record.finalityAtReconciliation), ['safe', 'latest', 'latest'])
  await verifyRecordedExternalNonces({ clients, manifest: second, awaitSafeBlock })
})

test('rejects an unrelated destination without advancing the manifest', async () => {
  const { clients } = fixture({ nonces: [10], tip: 11, to: '0x3316aaef074fe82f492a898ac91f740b8e2f0078' })
  const manifest = baseManifest()
  await assert.rejects(reconcileExternalNonces({ clients, manifest, hashes: [hash(10)],
    awaitSafeBlock: async () => ({ number: 110n, hash: blockHash(10) }) }), /reviewed Perps OrderRouter/u)
  assert.deepEqual(manifest, baseManifest())
})

test('keeps the intent unresolved until the disputed receipt is safe', async () => {
  const { clients } = fixture({ nonces: [10], tip: 11 })
  const manifest = baseManifest()
  await assert.rejects(reconcileExternalNonces({ clients, manifest, hashes: [hash(10)],
    awaitSafeBlock: async () => { throw Error('safe head has not reached the disputed receipt') } }),
  /safe head/u)
  assert.deepEqual(manifest, baseManifest())
})

test('requires hashes in contiguous nonce order', async () => {
  const { clients } = fixture({ nonces: [10, 11], tip: 12 })
  await assert.rejects(reconcileExternalNonces({ clients, manifest: baseManifest(), hashes: [hash(11), hash(10)],
    awaitSafeBlock: async () => ({ number: 111n, hash: blockHash(11) }) }), /nonce is not contiguous/u)
})

test('records a pending external nonce without skipping it', async () => {
  const { clients } = fixture({ nonces: [10], tip: 11, pending: 12 })
  const result = await reconcileExternalNonces({ clients, manifest: baseManifest(), hashes: [hash(10)],
    awaitSafeBlock: async () => ({ number: 110n, hash: blockHash(10) }) })
  assert.equal(result.expectedNonce, '11')
  assert.equal(result.externalNonceReconciliations[0].observedPendingNonce, '12')
})

test('rejects inconsistent provider nonce state after verifying the hashes', async () => {
  const { clients } = fixture({ nonces: [10], tip: 11, secondaryTip: 12 })
  await assert.rejects(reconcileExternalNonces({ clients, manifest: baseManifest(), hashes: [hash(10)],
    awaitSafeBlock: async () => ({ number: 110n, hash: blockHash(10) }) }), /latest nonce differs/u)
})

test('rechecks all recorded nonces in bounded parallel batches, including the tail', async () => {
  const nonces = Array.from({ length: 11 }, (_, index) => 10 + index)
  const { clients, transactions } = fixture({ nonces, tip: 21 })
  const manifest = {
    binding: { deployer },
    externalTransactions: nonces.map(nonce => ({
      hash: hash(nonce), nonce: String(nonce), blockNumber: String(100 + nonce),
      blockHash: blockHash(nonce), to: externalOrderRouter, selector: externalOrderSelector,
      valueWei: '0', classification: 'plether-sepolia-perps-executeOrder',
      finalityAtReconciliation: 'latest',
    })),
  }
  let active = 0
  let peak = 0
  for (const client of [clients.primary, clients.secondary]) {
    const original = client.getTransaction.bind(client)
    client.getTransaction = async args => {
      active += 1
      peak = Math.max(peak, active)
      await new Promise(resolve => setTimeout(resolve, 1))
      try { return await original(args) } finally { active -= 1 }
    }
  }
  await verifyRecordedExternalNonces({ clients, manifest,
    awaitSafeBlock: () => { throw Error('No safe-marked records') } })
  assert.ok(peak > 2 && peak <= 8, 'At most four records may be checked concurrently on two RPCs')

  transactions.get(hash(20)).to = '0x3316aaef074fe82f492a898ac91f740b8e2f0078'
  await assert.rejects(verifyRecordedExternalNonces({ clients, manifest,
    awaitSafeBlock: () => { throw Error('No safe-marked records') } }), /reviewed Perps OrderRouter/u)
})
