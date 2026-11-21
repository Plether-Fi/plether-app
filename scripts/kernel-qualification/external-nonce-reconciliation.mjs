import assert from 'node:assert/strict'

// An existing Plether deployment signer also submits Sepolia Perps orders.
// Only these reviewed, zero-value calls can be classified as external to the
// Kernel canary deployment. Any other nonce consumption needs a new review.
export const externalOrderRouter = '0x918663d6da8cb33b325a04c4a5e8c51d42f3b444'
export const externalOrderSelector = '0xc700abdc' // executeOrder(uint64,bytes[])

const hexHash = /^0x[0-9a-fA-F]{64}$/u
const decimal = /^(0|[1-9][0-9]*)$/u
const lower = value => String(value).toLowerCase()

function canonicalNonce(value, label) {
  assert.match(String(value), decimal, `${label} must be a canonical decimal nonce`)
  return BigInt(value)
}

function assertExternalTransaction(tx, receipt, hash, nonce, deployer, safeBlock) {
  assert.equal(lower(tx.hash), lower(hash), 'External transaction hash changed')
  assert.equal(lower(receipt.transactionHash), lower(hash), 'External receipt hash changed')
  assert.equal(lower(tx.from), lower(deployer), 'External transaction signer differs from deployer')
  assert.equal(BigInt(tx.nonce), nonce, 'External transaction nonce is not contiguous')
  assert.equal(lower(tx.to), externalOrderRouter, 'External nonce was not consumed by the reviewed Perps OrderRouter')
  assert.equal(lower(tx.input).slice(0, 10), externalOrderSelector, 'External call was not executeOrder(uint64,bytes[])')
  assert.equal(tx.value, 0n, 'External order call transferred ETH')
  assert.equal(receipt.status, 'success', 'External order transaction did not succeed')
  if (safeBlock) assert.ok(receipt.blockNumber <= safeBlock.number, 'Disputed external order receipt is not safe')
  assert.equal(lower(tx.blockHash), lower(receipt.blockHash), 'External transaction and receipt blocks differ')
  assert.equal(tx.blockNumber, receipt.blockNumber, 'External transaction and receipt heights differ')
}

async function verifiedExternalTransaction(clients, hash, nonce, deployer, safeBlock) {
  const [txA, txB, receiptA, receiptB] = await Promise.all([
    clients.primary.getTransaction({ hash }), clients.secondary.getTransaction({ hash }),
    clients.primary.getTransactionReceipt({ hash }), clients.secondary.getTransactionReceipt({ hash }),
  ])
  assertExternalTransaction(txA, receiptA, hash, nonce, deployer, safeBlock)
  assertExternalTransaction(txB, receiptB, hash, nonce, deployer, safeBlock)
  assert.equal(lower(txA.input), lower(txB.input), 'Independent RPCs disagree on external calldata')
  assert.equal(lower(receiptA.blockHash), lower(receiptB.blockHash), 'Independent RPCs disagree on external receipt block')
  assert.equal(receiptA.blockNumber, receiptB.blockNumber, 'Independent RPCs disagree on external receipt height')
  const [blockA, blockB] = await Promise.all([
    clients.primary.getBlock({ blockNumber: receiptA.blockNumber }),
    clients.secondary.getBlock({ blockNumber: receiptA.blockNumber }),
  ])
  assert.equal(lower(blockA.hash), lower(receiptA.blockHash), 'External receipt is not on the canonical primary chain')
  assert.equal(lower(blockB.hash), lower(receiptA.blockHash), 'External receipt is not on the canonical secondary chain')
  return {
    hash: lower(hash), nonce: String(nonce), blockNumber: String(receiptA.blockNumber),
    blockHash: receiptA.blockHash, to: externalOrderRouter, selector: externalOrderSelector,
    valueWei: '0', classification: 'plether-sepolia-perps-executeOrder',
    finalityAtReconciliation: safeBlock ? 'safe' : 'latest',
  }
}

async function cleanNonce(clients, expected) {
  const [latestA, pendingA, latestB, pendingB] = await Promise.all([clients.primary, clients.secondary].flatMap(client => [
    client.getTransactionCount({ address: expected.address, blockTag: 'latest' }),
    client.getTransactionCount({ address: expected.address, blockTag: 'pending' }),
  ]))
  assert.equal(BigInt(latestA), BigInt(latestB), 'Deployer latest nonce differs between providers')
  assert.equal(BigInt(pendingA), BigInt(pendingB), 'Deployer pending nonce differs between providers')
  assert.ok(BigInt(pendingA) >= BigInt(latestA), 'Deployer pending nonce is below confirmed nonce')
  assert.ok(BigInt(latestA) >= expected.nonce,
    'External transaction hashes exceed the deployer confirmed nonce')
  return { latest: BigInt(latestA), pending: BigInt(pendingA) }
}

export async function reconcileExternalNonces({ clients, manifest, hashes, awaitSafeBlock }) {
  if (manifest.inFlight) assert.ok(!manifest.inFlight.hash,
    'A script transaction hash exists; reconcile that transaction, not an external nonce')
  const firstNonce = canonicalNonce(manifest.expectedNonce, 'Manifest expectedNonce')
  if (manifest.inFlight) assert.equal(canonicalNonce(manifest.inFlight.nonce, 'In-flight nonce'), firstNonce,
    'In-flight nonce differs from expectedNonce')
  assert.ok(Array.isArray(hashes) && hashes.length > 0, 'Pass every external transaction hash in nonce order')
  assert.equal(new Set(hashes.map(lower)).size, hashes.length, 'External transaction hashes must be unique')
  for (const hash of hashes) assert.match(hash, hexHash, 'External transaction hash must be 32 bytes')
  // The script may have submitted the unresolved intent before losing its hash.
  // Finalize that one nonce before classifying it as an unrelated order. Later
  // orders only need independent canonical receipts: the active order executor
  // can stay ahead of the safe head indefinitely.
  const disputedReceipt = manifest.inFlight
    ? await clients.primary.getTransactionReceipt({ hash: hashes[0] }) : undefined
  const safeBlock = disputedReceipt ? await awaitSafeBlock(clients, disputedReceipt.blockNumber) : undefined
  const records = []
  for (const [index, hash] of hashes.entries()) {
    records.push(await verifiedExternalTransaction(clients, hash, firstNonce + BigInt(index),
      manifest.binding.deployer, index === 0 ? safeBlock : undefined))
  }
  const nextNonce = firstNonce + BigInt(hashes.length)
  const observedNonce = await cleanNonce(clients, { address: manifest.binding.deployer, nonce: nextNonce })
  const reconciled = {
    ...manifest,
    expectedNonce: String(nextNonce),
    externalTransactions: [...(manifest.externalTransactions ?? []), ...records],
    externalNonceReconciliations: [...(manifest.externalNonceReconciliations ?? []), {
      priorIntent: manifest.inFlight?.name ?? null, firstNonce: String(firstNonce), nextNonce: String(nextNonce),
      observedDeployerNonce: String(observedNonce.latest),
      observedPendingNonce: String(observedNonce.pending),
      disputedSafeBlockNumber: safeBlock ? String(safeBlock.number) : null,
      disputedSafeBlockHash: safeBlock?.hash ?? null,
      transactionHashes: records.map(record => record.hash),
    }],
  }
  delete reconciled.inFlight
  return reconciled
}

export async function verifyRecordedExternalNonces({ clients, manifest, awaitSafeBlock }) {
  const records = manifest.externalTransactions ?? []
  if (records.length === 0) return
  const disputed = records.filter(record => record.finalityAtReconciliation === 'safe')
  const lastSafeBlock = disputed.reduce((max, record) => {
    const block = canonicalNonce(record.blockNumber, 'Recorded disputed external block')
    return block > max ? block : max
  }, 0n)
  const safeBlock = disputed.length ? await awaitSafeBlock(clients, lastSafeBlock) : undefined
  for (const record of records) {
    const actual = await verifiedExternalTransaction(clients, record.hash,
      canonicalNonce(record.nonce, 'Recorded external nonce'), manifest.binding.deployer,
      record.finalityAtReconciliation === 'safe' ? safeBlock : undefined)
    assert.deepEqual(actual, record, 'Recorded external transaction changed')
  }
}
