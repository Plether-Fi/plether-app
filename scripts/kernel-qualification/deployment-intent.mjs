import assert from 'node:assert/strict'
import { createRequire } from 'node:module'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const { isAddressEqual, keccak256 } = require('viem')

function normalizedHash(hash, message) {
  assert.match(hash ?? '', /^0x[0-9a-f]{64}$/iu, message)
  return hash.toLowerCase()
}

function normalizedReceiptAddress(address) {
  if (address == null) return null
  assert.match(address, /^0x[0-9a-f]{40}$/iu, 'Invalid deployment receipt contractAddress')
  return address.toLowerCase()
}

export function fingerprintDeploymentRequest({ to, data, value = 0n }) {
  return { to: to?.toLowerCase() ?? null, inputHash: keccak256(data), valueWei: String(value) }
}

export function deploymentFeeFields({ gas, maxFeePerGas, maxPriorityFeePerGas }) {
  return { gas: String(gas), maxFeePerGas: String(maxFeePerGas), maxPriorityFeePerGas: String(maxPriorityFeePerGas) }
}

export async function signDeploymentTransaction({ request, chainId, nonce, fees, signTransaction }) {
  assert.ok(fees, 'Only a journaled EIP-1559 transaction can be rebroadcast')
  for (const field of ['gas', 'maxFeePerGas', 'maxPriorityFeePerGas']) {
    assert.match(fees[field] ?? '', /^(?:0|[1-9][0-9]*)$/u, `Invalid recorded ${field}`)
  }
  const gas = BigInt(fees.gas)
  const maxFeePerGas = BigInt(fees.maxFeePerGas)
  const maxPriorityFeePerGas = BigInt(fees.maxPriorityFeePerGas)
  assert.ok(gas > 0n && maxFeePerGas > 0n && maxPriorityFeePerGas <= maxFeePerGas, 'Invalid deployment gas or fees')
  assert.ok(Number.isSafeInteger(nonce) && nonce >= 0, 'Invalid deployment nonce')
  const signed = await signTransaction({ ...request, type: 'eip1559', chainId, nonce, gas, maxFeePerGas, maxPriorityFeePerGas })
  return { signed, hash: keccak256(signed) }
}

export async function rebroadcastRecordedDeployment({ manifest, request, signTransaction, assertCleanNonce, sendRawTransaction }) {
  const intent = manifest.inFlight
  assert.match(intent?.hash ?? '', /^0x[0-9a-f]{64}$/iu, 'Rebroadcast requires an exact recorded transaction hash')
  assert.equal(intent.nonce, manifest.expectedNonce, 'In-flight nonce differs from the manifest checkpoint')
  assert.match(intent.nonce, /^(?:0|[1-9][0-9]*)$/u)
  assert.deepEqual(fingerprintDeploymentRequest(request), intent.request, 'Reviewed deployment action differs from the intent')
  const nonce = Number(intent.nonce)
  const { signed, hash } = await signDeploymentTransaction({ request, chainId: manifest.chainId, nonce,
    fees: intent.fees, signTransaction })
  assert.equal(hash, intent.hash.toLowerCase(), 'Reconstructed transaction hash differs from the original signed intent')
  await assertCleanNonce(nonce)
  const sentHash = await sendRawTransaction(signed)
  assert.equal(sentHash.toLowerCase(), hash, 'Provider returned a different deployment transaction hash')
  return hash
}

export async function readMinedDeployment({ clients, manifest, intent, awaitSafeBlock, expectedRequest }) {
  const { name, nonce } = intent
  const hash = normalizedHash(intent.hash, 'Recover only a recorded deployment transaction hash')
  assert.equal(nonce, manifest.expectedNonce, 'In-flight nonce differs from the manifest checkpoint')
  assert.match(nonce ?? '', /^(?:0|[1-9][0-9]*)$/u)
  const observations = await Promise.all([clients.primary, clients.secondary].map(async client => {
    const [receipt, transaction] = await Promise.all([
      client.getTransactionReceipt({ hash }), client.getTransaction({ hash }),
    ])
    assert.equal(normalizedHash(receipt.transactionHash, 'Invalid deployment receipt hash'), hash, 'Receipt hash differs from intent')
    assert.equal(normalizedHash(transaction.hash, 'Invalid deployment transaction hash'), hash, 'Transaction hash differs from intent')
    assert.ok(isAddressEqual(transaction.from, manifest.binding.deployer), 'In-flight transaction sender differs')
    assert.equal(transaction.chainId, manifest.chainId, 'In-flight transaction chain differs')
    assert.equal(String(transaction.nonce), nonce, 'In-flight transaction nonce differs')
    const blockHash = normalizedHash(receipt.blockHash, 'Invalid deployment receipt block hash')
    assert.equal(normalizedHash(transaction.blockHash, 'Invalid deployment transaction block hash'), blockHash,
      'Transaction and receipt block hashes differ')
    assert.equal(transaction.blockNumber, receipt.blockNumber, 'Transaction and receipt heights differ')
    assert.ok(receipt.status === 'success' || receipt.status === 'reverted', 'Unknown deployment receipt status')
    return { receipt, blockHash, contractAddress: normalizedReceiptAddress(receipt.contractAddress),
      request: fingerprintDeploymentRequest({ to: transaction.to, data: transaction.input, value: transaction.value }) }
  }))
  const [first, second] = observations
  assert.deepEqual(first.request, second.request, 'Independent RPCs disagree on deployment transaction data')
  for (const field of ['blockHash', 'contractAddress']) {
    assert.equal(first[field], second[field], `Independent RPCs disagree on deployment receipt ${field}`)
  }
  for (const field of ['blockNumber', 'status']) {
    assert.equal(first.receipt[field], second.receipt[field], `Independent RPCs disagree on deployment receipt ${field}`)
  }
  const expected = await expectedRequest(name, first.receipt.blockNumber)
  assert.deepEqual(first.request, expected, 'Mined transaction does not match the reviewed deployment action')
  if (intent.request) assert.deepEqual(first.request, intent.request, 'Mined transaction differs from the signed intent')
  if (awaitSafeBlock) {
    const safe = await awaitSafeBlock(clients, first.receipt.blockNumber)
    assert.ok(safe.number >= first.receipt.blockNumber, 'Deployment receipt is not safe on both RPCs')
  }
  const canonical = await Promise.all([clients.primary, clients.secondary].map(client =>
    client.getBlock({ blockNumber: first.receipt.blockNumber })))
  for (const block of canonical) {
    assert.equal(normalizedHash(block.hash, 'Invalid canonical block hash'), first.blockHash, 'Deployment receipt is not canonical')
  }
  return first.receipt
}

export function completeMinedDeployment(manifest, receipt) {
  const { name, hash, nonce } = manifest.inFlight
  const record = { name, hash, nonce, blockNumber: String(receipt.blockNumber), blockHash: receipt.blockHash }
  const updated = { ...manifest, expectedNonce: String(BigInt(nonce) + 1n) }
  delete updated.inFlight
  if (receipt.status === 'success') updated.transactions = [...manifest.transactions, record]
  else updated.revertedTransactions = [...(manifest.revertedTransactions ?? []), record]
  return updated
}
