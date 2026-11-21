import assert from 'node:assert/strict'
import test from 'node:test'
import {
  assertBalanceEvidence, assertDirectTopup, assertTwoRpcAgreement, assertUnusedSigner,
  chainId, minimumTotalWei, minimumTopupWei, recipient,
} from './verify-topup.mjs'

const hash = `0x${'ab'.repeat(32)}`
const blockHash = `0x${'cd'.repeat(32)}`
const sender = '0x6cc9397c3b38739dacbfaa68ead5f5d77ba5f455'
const wei = 10n ** 18n

function evidence(overrides = {}) {
  const tx = {
    hash, from: sender, to: recipient, input: '0x',
    value: `0x${(wei / 2n).toString(16)}`,
    type: '0x2', chainId: `0x${chainId.toString(16)}`,
    blockNumber: '0x64', blockHash,
    ...overrides.tx,
  }
  const receipt = {
    transactionHash: hash, status: '0x1',
    blockNumber: '0x64', blockHash,
    ...overrides.receipt,
  }
  return [tx, receipt]
}

test('accepts a direct 0.5 ETH L2 transfer at a safe canonical block', () => {
  const [tx, receipt] = evidence()
  const transfer = assertDirectTopup(tx, receipt, hash, 120n)
  assert.equal(transfer.valueWei, wei / 2n)
  assert.equal(transfer.blockNumber, 100n)
  assert.equal(transfer.blockHash, blockHash)
})

test('rejects a transfer to another account or from the dedicated signer', () => {
  const [tx, receipt] = evidence()
  assert.throws(() => assertDirectTopup({ ...tx, to: sender }, receipt, hash, 120n),
    /recipient/u)
  assert.throws(() => assertDirectTopup({ ...tx, from: recipient }, receipt, hash, 120n),
    /another address/u)
})

test('rejects calldata, a bridge deposit, another chain, and a too-small transfer', () => {
  const [tx, receipt] = evidence()
  assert.throws(() => assertDirectTopup({ ...tx, input: '0x1234' }, receipt, hash, 120n),
    /without calldata/u)
  assert.throws(() => assertDirectTopup({ ...tx, type: '0x64' }, receipt, hash, 120n),
    /standard L2 user transaction/u)
  assert.throws(() => assertDirectTopup({ ...tx, type: '0x65' }, receipt, hash, 120n),
    /standard L2 user transaction/u)
  assert.throws(() => assertDirectTopup({ ...tx, chainId: '0x1' }, receipt, hash, 120n),
    /chain ID/u)
  assert.throws(() => assertDirectTopup({
    ...tx, value: `0x${(minimumTopupWei - 1n).toString(16)}`,
  }, receipt, hash, 120n), /below 0.27/u)
})

test('requires a successful mined receipt on the common safe chain', () => {
  const [tx, receipt] = evidence()
  assert.throws(() => assertDirectTopup(tx, { ...receipt, status: '0x0' }, hash, 120n),
    /reverted/u)
  assert.throws(() => assertDirectTopup(tx, receipt, hash, 99n), /safe chain/u)
  assert.throws(() => assertDirectTopup(tx, { ...receipt, blockHash: hash }, hash, 120n),
    /block hashes differ/u)
  assert.throws(() => assertDirectTopup({ ...tx, blockNumber: null }, receipt, hash, 120n),
    /not mined/u)
})

test('requires the two RPCs to agree on the sender, value, and inclusion block', () => {
  const [tx, receipt] = evidence()
  const transfer = assertDirectTopup(tx, receipt, hash, 120n)
  assert.deepEqual(assertTwoRpcAgreement(transfer, { ...transfer }), transfer)
  assert.throws(() => assertTwoRpcAgreement(transfer, {
    ...transfer, valueWei: transfer.valueWei + 1n,
  }), /transfer value/u)
})

test('attributes the balance increase and requires 2.75 ETH on safe and latest blocks', () => {
  const balance = assertBalanceEvidence(
    [248n * wei / 100n, 248n * wei / 100n],
    [298n * wei / 100n, 298n * wei / 100n],
    [298n * wei / 100n, 298n * wei / 100n],
    [298n * wei / 100n, 298n * wei / 100n],
    wei / 2n,
  )
  assert.ok(balance.safeWei >= minimumTotalWei)
  assert.throws(() => assertBalanceEvidence(
    [248n * wei / 100n, 248n * wei / 100n],
    [298n * wei / 100n, 298n * wei / 100n],
    [minimumTotalWei - 1n, minimumTotalWei - 1n],
    [298n * wei / 100n, 298n * wei / 100n],
    wei / 2n,
  ), /less than 2.75/u)
  assert.throws(() => assertBalanceEvidence(
    [248n * wei / 100n, 248n * wei / 100n],
    [298n * wei / 100n, 298n * wei / 100n + 1n],
    [298n * wei / 100n, 298n * wei / 100n],
    [298n * wei / 100n, 298n * wei / 100n],
    wei / 2n,
  ), /disagree/u)
})

test('accepts Viem numeric zero nonce and rejects a used or pending signer', () => {
  assert.doesNotThrow(() => assertUnusedSigner(undefined, 0, 0, 'Test'))
  assert.throws(() => assertUnusedSigner('0x1234', 0, 0, 'Test'), /has code/u)
  assert.throws(() => assertUnusedSigner(undefined, 1, 1, 'Test'), /already sent/u)
  assert.throws(() => assertUnusedSigner(undefined, 0, 1, 'Test'), /pending/u)
})
