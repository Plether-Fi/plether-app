import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { createRequire } from 'node:module'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { assertCleanNonce, broadcast, observeStableNonce, verifyFundingReceipt } from './fund-dedicated-deployer.mjs'

const require = createRequire(new URL('../../apps/frontend/package.json', import.meta.url))
const { privateKeyToAccount } = require('viem/accounts')

const source = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
const recipient = '0x1111111111111111111111111111111111111111'
const hash = `0x${'ab'.repeat(32)}`
const blockHash = `0x${'cd'.repeat(32)}`
const value = 4_600_000_000_000_000_000n
const record = { source, recipient, valueWei: String(value), nonce: '174300', hash }

function fixture({ transactionTo = recipient, afterBalance = value, status = 'success', safeNumber = 201n } = {}) {
  const tx = { hash, from: source, to: transactionTo, value, nonce: 174300, input: '0x', chainId: 421614,
    blockHash, blockNumber: 200n }
  const receipt = { transactionHash: hash, status, blockHash, blockNumber: 200n }
  function client() {
    return {
      async getChainId() { return 421614 },
      async getBlock({ blockTag, blockNumber }) {
        if (blockTag === 'safe') return { number: safeNumber, hash: blockHash }
        if (blockNumber === safeNumber || blockNumber === 200n) return { number: blockNumber, hash: blockHash }
        throw Error('Unexpected block')
      },
      async getTransaction() { return tx },
      async getTransactionReceipt() { return receipt },
      async getBalance({ blockNumber }) { return blockNumber === 199n ? 0n : afterBalance },
    }
  }
  return [client(), client()]
}

test('requires two providers to show the same clean latest and pending nonce', () => {
  assert.equal(assertCleanNonce([174300, 174300, 174300, 174300]), 174300n)
  assert.throws(() => assertCleanNonce([174300, 174301, 174300, 174301]), /pending transaction/u)
  assert.throws(() => assertCleanNonce([174300, 174300, 174301, 174301]), /disagree/u)
})

test('requires a stable source nonce through the entire quiet-window observation', async () => {
  let reads = 0
  let waits = 0
  assert.equal(await observeStableNonce(async () => { reads++; return 17n },
    async milliseconds => { assert.equal(milliseconds, 2_500); waits++ }, 17n, 3), 17n)
  assert.equal(reads, 3)
  assert.equal(waits, 3)
  await assert.rejects(observeStableNonce(async () => ++reads === 5 ? 18n : 17n,
    async () => {}, 17n, 3), /nonce changed during the quiet-window/u)
})

test('attests exact successful funding on two canonical safe RPCs', async () => {
  const result = await verifyFundingReceipt(fixture(), record)
  assert.equal(result.blockNumber, '200')
  assert.equal(result.recipientBalanceWei, String(value))
})

test('rejects a transaction to an unreviewed recipient', async () => {
  await assert.rejects(verifyFundingReceipt(fixture({ transactionTo: source }), record), /recipient changed/u)
})

test('rejects a funding receipt before the safe head reaches it', async () => {
  await assert.rejects(verifyFundingReceipt(fixture({ safeNumber: 199n }), record), /not on the common safe chain/u)
})

test('rejects a recipient balance delta other than 4.6 ETH', async () => {
  await assert.rejects(verifyFundingReceipt(fixture({ afterBalance: value - 1n }), record), /exactly 4.6 ETH/u)
})

test('a lost source nonce leaves a durable signed intent without sending', async () => {
  const key = `0x${'0'.repeat(63)}1`
  const previous = process.env.KERNEL_CANARY_FUNDING_PRIVATE_KEY
  process.env.KERNEL_CANARY_FUNDING_PRIVATE_KEY = key
  const folder = mkdtempSync(join(tmpdir(), 'kernel-funding-test-'))
  const path = join(folder, 'funding.json')
  let nonceReads = 0
  let sends = 0
  function client() {
    return {
      async getChainId() { return 421614 },
      async getBlock({ blockTag, blockNumber }) {
        if (blockTag === 'safe') return { number: 200n, hash: blockHash }
        assert.equal(blockNumber, 200n)
        return { number: 200n, hash: blockHash }
      },
      async getCode() { return '0x' },
      async getBalance({ address }) { return address === recipient ? 0n : 6_000_000_000_000_000_000n },
      async getTransactionCount() { return ++nonceReads <= 8 ? 174300 : 174301 },
      async estimateGas() { return 50_000n },
      async getGasPrice() { return 20_000_000n },
      async sendRawTransaction() { sends++; throw Error('must not broadcast') },
    }
  }
  try {
    await assert.rejects(broadcast({ deployment: 'test', source: privateKeyToAccount(key).address,
      recipient, providers: ['official', 'independent'] }, path, [client(), client()], 'test',
    { wait: async () => {}, stableSamples: 1 }), /nonce advanced before the funding send/u)
    assert.equal(sends, 0)
    const intent = JSON.parse(readFileSync(path, 'utf8'))
    assert.equal(intent.status, 'signed-intent')
    assert.equal(intent.nonce, '174300')
    assert.match(intent.hash, /^0x[0-9a-f]{64}$/u)
    assert.ok(!readFileSync(path, 'utf8').includes(key))
  } finally {
    if (previous === undefined) delete process.env.KERNEL_CANARY_FUNDING_PRIVATE_KEY
    else process.env.KERNEL_CANARY_FUNDING_PRIVATE_KEY = previous
    rmSync(folder, { recursive: true, force: true })
  }
})
