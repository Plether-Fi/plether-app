import assert from 'node:assert/strict'
import test from 'node:test'
import { verifyFundingReceipt } from './fund-dedicated-deployer.mjs'

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

test('rejects a recipient balance delta that differs from the reviewed amount', async () => {
  await assert.rejects(verifyFundingReceipt(fixture({ afterBalance: value - 1n }), record), /differs from reviewed amount/u)
})
