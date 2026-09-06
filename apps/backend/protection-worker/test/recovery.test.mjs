import { describe, it } from 'node:test'
import assert from 'node:assert/strict'
import { keccak256 } from 'viem'
import { ProtectionWorker } from '../worker.mjs'

const book = '0x1111111111111111111111111111111111111111'
const release = { network: { chainId: 421614 }, release: { deploymentBlock: 100 }, contracts: { positionProtectionBook: { address: book }, orderRouter: { address: book }, orderLifecycleBook: { address: book }, pletherOracle: { address: book } } }
const raw = '0x1234'
const hash = keccak256(raw)
function fixture() {
  const calls = []
  let rows = []
  const db = { query: async (sql, args) => {
    calls.push(['sql', sql, args])
    if (sql.startsWith('INSERT INTO perps_protection_transactions')) rows.push({ transaction_hash: args[2], raw_transaction: args[3], created_at: new Date(), protection_id: '1', status: 'pending' })
    if (sql.startsWith('SELECT * FROM perps_protection_transactions')) return { rows }
    if (sql.startsWith('UPDATE')) rows = []
    return { rows: [] }
  } }
  const publicClient = {
    simulateContract: async () => { calls.push(['simulate']) },
    sendRawTransaction: async ({ serializedTransaction }) => { calls.push(['send', serializedTransaction]); throw new Error('Network response lost') },
    getTransactionReceipt: async () => { const e = new Error(); e.name = 'TransactionReceiptNotFoundError'; throw e },
  }
  const walletClient = { prepareTransactionRequest: async () => { calls.push(['prepare']); return { gas: 100n } }, signTransaction: async () => raw }
  const worker = new ProtectionWorker({ db, publicClient, walletClient, account: { address: book }, release, executionEnabled: true, log: () => {} })
  return { worker, calls, publicClient, db }
}

describe('durable protection transactions', () => {
  it('journals signed bytes before sending and rebroadcasts only those bytes after an ambiguous response', async () => {
    const { worker, calls } = fixture()
    await assert.rejects(worker.submit('retry', 1n, book, 'PositionProtectionBook', 'retryPositionProtectionClose', [1n]), /Network/)
    assert.equal(await worker.reconcileTransaction(), true)
    assert.deepEqual(calls.filter(call => call[0] === 'send'), [['send', raw], ['send', raw]])
    assert.equal(calls.filter(call => call[0] === 'prepare').length, 1)
    assert.ok(calls.findIndex(call => call[0] === 'sql' && call[1].startsWith('INSERT')) < calls.findIndex(call => call[0] === 'send'))
    const journal = calls.find(call => call[0] === 'sql' && call[1].startsWith('INSERT'))
    assert.equal(journal[2][2], hash)
  })
  it('never signs or journals a reverted simulation', async () => {
    const { worker, calls, publicClient } = fixture()
    publicClient.simulateContract = async () => { throw new Error('Position changed') }
    await assert.rejects(worker.submit('retry', 1n, book, 'PositionProtectionBook', 'retryPositionProtectionClose', [1n]))
    assert.equal(calls.length, 0)
  })
  it('observe mode simulates but never signs or sends', async () => {
    const { worker, calls } = fixture()
    worker.executionEnabled = false
    assert.equal(await worker.submit('retry', 1n, book, 'PositionProtectionBook', 'retryPositionProtectionClose', [1n]), false)
    assert.deepEqual(calls, [['simulate']])
  })
  it('a safe receipt settles recovery without rebroadcasting', async () => {
    const { worker, calls, publicClient } = fixture()
    await assert.rejects(worker.submit('retry', 1n, book, 'PositionProtectionBook', 'retryPositionProtectionClose', [1n]))
    publicClient.getTransactionReceipt = async () => ({ blockNumber: 100n, blockHash: '0xabc', status: 'success' })
    publicClient.getBlock = async () => ({ number: 120n, hash: '0xabc' })
    assert.equal(await worker.reconcileTransaction(), false)
    assert.equal(calls.filter(call => call[0] === 'send').length, 1)
    assert.ok(calls.some(call => call[0] === 'sql' && call[1].startsWith('UPDATE') && call[2][0] === 'confirmed'))
  })
  it('rebroadcasts the same bytes when an RPC receipt belongs to an orphaned block', async () => {
    const { worker, calls, publicClient } = fixture()
    await assert.rejects(worker.submit('retry', 1n, book, 'PositionProtectionBook', 'retryPositionProtectionClose', [1n]))
    publicClient.getTransactionReceipt = async () => ({ blockNumber: 100n, blockHash: '0xorphan', status: 'success' })
    publicClient.getBlock = async () => ({ number: 120n, hash: '0xcanonical' })
    assert.equal(await worker.reconcileTransaction(), true)
    assert.deepEqual(calls.filter(call => call[0] === 'send'), [['send', raw], ['send', raw]])
    assert.equal(calls.filter(call => call[0] === 'prepare').length, 1)
    assert.equal(calls.some(call => call[0] === 'sql' && call[1].startsWith('UPDATE')), false)
  })
})
