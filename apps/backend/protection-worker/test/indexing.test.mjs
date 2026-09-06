import { after, before, it } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import pg from 'pg'
import { ProtectionWorker } from '../worker.mjs'

const url = process.env.PROTECTION_TEST_DATABASE_URL
const schema = `protection_test_${process.pid}_${Date.now()}`
let db
before(async () => {
  if (!url) return
  db = new pg.Client({ connectionString: url })
  await db.connect()
  await db.query(`CREATE SCHEMA ${schema}`)
  await db.query(`SET search_path TO ${schema}`)
  const ddl = fs.readFileSync(new URL('../../schema.sql', import.meta.url), 'utf8').split('-- v1.2.1 Book event history and independent trigger/retry worker checkpoints.')[1].split('-- Cached six-feed Pyth update payloads')[0]
  await db.query(ddl)
  await db.query(ddl) // Restart-safe migration.
})
after(async () => {
  if (!db) return
  await db.query(`DROP SCHEMA ${schema} CASCADE`)
  await db.end()
})

it('indexes ordered lifecycle events atomically, resumes, and rewinds a reorg in PostgreSQL', { skip: !url }, async () => {
  const book = '0x1111111111111111111111111111111111111111'
  const account = '0x2222222222222222222222222222222222222222'
  const release = { network: { chainId: 421614 }, release: { deploymentBlock: 100 }, contracts: Object.fromEntries(['positionProtectionBook', 'orderRouter', 'orderLifecycleBook', 'pletherOracle'].map(name => [name, { address: book }])) }
  let generation = 0
  let mismatch = false
  const hash = n => `0x${(Number(n) + generation * 1000).toString(16).padStart(64, '0')}`
  const state = { protectionId: 1n, account, status: 2, side: 0, triggeredLeg: 0, takeProfitTriggerPrice: 90n, stopLossTriggerPrice: 110n }
  const publicClient = {
    getBlock: async ({ blockNumber }) => ({ number: blockNumber ?? 115n, hash: hash(blockNumber ?? 115n) }),
    getLogs: async ({ fromBlock, toBlock }) => fromBlock <= 101n && toBlock >= 101n ? [1, 0].map(logIndex => ({ address: book, blockNumber: 101n, blockHash: mismatch ? '0xbad' : hash(101), logIndex, transactionHash: hash(999), eventName: logIndex === 0 ? 'PositionProtectionCreated' : 'PositionProtectionArmed', args: { protectionId: 1n, account } })) : [],
    readContract: async () => state,
  }
  const worker = new ProtectionWorker({ db, publicClient, account: { address: book }, release, log: () => {}, batchBlocks: 2n })
  assert.deepEqual(await worker.index(), { caughtUp: false, lagBlocks: 2n })
  assert.deepEqual((await db.query('SELECT event_name FROM perps_protection_events ORDER BY log_index')).rows.map(r => r.event_name), ['PositionProtectionCreated', 'PositionProtectionArmed'])
  assert.deepEqual(await worker.index(), { caughtUp: true, lagBlocks: 0n })
  assert.deepEqual(await worker.index(), { caughtUp: true, lagBlocks: 0n })
  assert.equal((await db.query('SELECT count(*) FROM perps_protection_events')).rows[0].count, '2')
  // All durable boundaries on the old branch become non-canonical.
  await db.query("INSERT INTO perps_protection_observations(chain_id,book,protection_id,checked_block,checked_block_hash,observation) VALUES(421614,$1,1,103,$2,'{}')", [book, hash(103)])
  generation = 1
  state.status = 6
  await worker.index()
  assert.equal((await db.query('SELECT count(*) FROM perps_protection_checkpoints')).rows[0].count, '1')
  assert.equal((await db.query('SELECT snapshot FROM perps_protection_events LIMIT 1')).rows[0].snapshot.statusName, 'Cancelled')
  assert.equal((await db.query('SELECT block_hash FROM perps_protection_events LIMIT 1')).rows[0].block_hash, hash(101))
  assert.equal((await db.query('SELECT count(*) FROM perps_protection_observations')).rows[0].count, '0')
  // A malformed/racing RPC range cannot advance the checkpoint or persist logs.
  generation = 2
  mismatch = true
  await assert.rejects(worker.index(), /block hash mismatch/)
  assert.equal((await db.query('SELECT count(*) FROM perps_protection_checkpoints')).rows[0].count, '0')
  assert.equal((await db.query('SELECT count(*) FROM perps_protection_events')).rows[0].count, '0')
})

it('enforces one pending signed transaction per Book, while retaining included receipts', { skip: !url }, async () => {
  const insert = "INSERT INTO perps_protection_transactions(chain_id,book,transaction_hash,raw_transaction,protection_id,action,status) VALUES(421614,'book',$1,'0x1234',1,'retry',$2)"
  await db.query(insert, ['first', 'pending'])
  await assert.rejects(db.query(insert, ['second', 'pending']), error => error.code === '23505')
  await db.query("UPDATE perps_protection_transactions SET status='included' WHERE transaction_hash='first'")
  await db.query(insert, ['second', 'pending'])
  assert.equal((await db.query("SELECT count(*) FROM perps_protection_transactions WHERE status IN ('pending','included')")).rows[0].count, '2')
})
