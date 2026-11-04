import fs from 'node:fs'
import test from 'node:test'
import assert from 'node:assert/strict'
import pg from 'pg'
import { ReturnAttributionWorker } from '../worker.mjs'
import { SOURCE_COMMIT } from '../evidence.mjs'
const url = process.env.ATTRIBUTION_TEST_DATABASE_URL
const hash = number => '0x' + BigInt(number).toString(16).padStart(64, '0')
const contracts = Object.fromEntries(['cfdEngine', 'cfdEngineSettlementSidecar', 'orderRouter', 'housePool', 'juniorVault', 'seniorVault', 'terminalNavBookV2'].map((key, i) => [key, { address: '0x' + String(i + 1).padStart(40, '0') }]))
const release = { network: { chainId: 421614 }, release: { deploymentBlock: 2, sourceCommit: SOURCE_COMMIT }, contracts }
async function fixture(run) {
  const db = new pg.Client({ connectionString: url }); await db.connect()
  const schema = 'attribution_' + process.pid + '_' + Math.random().toString(36).slice(2)
  try {
    await db.query(`CREATE SCHEMA "${schema}"`); await db.query(`SET search_path TO "${schema}"`)
    await db.query(fs.readFileSync(new URL('../schema.sql', import.meta.url), 'utf8'))
    let generation = 0, requests = 0, invalid = false
    const client = {
      getBlock: async ({ blockNumber }) => ({ number: blockNumber, hash: hash(blockNumber + BigInt(generation) * 1000n), timestamp: blockNumber }),
      request: async ({ method, params }) => {
        assert.equal(method, 'eth_getLogs'); requests++
        const from = BigInt(params[0].fromBlock), to = BigInt(params[0].toBlock)
        return [{ blockNumber: '0x' + from.toString(16), blockHash: hash(from + BigInt(generation) * 1000n), transactionHash: hash(100),
          address: contracts.housePool.address, topics: [hash(1)], data: '0x', logIndex: '0x0', removed: invalid, ...invalid ? { blockNumber: '0x' + (to + 1n).toString(16) } : {} }]
      },
    }
    const worker = new ReturnAttributionWorker({ db, client, release })
    await run({ db, worker, reorg: () => { generation++ }, invalidate: () => { invalid = true }, requests: () => requests })
  } finally { await db.query(`DROP SCHEMA "${schema}" CASCADE`); await db.end() }
}
test('index restart reuses a committed prefix and appends without duplicates', { skip: !url }, async () => fixture(async ({ db, worker, requests }) => {
  await worker.index(5n); assert.equal(requests(), 1)
  await worker.index(5n); assert.equal(requests(), 1)
  await worker.index(7n)
  assert.equal((await db.query('SELECT count(*) FROM vault_return_logs')).rows[0].count, '2')
  assert.equal((await db.query('SELECT through_block FROM vault_return_index_state')).rows[0].through_block, '7')
}))
test('malformed/noncanonical log batches cannot advance a checkpoint', { skip: !url }, async () => fixture(async ({ db, worker, invalidate }) => {
  await worker.index(5n); invalidate(); await assert.rejects(worker.index(7n), /Noncanonical/)
  assert.equal((await db.query('SELECT through_block FROM vault_return_index_state')).rows[0].through_block, '5')
}))
test('reorgs discard orphaned prefix/evidence and rebuild before reuse', { skip: !url }, async () => fixture(async ({ db, worker, reorg }) => {
  await worker.index(5n)
  await db.query('INSERT INTO vault_return_evidence VALUES($1,$2,$3,$4)', [worker.key, 'orphan', hash(2), '{}'])
  reorg(); await worker.index(5n)
  assert.equal((await db.query('SELECT count(*) FROM vault_return_evidence')).rows[0].count, '0')
  const rows = (await db.query('SELECT block_hash FROM vault_return_logs')).rows
  assert.equal(rows.length, 1); assert.equal(rows[0].block_hash, hash(1002))
}))
test('receipt mismatch cannot cache a settlement trace', { skip: !url }, async () => fixture(async ({ db, worker }) => {
  worker.client.request = async () => ({ transactionHash: hash(99), blockHash: hash(3), blockNumber: '0x3', status: '0x1' })
  await assert.rejects(worker.transactionEvidence(hash(100), hash(3), 3n), /identity\/status/)
  assert.equal((await db.query('SELECT count(*) FROM vault_return_evidence')).rows[0].count, '0')
}))
test('a complete but obsolete history window cannot publish a current report', { skip: !url }, async () => fixture(async ({ db, worker }) => {
  await db.query(`CREATE TABLE vault_performance_snapshots (
    chain_id integer,house_pool_address text,senior_vault_address text,junior_vault_address text,
    epoch_timestamp bigint,block_number numeric,block_hash text,block_timestamp bigint,
    junior_share_price_wad numeric,mark_fresh boolean)`)
  const eligible = Math.floor((Date.now() / 1000 - 120) / 3600) * 3600
  await db.query(`INSERT INTO vault_performance_snapshots
    SELECT $1,$2,$3,$4,$5::bigint-(169-series)*3600,series+1,$6,$5::bigint-(169-series)*3600,1000000000000000000,true
    FROM generate_series(0,168) AS series`,
  [release.network.chainId, worker.address.housePool, worker.address.seniorVault, worker.address.juniorVault, eligible, hash(1)])
  worker.client.getBlock = () => { throw new Error('Obsolete history must be rejected before RPC evidence collection') }
  assert.deepEqual(await worker.run(), { ready: false, reason: 'hourly_history_incomplete' })
  assert.equal((await db.query('SELECT count(*) FROM vault_return_reports')).rows[0].count, '0')
}))
