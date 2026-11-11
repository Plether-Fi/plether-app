import fs from 'node:fs'
import test from 'node:test'
import assert from 'node:assert/strict'
import pg from 'pg'
import { ReturnAttributionWorker } from '../worker.mjs'
import { SOURCE_COMMIT } from '../evidence.mjs'
import { validateRequest } from '../../../../infra/rpc/gateway/server.mjs'
const url = process.env.ATTRIBUTION_TEST_DATABASE_URL
const hash = number => '0x' + BigInt(number).toString(16).padStart(64, '0')
const contracts = Object.fromEntries(['cfdEngine', 'cfdEngineSettlementSidecar', 'orderRouter', 'housePool', 'juniorVault', 'seniorVault', 'terminalNavBookV2', 'pletherOracle'].map((key, i) => [key, { address: '0x' + String(i + 1).padStart(40, '0') }]))
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
test('settlement traces satisfy the deployed RPC gateway timeout policy before caching', { skip: !url }, async () => fixture(async ({ db, worker }) => {
  const trace = { type: 'CALL', from: contracts.housePool.address, to: contracts.juniorVault.address }
  worker.client.request = async ({ method, params }) => {
    validateRequest({ jsonrpc: '2.0', id: 1, method, params })
    return method === 'eth_getTransactionReceipt'
      ? { transactionHash: hash(100), blockHash: hash(3), blockNumber: '0x3', status: '0x1', logs: [] }
      : trace
  }
  assert.deepEqual((await worker.transactionEvidence(hash(100), hash(3), 3n)).trace, trace)
  assert.equal((await db.query('SELECT count(*) FROM vault_return_evidence')).rows[0].count, '1')
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

async function currentHistory(db, worker, staleIndices) {
  await db.query(`CREATE TABLE vault_performance_snapshots (
    chain_id integer,house_pool_address text,senior_vault_address text,junior_vault_address text,
    epoch_timestamp bigint,block_number numeric,block_hash text,block_timestamp bigint,
    junior_share_price_wad numeric,mark_fresh boolean)`)
  const eligible = Math.floor((Date.now() / 1000 - 120) / 3600) * 3600
  for (let index = 0; index < 169; index++) {
    const epoch = eligible - (168 - index) * 3600, block = index + 2
    await db.query('INSERT INTO vault_performance_snapshots VALUES($1,$2,$3,$4,$5,$6,$7,$5,$8,$9)',
      [release.network.chainId, worker.address.housePool, worker.address.seniorVault, worker.address.juniorVault,
        epoch, block, hash(block), '1000000000000000000', !staleIndices.includes(index)])
  }
  const timestamp = number => BigInt(eligible) - (170n - number) * 3600n
  worker.client.getBlock = async ({ blockNumber }) => ({ number: blockNumber, hash: hash(blockNumber), timestamp: timestamp(blockNumber) })
  worker.snapshot = async block => {
    const markFresh = !staleIndices.includes(Number(block - 2n))
    const assets = markFresh ? 999999n : 799999n
    return { block, blockHash: hash(block), timestamp: timestamp(block), assets, supply: 999999000n,
      rawSupply: 999999000n, sharePrice: (assets + 1n) * 10n ** 21n / 1000000000n, rawSenior: 0n,
      rawJunior: assets, highWaterMark: 0n, couponCheckpoint: 0n, seniorRateBps: 0n, terminalPnl: 0n, markFresh }
  }
}

test('publishes a matching report across stale intermediate hours without attributing fallback losses', { skip: !url }, async () => fixture(async ({ db, worker }) => {
  await currentHistory(db, worker, [106, 107])
  assert.equal((await worker.run()).ready, true)
  const [{ attribution }] = (await db.query('SELECT attribution FROM vault_return_reports')).rows
  assert.equal(attribution.netReturnUsdg, '0')
  assert.ok(Object.values(attribution.components).every(component => component.amountUsdg === '0' && component.contributionPp === 0))
  assert.ok(attribution.notes.some(note => note.includes('temporarily stale')))
  assert.equal((await worker.run()).cached, true)
}))

for (const index of [0, 168]) test(`rejects a stale reporting endpoint at index ${index}`, { skip: !url }, async () => fixture(async ({ db, worker }) => {
  await currentHistory(db, worker, [index])
  worker.client.getBlock = () => { throw new Error('Stale boundaries must be rejected before RPC evidence collection') }
  assert.deepEqual(await worker.run(), { ready: false, reason: 'stale_period_boundaries' })
  assert.equal((await db.query('SELECT count(*) FROM vault_return_reports')).rows[0].count, '0')
}))
