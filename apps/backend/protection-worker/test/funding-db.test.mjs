import { before, after, it } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import pg from 'pg'
import { privateKeyToAccount } from 'viem/accounts'
import { journalLiabilities, pendingLiability } from '../funding.mjs'

const url = process.env.PROTECTION_TEST_DATABASE_URL
const schema = `funding_test_${process.pid}_${Date.now()}`
let db
before(async () => {
  if (!url) return
  db = new pg.Client({ connectionString: url }); await db.connect()
  await db.query(`CREATE SCHEMA ${schema}`); await db.query(`SET search_path TO ${schema}`)
  const sql = fs.readFileSync(new URL('../../config/migrations/aa-funding-v1.sql',import.meta.url),'utf8')
  await db.query(sql); await db.query(sql)
  const baseline = fs.readFileSync(new URL('../../schema.sql',import.meta.url),'utf8')
  await db.query(baseline.split('-- v1.2.1 Book event history and independent trigger/retry worker checkpoints.')[1].split('-- Cached six-feed Pyth update payloads')[0])
})
after(async () => { if (db) { await db.query(`DROP SCHEMA ${schema} CASCADE`); await db.end() } })

it('publishes scoped evidence atomically and rejects invalid database categories', {skip:!url}, async () => {
  const insert = `INSERT INTO aa_funding_observations(chain_id,deployment,inventory_id,component,signer_address,state,reason,observed_at)
    VALUES(421614,'router',$1,'alto',$2,$3,$4,clock_timestamp())`
  const address='0x1111111111111111111111111111111111111111', inventory='a'.repeat(64)
  await db.query(insert,[inventory,address,'ready','READY'])
  await assert.rejects(db.query(insert,[inventory,'0x2222222222222222222222222222222222222222','ready','RAW_SECRET']),e=>e.code==='23514')
  const result=await db.query(`SELECT component,state,reason FROM aa_funding_observations WHERE chain_id=421614 AND deployment='router' AND inventory_id=$1 AND observed_at>clock_timestamp()-interval '15 seconds'`,[inventory])
  assert.deepEqual(result.rows,[{component:'alto',state:'ready',reason:'READY'}])
  await db.query('BEGIN'); await db.query('DELETE FROM aa_funding_observations'); await db.query('ROLLBACK')
  assert.equal((await db.query('SELECT count(*) FROM aa_funding_observations')).rows[0].count,'1')
  assert.equal((await db.query('SELECT count(*) FROM aa_funding_observations WHERE inventory_id=$1',['b'.repeat(64)])).rows[0].count,'0')
})

it('accounts for actual signed protection recovery bytes without replacing or removing them', {skip:!url}, async () => {
  // Public test scalar, never used outside this disposable local database.
  const account=privateKeyToAccount(`0x${'1'.repeat(64)}`)
  const raw=await account.signTransaction({chainId:421614,type:'eip1559',nonce:7,gas:100000n,maxFeePerGas:5n,maxPriorityFeePerGas:1n,to:account.address,value:100n})
  const release={contracts:{positionProtectionBook:{address:account.address}}}
  await db.query("INSERT INTO perps_protection_transactions(chain_id,book,transaction_hash,raw_transaction,protection_id,action,status) VALUES(421614,$1,'hash',$2,1,'retry','pending')",[account.address.toLowerCase(),raw])
  const rows=await journalLiabilities(db,421614,release,{component:'protection',address:account.address.toLowerCase()})
  assert.deepEqual(rows,[{nonce:7n,cost:500100n}])
  assert.equal(pendingLiability(7n,8n,rows),500100n)
  assert.equal(pendingLiability(8n,8n,rows),0n) // canonical balance already includes it
  assert.equal((await db.query('SELECT raw_transaction FROM perps_protection_transactions')).rows[0].raw_transaction,raw)
})
