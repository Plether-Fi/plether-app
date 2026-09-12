import fs from 'node:fs'
import { createHash } from 'node:crypto'
import { setTimeout } from 'node:timers/promises'
import pg from 'pg'
import { createPublicClient, http } from 'viem'
import { parseMonitors, observeFunding } from './funding.mjs'

// This process receives public signer addresses, never worker private keys.
const incidents = new Map()
function report(component, state, reason) {
  const now = performance.now(), key = `${state}:${reason}`, prior = incidents.get(component)
  if (prior?.key === key) {
    prior.count++
    if (now-prior.at < 60_000 || key === 'ready:READY') return
  }
  console.log(JSON.stringify({ level: state === 'ready' && reason === 'READY' ? 'info' : 'warn',
    event: 'worker_funding_observation', component, outcome: state, reason_code: reason,
    occurrence_count: prior?.key === key ? prior.count : 1 }))
  incidents.set(component, { key, at: now, count: 0 })
}

async function main() {
  const rawInventory = process.env.AA_FUNDING_MONITORS ?? '[]'
  const monitors = parseMonitors(rawInventory)
  const inventoryId = createHash('sha256').update(rawInventory).digest('hex')
  const release = JSON.parse(fs.readFileSync(process.env.PERPS_RELEASE_MANIFEST ?? '/app/config/perps/arbitrum-sepolia-v2.json','utf8'))
  if (!process.env.DATABASE_URL || !process.env.PERPS_RPC_URL) throw new Error('Funding dependencies missing')
  const client = createPublicClient({ transport: http(process.env.PERPS_RPC_URL, {
    timeout: 3000, retryCount: 0,
    fetchOptions: process.env.PERPS_RPC_AUTH_TOKEN ? { headers: { Authorization: `Bearer ${process.env.PERPS_RPC_AUTH_TOKEN}` } } : undefined,
  }) })
  const pool = new pg.Pool({ connectionString: process.env.DATABASE_URL, max: 5, connectionTimeoutMillis: 3000, query_timeout: 3000, statement_timeout: 3000 })
  // pg emits idle-connection errors outside query promises. Do not let Node's
  // default unhandled-error printer expose raw connection details.
  pool.on('error', () => { report('funding','unknown','FUNDING_UNVERIFIED') })
  let stopped = false, previous
  process.on('SIGTERM', () => { stopped = true })
  process.on('SIGINT', () => { stopped = true })
  const lease = await pool.connect()
  lease.on('error', () => { stopped = true; report('funding','unknown','FUNDING_UNVERIFIED') })
  try {
    const lock = await lease.query('SELECT pg_try_advisory_lock(hashtext($1),hashtext($2)) AS acquired', ['aa-funding-observer',`${release.network.chainId}:${release.contracts.orderRouter.address.toLowerCase()}`])
    if (!lock.rows[0]?.acquired) throw new Error('Funding observer already running')
    do {
      const started = performance.now(), observedAt = new Date()
      let snapshot
      try { snapshot = await observeFunding({ client, db: pool, release, monitors, previous }); previous = snapshot.block }
      catch { snapshot = { results: monitors.map(m => ({ ...m, state: 'unknown', reason: 'FUNDING_UNVERIFIED' })) } }
      // One transaction publishes the entire inventory; removed/changed signers
      // cannot linger as fresh evidence, and a partial write never means ready.
      const db = lease
      try {
        await db.query('BEGIN')
        await db.query('DELETE FROM aa_funding_observations WHERE chain_id=$1 AND deployment=$2', [release.network.chainId,release.contracts.orderRouter.address.toLowerCase()])
        for (const m of snapshot.results) {
          // Network time cannot extend the observation's validity.
          if (performance.now()-started > 10_000) { m.state = 'unknown'; m.reason = 'FUNDING_UNVERIFIED' }
          await db.query(`INSERT INTO aa_funding_observations(chain_id,deployment,component,signer_address,state,reason,balance_wei,liability_wei,reserve_wei,observed_at,inventory_id)
            VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11)`, [release.network.chainId,release.contracts.orderRouter.address.toLowerCase(),m.component,m.address,m.state,m.reason,
            m.balance?.toString() ?? null,m.liability?.toString() ?? null,m.reserve?.toString() ?? null,observedAt,inventoryId])
        }
        await db.query('COMMIT')
      } catch (error) { await db.query('ROLLBACK'); throw error }
      // Aggregate executor pools conservatively; one unknown executor cannot be
      // concealed by a funded peer which Alto may not select for the next bundle.
      for (const component of new Set(monitors.map(m => m.component))) {
        const rows = snapshot.results.filter(m => m.component === component)
        const worst = rows.every(m => m.state === 'blocked') ? rows[0]
          : rows.some(m => m.state === 'blocked') ? { state: 'unknown', reason: 'FUNDING_LOW' }
          : rows.find(m => m.state === 'unknown') ?? rows.find(m => m.reason === 'FUNDING_LOW') ?? rows[0]
        report(component,worst.state,worst.reason)
      }
      if (process.argv.includes('--once')) break
      await setTimeout(Math.max(0,10_000-(performance.now()-started)))
    } while (!stopped)
  } finally { lease.release(true); await pool.end() }
}

await main().catch(() => {
  // Raw RPC/DB exceptions can contain URLs, credentials and signed bytes.
  console.log(JSON.stringify({ level: 'error', event: 'worker_funding_monitor_failed', component: 'funding', reason_code: 'FUNDING_UNVERIFIED', outcome: 'unknown' }))
  process.exitCode = 1
})
