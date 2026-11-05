import fs from 'node:fs'
import { setTimeout } from 'node:timers/promises'
import pg from 'pg'
import { createPublicClient, http } from 'viem'
import { ReturnAttributionWorker } from './worker.mjs'
import { json, AttributionError } from './accounting.mjs'

function log(level, event, attributes) {
  console.log(json({ timestamp: new Date().toISOString(), level, event, service: 'vault-return-attribution', ...attributes }))
}
function failure(error) {
  // Provider and database errors can contain credentials in URLs/request bodies.
  return { error: error.name, ...(error instanceof AttributionError ? { reason: error.message } : {}) }
}
async function main() {
  const release = JSON.parse(fs.readFileSync(process.env.PERPS_RELEASE_MANIFEST ?? (fs.existsSync('/app/config/perps/arbitrum-sepolia-v2.0.0.json') ? '/app/config/perps/arbitrum-sepolia-v2.0.0.json' : new URL('../../../config/perps/arbitrum-sepolia-v2.0.0.json', import.meta.url))))
  const expected = JSON.parse(process.env.VAULT_RETURN_DEPLOYMENT ?? '{}')
  for (const [name, address] of Object.entries(expected)) {
    if (release.contracts[name]?.address.toLowerCase() !== address.toLowerCase()) throw new AttributionError('Vault history deployment does not match attribution release')
  }
  const rpc = process.env.VAULT_RETURN_ATTRIBUTION_RPC_URL ?? process.env.PERPS_RPC_URL
  if (!rpc || !process.env.DATABASE_URL) throw new AttributionError('PERPS_RPC_URL and DATABASE_URL are required')
  const auth = process.env.VAULT_RETURN_ATTRIBUTION_RPC_AUTH_TOKEN ?? process.env.PERPS_RPC_AUTH_TOKEN
  const client = createPublicClient({ transport: http(rpc, { timeout: 30_000, retryCount: 2,
    fetchOptions: auth ? { headers: { Authorization: `Bearer ${auth}` } } : undefined }) })
  const pool = new pg.Pool({ connectionString: process.env.DATABASE_URL, max: 1, connectionTimeoutMillis: 10_000 })
  const stop = new AbortController()
  process.on('SIGTERM', () => stop.abort())
  process.on('SIGINT', () => stop.abort())
  let db
  try {
    db = await pool.connect()
    const worker = new ReturnAttributionWorker({ db, client, release, log })
    if (!await worker.init()) { log('info', 'vault_return_leader_busy', {}); return }
    do {
      try { log('info', 'vault_return_cycle', await worker.run()) }
      catch (error) { log('error', 'vault_return_cycle_failed', failure(error)); if (process.argv.includes('--once')) throw error }
      if (process.argv.includes('--once')) break
      await setTimeout(30_000, undefined, { signal: stop.signal }).catch(error => { if (error.name !== 'AbortError') throw error })
    } while (!stop.signal.aborted)
  } finally { db?.release(); await pool.end() }
}
await main().catch(error => { log('error', 'vault_return_startup_failed', failure(error)); process.exitCode = 1 })
