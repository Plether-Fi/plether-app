import fs from 'node:fs'
import { setTimeout } from 'node:timers/promises'
import pg from 'pg'
import { createPublicClient, createWalletClient, http } from 'viem'
import { arbitrumSepolia } from 'viem/chains'
import { privateKeyToAccount } from 'viem/accounts'
import { ProtectionWorker } from './worker.mjs'
import { json } from './policy.mjs'

function positive(name, fallback) {
  const value = Number(process.env[name] ?? fallback)
  if (!Number.isSafeInteger(value) || value <= 0) throw new Error(`${name} must be a positive integer`)
  return value
}
function flag(name) {
  const value = process.env[name] ?? 'false'
  if (!['true', 'false'].includes(value)) throw new Error(`${name} must be true or false`)
  return value === 'true'
}
function log(level, event, attributes) { console.log(json({ timestamp: new Date().toISOString(), level, event, service: 'plether-position-protection-worker', ...attributes })) }

async function main() {
  const releasePath = process.env.PERPS_RELEASE_MANIFEST ?? new URL('../../../config/perps/arbitrum-sepolia-v2.json', import.meta.url)
  const release = JSON.parse(fs.readFileSync(releasePath, 'utf8'))
  if (!process.env.PERPS_RPC_URL || !process.env.DATABASE_URL) throw new Error('PERPS_RPC_URL and DATABASE_URL are required')
  const executionEnabled = flag('PROTECTION_WORKER_EXECUTION_ENABLED')
  const privateKey = process.env.PROTECTION_WORKER_PRIVATE_KEY
  if (executionEnabled && !privateKey) throw new Error('PROTECTION_WORKER_PRIVATE_KEY is required for execution')
  const account = privateKey ? privateKeyToAccount(privateKey) : { address: release.release.owner, type: 'json-rpc' }
  const transport = http(process.env.PERPS_RPC_URL, { timeout: 15_000, retryCount: 2, fetchOptions: process.env.PERPS_RPC_AUTH_TOKEN ? { headers: { Authorization: `Bearer ${process.env.PERPS_RPC_AUTH_TOKEN}` } } : undefined })
  const publicClient = createPublicClient({ chain: arbitrumSepolia, transport })
  const walletClient = privateKey ? createWalletClient({ account, chain: arbitrumSepolia, transport }) : undefined
  // DATABASE_URL follows pg/libpq syntax. Production requires certificate-verified TLS.
  const pool = new pg.Pool({ connectionString: process.env.DATABASE_URL, max: 1, connectionTimeoutMillis: 10_000 })
  let db
  let stopped = false
  process.on('SIGTERM', () => { stopped = true })
  process.on('SIGINT', () => { stopped = true })
  try {
    db = await pool.connect()
    const schema = fs.readFileSync(new URL('../schema.sql', import.meta.url), 'utf8')
    const protectionDdl = schema.split('-- v1.2.1 Book event history and independent trigger/retry worker checkpoints.')[1]?.split('-- Cached six-feed Pyth update payloads used by reveal payload APIs and keeper execution')[0]
    if (!protectionDdl) throw new Error('Protection migration is missing')
    await db.query(protectionDdl)
    const lock = await db.query('SELECT pg_try_advisory_lock($1,$2) AS acquired', [421614, 121])
    if (!lock.rows[0].acquired) throw new Error('Another protection worker holds the signer/index lease')
    const worker = new ProtectionWorker({ db, publicClient, walletClient, account, release, executionEnabled, log,
      batchBlocks: BigInt(positive('PROTECTION_WORKER_INDEX_BATCH_BLOCKS', 2000)),
      confirmations: BigInt(positive('PROTECTION_WORKER_CONFIRMATIONS', 12)),
      candidateBatch: positive('PROTECTION_WORKER_CANDIDATE_BATCH', 50),
      maxPayloadAge: positive('PROTECTION_WORKER_MAX_PAYLOAD_AGE_SECONDS', 15),
      keeperBatchSize: positive('KEEPER_MAX_BATCH_SIZE', 5), keeperPollSeconds: positive('KEEPER_POLL_SECONDS', 5),
    })
    await worker.verifyDeployment()
    const interval = positive('PROTECTION_WORKER_POLL_SECONDS', 5)
    do {
      try {
        const indexed = await worker.index()
        if (executionEnabled && await publicClient.getBalance({ address: account.address }) < 1_000_000_000_000_000n) log('error', 'protection_signer_low_balance', { account: account.address })
        if (indexed.caughtUp) await worker.evaluate()
        log('info', 'protection_worker_heartbeat', { ...indexed, executionEnabled })
        if (process.argv.includes('--once')) break
        await setTimeout(indexed.caughtUp ? interval * 1000 : 100)
      } catch (error) {
        // RPC errors can embed authorization URLs or request bodies. Log names only.
        log('error', 'protection_worker_failed', { error: error.name })
        if (process.argv.includes('--once')) throw error
        await setTimeout(interval * 1000)
      }
    } while (!stopped)
  } finally { db?.release(); await pool.end() }
}

// Transport and database errors may contain credentials. Never print raw errors.
await main().catch(error => {
  log('error', 'protection_worker_startup_failed', { error: error.name })
  process.exitCode = 1
})
