import assert from 'node:assert/strict'
import { test } from 'node:test'
import { EventEmitter } from 'node:events'
import { execFileSync } from 'node:child_process'
import { readFileSync } from 'node:fs'
import { databaseEnvironment, inspectJournal, JOURNAL_SQL, parseExecutorMetrics, readJournal, REQUIRED_CHECKS, runDrainQualification } from './qualify-drain.mjs'

const BASE = 1794230000000
const HASH = number => `0x${number.toString(16).padStart(64, '0')}`
const PAYMASTER = '0x9761091045616a388f5fe1433721b272c78fe31b'
const ENTRYPOINT = '0x4337084d9e255ff0702461cf8895ce9e3b5ff108'
const TOPIC = '0x49628fd1471006c1482da88028e9ce4dbb080b815c9b0344d39e5a8e6ec1419f'
const SENDER = `0x${'a'.repeat(40)}`
const OWNER = `0x${'b'.repeat(40)}`
const ENV = {
  PLETHER_ALTO_QUALIFY_MODE: 'journal_idle', PERPS_CHAIN_ID: '421614', AA_PAYMASTER_ADDRESS: PAYMASTER,
  AA_RECONCILER_MAX_SAFE_LAG_SECONDS: '1800',
  DATABASE_URL: 'postgresql://YOUR_USER:YOUR_PASSWORD@database.internal/plether?sslmode=require',
  PERPS_RPC_URL: 'https://rpc-internal.sepolia.plether.com/', PERPS_RPC_AUTH_TOKEN: 'secret-gateway-token-'.repeat(3),
  PLETHER_ALTO_BACKUP_RPC_URL: 'https://arb-sepolia.g.alchemy.com/v2/YOUR_KEY',
  PLETHER_ALTO_BASELINE_URL: 'http://internal-plether-sepolia-alto-example.ap-southeast-1.elb.amazonaws.com/',
}
const clone = value => structuredClone(value)
const epoch = time => String(Math.floor(time))
const hex = number => `0x${number.toString(16)}`
const word = value => BigInt(value).toString(16).padStart(64, '0')
function authorization(id, state) {
  const submitted = ['settled', 'expired', 'submitted'].includes(state)
  const signed = !['reserved', 'cancelled'].includes(state)
  return { digest: HASH(id), request_key: HASH(id + 100), expected_hash: signed ? HASH(id + 200) : null,
    user_hash: submitted ? HASH(id + 200) : null, sender: SENDER, owner: OWNER, nonce: String(id),
    valid_after: epoch(BASE / 1000 - 400), valid_until: epoch(BASE / 1000 - 200), max_cost: '1000', state,
    signature_present: signed, signature_valid: signed, signed_at_ms: signed ? epoch(BASE - 300000) : null,
    submitted_at_ms: submitted ? epoch(BASE - 250000) : null,
    settled_at_ms: ['settled', 'expired', 'cancelled'].includes(state) ? epoch(BASE - 50000) : null,
    operation_object: true, operation_sender: SENDER, operation_nonce: hex(id), operation_paymaster: null,
    operation_chain_id: null, operation_entry_point: null }
}
function logFor(row) {
  return { address: ENTRYPOINT, topics: [TOPIC, row.expected_hash, `0x${'0'.repeat(24)}${SENDER.slice(2)}`, `0x${'0'.repeat(24)}${PAYMASTER.slice(2)}`],
    data: `0x${word(row.nonce)}${word(1)}${word(100)}${word(900)}`,
    transactionHash: HASH(400), blockNumber: hex(500), blockHash: HASH(500), logIndex: '0x2', removed: false }
}
function snapshot(now) {
  const settled = authorization(1, 'settled')
  return { read_only: 'on', isolation: 'repeatable read', snapshot_at_ms: epoch(now),
    authorizations: [settled, authorization(2, 'expired'), authorization(3, 'signed'), authorization(4, 'reserved')],
    events: [{ digest: settled.digest, user_hash: settled.expected_hash, transaction_hash: HASH(400),
      block_number: '500', block_hash: HASH(500), success: true, actual_cost: '100', event: logFor(settled), finalized_at_ms: epoch(BASE - 50000) }],
    cursors: [{ chain_id: '421614', paymaster: PAYMASTER, safe_block: '600', safe_block_hash: HASH(600), updated_at_ms: epoch(now - 1000) }],
    health: [{ chain_id: '421614', paymaster: PAYMASTER, safe_block: '600', safe_block_hash: HASH(600), last_success_at_ms: epoch(now - 1000) }] }
}
function metrics(outstanding = 187, started = 1793728822) {
  return `alto_user_operations_in_mempool_count{status="outstanding"} ${outstanding}\nalto_executor_wallets_available_count 4\nalto_executor_wallets_total_count 4\nprocess_start_time_seconds ${started}\n`
}
function fixture(options = {}) {
  let now = BASE
  const events = []
  let databaseCalls = 0
  let metricCalls = 0
  function execFileImpl(command, args, config, callback) {
    events.push({ type: 'psql', command, args, config })
    const stdin = new EventEmitter()
    stdin.end = sql => {
      assert.equal(sql, JOURNAL_SQL)
      const raw = snapshot(now)
      options.snapshot?.(raw, databaseCalls++)
      queueMicrotask(() => callback(options.databaseError ?? null, options.databaseText ?? JSON.stringify(raw)))
    }
    return { stdin }
  }
  function header(number) {
    const timestamp = number === 900 ? now / 1000 - 1 : BASE / 1000 - (number === 700 ? 60 : number === 600 ? 100 : 200)
    return { number: hex(number), hash: HASH(number), timestamp: hex(Math.floor(timestamp)) }
  }
  async function fetchImpl(url, init) {
    const parsed = new URL(url)
    if (parsed.hostname.startsWith('internal-plether-sepolia-alto-')) {
      if (init.method === 'POST') {
        events.push({ type: 'barrier' })
        return new Response(JSON.stringify(options.barrierBody ?? { error: 'alto_rpc_cutover_in_progress' }), { status: options.barrierStatus ?? 503 })
      }
      events.push({ type: 'metrics' })
      const index = metricCalls++
      return new Response(options.metrics?.(index) ?? metrics(), { status: 200, headers: { date: new Date(now + (options.metricAge ?? 0)).toUTCString() } })
    }
    const request = JSON.parse(init.body)
    const provider = parsed.hostname === 'arb-sepolia.g.alchemy.com' ? 1 : 0
    events.push({ type: 'rpc', provider, method: request.method, signal: init.signal })
    if (!provider) {
      assert.equal(parsed.pathname, '/primary')
      assert.equal(init.headers.Authorization, `Bearer ${ENV.PERPS_RPC_AUTH_TOKEN}`)
    } else assert.equal(init.headers.Authorization, undefined)
    let result
    if (request.method === 'eth_chainId') result = '0x66eee'
    else if (request.method === 'eth_syncing') result = false
    else if (request.method === 'eth_getBlockByNumber') {
      const tag = request.params[0]
      result = header(tag === 'latest' ? 900 : tag === 'safe' ? 700 : Number(BigInt(tag)))
    } else if (request.method === 'eth_getTransactionReceipt') {
      const log = logFor(authorization(1, 'settled'))
      result = { transactionHash: log.transactionHash, blockNumber: log.blockNumber, blockHash: log.blockHash, status: '0x1', logs: [log] }
    } else throw new Error('unexpected_mock_method')
    const changed = options.rpc?.({ provider, request, result })
    if (changed !== undefined) result = changed
    return new Response(JSON.stringify({ jsonrpc: '2.0', id: request.id, result }), {
      status: 200, headers: { 'x-plether-rpc-upstream': options.upstream ?? 'primary' },
    })
  }
  return { events, run: extra => runDrainQualification({ env: ENV, fetchImpl, execFileImpl, now: () => now,
    sleep: async ms => { events.push({ type: 'sleep', ms }); now += ms }, ...extra }) }
}

test('complete proof uses both providers, final full reread, real barrier grace and two fresh wallet samples', async () => {
  const mock = fixture()
  const result = await mock.run()
  assert.equal(result.required_checks_complete, true)
  assert.deepEqual(result.tests, REQUIRED_CHECKS.map(name => ({ name, pass: true })))
  assert.equal(result.scope, 'journal_idle')
  assert.equal(result.idle_sample_count, 2)
  assert.equal(result.snapshot_count, 2)
  assert.equal(result.journal_read_count, 3)
  assert.equal(result.unresolved_count, 0)
  assert.equal(result.submitted_count, 2)
  assert.equal(result.canonical_settlement_count, 1)
  assert.equal(result.outstanding_gauge_last, 187)
  assert.deepEqual(mock.events.filter(event => event.type === 'sleep').map(event => event.ms), [22000, 5000])
  assert.deepEqual(mock.events.slice(0, 3).map(event => event.type), ['barrier', 'sleep', 'barrier'])
  for (const provider of [0, 1]) assert.equal(mock.events.filter(event => event.type === 'rpc' && event.provider === provider && event.method === 'eth_getTransactionReceipt').length, 2)
  const lastRead = mock.events.findLastIndex(event => event.type === 'psql')
  assert(lastRead > mock.events.findLastIndex(event => event.type === 'rpc' && event.method === 'eth_getTransactionReceipt'))
  assert.equal(mock.events.at(-1).type, 'metrics')
  assert(mock.events.filter(event => event.type === 'rpc').every(event => event.signal.aborted))
})

test('psql contract is uncapped, repeatable-read and read-only with secret confined to child env', async () => {
  assert.match(JOURNAL_SQL, /^BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY;/)
  assert.match(JOURNAL_SQL, /ROLLBACK;\s*$/)
  assert.doesNotMatch(JOURNAL_SQL, /\b(INSERT|UPDATE|DELETE|TRUNCATE|CREATE|ALTER|LIMIT|FOR UPDATE)\b/i)
  for (const name of ['aa_sponsorship_authorizations', 'aa_user_operation_events', 'aa_reconciler_cursor', 'aa_reconciler_health']) assert.match(JOURNAL_SQL, new RegExp(`FROM ${name}\\b`))
  const mock = fixture()
  await mock.run()
  for (const child of mock.events.filter(event => event.type === 'psql')) {
    assert.equal(child.command, '/usr/bin/psql')
    assert(child.args.includes('-X') && child.args.includes('--no-password'))
    assert(!JSON.stringify(child.args).includes('private'))
    assert.equal(child.config.env.PGDATABASE, 'plether')
    assert.equal(child.config.env.PGHOST, 'database.internal')
    assert.equal(child.config.env.PGPORT, '5432')
    assert.equal(child.config.env.PGUSER, 'YOUR_USER')
    assert.equal(child.config.env.PGPASSWORD, 'YOUR_PASSWORD')
    assert.equal(child.config.env.PGSSLMODE, 'require')
    assert.match(child.config.env.PGOPTIONS, /default_transaction_read_only=on/)
    assert.match(child.config.env.PGOPTIONS, /statement_timeout=15000/)
    assert.deepEqual(Object.keys(child.config.env).sort(), ['PATH', 'PGAPPNAME', 'PGCONNECT_TIMEOUT', 'PGDATABASE', 'PGHOST', 'PGOPTIONS', 'PGPASSWORD', 'PGPORT', 'PGSSLMODE', 'PGUSER'])
    assert.equal(child.config.timeout, 20000)
    assert.equal(child.config.killSignal, 'SIGKILL')
  }
})

test('libpq env decodes credentials and database while preserving verify-full root certificate and timeout', () => {
  const values = databaseEnvironment('postgresql://private%40user:p%40ss%25word@database.internal:5433/plether%2Ddb?sslmode=verify-full&sslrootcert=%2Fetc%2Fssl%2Froot%20cert.pem&connect_timeout=7&channel_binding=require')
  assert.equal(values.PGUSER, 'private@user')
  assert.equal(values.PGPASSWORD, 'p@ss%word')
  assert.equal(values.PGDATABASE, 'plether-db')
  assert.equal(values.PGHOST, 'database.internal')
  assert.equal(values.PGPORT, '5433')
  assert.equal(values.PGSSLMODE, 'verify-full')
  assert.equal(values.PGSSLROOTCERT, '/etc/ssl/root cert.pem')
  assert.equal(values.PGCONNECT_TIMEOUT, '7')
  assert.equal(values.PGCHANNELBINDING, 'require')
  for (const query of ['options=-c%20default_transaction_read_only%3Doff', 'host=elsewhere', 'sslmode=require&sslmode=disable', 'sslmode=invalid']) {
    assert.throws(() => databaseEnvironment(`postgresql://u:p@database.internal/plether?${query}`))
  }
  assert.throws(() => databaseEnvironment('postgresql://u:%00@database.internal/plether'))
})

test('zero or stale-positive outstanding cannot bypass an unresolved durable submission', async () => {
  for (const gauge of [0, 187]) {
    const result = await fixture({ metrics: () => metrics(gauge), snapshot: raw => raw.authorizations.push(authorization(10, 'submitted')) }).run()
    assert.equal(result.required_checks_complete, false)
    assert.equal(result.tests.find(test => test.name === 'journal_terminal').pass, false)
  }
})

test('every impossible authorization combination fails closed, including rows outside submitted_at', () => {
  const mutations = [
    raw => { raw.authorizations[0].expected_hash = null },
    raw => { raw.authorizations[0].user_hash = HASH(999) },
    raw => { raw.authorizations[0].signature_valid = false },
    raw => { raw.authorizations[0].signed_at_ms = null },
    raw => { raw.authorizations[0].valid_after = raw.authorizations[0].valid_until },
    raw => { raw.authorizations[0].operation_paymaster = OWNER },
    raw => { raw.authorizations[0].operation_chain_id = '1' },
    raw => { raw.authorizations[2].state = 'submitted' },
    raw => { raw.authorizations[3].user_hash = HASH(999) },
    raw => { raw.authorizations[1].submitted_at_ms = null },
    raw => { raw.authorizations[2].user_hash = raw.authorizations[2].expected_hash },
    raw => { raw.authorizations[0].settled_at_ms = String(BASE + 6000) },
  ]
  for (const mutation of mutations) {
    const raw = snapshot(BASE)
    mutation(raw)
    assert.throws(() => inspectJournal(raw, BASE))
  }
  const direct = snapshot(BASE)
  direct.authorizations[0].submitted_at_ms = null
  assert.equal(inspectJournal(direct, BASE).settled.length, 1)
})

test('terminal expired needs strict safe-time expiry and no event by either identity', async () => {
  for (const mutation of [
    raw => { raw.authorizations[1].valid_until = epoch(BASE / 1000 - 100) },
    raw => { raw.events.push({ ...clone(raw.events[0]), digest: raw.authorizations[1].digest, user_hash: raw.authorizations[1].expected_hash }) },
    raw => { raw.authorizations[1].expected_hash = raw.authorizations[0].expected_hash; raw.authorizations[1].user_hash = raw.authorizations[0].expected_hash },
  ]) assert.equal((await fixture({ snapshot: mutation }).run()).required_checks_complete, false)
})

test('settled evidence requires finalized exact DB event, no strays, no duplicates and safe boundary', async () => {
  for (const mutation of [
    raw => { raw.events[0].finalized_at_ms = null },
    raw => { raw.events[0].digest = HASH(999) },
    raw => { raw.events[0].user_hash = HASH(999) },
    raw => { raw.events[0].block_number = '601' },
    raw => { raw.events.push(clone(raw.events[0])) },
    raw => { raw.events = [] },
  ]) assert.equal((await fixture({ snapshot: mutation }).run()).required_checks_complete, false)
})

test('canonical settlements reject either provider mismatch, missing/duplicate event, wrong emitter/paymaster, receipt status and stale forks', async () => {
  const mutations = [
    ({ provider, request, result }) => { if (provider && request.method === 'eth_getTransactionReceipt') return null },
    ({ provider, request, result }) => { if (provider && request.method === 'eth_getTransactionReceipt') { result.logs = []; return result } },
    ({ request, result }) => { if (request.method === 'eth_getTransactionReceipt') { result.logs.push(clone(result.logs[0])); return result } },
    ({ request, result }) => { if (request.method === 'eth_getTransactionReceipt') { result.status = '0x0'; return result } },
    ({ request, result }) => { if (request.method === 'eth_getTransactionReceipt') { result.logs[0].address = OWNER; return result } },
    ({ request, result }) => { if (request.method === 'eth_getTransactionReceipt') { result.logs[0].topics[3] = HASH(999); return result } },
    ({ request, result }) => { if (request.method === 'eth_getTransactionReceipt') { result.logs[0].removed = true; return result } },
    ({ request, result }) => { if (request.method === 'eth_getTransactionReceipt') { result.logs[0].data = `0x${word(99)}${word(1)}${word(100)}${word(900)}`; return result } },
    ({ provider, request, result }) => { if (provider && request.method === 'eth_getBlockByNumber' && request.params[0] === '0x1f4') { result.hash = HASH(999); return result } },
  ]
  for (const rpc of mutations) assert.equal((await fixture({ rpc }).run()).required_checks_complete, false)
})

test('reconciler and canonical head checks reject stale/mismatched/foreign scope and fallback', async () => {
  for (const snapshot of [
    raw => { raw.health[0].last_success_at_ms = epoch(BASE - 121000) },
    raw => { raw.health[0].safe_block_hash = HASH(999) },
    raw => { raw.cursors.push({ ...raw.cursors[0], chain_id: '1' }) },
    raw => { raw.cursors[0].paymaster = OWNER },
    raw => { raw.read_only = 'off' },
  ]) assert.equal((await fixture({ snapshot }).run()).required_checks_complete, false)
  const rpc = ({ provider, request, result }) => {
    if (provider && request.method === 'eth_getBlockByNumber' && request.params[0] === 'safe') { result.hash = HASH(999); return result }
  }
  assert.equal((await fixture({ rpc }).run()).required_checks_complete, false)
  assert.equal((await fixture({ upstream: 'backup' }).run()).required_checks_complete, false)
})

test('wallet proof requires explicit gauges, fresh HTTPDate, no process restart; only known stale outstanding may be positive', async () => {
  assert.equal(parseExecutorMetrics(metrics()).outstanding, 187)
  for (const text of [metrics().replace(/alto_user_operations[^\n]+\n/, ''), metrics(-1), metrics().replace('available_count 4', 'available_count 3'),
    metrics() + 'alto_user_operations_in_mempool_count{status="queued"} 1\n', metrics() + 'alto_executor_wallets_total_count 4\n']) assert.throws(() => parseExecutorMetrics(text))
  assert.equal((await fixture({ metricAge: -20000 }).run()).required_checks_complete, false)
  assert.equal((await fixture({ metrics: index => metrics(187, 1793728822 + index) }).run()).required_checks_complete, false)
})

test('final complete reread rejects late accepted markers but permits new unsubmitted preparations', async () => {
  const late = fixture({ snapshot: (raw, index) => { if (index === 2) raw.authorizations.push(authorization(11, 'submitted')) } })
  assert.equal((await late.run()).required_checks_complete, false)
  const prepare = fixture({ snapshot: (raw, index) => { if (index === 2) raw.authorizations.push(authorization(11, 'reserved')) } })
  assert.equal((await prepare.run()).required_checks_complete, true)
  const malformedExpiry = fixture({ snapshot: (raw, index) => {
    if (index === 2) {
      const row = authorization(11, 'expired')
      row.submitted_at_ms = null; row.user_hash = null; row.valid_until = epoch(BASE / 1000 + 1000)
      raw.authorizations.push(row)
    }
  } })
  assert.equal((await malformedExpiry.run()).required_checks_complete, false)
})

test('barrier must be real before grace; secrets and child diagnostics never escape result', async () => {
  const noBarrier = fixture({ barrierStatus: 200 })
  assert.equal((await noBarrier.run()).required_checks_complete, false)
  assert(!noBarrier.events.some(event => event.type === 'sleep' || event.type === 'psql'))
  const error = new Error(`${ENV.DATABASE_URL} ${ENV.PERPS_RPC_AUTH_TOKEN}`)
  const failed = await fixture({ databaseError: error }).run()
  assert.equal(failed.required_checks_complete, false)
  const output = JSON.stringify(failed)
  for (const secret of Object.values(ENV).filter(value => value.includes('private') || value.includes('secret'))) assert(!output.includes(secret))
  assert.equal((await fixture().run({ env: { ...ENV, PERPS_CHAIN_ID: '1' } })).required_checks_complete, false)
})

test('deadline aborts grace and child failures are generic', async () => {
  const mock = fixture()
  const result = await mock.run({ deadlineMs: 5, sleep: async (ms, signal) => new Promise((resolve, reject) => {
    signal.addEventListener('abort', () => reject(new Error('secret timeout detail')), { once: true })
  }) })
  assert.equal(result.required_checks_complete, false)
  assert.equal(result.tests.find(test => test.name === 'acknowledgement_grace').pass, false)
  await assert.rejects(readJournal({ database: ENV.DATABASE_URL, execFileImpl: () => { throw new Error(ENV.DATABASE_URL) } }), { message: 'drain_database_failed' })
})

test('inline Node source emits exactly one safe fail-closed JSON event without configured secrets', () => {
  const source = readFileSync(new URL('./qualify-drain.mjs', import.meta.url), 'utf8')
  let stdout
  try { stdout = execFileSync(process.execPath, ['--input-type=module', '-e', source], { env: { PATH: process.env.PATH }, encoding: 'utf8' }) }
  catch (error) { assert.equal(error.status, 1); stdout = error.stdout; assert.equal(error.stderr, '') }
  const lines = stdout.trim().split('\n')
  assert.equal(lines.length, 1)
  assert.equal(JSON.parse(lines[0]).required_checks_complete, false)
})
