// Read-only cutover proof. The authorization journal, not Alto's stale
// outstanding gauge, proves that no accepted actionable operation is lost.
import { execFile } from 'node:child_process'
import { pathToFileURL } from 'node:url'

const GATEWAY = 'https://rpc-internal.sepolia.plether.com/'
const PAYMASTER = '0x9761091045616a388f5fe1433721b272c78fe31b'
const ENTRYPOINT = '0x4337084d9e255ff0702461cf8895ce9e3b5ff108'
const TOPIC = '0x49628fd1471006c1482da88028e9ce4dbb080b815c9b0344d39e5a8e6ec1419f'
const HASH = /^0x[0-9a-f]{64}$/
const ADDRESS = /^0x[0-9a-f]{40}$/
const HEX = /^0x(?:0|[1-9a-f][0-9a-f]*)$/
export const REQUIRED_CHECKS = Object.freeze([
  'drain_configuration', 'admission_barrier', 'acknowledgement_grace',
  'executor_wallets', 'process_continuity', 'journal_snapshot', 'journal_terminal',
  'reconciler_freshness', 'canonical_boundary', 'fresh_canonical_heads', 'canonical_settlements',
])

// There is deliberately no LIMIT, state filter, recovery-table expiry filter,
// or chain/paymaster WHERE clause. Unexpected scope and impossible markers
// must fail qualification instead of falling outside the scanned population.
export const JOURNAL_SQL = `BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY;
SELECT jsonb_build_object(
  'read_only', current_setting('transaction_read_only'),
  'isolation', current_setting('transaction_isolation'),
  'snapshot_at_ms', floor(extract(epoch FROM clock_timestamp()) * 1000)::bigint::text,
  'authorizations', COALESCE((SELECT jsonb_agg(jsonb_build_object(
    'digest',digest, 'request_key',request_key,
    'expected_hash',expected_user_operation_hash, 'user_hash',user_operation_hash,
    'sender',sender, 'owner',owner, 'nonce',nonce::text,
    'valid_after',valid_after::text, 'valid_until',valid_until::text,
    'max_cost',max_cost_wei::text, 'state',state,
    'signature_present',signature IS NOT NULL,
    'signature_valid',COALESCE(signature ~ '^0x[0-9a-f]{130}$',false),
    'signed_at_ms',floor(extract(epoch FROM signed_at)*1000)::bigint::text,
    'submitted_at_ms',floor(extract(epoch FROM submitted_at)*1000)::bigint::text,
    'settled_at_ms',floor(extract(epoch FROM settled_at)*1000)::bigint::text,
    'operation_object',jsonb_typeof(operation)='object',
    'operation_sender',operation->>'sender', 'operation_nonce',operation->>'nonce',
    'operation_paymaster',operation->>'paymaster', 'operation_chain_id',operation->>'chainId',
    'operation_entry_point',operation->>'entryPoint'
  )) FROM aa_sponsorship_authorizations),'[]'::jsonb),
  'events', COALESCE((SELECT jsonb_agg(jsonb_build_object(
    'digest',digest, 'user_hash',user_operation_hash, 'transaction_hash',transaction_hash,
    'block_number',block_number::text, 'block_hash',block_hash, 'success',success,
    'actual_cost',actual_gas_cost_wei::text, 'event',event_json,
    'finalized_at_ms',floor(extract(epoch FROM finalized_at)*1000)::bigint::text
  )) FROM aa_user_operation_events),'[]'::jsonb),
  'cursors', COALESCE((SELECT jsonb_agg(jsonb_build_object(
    'chain_id',chain_id::text, 'paymaster',paymaster, 'safe_block',safe_block::text,
    'safe_block_hash',safe_block_hash,
    'updated_at_ms',floor(extract(epoch FROM updated_at)*1000)::bigint::text
  )) FROM aa_reconciler_cursor),'[]'::jsonb),
  'health', COALESCE((SELECT jsonb_agg(jsonb_build_object(
    'chain_id',chain_id::text, 'paymaster',paymaster, 'safe_block',safe_block::text,
    'safe_block_hash',safe_block_hash,
    'last_success_at_ms',floor(extract(epoch FROM last_success_at)*1000)::bigint::text
  )) FROM aa_reconciler_health),'[]'::jsonb)
);
ROLLBACK;
`

function assert(ok) { if (!ok) throw new Error('drain_check_failed') }
function decimal(value) { assert(typeof value === 'string' && /^(0|[1-9][0-9]*)$/.test(value)); return BigInt(value) }
function quantity(value) { assert(typeof value === 'string' && HEX.test(value)); return BigInt(value) }
function lower(value) { assert(typeof value === 'string'); return value.toLowerCase() }
function milliseconds(value) { const number = Number(decimal(value)); assert(Number.isSafeInteger(number) && number > 0); return number }
function fresh(value, now, age) { assert(value <= now + 5000 && now - value <= age) }
function hex(value) { return `0x${value.toString(16)}` }

export function databaseEnvironment(database) {
  const url = new URL(database)
  assert(['postgres:', 'postgresql:'].includes(url.protocol) && url.hostname && url.pathname.length > 1 && !url.hash)
  const decoded = value => { const result = decodeURIComponent(value); assert(result && !result.includes('\0')); return result }
  const result = {
    PATH: '/usr/local/bin:/usr/bin:/bin', PGHOST: url.hostname, PGPORT: url.port || '5432',
    PGUSER: decoded(url.username), PGPASSWORD: decodeURIComponent(url.password), PGDATABASE: decoded(url.pathname.slice(1)),
    PGOPTIONS: '-c default_transaction_read_only=on -c statement_timeout=15000 -c lock_timeout=3000 -c idle_in_transaction_session_timeout=20000',
    PGCONNECT_TIMEOUT: '10', PGAPPNAME: 'plether-alto-drain-readonly',
  }
  assert(!result.PGPASSWORD.includes('\0'))
  const allowed = { sslmode: 'PGSSLMODE', sslrootcert: 'PGSSLROOTCERT', sslcert: 'PGSSLCERT', sslkey: 'PGSSLKEY',
    sslcrl: 'PGSSLCRL', sslcrldir: 'PGSSLCRLDIR', channel_binding: 'PGCHANNELBINDING', connect_timeout: 'PGCONNECT_TIMEOUT' }
  const seen = new Set()
  for (const [name, value] of url.searchParams) {
    assert(Object.hasOwn(allowed, name) && !seen.has(name) && value.length > 0 && !value.includes('\0'))
    seen.add(name); result[allowed[name]] = value
  }
  if (result.PGSSLMODE) assert(['disable', 'allow', 'prefer', 'require', 'verify-ca', 'verify-full'].includes(result.PGSSLMODE))
  if (result.PGCHANNELBINDING) assert(['disable', 'prefer', 'require'].includes(result.PGCHANNELBINDING))
  assert(/^[1-9][0-9]*$/.test(result.PGCONNECT_TIMEOUT) && Number(result.PGCONNECT_TIMEOUT) <= 120)
  return result
}

function configuration(env) {
  assert(env.PLETHER_ALTO_QUALIFY_MODE === 'journal_idle' && env.PERPS_CHAIN_ID === '421614')
  assert(lower(env.AA_PAYMASTER_ADDRESS) === PAYMASTER && env.PERPS_RPC_URL === GATEWAY)
  assert(!env.ALTO_EXECUTOR_PRIVATE_KEYS && !env.ALTO_UTILITY_PRIVATE_KEY && !env.AA_SIGNER_PRIVATE_KEY)
  const token = env.PERPS_RPC_AUTH_TOKEN
  assert(typeof token === 'string' && token.length >= 32 && token.length <= 4096 && /^[\x21-\x7e]+$/.test(token))
  databaseEnvironment(env.DATABASE_URL)
  const backup = new URL(env.PLETHER_ALTO_BACKUP_RPC_URL)
  assert(backup.protocol === 'https:' && backup.hostname === 'arb-sepolia.g.alchemy.com' &&
    !backup.username && !backup.password && !backup.hash && (!backup.port || backup.port === '443'))
  const baseline = new URL(env.PLETHER_ALTO_BASELINE_URL)
  assert(baseline.protocol === 'http:' && !baseline.username && !baseline.password && !baseline.search && !baseline.hash &&
    baseline.pathname === '/' && (!baseline.port || baseline.port === '80') &&
    /^internal-plether-sepolia-alto-[a-z0-9-]+\.ap-southeast-1\.elb\.amazonaws\.com$/.test(baseline.hostname))
  const safeLag = Number(decimal(env.AA_RECONCILER_MAX_SAFE_LAG_SECONDS))
  assert(Number.isSafeInteger(safeLag) && safeLag >= 60 && safeLag <= 1800)
  return { token, baseline: baseline.href, backup: backup.href, safeLag }
}

// Secrets are passed only in the child environment. -X disables psqlrc; no
// shell is used, no database URL occurs in argv, and child errors are discarded.
export function readJournal({ database, signal, execFileImpl = execFile }) {
  return new Promise((resolve, reject) => {
    const failure = () => reject(new Error('drain_database_failed'))
    let child
    try {
      const childEnv = databaseEnvironment(database)
      child = execFileImpl('/usr/bin/psql', ['-X', '-qAt', '--no-password', '--set=ON_ERROR_STOP=1', '--file=-'], {
        env: childEnv,
        signal, timeout: 20000, killSignal: 'SIGKILL', maxBuffer: 32 * 1024 * 1024,
      }, (error, stdout) => {
        if (error) { failure(); return }
        try { resolve(JSON.parse(stdout.trim())) } catch { failure() }
      })
      child.stdin.on('error', failure)
      child.stdin.end(JOURNAL_SQL)
    } catch { failure() }
  })
}

async function boundedText(response, maximum = 4 * 1024 * 1024) {
  assert(response.body?.getReader)
  const reader = response.body.getReader()
  const chunks = []
  let size = 0
  for (;;) {
    const { done, value } = await reader.read()
    if (done) break
    size += value.length
    if (size > maximum) { await reader.cancel(); throw new Error('drain_response_too_large') }
    chunks.push(value)
  }
  const all = new Uint8Array(size)
  let offset = 0
  for (const chunk of chunks) { all.set(chunk, offset); offset += chunk.length }
  return new TextDecoder().decode(all)
}

export function parseExecutorMetrics(text) {
  assert(typeof text === 'string' && text.length <= 1024 * 1024)
  const fields = new Map()
  const statuses = new Map()
  for (const line of text.split('\n')) {
    if (!line || line.startsWith('#')) continue
    const row = /^([a-zA-Z_:][a-zA-Z0-9_:]*)(?:\{([^}]*)\})?\s+([^\s]+)(?:\s+\d+)?$/.exec(line)
    if (!row) continue
    const [, name, labels, raw] = row
    if (!['alto_user_operations_in_mempool_count', 'alto_executor_wallets_available_count',
      'alto_executor_wallets_total_count', 'process_start_time_seconds'].includes(name)) continue
    const value = Number(raw)
    assert(Number.isFinite(value) && value >= 0)
    if (name === 'alto_user_operations_in_mempool_count') {
      const found = [...(labels ?? '').matchAll(/(?:^|,)\s*status="([^"\\]*)"/g)]
      assert(found.length === 1 && /^(outstanding|processing|submitted|queued)$/.test(found[0][1]))
      assert(Number.isSafeInteger(value) && !statuses.has(found[0][1]))
      statuses.set(found[0][1], value)
    } else { assert(!fields.has(name)); fields.set(name, value) }
  }
  const outstanding = statuses.get('outstanding')
  const available = fields.get('alto_executor_wallets_available_count')
  const total = fields.get('alto_executor_wallets_total_count')
  const started = fields.get('process_start_time_seconds')
  assert(Number.isSafeInteger(outstanding) && outstanding >= 0 && available === 4 && total === 4 && started > 0)
  assert([...statuses].every(([status, value]) => status === 'outstanding' || value === 0))
  // This value is diagnostic only. Zero also cannot bypass journal validation.
  return { outstanding, available, total, started }
}

function block(value) {
  assert(value && HASH.test(lower(value.hash)))
  return { number: quantity(value.number), hash: lower(value.hash), timestamp: quantity(value.timestamp) }
}
function sameBlock(a, b) { assert(a.number === b.number && a.hash === b.hash && a.timestamp === b.timestamp) }

export function inspectJournal(snapshot, now) {
  assert(snapshot?.read_only === 'on' && snapshot.isolation === 'repeatable read')
  const snapshotAt = milliseconds(snapshot.snapshot_at_ms)
  fresh(snapshotAt, now, 15000)
  assert(Array.isArray(snapshot.authorizations) && Array.isArray(snapshot.events))
  assert(Array.isArray(snapshot.cursors) && snapshot.cursors.length === 1 && Array.isArray(snapshot.health) && snapshot.health.length === 1)
  const cursor = snapshot.cursors[0]
  const health = snapshot.health[0]
  for (const row of [cursor, health]) {
    assert(row.chain_id === '421614' && row.paymaster === PAYMASTER && HASH.test(row.safe_block_hash))
    decimal(row.safe_block)
  }
  assert(cursor.safe_block === health.safe_block && cursor.safe_block_hash === health.safe_block_hash)
  fresh(milliseconds(cursor.updated_at_ms), now, 120000)
  fresh(milliseconds(health.last_success_at_ms), now, 120000)
  const byDigest = new Map()
  const hashes = new Set()
  let submitted = 0
  let unresolved = 0
  const settled = []
  const expired = []
  for (const row of snapshot.authorizations) {
    assert(row && HASH.test(row.digest) && HASH.test(row.request_key) && !byDigest.has(row.digest))
    assert(ADDRESS.test(row.sender) && ADDRESS.test(row.owner) && row.operation_object === true)
    assert(lower(row.operation_sender) === row.sender && quantity(lower(row.operation_nonce)) === decimal(row.nonce))
    if (row.operation_paymaster !== null) assert(lower(row.operation_paymaster) === PAYMASTER)
    if (row.operation_entry_point !== null) assert(lower(row.operation_entry_point) === ENTRYPOINT)
    if (row.operation_chain_id !== null) assert(row.operation_chain_id === '421614' || row.operation_chain_id === '0x66eee')
    const after = decimal(row.valid_after)
    assert(decimal(row.valid_until) > after && decimal(row.max_cost) > 0n)
    assert(['reserved', 'signed', 'submitted', 'settled', 'expired', 'cancelled'].includes(row.state))
    byDigest.set(row.digest, row)
    if (row.state === 'reserved' || row.state === 'cancelled') {
      assert(row.signature_present === false && row.signature_valid === false && row.expected_hash === null &&
        row.user_hash === null && row.signed_at_ms === null && row.submitted_at_ms === null)
      if (row.state === 'reserved') assert(row.settled_at_ms === null)
      else milliseconds(row.settled_at_ms)
      continue
    }
    assert(row.signature_present === true && row.signature_valid === true && HASH.test(row.expected_hash) && !hashes.has(row.expected_hash))
    hashes.add(row.expected_hash)
    const signedAt = milliseconds(row.signed_at_ms)
    assert(signedAt <= snapshotAt + 5000 && BigInt(signedAt) >= after * 1000n && BigInt(signedAt) < decimal(row.valid_until) * 1000n)
    if (row.user_hash !== null) assert(row.user_hash === row.expected_hash)
    if (row.submitted_at_ms !== null) {
      const submittedAt = milliseconds(row.submitted_at_ms)
      assert(submittedAt >= signedAt && submittedAt <= snapshotAt + 5000 && row.user_hash === row.expected_hash)
      submitted++
    }
    if (row.state === 'submitted') assert(row.submitted_at_ms !== null && row.user_hash === row.expected_hash && row.settled_at_ms === null)
    if (row.state === 'signed') assert(row.user_hash === null && row.submitted_at_ms === null && row.settled_at_ms === null)
    if (row.submitted_at_ms !== null || row.user_hash !== null || row.state === 'submitted') {
      if (!['settled', 'expired'].includes(row.state)) unresolved++
    }
    if (row.state === 'settled' || row.state === 'expired') {
      const settledAt = milliseconds(row.settled_at_ms)
      assert(settledAt >= signedAt && settledAt <= snapshotAt + 5000)
      if (row.state === 'settled') { assert(row.user_hash === row.expected_hash); settled.push(row) }
      else {
        assert((row.user_hash === null) === (row.submitted_at_ms === null))
        expired.push(row)
      }
    }
  }
  const events = new Map()
  const eventHashes = new Set()
  for (const event of snapshot.events) {
    assert(event && HASH.test(event.digest) && HASH.test(event.user_hash) && HASH.test(event.transaction_hash) && HASH.test(event.block_hash))
    assert(!events.has(event.digest) && !eventHashes.has(event.user_hash) && typeof event.success === 'boolean')
    const row = byDigest.get(event.digest)
    assert(row?.state === 'settled' && row.expected_hash === event.user_hash && row.user_hash === event.user_hash)
    assert(decimal(event.block_number) <= decimal(cursor.safe_block) && decimal(event.actual_cost) <= decimal(row.max_cost))
    assert(milliseconds(event.finalized_at_ms) <= snapshotAt + 5000)
    events.set(event.digest, event); eventHashes.add(event.user_hash)
  }
  assert(events.size === settled.length)
  for (const row of settled) assert(events.get(row.digest)?.user_hash === row.expected_hash)
  for (const row of expired) assert(!events.has(row.digest) && !eventHashes.has(row.expected_hash))
  return { snapshotAt, cursor, health, byDigest, events, settled, expired, submitted, unresolved }
}

function parsedEvent(value) {
  assert(value && lower(value.address) === ENTRYPOINT && Array.isArray(value.topics) && value.topics.length === 4)
  const topics = value.topics.map(lower)
  assert(topics.every(topic => HASH.test(topic)) && topics[0] === TOPIC &&
    /^0x0{24}[0-9a-f]{40}$/.test(topics[2]) && topics[3] === `0x${'0'.repeat(24)}${PAYMASTER.slice(2)}`)
  assert(typeof value.data === 'string' && /^0x[0-9a-fA-F]{256}$/.test(value.data))
  const words = [0, 1, 2, 3].map(i => BigInt(`0x${value.data.slice(2 + i * 64, 66 + i * 64)}`))
  assert(words[1] === 0n || words[1] === 1n)
  assert(value.removed === undefined || value.removed === false)
  assert(HASH.test(lower(value.transactionHash)) && HASH.test(lower(value.blockHash)))
  return { hash: topics[1], sender: `0x${topics[2].slice(26)}`, nonce: words[0], success: words[1] === 1n,
    cost: words[2], used: words[3], tx: lower(value.transactionHash), block: quantity(lower(value.blockNumber)),
    blockHash: lower(value.blockHash), index: quantity(lower(value.logIndex)) }
}

export async function verifySettlements(journal, rpc, signal) {
  const receipts = [new Map(), new Map()]
  const blocks = [new Map(), new Map()]
  const cached = (cache, key, run) => { if (!cache.has(key)) cache.set(key, run()); return cache.get(key) }
  const rows = [...journal.settled]
  let position = 0
  async function worker() {
    for (;;) {
      signal.throwIfAborted()
      const row = rows[position++]
      if (!row) break
      const event = journal.events.get(row.digest)
      const stored = parsedEvent(event.event)
      assert(stored.hash === row.expected_hash && stored.sender === row.sender && stored.nonce === decimal(row.nonce) &&
        stored.tx === event.transaction_hash && stored.block === decimal(event.block_number) && stored.blockHash === event.block_hash &&
        stored.success === event.success && stored.cost === decimal(event.actual_cost))
      for (let provider = 0; provider < 2; provider++) {
        const receipt = await cached(receipts[provider], stored.tx, () => rpc(provider, 'eth_getTransactionReceipt', [stored.tx]))
        assert(receipt && lower(receipt.transactionHash) === stored.tx && quantity(lower(receipt.status)) === 1n &&
          quantity(lower(receipt.blockNumber)) === stored.block && lower(receipt.blockHash) === stored.blockHash && Array.isArray(receipt.logs))
        const matches = receipt.logs.filter(log => typeof log.address === 'string' && lower(log.address) === ENTRYPOINT &&
          Array.isArray(log.topics) && typeof log.topics[0] === 'string' && lower(log.topics[0]) === TOPIC &&
          typeof log.topics[1] === 'string' && lower(log.topics[1]) === row.expected_hash)
        assert(matches.length === 1)
        const live = parsedEvent(matches[0])
        assert(Object.keys(stored).every(key => live[key] === stored[key]))
        const canonical = await cached(blocks[provider], event.block_number,
          async () => block(await rpc(provider, 'eth_getBlockByNumber', [hex(stored.block), false])))
        assert(canonical.number === stored.block && canonical.hash === stored.blockHash && canonical.number <= decimal(journal.cursor.safe_block))
      }
    }
  }
  await Promise.all(Array.from({ length: Math.min(4, rows.length) }, worker))
  return rows.length
}

function wait(ms, signal) {
  return new Promise((resolve, reject) => {
    signal.throwIfAborted()
    const done = () => { signal.removeEventListener('abort', abort); resolve() }
    const timer = setTimeout(done, ms)
    const abort = () => { clearTimeout(timer); reject(new Error('drain_check_failed')) }
    signal.addEventListener('abort', abort, { once: true })
  })
}

export async function runDrainQualification({ env = process.env, fetchImpl = globalThis.fetch,
  now = Date.now, sleep = wait, execFileImpl = execFile, deadlineMs = 120000, requestTimeoutMs = 10000 } = {}) {
  const startedAt = now()
  const controller = new AbortController()
  const timer = setTimeout(() => controller.abort(), deadlineMs)
  const signal = controller.signal
  const tests = Object.fromEntries(REQUIRED_CHECKS.map(name => [name, false]))
  let calls = 0
  let samples = []
  let journals = []
  let latestAge = 0
  let safeAge = 0
  let settledVerified = 0
  let journalReads = 0
  let reachedEnd = false
  async function check(name, run) {
    try { signal.throwIfAborted(); const value = await run(); tests[name] = true; return value }
    catch { tests[name] = false; throw new Error('drain_check_failed') }
  }
  try {
    const config = await check('drain_configuration', async () => configuration(env))
    async function request(url, init) {
      return fetchImpl(url, { ...init, redirect: 'error', signal: AbortSignal.any([signal, AbortSignal.timeout(requestTimeoutMs)]) })
    }
    async function barrier() {
      await check('admission_barrier', async () => {
        const response = await request(new URL('/', config.baseline), { method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'eth_chainId', params: [] }) })
        assert(response.status === 503)
        const value = JSON.parse(await boundedText(response, 4096))
        assert(value && Object.keys(value).length === 1 && value.error === 'alto_rpc_cutover_in_progress')
      })
    }
    async function rpc(provider, method, params = []) {
      const id = ++calls
      const response = await request(provider ? config.backup : new URL('/primary', GATEWAY), { method: 'POST',
        headers: { 'Content-Type': 'application/json', ...(!provider ? { Authorization: `Bearer ${config.token}` } : {}) },
        body: JSON.stringify({ jsonrpc: '2.0', id, method, params }) })
      assert(response.status === 200 && (provider || response.headers.get('x-plether-rpc-upstream') === 'primary'))
      const value = JSON.parse(await boundedText(response))
      assert(value?.jsonrpc === '2.0' && value.id === id && !value.error && Object.hasOwn(value, 'result'))
      return value.result
    }
    async function journalRead() {
      const value = await check('journal_snapshot', () => readJournal({ database: env.DATABASE_URL, signal, execFileImpl }))
      journalReads++
      return value
    }
    async function canonicalBoundary(journal) {
      return check('canonical_boundary', async () => {
        const headers = await Promise.all([0, 1].map(async provider => {
          assert(await rpc(provider, 'eth_chainId') === '0x66eee' && await rpc(provider, 'eth_syncing') === false)
          return block(await rpc(provider, 'eth_getBlockByNumber', [hex(decimal(journal.cursor.safe_block)), false]))
        }))
        sameBlock(...headers)
        assert(headers[0].number === decimal(journal.cursor.safe_block) && headers[0].hash === journal.cursor.safe_block_hash)
        return headers[0]
      })
    }
    async function canonicalHeads(boundary) {
      await check('fresh_canonical_heads', async () => {
        for (const tag of ['latest', 'safe']) {
          const heads = await Promise.all([0, 1].map(async provider => block(await rpc(provider, 'eth_getBlockByNumber', [tag, false]))))
          const common = heads[0].number < heads[1].number ? heads[0].number : heads[1].number
          const numeric = await Promise.all([0, 1].map(async provider => block(await rpc(provider, 'eth_getBlockByNumber', [hex(common), false]))))
          sameBlock(...numeric); assert(numeric[0].number === common)
          for (const head of heads) if (head.number === common) sameBlock(head, numeric[0])
          const ages = heads.map(head => now() / 1000 - Number(head.timestamp))
          assert(ages.every(age => age >= (tag === 'latest' ? -5 : -60) && age <= (tag === 'latest' ? 15 : config.safeLag)))
          if (tag === 'latest') { assert(Math.abs(Number(heads[0].timestamp - heads[1].timestamp)) <= 5); latestAge = Math.max(...ages) }
          else { assert(common >= boundary.number); safeAge = Math.max(...ages) }
        }
        assert(now() / 1000 - Number(boundary.timestamp) <= config.safeLag && Number(boundary.timestamp) <= now() / 1000 + 60)
      })
    }
    function population(raw) {
      const sorted = (rows, key) => [...rows].sort((a, b) => a[key].localeCompare(b[key]))
      const accepted = raw.authorizations.filter(row => row.submitted_at_ms !== null || row.user_hash !== null || row.state === 'submitted')
      const digests = new Set(accepted.map(row => row.digest))
      return JSON.stringify([sorted(accepted, 'digest'), sorted(raw.events.filter(event => digests.has(event.digest)), 'user_hash')])
    }
    await barrier()
    await check('acknowledgement_grace', async () => { await sleep(22000, signal); signal.throwIfAborted() })
    for (let sample = 0; sample < 2; sample++) {
      if (sample) await sleep(5000, signal)
      await barrier()
      const raw = await journalRead()
      let journal = await check('reconciler_freshness', async () => inspectJournal(raw, now()))
      await check('journal_terminal', async () => assert(journal.unresolved === 0))
      let boundary = await canonicalBoundary(journal)
      await canonicalHeads(boundary)
      await check('journal_terminal', async () => {
        assert(journal.unresolved === 0)
        for (const row of journal.expired) assert(decimal(row.valid_until) < boundary.timestamp)
      })
      settledVerified = await check('canonical_settlements', () => verifySettlements(journal, rpc, signal))
      if (sample === 1) {
        // Full re-read after the potentially long receipt scan: late accepted
        // markers or changed terminal evidence invalidate this attempt.
        const finalRaw = await journalRead()
        await check('journal_terminal', async () => assert(population(finalRaw) === population(raw)))
        const finalJournal = await check('reconciler_freshness', async () => inspectJournal(finalRaw, now()))
        await check('canonical_boundary', async () => assert(decimal(finalJournal.cursor.safe_block) >= decimal(journal.cursor.safe_block)))
        journal = finalJournal
      }
      boundary = await canonicalBoundary(journal)
      await canonicalHeads(boundary)
      await check('journal_terminal', async () => {
        assert(journal.unresolved === 0)
        for (const row of journal.expired) assert(decimal(row.valid_until) < boundary.timestamp)
      })
      // Recheck admission after the unbounded journal/receipt work. Metrics are
      // deliberately last, so the operator receives a fresh executor sample.
      await barrier()
      samples.push(await check('executor_wallets', async () => {
        const response = await request(new URL('/metrics', config.baseline), { method: 'GET', cache: 'no-store', headers: { 'Cache-Control': 'no-cache' } })
        assert(response.status === 200)
        const date = Date.parse(response.headers.get('date') ?? '')
        assert(Number.isFinite(date) && Math.abs(now() - date) <= 15000)
        const result = parseExecutorMetrics(await boundedText(response, 1024 * 1024))
        assert(Math.abs(now() - date) <= 15000)
        assert(result.started <= now() / 1000 + 5)
        return result
      }))
      journals.push(journal)
    }
    await check('process_continuity', async () => assert(samples.length === 2 && samples[0].started === samples[1].started))
    const last = journals.at(-1)
    await check('reconciler_freshness', async () => {
      fresh(milliseconds(last.health.last_success_at_ms), now(), 120000)
      fresh(milliseconds(last.cursor.updated_at_ms), now(), 120000)
    })
    signal.throwIfAborted()
    reachedEnd = true
  } catch { /* No URLs, child errors, secret values, or raw rows reach stdout. */ }
  finally {
    clearTimeout(timer)
    // A failed concurrent receipt scan must not leave sibling workers issuing
    // requests after the result is emitted or after the deadline is cleared.
    controller.abort()
  }
  const complete = reachedEnd && REQUIRED_CHECKS.every(name => tests[name]) && samples.length === 2 && journals.length === 2
  const last = journals.at(-1)
  const result = {
    event: 'alto_rpc_qualification', schema_version: 1, scope: 'journal_idle',
    required_checks_complete: complete, tests: REQUIRED_CHECKS.map(name => ({ name, pass: tests[name] })),
    started_at_ms: startedAt, finished_at_ms: now(), idle_sample_count: samples.length, snapshot_count: journals.length,
    rpc_call_count: calls, journal_read_count: journalReads,
  }
  if (complete) Object.assign(result, {
    idle_observed_at_ms: now(), submitted_count: last.submitted, unresolved_count: last.unresolved,
    settled_count: last.settled.length, expired_count: last.expired.length,
    outstanding_gauge_first: samples[0].outstanding, outstanding_gauge_last: samples[1].outstanding,
    executor_wallets_available: samples[1].available, executor_wallets_total: samples[1].total,
    process_started_at_seconds: samples[1].started, canonical_settlement_count: settledVerified,
    boundary_block_number: Number(decimal(last.cursor.safe_block)), latest_age_seconds: latestAge,
    safe_age_seconds: safeAge, reconciler_age_seconds: (now() - milliseconds(last.health.last_success_at_ms)) / 1000,
  })
  return result
}

if (!process.argv[1] || import.meta.url === pathToFileURL(process.argv[1]).href) {
  const result = await runDrainQualification()
  process.stdout.write(`${JSON.stringify(result)}\n`)
  if (!result.required_checks_complete) process.exitCode = 1
}
