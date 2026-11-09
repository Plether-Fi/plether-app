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
  'historical_expiry_cutoff_verified',
])

// There is deliberately no LIMIT, state filter, recovery-table expiry filter,
// or chain/paymaster WHERE clause. Unexpected scope and impossible markers
// must fail qualification instead of falling outside the scanned population.
const DETAIL_QUERY = `SELECT jsonb_build_object(
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
`

const META_QUERY = `SELECT jsonb_build_object(
  'read_only',current_setting('transaction_read_only'), 'isolation',current_setting('transaction_isolation'),
  'snapshot_at_ms',floor(extract(epoch FROM clock_timestamp())*1000)::bigint::text,
  'cursors',COALESCE((SELECT jsonb_agg(jsonb_build_object('chain_id',chain_id::text,'paymaster',paymaster,
    'safe_block',safe_block::text,'safe_block_hash',safe_block_hash,
    'updated_at_ms',floor(extract(epoch FROM updated_at)*1000)::bigint::text)) FROM aa_reconciler_cursor),'[]'::jsonb),
  'health',COALESCE((SELECT jsonb_agg(jsonb_build_object('chain_id',chain_id::text,'paymaster',paymaster,
    'safe_block',safe_block::text,'safe_block_hash',safe_block_hash,
    'last_success_at_ms',floor(extract(epoch FROM last_success_at)*1000)::bigint::text)) FROM aa_reconciler_health),'[]'::jsonb)
);`
export const BOUNDARY_SQL = `BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY;\n${META_QUERY}\nROLLBACK;\n`

// PostgreSQL has no arbitrary-width hexadecimal numeric cast. This bounded,
// read-only expression validates native uint256 JSON quantities without a
// function/schema mutation or lossy BIGINT/double conversion.
function hexNumeric(expression) {
  const padded = `lpad(substr(${expression},3),64,'0')`
  const significant = `ltrim(substr(${expression},3),'0')`
  const chunks = Array.from({ length: 8 }, (_, index) =>
    `(('x'||substr(${padded},${index * 8 + 1},8))::bit(32)::bigint)::numeric*${1n << BigInt((7 - index) * 32)}`)
  // Common block/log/gas/nonce words fit below 2^60. Strip ABI padding before
  // choosing the fast path; a 16-digit value keeps the exact uint256 fallback.
  return `(CASE WHEN (${expression}) ~ '^0x[0-9a-fA-F]{1,64}$' THEN
    CASE WHEN length(${significant})<=15 THEN
      ('x'||lpad(COALESCE(NULLIF(${significant},''),'0'),16,'0'))::bit(64)::bigint::numeric
    ELSE (${chunks.join('+')}) END ELSE NULL END)`
}

export function buildJournalSQL(cutoffSeconds) {
  decimal(cutoffSeconds)
  const data = `(e.event_json->>'data')`
  const authValid = `(a.request_key ~ '^0x[0-9a-f]{64}$' AND a.digest ~ '^0x[0-9a-f]{64}$'
    AND a.sender ~ '^0x[0-9a-f]{40}$' AND a.owner ~ '^0x[0-9a-f]{40}$'
    AND a.nonce>=0 AND a.max_cost_wei>0 AND a.valid_after>=0 AND a.valid_until>a.valid_after
    AND a.valid_until<281474976710656 AND a.valid_until-a.valid_after<=600
    AND jsonb_typeof(a.operation)='object' AND lower(a.operation->>'sender')=a.sender
    AND (a.operation->>'nonce') ~ '^0x(0|[1-9a-fA-F][0-9a-fA-F]{0,63})$'
    AND ${hexNumeric("a.operation->>'nonce'")}=a.nonce
    AND (a.operation->>'paymaster' IS NULL OR lower(a.operation->>'paymaster')='${PAYMASTER}')
    AND (a.operation->>'chainId' IS NULL OR a.operation->>'chainId' IN ('421614','0x66eee'))
    AND (a.operation->>'entryPoint' IS NULL OR lower(a.operation->>'entryPoint')='${ENTRYPOINT}')
    AND CASE WHEN a.state IN ('reserved','cancelled') THEN
      a.signature IS NULL AND a.expected_user_operation_hash IS NULL AND a.user_operation_hash IS NULL
      AND a.signed_at IS NULL AND a.submitted_at IS NULL
      AND CASE WHEN a.state='reserved' THEN a.settled_at IS NULL
        ELSE a.settled_at IS NOT NULL AND a.settled_at<=clock_timestamp()+interval '5 seconds' END
    WHEN a.state IN ('signed','submitted','settled','expired') THEN
      a.signature ~ '^0x[0-9a-f]{130}$' AND a.expected_user_operation_hash ~ '^0x[0-9a-f]{64}$'
      AND a.signed_at IS NOT NULL AND extract(epoch FROM a.signed_at)>=a.valid_after
      AND extract(epoch FROM a.signed_at)<a.valid_until AND a.signed_at<=clock_timestamp()+interval '5 seconds'
      AND (a.user_operation_hash IS NULL OR a.user_operation_hash=a.expected_user_operation_hash)
      AND (a.submitted_at IS NULL OR (a.user_operation_hash=a.expected_user_operation_hash
        AND a.submitted_at>=a.signed_at AND a.submitted_at<=clock_timestamp()+interval '5 seconds'))
      AND CASE WHEN a.state='signed' THEN a.user_operation_hash IS NULL AND a.submitted_at IS NULL AND a.settled_at IS NULL
        WHEN a.state='submitted' THEN a.submitted_at IS NOT NULL AND a.user_operation_hash=a.expected_user_operation_hash AND a.settled_at IS NULL
        WHEN a.state='settled' THEN a.user_operation_hash=a.expected_user_operation_hash
          AND a.settled_at>=a.signed_at AND a.settled_at<=clock_timestamp()+interval '5 seconds'
        ELSE ((a.user_operation_hash IS NULL)=(a.submitted_at IS NULL))
          AND a.settled_at>=a.signed_at AND a.settled_at<=clock_timestamp()+interval '5 seconds' END
    ELSE false END)`
  const eventValid = `(e.digest ~ '^0x[0-9a-f]{64}$' AND e.user_operation_hash ~ '^0x[0-9a-f]{64}$'
    AND e.transaction_hash ~ '^0x[0-9a-f]{64}$' AND e.block_hash ~ '^0x[0-9a-f]{64}$'
    AND e.block_number>=0 AND e.block_number<=(SELECT max(safe_block) FROM aa_reconciler_cursor)
    AND e.actual_gas_cost_wei>=0 AND e.actual_gas_cost_wei<=a.max_cost_wei
    AND e.finalized_at IS NOT NULL AND e.finalized_at<=clock_timestamp()+interval '5 seconds'
    AND a.valid AND a.state='settled' AND a.expected_user_operation_hash=e.user_operation_hash AND a.user_operation_hash=e.user_operation_hash
    AND jsonb_typeof(e.event_json)='object' AND lower(e.event_json->>'address')='${ENTRYPOINT}'
    AND CASE WHEN jsonb_typeof(e.event_json->'topics')='array' THEN jsonb_array_length(e.event_json->'topics')=4 ELSE false END
    AND lower(e.event_json->'topics'->>0)='${TOPIC}' AND lower(e.event_json->'topics'->>1)=e.user_operation_hash
    AND lower(e.event_json->'topics'->>2)='0x'||repeat('0',24)||substr(a.sender,3)
    AND lower(e.event_json->'topics'->>3)='0x'||repeat('0',24)||substr('${PAYMASTER}',3)
    AND length(${data})=258 AND ${data} ~ '^0x[0-9a-fA-F]+$'
    AND ${hexNumeric(`'0x'||substr(${data},3,64)`)}=a.nonce
    AND lower(substr(${data},67,64))=CASE WHEN e.success THEN repeat('0',63)||'1' ELSE repeat('0',64) END
    AND ${hexNumeric(`'0x'||substr(${data},131,64)`)}=e.actual_gas_cost_wei
    AND lower(e.event_json->>'transactionHash')=e.transaction_hash
    AND lower(e.event_json->>'blockHash')=e.block_hash
    AND ${hexNumeric("e.event_json->>'blockNumber'")}=e.block_number
    AND ${hexNumeric("e.event_json->>'logIndex'")}>=0
    AND (NOT(e.event_json?'removed') OR e.event_json->'removed'='false'::jsonb))`
  const accepted = `(a.submitted_at IS NOT NULL OR a.user_operation_hash IS NOT NULL OR a.state='submitted')`
  const historical = `(a.state IN ('settled','expired') AND a.valid_until<${cutoffSeconds})`
  // Consolidate counts and fingerprints into one aggregate per population.
  // Repeated scans of wide materialized rows otherwise create large temp I/O.
  const authTotals = `SELECT count(*) AS authorization_count,
    count(*) FILTER(WHERE a.submitted_at IS NOT NULL) AS submitted_count,
    count(*) FILTER(WHERE ${accepted} AND a.state NOT IN ('settled','expired')) AS unresolved_count,
    count(*) FILTER(WHERE a.state='settled') AS settled_count,
    count(*) FILTER(WHERE a.state='expired') AS expired_count,
    count(*) FILTER(WHERE ${historical} AND a.state='settled') AS history_settled_count,
    count(*) FILTER(WHERE ${historical} AND a.state='expired') AS history_expired_count,
    count(*) FILTER(WHERE NOT ${historical} AND a.state='settled') AS frontier_settled_count,
    count(*) FILTER(WHERE NOT ${historical} AND a.state='expired') AS frontier_expired_count,
    count(*) FILTER(WHERE ${historical}) AS historical_authorization_count,
    count(*) FILTER(WHERE NOT a.valid) AS invalid_authorization_count,
    count(*) FILTER(WHERE (a.state='settled' AND (COALESCE(es.valid_count,0)<>1 OR es.only_hash<>a.expected_user_operation_hash))
      OR (a.state='expired' AND EXISTS(SELECT 1 FROM aa_user_operation_events e
        WHERE e.digest=a.digest OR e.user_operation_hash=a.expected_user_operation_hash OR e.user_operation_hash=a.user_operation_hash))) AS invalid_terminal_count,
    COALESCE(string_agg(md5(to_jsonb(a)::text),'' ORDER BY a.digest) FILTER(WHERE ${historical} AND ${accepted}),'') AS auth_fingerprint
    FROM checked_auths a LEFT JOIN event_summary es ON es.digest=a.digest`
  const eventTotals = `SELECT count(*) FILTER(WHERE NOT e.valid) AS invalid_event_count,
    count(*) FILTER(WHERE ${historical}) AS historical_event_count,
    COALESCE(string_agg(md5(to_jsonb(e)::text),'' ORDER BY e.user_operation_hash)
      FILTER(WHERE ${historical} AND ${accepted}),'') AS event_fingerprint
    FROM checked_events e LEFT JOIN checked_auths a ON a.digest=e.digest`
  const accounting = `(SELECT (to_jsonb(a)-'auth_fingerprint') || (to_jsonb(e)-'event_fingerprint')
    || jsonb_build_object('historical_fingerprint',md5(a.auth_fingerprint||':'||e.event_fingerprint)) FROM auth_totals a CROSS JOIN event_totals e)`
  // Invalidity is evaluated over the entire population before frontier detail
  // selection. Expired signed terminal history is no longer actionable; its
  // receipts are intentionally not represented as freshly RPC-verified.
  const details = DETAIL_QUERY.replace('SELECT jsonb_build_object(', `SELECT ${accounting} || jsonb_build_object(`)
    .replace('FROM aa_sponsorship_authorizations)', 'FROM frontier_authorizations)')
    .replace('FROM aa_user_operation_events)', 'FROM frontier_events)')
  return `BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY;
WITH checked_auths AS MATERIALIZED (SELECT a.*, (${authValid}) IS TRUE AS valid FROM aa_sponsorship_authorizations a),
checked_events AS MATERIALIZED (SELECT e.*, (${eventValid}) IS TRUE AS valid FROM aa_user_operation_events e LEFT JOIN checked_auths a ON a.digest=e.digest),
event_summary AS MATERIALIZED (SELECT digest,count(*) FILTER(WHERE valid) AS valid_count,min(user_operation_hash) AS only_hash FROM checked_events GROUP BY digest),
auth_totals AS MATERIALIZED (${authTotals}),
event_totals AS MATERIALIZED (${eventTotals}),
frontier_authorizations AS MATERIALIZED (SELECT a.* FROM checked_auths a WHERE NOT ${historical}),
frontier_events AS MATERIALIZED (SELECT e.* FROM checked_events e WHERE e.digest IN(SELECT digest FROM frontier_authorizations)
  OR e.user_operation_hash IN(SELECT expected_user_operation_hash FROM frontier_authorizations)
  OR e.user_operation_hash IN(SELECT user_operation_hash FROM frontier_authorizations))
${details}
ROLLBACK;
`
}

// Static export remains useful for inspecting the full-scan SQL contract;
// execution always binds the independently attested numeric cutoff instead.
export const JOURNAL_SQL = buildJournalSQL('0')

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
export function readJournal({ database, signal, execFileImpl = execFile, sql = JOURNAL_SQL }) {
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
      child.stdin.end(sql)
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

export function inspectBoundary(snapshot, now) {
  assert(snapshot?.read_only === 'on' && snapshot.isolation === 'repeatable read')
  const snapshotAt = milliseconds(snapshot.snapshot_at_ms)
  fresh(snapshotAt, now, 15000)
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
  return { snapshotAt, cursor, health }
}

const COUNT_FIELDS = ['authorization_count', 'submitted_count', 'unresolved_count', 'settled_count', 'expired_count',
  'history_settled_count', 'history_expired_count', 'frontier_settled_count', 'frontier_expired_count',
  'historical_authorization_count', 'historical_event_count',
  'invalid_authorization_count', 'invalid_event_count', 'invalid_terminal_count']

export function inspectJournal(snapshot, now) {
  const { snapshotAt, cursor, health } = inspectBoundary(snapshot, now)
  assert(Array.isArray(snapshot.authorizations) && Array.isArray(snapshot.events))
  const counts = Object.fromEntries(COUNT_FIELDS.map(name => {
    assert(Number.isSafeInteger(snapshot[name]) && snapshot[name] >= 0)
    return [name, snapshot[name]]
  }))
  assert(counts.invalid_authorization_count === 0 && counts.invalid_event_count === 0 && counts.invalid_terminal_count === 0)
  assert(typeof snapshot.historical_fingerprint === 'string' && /^[0-9a-f]{32}$/.test(snapshot.historical_fingerprint))
  assert(counts.authorization_count === snapshot.authorizations.length + counts.historical_authorization_count &&
    counts.settled_count === counts.history_settled_count + counts.frontier_settled_count &&
    counts.expired_count === counts.history_expired_count + counts.frontier_expired_count &&
    counts.historical_authorization_count === counts.history_settled_count + counts.history_expired_count &&
    counts.historical_event_count === counts.history_settled_count)
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
  assert(counts.frontier_settled_count === settled.length && counts.frontier_expired_count === expired.length &&
    counts.unresolved_count === unresolved && counts.submitted_count >= submitted)
  return { snapshotAt, cursor, health, byDigest, events, settled, expired, submitted: counts.submitted_count,
    unresolved, counts, historicalFingerprint: snapshot.historical_fingerprint }
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
  let boundaryReads = 0
  let cutoff
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
      const value = await check('journal_snapshot', () => readJournal({ database: env.DATABASE_URL, signal, execFileImpl,
        sql: buildJournalSQL(cutoff.timestamp.toString()) }))
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
        if (cutoff) assert(headers[0].number >= cutoff.number && headers[0].timestamp >= cutoff.timestamp)
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
      return JSON.stringify([raw.historical_fingerprint, sorted(accepted, 'digest'), sorted(raw.events.filter(event => digests.has(event.digest)), 'user_hash')])
    }
    await barrier()
    await check('acknowledgement_grace', async () => { await sleep(22000, signal); signal.throwIfAborted() })
    await barrier()
    await check('historical_expiry_cutoff_verified', async () => {
      const raw = await readJournal({ database: env.DATABASE_URL, signal, execFileImpl, sql: BOUNDARY_SQL })
      boundaryReads++
      const initial = inspectBoundary(raw, now())
      cutoff = await canonicalBoundary(initial)
      await canonicalHeads(cutoff)
    })
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
      if (sample === 1) await check('historical_expiry_cutoff_verified', async () => {
        const headers = await Promise.all([0, 1].map(async provider =>
          block(await rpc(provider, 'eth_getBlockByNumber', [hex(cutoff.number), false]))))
        for (const header of headers) sameBlock(header, cutoff)
      })
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
      if (journals.length === 2) await check('journal_terminal', async () =>
        assert(journals[0].historicalFingerprint === journals[1].historicalFingerprint))
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
  const finishedAt = now()
  const result = {
    event: 'alto_rpc_qualification', schema_version: 1, scope: 'journal_idle',
    required_checks_complete: complete, tests: REQUIRED_CHECKS.map(name => ({ name, pass: tests[name] })),
    started_at_ms: startedAt, finished_at_ms: finishedAt, idle_sample_count: samples.length, snapshot_count: journals.length,
    rpc_call_count: calls, journal_read_count: journalReads, boundary_read_count: boundaryReads,
  }
  if (complete) Object.assign(result, {
    ...last.counts, idle_observed_at_ms: finishedAt,
    outstanding_gauge_first: samples[0].outstanding, outstanding_gauge_last: samples[1].outstanding,
    executor_wallets_available: samples[1].available, executor_wallets_total: samples[1].total,
    process_started_at_seconds: samples[1].started, canonical_settlement_count: settledVerified,
    boundary_block_number: Number(decimal(last.cursor.safe_block)), latest_age_seconds: latestAge,
    safe_age_seconds: safeAge, reconciler_age_seconds: (finishedAt - milliseconds(last.health.last_success_at_ms)) / 1000,
    historical_expiry_cutoff_block_number: Number(cutoff.number), historical_expiry_cutoff_timestamp_seconds: Number(cutoff.timestamp),
  })
  return result
}

if ((!process.argv[1] && import.meta.url.endsWith('/[eval1]')) ||
    (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href)) {
  const result = await runDrainQualification()
  process.stdout.write(`${JSON.stringify(result)}\n`)
  if (!result.required_checks_complete) process.exitCode = 1
}
