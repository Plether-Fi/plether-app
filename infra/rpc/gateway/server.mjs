import http from 'node:http'
import { timingSafeEqual } from 'node:crypto'
import { pathToFileURL } from 'node:url'

const READS = new Set([
  'eth_chainId', 'net_version', 'web3_clientVersion', 'eth_syncing',
  'eth_blockNumber', 'eth_getBlockByNumber', 'eth_getBlockByHash',
  'eth_getTransactionByHash', 'eth_getTransactionReceipt',
  'eth_getTransactionCount', 'eth_getBalance', 'eth_getCode', 'eth_getStorageAt',
  'eth_getLogs', 'eth_call', 'eth_estimateGas', 'eth_gasPrice',
  'eth_feeHistory', 'eth_maxPriorityFeePerGas', 'eth_getProof',
])
const WRITES = new Set(['eth_sendRawTransaction', 'eth_sendRawTransactionConditional'])
const ZERO_ARGUMENT_METHODS = new Set([
  'eth_chainId', 'net_version', 'web3_clientVersion', 'eth_syncing',
  'eth_blockNumber', 'eth_gasPrice', 'eth_maxPriorityFeePerGas',
])
// These capabilities remain on the existing managed provider. A pruned Nitro
// execution node is neither a bundler receipt service nor a deep trace archive.
const MANAGED = new Set(['eth_getUserOperationReceipt', 'debug_traceTransaction'])
const STATE_BLOCK_INDEX = {
  eth_call: 1, eth_estimateGas: 1, eth_getBalance: 1, eth_getCode: 1,
  eth_getTransactionCount: 1, eth_getStorageAt: 2, eth_getProof: 2,
}
const STATE_HISTORY_BLOCKS = 345600n
const HEX = /^0x[0-9a-fA-F]+$/
const HASH = /^0x[0-9a-fA-F]{64}$/
const ADDRESS = /^0x[0-9a-fA-F]{40}$/
const MAX_BODY = 512 * 1024
const MAX_RESPONSE = 8 * 1024 * 1024
// Only fixed, internal codes may enter logs. Never log parser/provider messages,
// request IDs, parameters, headers, paths, addresses or transaction payloads.
const REJECTIONS = new Set([
  'body_limit', 'invalid_batch', 'invalid_request', 'invalid_log_filter',
  'latest_logs_require_address_and_explicit_start',
  'log_range_requires_explicit_bounded_blocks', 'invalid_managed_request',
  'invalid_trace', 'one_simulation_per_request',
])

class UpstreamError extends Error {
  constructor(code, retryable = true, httpStatus) {
    super(code); this.code = code; this.retryable = retryable; this.httpStatus = httpStatus
  }
}

async function limitedBody(stream, limit) {
  const chunks = []
  let size = 0
  for await (const chunk of stream) {
    size += chunk.length
    if (size > limit) throw new Error('body_limit')
    chunks.push(chunk)
  }
  return Buffer.concat(chunks).toString('utf8')
}

export function validateRequest(body) {
  const calls = Array.isArray(body) ? body : [body]
  if (!calls.length || calls.length > 20) throw new Error('invalid_batch')
  const ids = new Set()
  let latestLogs = false
  for (const call of calls) {
    if (!call || Array.isArray(call) || call.jsonrpc !== '2.0'
      || !['string', 'number'].includes(typeof call.id)
      || (typeof call.id === 'number' && !Number.isSafeInteger(call.id))
      || String(call.id).length > 128 || ids.has(JSON.stringify(call.id))
      || !(READS.has(call.method) || WRITES.has(call.method) || MANAGED.has(call.method) || call.method === 'debug_traceCall')
      || !(Array.isArray(call.params) || (call.params === undefined && ZERO_ARGUMENT_METHODS.has(call.method)))) throw new Error('invalid_request')
    ids.add(JSON.stringify(call.id))
    if (call.method === 'eth_getLogs') {
      const filter = call.params[0]
      if (!filter || typeof filter !== 'object') throw new Error('invalid_log_filter')
      if (filter.blockHash) {
        if (!HASH.test(filter.blockHash) || filter.fromBlock || filter.toBlock) throw new Error('invalid_log_filter')
      } else if (filter.toBlock === 'latest') {
        const addresses = Array.isArray(filter.address) ? filter.address : [filter.address]
        if (!HEX.test(filter.fromBlock) || !addresses.length || addresses.length > 20
          || !addresses.every(address => typeof address === 'string' && ADDRESS.test(address))) {
          throw new Error('latest_logs_require_address_and_explicit_start')
        }
        // Alto polls a bounded history ending at the current head. Resolve that
        // head per request before admitting the actual numeric range.
        latestLogs = true
      } else if (!HEX.test(filter.fromBlock) || !HEX.test(filter.toBlock)
        || BigInt(filter.toBlock) < BigInt(filter.fromBlock)
        || BigInt(filter.toBlock) - BigInt(filter.fromBlock) >= 5000n) {
        throw new Error('log_range_requires_explicit_bounded_blocks')
      }
    }
    if (MANAGED.has(call.method) && (!HASH.test(call.params[0]) || call.params.length > (call.method === 'debug_traceTransaction' ? 2 : 1))) {
      throw new Error('invalid_managed_request')
    }
    if (call.method === 'debug_traceTransaction') {
      const options = call.params[1] ?? {}
      if (!options || Array.isArray(options) || typeof options !== 'object'
        || options.tracer !== 'callTracer' || (options.timeout && !/^(?:[1-9]|1[0-9]|20)s$/.test(options.timeout))) throw new Error('invalid_trace')
    }
  }
  const isHeavy = c => c.method === 'debug_traceCall' || c.method === 'debug_traceTransaction'
  if (calls.filter(isHeavy).length > 1) throw new Error('one_simulation_per_request')
  return { calls, writable: calls.some(c => WRITES.has(c.method)), heavy: calls.some(isHeavy), latestLogs }
}

async function pinLatestLogRanges(body, upstream, signal) {
  const height = await rpc(upstream, 'eth_blockNumber', [], signal)
  if (typeof height !== 'string' || !HEX.test(height)) throw new UpstreamError('invalid_latest_height')
  const calls = Array.isArray(body) ? body : [body]
  const pinned = calls.map(call => call.method === 'eth_getLogs' && call.params[0].toBlock === 'latest'
    ? { ...call, params: [{ ...call.params[0], toBlock: height }, ...call.params.slice(1)] } : call)
  const request = Array.isArray(body) ? pinned : pinned[0]
  // The original 5,000-block bound applies to the resolved head. An oversized
  // range remains a caller error, rather than a reason to fail over providers.
  validateRequest(request)
  return request
}

export function requiresManagedProvider(call, primaryHeight) {
  if (MANAGED.has(call.method)) return true
  const index = STATE_BLOCK_INDEX[call.method]
  if (index === undefined) return false
  let tag = call.params[index]
  if (tag && typeof tag === 'object') {
    if (tag.blockHash) return true // A hash alone cannot establish state age.
    tag = tag.blockNumber
  }
  return tag === 'earliest' || (HEX.test(tag) && primaryHeight !== undefined
    && BigInt(tag) < primaryHeight - STATE_HISTORY_BLOCKS)
}

function validResponse(body, request) {
  const requests = Array.isArray(request) ? request : [request]
  const replies = Array.isArray(body) ? body : [body]
  if (Array.isArray(request) !== Array.isArray(body) || replies.length !== requests.length) return false
  const ids = new Set(requests.map(c => JSON.stringify(c.id)))
  for (const reply of replies) {
    if (!reply || reply.jsonrpc !== '2.0' || !ids.delete(JSON.stringify(reply.id))
      || Object.hasOwn(reply, 'result') === Object.hasOwn(reply, 'error')) return false
  }
  return ids.size === 0
}

export async function upstreamCall(upstream, request, signal) {
  try {
    const response = await fetch(upstream.url, {
      method: 'POST', redirect: 'error', signal,
      headers: { 'content-type': 'application/json', ...(upstream.token ? { authorization: `Bearer ${upstream.token}` } : {}) },
      body: JSON.stringify(request),
    })
    if (!response.ok) {
      await response.body?.cancel()
      throw new UpstreamError('upstream_http', [429, 502, 503, 504].includes(response.status), response.status)
    }
    const raw = await limitedBody(response.body, MAX_RESPONSE)
    const body = JSON.parse(raw)
    if (!validResponse(body, request)) throw new UpstreamError('upstream_protocol')
    return { raw, body }
  } catch (error) {
    // Never expose URL-embedded credentials, headers or provider error messages.
    throw error instanceof UpstreamError ? error : new UpstreamError('upstream_transport')
  }
}

async function rpc(upstream, method, params, signal) {
  const { body } = await upstreamCall(upstream, { jsonrpc: '2.0', id: 1, method, params }, signal)
  if (body.error) throw new UpstreamError('probe_rpc_error')
  return body.result
}

function block(block) {
  if (!block || !HEX.test(block.number) || !HEX.test(block.timestamp) || !HASH.test(block.hash)) throw new Error('invalid_block')
  return block
}

async function inspect(upstream, chainId, signal) {
  const [chain, syncing, latest, safe, finalized] = await Promise.all([
    rpc(upstream, 'eth_chainId', [], signal), rpc(upstream, 'eth_syncing', [], signal),
    rpc(upstream, 'eth_getBlockByNumber', ['latest', false], signal),
    rpc(upstream, 'eth_getBlockByNumber', ['safe', false], signal),
    rpc(upstream, 'eth_getBlockByNumber', ['finalized', false], signal),
  ])
  if (chain !== chainId || syncing !== false) throw new Error('chain_or_sync')
  for (const b of [latest, safe, finalized]) block(b)
  if (BigInt(finalized.number) > BigInt(safe.number) || BigInt(safe.number) > BigInt(latest.number)) throw new Error('invalid_heads')
  const heights = new Map()
  for (const b of [latest, safe, finalized]) {
    const previous = heights.get(BigInt(b.number))
    if (previous && (previous.hash !== b.hash || BigInt(previous.timestamp) !== BigInt(b.timestamp))) throw new Error('inconsistent_heads')
    heights.set(BigInt(b.number), b)
  }
  return { latest, safe, finalized }
}

// Fresh tagged responses already identify a canonical block at their own height.
// Reuse them only within this probe. Different heights still require a numeric
// lookup on the higher upstream; no block/hash survives into the next probe.
function checkpointReader(upstream, heads, signal, counts) {
  const blocks = new Map(Object.values(heads).map(b => [BigInt(b.number), Promise.resolve(b)]))
  return height => {
    if (blocks.has(height)) { counts.reused++; return blocks.get(height) }
    counts.fetched++
    const pending = rpc(upstream, 'eth_getBlockByNumber', [`0x${height.toString(16)}`, false], signal).then(value => {
      if (BigInt(block(value).number) !== height) throw new Error('invalid_checkpoint_height')
      return value
    })
    blocks.set(height, pending)
    return pending
  }
}

export function createGateway(config, dependencies = {}) {
  const now = dependencies.now ?? Date.now
  const log = dependencies.log ?? (record => process.stdout.write(`${JSON.stringify(record)}\n`))
  const probeMs = config.probeMs ?? 5000
  const timeoutMs = config.timeoutMs ?? 8000
  const state = { primary: false, backup: false, checkedAt: 0, passes: 0 }
  let active = 0
  let heavy = 0
  let probing = false
  const expectedToken = Buffer.from(config.token)
  const reference = config.reference ?? config.backup
  const fresh = () => now() - state.checkedAt < probeMs * 3

  async function probe() {
    if (probing) return
    probing = true
    const signal = AbortSignal.timeout(Math.min(timeoutMs, probeMs))
    const checkpoints = { fetched: 0, reused: 0 }
    let reason = 'checkpoint_failed'
    const recordProbe = () => log({ event: 'rpc_probe', primary_ready: state.primary,
      backup_ready: state.backup, primary_unhealthy: state.primary ? 0 : 1, reason,
      checkpoint_fetches: checkpoints.fetched, checkpoint_reuses: checkpoints.reused })
    const comparisonCheck = inspect(reference, config.chainId, signal)
    const backupCheck = (reference === config.backup ? comparisonCheck : inspect(config.backup, config.chainId, signal)).then(
      value => ({ status: 'fulfilled', value }), () => ({ status: 'rejected' }),
    )
    async function updateBackup() {
      const backup = await backupCheck
      state.backup = backup.status === 'fulfilled'
      if (state.backup) {
        const age = BigInt(Math.floor(now() / 1000)) - BigInt(backup.value.latest.timestamp)
        state.backup = age >= -5n && age <= 15n
      }
    }
    try {
      const [primary, comparison] = await Promise.allSettled([
        inspect(config.primary, config.chainId, signal), comparisonCheck,
      ])
      let agrees = false
      reason = primary.status !== 'fulfilled' ? 'primary_inspection_failed' : 'latest_heads_diverged'
      if (primary.status === 'fulfilled' && comparison.status === 'fulfilled') {
        const p = primary.value.latest
        const b = comparison.value.latest
        const gap = BigInt(p.timestamp) - BigInt(b.timestamp)
        if (gap >= -5n && gap <= 5n) {
          // Compare canonical hashes at common latest, safe and finalized heights.
          agrees = true
          reason = 'canonical_mismatch'
          const primaryBlock = checkpointReader(config.primary, primary.value, signal, checkpoints)
          const referenceBlock = checkpointReader(reference, comparison.value, signal, checkpoints)
          for (const tag of ['latest', 'safe', 'finalized']) {
            const pb = primary.value[tag]
            const bb = comparison.value[tag]
            const common = BigInt(pb.number) < BigInt(bb.number) ? BigInt(pb.number) : BigInt(bb.number)
            reason = 'checkpoint_failed'
            const [pc, bc] = await Promise.all([primaryBlock(common), referenceBlock(common)])
            reason = 'canonical_mismatch'
            if (block(pc).hash !== block(bc).hash || BigInt(pc.number) !== BigInt(common) || BigInt(bc.number) !== BigInt(common)) { agrees = false; break }
          }
        }
      } else if (primary.status === 'fulfilled') {
        // An unverified node is not promoted during a reference outage. A previously
        // verified node may continue only while its latest head is recent.
        const age = BigInt(Math.floor(now() / 1000)) - BigInt(primary.value.latest.timestamp)
        agrees = state.primary && age >= -5n && age <= 15n
        reason = agrees ? 'reference_unavailable_previous_primary' : 'reference_unavailable'
      }
      if (primary.status === 'fulfilled') {
        state.primaryHeight = BigInt(primary.value.latest.number)
        const age = BigInt(Math.floor(now() / 1000)) - BigInt(primary.value.latest.timestamp)
        agrees &&= age >= -5n && age <= 15n
        if (age < -5n || age > 15n) reason = 'primary_stale'
      }
      await updateBackup()
      state.passes = agrees ? state.passes + 1 : 0
      state.primary = agrees && state.passes >= 2
      state.checkedAt = now()
      if (agrees && reason !== 'reference_unavailable_previous_primary') reason = state.primary ? 'ready' : 'promotion_pending'
      recordProbe()
    } catch {
      state.primary = false
      await updateBackup()
      state.passes = 0
      state.checkedAt = now()
      recordProbe()
    } finally { probing = false }
  }

  const server = http.createServer(async (req, res) => {
    // Health checks are deliberately separate from application-request counters.
    if (req.method === 'GET' && req.url === '/healthz') {
      const ready = fresh() && (state.primary || state.backup)
      res.writeHead(ready ? 200 : 503,
        { 'content-type': 'application/json', 'cache-control': 'no-store' })
      return res.end(JSON.stringify({ ready }))
    }
    const start = now()
    const diagnostic = {
      event: 'rpc_request', upstream: 'none',
      route: req.url === '/' ? 'ordinary' : req.url === '/primary' ? 'primary_only' : 'unknown',
      route_reason: 'not_selected', failover: false, attempts: [], methods: [],
      stage: 'admission', outcome: 'rejected', status: 500,
      primary_ready: state.primary, backup_ready: state.backup,
    }
    const reply = (status, value) => {
      diagnostic.status = status
      res.writeHead(status, { 'content-type': 'application/json', 'cache-control': 'no-store' })
      res.end(JSON.stringify(value))
    }
    const reject = (status, reason, publicReason = reason) => {
      diagnostic.rejection_reason = reason
      return reply(status, { error: publicReason })
    }
    let admitted = false
    let countedHeavy = false
    let role = 'none'
    try {
      if (req.method !== 'POST' || diagnostic.route === 'unknown') return reject(404, 'not_found')
      const supplied = Buffer.from((req.headers.authorization ?? '').replace(/^Bearer /, ''))
      if (supplied.length !== expectedToken.length || !timingSafeEqual(supplied, expectedToken)) return reject(401, 'unauthorized')
      if (active >= 16) return reject(429, 'busy')
      active++
      admitted = true
      diagnostic.stage = 'validation'
      const body = JSON.parse(await limitedBody(req, MAX_BODY))
      const envelope = Array.isArray(body) ? body : [body]
      // Method names are allowlisted even for rejected requests. Untrusted
      // method strings and all parameters stay out of the diagnostic record.
      diagnostic.methods = [...new Set(envelope.slice(0, 20).map(c =>
        READS.has(c?.method) || WRITES.has(c?.method) || MANAGED.has(c?.method) || c?.method === 'debug_traceCall'
          ? c.method : 'unsupported'))].sort()
      diagnostic.batch_size = Math.min(envelope.length, 21)
      const validation = validateRequest(body)
      diagnostic.stage = 'admission'
      if (validation.heavy && heavy >= 2) return reject(429, 'simulation_busy')
      if (validation.heavy) { heavy++; countedHeavy = true }
      diagnostic.stage = 'health'
      if (!fresh()) return reject(503, 'health_stale')
      const primaryOnly = req.url === '/primary'
      const managedMethod = validation.calls.some(c => MANAGED.has(c.method))
      const managed = !primaryOnly && validation.calls.some(c => requiresManagedProvider(c, state.primaryHeight))
      if (primaryOnly && managedMethod) return reject(400, 'managed_capability_requires_ordinary_route')
      diagnostic.route_reason = managed ? (managedMethod ? 'managed_method' : 'historical_state')
        : state.primary ? 'primary_healthy' : primaryOnly ? 'primary_required' : 'primary_unhealthy'
      role = managed ? (state.backup ? 'backup' : 'none')
        : state.primary ? 'primary' : (!primaryOnly && state.backup ? 'backup' : 'none')
      if (role === 'none') return reject(503, 'no_healthy_upstream')
      diagnostic.failover = role === 'backup' && !managed
      const signal = AbortSignal.timeout(validation.calls.some(c => c.method === 'debug_traceTransaction') ? 20000 : timeoutMs)
      const relay = async selectedRole => {
        diagnostic.attempts.push(selectedRole)
        const upstream = config[selectedRole]
        diagnostic.stage = validation.latestLogs ? 'normalization' : 'upstream'
        const request = validation.latestLogs ? await pinLatestLogRanges(body, upstream, signal) : body
        diagnostic.stage = 'upstream'
        return upstreamCall(upstream, request, signal)
      }
      let result
      try { result = await relay(role) } catch (error) {
        if (role === 'primary' && error instanceof UpstreamError) {
          diagnostic.primary_error = error.code
          diagnostic.primary_http_status = error.httpStatus
        }
        if (role === 'primary' && error.retryable) { state.primary = false; state.passes = 0 }
        if (role !== 'primary' || primaryOnly || validation.writable || !error.retryable || !state.backup || signal.aborted) throw error
        role = 'backup'
        diagnostic.failover = true
        diagnostic.route_reason = 'primary_upstream_failure'
        result = await relay(role)
      }
      const replies = Array.isArray(result.body) ? result.body : [result.body]
      diagnostic.rpc_error_count = replies.filter(r => Object.hasOwn(r, 'error')).length
      diagnostic.outcome = diagnostic.rpc_error_count ? 'rpc_error' : 'success'
      diagnostic.stage = 'complete'
      diagnostic.status = 200
      res.writeHead(200, { 'content-type': 'application/json', 'cache-control': 'no-store', 'x-plether-rpc-upstream': role })
      res.end(result.raw)
    } catch (error) {
      if (error instanceof UpstreamError) {
        diagnostic.outcome = 'upstream_error'
        diagnostic.upstream_error = error.code
        diagnostic.upstream_http_status = error.httpStatus
        reply(502, { error: 'upstream_unavailable_reconcile_writes_before_retry' })
      } else {
        const reason = error instanceof SyntaxError ? 'invalid_json'
          : REJECTIONS.has(error.message) ? error.message : 'invalid_request'
        reject(400, reason, 'invalid_request')
      }
    } finally {
      if (admitted) active--
      if (countedHeavy) heavy--
      log({ ...diagnostic, upstream: role, duration_ms: now() - start })
    }
  })
  server.requestTimeout = 10_000
  server.headersTimeout = 10_000
  server.maxRequestsPerSocket = 1000
  return { server, probe, state }
}

export function configFromEnv(env) {
  if (!env.RPC_GATEWAY_TOKEN || env.RPC_GATEWAY_TOKEN.length < 32) throw new Error('RPC_GATEWAY_TOKEN must have at least 32 characters')
  const endpoint = (value, allowHttp) => {
    const url = new URL(value)
    if ((!allowHttp && url.protocol !== 'https:') || !['http:', 'https:'].includes(url.protocol)
      || url.username || url.password || url.hash) throw new Error('invalid endpoint')
    return value
  }
  const backup = { url: endpoint(env.RPC_BACKUP_URL, false), token: env.RPC_BACKUP_TOKEN }
  if (env.RPC_REFERENCE_TOKEN && !env.RPC_REFERENCE_URL) throw new Error('reference token requires a reference URL')
  const referenceUrl = env.RPC_REFERENCE_URL ? endpoint(env.RPC_REFERENCE_URL, false) : backup.url
  const referenceToken = env.RPC_REFERENCE_TOKEN ?? (referenceUrl === backup.url ? backup.token : undefined)
  // The minimal pilot deliberately shares its existing Alchemy origin. A
  // separately operated reference remains optional for a later rollout.
  const reference = referenceUrl === backup.url && referenceToken === backup.token
    ? backup : { url: referenceUrl, token: referenceToken }
  return {
    token: env.RPC_GATEWAY_TOKEN,
    chainId: '0x66eee', // Arbitrum Sepolia 421614; this pilot cannot select mainnet.
    primary: { url: endpoint(env.RPC_PRIMARY_URL, true) },
    backup,
    reference,
  }
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  const { server, probe } = createGateway(configFromEnv(process.env))
  const timer = setInterval(() => void probe(), 5000)
  void probe()
  server.listen(3000, '0.0.0.0')
  process.on('SIGTERM', () => { clearInterval(timer); server.close(() => process.exit(0)) })
}
