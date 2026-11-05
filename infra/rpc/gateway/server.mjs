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
const HEX = /^0x[0-9a-fA-F]+$/
const HASH = /^0x[0-9a-fA-F]{64}$/
const MAX_BODY = 512 * 1024
const MAX_RESPONSE = 8 * 1024 * 1024

class UpstreamError extends Error {
  constructor(message, retryable = true) { super(message); this.retryable = retryable }
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
  for (const call of calls) {
    if (!call || Array.isArray(call) || call.jsonrpc !== '2.0'
      || !['string', 'number'].includes(typeof call.id)
      || (typeof call.id === 'number' && !Number.isSafeInteger(call.id))
      || String(call.id).length > 128 || ids.has(JSON.stringify(call.id))
      || !(READS.has(call.method) || WRITES.has(call.method) || call.method === 'debug_traceCall')
      || !Array.isArray(call.params)) throw new Error('invalid_request')
    ids.add(JSON.stringify(call.id))
    if (call.method === 'eth_getLogs') {
      const filter = call.params[0]
      if (!filter || typeof filter !== 'object') throw new Error('invalid_log_filter')
      if (filter.blockHash) {
        if (!HASH.test(filter.blockHash) || filter.fromBlock || filter.toBlock) throw new Error('invalid_log_filter')
      } else if (!HEX.test(filter.fromBlock) || !HEX.test(filter.toBlock)
        || BigInt(filter.toBlock) < BigInt(filter.fromBlock)
        || BigInt(filter.toBlock) - BigInt(filter.fromBlock) > 2000n) {
        throw new Error('log_range_requires_explicit_bounded_blocks')
      }
    }
  }
  if (calls.filter(c => c.method === 'debug_traceCall').length > 1) throw new Error('one_simulation_per_request')
  return { calls, writable: calls.some(c => WRITES.has(c.method)), heavy: calls.some(c => c.method === 'debug_traceCall') }
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
      throw new UpstreamError('upstream_http', [429, 502, 503, 504].includes(response.status))
    }
    const raw = await limitedBody(response.body, MAX_RESPONSE)
    const body = JSON.parse(raw)
    if (!validResponse(body, request)) throw new UpstreamError('upstream_protocol')
    return { raw, body }
  } catch (error) {
    // Never expose URL-embedded credentials, headers or provider error messages.
    throw new UpstreamError('upstream_unavailable', error instanceof UpstreamError ? error.retryable : true)
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
  return { latest, safe, finalized }
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
      if (primary.status === 'fulfilled' && comparison.status === 'fulfilled') {
        const p = primary.value.latest
        const b = comparison.value.latest
        const gap = BigInt(p.timestamp) - BigInt(b.timestamp)
        if (gap >= -5n && gap <= 5n) {
          // Compare canonical hashes at common latest, safe and finalized heights.
          agrees = true
          for (const tag of ['latest', 'safe', 'finalized']) {
            const pb = primary.value[tag]
            const bb = comparison.value[tag]
            const common = BigInt(pb.number) < BigInt(bb.number) ? pb.number : bb.number
            const [pc, bc] = await Promise.all([
              rpc(config.primary, 'eth_getBlockByNumber', [common, false], signal),
              rpc(reference, 'eth_getBlockByNumber', [common, false], signal),
            ])
            if (block(pc).hash !== block(bc).hash || BigInt(pc.number) !== BigInt(common) || BigInt(bc.number) !== BigInt(common)) { agrees = false; break }
          }
        }
      } else if (primary.status === 'fulfilled') {
        // An unverified node is not promoted during a reference outage. A previously
        // verified node may continue only while its latest head is recent.
        const age = BigInt(Math.floor(now() / 1000)) - BigInt(primary.value.latest.timestamp)
        agrees = state.primary && age >= -5n && age <= 15n
      }
      if (primary.status === 'fulfilled') {
        const age = BigInt(Math.floor(now() / 1000)) - BigInt(primary.value.latest.timestamp)
        agrees &&= age >= -5n && age <= 15n
      }
      await updateBackup()
      state.passes = agrees ? state.passes + 1 : 0
      state.primary = agrees && state.passes >= 2
      state.checkedAt = now()
      log({ event: 'rpc_probe', primary_ready: state.primary, backup_ready: state.backup, primary_unhealthy: state.primary ? 0 : 1 })
    } catch {
      state.primary = false
      await updateBackup()
      state.passes = 0
      state.checkedAt = now()
      log({ event: 'rpc_probe', primary_ready: false, backup_ready: state.backup, primary_unhealthy: 1 })
    } finally { probing = false }
  }

  const server = http.createServer(async (req, res) => {
    const reply = (status, value) => {
      res.writeHead(status, { 'content-type': 'application/json', 'cache-control': 'no-store' })
      res.end(JSON.stringify(value))
    }
    if (req.method === 'GET' && req.url === '/healthz') {
      return reply(fresh() && (state.primary || state.backup) ? 200 : 503, { ready: fresh() && (state.primary || state.backup) })
    }
    if (req.method !== 'POST' || !['/', '/primary'].includes(req.url)) return reply(404, { error: 'not_found' })
    const supplied = Buffer.from((req.headers.authorization ?? '').replace(/^Bearer /, ''))
    if (supplied.length !== expectedToken.length || !timingSafeEqual(supplied, expectedToken)) return reply(401, { error: 'unauthorized' })
    if (active >= 16) return reply(429, { error: 'busy' })
    active++
    let isHeavy = false
    let countedHeavy = false
    let role = 'none'
    const start = now()
    try {
      const body = JSON.parse(await limitedBody(req, MAX_BODY))
      const validation = validateRequest(body)
      isHeavy = validation.heavy
      if (isHeavy && heavy >= 2) return reply(429, { error: 'simulation_busy' })
      if (isHeavy) { heavy++; countedHeavy = true }
      if (!fresh()) return reply(503, { error: 'health_stale' })
      const primaryOnly = req.url === '/primary'
      role = state.primary ? 'primary' : (!primaryOnly && state.backup ? 'backup' : 'none')
      if (role === 'none') return reply(503, { error: 'no_healthy_upstream' })
      const signal = AbortSignal.timeout(timeoutMs)
      let result
      try { result = await upstreamCall(config[role], body, signal) } catch (error) {
        if (role === 'primary' && error.retryable) { state.primary = false; state.passes = 0 }
        if (role !== 'primary' || primaryOnly || validation.writable || !error.retryable || !state.backup || signal.aborted) throw error
        role = 'backup'
        result = await upstreamCall(config.backup, body, signal)
      }
      res.writeHead(200, { 'content-type': 'application/json', 'cache-control': 'no-store' })
      res.end(result.raw)
    } catch (error) {
      reply(error instanceof UpstreamError ? 502 : 400, { error: error instanceof UpstreamError ? 'upstream_unavailable_reconcile_writes_before_retry' : 'invalid_request' })
    } finally {
      active--
      if (countedHeavy) heavy--
      log({ event: 'rpc_request', upstream: role, duration_ms: now() - start })
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
