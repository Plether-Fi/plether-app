import { after, test } from 'node:test'
import assert from 'node:assert/strict'
import http from 'node:http'
import { createGateway, configFromEnv, validateRequest, requiresManagedProvider } from './server.mjs'

const token = 'qualification-only-token-never-used-in-aws'
const hash = `0x${'a'.repeat(64)}`
const calls = []
const closers = []
after(async () => { for (const server of closers) { server.closeAllConnections(); await new Promise(r => server.close(r)) } })

async function listen(server) {
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve))
  closers.push(server)
  return `http://127.0.0.1:${server.address().port}`
}
const request = (method = 'eth_blockNumber', id = 7) => ({ jsonrpc: '2.0', id, method, params: [] })
const latestLogs = (fromBlock = '0x100', id = 7) => ({
  ...request('eth_getLogs', id),
  params: [{ address: `0x${'1'.repeat(40)}`, fromBlock, toBlock: 'latest', topics: [hash] }],
})
async function bounded(promise) {
  let timer
  try {
    return await Promise.race([promise, new Promise((_, reject) => {
      timer = setTimeout(() => reject(new Error('fixture_request_not_started')), 2000)
    })])
  } finally { clearTimeout(timer) }
}
async function fixture({ sharedReference = false } = {}) {
  let clock = Date.now()
  const logs = []
  const state = { primary: {}, backup: {}, reference: {} }
  const endpoints = {}
  for (const role of ['primary', 'backup', 'reference']) {
    endpoints[role] = { url: await listen(http.createServer(async (req, res) => {
      let data = ''
      for await (const chunk of req) data += chunk
      const body = JSON.parse(data)
      calls.push({ role, body, authorization: req.headers.authorization })
      const batch = Array.isArray(body) ? body : [body]
      if (batch.some(c => c.method === 'eth_blockNumber')) {
        state[role].onBlockNumber?.()
        if (state[role].blockNumberWait) await state[role].blockNumberWait
      }
      if (batch.some(c => c.method === 'debug_traceCall')) {
        state[role].onTrace?.()
        if (state[role].traceWait) await state[role].traceWait
      }
      if (state[role].status) { res.writeHead(state[role].status); return res.end() }
      if (state[role].logStatus && batch.some(c => c.method === 'eth_getLogs')) {
        res.writeHead(state[role].logStatus); return res.end()
      }
      const answer = c => {
        let result
        if (c.method === 'eth_chainId') result = state[role].chain ?? '0x66eee'
        else if (c.method === 'eth_blockNumber') result = state[role].height ?? '0x100'
        else if (c.method === 'eth_getLogs') result = []
        else if (c.method === 'eth_syncing') result = state[role].syncing ?? false
        else if (c.method === 'eth_getBlockByNumber') {
          result = {
            number: /^0x/.test(c.params[0]) ? c.params[0] : '0x100', timestamp: `0x${(Math.floor(clock / 1000) - (state[role].lag ?? 0)).toString(16)}`,
            hash: state[role].hash ?? hash,
            ...state[role].blocks?.[c.params[0]],
          }
        } else result = '0x100'
        const blockFailure = state[role].blockLookupError && c.method === 'eth_getBlockByNumber' && /^0x/.test(c.params[0])
        return state[role].rpcError || blockFailure ? { jsonrpc: '2.0', id: c.id, error: { code: -32000, message: 'execution reverted' } }
          : { jsonrpc: '2.0', id: c.id, result }
      }
      res.setHeader('content-type', 'application/json')
      res.end(JSON.stringify(Array.isArray(body) ? body.map(answer) : answer(body)))
    })) }
  }
  endpoints.backup.token = 'upstream-private-key'
  const gateway = createGateway({ ...endpoints, reference: sharedReference ? endpoints.backup : endpoints.reference, token, chainId: '0x66eee', timeoutMs: 1000 }, {
    now: () => clock, log: log => logs.push(log),
  })
  const url = await listen(gateway.server)
  const send = (body = request(), path = '/', auth = token) => fetch(`${url}${path}`, {
    method: 'POST', headers: { authorization: `Bearer ${auth}` }, body: JSON.stringify(body),
  })
  await gateway.probe()
  await gateway.probe()
  return { ...gateway, state: gateway.state, upstream: state, send, url, logs, advance: ms => { clock += ms } }
}

test('allowlist rejects signing, invalid traces, notifications, duplicate IDs and unbounded log scans', () => {
  for (const method of ['eth_sendTransaction', 'personal_sign', 'debug_traceTransaction', 'admin_peers']) {
    assert.throws(() => validateRequest(request(method)))
  }
  assert.throws(() => validateRequest({ jsonrpc: '2.0', method: 'eth_call', params: [] }))
  assert.throws(() => validateRequest([request(), request()]))
  assert.throws(() => validateRequest({ ...request('eth_getLogs'), params: [{ fromBlock: 'earliest', toBlock: 'latest' }] }))
  assert.throws(() => validateRequest({ ...request('eth_getLogs'), params: [{ fromBlock: '0x1', toBlock: '0xffff' }] }))
  validateRequest({ ...request('eth_getLogs'), params: [{ fromBlock: '0x100', toBlock: '0x200' }] })
})

test('Viem zero-argument requests may omit params on primary and fallback routes', async () => {
  const f = await fixture()
  const body = { jsonrpc: '2.0', id: 'viem-chain', method: 'eth_chainId' }
  const primary = await f.send(body)
  assert.equal(primary.status, 200)
  assert.equal(primary.headers.get('x-plether-rpc-upstream'), 'primary')
  assert.deepEqual(await primary.json(), { jsonrpc: '2.0', id: 'viem-chain', result: '0x66eee' })
  f.state.primary = false
  const backup = await f.send(body)
  assert.equal(backup.status, 200)
  assert.equal(backup.headers.get('x-plether-rpc-upstream'), 'backup')
  await backup.body.cancel()
  for (const method of ['eth_call', 'eth_getLogs', 'eth_getBalance', 'eth_sendRawTransaction']) {
    assert.throws(() => validateRequest({ ...body, method }))
  }
  for (const params of [null, {}]) assert.throws(() => validateRequest({ ...body, params }))
})

test('managed receipts, bounded historical traces and old state use Alchemy without demoting the primary', async () => {
  const f = await fixture()
  f.state.primaryHeight = 1000000n
  const managed = [
    { ...request('eth_getUserOperationReceipt'), params: [hash] },
    { ...request('debug_traceTransaction'), params: [hash, { tracer: 'callTracer', timeout: '20s' }] },
    { ...request('eth_call'), params: [{ to: `0x${'0'.repeat(40)}`, data: '0x' }, '0x1'] },
  ]
  for (const body of managed) {
    assert.equal((await f.send(body)).status, 200)
    assert.equal(calls.at(-1).role, 'backup')
    assert.equal(f.state.primary, true)
    assert.equal(f.logs.at(-1).failover, false)
    assert.equal(f.logs.at(-1).route_reason, body.method === 'eth_call' ? 'historical_state' : 'managed_method')
  }
  assert.equal((await f.send()).status, 200)
  assert.equal(calls.at(-1).role, 'primary')
  const before = calls.length
  assert.equal((await f.send(managed[0], '/primary')).status, 400)
  assert.equal(calls.length, before)
})

test('Alto latest log polls pin a fresh selected-upstream head without using probe height or mutating the request', async () => {
  const f = await fixture()
  f.state.primaryHeight = 0x100n
  f.upstream.primary.height = '0x2f3'
  const body = latestLogs('0x100')
  const before = calls.length
  const response = await f.send([request('eth_chainId', 1), body], '/primary')
  assert.equal(response.status, 200)
  assert.equal(response.headers.get('x-plether-rpc-upstream'), 'primary')
  const sent = calls.slice(before)
  assert.equal(sent.length, 2)
  assert.equal(sent[0].body.method, 'eth_blockNumber')
  assert.equal(sent[1].body[1].params[0].toBlock, '0x2f3')
  assert.equal(body.params[0].toBlock, 'latest')
  assert.deepEqual(sent[1].body[1].params[0].topics, [hash])
  await response.body.cancel()
})

test('latest log ranges retain the 5,000-block bound before forwarding a scan', async () => {
  const f = await fixture()
  f.upstream.primary.height = '0x1388'
  let before = calls.length
  const allowed = await f.send(latestLogs('0x1'))
  assert.equal(allowed.status, 200)
  assert.equal(calls.slice(before).filter(c => c.body.method === 'eth_getLogs').length, 1)
  await allowed.body.cancel()
  for (const height of ['0x1389', '0x0']) {
    f.upstream.primary.height = height
    before = calls.length
    const denied = await f.send(latestLogs('0x1'))
    assert.equal(denied.status, 400)
    assert.equal(f.logs.at(-1).rejection_reason, 'log_range_requires_explicit_bounded_blocks')
    assert.equal(f.logs.at(-1).stage, 'normalization')
    assert.deepEqual(calls.slice(before).map(c => c.body.method), ['eth_blockNumber'])
    assert.equal(f.state.primary, true)
    await denied.body.cancel()
  }
})

test('latest log filters require valid addresses and numeric starts before any upstream access', async () => {
  const f = await fixture()
  for (const changes of [
    { address: undefined }, { address: [] }, { address: '0x123' },
    { address: ['0x123'] }, { fromBlock: undefined }, { fromBlock: 'earliest' },
    { blockHash: hash },
  ]) {
    const body = latestLogs()
    Object.assign(body.params[0], changes)
    const before = calls.length
    const response = await f.send(body)
    assert.equal(response.status, 400)
    assert.equal(calls.length, before)
    await response.body.cancel()
  }
  const body = latestLogs()
  const before = calls.length
  const invalidBatch = await f.send([body, request('personal_sign', 2)])
  assert.equal(invalidBatch.status, 400)
  assert.equal(calls.length, before)
  await invalidBatch.body.cancel()
})

test('read fallback re-pins latest logs to the backup head; direct-primary requests never fall back', async () => {
  const f = await fixture()
  f.upstream.primary.height = '0x120'
  f.upstream.backup.height = '0x140'
  f.upstream.primary.logStatus = 503
  const before = calls.length
  const response = await f.send(latestLogs())
  assert.equal(response.status, 200)
  assert.equal(response.headers.get('x-plether-rpc-upstream'), 'backup')
  assert.deepEqual(calls.slice(before).map(c => [c.role, c.body.method, c.body.params[0]?.toBlock]), [
    ['primary', 'eth_blockNumber', undefined], ['primary', 'eth_getLogs', '0x120'],
    ['backup', 'eth_blockNumber', undefined], ['backup', 'eth_getLogs', '0x140'],
  ])
  await response.body.cancel()
  f.state.primary = true
  const directBefore = calls.length
  const direct = await f.send(latestLogs(), '/primary')
  assert.equal(direct.status, 502)
  assert.deepEqual(calls.slice(directBefore).map(c => c.role), ['primary', 'primary'])
  await direct.body.cancel()
})

test('a latest log request cannot replay a batch containing a raw send', async () => {
  const f = await fixture()
  const raw = { ...request('eth_sendRawTransaction', 2), params: ['0xdeadbeef'] }
  f.upstream.primary.height = '0x120'
  f.upstream.primary.logStatus = 503
  const before = calls.length
  const response = await f.send([latestLogs('0x100', 1), raw])
  assert.equal(response.status, 502)
  assert.deepEqual(calls.slice(before).map(c => c.role), ['primary', 'primary'])
  assert.deepEqual(calls.at(-1).body[1], raw)
  await response.body.cancel()
})

test('latest-head normalization respects authentication, stale probes and active-request limits first', async () => {
  const f = await fixture()
  let before = calls.length
  const unauthorized = await f.send(latestLogs(), '/', 'wrong')
  assert.equal(unauthorized.status, 401)
  assert.equal(calls.length, before)
  await unauthorized.body.cancel()
  f.advance(16_000)
  const stale = await f.send(latestLogs())
  assert.equal(stale.status, 503)
  assert.equal(calls.length, before)
  await stale.body.cancel()
  await f.probe()
  await f.probe()
  let release
  f.upstream.primary.blockNumberWait = new Promise(resolve => { release = resolve })
  let started = 0
  let allStarted
  const barrier = new Promise(resolve => { allStarted = resolve })
  f.upstream.primary.onBlockNumber = () => { if (++started === 16) allStarted() }
  const active = Array.from({ length: 16 }, () => f.send(latestLogs()))
  try {
    await bounded(barrier)
    before = calls.length
    const busy = await f.send(latestLogs())
    assert.equal(busy.status, 429)
    assert.equal(f.logs.at(-1).rejection_reason, 'busy')
    assert.equal(f.logs.at(-1).upstream, 'none')
    assert.equal(calls.length, before)
    await busy.body.cancel()
  } finally { release() }
  const responses = await Promise.all(active)
  assert.equal(responses.every(response => response.status === 200), true)
  await Promise.all(responses.map(response => response.body.cancel()))
})

test('heavy-request admission rejects a third simulation before its latest-head lookup', async () => {
  const f = await fixture()
  let release
  f.upstream.primary.traceWait = new Promise(resolve => { release = resolve })
  let started = 0
  let allStarted
  const barrier = new Promise(resolve => { allStarted = resolve })
  f.upstream.primary.onTrace = () => { if (++started === 2) allStarted() }
  const body = [request('debug_traceCall', 1), latestLogs('0x100', 2)]
  const active = [f.send(body), f.send(body)]
  try {
    await bounded(barrier)
    const before = calls.length
    const busy = await f.send(body)
    assert.equal(busy.status, 429)
    assert.equal(f.logs.at(-1).rejection_reason, 'simulation_busy')
    assert.equal(f.logs.at(-1).upstream, 'none')
    assert.equal(calls.length, before)
    await busy.body.cancel()
  } finally { release() }
  const responses = await Promise.all(active)
  assert.equal(responses.every(response => response.status === 200), true)
  await Promise.all(responses.map(response => response.body.cancel()))
})

test('a latest-head timeout consumes the request deadline without forwarding logs or replaying writes', async () => {
  const f = await fixture()
  let release
  f.upstream.primary.blockNumberWait = new Promise(resolve => { release = resolve })
  const before = calls.length
  const body = [latestLogs('0x100', 1), { ...request('eth_sendRawTransaction', 2), params: ['0xdeadbeef'] }]
  try {
    const response = await f.send(body)
    assert.equal(response.status, 502)
    assert.deepEqual(calls.slice(before).map(c => [c.role, c.body.method]), [['primary', 'eth_blockNumber']])
    await response.body.cancel()
  } finally { release() }
})

test('state history routing is explicit and leaves latest/safe/finalized and the retained window on Nitro', () => {
  const call = tag => ({ ...request('eth_getBalance'), params: [`0x${'0'.repeat(40)}`, tag] })
  for (const tag of ['latest', 'safe', 'finalized', 'pending', '0xf4240', '0x9fc40']) {
    assert.equal(requiresManagedProvider(call(tag), 1000000n), false)
  }
  for (const tag of ['earliest', '0x1', { blockNumber: '0x1' }, { blockHash: hash }]) {
    assert.equal(requiresManagedProvider(call(tag), 1000000n), true)
  }
  assert.throws(() => validateRequest({ ...request('debug_traceTransaction'), params: [hash, { tracer: '{}', timeout: '20s' }] }))
  assert.throws(() => validateRequest({ ...request('debug_traceTransaction'), params: [hash, { tracer: 'callTracer', timeout: '21s' }] }))
})

test('primary promotion requires two probes and forwards valid JSON-RPC', async () => {
  const f = await fixture()
  assert.equal(f.state.primary, true)
  const response = await f.send()
  assert.equal(response.status, 200)
  assert.deepEqual(await response.json(), { jsonrpc: '2.0', id: 7, result: '0x100' })
  f.upstream.primary.syncing = { currentBlock: '0x1' }
  await f.probe()
  assert.equal(f.state.primary, false)
  f.upstream.primary.syncing = false
  await f.probe()
  assert.equal(f.state.primary, false)
  await f.probe()
  assert.equal(f.state.primary, true)
})

test('wrong chain, divergent canonical hashes and stale heads remove primary; direct route never falls back', async () => {
  for (const invalid of [{ chain: '0xa4b1' }, { hash: `0x${'b'.repeat(64)}` }, { lag: 30 }]) {
    const f = await fixture()
    Object.assign(f.upstream.primary, invalid)
    await f.probe()
    assert.equal(f.state.primary, false)
    const before = calls.length
    assert.equal((await f.send(request(), '/primary')).status, 503)
    assert.equal(calls.length, before)
    assert.equal((await f.send()).status, 200)
    assert.equal(calls.at(-1).role, 'backup')
    assert.equal(calls.at(-1).authorization, 'Bearer upstream-private-key')
  }
})

test('transport failures retry reads once; contract errors and permanent HTTP errors do not', async () => {
  const f = await fixture()
  f.upstream.primary.status = 503
  let before = calls.length
  assert.equal((await f.send()).status, 200)
  assert.deepEqual(calls.slice(before).map(c => c.role), ['primary', 'backup'])
  assert.equal(f.logs.at(-1).failover, true)
  assert.equal(f.logs.at(-1).route_reason, 'primary_upstream_failure')
  assert.equal(f.logs.at(-1).primary_error, 'upstream_http')
  assert.equal(f.logs.at(-1).primary_http_status, 503)
  assert.deepEqual(f.logs.at(-1).attempts, ['primary', 'backup'])
  f.upstream.primary.status = 401
  f.state.primary = true
  before = calls.length
  assert.equal((await f.send()).status, 502)
  assert.deepEqual(calls.slice(before).map(c => c.role), ['primary'])
  assert.equal(f.logs.at(-1).failover, false)
  assert.equal(f.logs.at(-1).outcome, 'upstream_error')
  assert.equal(f.logs.at(-1).upstream_http_status, 401)
  f.upstream.primary.status = 0
  f.upstream.primary.rpcError = true
  before = calls.length
  const response = await f.send()
  assert.equal(response.status, 200)
  assert.equal((await response.json()).error.code, -32000)
  assert.deepEqual(calls.slice(before).map(c => c.role), ['primary'])
  assert.equal(f.logs.at(-1).outcome, 'rpc_error')
  assert.equal(f.logs.at(-1).rpc_error_count, 1)
  assert.equal(f.logs.at(-1).failover, false)
})

test('ambiguous raw sends and batches containing writes are never replayed', async () => {
  const tx = { ...request('eth_sendRawTransaction'), params: ['0xdeadbeef'] }
  for (const body of [tx, [request('eth_blockNumber', 1), tx]]) {
    const f = await fixture()
    f.upstream.primary.status = 503
    const before = calls.length
    assert.equal((await f.send(body)).status, 502)
    assert.deepEqual(calls.slice(before).map(c => c.role), ['primary'])
    assert.deepEqual(calls.at(-1).body, body)
    assert.equal(f.logs.at(-1).failover, false)
    assert.equal(f.logs.at(-1).status, 502)
  }
})

test('authentication, stale probes and request limits fail closed without leaking credentials', async () => {
  const f = await fixture()
  assert.equal((await f.send(request(), '/', 'wrong')).status, 401)
  assert.equal((await fetch(`${f.url}/healthz`)).status, 200)
  assert.equal((await f.send(Array.from({ length: 21 }, (_, i) => request('eth_call', i)))).status, 400)
  f.advance(16_000)
  assert.equal((await f.send()).status, 503)
  assert.equal((await fetch(`${f.url}/healthz`)).status, 503)
  assert.doesNotMatch(JSON.stringify(f.logs), /upstream-private-key|127\.0\.0\.1|qualification-only-token/)
})

test('production configuration pins Sepolia, requires authentication and HTTPS upstream backup', () => {
  const env = { RPC_GATEWAY_TOKEN: token, RPC_PRIMARY_URL: 'http://10.0.1.1:8547', RPC_BACKUP_URL: 'https://fixture.invalid/v2', RPC_REFERENCE_URL: 'https://independent.fixture.invalid' }
  assert.equal(configFromEnv(env).chainId, '0x66eee')
  assert.throws(() => configFromEnv({ ...env, RPC_GATEWAY_TOKEN: '' }))
  assert.throws(() => configFromEnv({ ...env, RPC_BACKUP_URL: 'http://fixture.invalid' }))
})

test('Alchemy backup outage does not prevent primary qualification against independent reference', async () => {
  const f = await fixture()
  f.upstream.backup.status = 503
  f.state.primary = false
  f.state.passes = 0
  await f.probe()
  await f.probe()
  assert.equal(f.state.primary, true)
  assert.equal(f.state.backup, false)
  assert.equal((await f.send(request(), '/primary')).status, 200)
})

test('minimal configuration reuses the Alchemy backup URL and credentials for reference', () => {
  const env = { RPC_GATEWAY_TOKEN: token, RPC_PRIMARY_URL: 'http://10.0.1.1:8547', RPC_BACKUP_URL: 'https://arb-sepolia.g.alchemy.com/v2', RPC_BACKUP_TOKEN: 'fixture-only' }
  const config = configFromEnv(env)
  assert.equal(config.reference, config.backup)
  assert.equal(config.reference.token, 'fixture-only')
  assert.equal(configFromEnv({ ...env, RPC_REFERENCE_URL: env.RPC_BACKUP_URL }).reference.url, env.RPC_BACKUP_URL)
  assert.throws(() => configFromEnv({ ...env, RPC_REFERENCE_TOKEN: 'orphan-token' }))
})

test('shared reference qualifies primary and avoids duplicate standby probes', async () => {
  const f = await fixture({ sharedReference: true })
  const before = calls.length
  await f.probe()
  assert.equal(f.state.primary, true)
  const backupCalls = calls.slice(before).filter(c => c.role === 'backup')
  assert.equal(backupCalls.filter(c => c.body.method === 'eth_chainId').length, 1)
  assert.equal(backupCalls.length, 5)
  assert.deepEqual(f.logs.at(-1), {
    event: 'rpc_probe', primary_ready: true, backup_ready: true, primary_unhealthy: 0,
    reason: 'ready', checkpoint_fetches: 0, checkpoint_reuses: 6,
  })
})

test('a failed primary checkpoint query preserves the healthy shared backup', async () => {
  const f = await fixture({ sharedReference: true })
  f.upstream.primary.blocks = Object.fromEntries(['latest', 'safe', 'finalized'].map(tag => [tag, { number: '0x101' }]))
  f.upstream.primary.blockLookupError = true
  await f.probe()
  assert.equal(f.state.primary, false)
  assert.equal(f.state.backup, true)
  assert.equal((await f.send(request(), '/primary')).status, 503)
  assert.equal((await f.send()).status, 200)
  assert.equal(calls.at(-1).role, 'backup')
})

test('unequal heads fetch only missing common heights and coalesce repeated checkpoints within a probe', async () => {
  const f = await fixture({ sharedReference: true })
  f.upstream.primary.blocks = Object.fromEntries(['latest', 'safe', 'finalized'].map(tag => [tag, { number: '0x101' }]))
  const before = calls.length
  await f.probe()
  assert.equal(f.state.primary, true)
  const sent = calls.slice(before)
  assert.equal(sent.filter(c => c.role === 'backup').length, 5)
  assert.equal(sent.filter(c => c.role === 'primary').length, 6)
  assert.deepEqual(sent.filter(c => /^0x/.test(c.body.params[0])).map(c => [c.role, c.body.params[0]]), [['primary', '0x100']])
  assert.equal(f.logs.at(-1).checkpoint_fetches, 1)
  assert.equal(f.logs.at(-1).checkpoint_reuses, 5)
  // A previously fetched numeric checkpoint must also be refreshed next time.
  const next = calls.length
  await f.probe()
  assert.equal(calls.slice(next).filter(c => /^0x/.test(c.body.params[0])).length, 1)
  f.upstream.primary.blocks['0x100'] = { number: '0x101' }
  await f.probe()
  assert.equal(f.state.primary, false)
  assert.equal(f.state.backup, true)
  assert.equal(f.logs.at(-1).reason, 'checkpoint_failed')
})

test('fresh tagged checkpoints detect safe and finalized disagreement without numeric refetches', async () => {
  for (const divergent of ['safe', 'finalized']) {
    const f = await fixture({ sharedReference: true })
    for (const role of ['primary', 'backup']) {
      f.upstream[role].blocks = { latest: { number: '0x102' }, safe: { number: '0x101' }, finalized: { number: '0x100' } }
    }
    f.upstream.primary.blocks[divergent].hash = `0x${'b'.repeat(64)}`
    await f.probe()
    assert.equal(f.state.primary, false)
    assert.equal(f.state.backup, true)
    assert.equal(f.logs.at(-1).reason, 'canonical_mismatch')
    assert.equal(f.logs.at(-1).checkpoint_fetches, 0)
  }
})

test('a higher reference head fetches the common block on reference, not the already fetched primary', async () => {
  const f = await fixture({ sharedReference: true })
  f.upstream.backup.blocks = Object.fromEntries(['latest', 'safe', 'finalized'].map(tag => [tag, { number: '0x101' }]))
  const before = calls.length
  await f.probe()
  assert.equal(f.state.primary, true)
  const numeric = calls.slice(before).filter(c => /^0x/.test(c.body.params[0]))
  assert.deepEqual(numeric.map(c => [c.role, c.body.params[0]]), [['backup', '0x100']])
  f.upstream.backup.blocks['0x100'] = { hash: `0x${'b'.repeat(64)}` }
  await f.probe()
  assert.equal(f.state.primary, false)
  assert.equal(f.state.backup, true)
  assert.equal(f.logs.at(-1).reason, 'canonical_mismatch')
})

test('tagged responses are never reused across probes, including same-height reorgs', async () => {
  const f = await fixture({ sharedReference: true })
  f.upstream.primary.hash = `0x${'b'.repeat(64)}`
  await f.probe()
  assert.equal(f.state.primary, false)
  f.upstream.backup.hash = f.upstream.primary.hash
  await f.probe()
  assert.equal(f.state.primary, false)
  assert.equal(f.logs.at(-1).reason, 'promotion_pending')
  await f.probe()
  assert.equal(f.state.primary, true)
})

test('internally conflicting tagged blocks or invalid head order fail closed', async () => {
  for (const blocks of [
    { safe: { hash: `0x${'b'.repeat(64)}` } },
    { safe: { timestamp: '0x1' } },
    { safe: { number: '0x101' } },
  ]) {
    const f = await fixture({ sharedReference: true })
    f.upstream.primary.blocks = blocks
    await f.probe()
    assert.equal(f.state.primary, false)
    assert.equal(f.state.backup, true)
    assert.equal(f.logs.at(-1).reason, 'primary_inspection_failed')
  }
})

test('a reference outage cannot promote an unverified primary and stale health still expires', async () => {
  const f = await fixture({ sharedReference: true })
  f.upstream.backup.status = 503
  await f.probe()
  assert.equal(f.state.primary, true)
  assert.equal(f.state.backup, false)
  assert.equal(f.logs.at(-1).reason, 'reference_unavailable_previous_primary')
  f.state.primary = false
  f.state.passes = 0
  await f.probe()
  assert.equal(f.state.primary, false)
  assert.equal(f.logs.at(-1).reason, 'reference_unavailable')
  f.advance(16_000)
  const response = await f.send()
  assert.equal(response.status, 503)
  assert.equal(f.logs.at(-1).rejection_reason, 'health_stale')
  assert.equal(f.logs.at(-1).stage, 'health')
  await response.body.cancel()
})

test('request diagnostics distinguish healthy routing, health failover, managed routing and strict-primary rejection', async () => {
  const f = await fixture()
  const good = await f.send()
  await good.body.cancel()
  assert.equal(f.logs.at(-1).route_reason, 'primary_healthy')
  assert.equal(f.logs.at(-1).status, 200)
  assert.equal(f.logs.at(-1).failover, false)
  f.state.primary = false
  const fallback = await f.send()
  await fallback.body.cancel()
  assert.equal(f.logs.at(-1).route_reason, 'primary_unhealthy')
  assert.equal(f.logs.at(-1).failover, true)
  assert.deepEqual(f.logs.at(-1).attempts, ['backup'])
  const direct = await f.send(request(), '/primary')
  await direct.body.cancel()
  assert.equal(f.logs.at(-1).route_reason, 'primary_required')
  assert.equal(f.logs.at(-1).route, 'primary_only')
  assert.equal(f.logs.at(-1).rejection_reason, 'no_healthy_upstream')
  assert.equal(f.logs.at(-1).failover, false)
  const managed = await f.send({ ...request('debug_traceTransaction'), params: [hash, { tracer: 'callTracer' }] })
  await managed.body.cancel()
  assert.equal(f.logs.at(-1).route_reason, 'managed_method')
  assert.equal(f.logs.at(-1).failover, false)
})

test('rejection diagnostics cover validation, authorization and malformed bodies without logging sensitive input', async () => {
  const f = await fixture()
  const secret = 'sensitive-payload-credential-and-wallet'
  const invalid = await f.send({ ...request(secret), params: [secret] })
  assert.equal(invalid.status, 400)
  await invalid.body.cancel()
  assert.equal(f.logs.at(-1).rejection_reason, 'invalid_request')
  assert.deepEqual(f.logs.at(-1).methods, ['unsupported'])
  assert.equal(f.logs.at(-1).upstream, 'none')
  const unauth = await f.send(request(), '/', secret)
  await unauth.body.cancel()
  assert.equal(f.logs.at(-1).rejection_reason, 'unauthorized')
  assert.equal(f.logs.at(-1).stage, 'admission')
  const path = await fetch(`${f.url}/${secret}`)
  await path.body.cancel()
  assert.equal(f.logs.at(-1).rejection_reason, 'not_found')
  assert.equal(f.logs.at(-1).route, 'unknown')
  const privateCall = await f.send({ ...request('eth_call'), params: [{ to: `0x${'1'.repeat(40)}`, data: secret }, 'latest'] })
  await privateCall.body.cancel()
  const beforeHealth = f.logs.length
  await (await fetch(`${f.url}/healthz`)).body.cancel()
  assert.equal(f.logs.length, beforeHealth)
  for (const [body, reason] of [[`{"${secret}":`, 'invalid_json'], [secret.repeat(20000), 'body_limit']]) {
    let response
    try { response = await fetch(f.url, { method: 'POST', headers: { authorization: `Bearer ${token}` }, body }) } catch (error) {
      // An oversized incoming stream may be closed by Node before the 400 can
      // reach the client. It must still produce the bounded diagnostic record.
      assert.equal(reason, 'body_limit')
      assert.ok(['ECONNRESET', 'UND_ERR_SOCKET', 'EPIPE'].includes(error.cause?.code))
    }
    if (response) {
      assert.equal(response.status, 400)
      await response.body.cancel()
    }
    assert.equal(f.logs.at(-1).rejection_reason, reason)
    assert.equal(f.logs.at(-1).stage, 'validation')
  }
  assert.doesNotMatch(JSON.stringify(f.logs), /sensitive-payload|upstream-private-key|qualification-only-token|127\.0\.0\.1|0x1111111111111111111111111111111111111111/)
})
