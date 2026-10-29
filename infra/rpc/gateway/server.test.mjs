import { after, test } from 'node:test'
import assert from 'node:assert/strict'
import http from 'node:http'
import { createGateway, configFromEnv, validateRequest } from './server.mjs'

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
async function fixture() {
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
      if (state[role].status) { res.writeHead(state[role].status); return res.end() }
      const answer = c => {
        let result
        if (c.method === 'eth_chainId') result = state[role].chain ?? '0x66eee'
        else if (c.method === 'eth_syncing') result = state[role].syncing ?? false
        else if (c.method === 'eth_getBlockByNumber') {
          result = {
            number: '0x100', timestamp: `0x${(Math.floor(clock / 1000) - (state[role].lag ?? 0)).toString(16)}`,
            hash: state[role].hash ?? hash,
          }
        } else result = '0x100'
        return state[role].rpcError ? { jsonrpc: '2.0', id: c.id, error: { code: -32000, message: 'execution reverted' } }
          : { jsonrpc: '2.0', id: c.id, result }
      }
      res.setHeader('content-type', 'application/json')
      res.end(JSON.stringify(Array.isArray(body) ? body.map(answer) : answer(body)))
    })) }
  }
  endpoints.backup.token = 'upstream-private-key'
  const gateway = createGateway({ ...endpoints, token, chainId: '0x66eee', timeoutMs: 1000 }, {
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

test('allowlist rejects signing, historical tracing, notifications, duplicate IDs and unbounded log scans', () => {
  for (const method of ['eth_sendTransaction', 'personal_sign', 'debug_traceTransaction', 'admin_peers']) {
    assert.throws(() => validateRequest(request(method)))
  }
  assert.throws(() => validateRequest({ jsonrpc: '2.0', method: 'eth_call', params: [] }))
  assert.throws(() => validateRequest([request(), request()]))
  assert.throws(() => validateRequest({ ...request('eth_getLogs'), params: [{ fromBlock: 'earliest', toBlock: 'latest' }] }))
  assert.throws(() => validateRequest({ ...request('eth_getLogs'), params: [{ fromBlock: '0x1', toBlock: '0xffff' }] }))
  validateRequest({ ...request('eth_getLogs'), params: [{ fromBlock: '0x100', toBlock: '0x200' }] })
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
  f.upstream.primary.status = 401
  f.state.primary = true
  before = calls.length
  assert.equal((await f.send()).status, 502)
  assert.deepEqual(calls.slice(before).map(c => c.role), ['primary'])
  f.upstream.primary.status = 0
  f.upstream.primary.rpcError = true
  before = calls.length
  const response = await f.send()
  assert.equal(response.status, 200)
  assert.equal((await response.json()).error.code, -32000)
  assert.deepEqual(calls.slice(before).map(c => c.role), ['primary'])
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
