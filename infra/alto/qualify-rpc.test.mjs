import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { readFileSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import test from 'node:test'
import { parseIdleMetrics, runQualification } from './qualify-rpc.mjs'

const TOKEN = 'canary-auth-private-test-value-123456789'
const BACKUP = 'https://arb-sepolia.g.alchemy.com/v2/YOUR_KEY'
const BASELINE = 'http://internal-plether-sepolia-alto-123.ap-southeast-1.elb.amazonaws.com/'
const rpcEnv = {
  PLETHER_ALTO_QUALIFY_MODE: 'rpc', ALTO_RPC_URL: 'https://rpc-internal.sepolia.plether.com/',
  PLETHER_ALTO_RPC_AUTH_TOKEN: TOKEN, PLETHER_ALTO_BACKUP_RPC_URL: BACKUP,
  ALTO_CHAIN_TYPE: 'arbitrum', PLETHER_ALTO_NETWORK_CHAIN_ID: '421614',
  ALTO_ENTRYPOINTS: '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108',
  ALTO_ENTRYPOINT_SIMULATION_CONTRACT_V8: '0x1111111111111111111111111111111111111111',
  ALTO_PIMLICO_SIMULATION_CONTRACT: '0x2222222222222222222222222222222222222222',
}
const idleEnv = { PLETHER_ALTO_QUALIFY_MODE: 'idle', PLETHER_ALTO_BASELINE_URL: BASELINE }
const GOOD_METRICS = `# TYPE alto_user_operations_in_mempool_count gauge
alto_user_operations_in_mempool_count{status="outstanding"} 0
alto_executor_wallets_available_count 4
alto_executor_wallets_total_count 4
process_start_time_seconds 1234
`
const OVERRIDE_RESULT = `0x${'0'.repeat(60)}68ac`

function setup({ mutateRpc, mutateMetrics, barrier, staleMetrics, fallbackPrimary, fallbackWrongChain } = {}) {
  let clock = 1700000000000
  let sample = 0
  const requests = []
  const events = []
  const configs = []
  let fixtureClosed = false
  const json = (body, status = 200, role = 'primary') => new Response(JSON.stringify(body), {
    status, headers: { 'Content-Type': 'application/json', 'x-plether-rpc-upstream': role },
  })
  const fetchImpl = async (input, init) => {
    const url = new URL(input)
    const headers = new Headers(init.headers)
    requests.push({ url, init, headers })
    events.push(`${init.method} ${url.pathname}`)
    assert.equal(init.redirect, 'error')
    assert.ok(init.signal instanceof AbortSignal)
    if (url.hostname === '127.0.0.1') {
      assert.equal(headers.get('authorization'), `Bearer ${TOKEN}`)
      return url.pathname === '/primary' ? json({ error: 'no_healthy_upstream' }, fallbackPrimary ? 200 : 503)
        : json({ jsonrpc: '2.0', id: 1, result: fallbackWrongChain ? '0x1' : '0x66eee' }, 200, 'backup')
    }
    if (url.hostname.startsWith('internal-plether-sepolia-alto-')) {
      assert.equal(headers.get('authorization'), null)
      if (init.method === 'POST') {
        assert.deepEqual(JSON.parse(init.body), { jsonrpc: '2.0', id: 1, method: 'eth_chainId', params: [] })
        return barrier ? barrier() : json({ error: 'alto_rpc_cutover_in_progress' }, 503)
      }
      assert.equal(url.pathname, '/metrics')
      sample++
      return new Response(mutateMetrics ? mutateMetrics(sample, GOOD_METRICS) : GOOD_METRICS, {
        headers: { Date: new Date(clock - (staleMetrics ? 60000 : 0)).toUTCString() },
      })
    }
    assert.equal(url.hostname, 'rpc-internal.sepolia.plether.com')
    assert.equal(init.method, 'POST')
    assert.equal(headers.get('authorization'), url.pathname === '/primary' ? `Bearer ${TOKEN}` : null,
      'ordinary route must prove the actual preload supplies authentication')
    const request = JSON.parse(init.body)
    const { method, params } = request
    const number = method === 'eth_getBlockByNumber' && params[0].startsWith('0x') ? params[0] : '0x64'
    let value
    switch (method) {
      case 'eth_chainId': value = '0x66eee'; break
      case 'eth_syncing': value = false; break
      case 'eth_blockNumber': value = '0x64'; break
      case 'eth_getBlockByNumber': value = {
        number, hash: `0x${'a'.repeat(64)}`, timestamp: '0x' + Math.floor(clock / 1000).toString(16), baseFeePerGas: '0x1',
      }; break
      case 'eth_getBalance': value = '0x0'; break
      case 'eth_getStorageAt': value = `0x${'0'.repeat(64)}`; break
      case 'eth_getCode': value = params[0] === `0x${'0'.repeat(40)}` ? '0x' : '0x6000'; break
      case 'eth_call': value = params.length === 3 ? OVERRIDE_RESULT : '0x'; break
      case 'eth_feeHistory': value = {
        oldestBlock: '0x5a', baseFeePerGas: Array(11).fill('0x1'), gasUsedRatio: Array(10).fill(0.5), reward: Array.from({ length: 10 }, () => ['0x1']),
      }; break
      case 'eth_gasPrice': value = '0x1'; break
      case 'debug_traceCall':
        assert.equal(params[2].tracer, 'callTracer')
        assert.ok(params[2].stateOverrides)
        value = { type: 'CALL', output: OVERRIDE_RESULT }; break
      case 'eth_getLogs':
        assert.equal(params[0].toBlock, 'latest')
        assert.equal(params[0].fromBlock, '0x0')
        assert.equal(params[0].address, rpcEnv.ALTO_ENTRYPOINTS)
        value = []; break
      default: assert.fail(`unexpected read method ${method}`)
    }
    const response = { jsonrpc: '2.0', id: request.id, result: value }
    return mutateRpc ? mutateRpc({ method, params, response, json, url }) : json(response)
  }
  const gatewayFactory = (config, dependencies) => {
    configs.push(config)
    assert.equal(config.primary.url, 'http://127.0.0.1:1')
    assert.equal(config.backup.url, BACKUP)
    assert.equal(config.reference, config.backup)
    assert.equal(config.token, TOKEN)
    dependencies.log({ event: 'ignored_fixture_log' })
    const server = new EventEmitter()
    server.listen = (port, host, ready) => { assert.equal(port, 0); assert.equal(host, '127.0.0.1'); queueMicrotask(ready) }
    server.address = () => ({ port: 12345 })
    server.closeAllConnections = () => {}
    server.close = done => { fixtureClosed = true; done() }
    return { server, state: { primary: false, backup: true }, probe: async () => {} }
  }
  return { fetchImpl, gatewayFactory, requests, configs, events, now: () => clock,
    sleep: async milliseconds => { assert.ok([22000, 5000].includes(milliseconds)); events.push(`sleep ${milliseconds}`); clock += milliseconds },
    fixtureClosed: () => fixtureClosed }
}

test('RPC canary proves primary capabilities and isolated fallback without any signer or baseline metrics', async () => {
  const harness = setup()
  const proof = await runQualification({ ...harness, env: rpcEnv })
  assert.equal(proof.scope, 'rpc')
  assert.equal(proof.required_checks_complete, true)
  assert.ok(proof.tests.every(test => test.pass))
  assert.equal(proof.idle_sample_count, 0)
  assert.ok(proof.rpc_request_count > 20)
  assert.equal(harness.configs.length, 1)
  assert.equal(harness.fixtureClosed(), true)
  assert.equal(harness.requests.filter(request => request.url.pathname === '/metrics').length, 0)
  const serialized = JSON.stringify(proof)
  for (const secret of [TOKEN, BACKUP, rpcEnv.ALTO_RPC_URL]) assert.equal(serialized.includes(secret), false)
})

test('idle observer requires exact admission barrier and two fresh zero samples from the same process', async () => {
  const harness = setup()
  const proof = await runQualification({ ...harness, env: idleEnv })
  assert.equal(proof.scope, 'idle')
  assert.equal(proof.required_checks_complete, true)
  assert.equal(proof.idle_sample_count, 2)
  assert.equal(proof.finished_at_ms - proof.started_at_ms, 27000)
  assert.equal(proof.idle_observed_at_ms, proof.finished_at_ms)
  assert.deepEqual(harness.events, ['POST /', 'sleep 22000', 'POST /', 'GET /metrics', 'sleep 5000', 'POST /', 'GET /metrics'])
  assert.equal(harness.configs.length, 0)
  assert.equal(proof.rpc_request_count, 0)
})

test('the grace never starts until the admission barrier is observed and is rechecked afterwards', async () => {
  let calls = 0
  const before = setup({ barrier: () => new Response('{}', { status: 200 }) })
  assert.equal((await runQualification({ ...before, env: idleEnv })).required_checks_complete, false)
  assert.deepEqual(before.events, ['POST /'])
  const after = setup({ barrier: () => new Response(JSON.stringify({ error: 'alto_rpc_cutover_in_progress' }), { status: ++calls === 1 ? 503 : 200 }) })
  assert.equal((await runQualification({ ...after, env: idleEnv })).required_checks_complete, false)
  assert.deepEqual(after.events, ['POST /', 'sleep 22000', 'POST /'])
})

test('mode and exact gateway configuration fail closed before making requests', async () => {
  for (const env of [
    {}, { ...rpcEnv, PLETHER_ALTO_QUALIFY_MODE: 'full' },
    { ...rpcEnv, ALTO_RPC_URL: 'https://example.com/' },
    { ...rpcEnv, PLETHER_ALTO_RPC_AUTH_TOKEN: TOKEN + '\n' },
    { ...rpcEnv, PLETHER_ALTO_BACKUP_RPC_URL: 'https://arb-sepolia.g.alchemy.com.evil.test/v2/key' },
    { ...rpcEnv, ALTO_EXECUTOR_PRIVATE_KEYS: 'never-accept-signing-material' },
    { ...rpcEnv, ALTO_UTILITY_PRIVATE_KEY: 'never-accept-signing-material' },
    { ...rpcEnv, ALTO_SEND_TRANSACTION_RPC_URL: BACKUP },
    { ...idleEnv, PLETHER_ALTO_RPC_AUTH_TOKEN: TOKEN },
    { ...idleEnv, PLETHER_ALTO_BASELINE_URL: 'http://example.com/' },
  ]) {
    const harness = setup()
    const proof = await runQualification({ ...harness, env })
    assert.equal(proof.required_checks_complete, false)
    assert.equal(harness.requests.length, 0)
    assert.equal(JSON.stringify(proof).includes('never-accept-signing-material'), false)
  }
})

test('the self-contained source runs under the ECS inline entrypoint and emits one safe failure event', () => {
  const source = readFileSync(new URL('./qualify-rpc.mjs', import.meta.url), 'utf8')
  const child = spawnSync(process.execPath, ['--input-type=module', '-e', source], {
    encoding: 'utf8', env: { PATH: process.env.PATH, PLETHER_ALTO_QUALIFY_MODE: 'invalid',
      PLETHER_ALTO_RPC_AUTH_TOKEN: TOKEN, PLETHER_ALTO_BACKUP_RPC_URL: BACKUP },
  })
  assert.equal(child.status, 1)
  const lines = child.stdout.trim().split('\n')
  assert.equal(lines.length, 1)
  const proof = JSON.parse(lines[0])
  assert.equal(proof.event, 'alto_rpc_qualification')
  assert.equal(proof.scope, 'invalid')
  assert.equal(proof.required_checks_complete, false)
  assert.equal(child.stderr, '')
  for (const secret of [TOKEN, BACKUP]) assert.equal(child.stdout.includes(secret), false)
})

test('ordinary reads routed through backup cannot masquerade as primary qualification', async () => {
  const harness = setup({ mutateRpc: ({ response, json, url }) => json(response, 200, url.pathname === '/primary' ? 'primary' : 'backup') })
  const proof = await runQualification({ ...harness, env: rpcEnv })
  assert.equal(proof.required_checks_complete, false)
  assert.equal(proof.tests.find(test => test.name === 'chain_and_primary_route').pass, false)
})

test('matching null state results cannot masquerade as a successful state read', async () => {
  const harness = setup({ mutateRpc: ({ method, response, json }) => json(method === 'eth_getBalance'
    ? { ...response, result: null } : response) })
  const proof = await runQualification({ ...harness, env: rpcEnv })
  assert.equal(proof.required_checks_complete, false)
  assert.equal(proof.tests.find(test => test.name === 'safe_state_reads').pass, false)
})

test('a wrong chain, missing overrides and malformed fee history remain required failures', async () => {
  for (const brokenMethod of ['eth_chainId', 'eth_call', 'debug_traceCall', 'eth_feeHistory']) {
    const harness = setup({ mutateRpc: ({ method, response, json }) => json(method === brokenMethod
      ? { ...response, result: method === 'eth_chainId' ? '0x1' : null } : response) })
    const proof = await runQualification({ ...harness, env: rpcEnv })
    assert.equal(proof.required_checks_complete, false)
  }
})

test('fallback fixture must actually use backup and refuse the strict-primary route', async () => {
  for (const options of [{ fallbackPrimary: true }, { fallbackWrongChain: true }]) {
    const harness = setup(options)
    const proof = await runQualification({ ...harness, env: rpcEnv })
    assert.equal(proof.required_checks_complete, false)
    assert.equal(proof.tests.find(test => test.name === 'isolated_fallback').pass, false)
    assert.equal(harness.fixtureClosed(), true)
  }
})

test('an absent gauge is unknown rather than zero; busy, malformed and duplicate observations fail closed', () => {
  assert.deepEqual(parseIdleMetrics(GOOD_METRICS), { available: 4, total: 4, processStartedAt: 1234 })
  for (const text of [
    GOOD_METRICS.replace(/^alto_user_operations_in_mempool_count.*\n/m, ''),
    GOOD_METRICS.replace('status="outstanding"} 0', 'status="outstanding"} 1'),
    GOOD_METRICS.replace('available_count 4', 'available_count 3'),
    GOOD_METRICS.replace('total_count 4', 'total_count NaN'),
    GOOD_METRICS.replace('status="outstanding"} 0', 'status="outstanding"} -1'),
    GOOD_METRICS + 'alto_user_operations_in_mempool_count{status="outstanding"} 0\n',
    GOOD_METRICS + 'alto_user_operations_in_mempool_count{status="processing"} 1\n',
  ]) assert.throws(() => parseIdleMetrics(text), /canary_check_failed/)
})

test('a changed process, stale metrics or an absent admission barrier cannot authorize a restart', async () => {
  const variants = [
    { mutateMetrics: (sample, text) => sample === 2 ? text.replace('seconds 1234', 'seconds 1235') : text },
    { staleMetrics: true },
    { barrier: () => new Response(JSON.stringify({ jsonrpc: '2.0', id: 1, result: '0x66eee' }), { status: 200 }) },
    { barrier: () => new Response(JSON.stringify({ error: 'unrelated_failure' }), { status: 503 }) },
    { barrier: () => new Response(JSON.stringify({ error: 'alto_rpc_cutover_in_progress', ignored: true }), { status: 503 }) },
  ]
  for (const options of variants) {
    const harness = setup(options)
    const proof = await runQualification({ ...harness, env: idleEnv })
    assert.equal(proof.required_checks_complete, false)
    assert.equal(proof.idle_sample_count, 0)
  }
})

test('transport timeouts are bounded and never include provider details in the proof', async () => {
  const proof = await runQualification({ env: idleEnv, requestTimeoutMs: 5, deadlineMs: 100,
    fetchImpl: (_url, init) => new Promise((_resolve, reject) => {
      init.signal.addEventListener('abort', () => reject(new Error(TOKEN + BACKUP)), { once: true })
    }) })
  assert.equal(proof.required_checks_complete, false)
  assert.equal(JSON.stringify(proof).includes(TOKEN), false)
  assert.equal(JSON.stringify(proof).includes(BACKUP), false)
})
