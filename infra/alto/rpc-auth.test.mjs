import assert from 'node:assert/strict'
import { spawnSync } from 'node:child_process'
import http from 'node:http'
import { fileURLToPath } from 'node:url'
import { test } from 'node:test'
import { ALTO_RPC_GATEWAY_URL, createAltoRpcFetch, installAltoRpcAuth } from './rpc-auth.mjs'

const token = 'fixture-only-gateway-token-0123456789'
const env = { ALTO_RPC_URL: ALTO_RPC_GATEWAY_URL, PLETHER_ALTO_RPC_AUTH_TOKEN: token }
const modulePath = fileURLToPath(new URL('./rpc-auth.mjs', import.meta.url))

test('authenticates the exact gateway request while preserving request data and caller headers', async () => {
  const calls = []
  const response = new Response('{"jsonrpc":"2.0","id":7,"result":"0x66eee"}')
  const fetch = createAltoRpcFetch(env, (input, init) => { calls.push({ input, init }); return Promise.resolve(response) })
  const controller = new AbortController()
  const body = '{"jsonrpc":"2.0","id":7,"method":"eth_chainId","params":[]}'
  const headers = new Headers({ 'content-type': 'application/json', 'x-request-test': 'retained' })
  const init = { method: 'POST', body, signal: controller.signal, headers, redirect: 'follow', cache: 'no-store' }
  assert.equal(await fetch(ALTO_RPC_GATEWAY_URL, init), response)
  assert.equal(calls.length, 1)
  assert.equal(calls[0].input, ALTO_RPC_GATEWAY_URL)
  assert.equal(calls[0].init.method, init.method)
  assert.equal(calls[0].init.body, body)
  assert.equal(calls[0].init.signal, controller.signal)
  assert.equal(calls[0].init.cache, init.cache)
  assert.equal(calls[0].init.redirect, 'error')
  assert.equal(calls[0].init.headers.get('authorization'), `Bearer ${token}`)
  assert.equal(calls[0].init.headers.get('content-type'), 'application/json')
  assert.equal(calls[0].init.headers.get('x-request-test'), 'retained')
  assert.equal(headers.has('authorization'), false)
  assert.equal(init.redirect, 'follow')
})

test('preserves Request identity, inherited headers, and RequestInit header replacement', async () => {
  const calls = []
  const fetch = createAltoRpcFetch(env, (input, init) => { calls.push({ input, init }); return Promise.resolve(new Response('OK')) })
  const request = new Request(ALTO_RPC_GATEWAY_URL, { method: 'POST', body: 'unchanged', headers: { 'x-original': 'yes' } })
  await fetch(request)
  assert.equal(calls[0].input, request)
  assert.equal(calls[0].init.headers.get('x-original'), 'yes')
  assert.equal(request.headers.has('authorization'), false)
  assert.equal(request.bodyUsed, false)
  const init = { headers: { 'x-replacement': 'yes' } }
  await fetch(request, init)
  assert.equal(calls[1].input, request)
  assert.equal(calls[1].init.headers.has('x-original'), false)
  assert.equal(calls[1].init.headers.get('x-replacement'), 'yes')
  assert.deepEqual(init.headers, { 'x-replacement': 'yes' })
})

test('does not send the token to other origins, paths, queries, fragments, or credential-bearing URLs', async () => {
  const calls = []
  const fetch = createAltoRpcFetch(env, (input, init) => { calls.push({ input, init }); return Promise.resolve(new Response('OK')) })
  const init = { method: 'POST', body: 'untouched', headers: { 'x-test': 'original' }, redirect: 'follow' }
  for (const input of [
    'https://arb-sepolia.g.alchemy.com/v2/fixture-only',
    'https://rpc-internal.sepolia.plether.com.attacker.invalid/',
    'http://rpc-internal.sepolia.plether.com/',
    'https://rpc-internal.sepolia.plether.com/primary',
    'https://rpc-internal.sepolia.plether.com/?other=1',
    'https://rpc-internal.sepolia.plether.com/#fragment',
    'https://user:password@rpc-internal.sepolia.plether.com/',
    'relative-invalid-url',
  ]) {
    await fetch(input, init)
    assert.equal(calls.at(-1).input, input)
    assert.equal(calls.at(-1).init, init)
    assert.doesNotMatch(JSON.stringify(calls.at(-1)), new RegExp(token))
  }
  await fetch(new URL('https://other.invalid/'), init)
  assert.equal(calls.at(-1).init, init)
})

test('sends only one Authorization field and fails closed on conflicting credentials', async () => {
  const calls = []
  const fetch = createAltoRpcFetch(env, (input, init) => { calls.push({ input, init }); return Promise.resolve(new Response('OK')) })
  await fetch(new URL(ALTO_RPC_GATEWAY_URL), { headers: { Authorization: `Bearer ${token}` } })
  assert.equal([...calls[0].init.headers].filter(([name]) => name === 'authorization').length, 1)
  assert.throws(() => fetch(ALTO_RPC_GATEWAY_URL, { headers: { authorization: 'Bearer unrelated' } }), /^Error: alto_rpc_auth_header_conflict$/)
  assert.equal(calls.length, 1)
})

test('invalid configuration fails without including supplied credentials or URLs', () => {
  for (const changed of [
    { ALTO_RPC_URL: 'https://other.invalid/' },
    { ALTO_RPC_URL: `${ALTO_RPC_GATEWAY_URL}?key=${token}` },
    { ALTO_RPC_URL: `https://user:${token}@rpc-internal.sepolia.plether.com/` },
    { ALTO_RPC_URL: undefined },
    { ALTO_SEND_TRANSACTION_RPC_URL: 'https://other.invalid/' },
    { PLETHER_ALTO_RPC_AUTH_TOKEN: undefined },
    { PLETHER_ALTO_RPC_AUTH_TOKEN: '' },
    { PLETHER_ALTO_RPC_AUTH_TOKEN: 'a'.repeat(31) },
    { PLETHER_ALTO_RPC_AUTH_TOKEN: `${token}\r\ninjected` },
    { PLETHER_ALTO_RPC_AUTH_TOKEN: `${token} invalid` },
  ]) {
    let called = false
    assert.throws(() => createAltoRpcFetch({ ...env, ...changed }, () => { called = true }), error => {
      assert.match(error.message, /^alto_rpc_auth_invalid_(primary|send_route|token)$/)
      assert.equal(error.message.includes(token), false)
      assert.equal(error.message.includes('https://'), false)
      return true
    })
    assert.equal(called, false)
  }
  assert.throws(() => createAltoRpcFetch(env, undefined), /^Error: alto_rpc_auth_fetch_unavailable$/)
  assert.doesNotThrow(() => createAltoRpcFetch({ ...env, ALTO_SEND_TRANSACTION_RPC_URL: ALTO_RPC_GATEWAY_URL }, globalThis.fetch))
})

test('installer replaces only the supplied fetch boundary', async () => {
  const calls = []
  const target = { fetch: (...args) => { calls.push(args); return Promise.resolve(new Response('OK')) }, retained: 7 }
  const installed = installAltoRpcAuth(env, target)
  assert.equal(installed, target.fetch)
  assert.equal(target.retained, 7)
  await target.fetch(ALTO_RPC_GATEWAY_URL)
  assert.equal(calls[0][1].headers.get('authorization'), `Bearer ${token}`)
})

test('native HTTP fetch sends the authenticated body and does not follow redirects', async t => {
  const received = []
  const server = http.createServer(async (request, response) => {
    let body = ''
    for await (const chunk of request) body += chunk
    received.push({ method: request.method, authorization: request.headers.authorization, body, path: request.url })
    if (body === 'redirect') {
      response.writeHead(307, { location: '/leaked' })
      response.end()
    } else {
      response.setHeader('content-type', 'application/json')
      response.end('{"jsonrpc":"2.0","id":1,"result":"0x66eee"}')
    }
  })
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve))
  t.after(() => { server.closeAllConnections(); return new Promise(resolve => server.close(resolve)) })
  const address = `http://127.0.0.1:${server.address().port}/`
  const nativeFetch = globalThis.fetch
  // The server is an HTTP fixture for the fixed HTTPS gateway boundary.
  const fetch = createAltoRpcFetch(env, (_input, init) => nativeFetch(address, init))
  const response = await fetch(ALTO_RPC_GATEWAY_URL, { method: 'POST', body: 'original-rpc-body' })
  assert.equal((await response.json()).result, '0x66eee')
  assert.deepEqual(received[0], { method: 'POST', authorization: `Bearer ${token}`, body: 'original-rpc-body', path: '/' })
  await assert.rejects(fetch(ALTO_RPC_GATEWAY_URL, { method: 'POST', body: 'redirect' }), TypeError)
  assert.equal(received.length, 2)
  assert.equal(received.some(request => request.path === '/leaked'), false)
})

function preload(script, overrides = {}) {
  const clean = { ...process.env }
  for (const key of ['ALTO_RPC_URL', 'ALTO_SEND_TRANSACTION_RPC_URL', 'PLETHER_ALTO_RPC_AUTH_TOKEN']) delete clean[key]
  return spawnSync(process.execPath, ['--import', modulePath, '--input-type=module', '-e', script], {
    env: { ...clean, ...overrides }, encoding: 'utf8', timeout: 5000,
  })
}

test('Node preload is inert without RPC env and installs before the application when configured', () => {
  const absent = preload('if (fetch.name === "authenticatedAltoFetch") process.exit(1)')
  assert.equal(absent.status, 0)
  const present = preload('if (fetch.name !== "authenticatedAltoFetch") process.exit(1)', env)
  assert.equal(present.status, 0)
  assert.equal(present.stdout, '')
  assert.equal(present.stderr, '')
})

test('Node preload rejects missing authentication and wrong routes before application startup', () => {
  for (const overrides of [
    { ALTO_RPC_URL: ALTO_RPC_GATEWAY_URL },
    { ...env, ALTO_RPC_URL: 'https://other.invalid/' },
  ]) {
    const result = preload('console.log("APPLICATION_STARTED")', overrides)
    assert.notEqual(result.status, 0)
    assert.equal(result.stdout.includes('APPLICATION_STARTED'), false)
    assert.equal(result.stderr.includes(token), false)
    assert.equal(result.stderr.includes('https://other.invalid/'), false)
  }
})
