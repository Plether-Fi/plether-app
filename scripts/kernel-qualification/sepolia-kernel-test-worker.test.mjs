import assert from 'node:assert/strict'
import { test } from 'node:test'
import worker from './sepolia-kernel-test-worker.mjs'

const origin = 'https://app.sepolia.plether.com'
const endpoint = '/api/perps/v1/aa/kernel/rpc'
const mount = `${origin}/kernel-test`

test('serves only the opt-in host and its path', async () => {
  const env = { ASSETS: { fetch: () => assert.fail('unexpected asset read') } }
  for (const url of [origin, `${origin}/kernel-tests/`, 'https://example.com/kernel-test/']) {
    assert.equal((await worker.fetch(new Request(url), env)).status, 404)
  }
})

test('serves mounted assets and falls back only for document requests', async () => {
  const calls = []
  const env = { ASSETS: { fetch(request) {
    const path = new URL(request.url).pathname
    calls.push(path)
    return new Response(null, { status: path === '/' ? 200 : 404 })
  } } }
  assert.equal((await worker.fetch(new Request(`${mount}/vaults`, { headers: { Accept: 'text/html' } }), env)).status, 200)
  assert.equal((await worker.fetch(new Request(`${mount}/assets/missing.css`), env)).status, 404)
  assert.equal((await worker.fetch(new Request(`${mount}/api/unknown`, { headers: { Accept: 'text/html' } }), env)).status, 404)
  assert.deepEqual(calls, ['/vaults', '/', '/assets/missing.css'])
})

test('proxies only same-origin Kernel POSTs without credentials or redirects', async t => {
  const payload = '{"jsonrpc":"2.0","method":"plether_getKernelManifest","id":1,"params":[]}'
  const proxy = t.mock.method(globalThis, 'fetch', async (url, options) => {
    assert.equal(url, `https://plether-kernel-canary.pages.dev${endpoint}`)
    assert.deepEqual(options.headers, { 'Content-Type': 'application/json' })
    assert.equal(options.redirect, 'manual')
    assert.equal(await new Response(options.body).text(), payload)
    return new Response('{}', { headers: { 'Set-Cookie': 'upstream=true', 'Cache-Control': 'public' } })
  })
  assert.equal((await worker.fetch(new Request(`${mount}${endpoint}`), {})).status, 405)
  assert.equal((await worker.fetch(new Request(`${mount}${endpoint}`, { method: 'POST' }), {})).status, 403)
  const response = await worker.fetch(new Request(`${mount}${endpoint}`, {
    method: 'POST', body: payload, headers: { Origin: origin, Cookie: 'wallet=example', 'X-Plether-AA-Proxy-Token': 'untrusted' },
  }), {})
  assert.equal(proxy.mock.callCount(), 1)
  assert.equal(response.status, 200)
  assert.equal(response.headers.get('Set-Cookie'), null)
  assert.equal(response.headers.get('Cache-Control'), 'no-store')
})
