#!/usr/bin/env node
// Read-only browser-shaped AA requests through the existing localhost proxy.
// Include Origin: originless CLI requests cannot detect backend CORS failures.
import assert from 'node:assert/strict'

for (const origin of ['http://localhost:5173', 'http://127.0.0.1:5173']) {
  for (const method of ['eth_supportedEntryPoints', 'pimlico_getUserOperationGasPrice']) {
    const response = await fetch('http://127.0.0.1:5173/api/perps/v1/aa/rpc', {
      method: 'POST', headers: { Origin: origin, 'Content-Type': 'application/json' },
      body: JSON.stringify({ jsonrpc: '2.0', id: 1, method, params: [] }),
      signal: AbortSignal.timeout(15000),
    })
    assert.equal(response.status, 200, `${origin}: ${method}`)
    assert(response.headers.get('content-type')?.includes('application/json'))
    const body = await response.json()
    assert(!body.error && body.result)
    console.log(JSON.stringify({ origin, method, status: response.status }))
  }
}
const denied = await fetch('http://127.0.0.1:5173/api/perps/v1/aa/rpc', {
  method: 'POST', headers: { Origin: 'https://evil.invalid', 'Content-Type': 'application/json' },
  body: JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'eth_supportedEntryPoints', params: [] }),
  signal: AbortSignal.timeout(15000),
})
assert.equal(denied.status, 403)
console.log('Untrusted browser origin remains rejected (403).')
