// Executed only in a short-lived VPC task using the gateway's secret injection.
// The failed-primary instance is an in-process fixture; the live node is never
// stopped or disconnected to prove ordinary fallback.
import { createGateway, configFromEnv } from '/app/server.mjs'
const origin = 'https://rpc-internal.sepolia.plether.com'
const token = process.env.RPC_GATEWAY_TOKEN
const zero = `0x${'0'.repeat(40)}`
let failures = 0
function check(test, pass, fields = {}) {
  console.log(JSON.stringify({ event: 'rpc_app_qualification', test, pass, ...fields }))
  if (!pass) failures++
}
async function rpc(url, method, params = []) {
  const response = await fetch(url, {
    method: 'POST', headers: { 'content-type': 'application/json', authorization: `Bearer ${token}` },
    body: JSON.stringify({ jsonrpc: '2.0', id: 1, method, params }), signal: AbortSignal.timeout(20000),
  })
  const body = await response.json()
  if (!response.ok || body.id !== 1 || body.error) throw new Error('rpc_probe_failed')
  return { result: body.result, role: response.headers.get('x-plether-rpc-upstream') }
}
try {
  const chain = await rpc(origin + '/', 'eth_chainId')
  check('ordinary_route_uses_primary', chain.result === '0x66eee' && chain.role === 'primary')
  const direct = await rpc(origin + '/primary', 'eth_chainId')
  check('direct_route_uses_primary', direct.result === '0x66eee' && direct.role === 'primary')
  const old = await rpc(origin + '/', 'eth_getBalance', [zero, '0x1'])
  check('old_state_uses_managed_origin', typeof old.result === 'string' && old.role === 'backup')
  const receipt = await rpc(origin + '/', 'eth_getUserOperationReceipt', [`0x${'0'.repeat(64)}`])
  check('legacy_receipt_uses_managed_origin', receipt.result === null && receipt.role === 'backup')
  const samples = []
  for (let i = 0; i < 10; i++) {
    await Promise.all(Array.from({ length: 4 }, async () => {
      const start = performance.now()
      const response = await rpc(origin + '/', 'eth_getBalance', [zero, 'latest'])
      if (response.role !== 'primary') throw new Error('live_probe_fell_back')
      samples.push(performance.now() - start)
    }))
  }
  samples.sort((a, b) => a - b)
  const p95 = samples[Math.ceil(samples.length * 0.95) - 1]
  check('bounded_concurrent_read_canary', p95 < 2000, { requests: samples.length, concurrency: 4, p95_ms: Math.round(p95) })

  const fixture = createGateway({ ...configFromEnv(process.env), primary: { url: 'http://127.0.0.1:1' } }, { log: () => {} })
  await fixture.probe()
  await new Promise(resolve => fixture.server.listen(0, '127.0.0.1', resolve))
  try {
    const local = `http://127.0.0.1:${fixture.server.address().port}`
    const fallback = await rpc(local + '/', 'eth_chainId')
    check('failed_primary_uses_alchemy', fallback.result === '0x66eee' && fallback.role === 'backup')
    const rejected = await fetch(local + '/primary', {
      method: 'POST', headers: { authorization: `Bearer ${token}` },
      body: JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'eth_chainId', params: [] }),
    })
    check('failed_direct_primary_stays_closed', rejected.status === 503)
    await rejected.body?.cancel()
  } finally { fixture.server.closeAllConnections(); await new Promise(resolve => fixture.server.close(resolve)) }
} catch { check('transport_or_rpc', false) }
check('required_checks_complete', failures === 0)
process.exitCode = failures ? 1 : 0
