import assert from 'node:assert/strict'
import { createServer } from 'node:http'
import test from 'node:test'
import { createPublicClient, http } from 'viem'
import {
  customTransport,
  getPrimaryRpcAuthToken,
  primaryRpcFetchOptions,
} from './customTransport.js'

const token = 'plether-local-gateway-token-0123456789'
const logger = { error() {}, info() {} }

async function withRpcServer(check) {
  const requests = []
  const server = createServer((request, response) => {
    requests.push({ authorization: request.headers.authorization, url: request.url })
    response.writeHead(200, { 'content-type': 'application/json' })
    response.end(JSON.stringify({ jsonrpc: '2.0', id: 1, result: '0x66eee' }))
  })
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve))
  try {
    await check(`http://127.0.0.1:${server.address().port}`, requests)
  } finally {
    server.closeAllConnections()
    await new Promise(resolve => server.close(resolve))
  }
}

test('primary Alto RPC and simulation wallet send the gateway bearer', async () => {
  const previous = process.env.PLETHER_ALTO_RPC_AUTH_TOKEN
  process.env.PLETHER_ALTO_RPC_AUTH_TOKEN = token
  try {
    await withRpcServer(async (url, requests) => {
      const auth = getPrimaryRpcAuthToken()
      const publicClient = createPublicClient({
        transport: customTransport(url, {
          logger,
          fetchOptions: primaryRpcFetchOptions(url, url, auth),
        }),
      })
      const simulationWalletTransport = http(url, {
        fetchOptions: primaryRpcFetchOptions(url, url, auth),
      })
      const simulationClient = createPublicClient({ transport: simulationWalletTransport })
      assert.equal(await publicClient.getChainId(), 421614)
      assert.equal(await simulationClient.getChainId(), 421614)
      assert.equal(requests.length, 2)
      assert.deepEqual(requests.map(request => request.authorization), [
        `Bearer ${token}`,
        `Bearer ${token}`,
      ])
    })
  } finally {
    if (previous === undefined) delete process.env.PLETHER_ALTO_RPC_AUTH_TOKEN
    else process.env.PLETHER_ALTO_RPC_AUTH_TOKEN = previous
  }
})

test('a distinct transaction relay receives no primary gateway token', async () => {
  await withRpcServer(async (url, requests) => {
    const client = createPublicClient({
      transport: customTransport(url, {
        logger,
        fetchOptions: primaryRpcFetchOptions('https://private-nitro.example', url, token),
      }),
    })
    assert.equal(await client.getChainId(), 421614)
    assert.equal(requests.length, 1)
    assert.equal(requests[0].authorization, undefined)
  })
})

test('malformed token fails without including its value in the error', () => {
  const previous = process.env.PLETHER_ALTO_RPC_AUTH_TOKEN
  process.env.PLETHER_ALTO_RPC_AUTH_TOKEN = 'invalid\nsecret'
  try {
    assert.throws(getPrimaryRpcAuthToken, error => {
      assert.match(error.message, /malformed/)
      assert.doesNotMatch(error.message, /invalid|secret/)
      return true
    })
  } finally {
    if (previous === undefined) delete process.env.PLETHER_ALTO_RPC_AUTH_TOKEN
    else process.env.PLETHER_ALTO_RPC_AUTH_TOKEN = previous
  }
})
