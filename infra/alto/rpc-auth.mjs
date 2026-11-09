// Pinned Alto v1.2.7's CLI does not expose HTTP header options. Keep its RPC
// clients and payloads intact, and authenticate only its fixed private gateway.
export const ALTO_RPC_GATEWAY_URL = 'https://rpc-internal.sepolia.plether.com/'

function rpcConfiguration(env) {
  if (env.ALTO_RPC_URL !== ALTO_RPC_GATEWAY_URL) throw new Error('alto_rpc_auth_invalid_primary')
  if (env.ALTO_SEND_TRANSACTION_RPC_URL !== undefined && env.ALTO_SEND_TRANSACTION_RPC_URL !== ''
    && env.ALTO_SEND_TRANSACTION_RPC_URL !== ALTO_RPC_GATEWAY_URL) {
    throw new Error('alto_rpc_auth_invalid_send_route')
  }
  const token = env.PLETHER_ALTO_RPC_AUTH_TOKEN
  // A token must also be safe as an HTTP field value. Error messages deliberately
  // contain no supplied values, endpoint details, or request data.
  if (typeof token !== 'string' || token.length < 32 || token.length > 4096 || !/^[\x21-\x7e]+$/.test(token)) {
    throw new Error('alto_rpc_auth_invalid_token')
  }
  return `Bearer ${token}`
}

function isGatewayRequest(input) {
  const value = input instanceof Request ? input.url : input instanceof URL ? input.href : input
  if (typeof value !== 'string') return false
  try {
    // URL normalization permits the normal default port, but credentials,
    // queries, fragments, alternate origins and alternate paths never match.
    return new URL(value).href === ALTO_RPC_GATEWAY_URL
  } catch {
    return false
  }
}

/** Decorate the native fetch boundary used by pinned Alto's viem transports. */
export function createAltoRpcFetch(env, originalFetch) {
  const authorization = rpcConfiguration(env)
  if (typeof originalFetch !== 'function') throw new Error('alto_rpc_auth_fetch_unavailable')
  return function authenticatedAltoFetch(input, init) {
    if (!isGatewayRequest(input)) return originalFetch(input, init)
    // RequestInit headers replace Request headers under native fetch semantics.
    const headers = new Headers(init?.headers ?? (input instanceof Request ? input.headers : undefined))
    const supplied = headers.get('authorization')
    if (supplied !== null && supplied !== authorization) throw new Error('alto_rpc_auth_header_conflict')
    headers.set('authorization', authorization)
    return originalFetch(input, { ...init, headers, redirect: 'error' })
  }
}

export function installAltoRpcAuth(env = process.env, target = globalThis) {
  const authenticatedFetch = createAltoRpcFetch(env, target.fetch)
  target.fetch = authenticatedFetch
  return authenticatedFetch
}

// Normal `node --test` imports without RPC configuration remain inert. A real
// Alto process has its mandatory ALTO_RPC_URL and must authenticate or fail.
if (['ALTO_RPC_URL', 'ALTO_SEND_TRANSACTION_RPC_URL', 'PLETHER_ALTO_RPC_AUTH_TOKEN']
  .some(name => process.env[name] !== undefined)) installAltoRpcAuth()
