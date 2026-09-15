import { http } from 'viem'

const HEADER = 'X-Plether-AA-Recovery'
const STORAGE = 'plether:aa:recovery-capabilities:v1'
const MAX_ENTRIES = 1000
const READS = new Set(['eth_getUserOperationReceipt', 'eth_getUserOperationByHash', 'pimlico_getUserOperationStatus'])
const memory = new Map<string, string>()

function bound(entries: Map<string, string>) {
  while (entries.size > MAX_ENTRIES) {
    const oldest = entries.keys().next().value
    if (oldest === undefined) break
    entries.delete(oldest)
  }
}

function parse(token: string): { hash: string; expires: number } | undefined {
  const match = /^v1\.(0x[0-9a-f]{64})\.0x[0-9a-f]{64}\.(\d{1,12})\.[0-9a-f]{64}$/.exec(token)
  if (!match || Number(match[2]) * 1000 <= Date.now()) return undefined
  return { hash: match[1], expires: Number(match[2]) }
}

function credentials(): Map<string, string> {
  // Merge across tabs; never let disabled/full storage interrupt a transaction.
  try {
    const saved: unknown = JSON.parse(localStorage.getItem(STORAGE) ?? '{}')
    if (saved && typeof saved === 'object' && !Array.isArray(saved)) {
      for (const [key, token] of Object.entries(saved).slice(-MAX_ENTRIES)) {
        if (typeof token === 'string' && parse(token)) memory.set(key, token)
      }
    }
  } catch { /* In-memory recovery remains available. */ }
  for (const [key, token] of memory) if (!parse(token)) memory.delete(key)
  bound(memory)
  return memory
}

/** Credentials are sent only on exact-endpoint, hash-scoped recovery reads.
 * Saved from preparation responses before signing/submission, including the
 * response-lost case. These credentials cannot authorize spending. */
export function recoveryFetch(rpcUrl: string, fetcher: typeof fetch = fetch, credentialScope = rpcUrl): typeof fetch {
  const endpoint = new URL(rpcUrl, globalThis.location.origin).href
  const scope = new URL(credentialScope, globalThis.location.origin).href
  return async (input, init) => {
    const target = new URL(input instanceof Request ? input.url : String(input), globalThis.location.origin).href
    if (target !== endpoint) throw new Error('Recovery transport endpoint mismatch')
    const headers = new Headers(init?.headers)
    headers.delete(HEADER)
    let hash: string | undefined
    try {
      const body: unknown = JSON.parse(typeof init?.body === 'string' ? init.body : '{}')
      if (body && typeof body === 'object' && 'method' in body && typeof body.method === 'string' && READS.has(body.method)
        && 'params' in body && Array.isArray(body.params) && body.params.length === 1 && typeof body.params[0] === 'string') {
        hash = body.params[0].toLowerCase()
      }
      if (body && typeof body === 'object' && 'method' in body && body.method === 'plether_getPreparationStatus'
        && 'params' in body && Array.isArray(body.params) && body.params.length === 1) {
        const locator = body.params[0] as { userOperationHash?: unknown; preparationId?: unknown }
        if (locator && locator.preparationId === undefined && typeof locator.userOperationHash === 'string') hash = locator.userOperationHash.toLowerCase()
      }
    } catch { /* Upstream validates malformed requests. */ }
    const saved = hash && credentials().get(`${scope}|${hash}`)
    if (saved) headers.set(HEADER, saved)
    const response = await fetcher(input, { ...init, headers, redirect: 'error' })
    const token = response.headers.get(HEADER)
    const parsed = token && parse(token)
    if (parsed && (!hash || parsed.hash === hash)) {
      const entries = credentials()
      entries.set(`${scope}|${parsed.hash}`, token)
      bound(entries)
      try { localStorage.setItem(STORAGE, JSON.stringify(Object.fromEntries(entries))) } catch { /* Memory fallback. */ }
    }
    return response
  }
}

export function recoveryHttp(url: string, options: Parameters<typeof http>[1] = {}, credentialScope = url) {
  return http(url, { ...options, fetchFn: recoveryFetch(url, fetch, credentialScope) })
}

export function resetRecoveryCredentialsForTests() { memory.clear() }
