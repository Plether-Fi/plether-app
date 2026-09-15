import { beforeEach, describe, expect, it, vi } from 'vitest'
import { recoveryFetch, recoveryHttp, resetRecoveryCredentialsForTests } from '../recoveryTransport'
import { isRecoveryPending } from '../errors'

const url = 'https://app.sepolia.plether.com/api/perps/v1/aa/rpc'
const hash = `0x${'a'.repeat(64)}`
const token = () => `v1.${hash}.0x${'b'.repeat(64)}.${Math.floor(Date.now() / 1000) + 600}.${'c'.repeat(64)}`
const request = (method = 'eth_getUserOperationReceipt', op = hash) => ({
  method: 'POST', body: JSON.stringify({ jsonrpc: '2.0', id: 1, method, params: [op] }),
})

describe('operation recovery transport', () => {
  beforeEach(() => { localStorage.clear(); resetRecoveryCredentialsForTests() })
  it('sends recovery capabilities only for hash-scoped preparation status reads', async () => {
    const credential = token()
    await recoveryFetch(url, vi.fn(async () => new Response('{}', { headers: { 'X-Plether-AA-Recovery': credential } })))(url, request('plether_prepareUserOperation'))
    const fetcher = vi.fn<typeof fetch>(async () => new Response('{}'))
    const read = recoveryFetch(url, fetcher)
    const body = (locator: object) => ({ method: 'POST', body: JSON.stringify({ method: 'plether_getPreparationStatus', params: [locator] }) })
    await read(url, body({ version: 1, userOperationHash: hash }))
    expect(new Headers(fetcher.mock.calls[0][1]?.headers).get('X-Plether-AA-Recovery')).toBe(credential)
    await read(url, body({ version: 1, preparationId: hash }))
    expect(new Headers(fetcher.mock.calls[1][1]?.headers).has('X-Plether-AA-Recovery')).toBe(false)
  })
  it('persists the preparation credential before submission and restores it after reload/IP change', async () => {
    const credential = token()
    const prepare = recoveryFetch(url, vi.fn(async () => new Response('{}', { headers: { 'X-Plether-AA-Recovery': credential } })))
    await prepare(url, request('plether_prepareUserOperation'))
    resetRecoveryCredentialsForTests()
    const fetcher = vi.fn<typeof fetch>(async () => new Response('{}'))
    await recoveryFetch(url, fetcher)(url, request())
    expect(new Headers(fetcher.mock.calls[0][1]?.headers).get('X-Plether-AA-Recovery')).toBe(credential)
  })
  it('never sends credentials for another hash, submissions, or a different endpoint', async () => {
    await recoveryFetch(url, vi.fn(async () => new Response('{}', { headers: { 'X-Plether-AA-Recovery': token() } })))(url, request())
    const fetcher = vi.fn<typeof fetch>(async () => new Response('{}'))
    const wrapped = recoveryFetch(url, fetcher)
    for (const input of [request('eth_sendUserOperation'), request('eth_getUserOperationReceipt', `0x${'d'.repeat(64)}`)]) {
      await wrapped(url, { ...input, headers: { 'X-Plether-AA-Recovery': 'spoofed' } })
      expect(new Headers(fetcher.mock.lastCall?.[1]?.headers).has('X-Plether-AA-Recovery')).toBe(false)
    }
    await expect(wrapped('https://unrelated.example/api', request())).rejects.toThrow('endpoint mismatch')
    expect(fetcher).toHaveBeenCalledTimes(2)
    expect(fetcher.mock.lastCall?.[1]?.redirect).toBe('error')
  })
  it('shares preparation credentials with the explicitly paired bundler route, not arbitrary endpoints', async () => {
    const paymasterUrl = `${url}/preparation`
    const credential = token()
    await recoveryFetch(paymasterUrl, vi.fn(async () => new Response('{}', { headers: { 'X-Plether-AA-Recovery': credential } })), url)(paymasterUrl, request('plether_prepareUserOperation'))
    const fetcher = vi.fn<typeof fetch>(async () => new Response('{}'))
    await recoveryFetch(url, fetcher)(url, request())
    expect(new Headers(fetcher.mock.lastCall?.[1]?.headers).get('X-Plether-AA-Recovery')).toBe(credential)
    const other = 'https://other.example/aa/rpc'
    await recoveryFetch(other, fetcher)(other, request())
    expect(new Headers(fetcher.mock.lastCall?.[1]?.headers).has('X-Plether-AA-Recovery')).toBe(false)
  })
  it('ignores expired, malformed and wrong-operation credentials', async () => {
    for (const value of ['bad', token().replace(/\.\d+\./, '.1.'), token().replace(hash, `0x${'d'.repeat(64)}`)]) {
      await recoveryFetch(url, vi.fn(async () => new Response('{}', { headers: { 'X-Plether-AA-Recovery': value } })))(url, request())
    }
    expect(localStorage.getItem('plether:aa:recovery-capabilities:v1')).toBeNull()
  })
  it('works when persistent storage is unavailable without logging the credential', async () => {
    vi.spyOn(Storage.prototype, 'setItem').mockImplementation(() => { throw new Error('unavailable') })
    const wrapped = recoveryFetch(url, vi.fn(async () => new Response('{}', { headers: { 'X-Plether-AA-Recovery': token() } })))
    await expect(wrapped(url, request())).resolves.toBeInstanceOf(Response)
    vi.restoreAllMocks()
  })
  it('retains the pending reason through the real viem transport without immediate retries', async () => {
    const fetcher = vi.spyOn(globalThis, 'fetch').mockResolvedValue(new Response(JSON.stringify({
      jsonrpc: '2.0', id: 1, error: { code: -32001, message: 'Awaiting verified evidence', data: { reason: 'RECOVERY_PENDING', retryable: true, retryAfter: 60 } },
    }), { headers: { 'Content-Type': 'application/json', 'Retry-After': '60' } }))
    const result = await recoveryHttp(url)({}).request({ method: 'eth_getUserOperationReceipt', params: [hash] }).catch((error: unknown) => error)
    expect(isRecoveryPending(result)).toBe(true)
    expect(fetcher).toHaveBeenCalledOnce()
    vi.restoreAllMocks()
  })
})
