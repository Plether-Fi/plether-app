import { afterEach, describe, expect, it, vi } from 'vitest'
import { createWalletPreparationRecovery, PREPARATION_RECOVERY_HEADER, recoveryChallengeMessage, parseWalletRecoveryResult } from '../walletRecovery'
import { preparationIdentifier } from '../nativePreparation'
import type { Address, Hex } from 'viem'
const owner = `0x${'1'.repeat(40)}` as Address
const sender = `0x${'2'.repeat(40)}` as Address
const paymaster = `0x${'3'.repeat(40)}` as Address
const id = '12345678-1234-4123-8123-123456789abc'
const token = 'b'.repeat(64)
const missing = { version: 1, recoveryState: 'missing', reason: 'PREPARATION_NOT_CREATED', canRetire: true, operationHashes: [] }
function setup() {
  const calls: { method: string; headers: Headers; params: Record<string, unknown>[] }[] = []
  let alter = false
  const sign = vi.fn(async () => `0x${'12'.repeat(65)}` as Hex)
  const fetcher = vi.fn(async (_url: URL | RequestInfo, init?: RequestInit) => {
    const body = JSON.parse(String(init?.body)) as { method: string; params: Record<string, unknown>[] }
    calls.push({ ...body, headers: new Headers(init?.headers) })
    let result: unknown = missing
    if (body.method === 'plether_getRecoveryChallenge') {
      const nonce = 'a'.repeat(64), expiresAt = Math.floor(Date.now() / 1000) + 300
      result = { version: 1, challengeId: nonce, expiresAt,
        message: recoveryChallengeMessage({ origin: location.origin, chainId: 421614, paymaster, sender,
          preparationId: preparationIdentifier(id), owner, nonce, expiresAt }) + (alter ? '\nextra instruction' : '') }
    }
    if (body.method === 'plether_verifyRecoveryChallenge') result = { version: 1, sessionToken: token, expiresIn: 900 }
    if (body.method === 'plether_retirePreparation') result = { ...missing, recoveryState: 'retired', reason: 'PREPARATION_RETIRED', canRetire: false }
    return new Response(JSON.stringify({ result }), { headers: { 'Content-Type': 'application/json' } })
  })
  const api = createWalletPreparationRecovery({ rpcUrl: '/api/perps/v1/aa/rpc', chainId: 421614, paymaster, sender, owner, signMessage: sign, fetcher })
  return { api, sign, fetcher, calls, tamper: () => { alter = true } }
}
afterEach(() => { vi.useRealTimers() })
describe('wallet preparation recovery', () => {
  it('only signs after explicit verification and sends the scoped session on recovery requests', async () => {
    const { api, sign, calls } = setup()
    expect(sign).not.toHaveBeenCalled()
    await api.verify(id)
    expect(sign).toHaveBeenCalledTimes(1)
    expect(calls.map(call => call.method)).toEqual(['plether_getRecoveryChallenge', 'plether_verifyRecoveryChallenge'])
    await expect(api.status(id)).resolves.toEqual(missing)
    expect(calls.at(-1)?.headers.get(PREPARATION_RECOVERY_HEADER)).toBe(token)
    expect(api.headers('another-preparation')).toEqual({})
    const hash = `0x${'a'.repeat(64)}` as Hex
    expect(api.operationHeaders(hash)).toEqual({})
    api.bindOperation(id, hash)
    expect(api.operationHeaders(hash)[PREPARATION_RECOVERY_HEADER]).toBe(token)
    expect(api.operationHeaders(`0x${'b'.repeat(64)}`)).toEqual({})
    await expect(api.retire(id)).resolves.toMatchObject({ recoveryState: 'retired' })
    expect(sign).toHaveBeenCalledTimes(1)
    expect(calls.every(call => !/sendUserOperation|prepareUserOperation/.test(call.method))).toBe(true)
  })
  it('rejects altered wallet messages before opening the wallet', async () => {
    const { api, sign, tamper } = setup()
    tamper()
    await expect(api.verify(id)).rejects.toMatchObject({ reason: 'INVALID_RECOVERY_RESPONSE' })
    expect(sign).not.toHaveBeenCalled()
  })
  it('expires sessions in memory and never restores them from another runtime', async () => {
    vi.useFakeTimers()
    const { api } = setup()
    await api.verify(id)
    expect(api.headers(id)[PREPARATION_RECOVERY_HEADER]).toBe(token)
    expect(setup().api.headers(id)).toEqual({})
    await vi.advanceTimersByTimeAsync(900_000)
    expect(api.headers(id)).toEqual({})
  })
  it('does not expose raw backend errors or credentials', async () => {
    const fetcher = vi.fn(async () => new Response(JSON.stringify({ error: { message: `secret ${token}`, data: { reason: 'RECOVERY_VERIFICATION_REQUIRED' } } }), { status: 403 }))
    const api = createWalletPreparationRecovery({ rpcUrl: '/api/perps/v1/aa/rpc', chainId: 421614, paymaster, sender, owner, signMessage: vi.fn(), fetcher })
    await expect(api.status(id)).rejects.toThrow('Verify your owner wallet')
    await expect(api.status(id)).rejects.not.toThrow(token)
  })
  it('bounds and validates recovered hashes and retirement authority', () => {
    expect(() => parseWalletRecoveryResult({ ...missing, canRetire: 'true' })).toThrow()
    expect(() => parseWalletRecoveryResult({ ...missing, operationHashes: ['arbitrary'] })).toThrow()
    expect(() => parseWalletRecoveryResult({ ...missing, operationHashes: Array(21).fill(`0x${'1'.repeat(64)}`) })).toThrow()
  })
})
