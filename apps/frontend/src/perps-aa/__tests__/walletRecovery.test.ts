import { afterEach, describe, expect, it, vi } from 'vitest'
import { createWalletPreparationRecovery, PREPARATION_RECOVERY_HEADER, recoveryChallengeMessage, parseWalletRecoveryResult, recoveryReason, recoveryMessage } from '../walletRecovery'
import { preparationIdentifier } from '../nativePreparation'
import type { Address, Hex } from 'viem'
const owner = `0x${'1'.repeat(40)}` as Address
const sender = `0x${'2'.repeat(40)}` as Address
const paymaster = `0x${'3'.repeat(40)}` as Address
const id = '12345678-1234-4123-8123-123456789abc'
const token = 'b'.repeat(64)
const missing = { version: 1, recoveryState: 'missing', reason: 'PREPARATION_NOT_CREATED', canRetire: true, operationHashes: [] }
function setup(challengeLifetime = 300, serverNow = Date.now(), responseDate: string | null = new Date(serverNow).toUTCString()) {
  const calls: { method: string; headers: Headers; params: Record<string, unknown>[] }[] = []
  let alter = false
  let statusResult: unknown = missing
  const sign = vi.fn(async () => `0x${'12'.repeat(65)}` as Hex)
  const fetcher = vi.fn(async (_url: URL | RequestInfo, init?: RequestInit) => {
    const body = JSON.parse(String(init?.body)) as { method: string; params: Record<string, unknown>[] }
    calls.push({ ...body, headers: new Headers(init?.headers) })
    let result: unknown = statusResult
    if (body.method === 'plether_getRecoveryChallenge') {
      const nonce = 'a'.repeat(64), expiresAt = Math.floor(serverNow / 1000) + challengeLifetime
      result = { version: 1, challengeId: nonce, expiresAt,
        message: recoveryChallengeMessage({ origin: location.origin, chainId: 421614, paymaster, sender,
          preparationId: preparationIdentifier(id), owner, nonce, expiresAt }) + (alter ? '\nextra instruction' : '') }
    }
    if (body.method === 'plether_verifyRecoveryChallenge') result = { version: 1, sessionToken: token, expiresIn: 900 }
    if (body.method === 'plether_retirePreparation') result = { ...missing, recoveryState: 'retired', reason: 'PREPARATION_RETIRED', canRetire: false }
    return new Response(JSON.stringify({ result }), { headers: { 'Content-Type': 'application/json',
      ...(responseDate === null ? {} : { Date: responseDate }) } })
  })
  const api = createWalletPreparationRecovery({ rpcUrl: '/api/perps/v1/aa/rpc', chainId: 421614, paymaster, sender, owner, signMessage: sign, fetcher })
  return { api, sign, fetcher, calls, setStatus: (value: unknown) => { statusResult = value }, tamper: () => { alter = true } }
}
afterEach(() => { vi.useRealTimers(); vi.restoreAllMocks() })
describe('wallet preparation recovery', () => {
  it('preserves the backend retirement blocker without changing permission', () => {
    const status = { version: 1, canRetire: false, recoveryVerified: true, phase: 'prepared', reason: 'PREPARATION_UNUSABLE',
      serverTime: '1', recoverable: false, freshReviewAllowed: true, retirementReason: 'RECOVERY_LIABILITY_PENDING' }
    expect(parseWalletRecoveryResult(status)).toMatchObject({ canRetire: false, retirementReason: 'RECOVERY_LIABILITY_PENDING' })
    expect(() => parseWalletRecoveryResult({ ...status, retirementReason: 'untrusted raw error' })).toThrow('could not be verified')
    expect(recoveryMessage('INVALID_ORDER_DEADLINE')).toContain('review the order again')
  })
  it.each([
    [{ cause: { code: 4001 } }, 'WALLET_SIGNATURE_DECLINED', 'signature was declined'],
    [{ code: -32002 }, 'WALLET_REQUEST_PENDING', 'request is already open'],
    [new DOMException('private transport details', 'TimeoutError'), 'RECOVERY_TIMEOUT', 'timed out'],
  ])('explains wallet and transport failures without exposing raw errors', (cause, reason, message) => {
    expect(recoveryReason(cause)).toBe(reason)
    expect(recoveryMessage(recoveryReason(cause))).toContain(message)
    expect(recoveryMessage(recoveryReason(cause))).not.toContain('private transport details')
  })
  it('does not report an arbitrary error reason as a support code', () => {
    expect(recoveryReason({ reason: 'secret wallet contents', cause: { code: 4001 } })).toBe('WALLET_SIGNATURE_DECLINED')
  })
  it.each([
    'RECOVERY_VERIFICATION_REQUIRED',
    '{"data":{"reason":"secret wallet contents"}}',
    'x'.repeat(8_193),
  ])('ignores unstructured, sensitive, or oversized HTTP details', details => {
    expect(recoveryReason({ name: 'HttpRequestError', details })).toBeUndefined()
  })
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
  it('binds a verified existing operation for an explicit retry without sending it', async () => {
    const { api, sign, calls, setStatus } = setup()
    const hash = `0x${'c'.repeat(64)}` as Hex
    await api.verify(id)
    setStatus({ version: 1, canRetire: false, recoveryVerified: true, phase: 'submitted', reason: 'PREPARATION_UNUSABLE',
      serverTime: '1', recoverable: false, freshReviewAllowed: false, userOperationHash: hash })
    await api.status(id)
    expect(api.operationHeaders(hash)[PREPARATION_RECOVERY_HEADER]).toBe(token)
    expect(sign).toHaveBeenCalledTimes(1)
    expect(calls.every(call => !/sendUserOperation|prepareUserOperation/.test(call.method))).toBe(true)
  })
  it('rejects altered wallet messages before opening the wallet', async () => {
    const { api, sign, tamper } = setup()
    tamper()
    await expect(api.verify(id)).rejects.toMatchObject({ reason: 'INVALID_RECOVERY_RESPONSE' })
    expect(sign).not.toHaveBeenCalled()
  })
  it.each([
    [0, 'RECOVERY_CHALLENGE_EXPIRED'],
    [360, 'INVALID_RECOVERY_RESPONSE'],
  ])('explains invalid challenge timing (%i seconds) without signing', async (lifetime, reason) => {
    const { api, sign, calls } = setup(lifetime)
    await expect(api.verify(id)).rejects.toMatchObject({ reason })
    expect(sign).not.toHaveBeenCalled()
    expect(calls.map(call => call.method)).toEqual(['plether_getRecoveryChallenge'])
    expect(api.headers(id)).toEqual({})
  })
  it.each([-86_400_000, -6_000, 6_000, 86_400_000])('verifies with a device clock offset of %i ms', async offset => {
    const serverNow = Date.now()
    vi.spyOn(Date, 'now').mockReturnValue(serverNow + offset)
    const { api, sign } = setup(300, serverNow)
    await api.verify(id)
    expect(sign).toHaveBeenCalledTimes(1)
    expect(api.headers(id)[PREPARATION_RECOVERY_HEADER]).toBe(token)
  })
  it.each([null, 'invalid'])('does not sign without a valid response timestamp (%s)', async date => {
    const { api, sign } = setup(300, Date.now(), date)
    await expect(api.verify(id)).rejects.toMatchObject({ reason: 'INVALID_RECOVERY_RESPONSE' })
    expect(sign).not.toHaveBeenCalled()
  })
  it('does not let a wall-clock correction expire or extend a session', async () => {
    let elapsed = 0
    vi.spyOn(performance, 'now').mockImplementation(() => elapsed)
    const { api } = setup()
    await api.verify(id)
    vi.spyOn(Date, 'now').mockReturnValue(Date.now() + 86_400_000)
    expect(api.headers(id)[PREPARATION_RECOVERY_HEADER]).toBe(token)
    elapsed = 895_000
    vi.mocked(Date.now).mockReturnValue(0)
    expect(api.headers(id)).toEqual({})
  })
  it('preserves backend expiry rejection after wallet approval', async () => {
    const { api, fetcher, sign } = setup()
    const success = fetcher.getMockImplementation()!
    fetcher.mockImplementation(async (url, init) => {
      if (JSON.parse(String(init?.body)).method === 'plether_verifyRecoveryChallenge') {
        return new Response(JSON.stringify({ error: { data: { reason: 'RECOVERY_CHALLENGE_EXPIRED' } } }), { status: 403 })
      }
      return success(url, init)
    })
    await expect(api.verify(id)).rejects.toMatchObject({ reason: 'RECOVERY_CHALLENGE_EXPIRED' })
    expect(sign).toHaveBeenCalledTimes(1)
    expect(api.headers(id)).toEqual({})
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
