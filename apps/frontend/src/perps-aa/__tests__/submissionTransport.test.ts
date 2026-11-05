import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { submissionHttp, SUBMISSION_TIMEOUT_MS } from '../submissionTransport'

const url = 'https://app.sepolia.plether.com/api/perps/v1/aa/rpc'
const hash = `0x${'a'.repeat(64)}`
const request = { method: 'eth_sendUserOperation', params: [{ signature: '0x1234' }, '0xentrypoint'] }

describe('native submission transport', () => {
  beforeEach(() => { vi.useFakeTimers() })
  afterEach(() => { vi.useRealTimers(); vi.restoreAllMocks() })

  it.each([{}, { 'X-Plether-AA-Preparation-Recovery': 'test-session' }])(
    'accepts a slow acknowledgement with headers %j without retrying', async headers => {
      const fetcher = vi.spyOn(globalThis, 'fetch').mockImplementation(async () => {
        await new Promise(resolve => setTimeout(resolve, 15_000))
        return Response.json({ jsonrpc: '2.0', id: 1, result: hash })
      })
      const transport = submissionHttp(url, headers)({ retryCount: 3 })
      expect(transport.config).toMatchObject({ timeout: SUBMISSION_TIMEOUT_MS, retryCount: 0 })
      const result = transport.request(request)
      await vi.advanceTimersByTimeAsync(15_000)
      await expect(result).resolves.toBe(hash)
      expect(fetcher).toHaveBeenCalledOnce()
      expect(JSON.parse(String(fetcher.mock.calls[0][1]?.body))).toMatchObject(request)
      expect(new Headers(fetcher.mock.calls[0][1]?.headers).get('X-Plether-AA-Preparation-Recovery'))
        .toBe(headers['X-Plether-AA-Preparation-Recovery'] ?? null)
    },
  )

  it('aborts at 30 seconds and never automatically resends after a lost response', async () => {
    let signal: AbortSignal | null | undefined
    const fetcher = vi.spyOn(globalThis, 'fetch').mockImplementation((_url, init) => {
      signal = init?.signal
      return new Promise((_resolve, reject) => {
        signal?.addEventListener('abort', () => reject(new DOMException('Aborted', 'AbortError')), { once: true })
      })
    })
    const result = submissionHttp(url)({ retryCount: 3 }).request(request).catch((error: unknown) => error)
    await vi.advanceTimersByTimeAsync(SUBMISSION_TIMEOUT_MS - 1)
    expect(signal?.aborted).toBe(false)
    await vi.advanceTimersByTimeAsync(1)
    expect(signal?.aborted).toBe(true)
    expect(await result).toMatchObject({ name: 'TimeoutError' })
    await vi.advanceTimersByTimeAsync(60_000)
    expect(fetcher).toHaveBeenCalledOnce()
  })

  it.each(['network', '502'])('does not retry an ambiguous %s failure', async failure => {
    const fetcher = vi.spyOn(globalThis, 'fetch').mockImplementation(async () => {
      if (failure === 'network') throw new TypeError('Failed to fetch')
      return Response.json({ error: { message: 'Bad gateway' } }, { status: 502 })
    })
    const result = submissionHttp(url)({ retryCount: 3 }).request(request).catch((error: unknown) => error)
    await vi.advanceTimersByTimeAsync(60_000)
    expect(await result).toBeInstanceOf(Error)
    expect(fetcher).toHaveBeenCalledOnce()
  })
})
