import { afterEach, describe, expect, it, vi } from 'vitest'
import { reportAttemptStage } from '../attemptDiagnostics'
afterEach(() => vi.unstubAllGlobals())
describe('attempt diagnostics', () => {
  it('projects only allowlisted failure details and drops unexpected fields', () => {
    const fetch = vi.fn<typeof globalThis.fetch>(async () => new Response('{}'))
    vi.stubGlobal('fetch', fetch)
    const id = '22345678-1234-4123-8123-123456789abc'
    const details = { failureStep: 'wallet_approval' as const, reasonCode: 'WALLET_DECLINED' as const, signature: 'secret' }
    reportAttemptStage(id, 'execution_interrupted', details)
    expect(JSON.parse(fetch.mock.calls[0]![1]!.body as string)).toEqual({ attemptId: id, stage: 'execution_interrupted',
      failureStep: 'wallet_approval', reasonCode: 'WALLET_DECLINED' })
    reportAttemptStage(id, 'deadline_elapsed', { failureStep: 'submission', reasonCode: 'secret' } as never)
    expect(JSON.parse(fetch.mock.calls[1]![1]!.body as string)).toEqual({ attemptId: id, stage: 'deadline_elapsed' })
  })
  it('sends only a support reference and allowlisted stage, once per stage', () => {
    const fetch = vi.fn<typeof globalThis.fetch>(async () => new Response('{}'))
    vi.stubGlobal('fetch', fetch)
    const id = '12345678-1234-4123-8123-123456789abc'
    reportAttemptStage(id, 'wallet_approved')
    reportAttemptStage(id, 'wallet_approved')
    expect(fetch).toHaveBeenCalledTimes(1)
    expect(JSON.parse(fetch.mock.calls[0]![1]!.body as string)).toEqual({ attemptId: id, stage: 'wallet_approved' })
  })
  it('ignores invalid references and telemetry transport failures', async () => {
    const fetch = vi.fn().mockRejectedValue(new Error('offline'))
    vi.stubGlobal('fetch', fetch)
    reportAttemptStage('private raw error', 'submission_failed')
    expect(fetch).not.toHaveBeenCalled()
    expect(() => reportAttemptStage('12345678-1234-4123-8123-123456789abd', 'submission_failed')).not.toThrow()
    await Promise.resolve()
  })
})
