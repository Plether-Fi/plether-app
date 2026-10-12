import { afterEach, describe, expect, it, vi } from 'vitest'
import { getDivisions } from './query'
import { publishWeek } from './engine'
import { input } from '../test/divisions'

afterEach(() => { vi.unstubAllGlobals(); vi.useRealTimers() })

describe('Divisions publications', () => {
  it('returns the pre-publication state on a missing file', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response('', { status: 404 })))
    expect(await getDivisions()).toBeNull()
  })
  it('never accepts the old competition response or a SPA HTML fallback', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(Response.json({ competition: {}, standings: [] })))
    await expect(getDivisions()).rejects.toThrow(/invalid/)
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response('<html>SPA</html>')))
    await expect(getDivisions()).rejects.toThrow()
  })
  it('rejects a publication timestamp in the future', async () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-10-12T00:00:00Z'))
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(Response.json(publishWeek(input(), null))))
    await expect(getDivisions()).rejects.toThrow(/invalid/)
  })
  it('reads a valid publication without caching and propagates cancellation', async () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-10-19T12:00:00Z'))
    const publication = publishWeek(input(), null)
    const fetcher = vi.fn().mockResolvedValue(Response.json(publication))
    vi.stubGlobal('fetch', fetcher)
    const signal = new AbortController().signal
    expect(await getDivisions(signal)).toEqual(publication)
    expect(fetcher).toHaveBeenCalledWith('/divisions/current.json', expect.objectContaining({ cache: 'no-store', signal }))
  })
})
