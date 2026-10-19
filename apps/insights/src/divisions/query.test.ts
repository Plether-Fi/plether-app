import { afterEach, describe, expect, it, vi } from 'vitest'
import { getDivisions, getTradedWallets } from './query'
import { publishWeek } from './engine'
import { input } from '../test/divisions'

afterEach(() => { vi.unstubAllGlobals(); vi.useRealTimers() })

describe('Divisions publications', () => {
  it('returns the pre-publication state on a missing file', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response('', { status: 404 })))
    expect(await getDivisions()).toBeNull()
  })
  it('treats a Pages HTML fallback as a missing publication', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response('<html>SPA</html>', { headers: { 'Content-Type': 'text/html; charset=utf-8' } })))
    await expect(getDivisions()).resolves.toBeNull()
  })
  it('rejects invalid publication data without exposing a JSON parser error', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(Response.json({ competition: {}, standings: [] })))
    await expect(getDivisions()).rejects.toThrow(/invalid/)
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response('{bad json', { headers: { 'Content-Type': 'application/json' } })))
    await expect(getDivisions()).rejects.toThrow('Divisions publication is invalid')
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

describe('all traded wallets', () => {
  it('reads wallets independently of registration or weekly publication', async () => {
    const data = { indexedThroughBlock: '320518230', traders: [
      { address: '0x0000000000000000000000000000000000000001', executedTrades: 2, lastTradeAt: '2026-10-18T20:00:00Z' },
    ] }
    const fetcher = vi.fn().mockResolvedValue(Response.json({ data }))
    vi.stubGlobal('fetch', fetcher)
    expect(await getTradedWallets()).toEqual(data)
    expect(fetcher).toHaveBeenCalledWith('/api/insights/v1/divisions/traders', expect.objectContaining({ headers: { Accept: 'application/json' } }))
  })
  it('rejects malformed wallet rows instead of hiding missing traders', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(Response.json({ data: { indexedThroughBlock: '1', traders: [{ address: 'bad', executedTrades: 1, lastTradeAt: 'yesterday' }] } })))
    await expect(getTradedWallets()).rejects.toThrow('Traded wallet feed is invalid')
  })
})
