import { afterEach, describe, expect, it, vi } from 'vitest'
import { getDivisions, getDivisionsHistory, getLiveDivisions, getTradedWallets } from './query'
import { publishWeek } from './engine'
import { input } from '../test/divisions'

afterEach(() => { vi.unstubAllGlobals(); vi.useRealTimers() })

describe('Divisions publications', () => {
  it('returns the pre-publication state only for an explicit empty result', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(Response.json({ data: null })))
    expect(await getDivisions()).toBeNull()
  })
  it('rejects missing endpoints and HTML fallbacks rather than hiding publication failures', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response('', { status: 404 })))
    await expect(getDivisions()).rejects.toThrow(/unavailable/)
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response('<html>SPA</html>')))
    await expect(getDivisions()).rejects.toThrow(/invalid/)
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
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(Response.json({ data: publishWeek(input(), null) })))
    await expect(getDivisions()).rejects.toThrow(/invalid/)
  })
  it('reads a valid publication without caching and propagates cancellation', async () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-10-19T12:00:00Z'))
    const publication = publishWeek(input(), null)
    const fetcher = vi.fn().mockResolvedValue(Response.json({ data: publication }))
    vi.stubGlobal('fetch', fetcher)
    const signal = new AbortController().signal
    expect(await getDivisions(signal)).toEqual(publication)
    expect(fetcher).toHaveBeenCalledWith('/api/insights/v1/divisions/current', expect.objectContaining({ cache: 'no-store', signal }))
  })
})

describe('all traded wallets', () => {
  it('reads wallets independently of registration or weekly publication', async () => {
    const data = { releaseRouter: '0x9b9ddc783780a14e4930dbbcd94d6356a82725e8', indexedThroughBlock: '320518230', traders: [
      { address: '0x0000000000000000000000000000000000000001', executedTrades: 2, lastTradeAt: '2026-10-18T20:00:00Z' },
    ] }
    const fetcher = vi.fn().mockResolvedValue(Response.json({ data }))
    vi.stubGlobal('fetch', fetcher)
    expect(await getTradedWallets()).toEqual(data)
    expect(fetcher).toHaveBeenCalledWith('/api/insights/v1/divisions/traders', expect.objectContaining({ headers: { Accept: 'application/json' } }))
  })
  it('rejects malformed wallet rows instead of hiding missing traders', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(Response.json({ data: { releaseRouter: '0x9b9ddc783780a14e4930dbbcd94d6356a82725e8', indexedThroughBlock: '1', traders: [{ address: 'bad', executedTrades: 1, lastTradeAt: 'yesterday' }] } })))
    await expect(getTradedWallets()).rejects.toThrow('Traded wallet feed is invalid')
  })
})

describe('live Divisions rankings', () => {
  it('reads a complete provisional snapshot from the backend', async () => {
    const data = { status: 'ready', weekStartsAt: '2026-10-18T21:00:00Z', snapshotAt: '2026-10-19T12:00:00Z', snapshotBlock: '321', walletCount: 1, standings: [
      { address: '0x0000000000000000000000000000000000000001', rank: 1, weeklyReturnBps: 125, activeDays: 1, discordUsername: 'mellboi' },
    ] }
    const fetcher = vi.fn().mockResolvedValue(Response.json({ data }))
    vi.stubGlobal('fetch', fetcher)
    expect(await getLiveDivisions()).toEqual(data)
    expect(fetcher).toHaveBeenCalledWith('/api/insights/v1/divisions/live', expect.objectContaining({ headers: { Accept: 'application/json' } }))
  })
  it('rejects malformed public handles while accepting unlinked and older responses', async () => {
    for (const discordUsername of [null, undefined, 'trader', 42, { username: 'private' }, '']) {
      const data = { status: 'ready', weekStartsAt: '2026-10-18T21:00:00Z', snapshotAt: '2026-10-19T12:00:00Z', snapshotBlock: '321', walletCount: 1, standings: [
        { address: '0x0000000000000000000000000000000000000001', rank: 1, weeklyReturnBps: 125, activeDays: 1, discordUsername },
      ] }
      vi.stubGlobal('fetch', vi.fn().mockResolvedValue(Response.json({ data })))
      if (discordUsername === null || discordUsername === undefined || discordUsername === 'trader') {
        await expect(getLiveDivisions()).resolves.toMatchObject({ status: 'ready' })
      } else await expect(getLiveDivisions()).rejects.toThrow('Live Divisions response is invalid')
    }
  })
  it('rejects a partial snapshot whose declared wallet count differs', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(Response.json({ data: {
      status: 'ready', weekStartsAt: '2026-10-18T21:00:00Z', snapshotAt: '2026-10-19T12:00:00Z', snapshotBlock: '321', walletCount: 2, standings: [],
    } })))
    await expect(getLiveDivisions()).rejects.toThrow('Live Divisions response is invalid')
  })
})


describe('published week pagination', () => {
  const publication = publishWeek(input(), null)
  const cursor = String(Date.parse(publication.week.startsAt) / 1000)
  it('requests a specific week with cancellation and reads neighboring cursors', async () => {
    const data = { publication, olderCursor: null, newerCursor: String(Number(cursor) + 604800) }
    const fetcher = vi.fn().mockResolvedValue(Response.json({ data }))
    vi.stubGlobal('fetch', fetcher)
    const signal = new AbortController().signal
    expect(await getDivisionsHistory(cursor, signal)).toEqual(data)
    expect(fetcher).toHaveBeenCalledWith(`/api/insights/v1/divisions/history?week=${cursor}`, expect.objectContaining({ signal, cache: 'no-store' }))
  })
  it('requests latest results without a cursor and accepts an explicit empty history', async () => {
    const data = { publication: null, olderCursor: null, newerCursor: null }
    const fetcher = vi.fn().mockResolvedValue(Response.json({ data }))
    vi.stubGlobal('fetch', fetcher)
    expect(await getDivisionsHistory(null)).toEqual(data)
    expect(fetcher.mock.calls[0][0]).toBe('/api/insights/v1/divisions/history')
  })
  it('rejects missing weeks instead of silently returning the latest week', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response('', { status: 404 })))
    await expect(getDivisionsHistory('0')).rejects.toThrow('This week has not been published')
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(Response.json({ data: { publication, olderCursor: null, newerCursor: null } })))
    await expect(getDivisionsHistory(String(Number(cursor) + 604800))).rejects.toThrow('Weekly results are invalid')
  })
  it('rejects malformed or reversed cursors and empty cursor pages', async () => {
    for (const data of [
      { publication, olderCursor: cursor, newerCursor: null },
      { publication, olderCursor: null, newerCursor: cursor },
      { publication, olderCursor: 'invalid', newerCursor: null },
      { publication: null, olderCursor: cursor, newerCursor: null },
    ]) {
      vi.stubGlobal('fetch', vi.fn().mockResolvedValue(Response.json({ data })))
      await expect(getDivisionsHistory(null)).rejects.toThrow('Weekly results are invalid')
    }
  })
  it('rejects invalid URL cursors before making a request', async () => {
    const fetcher = vi.fn()
    vi.stubGlobal('fetch', fetcher)
    for (const invalid of ['-1', 'bad', '999999999999999999999']) {
      await expect(getDivisionsHistory(invalid)).rejects.toThrow('Invalid results week')
    }
    expect(fetcher).not.toHaveBeenCalled()
  })
})
