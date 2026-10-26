import { describe, expect, it } from 'vitest'
import { formatReturn, publishWeek } from './engine'
import { activeSessionDay, FIRST_WEEK_START, tradingWeek } from './rules'
import { isPublication, isWeeklyInput } from './schema'
import type { DivisionPublication } from './types'
import { account, input } from '../test/divisions'

describe('Divisions ranking and progression', () => {
  it('ranks percent return instead of absolute profit or a fixed 100k bankroll', () => {
    const publication = publishWeek(input(0, [account(1, { startingEquityAssets: '1000000000000', closingEquityAssets: '1050000000000' }), account(2)]), null)
    expect(publication.standings.map((row) => row.address)).toEqual([account(2).address, account(1).address])
    expect(publication.standings[0].tier).toBe('Bronze')
    expect(publication.announcement.tradersOfTheWeek).toEqual([account(2).address, account(1).address])
  })

  it('adjusts both P&L and the denominator for cash flows', () => {
    const publication = publishWeek(input(0, [account(1, { depositsAssets: '100000000000', withdrawalsAssets: '50000000000', closingEquityAssets: '165000000000' })]), null)
    expect(publication.standings[0]).toMatchObject({ weeklyPnlAssets: '15000000000', adjustedEquityAssets: '150000000000', qualifying: true })
    expect(formatReturn('15000000000', '150000000000')).toBe('+10.00%')
    const flat = publishWeek(input(0, [account(1, { depositsAssets: '100000000000', closingEquityAssets: '200000000000' })]), null)
    expect(flat.standings[0]).toMatchObject({ weeklyPnlAssets: '0', qualifying: false, tier: 'Iron' })
  })

  it('requires strictly positive net profit and two distinct session days', () => {
    for (const overrides of [{ closingEquityAssets: '100000000000' }, { closingEquityAssets: '99999999999' }, { executedTradeTimes: ['2026-10-12T10:00:00Z', '2026-10-12T20:00:00Z'] }]) {
      expect(publishWeek(input(0, [account(1, overrides)]), null).standings[0].qualifying).toBe(false)
    }
    expect(publishWeek(input(0, [account(1, { closingEquityAssets: '100000000001' })]), null).standings[0].qualifying).toBe(true)
  })

  it('promotes at most one tier per week and reaches Diamond only on week five', () => {
    let previous: DivisionPublication | null = null
    for (const [index, tier] of ['Bronze', 'Silver', 'Gold', 'Platinum', 'Diamond'].entries()) {
      previous = publishWeek(input(index), previous)
      expect(previous.standings[0].tier).toBe(tier)
      expect(previous.announcement.promotions).toHaveLength(index >= 2 ? 1 : 0)
    }
    expect(previous?.announcement.diamondRoster).toEqual([account().address])
  })

  it('demotes after two inactive weeks and resets the streak', () => {
    const first = publishWeek(input(), null)
    const idle = account(1, { executedTradeTimes: [], closingEquityAssets: '100000000000' })
    const second = publishWeek(input(1, [idle]), first)
    expect(second.standings[0]).toMatchObject({ tier: 'Bronze', nonQualifyingWeeks: 1 })
    const third = publishWeek(input(2, [idle]), second)
    expect(third.standings[0]).toMatchObject({ tier: 'Iron', movement: 'demoted', nonQualifyingWeeks: 0 })
    expect(publishWeek(input(3, [idle]), third).standings[0].tier).toBe('Iron')
  })

  it('resets a non-qualifying streak even if qualification does not earn promotion', () => {
    const first = publishWeek(input(), null)
    const idle = publishWeek(input(1, [account(1, { executedTradeTimes: [] })]), first)
    const many = Array.from({ length: 10 }, (_, i) => account(i + 1, { closingEquityAssets: String(110000000000 + i * 1000000000) }))
    const qualified = publishWeek(input(2, many), idle)
    expect(qualified.standings.find((row) => row.address === account().address)).toMatchObject({ tier: 'Bronze', qualifying: true, nonQualifyingWeeks: 0 })
  })

  it('uses all profitable accounts for percentiles, includes cutoff ties and rounds up', () => {
    const accounts = Array.from({ length: 20 }, (_, i) => account(i + 1, { closingEquityAssets: String(120000000000 - i * 1000000000) }))
    let first = publishWeek(input(0, accounts), null)
    let second = publishWeek(input(1, accounts), first)
    const third = publishWeek(input(2, accounts), second)
    expect(third.standings[3].tier).toBe('Gold')
    expect(third.standings[4].tier).toBe('Silver')
    const ties = accounts.map((row, index) => index < 5 ? { ...row, closingEquityAssets: '120000000000' } : row)
    first = publishWeek(input(0, ties), null)
    second = publishWeek(input(1, ties), first)
    expect(publishWeek(input(2, ties), second).standings.slice(0, 5).every((row) => row.tier === 'Gold')).toBe(true)
    expect(first.standings.slice(0, 5).map((row) => row.rank)).toEqual([1, 1, 1, 1, 1])
  })

  it('does not rank nonpositive capital and never assigns a tier to an untraded wallet', () => {
    const result = publishWeek(input(0, [account(1, { withdrawalsAssets: '100000000000' }), account(2, { hasTraded: false, executedTradeTimes: [] })]), null)
    expect(result.standings).toHaveLength(1)
    expect(result.standings[0]).toMatchObject({ rank: null, qualifying: false, tier: 'Iron' })
    expect(formatReturn('1', '0')).toBe('—')
  })

  it('rejects gaps, duplicate wallets, missing data and premature publication', () => {
    const first = publishWeek(input(), null)
    expect(() => publishWeek(input(2), first)).toThrow(/every week/)
    expect(() => publishWeek(input(1, []), first)).toThrow(/Missing weekly snapshot/)
    expect(() => publishWeek(input(0, [account(), account()]), null)).toThrow(/exactly one/)
    expect(() => publishWeek({ ...input(), publishedAt: '2026-10-17T00:00:00Z' }, null)).toThrow(/Monday/)
    expect(() => publishWeek({ ...input(), startsAt: '2026-10-12T00:00:00Z' }, null)).toThrow(/Sunday/)
  })

  it('preserves exact rankings beyond safe JavaScript integer precision', () => {
    const capital = 10n ** 30n
    const result = publishWeek(input(0, [account(1, { startingEquityAssets: capital.toString(), closingEquityAssets: (capital + 2n).toString() }), account(2, { startingEquityAssets: capital.toString(), closingEquityAssets: (capital + 3n).toString() })]), null)
    expect(result.standings[0].address).toBe(account(2).address)
    expect(result.standings.map((row) => row.rank)).toEqual([1, 2])
  })

  it('validates serialized inputs and publications before using them', () => {
    expect(isWeeklyInput(input())).toBe(true)
    expect(isWeeklyInput({ ...input(), accounts: [{ address: account().address }] })).toBe(false)
    const publication = publishWeek(input(), null)
    expect(isPublication(publication)).toBe(true)
    expect(isPublication({ ...publication, standings: [{ ...publication.standings[0], tier: 'Emperor' }] })).toBe(false)
  })
})

describe('UTC trading sessions', () => {
  it('counts Sunday night as Monday, a 21:00 crossing as a new day, and excludes Friday close', () => {
    const week = tradingWeek(Date.parse(FIRST_WEEK_START))
    expect(activeSessionDay('2026-10-11T21:00:00Z', week.startsAt, week.closesAt)).toBe('2026-10-12')
    expect(activeSessionDay('2026-10-12T21:00:00Z', week.startsAt, week.closesAt)).toBe('2026-10-13')
    expect(activeSessionDay('2026-10-16T20:59:59Z', week.startsAt, week.closesAt)).toBe('2026-10-16')
    expect(activeSessionDay('2026-10-16T21:00:00Z', week.startsAt, week.closesAt)).toBeNull()
    expect(week.publishesAt).toBe('2026-10-19T00:00:00.000Z')
    expect(tradingWeek(Date.parse('2026-10-18T21:00:00Z')).closesAt).toBe('2026-10-23T21:00:00.000Z')
  })
})
