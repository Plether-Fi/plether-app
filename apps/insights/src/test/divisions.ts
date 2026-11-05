import { FIRST_WEEK_START, tradingWeek } from '../divisions/rules'
import type { WeeklyAccount, WeeklyInput } from '../divisions/types'

export function account(index = 1, overrides: Partial<WeeklyAccount> = {}): WeeklyAccount {
  return {
    address: `0x${index.toString(16).padStart(40, '0')}`, displayName: `Trader ${String(index)}`, hasTraded: true,
    startingEquityAssets: '100000000000', closingEquityAssets: '110000000000', depositsAssets: '0', withdrawalsAssets: '0',
    executedTradeTimes: ['2026-10-12T10:00:00Z', '2026-10-13T10:00:00Z'], ...overrides,
  }
}

export function input(weekIndex = 0, accounts = [account()]): WeeklyInput {
  const startsAt = new Date(Date.parse(FIRST_WEEK_START) + weekIndex * 7 * 86_400_000).toISOString()
  const week = tradingWeek(Date.parse(startsAt))
  return {
    startsAt, publishedAt: week.publishesAt,
    baselineBlock: { number: String(100 + weekIndex * 100), hash: `0x${'a'.repeat(64)}` },
    closingBlock: { number: String(199 + weekIndex * 100), hash: `0x${'b'.repeat(64)}` },
    accounts: accounts.map((row) => ({ ...row, executedTradeTimes: row.executedTradeTimes.map((timestamp) => new Date(Date.parse(timestamp) + weekIndex * 7 * 86_400_000).toISOString()) })),
  }
}

