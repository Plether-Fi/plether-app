import type { DivisionPublication, WeeklyInput } from './types.ts'
import { TIERS, tradingWeek } from './rules.ts'

function record(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null
}

function boundary(value: unknown): boolean {
  return record(value) && typeof value.number === 'string' && /^\d+$/.test(value.number)
    && typeof value.hash === 'string' && /^0x[0-9a-fA-F]{64}$/.test(value.hash)
}

export function isWeeklyInput(value: unknown): value is WeeklyInput {
  return record(value) && typeof value.startsAt === 'string' && typeof value.publishedAt === 'string'
    && boundary(value.baselineBlock) && boundary(value.closingBlock) && Array.isArray(value.accounts)
    && value.accounts.every((row) => record(row) && typeof row.address === 'string'
      && (row.displayName === null || typeof row.displayName === 'string') && typeof row.hasTraded === 'boolean'
      && ['startingEquityUsdc', 'closingEquityUsdc', 'depositsUsdc', 'withdrawalsUsdc'].every((key) => typeof row[key] === 'string' && /^\d+$/.test(row[key]))
      && Array.isArray(row.executedTradeTimes) && row.executedTradeTimes.every((time) => typeof time === 'string'))
}

// Published files are a separate data source; never reinterpret September's
// cumulative P&L as a weekly score or infer a persistent tier from one week.
export function isPublication(value: unknown): value is DivisionPublication {
  if (!record(value) || value.version !== 1 || !record(value.source) || !boundary(value.source.baselineBlock) || !boundary(value.source.closingBlock)
    || !record(value.week) || !Array.isArray(value.standings) || !record(value.announcement)) return false
  const { startsAt, closesAt, publishesAt } = value.week
  if (typeof startsAt !== 'string' || typeof closesAt !== 'string' || typeof publishesAt !== 'string' || typeof value.publishedAt !== 'string') return false
  if (!Number.isFinite(Date.parse(startsAt))) return false
  const week = tradingWeek(Date.parse(startsAt))
  if (Date.parse(startsAt) !== Date.parse(week.startsAt) || Date.parse(closesAt) !== Date.parse(week.closesAt)
    || Date.parse(publishesAt) !== Date.parse(week.publishesAt) || !(Date.parse(value.publishedAt) >= Date.parse(publishesAt))) return false
  const addresses = new Set<string>()
  for (const row of value.standings) {
    if (!record(row) || typeof row.address !== 'string' || !/^0x[0-9a-f]{40}$/.test(row.address) || addresses.has(row.address)) return false
    addresses.add(row.address)
    if (!(row.displayName === null || typeof row.displayName === 'string') || !TIERS.some((tier) => tier === row.tier)
      || !TIERS.some((tier) => tier === row.previousTier) || !['promoted', 'demoted', 'unchanged'].includes(String(row.movement))
      || typeof row.qualifying !== 'boolean' || !Number.isInteger(row.activeDays) || Number(row.activeDays) < 0 || Number(row.activeDays) > 5
      || !Number.isInteger(row.nonQualifyingWeeks) || Number(row.nonQualifyingWeeks) < 0 || Number(row.nonQualifyingWeeks) > 1
      || !(row.rank === null || (Number.isInteger(row.rank) && Number(row.rank) > 0))
      || typeof row.weeklyPnlUsdc !== 'string' || !/^-?\d+$/.test(row.weeklyPnlUsdc)
      || typeof row.adjustedEquityUsdc !== 'string' || !/^-?\d+$/.test(row.adjustedEquityUsdc)) return false
  }
  const { tradersOfTheWeek, promotions, diamondRoster } = value.announcement
  return Array.isArray(tradersOfTheWeek) && tradersOfTheWeek.length <= 3 && tradersOfTheWeek.every((address) => typeof address === 'string' && addresses.has(address))
    && Array.isArray(diamondRoster) && diamondRoster.every((address) => typeof address === 'string' && addresses.has(address))
    && Array.isArray(promotions) && promotions.every((row) => record(row) && typeof row.address === 'string' && addresses.has(row.address) && ['Gold', 'Platinum', 'Diamond'].includes(String(row.tier)))
}
