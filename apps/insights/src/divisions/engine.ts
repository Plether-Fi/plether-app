import { activeSessionDay, FIRST_WEEK_START, TIERS, tradingWeek } from './rules.ts'
import type { Tier } from './rules.ts'
import type { DivisionPublication, DivisionStanding, WeeklyInput } from './types.ts'
import { isPublication, isWeeklyInput } from './schema.ts'

function units(value: string): bigint {
  if (!/^\d+$/.test(value)) throw new Error('Snapshot and cash-flow amounts must be nonnegative integer USDC units')
  return BigInt(value)
}

function compareReturn(a: DivisionStanding, b: DivisionStanding): number {
  const left = BigInt(a.weeklyPnlUsdc) * BigInt(b.adjustedEquityUsdc)
  const right = BigInt(b.weeklyPnlUsdc) * BigInt(a.adjustedEquityUsdc)
  return left > right ? -1 : left < right ? 1 : 0
}

function targetTier(rank: number, profitableCount: number): Tier {
  for (const [tier, percentage] of [['Diamond', 5], ['Platinum', 10], ['Gold', 20], ['Silver', 50]] as const) {
    if (rank <= Math.ceil(profitableCount * percentage / 100)) return tier
  }
  return 'Bronze'
}

export function publishWeek(input: WeeklyInput, previous: DivisionPublication | null): DivisionPublication {
  if (!isWeeklyInput(input) || !Number.isFinite(Date.parse(input.startsAt))) throw new Error('Invalid weekly snapshot input')
  if (previous && !isPublication(previous)) throw new Error('Invalid previous publication')
  const week = tradingWeek(Date.parse(input.startsAt))
  if (Date.parse(input.startsAt) !== Date.parse(week.startsAt)) throw new Error('Week must start on Sunday at 21:00 UTC')
  const expectedStart = previous ? Date.parse(previous.week.startsAt) + 7 * 86_400_000 : Date.parse(FIRST_WEEK_START)
  if (Date.parse(input.startsAt) !== expectedStart) throw new Error('Publish every week in order, including inactive weeks')
  if (!Number.isFinite(Date.parse(input.publishedAt)) || Date.parse(input.publishedAt) < Date.parse(week.publishesAt)) {
    throw new Error('Final badges cannot be published before Monday')
  }
  for (const block of [input.baselineBlock, input.closingBlock]) {
    if (!/^\d+$/.test(block.number) || !/^0x[0-9a-fA-F]{64}$/.test(block.hash)) throw new Error('Canonical boundary blocks are required')
  }
  if (BigInt(input.closingBlock.number) <= BigInt(input.baselineBlock.number)) throw new Error('Closing block must follow baseline block')
  if (previous && BigInt(input.baselineBlock.number) <= BigInt(previous.source.closingBlock.number)) throw new Error('Weekly boundary blocks must advance')
  const prior = new Map(previous?.standings.map((row) => [row.address.toLowerCase(), row]) ?? [])
  const seen = new Set<string>()
  const standings: DivisionStanding[] = []
  for (const account of input.accounts) {
    const address = account.address.toLowerCase()
    if (!/^0x[0-9a-f]{40}$/.test(address) || seen.has(address)) throw new Error('Each wallet must have exactly one valid weekly snapshot')
    seen.add(address)
    const old = prior.get(address)
    if (!account.hasTraded && (old || account.executedTradeTimes.length)) throw new Error('Trading history cannot be removed')
    const adjusted = units(account.startingEquityUsdc) + units(account.depositsUsdc) - units(account.withdrawalsUsdc)
    const pnl = units(account.closingEquityUsdc) - adjusted
    const days = new Set<string>()
    for (const timestamp of account.executedTradeTimes) {
      const day = activeSessionDay(timestamp, week.startsAt, week.closesAt)
      if (!day) throw new Error('Trade execution is outside this trading week')
      days.add(day)
    }
    if (!account.hasTraded) continue
    standings.push({
      address, displayName: account.displayName,
      tier: old?.tier ?? 'Iron', previousTier: old?.tier ?? 'Iron',
      nonQualifyingWeeks: old?.nonQualifyingWeeks ?? 0,
      weeklyPnlUsdc: pnl.toString(), adjustedEquityUsdc: adjusted.toString(),
      activeDays: days.size, qualifying: adjusted > 0n && pnl > 0n && days.size >= 2,
      rank: null, movement: 'unchanged',
    })
  }
  for (const address of prior.keys()) {
    if (!seen.has(address)) throw new Error(`Missing weekly snapshot for ${address}; missing data is not inactivity`)
  }
  const ranked = standings.filter((row) => BigInt(row.adjustedEquityUsdc) > 0n)
    .sort((a, b) => compareReturn(a, b) || a.address.localeCompare(b.address))
  ranked.forEach((row, index) => {
    row.rank = index > 0 && compareReturn(row, ranked[index - 1]) === 0 ? ranked[index - 1].rank : index + 1
  })
  const profitableCount = ranked.filter((row) => BigInt(row.weeklyPnlUsdc) > 0n).length
  for (const row of standings) {
    const currentIndex = TIERS.indexOf(row.tier)
    if (row.qualifying && row.rank !== null) {
      row.nonQualifyingWeeks = 0
      const targetIndex = TIERS.indexOf(targetTier(row.rank, profitableCount))
      if (targetIndex > currentIndex) {
        row.tier = TIERS[currentIndex + 1]
        row.movement = 'promoted'
      }
    } else {
      row.nonQualifyingWeeks += 1
      if (row.nonQualifyingWeeks >= 2) {
        row.nonQualifyingWeeks = 0
        if (currentIndex > 0) {
          row.tier = TIERS[currentIndex - 1]
          row.movement = 'demoted'
        }
      }
    }
  }
  standings.sort((a, b) => (a.rank ?? Infinity) - (b.rank ?? Infinity) || a.address.localeCompare(b.address))
  return {
    version: 1, source: { baselineBlock: input.baselineBlock, closingBlock: input.closingBlock }, week, publishedAt: input.publishedAt, standings,
    announcement: {
      tradersOfTheWeek: ranked.filter((row) => BigInt(row.weeklyPnlUsdc) > 0n).slice(0, 3).map((row) => row.address),
      promotions: standings.filter((row) => row.movement === 'promoted' && TIERS.indexOf(row.tier) >= TIERS.indexOf('Gold'))
        .map((row) => ({ address: row.address, tier: row.tier })),
      diamondRoster: standings.filter((row) => row.tier === 'Diamond').map((row) => row.address),
    },
  }
}

export function formatReturn(pnl: string, capital: string): string {
  const denominator = BigInt(capital)
  if (denominator <= 0n) return '—'
  const numerator = BigInt(pnl)
  const absolute = numerator < 0n ? -numerator : numerator
  const hundredths = (absolute * 10_000n + denominator / 2n) / denominator
  return `${numerator > 0n ? '+' : numerator < 0n ? '-' : ''}${(hundredths / 100n).toString()}.${(hundredths % 100n).toString().padStart(2, '0')}%`
}
