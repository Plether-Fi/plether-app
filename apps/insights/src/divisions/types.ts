import type { Tier } from './rules'

export interface DivisionState {
  address: string
  displayName: string | null
  tier: Tier
  nonQualifyingWeeks: number
}

export interface WeeklyAccount {
  address: string
  displayName: string | null
  hasTraded: boolean
  // Complete economic account value, including marked positions, fees and claims.
  startingEquityUsdc: string
  closingEquityUsdc: string
  depositsUsdc: string
  withdrawalsUsdc: string
  // Only successfully executed, nonzero voluntary position changes.
  executedTradeTimes: string[]
}

export interface DivisionStanding extends DivisionState {
  rank: number | null
  weeklyPnlUsdc: string
  adjustedEquityUsdc: string
  // Full-precision ratio is kept as integer numerator / denominator.
  activeDays: number
  qualifying: boolean
  previousTier: Tier
  movement: 'promoted' | 'demoted' | 'unchanged'
}

export interface DivisionPublication {
  version: 1
  source: { baselineBlock: WeeklyInput['baselineBlock']; closingBlock: WeeklyInput['closingBlock'] }
  week: { startsAt: string; closesAt: string; publishesAt: string }
  publishedAt: string
  standings: DivisionStanding[]
  announcement: {
    tradersOfTheWeek: string[]
    promotions: { address: string; tier: Tier }[]
    diamondRoster: string[]
  }
}

export interface WeeklyInput {
  startsAt: string
  publishedAt: string
  // Canonical finalized boundary block identifiers retained for audit.
  baselineBlock: { number: string; hash: string }
  closingBlock: { number: string; hash: string }
  accounts: WeeklyAccount[]
}
