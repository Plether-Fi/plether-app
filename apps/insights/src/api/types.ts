export type UsdcUnits = string

export type CompetitionStatus = 'scheduled' | 'live' | 'ended' | 'review' | 'final'

export interface CompetitionPrize {
  place: number
  amount: UsdcUnits
}

export interface Competition {
  id: string
  slug: string
  name: string
  status: CompetitionStatus
  startsAt: string
  tradingCutoffAt: string
  resultsAt: string
  startingBalance: UsdcUnits
  pnlEligibilityThreshold: UsdcUnits
  minActiveDays: number
  prizes: CompetitionPrize[]
  latestIndexedBlock: number | null
  latestIndexedAt: string | null
  participantCount?: number
  eligibleCount?: number
}

export interface Standing {
  rank: number | null
  address: string
  displayName: string | null
  pnl: UsdcUnits | null
  realizedPnl: UsdcUnits
  roiBps: number | null
  volume: UsdcUnits
  trades: number
  activeDays: number
  liquidations: number
  prizePlace: number | null
  prizePlaces: number[]
  prizeAmountUsdc: UsdcUnits | null
  eligible: boolean
  eligibilityStatus: string
  eligibilityReasons: string[]
}

export interface WalletPosition {
  market: string
  side: 'long' | 'short' | null
  size: UsdcUnits | null
  sizeDelta: string | null
  margin: UsdcUnits | null
  entryPrice: string | null
  markPrice: string | null
  unrealizedPnl: UsdcUnits | null
  liquidatable: boolean | null
}

export interface WalletDetails extends Standing {
  equity: UsdcUnits | null
  position: WalletPosition | null
}

export interface WalletActivity {
  id: string
  type: string
  occurredAt: string
  market: string | null
  side: 'long' | 'short' | null
  size: UsdcUnits | null
  sizeDelta: string | null
  price: string | null
  pnl: UsdcUnits | null
  txHash: string | null
}

export interface LeaderboardResponse {
  competition: Competition
  standings: Standing[]
  nextCursor: string | null
  provisional: boolean
}

export interface WalletResponse {
  competition: Competition
  wallet: WalletDetails
  activity: WalletActivity[] | null
}

export interface InsightsStatus {
  healthy: boolean
  latestIndexedBlock: number | null
  latestIndexedAt: string | null
  chainId?: number
}

export interface ApiErrorBody {
  error?: {
    code?: string
    message?: string
  }
  message?: string
}
