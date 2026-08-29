export type UsdcUnits = string

export type CompetitionStatus = 'scheduled' | 'live' | 'ended' | 'review' | 'final'

export interface CompetitionPrize {
  place: number
  amount: UsdcUnits
}

export type CompetitionRegistrationStatus = 'upcoming' | 'open' | 'closed'

export interface CompetitionRegistration {
  status: CompetitionRegistrationStatus
  opensAt: string
  closesAt: string
  minimumXAccountAgeDays: number
  targetXHandle: string
  rulesVersion: string
  privacyVersion: string
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
  registration?: CompetitionRegistration
  fxSessionBoundaryUtc?: string
}

export type RegistrationStepStatus = 'pending' | 'verified'
export type RegistrationApplicationStatus = 'in_progress' | 'completed'

export interface RegistrationSession {
  status: RegistrationApplicationStatus
  csrfToken: string
  expiresAt: string
  oauthErrorCode?: string | null
  steps: {
    xIdentity: RegistrationStepStatus
    xFollow: RegistrationStepStatus
    wallet: RegistrationStepStatus
    completed: boolean
  }
  requiredConsents: {
    rulesVersion: string
    privacyVersion: string
  }
  identity?: {
    xHandle: string
    maskedEmail: string
  }
  wallet?: {
    ownerAddress: string
    tradingAccount: string
  }
}

export interface WalletChallenge {
  message: string
  expiresAt: string
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
  executionFee: UsdcUnits | null
  vpi: UsdcUnits | null
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
  activityStatus: 'live' | 'omitted_after_finalization'
}

export interface InsightsStatus {
  healthy: boolean
  latestIndexedBlock: number | null
  latestIndexedAt: string | null
  chainId?: number
  participantCount?: number
  eligibleCount?: number
}

export interface ApiErrorBody {
  error?: {
    code?: string
    message?: string
  }
  message?: string
}
