export type AssetsUnits = string

export type CompetitionStatus = 'scheduled' | 'live' | 'ended' | 'review' | 'final'

export interface CompetitionPrize {
  place: number
  amount: AssetsUnits
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
  startingBalance: AssetsUnits
  pnlEligibilityThreshold: AssetsUnits
  minActiveDays: number
  prizes: CompetitionPrize[]
  latestIndexedBlock: number | null
  latestIndexedAt: string | null
  participantCount?: number
  eligibleCount?: number
  integrityStatus?: 'pending' | 'stale' | 'current'
  integrityCheckedAt?: string
  integrityAsOfBlock?: string
  releaseReady?: boolean
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
  snapshotAt?: string | null
  rank: number | null
  address: string
  displayName: string | null
  pnl: AssetsUnits | null
  realizedPnl: AssetsUnits
  roiBps: number | null
  volume: AssetsUnits
  trades: number
  activeDays: number
  liquidations: number
  prizePlace: number | null
  prizePlaces: number[]
  prizeAmountAssets: AssetsUnits | null
  eligible: boolean
  eligibilityStatus: string
  eligibilityReasons: string[]
}

export interface WalletPosition {
  market: string
  side: 'long' | 'short' | null
  size: AssetsUnits | null
  sizeDelta: string | null
  margin: AssetsUnits | null
  entryPrice: string | null
  markPrice: string | null
  unrealizedPnl: AssetsUnits | null
  liquidatable: boolean | null
}

export interface WalletDetails extends Standing {
  equity: AssetsUnits | null
  position: WalletPosition | null
}

export interface TradeSettlement {
  protocolFeeCollectedAssets?: string
  kind: 'ActionChargeSettled' | 'ActionRebateSettled' | 'FrozenCloseSpreadSettled'
  assessedAssets: string
  recoveredAssets: string
  waivedAssets: string
}

export interface TradeExecution {
  orderId: string
  protocolVersion: string
  status: 'pending' | 'complete'
  receipt: Record<string, string>
  settlements: TradeSettlement[]
}

export interface WalletActivity {
  id: string
  type: string
  occurredAt: string
  market: string | null
  side: 'long' | 'short' | null
  size: AssetsUnits | null
  sizeDelta: string | null
  price: string | null
  pnl: AssetsUnits | null
  execution?: TradeExecution | null
  executionFee: AssetsUnits | null
  vpi: AssetsUnits | null
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
