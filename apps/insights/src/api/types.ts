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

export interface AvailabilityReason {
  field: string
  reason: string
}

export interface ConfirmedBlock {
  number: string
  hash: string
  timestamp: number
}

export interface ProtocolEnvelope {
  releaseId: string
  chainId: string
  confirmedBlock: ConfirmedBlock
  indexerTimestamp: number | null
  calculationVersion: string
  evidence: Record<string, unknown>
  availability: AvailabilityReason[]
}

export interface ProtocolRelease {
  releaseId: string
  name: string
  chainId: number
  deploymentBlock: string
  calculationVersion: string
  contracts: Record<string, string>
}

export interface CurrentReleaseResponse extends ProtocolEnvelope {
  explorerEnabled: boolean
  release: ProtocolRelease
}

export interface ProtocolAction {
  actionId: string
  transactionHash: string
  blockNumber: string
  blockHash: string
  transactionIndex: string
  logIndex: string
  timestamp: number
  actionType: string
  outcome: string
  account: string | null
  keeper: string | null
  orderId: string | null
  contractAddress: string
  data: Record<string, unknown>
  evidence: Record<string, unknown>
  units: Record<string, string>
}

export interface ProtocolOverviewResponse extends ProtocolEnvelope {
  overview: {
    counts: Record<string, string>
    housePool: Record<string, unknown> | null
    protocolStatus: Record<string, unknown> | null
    anomalies: {
      code: string
      severity: 'info' | 'warning' | 'critical'
      message: string
      details: Record<string, unknown> | null
    }[]
    anomalyEvaluation: 'complete' | 'partial'
    indexerLagBlocks: string
  }
}

export interface ProtocolTransactionsResponse extends ProtocolEnvelope {
  transactions: {
    items: ProtocolAction[]
    nextCursor: string | null
    filters: Record<string, string | null>
  }
}

export interface ProtocolTransactionStateImpact {
  accounts: Record<string, unknown>[]
  housePool: Record<string, unknown> | null
  senior: Record<string, unknown> | null
  junior: Record<string, unknown> | null
  sourceBlocks: Record<string, unknown>
  provenance: unknown
  formula: unknown
  evidenceReferences: unknown
  availability?: AvailabilityReason[]
}

export interface ProtocolTransactionAnalysis {
  economics: Record<string, unknown>
  liquidations: Record<string, unknown>[]
  marginActions: Record<string, unknown>[]
  trancheActions: Record<string, unknown>[]
  availability: AvailabilityReason[]
  provenance: unknown
  formula?: unknown
  evidenceReferences?: unknown
}

export interface ProtocolTransactionResponse extends ProtocolEnvelope {
  transaction: {
    chainTransaction: Record<string, unknown>
    actions: ProtocolAction[]
    events: Record<string, unknown>[]
    batchActionCount: number
    stateImpact: ProtocolTransactionStateImpact
    analysis: ProtocolTransactionAnalysis
  }
}

export interface ProtocolOrderResponse extends ProtocolEnvelope {
  order: Record<string, unknown>
}

export interface HousePoolResponse extends ProtocolEnvelope {
  housePool: Record<string, unknown> | null
}

export interface TrancheResponse extends ProtocolEnvelope {
  tranche: Record<string, unknown>
}

export interface TrancheHistoryCheckpoint {
  blockNumber: string
  blockHash: string
  timestamp: number
  principalUsdc: string | null
  navUsdc: string | null
  shareSupply: string | null
  assetsPerShare: string | null
  drawdownUsdc: string | null
  impairmentGapUsdc: string | null
  coverageRatioBps: string | null
  calculationVersion: string
  formulaIdentifier: string
  formula: Record<string, string>
  evidence: Record<string, unknown>
  sourceScopes: {
    scope: string
    blockNumber: string
    blockHash: string
  }[]
  availability: AvailabilityReason[]
  units: Record<string, string>
}

export interface TrancheHistoryResponse extends ProtocolEnvelope {
  history: {
    tranche: string
    items: ProtocolAction[]
    nextCursor: string | null
    nextCursors: {
      combined: string | null
      actions: string | null
      checkpoints: string | null
    }
    pagination: {
      actionsComplete: boolean
      checkpointsComplete: boolean
    }
    checkpoints: TrancheHistoryCheckpoint[]
    csvColumns: string[]
  }
}

export interface KeepersResponse extends ProtocolEnvelope {
  keepers: {
    window: string
    definition: string
    activeKeeperCount: string
    actionCount: string
    backlogProcessed: string
    actionMix: Record<string, string>
    latencySeconds: Record<string, string | null>
    observedLiquidationRewardsUsdc?: string
    totalGrossRewardsUsdc?: null
    nativeGasAndPythCosts: {
      gasCostWei: string
      transactionNativeValueWei: string
      missingGasReceiptCount: string
      missingNativeValueCount: string
      nativeValueInterpretation: string
      profitUsdc: null
    }
    observedRewardConcentration?: {
      topOneShareBps: string | null
      topThreeShareBps: string | null
      slices: {
        address: string
        observedLiquidationRewardsUsdc: string
      }[]
    }
    keepers: Record<string, unknown>[]
    nextCursor: string | null
    units: Record<string, string>
    /** Compatibility with responses produced during the protocol-explorer rollout. */
    grossRewardsUsdc?: string
    /** Compatibility with responses produced during the protocol-explorer rollout. */
    rewardConcentration?: {
      address: string
      grossRewardsUsdc: string
    }[]
    /** Compatibility with responses produced during the protocol-explorer rollout. */
    topOneRewardShareBps?: string | null
    /** Compatibility with responses produced during the protocol-explorer rollout. */
    topThreeRewardShareBps?: string | null
  }
}

export interface KeeperResponse extends ProtocolEnvelope {
  keeper: {
    address: string
    window: string
    summary: Record<string, unknown>
    actions: ProtocolAction[]
    nextCursor: string | null
  }
}

export interface ParametersResponse extends ProtocolEnvelope {
  parameters: {
    current: {
      definition: Record<string, unknown>
      rawValue: string | null
      formattedValue: string | null
      effectiveBlock: string
      sourceAddress: string | null
      evidence: string
      availability: AvailabilityReason[]
    }[]
    pending: Record<string, unknown>[]
    /** Legacy preview field; current servers expose paginated history only via parameterChangesPath. */
    history?: Record<string, unknown>[]
    parameterChangesPath: string
    catalogVersion: string
  }
}

export interface ParameterChangesResponse extends ProtocolEnvelope {
  parameterChanges: {
    items: Record<string, unknown>[]
    nextCursor: string | null
  }
}

export interface ProtocolWalletActivity {
  activityId: string
  transactionHash: string | null
  timestamp: number | null
  actionType: string
  outcome: string
  gasCostWei: string | null
  nativeValueWei: string | null
  evidence: Record<string, unknown>
  availability: AvailabilityReason[]
  raw: Record<string, unknown>
}

export interface ProtocolWalletSummary {
  address: string
  roles: string[]
  roleSources: Record<string, unknown>[]
  status: string
  nativeBalanceWei: string | null
  observedGasCostWei: string | null
  observedTransactionNativeValueWei: string | null
  observedActionCount: string | null
  observedTransactionCount: string | null
  medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei: string | null
  estimatedTransactionsAtObservedGrossSpend: string | null
  runwayFormula: Record<string, unknown> | null
  lastActivityTimestamp: number | null
  lastActivityTransactionHash: string | null
  evidence: Record<string, unknown>
  availability: AvailabilityReason[]
  raw: Record<string, unknown>
}

export interface ProtocolWalletsResponse extends ProtocolEnvelope {
  wallets: {
    window: string
    windowStart: number | null
    windowEnd: number | null
    definition: Record<string, unknown>
    items: ProtocolWalletSummary[]
    nextCursor: string | null
    oracleUpdaterIdentityAvailable: boolean | null
    oracleUpdaterActivityAttributable: boolean | null
    totalTrackedWalletCount: string | null
    totalAtRiskWalletCount: string | null
    units: Record<string, string>
  }
}

export interface ProtocolWalletResponse extends ProtocolEnvelope {
  wallet: ProtocolWalletSummary & {
    activity: ProtocolWalletActivity[]
    nextCursor: string | null
  }
}
