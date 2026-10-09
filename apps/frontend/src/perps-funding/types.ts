import type { Address, Hex } from 'viem'

export interface FundingSource {
  chainId: number
  name: string
  token: Address
  symbol: string
  decimals: number
  transactionTargets: Address[]
  approvalSpenders: Address[]
  swapExchanges: Address[]
  spokePools: Address[]
}

/** A separately reviewed release; no production or testnet route is inferred. */
export interface FundingManifest {
  clearinghouseCodeHash: Hex
  factoryCodeHash: Hex
  confirmations: number
  startBlock: number
  releaseId: string
  provider: string
  destinationChainId: number
  token: Address
  clearinghouse: Address
  receiverFactory: Address
  sources: FundingSource[]
}

export interface FundingDestination {
  owner: Address
  beneficiary: Address
  destinationChainId: number
  token: Address
  clearinghouse: Address
  receiverFactory: Address
  releaseId: string
}

export interface FundingConfig {
  clearinghouseCodeHash?: Hex
  factoryCodeHash?: Hex
  confirmations?: number
  startBlock?: number
  enabled: boolean
  reason?: string
  provider?: string
  destinationChainId?: number
  releaseId?: string
  clearinghouse?: Address
  token?: Address
  receiverFactory?: Address
}

export interface SourceTransaction {
  kind: 'approval' | 'bridge'
  chainId: number
  to: Address
  data: Hex
  value: string
}

export interface FundingQuote {
  intentSalt: Hex
  ownerAddress: Address
  receiverFactory: Address
  quoteId: string
  expiresAt: number
  provider: string
  sourceChainId: number
  sourceToken: Address
  sourceAmount: string
  destinationChainId: number
  token: Address
  beneficiary: Address
  clearinghouse: Address
  receiver: Address
  releaseId: string
  estimatedAmount: string
  minimumAmount: string
  sourceTransactions: SourceTransaction[]
}

export type FundingStatus = 'awaiting-source' | 'bridging' | 'received' | 'depositing' | 'confirmed' | 'retryable' | 'failed'

export interface FundingIntent extends FundingQuote {
  intentId: string
  status: FundingStatus
  sourceTxHash?: Hex
  depositTxHash?: Hex
  depositBlockNumber?: string
  depositBlockHash?: Hex
  creditedAmount?: string
  reason?: string
  bridgeStatus?: 'pending' | 'filled' | 'expired' | 'refunded'
  sourceStatus?: 'pending' | 'confirmed' | 'reverted'
  sourceTerminal?: boolean
  sourceBlockNumber?: string
  sourceBlockHash?: Hex
  sourceConfirmations?: number
}

export interface SavedFunding {
  version: 1
  destination: FundingDestination
  intent: FundingIntent
  /** Persist before reporting the broadcast to the API, so interrupted reporting is resumable. */
  sourceTxHash?: Hex
  /** An interrupted wallet request must not be automatically resent. */
  sourceSubmissionPending?: boolean
}
