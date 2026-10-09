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
  multicallHandlerCodeHash: Hex
  destinationSpokePoolCodeHash: Hex
  destinationSpokePoolImplementation: Address
  destinationSpokePoolImplementationCodeHash: Hex
  confirmations: number
  startBlock: number
  releaseId: string
  provider: string
  destinationChainId: number
  token: Address
  clearinghouse: Address
  multicallHandler: Address
  destinationSpokePool: Address
  sources: FundingSource[]
}

export interface FundingAccountDepositDestination {
  owner: Address
  beneficiary: Address
  destinationChainId: number
  token: Address
  clearinghouse: Address
  releaseId: string
}

export interface FundingDestination extends FundingAccountDepositDestination {
  multicallHandler: Address
  destinationSpokePool: Address
}

export interface FundingConfig {
  clearinghouseCodeHash?: Hex
  multicallHandlerCodeHash?: Hex
  destinationSpokePoolCodeHash?: Hex
  destinationSpokePoolImplementation?: Address
  destinationSpokePoolImplementationCodeHash?: Hex
  confirmations?: number
  startBlock?: number
  enabled: boolean
  reason?: string
  provider?: string
  destinationChainId?: number
  releaseId?: string
  clearinghouse?: Address
  token?: Address
  multicallHandler?: Address
  destinationSpokePool?: Address
}

export interface SourceTransaction {
  kind: 'approval' | 'bridge'
  chainId: number
  to: Address
  data: Hex
  value: string
}

export interface FundingQuote {
  ownerAddress: Address
  multicallHandler: Address
  destinationSpokePool: Address
  quoteId: Hex
  destinationMessage: Hex
  expiresAt: number
  provider: string
  sourceChainId: number
  sourceToken: Address
  sourceAmount: string
  destinationChainId: number
  token: Address
  beneficiary: Address
  clearinghouse: Address
  releaseId: string
  estimatedAmount: string
  minimumAmount: string
  sourceTransactions: SourceTransaction[]
}

export type FundingStatus = 'awaiting-source' | 'bridging' | 'received' | 'depositing' | 'confirmed' | 'needs-deposit' | 'failed'

export interface FundingIntent extends FundingQuote {
  intentId: string
  status: FundingStatus
  sourceTxHash?: Hex
  depositTxHash?: Hex
  depositBlockNumber?: string
  depositBlockHash?: Hex
  creditedAmount?: string
  fallbackTxHash?: Hex
  fallbackBlockNumber?: string
  fallbackBlockHash?: Hex
  fallbackAmount?: string
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
