import type { Address } from 'viem'

// Token types
export type TokenSymbol = 'USDC' | 'plDXY-BEAR' | 'plDXY-BULL' | 'splDXY-BEAR' | 'splDXY-BULL'

export interface Token {
  symbol: TokenSymbol
  name: string
  address: Address
  decimals: number
  icon?: string
}

// Position types
export type PositionSide = 'BEAR' | 'BULL'

export interface LeveragePosition {
  id: string
  side: PositionSide
  size: bigint
  collateral: bigint
  leverage: number
  entryPrice: bigint
  liquidationPrice: bigint
  healthFactor: number
  pnl: bigint
  pnlPercentage: number
}

export interface StakingPosition {
  side: PositionSide
  stakedAmount: bigint
  pendingRewards: bigint
}

export interface MorphoPosition {
  supplied: bigint
  borrowed: bigint
  availableToBorrow: bigint
}

// Transaction history
export type TransactionType =
  | 'mint'
  | 'burn'
  | 'swap_buy_bear'
  | 'swap_sell_bear'
  | 'swap_buy_bull'
  | 'swap_sell_bull'
  | 'stake_bear'
  | 'stake_bull'
  | 'unstake_bear'
  | 'unstake_bull'
  | 'leverage_open'
  | 'leverage_close'
  | 'leverage_adjust'
  | 'morpho_supply'
  | 'morpho_withdraw'
  | 'morpho_borrow'
  | 'morpho_repay'

export interface HistoricalTransaction {
  id: string
  hash: string
  type: TransactionType
  timestamp: number
  amount: bigint
  tokenSymbol: TokenSymbol
  status: 'success' | 'failed'
}

// Portfolio
export interface PortfolioSummary {
  totalValueUsd: bigint
  spotValue: bigint
  stakedValue: bigint
  leverageValue: bigint
  lendingValue: bigint
}
