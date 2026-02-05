/**
 * API response and data types for the Plether backend API.
 * These types match the backend API specification in specs/backend-api.md
 */

// =============================================================================
// Common Types
// =============================================================================

export interface ApiResponse<T> {
  data: T;
  meta: ApiMeta;
}

export interface ApiMeta {
  cached: boolean;
  cachedAt?: number;
  stale?: boolean;
  blockNumber: number;
  chainId: number;
}

export interface ApiError {
  error: {
    code: ApiErrorCode;
    message: string;
    details?: unknown;
  };
}

export type ApiErrorCode =
  | 'INVALID_ADDRESS'
  | 'INVALID_AMOUNT'
  | 'INVALID_SIDE'
  | 'RPC_ERROR'
  | 'RATE_LIMITED'
  | 'INTERNAL_ERROR'
  | 'NETWORK_ERROR';

// =============================================================================
// Protocol Types
// =============================================================================

export interface ProtocolStatus {
  prices: {
    bear: string;
    bull: string;
    cap: string;
  };
  status: ProtocolState;
  oracle: {
    price: string;
    updatedAt: number;
    decimals: number;
  };
  staking: {
    bear: StakingStats;
    bull: StakingStats;
  };
  apy: {
    bear: ApyStats;
    bull: ApyStats;
  };
  timestamp: number;
}

export type ProtocolState = 'ACTIVE' | 'PAUSED' | 'LIQUIDATED';

export interface StakingStats {
  totalAssets: string;
  totalShares: string;
  exchangeRate: string;
}

export interface ApyStats {
  supply: number;
  borrow: number;
  utilization: number;
}

export interface ProtocolConfig {
  contracts: {
    usdc: string;
    dxyBear: string;
    dxyBull: string;
    sdxyBear: string;
    sdxyBull: string;
    syntheticSplitter: string;
    curvePool: string;
    zapRouter: string;
    leverageRouter: string;
    bullLeverageRouter: string;
    basketOracle: string;
    morpho: string;
    morphoBearMarket: string;
    morphoBullMarket: string;
  };
  decimals: {
    usdc: 6;
    plDxyBear: 18;
    plDxyBull: 18;
    oraclePrice: 8;
    morphoShares: 18;
  };
  constants: {
    maxSlippage: number;
    minLeverage: number;
    maxLeverage: number;
    liquidationLtv: number;
  };
  chainId: number;
}

// =============================================================================
// User Types
// =============================================================================

export interface UserDashboard {
  balances: UserBalances;
  leverage: {
    bear: LeveragePosition | null;
    bull: LeveragePosition | null;
  };
  lending: {
    bear: LendingPosition | null;
    bull: LendingPosition | null;
  };
}

export interface UserBalances {
  usdc: string;
  bear: string;
  bull: string;
  stakedBear: string;
  stakedBull: string;
  stakedBearAssets: string;
  stakedBullAssets: string;
}

export interface LeveragePosition {
  collateral: string;
  collateralUsd: string;
  debt: string;
  healthFactor: string;
  liquidationPrice: string;
  leverage: string;
  netValue: string;
}

export interface LendingPosition {
  supplied: string;
  suppliedShares: string;
  borrowed: string;
  borrowedShares: string;
  availableToBorrow: string;
  collateral: string;
  healthFactor: string;
}

export interface UserAllowances {
  usdc: {
    splitter: string;
    zap: string;
    morphoBear: string;
    morphoBull: string;
  };
  bear: {
    splitter: string;
    staking: string;
    leverageRouter: string;
    curvePool: string;
  };
  bull: {
    splitter: string;
    staking: string;
    leverageRouter: string;
  };
}

export interface UserPositions {
  leverage: {
    bear: LeveragePosition | null;
    bull: LeveragePosition | null;
  };
  lending: {
    bear: LendingPosition | null;
    bull: LendingPosition | null;
  };
}

// =============================================================================
// Quote Types
// =============================================================================

export interface MintQuote {
  usdcIn: string;
  bearOut: string;
  bullOut: string;
  pricePerToken: string;
}

export interface BurnQuote {
  pairIn: string;
  usdcOut: string;
  bearIn: string;
  bullIn: string;
}

export interface ZapQuote {
  direction: 'buy' | 'sell';
  input: {
    token: 'usdc' | 'bull';
    amount: string;
  };
  output: {
    token: 'bull' | 'usdc';
    amount: string;
    minAmount: string;
  };
  priceImpact: string;
  route: string[];
}

export interface TradeQuote {
  from: 'usdc' | 'bear';
  to: 'bear' | 'usdc';
  amountIn: string;
  amountOut: string;
  minAmountOut: string;
  spotPrice: string;
  priceImpact: string;
  fee: string;
}

export interface LeverageQuote {
  side: 'bear' | 'bull';
  principal: string;
  leverage: string;
  positionSize: string;
  positionSizeUsd: string;
  debt: string;
  healthFactor: string;
  liquidationPrice: string;
  priceImpact: string;
  borrowRate: string;
}

// =============================================================================
// History Types
// =============================================================================

export interface TransactionHistory {
  transactions: Transaction[];
  pagination: Pagination;
}

export interface Pagination {
  page: number;
  limit: number;
  total: number;
  hasMore: boolean;
}

export interface Transaction {
  id: string;
  type: TransactionType;
  timestamp: number;
  blockNumber: number;
  side?: 'bear' | 'bull';
  data: TransactionData;
  status: 'success' | 'failed';
}

export type TransactionType =
  | 'mint'
  | 'burn'
  | 'zap_buy'
  | 'zap_sell'
  | 'swap'
  | 'stake'
  | 'unstake'
  | 'leverage_open'
  | 'leverage_close'
  | 'collateral_add'
  | 'collateral_remove'
  | 'supply'
  | 'withdraw'
  | 'borrow'
  | 'repay';

export type TransactionData =
  | MintTransactionData
  | BurnTransactionData
  | ZapTransactionData
  | SwapTransactionData
  | StakeTransactionData
  | LeverageOpenData
  | LeverageCloseData
  | CollateralAdjustData
  | LendingTransactionData;

export interface MintTransactionData {
  usdcIn: string;
  bearOut: string;
  bullOut: string;
}

export interface BurnTransactionData {
  bearIn: string;
  bullIn: string;
  usdcOut: string;
}

export interface ZapTransactionData {
  direction: 'buy' | 'sell';
  usdcAmount: string;
  bullAmount: string;
}

export interface SwapTransactionData {
  from: 'usdc' | 'bear';
  to: 'bear' | 'usdc';
  amountIn: string;
  amountOut: string;
}

export interface StakeTransactionData {
  side: 'bear' | 'bull';
  assets: string;
  shares: string;
}

export interface LeverageOpenData {
  side: 'bear' | 'bull';
  principal: string;
  leverage: string;
  positionSize: string;
  debt: string;
}

export interface LeverageCloseData {
  side: 'bear' | 'bull';
  collateral: string;
  debt: string;
  profit: string;
}

export interface CollateralAdjustData {
  side: 'bear' | 'bull';
  amount: string;
  isAdd: boolean;
}

export interface LendingTransactionData {
  side: 'bear' | 'bull';
  action: 'supply' | 'withdraw' | 'borrow' | 'repay';
  assets: string;
  shares: string;
}

// =============================================================================
// WebSocket Types
// =============================================================================

export type WebSocketMessage =
  | PricesMessage
  | StatusMessage
  | BalanceMessage
  | PositionMessage
  | BlockMessage
  | PingMessage;

export interface PricesMessage {
  type: 'prices';
  data: {
    bear: string;
    bull: string;
    oracle: string;
    timestamp: number;
    blockNumber: number;
  };
}

export interface StatusMessage {
  type: 'status';
  data: {
    status: ProtocolState;
  };
}

export interface BalanceMessage {
  type: 'balance';
  data: {
    token: 'usdc' | 'bear' | 'bull' | 'stakedBear' | 'stakedBull';
    amount: string;
  };
}

export interface PositionMessage {
  type: 'position';
  data: {
    type: 'leverage' | 'lending';
    side: 'bear' | 'bull';
    position: LeveragePosition | LendingPosition | null;
  };
}

export interface BlockMessage {
  type: 'block';
  data: {
    number: number;
    timestamp: number;
  };
}

export interface PingMessage {
  type: 'ping';
}

export type WebSocketClientMessage =
  | { type: 'subscribe'; address: string }
  | { type: 'unsubscribe' }
  | { type: 'pong' };

// =============================================================================
// Request Parameter Types
// =============================================================================

export type Side = 'bear' | 'bull';
export type ZapDirection = 'buy' | 'sell';
export type TradeFrom = 'usdc' | 'bear';

export interface HistoryParams {
  page?: number;
  limit?: number;
  type?: TransactionType;
  side?: Side;
}

export interface AllowancesParams {
  spenders?: string[];
}
