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

export interface PriceChange {
  bear: number;
  bull: number;
}

export interface ProtocolStatus {
  prices: {
    bear: string;
    bull: string;
    cap: string;
    priceChange24h: PriceChange | null;
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
    apy7d: { bear: number | null; bull: number | null };
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

export interface MarketConfig {
  bearId: string;
  bullId: string;
  bearLltv: string;
  bullLltv: string;
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
    adverseConfidenceMultiplierBps: string;
  };
  markets: MarketConfig;
  chainId: number;
}

// =============================================================================
// Perps Basket Types
// =============================================================================

export type BasketHistoryRange = '24h' | '7d' | '30d' | '1y';

export interface BasketComponentPrice {
  symbol: string;
  feedSymbol: string;
  feedId: string;
  price: string;
  rawPrice: string;
  confidence: string;
  exponent: number;
  publishTime: number;
  inverted: boolean;
  weightBps: number;
  basePrice: string;
}

export interface BasketHistoryPoint {
  timestamp: number;
  basketPrice: string;
  volumeUsdc?: string;
  components?: BasketComponentPrice[];
}

export interface BasketHistory {
  range: BasketHistoryRange;
  intervalSeconds: number;
  source: 'pyth_benchmarks';
  generatedAt: number;
  latestPrice: string | null;
  changePct: number | null;
  points: BasketHistoryPoint[];
}

export interface BasketLatest {
  timestamp: number;
  basketPrice: string;
  components: BasketComponentPrice[];
  generatedAt: number;
  source: string;
}

export type PerpsCandleIntervalSeconds = 60 | 180 | 300 | 900 | 1800 | 3600 | 86400;
export type PerpsBasketCandleQuality = 'observed' | 'legacy_sampled' | 'mixed';

/**
 * A backend-aggregated candle in the contract's raw oracle-price domain.
 * Consumers displaying plDXY must invert the prices and swap high/low.
 */
export interface PerpsBasketCandle {
  timestamp: number;
  rawOpenPrice: string;
  rawHighPrice: string;
  rawLowPrice: string;
  rawClosePrice: string;
  /** Null means the volume source has not proved coverage for this bucket. */
  volumeUsdc: string | null;
  /** Null means trade-count coverage is unknown, which is distinct from zero. */
  tradeCount: number | null;
  sampleCount: number;
  quality: PerpsBasketCandleQuality;
  revision: number;
  /** Whether the price source proved this bucket. Native chart availability is price-led. */
  priceComplete: boolean;
  /** Whether the current chain/router volume source proved this bucket. */
  volumeComplete: boolean;
  /** Legacy combined completeness: priceComplete && volumeComplete. */
  complete: boolean;
}

export interface PerpsBasketCandlePage {
  intervalSeconds: PerpsCandleIntervalSeconds;
  /** Exclusive end of this fixed 500-bucket page. */
  cursor: number;
  /** Immutable basket-series identity for every candle in this response. */
  seriesId: string;
  /** Hash of the immutable basket configuration used by this series. */
  configurationHash: string;
  /** Raw oracle-domain value representing a displayed price of zero. */
  displayPriceCap: string;
  /** Chain half of the canonical current-router volume scope. */
  volumeChainId: number;
  /** Canonical current-router scope used for nullable candle volume. */
  volumeRouter: string;
  /** Inclusive start of trusted coverage for the current chain/router, if published. */
  volumeCoverageStart: number | null;
  /** Exclusive end of trusted coverage for the current chain/router, if published. */
  volumeCoverageEnd: number | null;
  /** Exclusive end through which current-router volume is finalized. */
  volumeFinalizedThrough: number | null;
  /** Whether the current-router volume coverage bounds are safe to use. */
  volumeCoverageComplete: boolean;
  /** Exclusive end cursor for the next older page, or null at coverage start. */
  previousCursor: number | null;
  hasEarlier: boolean;
  coverageStart: number | null;
  coverageEnd: number | null;
  coverageComplete: boolean;
  finalizedThrough: number | null;
  datasetGeneration: number;
  candles: PerpsBasketCandle[];
}

export interface PerpsBasketCurrentCandle {
  intervalSeconds: PerpsCandleIntervalSeconds;
  seriesId: string;
  configurationHash: string;
  displayPriceCap: string;
  volumeChainId: number;
  volumeRouter: string;
  volumeCoverageStart: number | null;
  volumeCoverageEnd: number | null;
  volumeFinalizedThrough: number | null;
  volumeCoverageComplete: boolean;
  datasetGeneration: number;
  coverageStart: number | null;
  coverageEnd: number | null;
  coverageComplete: boolean;
  finalizedThrough: number | null;
  candle: PerpsBasketCandle | null;
}

export interface PerpsRevealPayload {
  orderId: string;
  updateData: string[];
  fetchedAt: number;
  publishTimes: number[];
  minPublishTime: number;
  maxPublishTime: number;
  source: string;
}

export interface PerpsMarketStats {
  rangeSeconds: number;
  generatedAt: number;
  volume24hUsdc: string;
}

export interface TestnetFaucetClaim {
  address: string;
  amount: string;
  token: string;
  txHash: string | null;
  status: 'submitted' | 'minted' | 'already_claimed' | 'already_funded';
}

// =============================================================================
// User Types
// =============================================================================

export interface MorphoAuthorization {
  bearLeverageRouter: boolean;
  bullLeverageRouter: boolean;
}

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
  allowances: UserAllowances;
  authorization: MorphoAuthorization;
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
    curvePool: string;
    leverageRouter: string;
    bullLeverageRouter: string;
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
    zapRouter: string;
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
  hash: string;
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
  | 'repay'
  | 'lending_supply'
  | 'lending_withdraw'
  | 'lending_borrow'
  | 'lending_repay';

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
