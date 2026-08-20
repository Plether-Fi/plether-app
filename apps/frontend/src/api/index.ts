/**
 * Plether API Module
 *
 * Exports the API client, types, and React Query hooks for
 * interacting with the Plether backend API.
 */

// Client
export {
  apiScopeToApiPath,
  createScopedApiClient,
  perpsApi,
  spotApi,
  PlethApiClient,
  PlethApiError,
  chainIdToApiPath,
  getConfiguredApiBaseUrl,
  getConfiguredApiSource,
  getConfiguredApiWsUrl,
  getScopedApiBaseUrl,
  getScopedApiSource,
  getScopedApiWsUrl,
  isUpstreamApiError,
} from './client';
export type { ApiScope, PlethApiConfig } from './client';

// Types
export type {
  // Response types
  ApiResponse,
  ApiMeta,
  ApiError,
  ApiErrorCode,
  // Protocol types
  ProtocolStatus,
  ProtocolState,
  ProtocolConfig,
  MarketConfig,
  StakingStats,
  ApyStats,
  BasketHistory,
  BasketHistoryPoint,
  BasketLatest,
  BasketComponentPrice,
  BasketHistoryRange,
  PerpsBasketCandle,
  PerpsBasketCandleQuality,
  PerpsBasketCandlePage,
  PerpsBasketCurrentCandle,
  PerpsCandleIntervalSeconds,
  PerpsRevealPayload,
  PerpsMarketStats,
  // User types
  UserDashboard,
  UserBalances,
  UserPositions,
  UserAllowances,
  LeveragePosition,
  LendingPosition,
  MorphoAuthorization,
  // Quote types
  MintQuote,
  BurnQuote,
  ZapQuote,
  TradeQuote,
  LeverageQuote,
  // History types
  TransactionHistory,
  Transaction,
  TransactionType,
  TransactionData,
  Pagination,
  // WebSocket types
  WebSocketMessage,
  PricesMessage,
  StatusMessage,
  BalanceMessage,
  PositionMessage,
  BlockMessage,
  // Parameter types
  Side,
  ZapDirection,
  TradeFrom,
  HistoryParams,
  AllowancesParams,
} from './types';

// React Query hooks
export {
  // Protocol hooks
  useProtocolStatus,
  useProtocolConfig,
  usePerpsBasketHistory,
  usePerpsBasketLatest,
  usePerpsBasketCurrentCandle,
  usePerpsMarketStats,
  // User hooks
  useUserDashboard,
  useUserBalances,
  useUserPositions,
  useUserAllowances,
  // Quote hooks
  useMintQuote,
  useBurnQuote,
  useZapQuote,
  useTradeQuote,
  useLeverageQuote,
  // History hooks
  useTransactionHistory,
  useLeverageHistory,
  useLendingHistory,
  // WebSocket hooks
  useWebSocketPrices,
  useWebSocketConnection,
  // Query keys
  apiQueryKeys,
} from './hooks';
