/**
 * Plether API Module
 *
 * Exports the API client, types, and React Query hooks for
 * interacting with the Plether backend API.
 */

// Client
export { plethApi, PlethApiClient, PlethApiError } from './client';
export type { PlethApiConfig } from './client';

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
  StakingStats,
  ApyStats,
  // User types
  UserDashboard,
  UserBalances,
  UserPositions,
  UserAllowances,
  LeveragePosition,
  LendingPosition,
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
