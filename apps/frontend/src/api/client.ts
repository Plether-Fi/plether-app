/**
 * Plether API Client
 *
 * Typed client for interacting with the Plether backend API.
 * Handles HTTP requests, error parsing, and WebSocket connections.
 */

import { Result } from 'better-result';
import type {
  ApiResponse,
  ApiError,
  ApiErrorCode,
  ProtocolStatus,
  ProtocolConfig,
  UserDashboard,
  UserBalances,
  UserPositions,
  UserAllowances,
  MintQuote,
  BurnQuote,
  ZapQuote,
  TradeQuote,
  LeverageQuote,
  TransactionHistory,
  WebSocketMessage,
  WebSocketClientMessage,
  Side,
  ZapDirection,
  TradeFrom,
  HistoryParams,
  AllowancesParams,
  PricesMessage,
} from './types';

// =============================================================================
// Error Types
// =============================================================================

export class PlethApiError extends Error {
  readonly code: ApiErrorCode;
  readonly status?: number;
  readonly details?: unknown;

  constructor(code: ApiErrorCode, message: string, status?: number, details?: unknown) {
    super(message);
    this.name = 'PlethApiError';
    this.code = code;
    this.status = status;
    this.details = details;
  }
}

// =============================================================================
// Configuration
// =============================================================================

export interface PlethApiConfig {
  baseUrl: string;
  wsUrl?: string;
  timeout?: number;
  onError?: (error: PlethApiError) => void;
}

function deriveWsUrl(baseUrl: string): string {
  if (baseUrl.startsWith('http')) return baseUrl.replace(/^http/, 'ws');
  const proto = location.protocol === 'https:' ? 'wss:' : 'ws:';
  return `${proto}//${location.host}${baseUrl}`;
}

const DEV_API_URL = import.meta.env.VITE_API_URL as string | undefined;

export function chainIdToApiPath(chainId: number): string {
  if (chainId === 11155111) return '/api/sepolia_v1';
  return '/api/v1';
}

function getInitialBaseUrl(): string {
  if (DEV_API_URL) return DEV_API_URL;
  return chainIdToApiPath(1);
}

const DEFAULT_CONFIG: Required<Omit<PlethApiConfig, 'onError'>> = {
  baseUrl: getInitialBaseUrl(),
  wsUrl: deriveWsUrl(getInitialBaseUrl()),
  timeout: 30000,
};

// =============================================================================
// HTTP Client
// =============================================================================

async function fetchApi<T>(
  config: PlethApiConfig,
  path: string,
  options?: RequestInit
): Promise<Result<ApiResponse<T>, PlethApiError>> {
  const url = `${config.baseUrl}${path}`;
  const controller = new AbortController();
  const timeoutId = setTimeout(() => {
    controller.abort();
  }, config.timeout ?? DEFAULT_CONFIG.timeout);

  try {
    const response = await fetch(url, {
      method: options?.method,
      body: options?.body,
      cache: options?.cache,
      credentials: options?.credentials,
      integrity: options?.integrity,
      keepalive: options?.keepalive,
      mode: options?.mode,
      redirect: options?.redirect,
      referrer: options?.referrer,
      referrerPolicy: options?.referrerPolicy,
      signal: controller.signal,
      headers: Object.assign({ 'Content-Type': 'application/json' }, options?.headers),
    });

    clearTimeout(timeoutId);

    if (!response.ok) {
      const errorBody = (await response.json().catch(() => ({
        error: {
          code: 'INTERNAL_ERROR' as ApiErrorCode,
          message: response.statusText,
        },
      }))) as ApiError;

      const apiError = new PlethApiError(
        errorBody.error.code,
        errorBody.error.message,
        response.status,
        errorBody.error.details
      );

      config.onError?.(apiError);
      return Result.err(apiError);
    }

    const data = (await response.json()) as ApiResponse<T>;
    return Result.ok(data);
  } catch (err) {
    clearTimeout(timeoutId);

    const apiError = new PlethApiError(
      'NETWORK_ERROR',
      err instanceof Error ? err.message : 'Network request failed',
      undefined,
      err
    );

    config.onError?.(apiError);
    return Result.err(apiError);
  }
}

// =============================================================================
// API Client Class
// =============================================================================

export class PlethApiClient {
  private config: PlethApiConfig;
  private ws: WebSocket | null = null;
  private wsReconnectAttempts = 0;
  private readonly maxReconnectAttempts = 5;
  private wsListeners = new Set<(message: WebSocketMessage) => void>();
  private reconnectTimeout: ReturnType<typeof setTimeout> | null = null;
  private chainId: number | undefined;

  constructor(config?: Partial<PlethApiConfig>) {
    this.config = { ...DEFAULT_CONFIG, ...config };
  }

  setChainId(chainId: number): void {
    if (DEV_API_URL || chainId === this.chainId) return;
    this.chainId = chainId;
    const baseUrl = chainIdToApiPath(chainId);
    this.config.baseUrl = baseUrl;
    this.config.wsUrl = deriveWsUrl(baseUrl);
    this.reconnectWebSocket();
  }

  private reconnectWebSocket(): void {
    if (!this.ws && !this.reconnectTimeout) return;
    const hadConnection = this.ws !== null || this.reconnectTimeout !== null;
    this.disconnectWebSocket();
    if (hadConnection) {
      this.wsReconnectAttempts = 0;
      this.connectWebSocket();
    }
  }

  // ===========================================================================
  // Protocol Endpoints
  // ===========================================================================

  async getProtocolStatus(): Promise<Result<ApiResponse<ProtocolStatus>, PlethApiError>> {
    return fetchApi<ProtocolStatus>(this.config, '/protocol/status');
  }

  async getProtocolConfig(): Promise<Result<ApiResponse<ProtocolConfig>, PlethApiError>> {
    return fetchApi<ProtocolConfig>(this.config, '/protocol/config');
  }

  // ===========================================================================
  // User Endpoints
  // ===========================================================================

  async getUserDashboard(
    address: string
  ): Promise<Result<ApiResponse<UserDashboard>, PlethApiError>> {
    return fetchApi<UserDashboard>(this.config, `/user/${address}/dashboard`);
  }

  async getUserBalances(
    address: string
  ): Promise<Result<ApiResponse<UserBalances>, PlethApiError>> {
    return fetchApi<UserBalances>(this.config, `/user/${address}/balances`);
  }

  async getUserPositions(
    address: string
  ): Promise<Result<ApiResponse<UserPositions>, PlethApiError>> {
    return fetchApi<UserPositions>(this.config, `/user/${address}/positions`);
  }

  async getUserAllowances(
    address: string,
    params?: AllowancesParams
  ): Promise<Result<ApiResponse<UserAllowances>, PlethApiError>> {
    const searchParams = new URLSearchParams();
    if (params?.spenders?.length) {
      searchParams.set('spenders', params.spenders.join(','));
    }
    const query = searchParams.toString();
    const path = `/user/${address}/allowances${query ? `?${query}` : ''}`;
    return fetchApi<UserAllowances>(this.config, path);
  }

  // ===========================================================================
  // Quote Endpoints
  // ===========================================================================

  async getMintQuote(amount: string): Promise<Result<ApiResponse<MintQuote>, PlethApiError>> {
    return fetchApi<MintQuote>(this.config, `/quotes/mint?amount=${amount}`);
  }

  async getBurnQuote(amount: string): Promise<Result<ApiResponse<BurnQuote>, PlethApiError>> {
    return fetchApi<BurnQuote>(this.config, `/quotes/burn?amount=${amount}`);
  }

  async getZapQuote(
    direction: ZapDirection,
    amount: string
  ): Promise<Result<ApiResponse<ZapQuote>, PlethApiError>> {
    return fetchApi<ZapQuote>(
      this.config,
      `/quotes/zap?direction=${direction}&amount=${amount}`
    );
  }

  async getTradeQuote(
    from: TradeFrom,
    amount: string
  ): Promise<Result<ApiResponse<TradeQuote>, PlethApiError>> {
    return fetchApi<TradeQuote>(this.config, `/quotes/trade?from=${from}&amount=${amount}`);
  }

  async getLeverageQuote(
    side: Side,
    principal: string,
    leverage: string
  ): Promise<Result<ApiResponse<LeverageQuote>, PlethApiError>> {
    return fetchApi<LeverageQuote>(
      this.config,
      `/quotes/leverage?side=${side}&principal=${principal}&leverage=${leverage}`
    );
  }

  // ===========================================================================
  // History Endpoints
  // ===========================================================================

  async getTransactionHistory(
    address: string,
    params?: HistoryParams
  ): Promise<Result<ApiResponse<TransactionHistory>, PlethApiError>> {
    const searchParams = new URLSearchParams();
    if (params?.page) searchParams.set('page', String(params.page));
    if (params?.limit) searchParams.set('limit', String(params.limit));
    if (params?.type) searchParams.set('type', params.type);
    if (params?.side) searchParams.set('side', params.side);
    const query = searchParams.toString();
    const path = `/user/${address}/history${query ? `?${query}` : ''}`;
    return fetchApi<TransactionHistory>(this.config, path);
  }

  async getLeverageHistory(
    address: string,
    params?: { side?: Side; page?: number; limit?: number }
  ): Promise<Result<ApiResponse<TransactionHistory>, PlethApiError>> {
    const searchParams = new URLSearchParams();
    if (params?.page) searchParams.set('page', String(params.page));
    if (params?.limit) searchParams.set('limit', String(params.limit));
    if (params?.side) searchParams.set('side', params.side);
    const query = searchParams.toString();
    const path = `/user/${address}/history/leverage${query ? `?${query}` : ''}`;
    return fetchApi<TransactionHistory>(this.config, path);
  }

  async getLendingHistory(
    address: string,
    params?: { side?: Side; page?: number; limit?: number }
  ): Promise<Result<ApiResponse<TransactionHistory>, PlethApiError>> {
    const searchParams = new URLSearchParams();
    if (params?.page) searchParams.set('page', String(params.page));
    if (params?.limit) searchParams.set('limit', String(params.limit));
    if (params?.side) searchParams.set('side', params.side);
    const query = searchParams.toString();
    const path = `/user/${address}/history/lending${query ? `?${query}` : ''}`;
    return fetchApi<TransactionHistory>(this.config, path);
  }

  // ===========================================================================
  // WebSocket
  // ===========================================================================

  connectWebSocket(address?: string): void {
    if (this.ws?.readyState === WebSocket.OPEN) {
      if (address) {
        this.send({ type: 'subscribe', address });
      }
      return;
    }

    const wsUrl = this.config.wsUrl ?? DEFAULT_CONFIG.wsUrl;
    const url = address ? `${wsUrl}/ws?address=${address}` : `${wsUrl}/ws`;

    this.ws = new WebSocket(url);

    this.ws.onopen = () => {
      this.wsReconnectAttempts = 0;
    };

    this.ws.onmessage = (event) => {
      try {
        const message = JSON.parse(String(event.data)) as WebSocketMessage;

        if (message.type === 'ping') {
          this.send({ type: 'pong' });
          return;
        }

        for (const listener of this.wsListeners) {
          listener(message);
        }
      } catch {
        // Ignore malformed messages
      }
    };

    this.ws.onclose = () => {
      this.ws = null;
      this.attemptReconnect(address);
    };

    this.ws.onerror = () => {
      this.ws?.close();
    };
  }

  private attemptReconnect(address?: string): void {
    if (this.wsReconnectAttempts >= this.maxReconnectAttempts) {
      return;
    }

    const delay = Math.min(1000 * Math.pow(2, this.wsReconnectAttempts), 30000);
    this.wsReconnectAttempts++;

    this.reconnectTimeout = setTimeout(() => {
      this.connectWebSocket(address);
    }, delay);
  }

  disconnectWebSocket(): void {
    if (this.reconnectTimeout) {
      clearTimeout(this.reconnectTimeout);
      this.reconnectTimeout = null;
    }
    this.wsReconnectAttempts = this.maxReconnectAttempts; // Prevent reconnection
    this.ws?.close();
    this.ws = null;
  }

  private send(message: WebSocketClientMessage): void {
    if (this.ws?.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify(message));
    }
  }

  subscribeToUser(address: string): void {
    this.send({ type: 'subscribe', address });
  }

  unsubscribeFromUser(): void {
    this.send({ type: 'unsubscribe' });
  }

  onMessage(listener: (message: WebSocketMessage) => void): () => void {
    this.wsListeners.add(listener);
    return () => {
      this.wsListeners.delete(listener);
    };
  }

  subscribeToPrices(callback: (prices: PricesMessage['data']) => void): () => void {
    return this.onMessage((message) => {
      if (message.type === 'prices') {
        callback(message.data);
      }
    });
  }

  get isWebSocketConnected(): boolean {
    return this.ws?.readyState === WebSocket.OPEN;
  }
}

// =============================================================================
// Default Client Instance
// =============================================================================

export const plethApi = new PlethApiClient();
