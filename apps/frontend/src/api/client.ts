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
  BasketHistory,
  BasketLatest,
  BasketHistoryRange,
  PerpsRevealPayload,
  PerpsMarketStats,
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
const DEFAULT_API_CHAIN_ID = parseDefaultChainId(import.meta.env.VITE_DEFAULT_CHAIN_ID as string | undefined);
const TESTNET_API_CHAIN_IDS = new Set([11155111, 421614]);

export function chainIdToApiPath(chainId: number): string {
  if (TESTNET_API_CHAIN_IDS.has(chainId)) return '/api/sepolia_v1';
  return '/api/v1';
}

function parseDefaultChainId(value: string | undefined): number {
  const parsed = Number(value);
  return Number.isInteger(parsed) ? parsed : 1;
}

export function defaultApiBaseUrl(): string {
  if (DEV_API_URL) return DEV_API_URL;
  return chainIdToApiPath(DEFAULT_API_CHAIN_ID);
}

export function defaultApiChainId(): number {
  return DEFAULT_API_CHAIN_ID;
}

export function getConfiguredApiBaseUrl(chainId: number): string {
  if (DEV_API_URL) return DEV_API_URL;
  return chainIdToApiPath(chainId);
}

export function getConfiguredApiWsUrl(chainId: number): string {
  return deriveWsUrl(getConfiguredApiBaseUrl(chainId));
}

export function getConfiguredApiSource(): string {
  return DEV_API_URL ? 'VITE_API_URL' : 'active chain route';
}

function getInitialBaseUrl(): string {
  return defaultApiBaseUrl();
}

const DEFAULT_CONFIG: Required<Omit<PlethApiConfig, 'onError'>> = {
  baseUrl: getInitialBaseUrl(),
  wsUrl: deriveWsUrl(getInitialBaseUrl()),
  timeout: 30000,
};

const API_ERROR_CODES = new Set<string>([
  'INVALID_ADDRESS',
  'INVALID_AMOUNT',
  'INVALID_SIDE',
  'RPC_ERROR',
  'RATE_LIMITED',
  'INTERNAL_ERROR',
  'NETWORK_ERROR',
]);

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null;
}

function isApiErrorCode(code: unknown): code is ApiErrorCode {
  return typeof code === 'string' && API_ERROR_CODES.has(code);
}

function isApiError(value: unknown): value is ApiError {
  if (!isRecord(value)) return false;

  const { error } = value;
  if (!isRecord(error)) return false;

  return isApiErrorCode(error.code) && typeof error.message === 'string';
}

function isJsonResponse(response: Response): boolean {
  return response.headers.get('content-type')?.toLowerCase().includes('application/json') ?? false;
}

async function readResponsePreview(response: Response): Promise<string> {
  const body = await response.text().catch(() => '');
  return body.trim().slice(0, 180);
}

function createNonJsonApiError(response: Response, url: string, preview: string): PlethApiError {
  return new PlethApiError(
    'NETWORK_ERROR',
    'network: backend API returned a non-JSON response. Check that the backend is running and the frontend API URL points to it.',
    response.status,
    {
      url,
      contentType: response.headers.get('content-type'),
      preview,
    }
  );
}

async function parseErrorResponse(response: Response, url: string): Promise<PlethApiError> {
  if (!isJsonResponse(response)) {
    return createNonJsonApiError(response, url, await readResponsePreview(response));
  }

  const parsed: unknown = await response.json().catch(() => undefined);
  if (isApiError(parsed)) {
    return new PlethApiError(
      parsed.error.code,
      parsed.error.message,
      response.status,
      parsed.error.details
    );
  }

  return new PlethApiError(
    'INTERNAL_ERROR',
    response.statusText || 'Backend API request failed',
    response.status,
    parsed
  );
}

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
      const apiError = await parseErrorResponse(response, url);

      config.onError?.(apiError);
      return Result.err(apiError);
    }

    if (!isJsonResponse(response)) {
      const apiError = createNonJsonApiError(response, url, await readResponsePreview(response));

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
    const baseUrl = getConfiguredApiBaseUrl(chainId);
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
  // Perps Endpoints
  // ===========================================================================

  async getPerpsBasketHistory(
    range: BasketHistoryRange = '7d',
    intervalSeconds = 60 * 60
  ): Promise<Result<ApiResponse<BasketHistory>, PlethApiError>> {
    return fetchApi<BasketHistory>(
      this.config,
      `/perps/basket/history?range=${range}&interval=${String(intervalSeconds)}`
    );
  }

  async getPerpsBasketLatest(): Promise<Result<ApiResponse<BasketLatest>, PlethApiError>> {
    return fetchApi<BasketLatest>(this.config, '/perps/basket/latest');
  }

  async getPerpsMarketStats(): Promise<Result<ApiResponse<PerpsMarketStats>, PlethApiError>> {
    return fetchApi<PerpsMarketStats>(this.config, '/perps/market/stats');
  }

  async getPerpsRevealPayload(
    orderId: string,
    minPublishTime: number,
    maxPublishTime: number
  ): Promise<Result<ApiResponse<PerpsRevealPayload>, PlethApiError>> {
    const params = new URLSearchParams({
      minPublishTime: String(minPublishTime),
      maxPublishTime: String(maxPublishTime),
    });
    return fetchApi<PerpsRevealPayload>(
      this.config,
      `/perps/orders/${orderId}/reveal-payload?${params.toString()}`
    );
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
