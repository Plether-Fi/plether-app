/**
 * Plether API Client
 *
 * Typed client for interacting with the Plether backend API.
 * Handles HTTP requests, error parsing, and WebSocket connections.
 */

import { Result } from 'better-result';
import { captureFrontendLog } from '../analytics/client';
import { isSepoliaDeployment } from '../utils/deployment';
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
  PerpsBasketCandlePage,
  PerpsBasketCurrentCandle,
  PerpsCandleIntervalSeconds,
  PerpsRevealPayload,
  PerpsMarketStats,
  VaultHistory,
  TestnetFaucetClaim,
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

export function isUpstreamApiError(error: unknown): boolean {
  if (typeof error !== 'object' || error === null) return false;

  const maybeError = error as { code?: unknown; status?: unknown };
  return (
    maybeError.code === 'RPC_ERROR' ||
    maybeError.code === 'NETWORK_ERROR' ||
    maybeError.status === 502 ||
    maybeError.status === 503 ||
    maybeError.status === 504
  );
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
export type ApiScope = 'spot' | 'perps';

export function apiScopeToApiPath(scope: ApiScope): string {
  return scope === 'perps' ? '/api/perps/v1' : '/api/spot/v1';
}

export function chainIdToApiPath(chainId: number): string {
  return apiScopeToApiPath(TESTNET_API_CHAIN_IDS.has(chainId) ? 'perps' : 'spot');
}

function parseDefaultChainId(value: string | undefined): number {
  const parsed = Number(value);
  return Number.isInteger(parsed) ? parsed : 1;
}

export function defaultApiBaseUrl(): string {
  if (DEV_API_URL) return DEV_API_URL;
  if (isSepoliaDeployment()) return apiScopeToApiPath('perps');
  return chainIdToApiPath(DEFAULT_API_CHAIN_ID);
}

export function defaultApiChainId(): number {
  if (isSepoliaDeployment()) return 421614;
  return DEFAULT_API_CHAIN_ID;
}

export function getConfiguredApiBaseUrl(chainId: number): string {
  if (DEV_API_URL) return DEV_API_URL;
  if (isSepoliaDeployment()) return apiScopeToApiPath('perps');
  return chainIdToApiPath(chainId);
}

export function getConfiguredApiWsUrl(chainId: number): string {
  return deriveWsUrl(getConfiguredApiBaseUrl(chainId));
}

export function getScopedApiBaseUrl(scope: ApiScope): string {
  if (DEV_API_URL) return DEV_API_URL;
  return apiScopeToApiPath(scope);
}

export function getScopedApiWsUrl(scope: ApiScope): string {
  return deriveWsUrl(getScopedApiBaseUrl(scope));
}

export function getConfiguredApiSource(): string {
  return DEV_API_URL ? 'VITE_API_URL' : 'active chain route';
}

export function getScopedApiSource(): string {
  return DEV_API_URL ? 'VITE_API_URL' : 'fixed product scope';
}

function getInitialBaseUrl(): string {
  return defaultApiBaseUrl();
}

const DEFAULT_CONFIG: Required<Omit<PlethApiConfig, 'onError'>> = {
  baseUrl: getInitialBaseUrl(),
  wsUrl: deriveWsUrl(getInitialBaseUrl()),
  timeout: 30000,
};

// The backend returns a durable submitted state instead of holding the request
// open while Arbitrum confirms it. Sixty seconds covers the bounded database
// and signer-lock stages. The backend stops at 60 seconds; five seconds of
// transport margin still keeps the browser below the ALB's 75-second timeout.
export const TESTNET_FAUCET_TIMEOUT_MS = 65_000;
export const TESTNET_FAUCET_TIMEOUT_MESSAGE =
  'The faucet is taking longer than expected. Your request may still complete. Wait a moment, then try again—retrying is safe.';
const NETWORK_ERROR_MESSAGE =
  'We could not reach Plether. Check your connection and try again.';

interface ApiRequestPolicy {
  operation?: string;
  timeoutMs?: number;
  timeoutMessage?: string;
}

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

function logApiFailure(
  apiError: PlethApiError,
  policy: ApiRequestPolicy,
  durationMs: number,
  timeoutMs: number,
  didTimeout = false
): void {
  captureFrontendLog('error', 'frontend api request failed', {
    component: 'api_client',
    operation: policy.operation ?? 'request',
    outcome: 'failure',
    error_category: didTimeout ? 'timeout' : apiError.code.toLowerCase(),
    http_status: apiError.status,
    duration_ms: durationMs,
    timeout_ms: timeoutMs,
    reason_code: didTimeout ? 'client_timeout' : undefined,
  });
}

function logApiSuccess(
  policy: ApiRequestPolicy,
  durationMs: number,
  timeoutMs: number
): void {
  if (!policy.operation) return;

  captureFrontendLog('info', 'frontend api request completed', {
    component: 'api_client',
    operation: policy.operation,
    outcome: 'success',
    duration_ms: durationMs,
    timeout_ms: timeoutMs,
  });
}

async function fetchApi<T>(
  config: PlethApiConfig,
  path: string,
  options?: RequestInit,
  policy: ApiRequestPolicy = {}
): Promise<Result<ApiResponse<T>, PlethApiError>> {
  const url = `${config.baseUrl}${path}`;
  const timeoutMs = policy.timeoutMs ?? config.timeout ?? DEFAULT_CONFIG.timeout;
  const startedAt = Date.now();
  const controller = new AbortController();
  const headers = new Headers(options?.headers);
  if (options?.body !== undefined && !headers.has('Content-Type')) {
    headers.set('Content-Type', 'application/json');
  }
  const timeoutReason = new DOMException('Request timed out', 'TimeoutError');
  const abortFromCaller = () => {
    controller.abort(options?.signal?.reason);
  };
  if (options?.signal?.aborted) {
    abortFromCaller();
  } else {
    options?.signal?.addEventListener('abort', abortFromCaller, { once: true });
  }
  const timeoutId = setTimeout(() => {
    controller.abort(timeoutReason);
  }, timeoutMs);

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
      headers,
    });

    if (!response.ok) {
      const apiError = await parseErrorResponse(response, url);
      if (controller.signal.aborted) {
        throw controller.signal.reason;
      }

      logApiFailure(apiError, policy, Date.now() - startedAt, timeoutMs);
      config.onError?.(apiError);
      return Result.err(apiError);
    }

    if (!isJsonResponse(response)) {
      const preview = await readResponsePreview(response);
      if (controller.signal.aborted) {
        throw controller.signal.reason;
      }
      const apiError = createNonJsonApiError(response, url, preview);

      logApiFailure(apiError, policy, Date.now() - startedAt, timeoutMs);
      config.onError?.(apiError);
      return Result.err(apiError);
    }

    const data = (await response.json()) as ApiResponse<T>;
    logApiSuccess(policy, Date.now() - startedAt, timeoutMs);
    return Result.ok(data);
  } catch (err) {
    const didTimeout = controller.signal.reason === timeoutReason;
    // Preserve caller-driven cancellation so TanStack Query can discard work
    // that is no longer observed instead of caching it as an API failure.
    if (options?.signal?.aborted && !didTimeout) {
      throw err;
    }

    const apiError = new PlethApiError(
      'NETWORK_ERROR',
      didTimeout
        ? policy.timeoutMessage ?? 'The request took too long. Please try again.'
        : NETWORK_ERROR_MESSAGE,
      undefined,
      err
    );

    logApiFailure(apiError, policy, Date.now() - startedAt, timeoutMs, didTimeout);
    config.onError?.(apiError);
    return Result.err(apiError);
  } finally {
    clearTimeout(timeoutId);
    options?.signal?.removeEventListener('abort', abortFromCaller);
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

  async claimTestnetFaucet(
    address: string
  ): Promise<Result<ApiResponse<TestnetFaucetClaim>, PlethApiError>> {
    return fetchApi<TestnetFaucetClaim>(this.config, '/testnet/faucet', {
      method: 'POST',
      body: JSON.stringify({ address, confirmationMode: 'async' }),
    }, {
      operation: 'claim_testnet_faucet',
      timeoutMs: TESTNET_FAUCET_TIMEOUT_MS,
      timeoutMessage: TESTNET_FAUCET_TIMEOUT_MESSAGE,
    });
  }

  // ===========================================================================
  // Perps Endpoints
  // ===========================================================================

  async getPerpsBasketHistory(
    range: BasketHistoryRange = '7d',
    intervalSeconds = 60 * 60,
    includeComponents = false,
    signal?: AbortSignal
  ): Promise<Result<ApiResponse<BasketHistory>, PlethApiError>> {
    const params = new URLSearchParams({
      range,
      interval: String(intervalSeconds),
    });
    if (includeComponents) params.set('includeComponents', 'true');

    return fetchApi<BasketHistory>(
      this.config,
      `/perps/basket/history?${params.toString()}`,
      { credentials: 'omit', signal }
    );
  }

  async getPerpsBasketLatest(signal?: AbortSignal): Promise<Result<ApiResponse<BasketLatest>, PlethApiError>> {
    return fetchApi<BasketLatest>(this.config, '/perps/basket/latest', {
      credentials: 'omit',
      signal,
    });
  }

  async getPerpsBasketCandles(
    intervalSeconds: PerpsCandleIntervalSeconds,
    cursor: number,
    signal?: AbortSignal,
    revalidate = false
  ): Promise<Result<ApiResponse<PerpsBasketCandlePage>, PlethApiError>> {
    const params = new URLSearchParams({
      interval: String(intervalSeconds),
      cursor: String(cursor),
    });
    return fetchApi<PerpsBasketCandlePage>(
      this.config,
      `/perps/basket/candles?${params.toString()}`,
      {
        credentials: 'omit',
        signal,
        cache: revalidate ? 'no-cache' : undefined,
      }
    );
  }

  async getPerpsBasketCurrentCandle(
    intervalSeconds: PerpsCandleIntervalSeconds,
    signal?: AbortSignal,
    revalidate = false
  ): Promise<Result<ApiResponse<PerpsBasketCurrentCandle>, PlethApiError>> {
    const params = new URLSearchParams({ interval: String(intervalSeconds) });
    return fetchApi<PerpsBasketCurrentCandle>(
      this.config,
      `/perps/basket/candles/current?${params.toString()}`,
      {
        credentials: 'omit',
        signal,
        cache: revalidate ? 'no-cache' : undefined,
      }
    );
  }

  async getPerpsMarketStats(signal?: AbortSignal): Promise<Result<ApiResponse<PerpsMarketStats>, PlethApiError>> {
    return fetchApi<PerpsMarketStats>(this.config, '/perps/market/stats', {
      credentials: 'omit',
      signal,
    });
  }

  async getPerpsVaultHistory(
    signal?: AbortSignal
  ): Promise<Result<ApiResponse<VaultHistory>, PlethApiError>> {
    const params = new URLSearchParams({
      range: '7d',
      interval: '3600',
    });
    return fetchApi<VaultHistory>(
      this.config,
      `/perps/vaults/history?${params.toString()}`,
      {
        credentials: 'omit',
        signal,
      }
    );
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
// Scoped Client Instances
// =============================================================================

export function createScopedApiClient(scope: ApiScope): PlethApiClient {
  const baseUrl = getScopedApiBaseUrl(scope);
  return new PlethApiClient({
    baseUrl,
    wsUrl: deriveWsUrl(baseUrl),
  });
}

export const spotApi = createScopedApiClient('spot');
export const perpsApi = createScopedApiClient('perps');
