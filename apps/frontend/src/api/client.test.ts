import { describe, expect, it } from 'vitest';

import {
  PlethApiError,
  apiScopeToApiPath,
  getScopedApiBaseUrl,
  isUpstreamApiError,
} from './client';

describe('apiScopeToApiPath', () => {
  it('pins spot to the spot API namespace', () => {
    expect(apiScopeToApiPath('spot')).toBe('/api/spot/v1');
  });

  it('pins perps to the perps API namespace', () => {
    expect(apiScopeToApiPath('perps')).toBe('/api/perps/v1');
  });
});

describe('getScopedApiBaseUrl', () => {
  it('keeps spot on mainnet even when a testnet chain would route elsewhere', () => {
    const expectedSpotBaseUrl = import.meta.env.VITE_API_URL as string | undefined ?? '/api/spot/v1';

    expect(apiScopeToApiPath('spot')).toBe('/api/spot/v1');
    expect(getScopedApiBaseUrl('spot')).toBe(expectedSpotBaseUrl);
  });

  it('keeps perps on the testnet backend scope', () => {
    const expectedPerpsBaseUrl = import.meta.env.VITE_API_URL as string | undefined ?? '/api/perps/v1';

    expect(apiScopeToApiPath('perps')).toBe('/api/perps/v1');
    expect(getScopedApiBaseUrl('perps')).toBe(expectedPerpsBaseUrl);
  });
});

describe('isUpstreamApiError', () => {
  it('matches RPC and network API errors', () => {
    expect(isUpstreamApiError(new PlethApiError('RPC_ERROR', 'execution reverted', 400))).toBe(true);
    expect(isUpstreamApiError(new PlethApiError('NETWORK_ERROR', 'node unreachable'))).toBe(true);
  });

  it('matches service-unavailable HTTP statuses', () => {
    expect(isUpstreamApiError(new PlethApiError('INTERNAL_ERROR', 'bad gateway', 502))).toBe(true);
    expect(isUpstreamApiError(new PlethApiError('INTERNAL_ERROR', 'unavailable', 503))).toBe(true);
    expect(isUpstreamApiError(new PlethApiError('INTERNAL_ERROR', 'timeout', 504))).toBe(true);
  });

  it('does not match user input errors', () => {
    expect(isUpstreamApiError(new PlethApiError('INVALID_ADDRESS', 'invalid address', 400))).toBe(false);
    expect(isUpstreamApiError(new PlethApiError('INVALID_AMOUNT', 'invalid amount', 400))).toBe(false);
  });
});
