import { describe, expect, it } from 'vitest';

import { chainIdToApiPath } from './client';

describe('chainIdToApiPath', () => {
  it('routes Sepolia testnets to the Sepolia backend binding', () => {
    expect(chainIdToApiPath(11155111)).toBe('/api/sepolia_v1');
    expect(chainIdToApiPath(421614)).toBe('/api/sepolia_v1');
  });

  it('routes mainnet chains to the mainnet backend binding', () => {
    expect(chainIdToApiPath(1)).toBe('/api/v1');
    expect(chainIdToApiPath(42161)).toBe('/api/v1');
  });
});
