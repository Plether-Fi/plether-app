import { act, renderHook, waitFor } from '@testing-library/react'
import type { ReactNode } from 'react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address } from 'viem'
import {
  PerpsIdentityProvider,
  type PerpsAccountAddressResolver,
} from './PerpsIdentityProvider'
import {
  PERMISSIONLESS_SIMPLE_ACCOUNT_V08_FACTORY,
  PERPS_ENTRY_POINT_V08,
} from './manifest'
import { usePerpsIdentity } from './usePerpsIdentity'

const ownerAddress =
  '0x1111111111111111111111111111111111111111' as Address
const accountAddress =
  '0x2222222222222222222222222222222222222222' as Address

function validManifest(): Record<string, unknown> {
  return {
    version: 'perps-aa-arbitrum-sepolia-v1',
    chainId: 421614,
    entryPoint: PERPS_ENTRY_POINT_V08,
    entryPointVersion: '0.8',
    pimlicoRpcUrl: '/api/perps/v1/aa/pimlico',
    smartAccountMode: 'simple',
    smartAccountVersion: 'permissionless-simple-v0.8',
    smartAccountIndex: '0',
    smartAccountFactory: PERMISSIONLESS_SIMPLE_ACCOUNT_V08_FACTORY,
    usdc: '0x9999999999999999999999999999999999999999',
    usdcSupportsEip3009: false,
    usdcEip712Name: null,
    usdcEip712Version: null,
    marginClearinghouse: '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
    cfdEngine: '0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
    orderRouter: '0xcccccccccccccccccccccccccccccccccccccccc',
    userOperationExplorerUrlTemplate:
      'https://explorer.example.com/user-op/{userOperationHash}',
    transactionExplorerUrlTemplate:
      'https://explorer.example.com/tx/{transactionHash}',
    testnetFaucet: null,
    sponsorshipEnabled: true,
  }
}

describe('PerpsIdentityProvider', () => {
  beforeEach(() => {
    globalThis.localStorage.clear()
  })

  it('blocks perps when the sponsorship manifest is not configured', () => {
    function wrapper({ children }: { children: ReactNode }) {
      return (
        <PerpsIdentityProvider
          ownerAddress={ownerAddress}
          chainId={421614}
        >
          {children}
        </PerpsIdentityProvider>
      )
    }

    const { result } = renderHook(() => usePerpsIdentity(), { wrapper })

    expect(result.current).toMatchObject({
      status: 'blocked',
      ownerAddress,
      accountAddress: undefined,
      isAaManifestConfigured: true,
      sponsorshipEnabled: false,
      error: { code: 'SPONSORSHIP_MANIFEST_REQUIRED' },
    })
  })

  it('fails closed when a manifest URL lacks a concrete account resolver', () => {
    function wrapper({ children }: { children: ReactNode }) {
      return (
        <PerpsIdentityProvider
          ownerAddress={ownerAddress}
          chainId={421614}
          manifestUrl="/perps-aa-manifest.json"
        >
          {children}
        </PerpsIdentityProvider>
      )
    }

    const { result } = renderHook(() => usePerpsIdentity(), { wrapper })

    expect(result.current).toMatchObject({
      status: 'blocked',
      ownerAddress,
      accountAddress: undefined,
      isAaManifestConfigured: true,
      sponsorshipEnabled: false,
      error: { code: 'ACCOUNT_RESOLVER_MISSING' },
    })
  })

  it('keeps a newly derived account blocked until identity selection is persisted', async () => {
    const fetchManifest = vi.fn(async () => new Response(
      JSON.stringify(validManifest()),
      {
        status: 200,
        headers: { 'Content-Type': 'application/json' },
      }
    ))
    const accountAddressResolver: PerpsAccountAddressResolver = vi.fn(
      async () => ({
        accountAddress,
        accountVersion: 'permissionless-simple-v0.8',
        accountIndex: '0',
        entryPoint: PERPS_ENTRY_POINT_V08,
        entryPointVersion: '0.8',
        factoryAddress: PERMISSIONLESS_SIMPLE_ACCOUNT_V08_FACTORY,
      })
    )

    function wrapper({ children }: { children: ReactNode }) {
      return (
        <PerpsIdentityProvider
          ownerAddress={ownerAddress}
          chainId={421614}
          manifestUrl="/perps-aa-manifest.json"
          accountAddressResolver={accountAddressResolver}
          fetch={fetchManifest}
        >
          {children}
        </PerpsIdentityProvider>
      )
    }

    const { result } = renderHook(() => usePerpsIdentity(), { wrapper })

    await waitFor(() => {
      expect(result.current.status).toBe('selection-required')
    })
    expect(result.current).toMatchObject({
      accountAddress: undefined,
      isAaManifestConfigured: true,
      sponsorshipEnabled: true,
      proposedIdentity: { accountAddress },
    })

    act(() => {
      expect(result.current.confirmIdentityAfterContinuityCheck()).toBe(true)
    })

    expect(result.current).toMatchObject({
      status: 'ready',
      ownerAddress,
      accountAddress,
      sponsorshipEnabled: true,
    })
  })
})
