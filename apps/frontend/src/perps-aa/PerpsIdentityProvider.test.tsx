import { act, renderHook, waitFor } from '@testing-library/react'
import type { ReactNode } from 'react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address } from 'viem'
import {
  PerpsIdentityProvider,
  type PerpsAccountAddressResolver,
} from './PerpsIdentityProvider'
import {
  PERMISSIONLESS_SIMPLE_ACCOUNT_V08_FACTORY,
  PERPS_ENTRY_POINT_V08,
} from './manifest'
import {
  createPersistedPerpsIdentity,
  readPersistedPerpsIdentity,
  writePersistedPerpsIdentity,
} from './identityPersistence'
import { usePerpsIdentity } from './usePerpsIdentity'

const ownerAddress =
  '0x1111111111111111111111111111111111111111' as Address
const accountAddress =
  '0x2222222222222222222222222222222222222222' as Address

function validManifest(): Record<string, unknown> {
  return {
    version: 'perps-aa-arbitrum-sepolia-v2',
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
    orderLifecycleBook: '0xdddddddddddddddddddddddddddddddddddddddd',
    policyEvaluator: '0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee',
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

  afterEach(() => {
    vi.useRealTimers()
    vi.restoreAllMocks()
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

  it('automatically persists a newly derived account', async () => {
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
      expect(result.current.status).toBe('ready')
    })
    expect(result.current).toMatchObject({
      accountAddress,
      isAaManifestConfigured: true,
      sponsorshipEnabled: true,
      proposedIdentity: null,
    })
    expect(
      readPersistedPerpsIdentity(
        globalThis.localStorage,
        421614,
        ownerAddress
      ).status
    ).toBe('found')
  })

  it('automatically replaces a valid identity from an earlier testnet deployment', async () => {
    const previousIdentity = createPersistedPerpsIdentity({
      chainId: 421614,
      ownerAddress,
      accountAddress,
      accountMode: 'simple',
      entryPoint: PERPS_ENTRY_POINT_V08,
      entryPointVersion: '0.8',
      factoryAddress: PERMISSIONLESS_SIMPLE_ACCOUNT_V08_FACTORY,
      accountVersion: 'permissionless-simple-v0.8',
      accountIndex: '0',
      manifestVersion: 'perps-aa-arbitrum-sepolia-20260826-v2',
    })
    expect(
      writePersistedPerpsIdentity(globalThis.localStorage, previousIdentity).ok
    ).toBe(true)

    const nextManifest = validManifest()
    nextManifest.version = 'perps-aa-arbitrum-sepolia-20260830-v2'
    const fetchManifest = vi.fn(async () => new Response(
      JSON.stringify(nextManifest),
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
      expect(result.current.status).toBe('ready')
    })
    expect(result.current.identity?.manifestVersion).toBe(
      'perps-aa-arbitrum-sepolia-20260830-v2'
    )
  })

  it('retains the verified identity while a background refresh is in flight', async () => {
    let resolveRefresh: ((response: Response) => void) | undefined
    const refreshResponse = new Promise<Response>((resolve) => {
      resolveRefresh = resolve
    })
    const manifestResponse = () => new Response(
      JSON.stringify(validManifest()),
      {
        status: 200,
        headers: { 'Content-Type': 'application/json' },
      }
    )
    const fetchManifest = vi.fn()
      .mockResolvedValueOnce(manifestResponse())
      .mockImplementationOnce(() => refreshResponse)
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
          refreshIntervalMs={false}
        >
          {children}
        </PerpsIdentityProvider>
      )
    }

    const { result } = renderHook(() => usePerpsIdentity(), { wrapper })

    await waitFor(() => {
      expect(result.current.status).toBe('ready')
    })

    act(() => {
      result.current.reloadIdentity()
    })
    await waitFor(() => {
      expect(fetchManifest).toHaveBeenCalledTimes(2)
    })

    expect(result.current).toMatchObject({
      status: 'ready',
      ownerAddress,
      accountAddress,
      sponsorshipEnabled: true,
    })

    act(() => {
      resolveRefresh?.(manifestResponse())
    })
    await waitFor(() => {
      expect(result.current.status).toBe('ready')
    })
  })

  it('refreshes every 30 seconds only while visible and revalidates on return', async () => {
    vi.useFakeTimers()
    let visibilityState: DocumentVisibilityState = 'visible'
    vi.spyOn(document, 'visibilityState', 'get').mockImplementation(
      () => visibilityState
    )

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
    await act(async () => {
      await vi.advanceTimersByTimeAsync(0)
    })
    expect(result.current.status).toBe('ready')
    expect(fetchManifest).toHaveBeenCalledTimes(1)

    await act(async () => {
      await vi.advanceTimersByTimeAsync(30_000)
    })
    expect(fetchManifest).toHaveBeenCalledTimes(2)

    visibilityState = 'hidden'
    act(() => {
      document.dispatchEvent(new Event('visibilitychange'))
    })
    await act(async () => {
      await vi.advanceTimersByTimeAsync(120_000)
    })
    expect(fetchManifest).toHaveBeenCalledTimes(2)

    visibilityState = 'visible'
    await act(async () => {
      document.dispatchEvent(new Event('visibilitychange'))
      await vi.advanceTimersByTimeAsync(0)
    })
    expect(fetchManifest).toHaveBeenCalledTimes(3)

    await act(async () => {
      await vi.advanceTimersByTimeAsync(29_999)
    })
    expect(fetchManifest).toHaveBeenCalledTimes(3)

    await act(async () => {
      await vi.advanceTimersByTimeAsync(1)
    })
    expect(fetchManifest).toHaveBeenCalledTimes(4)
  })
})
