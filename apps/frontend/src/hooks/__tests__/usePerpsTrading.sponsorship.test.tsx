import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { renderHook } from '@testing-library/react'
import { type ReactNode } from 'react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { PERPS_DEFAULT_SEPOLIA_DEPLOYMENT } from '../../contracts/perpsAddresses'
import type { Address, Hex } from 'viem'

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const USER_OPERATION_HASH = `0x${'77'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'88'.repeat(32)}` as Hex

const mocks = vi.hoisted(() => ({
  manifestOverrides: {} as Record<string, unknown>,
  runtimeOverrides: {} as Record<string, unknown>,
  executeSponsoredPerpsAction: vi.fn(),
  trackSponsoredOperationPreflightFailure: vi.fn(),
  writeContractAsync: vi.fn(),
  waitForTransactionReceipt: vi.fn(),
  invalidateQueries: vi.fn(),
}))

vi.mock('wagmi', () => ({
  useWalletClient: () => ({ data: undefined }),
  usePublicClient: () => ({
    waitForTransactionReceipt: mocks.waitForTransactionReceipt,
  }),
  useWriteContract: () => ({
    writeContractAsync: mocks.writeContractAsync,
  }),
  useSignTypedData: () => ({
    signTypedDataAsync: vi.fn(),
  }),
}))

vi.mock('../../perps-aa', async (importOriginal) => {
  const actual = await importOriginal<typeof import('../../perps-aa')>()
  const manifest = {
    version: 'perps-aa-arbitrum-sepolia-v2',
    chainId: 421614,
    entryPoint: '0x3333333333333333333333333333333333333333',
    entryPointVersion: '0.8' as const,
    pimlicoRpcUrl: '/api/perps/v1/aa/pimlico',
    smartAccountMode: 'simple' as const,
    smartAccountVersion: 'permissionless-simple-v0.8' as const,
    smartAccountIndex: '0',
    smartAccountFactory: '0x4444444444444444444444444444444444444444',
    usdc: '0xf7cbfcc74f2d9eb6fa7dc11941b3bef9fd7f8eb8',
    usdcSupportsEip3009: false,
    usdcEip712Name: null,
    usdcEip712Version: null,
    marginClearinghouse: '0xfa6e677ec1062757c1194d411a5e61e1e9644499',
    cfdEngine: '0xafece93321be41aa73474457e2f47cf7b2fb738f',
    orderRouter: '0x6215d36fcbd610ca1525252eebcbfd8b223a6072',
    orderLifecycleBook: '0x753eb48305ffb88bb70869ade2c4efa941879221',
    policyEvaluator: '0x43c93d3028fcd4c1f578a50639750b8fbfdee799',
    positionProtectionBook: '0x3204c51cd567d6490c011399ccbaaf67b5d3d768',
    userOperationExplorerUrlTemplate:
      'https://example.com/user-operation/{userOperationHash}',
    transactionExplorerUrlTemplate:
      'https://example.com/transaction/{transactionHash}',
    testnetFaucet: null,
    sponsorshipEnabled: true,
  }
  return {
    ...actual,
    executeSponsoredPerpsAction: mocks.executeSponsoredPerpsAction,
    trackSponsoredOperationPreflightFailure:
      mocks.trackSponsoredOperationPreflightFailure,
    usePerpsIdentity: () => ({
      status: 'ready',
      ownerAddress: '0x1111111111111111111111111111111111111111',
      accountAddress: '0x2222222222222222222222222222222222222222',
      chainId: Number(mocks.manifestOverrides.chainId ?? 421614),
      isAaManifestConfigured: true,
      sponsorshipEnabled: true,
      manifest: { ...manifest, ...mocks.manifestOverrides },
      identity: null,
      proposedIdentity: null,
      changedIdentityFields: [],
      error: null,
      confirmIdentityAfterContinuityCheck: () => false,
      reloadIdentity: () => undefined,
    }),
    usePerpsAaRuntime: () => ({
      chainId: 421614,
      ownerAddress: OWNER,
      factoryAddress: '0x4444444444444444444444444444444444444444',
      accountVersion: 'permissionless-simple-v0.8',
      accountIndex: '0',
      smartAccount: {
        accountAddress: '0x2222222222222222222222222222222222222222',
        entryPoint: '0x3333333333333333333333333333333333333333',
      },
      ...mocks.runtimeOverrides,
    }),
  }
})

import { usePerpsTrading } from '../usePerpsTrading'

function wrapper({ children }: { children: ReactNode }) {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  })
  queryClient.invalidateQueries = mocks.invalidateQueries
  return (
    <QueryClientProvider client={queryClient}>
      {children}
    </QueryClientProvider>
  )
}

describe('usePerpsTrading sponsorship route', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mocks.manifestOverrides = {}
    mocks.runtimeOverrides = {}
    mocks.executeSponsoredPerpsAction.mockResolvedValue({
      userOperationHash: USER_OPERATION_HASH,
      transactionHash: TRANSACTION_HASH,
    })
    mocks.writeContractAsync.mockResolvedValue(TRANSACTION_HASH)
    mocks.waitForTransactionReceipt.mockResolvedValue({
      status: 'success',
      transactionHash: TRANSACTION_HASH,
    })
  })

  afterEach(() => {
    vi.unstubAllEnvs()
    vi.restoreAllMocks()
    vi.resetModules()
  })

  it('uses the active mainnet release for funding and excludes Sepolia close assistance', async () => {
    vi.stubEnv('VITE_PERPS_DEPLOYMENT_JSON', JSON.stringify({
      ...PERPS_DEFAULT_SEPOLIA_DEPLOYMENT,
      chainId: 42161,
      closePreview: { ...PERPS_DEFAULT_SEPOLIA_DEPLOYMENT.closePreview, chainId: 42161 },
    }))
    mocks.manifestOverrides = { chainId: 42161 }
    mocks.runtimeOverrides = { chainId: 42161 }
    vi.resetModules()
    const { usePerpsTrading: useActivePerpsTrading } = await import('../usePerpsTrading')
    const { result } = renderHook(() => useActivePerpsTrading(), { wrapper })
    await expect(result.current.fundTradingAccount(25_000_000n)).resolves.toBe(TRANSACTION_HASH)
    expect(mocks.writeContractAsync).toHaveBeenCalledWith(expect.objectContaining({
      chainId: 42161, address: PERPS_DEFAULT_SEPOLIA_DEPLOYMENT.contracts.usdc,
      args: [ACCOUNT, 25_000_000n],
    }))
    const prepareOrder = vi.spyOn(await import('../../contracts/preparePerpsOrderV2'), 'preparePerpsOrderV2')
      .mockResolvedValue({} as never)
    const closeAssistance = vi.spyOn(await import('../../perps-aa/sponsoredClose'), 'loadCloseAssistanceConfig')
    const close = { direction: 'short' as const, notionalUsdc: 100_000_000n, sizeDelta: 100n * 10n ** 18n,
      marginUsdc: 0n, oraclePrice: 100_000_000n, slippagePercent: 1, isClose: true, selectedMaxLeverageBps: 20_000 }
    await result.current.prepareOrder(close)
    expect(prepareOrder).toHaveBeenCalledWith(expect.anything(), expect.objectContaining({ chainId: 42161 }),
      expect.objectContaining({ closeAssistance: undefined }))
    expect(closeAssistance).not.toHaveBeenCalled()
    await expect(result.current.commitOrder({ ...close, preparedOrder: {
      account: ACCOUNT, request: { side: 1, sizeDelta: close.sizeDelta, isClose: true, marginDelta: 0n },
      sponsoredClose: { amountUsdc: 100_000n },
    } as never })).rejects.toThrow('Close assistance is only available on Arbitrum Sepolia')
    expect(mocks.executeSponsoredPerpsAction).not.toHaveBeenCalled()
  })

  it.each(['usdc', 'marginClearinghouse', 'cfdEngine', 'orderRouter', 'orderLifecycleBook', 'policyEvaluator', 'positionProtectionBook'])
    ('rejects a mismatched %s before funding or requesting sponsorship', async key => {
      mocks.manifestOverrides = { [key]: '0x9999999999999999999999999999999999999999' }
      const { result } = renderHook(() => usePerpsTrading(), { wrapper })
      await expect(result.current.fundTradingAccount(25_000_000n)).rejects.toMatchObject({
        cause: expect.objectContaining({ reason: 'MANIFEST_MISMATCH' }),
      })
      await expect(result.current.depositMargin(25_000_000n, 0n)).rejects.toMatchObject({
        cause: expect.objectContaining({ reason: 'MANIFEST_MISMATCH' }),
      })
      expect(mocks.writeContractAsync).not.toHaveBeenCalled()
      expect(mocks.executeSponsoredPerpsAction).not.toHaveBeenCalled()
    })

  it('rejects a manifest on a different chain even when all contract addresses match', async () => {
    mocks.manifestOverrides = { chainId: 42161 }
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })
    await expect(result.current.fundTradingAccount(25_000_000n)).rejects.toMatchObject({
      cause: expect.objectContaining({ reason: 'MANIFEST_MISMATCH' }),
    })
    expect(mocks.writeContractAsync).not.toHaveBeenCalled()
  })

  it.each([
    { chainId: 42161 },
    { ownerAddress: '0x9999999999999999999999999999999999999999' },
    { factoryAddress: '0x9999999999999999999999999999999999999999' },
    { accountVersion: 'unreviewed' },
    { accountIndex: '1' },
    { smartAccount: { accountAddress: '0x9999999999999999999999999999999999999999', entryPoint: '0x3333333333333333333333333333333333333333' } },
    { smartAccount: { accountAddress: ACCOUNT, entryPoint: '0x9999999999999999999999999999999999999999' } },
  ])('rejects stale runtime identity before any wallet funding write: %j', async runtimeOverrides => {
    mocks.runtimeOverrides = runtimeOverrides
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })
    await expect(result.current.fundTradingAccount(25_000_000n)).rejects.toMatchObject({
      cause: expect.objectContaining({ reason: 'ACCOUNT_NOT_TRUSTED' }),
    })
    expect(mocks.writeContractAsync).not.toHaveBeenCalled()
    expect(mocks.executeSponsoredPerpsAction).not.toHaveBeenCalled()
  })

  it('funds the Trading Account with an exact owner-wallet USDC transfer', async () => {
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.fundTradingAccount(25_000_000n))
      .resolves.toBe(TRANSACTION_HASH)

    expect(mocks.writeContractAsync).toHaveBeenCalledWith(
      expect.objectContaining({
        account: OWNER,
        chainId: 421614,
        address: '0xf7cbfcc74f2d9eb6fa7dc11941b3bef9fd7f8eb8',
        functionName: 'transfer',
        args: [ACCOUNT, 25_000_000n],
      })
    )
    expect(mocks.waitForTransactionReceipt).toHaveBeenCalledWith({
      hash: TRANSACTION_HASH,
    })
    expect(mocks.executeSponsoredPerpsAction).not.toHaveBeenCalled()
  })

  it('builds an atomic Trading Account balance deposit without a direct EOA write', async () => {
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.depositMargin(25_000_000n, 0n))
      .resolves.toBe(TRANSACTION_HASH)

    expect(mocks.executeSponsoredPerpsAction).toHaveBeenCalledWith(
      expect.objectContaining({
        ownerAddress: OWNER,
        action: expect.objectContaining({
          kind: 'deposit',
          account: ACCOUNT,
          calls: expect.arrayContaining([
            expect.objectContaining({ value: 0n }),
            expect.objectContaining({ value: 0n }),
          ]),
        }),
      })
    )
    expect(mocks.writeContractAsync).not.toHaveBeenCalled()
  })

  it('tracks invalid deposits as explicit preflight failures', async () => {
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.depositMargin(0n)).rejects.toMatchObject({
      message: expect.stringContaining('Enter an amount greater than zero and within the available balance.'),
      cause: expect.objectContaining({ reason: 'INVALID_AMOUNT' }),
    })

    expect(mocks.executeSponsoredPerpsAction).not.toHaveBeenCalled()
    expect(
      mocks.trackSponsoredOperationPreflightFailure
    ).toHaveBeenCalledWith(
      expect.objectContaining({ action: 'deposit' }),
      expect.objectContaining({ reason: 'INVALID_AMOUNT' })
    )
  })

  it('withdraws from the Simple Trading Account to its verified Owner Wallet', async () => {
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.withdrawMargin(10_000_000n))
      .resolves.toBe(TRANSACTION_HASH)

    expect(mocks.executeSponsoredPerpsAction).toHaveBeenCalledWith(
      expect.objectContaining({
        action: expect.objectContaining({
          kind: 'withdraw-to-owner',
          account: ACCOUNT,
          calls: expect.arrayContaining([
            expect.objectContaining({ value: 0n }),
            expect.objectContaining({ value: 0n }),
          ]),
        }),
      })
    )
    expect(mocks.writeContractAsync).not.toHaveBeenCalled()
  })

  it('never exposes direct payable order finalization for a sponsored account', async () => {
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.executeOrder(1n)).rejects.toThrow(
      'keeper-operated'
    )
    expect(mocks.executeSponsoredPerpsAction).not.toHaveBeenCalled()
    expect(mocks.writeContractAsync).not.toHaveBeenCalled()
  })
})
