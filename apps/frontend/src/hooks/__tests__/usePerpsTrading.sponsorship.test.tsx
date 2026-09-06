import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { renderHook } from '@testing-library/react'
import { type ReactNode } from 'react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const USER_OPERATION_HASH = `0x${'77'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'88'.repeat(32)}` as Hex

const mocks = vi.hoisted(() => ({
  executeSponsoredPerpsAction: vi.fn(),
  trackSponsoredOperationPreflightFailure: vi.fn(),
  writeContractAsync: vi.fn(),
  waitForTransactionReceipt: vi.fn(),
  invalidateQueries: vi.fn(),
}))

vi.mock('wagmi', () => ({
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
    usdc: '0xAbEe441b564DC084857468fA244AEE0A444B07DF',
    usdcSupportsEip3009: false,
    usdcEip712Name: null,
    usdcEip712Version: null,
    marginClearinghouse: '0x91c85540A1f64C9AEC2C801fcc927F037d619f17',
    cfdEngine: '0x2CEDc3f0059f0E9C1099bE96974f459E58c428d6',
    orderRouter: '0x2b9790AD11cE5fB1B91aC3415B08cD1Ec7D0cE0B',
    orderLifecycleBook: '0xca57215a3859462eb380ea40969762Ac89D99522',
    positionProtectionBook: '0x63973Eb0B5a862dfc95348D4d575FC55C9546F04',
    policyEvaluator: '0x611b34a98261D60f0aE8584F4Dd1fF09CF663466',
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
      chainId: 421614,
      isAaManifestConfigured: true,
      sponsorshipEnabled: true,
      manifest,
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

  it('funds the Trading Account with an exact owner-wallet USDC transfer', async () => {
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.fundTradingAccount(25_000_000n))
      .resolves.toBe(TRANSACTION_HASH)

    expect(mocks.writeContractAsync).toHaveBeenCalledWith(
      expect.objectContaining({
        account: OWNER,
        chainId: 421614,
        address: '0xAbEe441b564DC084857468fA244AEE0A444B07DF',
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

    await expect(result.current.depositMargin(0n)).rejects.toThrow(
      'Deposit amount must be greater than zero'
    )

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
