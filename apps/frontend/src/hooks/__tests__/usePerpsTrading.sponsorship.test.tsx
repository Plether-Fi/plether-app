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
    usdc: '0xc3CE8590B7EcDE7454f9D5b51a797bbDe96fe56B',
    usdcSupportsEip3009: false,
    usdcEip712Name: null,
    usdcEip712Version: null,
    marginClearinghouse: '0xA863F985EedA8BF5BE2320693BB93d109EBB2dBd',
    cfdEngine: '0x9611E643aC4691E8fDeD8a0c2C22c56438B6f352',
    orderRouter: '0xbd2f286efca5F761E21452673ab9b8C14e17aad7',
    orderLifecycleBook: '0x616aD381Df40047e9b060a1E85085B3Ed2CC6D3C',
    policyEvaluator: '0x1ed622ed2Cbd64bd36115dB9D4f4c0006b5894fB',
    positionProtectionBook: '0x35f495fFDbB4d6ae395691D4632629f67603C926',
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
        address: '0xc3CE8590B7EcDE7454f9D5b51a797bbDe96fe56B',
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
