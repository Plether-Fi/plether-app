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
  writeContractAsync: vi.fn(),
  invalidateQueries: vi.fn(),
}))

vi.mock('wagmi', () => ({
  usePublicClient: () => ({
    waitForTransactionReceipt: vi.fn(),
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
    version: 'perps-aa-arbitrum-sepolia-v1',
    chainId: 421614,
    entryPoint: '0x3333333333333333333333333333333333333333',
    entryPointVersion: '0.8' as const,
    pimlicoRpcUrl: '/api/perps/v1/aa/pimlico',
    smartAccountMode: 'simple' as const,
    smartAccountVersion: 'permissionless-simple-v0.8' as const,
    smartAccountIndex: '0',
    smartAccountFactory: '0x4444444444444444444444444444444444444444',
    usdc: '0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3',
    usdcSupportsEip3009: false,
    usdcEip712Name: null,
    usdcEip712Version: null,
    marginClearinghouse: '0x19c2f60f6312EAF9acDE4C2b04551a05cA9bE76e',
    cfdEngine: '0x6A25eA1015b5f032d8a2D95d57AEfcB99219bF0a',
    orderRouter: '0x04E3103752f623fBcDcD01f588590Af4c53E4c1E',
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
