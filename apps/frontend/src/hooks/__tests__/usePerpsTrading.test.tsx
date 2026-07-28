import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { renderHook } from '@testing-library/react'
import { type ReactNode } from 'react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'
import {
  PERPS_ARBITRUM_SEPOLIA,
  PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
} from '../../contracts/perpsAddresses'
import { usePerpsTrading } from '../usePerpsTrading'

const OWNER = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B' as Address
const ACCOUNT = '0x9314586D4068C73B23a64d7406Ca8FfEeCc2cBFc' as Address
const USER_OPERATION_HASH = `0x${'77'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'88'.repeat(32)}` as Hex

const mocks = vi.hoisted(() => ({
  identityReady: false,
  getBlock: vi.fn(),
  readContract: vi.fn(),
  simulateContract: vi.fn(),
  waitForTransactionReceipt: vi.fn(),
  writeContractAsync: vi.fn(),
  signTypedDataAsync: vi.fn(),
  executeSponsoredPerpsAction: vi.fn(),
  invalidateQueries: vi.fn(),
  parseEventLogs: vi.fn(),
}))

vi.mock('viem', async (importOriginal) => {
  const actual = await importOriginal<typeof import('viem')>()
  return {
    ...actual,
    parseEventLogs: mocks.parseEventLogs,
  }
})

vi.mock('wagmi', () => ({
  usePublicClient: () => ({
    getBlock: mocks.getBlock,
    readContract: mocks.readContract,
    simulateContract: mocks.simulateContract,
    waitForTransactionReceipt: mocks.waitForTransactionReceipt,
  }),
  useWriteContract: () => ({
    writeContractAsync: mocks.writeContractAsync,
  }),
  useSignTypedData: () => ({
    signTypedDataAsync: mocks.signTypedDataAsync,
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
    usePerpsAaRuntime: () => mocks.identityReady
      ? {
          chainId: 421614,
          ownerAddress: OWNER,
          factoryAddress: '0x4444444444444444444444444444444444444444',
          accountVersion: 'permissionless-simple-v0.8',
          accountIndex: '0',
          smartAccount: {
            accountAddress: ACCOUNT,
            entryPoint: '0x3333333333333333333333333333333333333333',
          },
        }
      : undefined,
    usePerpsIdentity: () => mocks.identityReady
      ? {
          status: 'ready',
          ownerAddress: OWNER,
          accountAddress: ACCOUNT,
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
        }
      : {
          status: 'blocked',
          ownerAddress: OWNER,
          accountAddress: undefined,
          chainId: 421614,
          isAaManifestConfigured: true,
          sponsorshipEnabled: false,
          manifest: null,
          identity: null,
          proposedIdentity: null,
          changedIdentityFields: [],
          error: {
            code: 'SPONSORSHIP_MANIFEST_REQUIRED',
            message: 'Perps is sponsorship-only on testnet.',
          },
          confirmIdentityAfterContinuityCheck: () => false,
          reloadIdentity: () => undefined,
        },
  }
})

function wrapper({ children }: { children: ReactNode }) {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
    },
  })
  queryClient.invalidateQueries = mocks.invalidateQueries

  return (
    <QueryClientProvider client={queryClient}>
      {children}
    </QueryClientProvider>
  )
}

function sponsoredResult() {
  return {
    userOperationHash: USER_OPERATION_HASH,
    transactionHash: TRANSACTION_HASH,
    receipt: {
      success: true,
      receipt: {
        transactionHash: TRANSACTION_HASH,
        logs: [],
      },
    },
  }
}

describe('usePerpsTrading', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mocks.identityReady = false
    mocks.getBlock.mockResolvedValue({ timestamp: 1_700_000_000n })
    mocks.simulateContract.mockResolvedValue({})
    mocks.executeSponsoredPerpsAction.mockResolvedValue(sponsoredResult())
    mocks.parseEventLogs.mockReturnValue([{ args: { orderId: 42n } }])
    mocks.readContract.mockImplementation(({ functionName }: { functionName: string }) => {
      switch (functionName) {
        case 'getPendingOrders':
          return []
        case 'maxPendingOrders':
          return 10n
        case 'getTraderAccount':
          return {
            equityUsdc: 900_000_000n,
            withdrawableUsdc: 500_000_000n,
            pendingOrderMarginUsdc: 250_000_000n,
            pendingExecutionBountyUsdc: 10_000n,
          }
        case 'previewOpenRevertCode':
          return 0
        case 'previewClose':
          return { valid: true, invalidReason: 0 }
        default:
          throw new Error(`Unexpected readContract call: ${functionName}`)
      }
    })
  })

  it('never exposes a direct owner-wallet approval path', async () => {
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.approveUsdcForMargin(25_000_000n))
      .rejects.toThrow('Direct USDC approvals are disabled')
  })

  it('blocks order submission until the Trading Account is ready', async () => {
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.commitOrder({
      direction: 'long',
      notionalUsdc: 1_000_000_000n,
      sizeDelta: 1_000_000_000_000_000_000n,
      marginUsdc: 200_000_000n,
      oraclePrice: 98_300_000n,
      slippagePercent: 0.1,
      isClose: false,
    })).rejects.toThrow(
      'Confirm the Plether Trading Account before committing an order'
    )

    expect(mocks.simulateContract).not.toHaveBeenCalled()
    expect(mocks.executeSponsoredPerpsAction).not.toHaveBeenCalled()
  })

  it('keeps manual finalization and cleanup keeper-only', async () => {
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.executeOrder(42n))
      .rejects.toThrow('Order finalization is keeper-operated')
    await expect(result.current.cleanupExpiredOrder(42n))
      .rejects.toThrow('Expired-order cleanup is keeper-operated')
  })

  it('signals the wallet request from the managed sponsored operation', async () => {
    mocks.identityReady = true
    const onWalletRequestStart = vi.fn()
    mocks.executeSponsoredPerpsAction.mockImplementationOnce(async (input) => {
      input.onStatus?.('awaiting-signature')
      return sponsoredResult()
    })

    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.commitOrder({
      direction: 'long',
      notionalUsdc: 1_000_000_000n,
      sizeDelta: 1_000_000_000_000_000_000n,
      marginUsdc: 200_000_000n,
      oraclePrice: 98_300_000n,
      slippagePercent: 0.1,
      isClose: false,
      onWalletRequestStart,
    })).resolves.toEqual({
      hash: TRANSACTION_HASH,
      userOperationHash: USER_OPERATION_HASH,
      orderId: 42n,
    })

    expect(onWalletRequestStart).toHaveBeenCalledTimes(1)
    expect(mocks.executeSponsoredPerpsAction).toHaveBeenCalledWith(
      expect.objectContaining({
        ownerAddress: OWNER,
        action: expect.objectContaining({
          kind: 'place-order',
          account: ACCOUNT,
        }),
      })
    )
  })

  it('shows commit diagnostics when an undecoded pre-submission simulation fails', async () => {
    mocks.identityReady = true
    mocks.simulateContract.mockRejectedValueOnce(new Error('Transaction failed'))

    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.commitOrder({
      direction: 'long',
      notionalUsdc: 1_000_000_000n,
      sizeDelta: 1_000_000_000_000_000_000n,
      marginUsdc: 200_000_000n,
      oraclePrice: 98_300_000n,
      slippagePercent: 0.1,
      isClose: false,
    })).rejects.toThrow([
      'Commit was not submitted, or the wallet/RPC did not return a transaction hash. No order was created.',
      'No transaction hash was returned by the wallet/RPC, so no mined transaction could be checked.',
      'Current account state: 0/10 pending orders, equity 900 USDC, free/withdrawable 500 USDC, pending margin 250 USDC, pending bounty 0.01 USDC.',
      'Latest open preview still passes.',
      'A fresh commit simulation still passes, so this looks like a wallet/RPC submission failure rather than a contract rejection. Retry the commit; if your wallet still shows a pending request, reject it first or reconnect the wallet.',
    ].join('\n'))

    expect(mocks.executeSponsoredPerpsAction).not.toHaveBeenCalled()
  })

  it('shows the transaction hash and diagnostics when sponsored submission fails opaquely', async () => {
    mocks.identityReady = true
    const failedHash = `0x${'11'.repeat(32)}` as Hex
    mocks.executeSponsoredPerpsAction.mockRejectedValueOnce(
      new Error('Transaction failed', {
        cause: new Error(`Failed transaction: ${failedHash}`),
      })
    )

    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.commitOrder({
      direction: 'long',
      notionalUsdc: 1_000_000_000n,
      sizeDelta: 1_000_000_000_000_000_000n,
      marginUsdc: 200_000_000n,
      oraclePrice: 98_300_000n,
      slippagePercent: 0.1,
      isClose: false,
    })).rejects.toThrow([
      'Commit failed before an order was created, and the RPC did not return a decodable contract error.',
      `Failed tx: ${failedHash}`,
      'Current account state: 0/10 pending orders, equity 900 USDC, free/withdrawable 500 USDC, pending margin 250 USDC, pending bounty 0.01 USDC.',
      'Latest open preview still passes.',
      'A fresh commit simulation still passes, so the mined revert likely came from state changing between simulation and confirmation or from RPC-hidden revert data.',
    ].join('\n'))
  })

  it('adds isolated margin through the Trading Account and invalidates only perps reads', async () => {
    mocks.identityReady = true

    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.addPositionMargin(250_000_000n))
      .resolves.toBe(TRANSACTION_HASH)

    expect(mocks.executeSponsoredPerpsAction).toHaveBeenCalledWith(
      expect.objectContaining({
        ownerAddress: OWNER,
        action: expect.objectContaining({
          kind: 'add-margin',
          account: ACCOUNT,
          calls: [
            expect.objectContaining({
              to: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
              value: 0n,
            }),
          ],
        }),
      })
    )
    expect(mocks.invalidateQueries).toHaveBeenCalled()
    const invalidateOptions = mocks.invalidateQueries.mock.calls[0][0] as {
      predicate: (query: { queryKey: readonly unknown[] }) => boolean
    }
    expect(invalidateOptions.predicate({
      queryKey: ['readContracts', {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        contracts: [{ address: PERPS_ARBITRUM_SEPOLIA.cfdEngine }],
      }],
    })).toBe(true)
    expect(invalidateOptions.predicate({
      queryKey: ['readContract', {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: '0x0000000000000000000000000000000000000001',
      }],
    })).toBe(false)
    expect(invalidateOptions.predicate({
      queryKey: ['readContract', {
        chainId: 11155111,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
      }],
    })).toBe(false)
    expect(invalidateOptions.predicate({
      queryKey: ['protocol', 'status'],
    })).toBe(false)
  })
})
