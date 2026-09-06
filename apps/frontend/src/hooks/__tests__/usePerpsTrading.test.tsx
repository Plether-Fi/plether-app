import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { renderHook } from '@testing-library/react'
import { type ReactNode } from 'react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'
import {
  PERPS_ARBITRUM_SEPOLIA,
  PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
} from '../../contracts/perpsAddresses'
import type { PreparedPerpsOrderV2 } from '../../contracts/perpsOrderV2'
import { usePerpsTrading } from '../usePerpsTrading'

const OWNER = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B' as Address
const ACCOUNT = '0x9314586D4068C73B23a64d7406Ca8FfEeCc2cBFc' as Address
const USER_OPERATION_HASH = `0x${'77'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'88'.repeat(32)}` as Hex
const CLIENT_ORDER_ID = `0x${'12'.repeat(32)}` as Hex
const CONFIG_HASH = `0x${'34'.repeat(32)}` as Hex
const REVIEWED_BLOCK_HASH = `0x${'56'.repeat(32)}` as Hex
const ORDER_LIFECYCLE_BOOK = '0x1111111111111111111111111111111111111111' as Address

const mocks = vi.hoisted(() => ({
  identityReady: false,
  intentResolution: 0,
  resolvedOrderId: 0n,
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
    orderLifecycleBook: '0x1111111111111111111111111111111111111111',
    policyEvaluator: '0x2222222222222222222222222222222222222222',
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
      logs: [{
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
      }, {
        address: ORDER_LIFECYCLE_BOOK,
      }],
      receipt: {
        transactionHash: TRANSACTION_HASH,
        logs: [],
        status: 'success',
      },
    },
  }
}

function preparedOrder(): PreparedPerpsOrderV2 {
  const request = {
    clientOrderId: CLIENT_ORDER_ID,
    side: 0,
    sizeDelta: 100_000_000_000_000_000_000n,
    marginDelta: 200_000_000n,
    targetPrice: 98_398_300n,
    isClose: false,
    bounds: {
      validUntil: 1_700_000_300n,
      allowedExecutionModes: 1,
      expectedConfigHash: CONFIG_HASH,
      maxExecutionBountyUsdc: 10_000n,
      maxExecutionNotionalUsdc: 1_000_000_000n,
      maxGrossAccountDebitUsdc: 200_010_000n,
      maxActionChargeUsdc: 2_000_000n,
      maxExplicitFeesUsdc: 2_000_000n,
      maxPostPositionSize: 100_000_000_000_000_000_000n,
      minPostSettlementBalanceUsdc: 700_000_000n,
      minPostPositionEquityUsdc: 198_000_000n,
      maxPostLeverageBps: 50_000,
    },
  } as const
  return {
    account: ACCOUNT,
    manifestVersion: 'perps-aa-arbitrum-sepolia-v2',
    orderRouter: PERPS_ARBITRUM_SEPOLIA.orderRouter,
    orderLifecycleBook: ORDER_LIFECYCLE_BOOK,
    request,
    executionBountyUsdc: request.bounds.maxExecutionBountyUsdc,
    reviewedBlockNumber: 123n,
    reviewedBlockHash: REVIEWED_BLOCK_HASH,
    reviewedPrice: 98_300_000n,
    protection: {
      validUntil: request.bounds.validUntil,
      executionMode: 1,
      executionBountyUsdc: request.bounds.maxExecutionBountyUsdc,
    },
  }
}

function commitInput() {
  return {
    direction: 'long' as const,
    notionalUsdc: 1_000_000_000n,
    sizeDelta: 100_000_000_000_000_000_000n,
    marginUsdc: 200_000_000n,
    oraclePrice: 98_300_000n,
    slippagePercent: 0.1,
    isClose: false,
    selectedMaxLeverageBps: 50_000,
    preparedOrder: preparedOrder(),
  }
}

describe('usePerpsTrading', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mocks.identityReady = false
    mocks.intentResolution = 0
    mocks.resolvedOrderId = 0n
    mocks.getBlock.mockResolvedValue({ timestamp: 1_700_000_000n })
    mocks.simulateContract.mockResolvedValue({})
    mocks.executeSponsoredPerpsAction.mockResolvedValue(sponsoredResult())
    mocks.parseEventLogs.mockImplementation(({ eventName }: { eventName: string }) =>
      eventName === 'IntentRegistered'
        ? [{ args: { account: ACCOUNT, clientOrderId: CLIENT_ORDER_ID, orderId: 42n } }]
        : [{ args: { account: ACCOUNT, orderId: 42n } }]
    )
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
        case 'resolveClientIntent':
          return [mocks.intentResolution, mocks.resolvedOrderId, CONFIG_HASH]
        case 'clientIntent':
          return {
            orderId: mocks.resolvedOrderId,
            requestHash: CONFIG_HASH,
          }
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

    await expect(result.current.commitOrder(commitInput())).rejects.toThrow(
      'Confirm the Plether Trading Account before committing an order'
    )

    expect(mocks.simulateContract).not.toHaveBeenCalled()
    expect(mocks.executeSponsoredPerpsAction).not.toHaveBeenCalled()
  })

  it('rejects an unaligned close before simulation or signing', async () => {
    mocks.identityReady = true
    const unalignedSizeDelta = 100_000_000_000_000_000_001n
    const basePreparedOrder = preparedOrder()
    const closePreparedOrder: PreparedPerpsOrderV2 = {
      ...basePreparedOrder,
      request: {
        ...basePreparedOrder.request,
        sizeDelta: unalignedSizeDelta,
        marginDelta: 0n,
        isClose: true,
      },
    }

    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.commitOrder({
      ...commitInput(),
      sizeDelta: unalignedSizeDelta,
      marginUsdc: 0n,
      isClose: true,
      preparedOrder: closePreparedOrder,
    })).rejects.toThrow('Order size must use 100 plDXY increments')

    expect(mocks.simulateContract).not.toHaveBeenCalled()
    expect(mocks.executeSponsoredPerpsAction).not.toHaveBeenCalled()
  })

  it('rejects an unaligned explicit close while preparing its protections', async () => {
    mocks.identityReady = true
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.prepareOrder({
      direction: 'long',
      notionalUsdc: 1_000_000_000n,
      sizeDelta: 100_000_000_000_000_000_001n,
      marginUsdc: 0n,
      oraclePrice: 98_300_000n,
      slippagePercent: 0.1,
      isClose: true,
      selectedMaxLeverageBps: 50_000,
    })).rejects.toThrow('Order size must use 100 plDXY increments')

    expect(mocks.getBlock).not.toHaveBeenCalled()
    expect(mocks.simulateContract).not.toHaveBeenCalled()
  })

  it('keeps manual finalization and cleanup keeper-only', async () => {
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.executeOrder(42n))
      .rejects.toThrow('Order finalization is keeper-operated')
    await expect(result.current.cleanupExpiredOrder(42n))
      .rejects.toThrow('Expired-order cleanup is keeper-operated')
  })

  it('reads terminal constraint evidence directly from the lifecycle book', async () => {
    mocks.identityReady = true
    mocks.readContract.mockResolvedValueOnce({
      account: ACCOUNT,
      clientOrderId: CLIENT_ORDER_ID,
      status: 3,
      reason: 8,
      executionMode: 1,
      terminalBlock: 11_604_786n,
      terminalTime: 1_788_167_807n,
      executionPrice: 98_750_341n,
      failedConstraint: 2,
      receiptHash: `0x${'46'.repeat(32)}`,
    })
    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.readOrderLifecycleOutcome(12n)).resolves.toEqual({
      orderId: 12n,
      account: ACCOUNT,
      clientOrderId: CLIENT_ORDER_ID,
      status: 3,
      terminalReason: 8,
      executionMode: 1,
      terminalBlock: 11_604_786n,
      terminalTime: 1_788_167_807n,
      executionPrice: 98_750_341n,
      failedConstraint: 2,
      receiptHash: `0x${'46'.repeat(32)}`,
    })
    expect(mocks.readContract).toHaveBeenCalledWith(
      expect.objectContaining({
        address: ORDER_LIFECYCLE_BOOK,
        functionName: 'outcome',
        args: [12n],
        blockTag: 'safe',
      })
    )
  })

  it('forwards managed sponsored operation status changes', async () => {
    mocks.identityReady = true
    const onStatus = vi.fn()
    const onIncluded = vi.fn()
    mocks.executeSponsoredPerpsAction.mockImplementationOnce(async (input) => {
      input.onStatus?.('awaiting-signature')
      input.onStatus?.('submitting')
      input.onStatus?.('confirming')
      input.onIncluded?.(sponsoredResult())
      return sponsoredResult()
    })

    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.commitOrder({
      ...commitInput(),
      onStatus,
      onIncluded,
    })).resolves.toEqual({
      account: ACCOUNT,
      clientOrderId: CLIENT_ORDER_ID,
      hash: TRANSACTION_HASH,
      userOperationHash: USER_OPERATION_HASH,
      orderId: 42n,
      replayed: false,
    })

    expect(onStatus.mock.calls).toEqual([
      ['awaiting-signature'],
      ['submitting'],
      ['confirming'],
    ])
    expect(onIncluded).toHaveBeenCalledOnce()
    expect(onIncluded).toHaveBeenCalledWith({
      account: ACCOUNT,
      clientOrderId: CLIENT_ORDER_ID,
      hash: TRANSACTION_HASH,
      userOperationHash: USER_OPERATION_HASH,
      orderId: 42n,
      replayed: false,
    })
    expect(mocks.invalidateQueries).toHaveBeenCalledOnce()
    expect(mocks.parseEventLogs).toHaveBeenCalledWith(
      expect.objectContaining({
        logs: [{
          address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        }],
      })
    )
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

  it('commits the exact leverage-buffered margin from final review', async () => {
    mocks.identityReady = true
    const reviewed = preparedOrder()
    reviewed.request.marginDelta = 201_500_000n

    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.commitOrder({
      ...commitInput(),
      preparedOrder: reviewed,
    })).resolves.toMatchObject({ orderId: 42n })

    expect(mocks.simulateContract).toHaveBeenCalledWith(
      expect.objectContaining({
        args: [expect.objectContaining({
          marginDelta: 201_500_000n,
        })],
      })
    )
  })

  it('accepts the relaxed web leverage bound after leverage was reviewed', async () => {
    mocks.identityReady = true
    const reviewed = preparedOrder()
    reviewed.request.bounds.maxPostLeverageBps = 0xffff_ffff

    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.commitOrder({
      ...commitInput(),
      preparedOrder: reviewed,
    })).resolves.toMatchObject({ orderId: 42n })
    expect(mocks.simulateContract).toHaveBeenCalledOnce()
  })

  it('rejects an OrderCommitted log that belongs to another account', async () => {
    mocks.identityReady = true
    mocks.parseEventLogs.mockReturnValue([{
      args: {
        account: OWNER,
        orderId: 99n,
      },
    }])

    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.commitOrder(commitInput())).rejects.toThrow(
      'no unique matching OrderCommitted event was found'
    )
  })

  it('returns an exact replay without creating another UserOperation', async () => {
    mocks.identityReady = true
    mocks.intentResolution = 1
    mocks.resolvedOrderId = 42n
    const onIncluded = vi.fn()

    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.commitOrder({
      ...commitInput(),
      onIncluded,
    })).resolves.toEqual({
      account: ACCOUNT,
      clientOrderId: CLIENT_ORDER_ID,
      orderId: 42n,
      replayed: true,
    })

    expect(onIncluded).toHaveBeenCalledWith({
      account: ACCOUNT,
      clientOrderId: CLIENT_ORDER_ID,
      orderId: 42n,
      replayed: true,
    })
    expect(mocks.simulateContract).not.toHaveBeenCalled()
    expect(mocks.executeSponsoredPerpsAction).not.toHaveBeenCalled()
  })

  it('blocks a conflicting client order ID before simulation or signing', async () => {
    mocks.identityReady = true
    mocks.intentResolution = 2
    mocks.resolvedOrderId = 99n

    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.commitOrder(commitInput())).rejects.toThrow(
      'this client order ID is already bound to a different immutable request'
    )
    expect(mocks.simulateContract).not.toHaveBeenCalled()
    expect(mocks.executeSponsoredPerpsAction).not.toHaveBeenCalled()
  })

  it('shows commit diagnostics when an undecoded pre-submission simulation fails', async () => {
    mocks.identityReady = true
    mocks.simulateContract.mockRejectedValueOnce(new Error('Transaction failed'))

    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.commitOrder(commitInput())).rejects.toThrow([
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

    await expect(result.current.commitOrder(commitInput())).rejects.toThrow([
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
        contracts: [{
          address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
          functionName: 'positions',
        }],
      }],
    })).toBe(true)
    expect(invalidateOptions.predicate({
      queryKey: ['readContracts', {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        contracts: [{
          address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
          functionName: 'riskParams',
        }],
      }],
    })).toBe(false)
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
