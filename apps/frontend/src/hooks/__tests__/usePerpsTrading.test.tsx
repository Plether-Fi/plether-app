import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { renderHook } from '@testing-library/react'
import { type ReactNode } from 'react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { usePerpsTrading } from '../usePerpsTrading'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../../contracts/perpsAddresses'

const mocks = vi.hoisted(() => ({
  writeContractAsync: vi.fn(),
  estimateFeesPerGas: vi.fn(),
  getGasPrice: vi.fn(),
  getBlock: vi.fn(),
  readContract: vi.fn(),
  simulateContract: vi.fn(),
  waitForTransactionReceipt: vi.fn(),
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
  useAccount: () => ({
    address: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
  }),
  usePublicClient: () => ({
    estimateFeesPerGas: mocks.estimateFeesPerGas,
    getGasPrice: mocks.getGasPrice,
    getBlock: mocks.getBlock,
    readContract: mocks.readContract,
    simulateContract: mocks.simulateContract,
    waitForTransactionReceipt: mocks.waitForTransactionReceipt,
  }),
  useWriteContract: () => ({
    writeContractAsync: mocks.writeContractAsync,
  }),
}))

function wrapper({ children }: { children: ReactNode }) {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
    },
  })
  queryClient.invalidateQueries = mocks.invalidateQueries

  return <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
}

describe('usePerpsTrading', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mocks.parseEventLogs.mockReturnValue([{ args: { orderId: 42n } }])
    mocks.waitForTransactionReceipt.mockResolvedValue({ status: 'success', logs: [] })
    mocks.writeContractAsync.mockResolvedValue('0xabc')
    mocks.simulateContract.mockResolvedValue({})
    mocks.getBlock.mockResolvedValue({ timestamp: 1_700_000_000n })
    mocks.readContract.mockImplementation(({ functionName }: { functionName: string }) => {
      switch (functionName) {
        case 'getPendingOrders':
          return []
        case 'maxPendingOrders':
          return 10n
        case 'previewOpenRevertCode':
          return 0
        case 'previewClose':
          return { valid: true, invalidReason: 0 }
        default:
          throw new Error(`Unexpected readContract call: ${functionName}`)
      }
    })
  })

  it('still opens the wallet writer when pre-wallet fee RPC calls fail', async () => {
    mocks.estimateFeesPerGas.mockRejectedValue(new Error('fee endpoint unavailable'))
    mocks.getGasPrice.mockRejectedValue(new Error('gas price endpoint unavailable'))

    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(
      result.current.commitOrder({
        direction: 'long',
        notionalUsdc: 1_000_000_000n,
        sizeDelta: 1_000_000_000_000_000_000n,
        marginUsdc: 200_000_000n,
        oraclePrice: 98_300_000n,
        slippagePercent: 0.1,
        isClose: false,
      })
    ).resolves.toEqual({
      hash: '0xabc',
      orderId: 42n,
    })

    expect(mocks.writeContractAsync).toHaveBeenCalledTimes(1)
    expect(mocks.writeContractAsync).toHaveBeenCalledWith(expect.objectContaining({
      chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
      address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
      functionName: 'commitOrder',
      args: [0, 1_000_000_000_000_000_000n, 200_000_000n, expect.any(BigInt), false],
    }))
    expect(mocks.writeContractAsync.mock.calls[0][0]).not.toHaveProperty('maxFeePerGas')
    expect(mocks.writeContractAsync.mock.calls[0][0]).not.toHaveProperty('gasPrice')
  })

  it('still opens the wallet writer when pre-wallet fee RPC calls hang', async () => {
    vi.useFakeTimers()
    try {
      mocks.estimateFeesPerGas.mockReturnValue(new Promise(() => {}))
      mocks.getGasPrice.mockReturnValue(new Promise(() => {}))
      const onWalletRequestStart = vi.fn()

      const { result } = renderHook(() => usePerpsTrading(), { wrapper })

      const commitPromise = result.current.commitOrder({
        direction: 'long',
        notionalUsdc: 1_000_000_000n,
        sizeDelta: 1_000_000_000_000_000_000n,
        marginUsdc: 200_000_000n,
        oraclePrice: 98_300_000n,
        slippagePercent: 0.1,
        isClose: false,
        onWalletRequestStart,
      })

      expect(mocks.writeContractAsync).not.toHaveBeenCalled()
      await vi.advanceTimersByTimeAsync(2_500)
      await vi.advanceTimersByTimeAsync(2_500)

      await expect(commitPromise).resolves.toEqual({
        hash: '0xabc',
        orderId: 42n,
      })

      expect(onWalletRequestStart).toHaveBeenCalledTimes(1)
      expect(mocks.writeContractAsync).toHaveBeenCalledTimes(1)
      expect(onWalletRequestStart.mock.invocationCallOrder[0]).toBeLessThan(
        mocks.writeContractAsync.mock.invocationCallOrder[0]
      )
      expect(mocks.writeContractAsync.mock.calls[0][0]).not.toHaveProperty('maxFeePerGas')
      expect(mocks.writeContractAsync.mock.calls[0][0]).not.toHaveProperty('gasPrice')
    } finally {
      vi.useRealTimers()
    }
  })

  it('passes buffered EIP-1559 fees when fee estimation succeeds', async () => {
    mocks.estimateFeesPerGas.mockResolvedValue({
      maxFeePerGas: 100n,
      maxPriorityFeePerGas: 10n,
    })

    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await result.current.commitOrder({
      direction: 'short',
      notionalUsdc: 1_000_000_000n,
      sizeDelta: 1_000_000_000_000_000_000n,
      marginUsdc: 200_000_000n,
      oraclePrice: 98_300_000n,
      slippagePercent: 0.1,
      isClose: false,
    })

    expect(mocks.writeContractAsync).toHaveBeenCalledWith(expect.objectContaining({
      maxFeePerGas: 126n,
      maxPriorityFeePerGas: 13n,
    }))
  })

  it('shows tx hash and commit diagnostics when a submitted commit receipt reverts', async () => {
    const failedHash = '0x1111111111111111111111111111111111111111111111111111111111111111'
    mocks.writeContractAsync.mockResolvedValue(failedHash)
    mocks.waitForTransactionReceipt.mockResolvedValue({ status: 'reverted', logs: [] })
    mocks.readContract.mockImplementation(({ functionName }: { functionName: string }) => {
      switch (functionName) {
        case 'getPendingOrders':
          return []
        case 'maxPendingOrders':
          return 10n
        case 'getTraderAccount':
          return {
            equityUsdc: 1_000_000_000n,
            withdrawableUsdc: 750_000_000n,
            pendingOrderMarginUsdc: 0n,
            pendingExecutionBountyUsdc: 0n,
          }
        case 'previewOpenRevertCode':
          return 0
        default:
          throw new Error(`Unexpected readContract call: ${functionName}`)
      }
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
    })).rejects.toThrow([
      'Commit reverted after wallet confirmation, but the receipt did not include decodable revert data.',
      `Failed tx: ${failedHash}`,
      'Current account state: 0/10 pending orders, equity 1 000 USDC, free/withdrawable 750 USDC, pending margin 0 USDC, pending bounty 0 USDC.',
      'Latest open preview still passes.',
      'A fresh commit simulation still passes',
    ].join('\n'))
  })

  it('shows commit diagnostics when an undecoded pre-wallet commit simulation fails', async () => {
    mocks.simulateContract.mockRejectedValueOnce(new Error('Transaction failed'))
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
        default:
          throw new Error(`Unexpected readContract call: ${functionName}`)
      }
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
    })).rejects.toThrow([
      'Commit failed before an order was created, and the RPC did not return a decodable contract error.',
      'No transaction hash was returned by the wallet/RPC.',
      'Current account state: 0/10 pending orders, equity 900 USDC, free/withdrawable 500 USDC, pending margin 250 USDC, pending bounty 0.01 USDC.',
      'Latest open preview still passes.',
      'A fresh commit simulation still passes',
    ].join('\n'))
  })

  it('adds isolated margin to the active CFD position', async () => {
    mocks.estimateFeesPerGas.mockResolvedValue({
      maxFeePerGas: 100n,
      maxPriorityFeePerGas: 10n,
    })

    const { result } = renderHook(() => usePerpsTrading(), { wrapper })

    await expect(result.current.addPositionMargin(250000000n)).resolves.toBe('0xabc')

    expect(mocks.simulateContract).toHaveBeenCalledWith(expect.objectContaining({
      account: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
      address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
      functionName: 'addMargin',
      args: ['0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B', 250000000n],
    }))
    expect(mocks.writeContractAsync).toHaveBeenCalledWith(expect.objectContaining({
      chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
      address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
      functionName: 'addMargin',
      args: ['0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B', 250000000n],
      maxFeePerGas: 126n,
      maxPriorityFeePerGas: 13n,
    }))
    expect(mocks.invalidateQueries).toHaveBeenCalled()
  })
})
