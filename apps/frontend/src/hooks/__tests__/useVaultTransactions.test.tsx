import { act, renderHook } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { PERPS_ARBITRUM_SEPOLIA } from '../../contracts/perpsAddresses'
import { useVaultTransactions } from '../useVaultTransactions'

const mocks = vi.hoisted(() => ({
  address: '0x1111111111111111111111111111111111111111' as `0x${string}` | undefined,
  chainId: 421614,
  execute: vi.fn(),
  reset: vi.fn(),
  simulateContract: vi.fn(),
  writeContractAsync: vi.fn(),
}))

vi.mock('wagmi', () => ({
  useConfig: () => ({ id: 'test-config' }),
  usePublicClient: () => ({
    simulateContract: mocks.simulateContract,
  }),
  useWriteContract: () => ({
    writeContractAsync: mocks.writeContractAsync,
  }),
}))

vi.mock('@wagmi/core', () => ({
  getAccount: () => ({
    address: mocks.address,
    chainId: mocks.chainId,
  }),
}))

vi.mock('../useTransactionSequence', () => ({
  useTransactionSequence: () => ({
    execute: mocks.execute,
    reset: mocks.reset,
    isRunning: false,
    isSuccess: false,
    isError: false,
    error: null,
  }),
}))

interface SequenceConfig {
  type: string
  buildSteps: () => {
    label: string
    action: () => Promise<`0x${string}` | undefined>
  }[]
}

describe('useVaultTransactions', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mocks.address = '0x1111111111111111111111111111111111111111'
    mocks.chainId = 421614
    mocks.execute.mockResolvedValue(undefined)
    mocks.simulateContract.mockResolvedValue({ request: {} })
    mocks.writeContractAsync.mockResolvedValue('0xabc')
  })

  it('approves exact USDC before an immediate deposit when allowance is insufficient', async () => {
    const { result } = renderHook(() => useVaultTransactions({
      vaultAddress: PERPS_ARBITRUM_SEPOLIA.seniorVault,
      allowance: 0n,
    }))

    act(() => {
      result.current.deposit(2_000_000n)
    })

    const config = mocks.execute.mock.calls[0][0] as SequenceConfig
    expect(config.type).toBe('supply')
    const steps = config.buildSteps()
    expect(steps.map(({ label }) => label)).toEqual(['Approve USDC', 'Deposit USDC'])

    await steps[0].action()
    await steps[1].action()

    expect(mocks.simulateContract).toHaveBeenNthCalledWith(1, expect.objectContaining({
      address: PERPS_ARBITRUM_SEPOLIA.usdc,
      functionName: 'approve',
      args: [PERPS_ARBITRUM_SEPOLIA.seniorVault, 2_000_000n],
    }))
    expect(mocks.simulateContract).toHaveBeenNthCalledWith(2, expect.objectContaining({
      address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
      functionName: 'deposit',
      args: [2_000_000n, mocks.address],
    }))
    expect(mocks.writeContractAsync).toHaveBeenCalledTimes(2)
  })

  it('skips approval when the current allowance covers the deposit', () => {
    const { result } = renderHook(() => useVaultTransactions({
      vaultAddress: PERPS_ARBITRUM_SEPOLIA.juniorVault,
      allowance: 5_000_000n,
    }))

    act(() => {
      result.current.deposit(2_000_000n)
    })

    const config = mocks.execute.mock.calls[0][0] as SequenceConfig
    expect(config.buildSteps().map(({ label }) => label)).toEqual(['Deposit USDC'])
  })

  it('approves exact USDC and submits a queued deposit request', async () => {
    const { result } = renderHook(() => useVaultTransactions({
      vaultAddress: PERPS_ARBITRUM_SEPOLIA.seniorVault,
      allowance: 0n,
    }))

    act(() => {
      result.current.requestDeposit(4_000_000n)
    })

    const config = mocks.execute.mock.calls[0][0] as SequenceConfig
    expect(config.type).toBe('supply')
    const steps = config.buildSteps()
    expect(steps.map(({ label }) => label)).toEqual(['Approve USDC', 'Queue deposit'])

    await steps[0].action()
    await steps[1].action()

    expect(mocks.simulateContract).toHaveBeenNthCalledWith(2, expect.objectContaining({
      address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
      functionName: 'requestDeposit',
      args: [4_000_000n, mocks.address],
    }))
    expect(mocks.writeContractAsync).toHaveBeenNthCalledWith(2, expect.objectContaining({
      chainId: 421614,
      functionName: 'requestDeposit',
    }))
  })

  it('simulates and submits a synchronous owner withdrawal', async () => {
    const { result } = renderHook(() => useVaultTransactions({
      vaultAddress: PERPS_ARBITRUM_SEPOLIA.juniorVault,
      allowance: 0n,
    }))

    act(() => {
      result.current.withdraw(3_000_000n)
    })

    const config = mocks.execute.mock.calls[0][0] as SequenceConfig
    expect(config.type).toBe('withdraw')
    const [withdrawStep] = config.buildSteps()
    await withdrawStep.action()

    expect(mocks.simulateContract).toHaveBeenCalledWith(expect.objectContaining({
      address: PERPS_ARBITRUM_SEPOLIA.juniorVault,
      functionName: 'withdraw',
      args: [3_000_000n, mocks.address, mocks.address],
    }))
    expect(mocks.writeContractAsync).toHaveBeenCalledWith(expect.objectContaining({
      account: mocks.address,
      chainId: 421614,
      functionName: 'withdraw',
    }))
  })

  it.each([
    ['cancelPendingDeposit', 'Cancel request', 'withdraw'],
    ['finalizeDepositEpoch', 'Finalize epoch', 'supply'],
    ['claimDepositShares', 'Claim shares', 'supply'],
  ] as const)('simulates and submits %s for an epoch', async (method, label, type) => {
    const { result } = renderHook(() => useVaultTransactions({
      vaultAddress: PERPS_ARBITRUM_SEPOLIA.juniorVault,
      allowance: 0n,
    }))

    act(() => {
      result.current[method](500_002n)
    })

    const config = mocks.execute.mock.calls[0][0] as SequenceConfig
    expect(config.type).toBe(type)
    const [step] = config.buildSteps()
    expect(step.label).toBe(label)
    await step.action()

    expect(mocks.simulateContract).toHaveBeenCalledWith(expect.objectContaining({
      address: PERPS_ARBITRUM_SEPOLIA.juniorVault,
      functionName: method,
      args: [500_002n],
    }))
    expect(mocks.writeContractAsync).toHaveBeenCalledWith(expect.objectContaining({
      account: mocks.address,
      chainId: 421614,
      functionName: method,
    }))
  })

  it('stops a multi-step deposit if the connected account changes after approval', async () => {
    const { result } = renderHook(() => useVaultTransactions({
      vaultAddress: PERPS_ARBITRUM_SEPOLIA.seniorVault,
      allowance: 0n,
    }))

    act(() => {
      result.current.deposit(2_000_000n)
    })

    const config = mocks.execute.mock.calls[0][0] as SequenceConfig
    const [approveStep, depositStep] = config.buildSteps()
    await approveStep.action()
    mocks.address = '0x2222222222222222222222222222222222222222'

    await expect(depositStep.action()).rejects.toThrow('wallet account changed')
    expect(mocks.writeContractAsync).toHaveBeenCalledTimes(1)
  })

  it('rejects submission outside Arbitrum Sepolia before simulation', async () => {
    mocks.chainId = 1
    const { result } = renderHook(() => useVaultTransactions({
      vaultAddress: PERPS_ARBITRUM_SEPOLIA.seniorVault,
      allowance: 10_000_000n,
    }))

    act(() => {
      result.current.deposit(2_000_000n)
    })

    const config = mocks.execute.mock.calls[0][0] as SequenceConfig
    const [depositStep] = config.buildSteps()
    await expect(depositStep.action()).rejects.toThrow('Switch to Arbitrum Sepolia')
    expect(mocks.simulateContract).not.toHaveBeenCalled()
    expect(mocks.writeContractAsync).not.toHaveBeenCalled()
  })
})
