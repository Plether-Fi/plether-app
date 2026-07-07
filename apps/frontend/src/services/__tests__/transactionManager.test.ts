import { describe, it, expect, vi, beforeEach } from 'vitest'
import { useTransactionStore } from '../../stores/transactionStore'

const MOCK_ADDRESS = '0x1234567890123456789012345678901234567890' as const
const MOCK_TX_HASH = '0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890' as const
const MOCK_SIGNATURE = '0x' + 'ab'.repeat(32) + 'cd'.repeat(32) + '1b' as `0x${string}`

const mockReadContract = vi.fn()
const mockWriteContract = vi.fn()
const mockSignTypedData = vi.fn()
const mockGetWalletClient = vi.fn()
const mockWaitForTransactionReceipt = vi.fn()
const mockGetPublicClient = vi.fn()

vi.mock('@wagmi/core', () => ({
  readContract: (...args: unknown[]) => mockReadContract(...args),
  writeContract: (...args: unknown[]) => mockWriteContract(...args),
  signTypedData: (...args: unknown[]) => mockSignTypedData(...args),
  getWalletClient: (...args: unknown[]) => mockGetWalletClient(...args),
  waitForTransactionReceipt: (...args: unknown[]) => mockWaitForTransactionReceipt(...args),
  getPublicClient: (...args: unknown[]) => mockGetPublicClient(...args),
}))

import { transactionManager } from '../transactionManager'

const mockConfig = {
  state: { chainId: 11155111 },
} as Parameters<typeof transactionManager.setConfig>[0]

function setupMocks() {
  mockGetWalletClient.mockResolvedValue({
    account: { address: MOCK_ADDRESS },
  })
  mockSignTypedData.mockResolvedValue(MOCK_SIGNATURE)
  mockWriteContract.mockResolvedValue(MOCK_TX_HASH)
  mockWaitForTransactionReceipt.mockResolvedValue({ status: 'success' })
  mockGetPublicClient.mockReturnValue(null)
}

function getSignTypedDataDomain() {
  const call = mockSignTypedData.mock.calls[0]
  return call[1].domain
}

describe('transactionManager permit signing', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    useTransactionStore.getState().transactions = []
    transactionManager.setConfig(mockConfig)
    setupMocks()
  })

  it('uses eip712Domain version when supported', async () => {
    mockReadContract.mockImplementation((_config: unknown, args: { functionName: string }) => {
      if (args.functionName === 'nonces') return Promise.resolve(0n)
      if (args.functionName === 'name') return Promise.resolve('plDXY-BEAR')
      if (args.functionName === 'eip712Domain') {
        return Promise.resolve(['0x0f', 'plDXY-BEAR', '1', 11155111n, MOCK_ADDRESS, '0x' + '00'.repeat(32), []])
      }
      return Promise.resolve()
    })

    await transactionManager.executeStake('BEAR', 1000000n)

    expect(mockSignTypedData).toHaveBeenCalledOnce()
    const domain = getSignTypedDataDomain()
    expect(domain.version).toBe('1')
    expect(domain.name).toBe('plDXY-BEAR')
  })

  it('falls back to version "2" when eip712Domain reverts', async () => {
    mockReadContract.mockImplementation((_config: unknown, args: { functionName: string }) => {
      if (args.functionName === 'nonces') return Promise.resolve(0n)
      if (args.functionName === 'name') return Promise.resolve('USD Coin')
      if (args.functionName === 'eip712Domain') {
        return Promise.reject(new Error('reverted'))
      }
      return Promise.resolve()
    })

    await transactionManager.executeMint(1000000n, 1000000n)

    expect(mockSignTypedData).toHaveBeenCalledOnce()
    const domain = getSignTypedDataDomain()
    expect(domain.version).toBe('2')
    expect(domain.name).toBe('USD Coin')
  })

  it('buys plDXY-BULL with approval and zapMint when USDC permit is unavailable', async () => {
    mockReadContract.mockImplementation((_config: unknown, args: { functionName: string }) => {
      if (args.functionName === 'allowance') return Promise.resolve(0n)
      throw new Error(`Unexpected readContract ${args.functionName}`)
    })

    await transactionManager.executeZapBuy(1000000n, 900000000000000000n, 100n)

    expect(mockSignTypedData).not.toHaveBeenCalled()
    expect(mockWriteContract).toHaveBeenCalledTimes(2)
    expect(mockWriteContract.mock.calls[0][1].functionName).toBe('approve')
    expect(mockWriteContract.mock.calls[0][1].args[1]).toBe(1000000n)
    expect(mockWriteContract.mock.calls[1][1].functionName).toBe('zapMint')
    expect(mockWriteContract.mock.calls[1][1].args.slice(0, 3)).toEqual([
      1000000n,
      900000000000000000n,
      100n,
    ])
  })
})
