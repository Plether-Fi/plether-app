import { buildDepositForCalls, marginClearinghouseFundingAbi } from '@plether-fi/perps-aa-client'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { encodeAbiParameters, encodeEventTopics, keccak256, type Hex, type PublicClient, type WalletClient } from 'viem'
import { arbitrumSepolia } from 'viem/chains'
import { depositKernelMarginFromOwner } from '../kernelMarginFunding'
import type { KernelPerpsAaSmartAccountRuntime } from '../runtimeContext'

const { records } = vi.hoisted(() => ({ records: new Map<string, unknown>() }))
vi.mock('idb-keyval', () => ({
  createStore: () => undefined,
  get: async (key: string) => records.get(key),
  set: async (key: string, value: unknown) => { records.set(key, value) },
  del: async (key: string) => { records.delete(key) },
}))
vi.mock('../kernelOperationJournal', () => ({
  kernelAttemptScopeKey: () => 'test-owner-deposit',
  withKernelOperationLane: <T,>(_scope: unknown, deposit: () => Promise<T>) => deposit(),
}))
vi.mock('../../contracts/perpsAddresses', async importOriginal => {
  const actual = await importOriginal<typeof import('../../contracts/perpsAddresses')>()
  return {
    ...actual,
    PERPS_RELEASE: { contracts: {
      settlementToken: { address: '0x4400000000000000000000000000000000000000', runtimeCodeHash: keccak256('0x01') },
      marginClearinghouse: { address: '0x5500000000000000000000000000000000000000', runtimeCodeHash: keccak256('0x01') },
    } },
  }
})

const owner = '0x1100000000000000000000000000000000000000'
const sender = '0x2200000000000000000000000000000000000000'
const settlementToken = '0x4400000000000000000000000000000000000000'
const clearinghouse = '0x5500000000000000000000000000000000000000'
const hash = `0x${'aa'.repeat(32)}` as Hex
const approvalHash = `0x${'bb'.repeat(32)}` as Hex
const blockHash = `0x${'cc'.repeat(32)}` as Hex
const amount = 1_000_000n
const block = { number: 100n, hash: blockHash }
const [approval, deposit] = buildDepositForCalls({ settlementToken, clearinghouse, beneficiary: sender, amount })
const transaction = { hash, from: owner, to: clearinghouse, value: 0n, input: deposit.data, blockHash, blockNumber: 100n, nonce: 2 }
const receipt = { transactionHash: hash, status: 'success', blockHash, blockNumber: 100n,
  logs: [{ address: clearinghouse, topics: encodeEventTopics({ abi: marginClearinghouseFundingAbi, eventName: 'DepositFor',
    args: { payer: owner, account: sender } }), data: encodeAbiParameters([{ type: 'uint256' }], [amount]) }],
}

function fixture(allowance = amount) {
  const readAllowance = vi.fn(async () => allowance)
  const requireCurrent = vi.fn()
  const getAddresses = vi.fn(async () => [owner])
  const sendTransaction = vi.fn(async () => hash)
  const waitForTransactionReceipt = vi.fn(async () => receipt)
  const simulateContract = vi.fn(async () => ({}))
  const client = { chain: arbitrumSepolia,
    readContract: async ({ functionName }: { functionName: string }) =>
      functionName === 'balanceOf' ? amount : functionName === 'allowance' ? readAllowance() : settlementToken,
    getBytecode: async () => '0x01', getBlock: async () => block, getBlockNumber: async () => block.number,
    estimateFeesPerGas: async () => ({ maxFeePerGas: 25n, maxPriorityFeePerGas: 0n }),
    getTransactionCount: async () => 2, getTransaction: async () => transaction, waitForTransactionReceipt, simulateContract,
  } as unknown as PublicClient
  const wallet = { account: { address: owner }, getAddresses, getChainId: async () => arbitrumSepolia.id, sendTransaction } as unknown as WalletClient
  const runtime = { ownerAddress: owner, gasPayment: { paysEthFromWallet: true },
    kernel: { scope: { sender, chainId: arbitrumSepolia.id },
      verifyAccount: async () => ({ address: sender, ownerAddress: owner, deployed: false, blockNumber: block.number, blockHash }) },
  } as unknown as KernelPerpsAaSmartAccountRuntime
  const execute = () => depositKernelMarginFromOwner({ client, wallet, runtime, amount, requireCurrent })
  return { execute, getAddresses, readAllowance, sendTransaction, simulateContract, waitForTransactionReceipt }
}

beforeEach(() => { records.clear() })

describe('Owner Wallet margin deposit', () => {
  it('sends only depositFor when allowance is sufficient, even for an undeployed account', async () => {
    const f = fixture()
    await expect(f.execute()).resolves.toBe(hash)
    expect(f.sendTransaction).toHaveBeenCalledExactlyOnceWith(expect.objectContaining({ ...deposit, account: owner, nonce: 2 }))
    expect(f.simulateContract).toHaveBeenCalledWith(expect.objectContaining({ args: [sender, amount], account: owner }))
    expect(records.size).toBe(0)
  })

  it('confirms an exact approval before sending depositFor when allowance is insufficient', async () => {
    const f = fixture(0n)
    f.readAllowance.mockResolvedValueOnce(0n).mockResolvedValueOnce(amount)
    f.sendTransaction.mockResolvedValueOnce(approvalHash)
    await expect(f.execute()).resolves.toBe(hash)
    expect(f.sendTransaction).toHaveBeenNthCalledWith(1, expect.objectContaining({ ...approval, account: owner }))
    expect(f.waitForTransactionReceipt).toHaveBeenNthCalledWith(1, expect.objectContaining({ hash: approvalHash }))
    expect(f.sendTransaction).toHaveBeenNthCalledWith(2, expect.objectContaining({ ...deposit, account: owner }))
    expect(f.waitForTransactionReceipt.mock.invocationCallOrder[0]).toBeLessThan(f.sendTransaction.mock.invocationCallOrder[1]!)
  })

  it('stops between approval and deposit if the Owner Wallet changes', async () => {
    const f = fixture(0n)
    f.readAllowance.mockResolvedValueOnce(0n).mockResolvedValueOnce(amount)
    f.getAddresses.mockResolvedValueOnce([owner]).mockResolvedValueOnce([])
    await expect(f.execute()).rejects.toThrow('Reconnect the Owner Wallet')
    expect(f.sendTransaction).toHaveBeenCalledTimes(1)
    expect(records.size).toBe(0)
  })

  it('recovers an ambiguous receipt without sending a duplicate deposit', async () => {
    const f = fixture()
    f.waitForTransactionReceipt.mockRejectedValueOnce(new Error('RPC timed out'))
    await expect(f.execute()).rejects.toThrow('deposit is still unverified')
    expect(records.size).toBe(1)
    await expect(f.execute()).resolves.toBe(hash)
    expect(f.sendTransaction).toHaveBeenCalledTimes(1)
    expect(records.size).toBe(0)
  })

  it('allows a fresh deposit after the wallet RPC explicitly rejects its fee cap', async () => {
    const f = fixture()
    const error = new Error('Wallet submission failed', { cause: new Error('max fee per gas less than block base fee') })
    f.sendTransaction.mockRejectedValueOnce(error)
    await expect(f.execute()).rejects.toBe(error)
    expect(records.size).toBe(0)
    expect(f.waitForTransactionReceipt).not.toHaveBeenCalled()
    await expect(f.execute()).resolves.toBe(hash)
    expect(f.sendTransaction).toHaveBeenCalledTimes(2)
  })

  it('keeps an ambiguous outdated-transaction response in recovery', async () => {
    const f = fixture()
    f.sendTransaction.mockRejectedValueOnce(Object.assign(new Error('transaction is outdated'), { name: 'FeeCapTooLowError' }))
    await expect(f.execute()).rejects.toThrow('deposit could not be verified')
    expect(records.size).toBe(1)
    await expect(f.execute()).rejects.toThrow('deposit is still unverified')
    expect(f.sendTransaction).toHaveBeenCalledTimes(1)
  })
})
