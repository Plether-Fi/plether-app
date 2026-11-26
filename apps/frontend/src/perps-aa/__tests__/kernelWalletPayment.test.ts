import { describe, expect, it, vi } from 'vitest'
import {
  encodeAbiParameters, encodeEventTopics, parseAbiItem, parseAbiParameters, zeroAddress,
  TransactionReceiptNotFoundError, type Address, type Hex, type PublicClient,
} from 'viem'
import { arbitrumSepolia } from 'viem/chains'
import { kernelSepoliaCanaryProfile } from '../kernelDeploymentPins'
import type { KernelOperationAttempt } from '../kernelOperationJournal'
import { type KernelWalletPreparation } from '../kernelRpc'
import { encodeKernelWalletTransaction, quoteKernelWalletPayment, readKernelWalletOutcome } from '../kernelWalletPayment'

vi.mock('../deadlineClock', () => ({ deadlineNow: () => 1_000_000, refreshDeadlineClock: async () => ({ now: () => 1_000_000 }) }))

const entryPoint = kernelSepoliaCanaryProfile.entryPoint
const ownerAddress = '0x1100000000000000000000000000000000000000' as Address
const sender = '0x2200000000000000000000000000000000000000' as Address
const hash = `0x${'33'.repeat(32)}` as Hex
const blockHash = `0x${'44'.repeat(32)}` as Hex
const signature = `0x${'55'.repeat(65)}` as Hex
const event = parseAbiItem('event UserOperationEvent(bytes32 indexed userOpHash, address indexed sender, address indexed paymaster, uint256 nonce, bool success, uint256 actualGasCost, uint256 actualGasUsed)')

async function setup(success: boolean, outerStatus: 'success' | 'reverted' = 'success') {
  const getBalance = vi.fn(async () => 3_000_000n)
  const block = { number: 92n, hash: blockHash }
  const client = { chain: arbitrumSepolia, estimateFeesPerGas: async () => ({ maxFeePerGas: 1n, maxPriorityFeePerGas: 0n }),
    getBalance, readContract: vi.fn(async ({ functionName }: { functionName: string }) => functionName === 'getNonce' ? 1n : [10_000n]),
    call: vi.fn(async () => ({})),
    getBlock: vi.fn(async () => block), getTransactionCount: vi.fn(async ({ blockNumber }: { blockNumber?: bigint }) => blockNumber !== undefined && blockNumber < 92n ? 14 : 15),
    getTransaction: vi.fn(), getTransactionReceipt: vi.fn(), getLogs: vi.fn(async () => []),
  }
  const rpc = client as unknown as PublicClient
  const prepared: KernelWalletPreparation = await quoteKernelWalletPayment(rpc, ownerAddress, {
    mode: 'eth', sender, callData: '0x1234', preparationId: hash, expectedNonce: 1n,
  })
  const topics = encodeEventTopics({ abi: [event], eventName: 'UserOperationEvent',
    args: { userOpHash: prepared.userOperationHash, sender, paymaster: zeroAddress } })
  const transaction = {
    hash, from: ownerAddress, to: entryPoint, blockHash, blockNumber: 92n, nonce: 14,
    input: encodeKernelWalletTransaction(prepared.operation, ownerAddress, signature), value: 0n,
  }
  const receipt = { transactionHash: hash, status: outerStatus, blockNumber: 92n, blockHash, gasUsed: 12n, effectiveGasPrice: 2n,
    logs: outerStatus === 'success' ? [{ address: entryPoint, topics,
      data: encodeAbiParameters(parseAbiParameters('uint256,bool,uint256,uint256'), [1n, success, 0n, 12n]) }] : [],
  }
  client.getTransaction.mockResolvedValue(transaction)
  client.getTransactionReceipt.mockResolvedValue(receipt)
  client.getBlock.mockResolvedValue({ ...block, transactions: [transaction] })
  const attempt = { ownerAddress, signature, prepared, walletSubmission: { fromBlock: 92n, transactionHash: hash, nonce: 14 } } as KernelOperationAttempt
  return { client, rpc, attempt, receipt, transaction }
}

describe('wallet-paid Kernel inclusion', () => {
  it('preserves an inner revert even when the outer wallet transaction succeeds', async () => {
    const { rpc, attempt, receipt } = await setup(false)
    await expect(readKernelWalletOutcome(rpc, attempt)).resolves.toMatchObject({
      state: 'included', outerReceipt: receipt, receipt: { success: false, actualGasCost: 0n },
    })
  })

  it('recovers an outer revert after the wallet response was lost', async () => {
    const { rpc, attempt, receipt } = await setup(false, 'reverted')
    await expect(readKernelWalletOutcome(rpc, { ...attempt, walletSubmission: { fromBlock: 92n, nonce: 14 } }))
      .resolves.toEqual({ state: 'wallet_failed', outerReceipt: receipt })
  })

  it('verifies a cancellation at the saved wallet nonce without treating it as executed', async () => {
    const { client, rpc, attempt, transaction } = await setup(true)
    const originalHash = `0x${'66'.repeat(32)}` as Hex
    client.getTransactionReceipt.mockRejectedValueOnce(new TransactionReceiptNotFoundError({ hash: originalHash }))
    client.getTransaction.mockResolvedValueOnce({ ...transaction, to: ownerAddress, input: '0x' })
    await expect(readKernelWalletOutcome(rpc, { ...attempt, walletSubmission: { fromBlock: 92n, nonce: 14, transactionHash: originalHash } }))
      .resolves.toEqual({ state: 'wallet_replaced' })
  })

  it('resolves wallet_replaced when transactionHash was never returned and the owner nonce was replaced', async () => {
    const { client, rpc, attempt, transaction } = await setup(true)
    client.getTransaction.mockResolvedValueOnce({ ...transaction, to: ownerAddress, input: '0x' })
    await expect(readKernelWalletOutcome(rpc, { ...attempt, walletSubmission: { fromBlock: 92n, nonce: 14 } }))
      .resolves.toEqual({ state: 'wallet_replaced' })
  })

  it('retires an old unfound wallet attempt as nonce_consumed past the 4,000-block log window', async () => {
    const { client, rpc, attempt } = await setup(true)
    client.getBlock.mockResolvedValue({ number: 5_000n, hash: blockHash, transactions: [] })
    client.getTransactionCount.mockResolvedValue(14)
    client.readContract.mockResolvedValue(2n)
    await expect(readKernelWalletOutcome(rpc, { ...attempt, walletSubmission: { fromBlock: 92n, nonce: 14 } }))
      .resolves.toEqual({ state: 'nonce_consumed' })
  })
})
