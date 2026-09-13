import { describe, expect, it, vi } from 'vitest'
import { encodeAbiParameters, encodeEventTopics, parseAbiItem, type Hex, type PublicClient } from 'viem'
import { recoverCanonicalInclusion } from '../canonicalRecovery'
import smoke from '../../../../../scripts/fixtures/aa-recovery-smoke-20260913.json'

const entryPoint = '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108' as const
const sender = '0x1111111111111111111111111111111111111111' as const
const paymaster = '0x2222222222222222222222222222222222222222' as const
const hash = (byte: string) => `0x${byte.repeat(64)}` as Hex
const abi = [parseAbiItem('event UserOperationEvent(bytes32 indexed userOpHash, address indexed sender, address indexed paymaster, uint256 nonce, bool success, uint256 actualGasCost, uint256 actualGasUsed)')]

function fixture(success = true) {
  const safe = { number: 101n, hash: hash('b'), timestamp: 1000n }
  const log = { address: entryPoint, topics: encodeEventTopics({ abi, eventName: 'UserOperationEvent',
    args: { userOpHash: hash('1'), sender, paymaster } }),
    data: encodeAbiParameters([{ type: 'uint256' }, { type: 'bool' }, { type: 'uint256' }, { type: 'uint256' }], [7n, success, 1000n, 900n]),
    transactionHash: hash('2'), blockHash: hash('a'), blockNumber: 100n, removed: false, logIndex: 2 }
  const receipt = { status: 'success', transactionHash: hash('2'), blockHash: hash('a'), blockNumber: 100n, logs: [log] }
  const getBlock = vi.fn(async (input: { blockNumber?: bigint }) => input.blockNumber === 100n
    ? { number: 100n, hash: hash('a'), timestamp: 990n } : safe)
  const client = { getChainId: vi.fn(async () => 421614), getTransactionReceipt: vi.fn(async () => receipt), getBlock }
  const input = { client: client as unknown as PublicClient, chainId: 421614, entryPoint, sender,
    operationHash: hash('1'), context: { transactionHash: hash('2'), nonce: 7n, paymaster }, safeBlock: { ...safe } }
  return { input, client, receipt, log, safe }
}

describe('recovery without a bundler receipt', () => {
  it.each(smoke.scenarios)('replays the recorded $name without Alto or resubmission', async scenario => {
    const receipt = { ...scenario.receipt, status: 'success', blockNumber: BigInt(scenario.receipt.blockNumber),
      logs: scenario.receipt.logs.map(log => ({ ...log, blockNumber: BigInt(log.blockNumber), logIndex: Number(BigInt(log.logIndex)) })) }
    const safeBlock = { number: BigInt(smoke.safe.number), hash: smoke.safe.hash as Hex, timestamp: BigInt(smoke.safe.timestamp) }
    const client = { getChainId: vi.fn(async () => smoke.chainId), getTransactionReceipt: vi.fn(async () => receipt),
      getBlock: vi.fn(async ({ blockNumber }: { blockNumber?: bigint }) => blockNumber === receipt.blockNumber
        ? { number: receipt.blockNumber, hash: scenario.header.hash, timestamp: BigInt(scenario.header.timestamp) } : safeBlock) }
    await expect(recoverCanonicalInclusion({ client: client as unknown as PublicClient, chainId: smoke.chainId,
      entryPoint: smoke.entryPoint as Hex, sender: smoke.sender as Hex, operationHash: scenario.operationHash as Hex,
      context: { transactionHash: scenario.receipt.transactionHash as Hex, nonce: BigInt(scenario.nonce), paymaster: smoke.paymaster as Hex }, safeBlock,
    })).resolves.toMatchObject({ kind: 'included', success: true, transactionHash: scenario.receipt.transactionHash })
  })
  it.each([true, false])('derives success=%s from the exact event inside a successful bundle', async success => {
    const { input, client } = fixture(success)
    await expect(recoverCanonicalInclusion(input)).resolves.toEqual({ kind: 'included', success,
      transactionHash: hash('2'), blockNumber: 100n })
    expect(client.getTransactionReceipt).toHaveBeenCalledExactlyOnceWith({ hash: hash('2') })
  })
  it.each(['hash', 'sender', 'nonce', 'paymaster', 'emitter', 'duplicate', 'malformedDuplicate', 'removed', 'transaction', 'block', 'chain', 'missingNonce'])('rejects %s mismatch', async mutation => {
    const { input, log, receipt, client } = fixture()
    if (mutation === 'hash') input.operationHash = hash('3')
    if (mutation === 'sender') input.sender = paymaster as unknown as typeof sender
    if (mutation === 'nonce') input.context.nonce = 8n
    if (mutation === 'paymaster') input.context.paymaster = sender as unknown as typeof paymaster
    if (mutation === 'emitter') log.address = sender as unknown as typeof entryPoint
    if (mutation === 'duplicate') receipt.logs.push({ ...log, logIndex: 3 })
    if (mutation === 'malformedDuplicate') receipt.logs.push({ ...log, data: '0x', logIndex: 3 })
    if (mutation === 'removed') log.removed = true
    if (mutation === 'transaction') log.transactionHash = hash('3')
    if (mutation === 'block') log.blockHash = hash('3')
    if (mutation === 'chain') client.getChainId.mockResolvedValue(42161)
    if (mutation === 'missingNonce') Reflect.deleteProperty(input.context, 'nonce')
    await expect(recoverCanonicalInclusion(input)).rejects.toThrow()
  })
  it('keeps inclusion above the safe head pending', async () => {
    const { input, client } = fixture()
    input.safeBlock.number = 99n
    client.getBlock.mockImplementation(async ({ blockNumber }) => blockNumber === 100n
      ? { number: 100n, hash: hash('a'), timestamp: 990n } : input.safeBlock)
    await expect(recoverCanonicalInclusion(input)).resolves.toEqual({ kind: 'not-safe-yet' })
  })
  it.each(['reorg', 'safeReorg', 'regression', 'timestampRegression', 'providerFailure'])('does not confirm on %s', async mutation => {
    const { input, client, safe } = fixture()
    if (mutation === 'reorg') client.getBlock.mockResolvedValue({ ...safe, number: 100n, hash: hash('c') })
    if (mutation === 'safeReorg') safe.hash = hash('c')
    if (mutation === 'regression') safe.number = 99n
    if (mutation === 'timestampRegression') safe.timestamp = 980n
    if (mutation === 'providerFailure') client.getTransactionReceipt.mockRejectedValue(new Error('unavailable'))
    await expect(recoverCanonicalInclusion(input)).rejects.toThrow()
  })
})
