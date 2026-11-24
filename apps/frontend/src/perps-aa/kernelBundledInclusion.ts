import { isAddressEqual, parseAbiItem, parseEventLogs, toEventSelector, type PublicClient } from 'viem'
import { type UserOperationReceipt } from 'viem/account-abstraction'
import type { KernelOperationAttempt } from './kernelOperationJournal'

const operationEvent = parseAbiItem('event UserOperationEvent(bytes32 indexed userOpHash, address indexed sender, address indexed paymaster, uint256 nonce, bool success, uint256 actualGasCost, uint256 actualGasUsed)')
const operationTopic = toEventSelector(operationEvent)
const executionTopic = toEventSelector('BeforeExecution()')

export type KernelBundledInclusionReader = (attempt: KernelOperationAttempt) => Promise<UserOperationReceipt<'0.9'> | undefined>

export async function readKernelBundledInclusion(client: PublicClient, attempt: KernelOperationAttempt) {
  const { prepared, expectedPaymaster: paymaster } = attempt
  const { entryPoint, userOperationHash: userOpHash, operation: { sender, nonce } } = prepared
  const head = await client.getBlockNumber()
  // The public Sepolia RPC serves at most 1,000 blocks per log request.
  const candidates = await client.getLogs({ address: entryPoint, event: operationEvent,
    args: { userOpHash, sender, paymaster }, fromBlock: head > 999n ? head - 999n : 0n, toBlock: head, strict: true })
  const matches = candidates.filter(log => !log.removed && log.args.nonce === nonce)
  if (matches.length !== 1) return undefined
  const locator = matches[0]
  const [receipt, block] = await Promise.all([
    client.getTransactionReceipt({ hash: locator.transactionHash }),
    client.getBlock({ blockNumber: locator.blockNumber }),
  ])
  if (block.number !== locator.blockNumber || block.hash !== locator.blockHash ||
      receipt.transactionHash !== locator.transactionHash || receipt.blockHash !== block.hash ||
      receipt.blockNumber !== block.number || receipt.status !== 'success') return undefined
  const events = parseEventLogs({ abi: [operationEvent], logs: receipt.logs }).filter(log =>
    isAddressEqual(log.address, entryPoint) && log.args.userOpHash.toLowerCase() === userOpHash.toLowerCase())
  if (events.length !== 1) return undefined
  const event = events[0]
  if (event.logIndex !== locator.logIndex || event.args.nonce !== nonce ||
      !isAddressEqual(event.args.sender, sender) || !isAddressEqual(event.args.paymaster, paymaster)) return undefined
  const boundary = receipt.logs.filter(log => log.logIndex < event.logIndex && isAddressEqual(log.address, entryPoint) &&
    (log.topics[0] === operationTopic || log.topics[0] === executionTopic)).at(-1)
  if (!boundary) return undefined
  const logs = receipt.logs.filter(log => log.logIndex > boundary.logIndex && log.logIndex <= event.logIndex)
  if (logs.some(log => log.removed || log.blockHash !== block.hash || log.blockNumber !== block.number ||
      log.transactionHash !== receipt.transactionHash)) return undefined
  return { entryPoint, sender, paymaster, nonce, userOpHash, receipt, logs,
    actualGasCost: event.args.actualGasCost, actualGasUsed: event.args.actualGasUsed, success: event.args.success }
}
