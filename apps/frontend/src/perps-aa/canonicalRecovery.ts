import { decodeEventLog, isAddressEqual, parseAbiItem, toEventSelector, type Address, type Hex, type PublicClient } from 'viem'
import type { RecoveryOperationContext, SponsoredOperationRecoverySnapshot } from './runtimeContext'

const eventAbi = [parseAbiItem('event UserOperationEvent(bytes32 indexed userOpHash, address indexed sender, address indexed paymaster, uint256 nonce, bool success, uint256 actualGasCost, uint256 actualGasUsed)')]

/** A transaction pointer is never proof of the operation's identity or success. */
export async function recoverCanonicalInclusion(input: {
  client: PublicClient
  chainId: number
  entryPoint: Address
  sender: Address
  operationHash: Hex
  context: RecoveryOperationContext & { transactionHash: Hex }
  safeBlock: { number: bigint; hash: Hex; timestamp: bigint }
}): Promise<SponsoredOperationRecoverySnapshot['userOperationEvidence']> {
  const { client, context, safeBlock } = input
  if (context.nonce === undefined) throw new Error('Recovery requires a verified operation nonce')
  const [chainId, receipt] = await Promise.all([
    client.getChainId(),
    client.getTransactionReceipt({ hash: context.transactionHash }),
  ])
  if (chainId !== input.chainId || receipt.status !== 'success' ||
      receipt.transactionHash.toLowerCase() !== context.transactionHash.toLowerCase()) {
    throw new Error('Recovery transaction identity mismatch')
  }
  const candidates = receipt.logs.filter(log => isAddressEqual(log.address, input.entryPoint) &&
    log.topics[1]?.toLowerCase() === input.operationHash.toLowerCase() &&
    log.topics[0]?.toLowerCase() === toEventSelector(eventAbi[0]))
  if (candidates.length !== 1) throw new Error('Recovery event is missing or ambiguous')
  const log = candidates[0]
  const { args } = decodeEventLog({ abi: eventAbi, data: log.data, topics: log.topics, strict: true })
  if (!isAddressEqual(args.sender, input.sender) ||
      args.nonce !== context.nonce ||
      (context.paymaster !== undefined && !isAddressEqual(args.paymaster, context.paymaster)) ||
      log.removed || log.blockNumber !== receipt.blockNumber ||
      log.blockHash.toLowerCase() !== receipt.blockHash.toLowerCase() ||
      log.transactionHash.toLowerCase() !== receipt.transactionHash.toLowerCase()) {
    throw new Error('Recovery event identity mismatch')
  }
  const [canonical, safeAgain, currentSafe] = await Promise.all([
    client.getBlock({ blockNumber: receipt.blockNumber }),
    client.getBlock({ blockNumber: safeBlock.number }),
    client.getBlock({ blockTag: 'safe' }),
  ])
  if (canonical.number !== receipt.blockNumber || safeAgain.number !== safeBlock.number ||
      canonical.hash.toLowerCase() !== receipt.blockHash.toLowerCase() ||
      safeAgain.hash.toLowerCase() !== safeBlock.hash.toLowerCase() ||
      currentSafe.number < safeBlock.number || currentSafe.timestamp < safeBlock.timestamp ||
      (currentSafe.number === safeBlock.number && currentSafe.hash.toLowerCase() !== safeBlock.hash.toLowerCase())) {
    throw new Error('Recovery canonical boundary changed')
  }
  return receipt.blockNumber > safeBlock.number
    ? { kind: 'not-safe-yet' }
    : { kind: 'included', success: args.success, transactionHash: receipt.transactionHash, blockNumber: receipt.blockNumber }
}
