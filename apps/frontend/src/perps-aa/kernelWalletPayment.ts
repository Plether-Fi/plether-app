import {
  encodeFunctionData, formatEther, isAddressEqual, parseAbi, parseAbiItem, parseEventLogs,
  TransactionNotFoundError, TransactionReceiptNotFoundError, zeroAddress,
  type Address, type Hex, type PublicClient, type TransactionReceipt,
} from 'viem'
import { entryPoint09Abi, toPackedUserOperation, type UserOperation, type UserOperationReceipt } from 'viem/account-abstraction'
import { deadlineNow, refreshDeadlineClock } from './deadlineClock'
import { estimateKernelOwnerTransactionFees } from './kernelEthFunding'
import type { KernelOperationAttempt } from './kernelOperationJournal'
import { assertKernelPreparedOperation, getKernelOperationHash, type KernelActionPreparation, type KernelPreparationIntent, type KernelWalletPreparation } from './kernelRpc'
import { kernelSepoliaCanaryProfile } from './kernelDeploymentPins'

const profile = kernelSepoliaCanaryProfile
const nodeInterface = '0x00000000000000000000000000000000000000C8'
const nodeInterfaceAbi = parseAbi(['function gasEstimateL1Component(address to, bool contractCreation, bytes data) view returns (uint64, uint256, uint256)'])
const operationEvent = parseAbiItem('event UserOperationEvent(bytes32 indexed userOpHash, address indexed sender, address indexed paymaster, uint256 nonce, bool success, uint256 actualGasCost, uint256 actualGasUsed)')
// Leave room for validation and EntryPoint overhead under the Sepolia
// public RPC's two-million-gas estimate ceiling.
const callGasLimit = 1_600_000n
const verificationGasLimit = 650_000n
const maxWalletFee = 50_000_000_000_000_000n
// Use a full-length, high-entropy stub for the unsigned L1 data estimate.
const quoteSignature = '0xdf4588ce88047951a9a6160fd30279580c36f36651904cb0a6bc41513709d93bf2ece53c99363ed2bbdb161d07dd940cab8d328896429925e7967526dcb2c9a871c'

export function encodeKernelWalletTransaction(operation: UserOperation<'0.9'>, ownerAddress: Address, signature: Hex): Hex {
  return encodeFunctionData({
    abi: entryPoint09Abi,
    functionName: 'handleOps',
    args: [[toPackedUserOperation({ ...operation, signature })], ownerAddress],
  })
}

export function assertKernelActionPreparation(prepared: KernelActionPreparation): void {
  if (prepared.transport !== 'wallet') {
    assertKernelPreparedOperation(prepared)
    return
  }
  const { operation } = prepared
  const expectedKeys = ['sender', 'nonce', 'callData', 'factory', 'factoryData', 'callGasLimit',
    'verificationGasLimit', 'preVerificationGas', 'maxFeePerGas', 'maxPriorityFeePerGas', 'signature']
  // Storage is an external input, even though in-memory quotes are typed.
  const { version, mode }: { version: number; mode: string } = prepared
  if (version !== 1 || mode !== 'eth' || !isAddressEqual(prepared.entryPoint, profile.entryPoint) ||
      !/^0x[0-9a-f]{64}$/.test(prepared.preparationId) || !Number.isSafeInteger(prepared.expiresAt) ||
      Object.keys(operation).some(key => !expectedKeys.includes(key)) ||
      operation.signature !== '0x' || operation.nonce < 0n || operation.nonce >> 64n !== 0n ||
      operation.maxFeePerGas !== 0n || operation.maxPriorityFeePerGas !== 0n || operation.preVerificationGas !== 0n ||
      operation.callGasLimit !== callGasLimit || operation.verificationGasLimit !== verificationGasLimit ||
      (operation.factory === undefined) !== (operation.factoryData === undefined) ||
      (operation.factory !== undefined && !isAddressEqual(operation.factory, profile.factory)) ||
      prepared.maxNetworkGasWei <= 0n || prepared.maxNetworkGasWei > maxWalletFee ||
      getKernelOperationHash(operation) !== prepared.userOperationHash) {
    throw new Error('The reviewed wallet transaction is invalid')
  }
}

function requireWalletBalance(balance: bigint, maximum: bigint) {
  if (balance < maximum) {
    throw new Error(`The Owner Wallet needs up to ${formatEther(maximum)} Arbitrum Sepolia ETH for gas. It has ${formatEther(balance)} ETH.`)
  }
}

export async function quoteKernelWalletPayment(
  client: PublicClient, ownerAddress: Address, intent: KernelPreparationIntent,
): Promise<KernelWalletPreparation> {
  const { sender, expectedNonce: nonce, callData, factory, factoryData, preparationId } = intent
  const operation: UserOperation<'0.9'> = {
    sender, nonce, callData, callGasLimit, verificationGasLimit, preVerificationGas: 0n,
    maxFeePerGas: 0n, maxPriorityFeePerGas: 0n, signature: '0x', ...(factory ? { factory, factoryData } : {}),
  }
  const data = encodeKernelWalletTransaction(operation, ownerAddress, quoteSignature)
  const [fees, balance, [l1Gas]] = await Promise.all([
    estimateKernelOwnerTransactionFees(client),
    client.getBalance({ address: ownerAddress }),
    client.readContract({ address: nodeInterface, abi: nodeInterfaceAbi, functionName: 'gasEstimateL1Component',
      args: [profile.entryPoint, false, data], account: ownerAddress }),
    // An existing account can simulate the exact atomic batch as EntryPoint
    // before the wallet signs. A counterfactual account is validated in the signed bundle.
    factory === undefined ? client.call({ account: profile.entryPoint, to: sender, data: callData, gas: callGasLimit }) : undefined,
    refreshDeadlineClock().catch(() => undefined),
  ])
  // Cover both signed gas limits, the EIP-150 forwarding margin, EntryPoint
  // overhead and a change in the L1 posting estimate between the two prompts.
  const gasBudget = (callGasLimit + verificationGasLimit) * 105n / 100n + 250_000n + l1Gas * 3n / 2n
  const maxNetworkGasWei = gasBudget * fees.maxFeePerGas
  const prepared: KernelWalletPreparation = {
    version: 1, transport: 'wallet', mode: 'eth', preparationId, entryPoint: profile.entryPoint,
    operation, userOperationHash: getKernelOperationHash(operation),
    expiresAt: Math.floor(deadlineNow() / 1_000) + 90, maxNetworkGasWei,
  }
  assertKernelActionPreparation(prepared)
  requireWalletBalance(balance, maxNetworkGasWei)
  return prepared
}

export async function estimateSignedKernelWalletPayment(
  client: PublicClient, ownerAddress: Address, prepared: KernelWalletPreparation, signature: Hex,
) {
  const tx = { account: ownerAddress, to: prepared.entryPoint, data: encodeKernelWalletTransaction(prepared.operation, ownerAddress, signature), value: 0n } as const
  const [fees, balance] = await Promise.all([
    estimateKernelOwnerTransactionFees(client),
    client.getBalance({ address: ownerAddress }),
    // Re-simulate deployed inner calls before Prompt 2 because handleOps
    // catches inner reverts and would otherwise estimateGas cleanly.
    prepared.operation.factory === undefined
      ? client.call({ account: prepared.entryPoint, to: prepared.operation.sender, data: prepared.operation.callData, gas: prepared.operation.callGasLimit })
      : undefined,
  ])
  const estimate = await client.estimateGas({ ...tx, ...fees })
  const gas = estimate + (estimate / 10n > 30_000n ? estimate / 10n : 30_000n)
  const maximum = gas * fees.maxFeePerGas
  if (maximum <= 0n || maximum > prepared.maxNetworkGasWei) {
    throw new Error('The wallet gas estimate changed. Review a fresh quote to continue.')
  }
  requireWalletBalance(balance, maximum)
  return { ...tx, ...fees, gas }
}

export interface KernelWalletSubmission {
  fromBlock: bigint
  nonce: number
  transactionHash?: Hex
}

export type KernelWalletOutcome =
  | { state: 'pending' }
  | { state: 'nonce_consumed' }
  | { state: 'wallet_failed'; outerReceipt: TransactionReceipt }
  | { state: 'wallet_replaced' }
  | { state: 'included'; outerReceipt: TransactionReceipt; receipt: UserOperationReceipt<'0.9'> }

export type KernelWalletOutcomeReader = (attempt: KernelOperationAttempt) => Promise<KernelWalletOutcome>

async function readReceipt(client: PublicClient, hash: Hex) {
  try {
    return await client.getTransactionReceipt({ hash })
  } catch (error) {
    if (!(error instanceof TransactionReceiptNotFoundError)) throw error
    return undefined
  }
}

export async function findKernelOwnerTransaction(client: PublicClient, ownerAddress: Address, submission: KernelWalletSubmission, head: bigint) {
  if (head < submission.fromBlock) return undefined
  const readNonce = (blockNumber: bigint) => client.getTransactionCount({ address: ownerAddress, blockNumber })
  if (await readNonce(head) <= submission.nonce) return undefined
  // Locate the first block where the nonce was used without scanning every
  // recently mined Arbitrum block after a wallet disconnect. If the nonce was
  // already consumed before fromBlock (e.g. due to RPC lag), bracket backward.
  let start = submission.fromBlock
  let end = head
  if (start > 0n && await readNonce(start - 1n) > submission.nonce) {
    end = start - 1n
    let step = 16n
    while (start > 0n) {
      const candidate = start > step ? start - step : 0n
      if (await readNonce(candidate) <= submission.nonce) {
        start = candidate
        break
      }
      start = candidate
      if (step >= 4_096n) break
      step *= 2n
    }
  }
  while (start < end) {
    const middle = start + (end - start) / 2n
    if (await readNonce(middle) > submission.nonce) end = middle
    else start = middle + 1n
  }
  const block = await client.getBlock({ blockNumber: start, includeTransactions: true })
  const transaction = block.transactions.find(tx => isAddressEqual(tx.from, ownerAddress) && tx.nonce === submission.nonce)
  return transaction ? await readReceipt(client, transaction.hash) : undefined
}

async function findEntryPointWalletReceipt(
  client: PublicClient,
  entryPoint: Address,
  userOperationHash: Hex,
  sender: Address,
  nonce: bigint,
  fromBlock: bigint,
  headNumber: bigint,
) {
  const liveNonce = await client.readContract({
    address: entryPoint, abi: entryPoint09Abi, functionName: 'getNonce',
    args: [sender, 0n], blockNumber: headNumber,
  })
  if (liveNonce <= nonce) return { receipt: undefined, liveNonce }
  const horizon = headNumber - fromBlock < 4_000n ? fromBlock : headNumber - 3_999n
  for (let end = headNumber; end >= horizon;) {
    const start = end - horizon < 999n ? horizon : end - 999n
    const events = await client.getLogs({
      address: entryPoint, event: operationEvent,
      args: { userOpHash: userOperationHash, sender, paymaster: zeroAddress },
      fromBlock: start, toBlock: end, strict: true,
    })
    const event = events.find(log => log.args.nonce === nonce)
    if (event) {
      return { receipt: await readReceipt(client, event.transactionHash), liveNonce }
    }
    end = start - 1n
  }
  return { receipt: undefined, liveNonce }
}

export async function readKernelWalletOutcome(client: PublicClient, attempt: KernelOperationAttempt): Promise<KernelWalletOutcome> {
  const { prepared, walletSubmission, ownerAddress, signature } = attempt
  if (prepared.transport !== 'wallet' || !walletSubmission) throw new Error('The wallet submission is unavailable')
  assertKernelActionPreparation(prepared)
  const { sender, nonce } = prepared.operation
  const { userOperationHash, entryPoint } = prepared
  const head = await client.getBlock({ blockTag: 'latest' })
  if (head.number < walletSubmission.fromBlock) return { state: 'pending' }
  let receipt = walletSubmission.transactionHash === undefined ? undefined : await readReceipt(client, walletSubmission.transactionHash)
  receipt ??= await findKernelOwnerTransaction(client, ownerAddress, walletSubmission, head.number)
  if (!receipt) {
    // This also recovers a speed-up or a wallet response lost after broadcast.
    const found = await findEntryPointWalletReceipt(client, entryPoint, userOperationHash, sender, nonce, walletSubmission.fromBlock, head.number)
    receipt = found.receipt
    if (!receipt && found.liveNonce > nonce) {
      const canonical = await client.getBlock({ blockNumber: head.number })
      return canonical.hash === head.hash ? { state: 'nonce_consumed' } : { state: 'pending' }
    }
    if (!receipt) return { state: 'pending' }
  }
  const verifyMatchingTransaction = async (candidate: TransactionReceipt): Promise<'matched' | 'mismatched' | 'pending'> => {
    const [transaction, block] = await Promise.all([
      client.getTransaction({ hash: candidate.transactionHash }).catch((error: unknown) => {
        if (error instanceof TransactionNotFoundError) return undefined
        throw error
      }),
      client.getBlock({ blockNumber: candidate.blockNumber }),
    ])
    if (!transaction || block.number !== candidate.blockNumber || block.hash !== candidate.blockHash ||
        transaction.hash !== candidate.transactionHash || transaction.blockNumber !== candidate.blockNumber ||
        transaction.blockHash !== candidate.blockHash) return 'pending'
    if (!transaction.to || !isAddressEqual(transaction.from, ownerAddress) || !isAddressEqual(transaction.to, entryPoint) ||
        transaction.value !== 0n || transaction.input.toLowerCase() !== encodeKernelWalletTransaction(prepared.operation, ownerAddress, signature).toLowerCase()) {
      return transaction.nonce === walletSubmission.nonce && isAddressEqual(transaction.from, ownerAddress) ? 'mismatched' : 'pending'
    }
    return 'matched'
  }
  const matchState = await verifyMatchingTransaction(receipt)
  if (matchState === 'pending') return { state: 'pending' }
  if (matchState === 'mismatched') {
    // Before reporting wallet_replaced, verify whether the UserOperation still
    // executed under a wallet-overridden nonce.
    const fallback = await findEntryPointWalletReceipt(client, entryPoint, userOperationHash, sender, nonce, walletSubmission.fromBlock, head.number)
    if (fallback.receipt && await verifyMatchingTransaction(fallback.receipt) === 'matched') {
      receipt = fallback.receipt
    } else if (fallback.liveNonce > nonce) {
      const canonical = await client.getBlock({ blockNumber: head.number })
      return canonical.hash === head.hash ? { state: 'nonce_consumed' } : { state: 'pending' }
    } else {
      return { state: 'wallet_replaced' }
    }
  }
  if (receipt.status !== 'success') return { state: 'wallet_failed', outerReceipt: receipt }
  const matches = parseEventLogs({ abi: [operationEvent], logs: receipt.logs }).filter(log =>
    isAddressEqual(log.address, entryPoint) && log.args.userOpHash.toLowerCase() === userOperationHash &&
    isAddressEqual(log.args.sender, sender) && isAddressEqual(log.args.paymaster, zeroAddress) &&
    log.args.nonce === nonce && log.args.actualGasCost === 0n)
  const [event] = matches
  if (matches.length !== 1) return { state: 'wallet_failed', outerReceipt: receipt }
  return { state: 'included', outerReceipt: receipt, receipt: {
    actualGasCost: event.args.actualGasCost, actualGasUsed: event.args.actualGasUsed, entryPoint,
    logs: receipt.logs, nonce, receipt, sender, success: event.args.success, userOpHash: userOperationHash,
  } }
}
