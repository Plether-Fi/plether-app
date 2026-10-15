import { orderLifecycleV5Abi } from '@plether-fi/perps-aa-client'
import { encodeFunctionData, getAddress, isAddressEqual, parseAbi, parseEventLogs, type Address, type Hex, type PublicClient, type WalletClient } from 'viem'
import { buildPlaceOrderV3Action } from './orderActionV3'
import { PERMISSIONLESS_SIMPLE_ACCOUNT_V08_FACTORY, PERPS_ENTRY_POINT_V08, type PerpsAaDeploymentManifest } from './manifest'
import { PERPS_ORDER_ROUTER_ABI, PERPS_ORDER_LIFECYCLE_BOOK_ABI, PERPS_POSITION_PROTECTION_BOOK_ABI } from '../contracts/abis'
import { verifyPerpsV3DeploymentBindings } from '../contracts/verifyPerpsV3Bindings'
import type { PerpsOrderRequestV3 } from '../contracts/perpsOrderV3'

const accountAbi = parseAbi(['function owner() view returns (address)', 'function entryPoint() view returns (address)', 'function execute(address dest,uint256 value,bytes func)'])
const factoryAbi = parseAbi(['function getAddress(address owner,uint256 salt) view returns (address)', 'function accountImplementation() view returns (address)'])
const oracleReadAbi = parseAbi(['function pythFeedIds(uint256 index) view returns (bytes32)', 'function isOracleFrozen() view returns (bool)', 'function getOrderExecutionFee(bytes[] data) view returns (uint256)', 'function orderSettlementWindow() view returns (uint256)'])
const routerReadAbi = parseAbi(['function nextExecuteId() view returns (uint64)', 'function pletherOracle() view returns (address)'])
const implementationSlot = '0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc' as Hex
export interface NativeCloseContext { client: PublicClient; wallet: WalletClient; manifest: PerpsAaDeploymentManifest; account: Address; owner: Address }

async function requireCanonicalInclusion(client: PublicClient, receipt: { blockNumber: bigint; blockHash: Hex }) {
  const block = await client.getBlock({ blockNumber: receipt.blockNumber })
  if (block.hash !== receipt.blockHash) throw new Error('Transaction inclusion changed. Reconcile the canonical order before retrying.')
}

function requireOwnerWallet({ wallet, client, manifest, owner }: NativeCloseContext) {
  if (!wallet.account || !isAddressEqual(wallet.account.address, owner) || wallet.chain?.id !== manifest.chainId || client.chain?.id !== manifest.chainId) throw new Error('Connect the Trading Account owner wallet on the reviewed chain. Native gas is paid by this wallet.')
  return wallet.account
}

async function verifyOwnerAccount(context: NativeCloseContext) {
  const { client, manifest, account, owner } = context
  const signer = requireOwnerWallet(context)
  // The manifest parser admits only the permissionless SimpleAccount v0.8 profile.
  if (!isAddressEqual(manifest.smartAccountFactory, PERMISSIONLESS_SIMPLE_ACCOUNT_V08_FACTORY) || !isAddressEqual(manifest.entryPoint, PERPS_ENTRY_POINT_V08)) throw new Error('Unsupported Trading Account implementation.')
  const { blockNumber } = await verifyPerpsV3DeploymentBindings(client, manifest)
  const [predicted, actualOwner, entryPoint, implementation, slot, code] = await Promise.all([
    client.readContract({ address: manifest.smartAccountFactory, abi: factoryAbi, functionName: 'getAddress', args: [owner, BigInt(manifest.smartAccountIndex)], blockNumber }),
    client.readContract({ address: account, abi: accountAbi, functionName: 'owner', blockNumber }),
    client.readContract({ address: account, abi: accountAbi, functionName: 'entryPoint', blockNumber }),
    client.readContract({ address: manifest.smartAccountFactory, abi: factoryAbi, functionName: 'accountImplementation', blockNumber }),
    client.getStorageAt({ address: account, slot: implementationSlot, blockNumber }),
    client.getCode({ address: account, blockNumber }),
  ])
  if (!code || code === '0x' || !slot || !isAddressEqual(getAddress(`0x${slot.slice(-40)}`), implementation) || !isAddressEqual(predicted, account) || !isAddressEqual(actualOwner, owner) || !isAddressEqual(entryPoint, manifest.entryPoint)) throw new Error('The deployed Trading Account does not match its reviewed owner, factory, or implementation.')
  return signer
}

/** Direct owner execution preserves the account at the Router, without a sponsor or bundler. */
export async function commitNativeClose(context: NativeCloseContext, request: PerpsOrderRequestV3) {
  const { client, wallet, manifest, account } = context
  if (!request.isClose) throw new Error('Native account fallback is restricted to closes.')
  const signer = await verifyOwnerAccount(context)
  const call = buildPlaceOrderV3Action({ account, orderRouter: manifest.orderRouter, request }).calls[0]
  const simulation = await client.simulateContract({ account: signer, address: account, abi: accountAbi, functionName: 'execute', args: [call.to, call.value, call.data] })
  const hash = await wallet.writeContract({ ...simulation.request, account: signer, chain: wallet.chain })
  const receipt = await client.waitForTransactionReceipt({ hash })
  if (receipt.status !== 'success') throw new Error(`Close commitment reverted: ${hash}`)
  await requireCanonicalInclusion(client, receipt)
  const [resolution, orderId] = await client.readContract({ address: manifest.orderLifecycleBook, abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI, functionName: 'resolveClientIntent', args: [account, request], blockNumber: receipt.blockNumber })
  if (resolution !== 1 || orderId === 0n) throw new Error(`Close receipt needs reconciliation: ${hash}`)
  return { hash, orderId, account, clientOrderId: request.clientOrderId, replayed: false }
}

/** Independent historical source; latest prices cannot replace a committed price window. */
export async function historicalClosePayload(client: PublicClient, oracle: Address, min: bigint, max: bigint, fetcher: typeof fetch = fetch): Promise<Hex[]> {
  const ids = await Promise.all(Array.from({ length: 6 }, (_, i) => client.readContract({ address: oracle, abi: oracleReadAbi, functionName: 'pythFeedIds', args: [BigInt(i)] })))
  const url = new URL(`https://hermes.pyth.network/v2/updates/price/${min.toString()}`)
  url.searchParams.set('encoding', 'hex')
  url.searchParams.set('parsed', 'true')
  ids.forEach(id => { url.searchParams.append('ids[]', id.slice(2)) })
  const response = await fetcher(url, { signal: AbortSignal.timeout(15_000) })
  if (!response.ok) throw new Error('Historical oracle data is unavailable. Retry, or clean up after expiry.')
  const body = await response.json() as { binary?: { data?: string[] }; parsed?: { id: string; price: { publish_time: number } }[] }
  if (body.parsed?.length !== ids.length || ids.some(id => !body.parsed?.some(p => `0x${p.id}`.toLowerCase() === id.toLowerCase() && BigInt(p.price.publish_time) >= min && BigInt(p.price.publish_time) <= max)) || !body.binary?.data?.length || body.binary.data.some(data => !/^(?:[0-9a-f]{2})+$/i.test(data))) throw new Error('Historical oracle payload is incomplete or outside the order window.')
  return body.binary.data.map<Hex>(data => `0x${data}`)
}

export class NativeQueueBlockedError extends Error {
  readonly orderId: bigint
  constructor(orderId: bigint) { super(`Order ${orderId.toString()} is ahead in the FIFO queue. It must execute or be cleaned up first.`); this.orderId = orderId }
}

export async function executeNativeOrder(context: NativeCloseContext, orderId: bigint, cleanupOnly = false) {
  const { client, wallet, manifest } = context
  const signer = requireOwnerWallet(context)
  await verifyPerpsV3DeploymentBindings(client, manifest)
  const block = await client.getBlock({ blockTag: 'latest' })
  const timing = await client.readContract({ address: manifest.orderLifecycleBook, abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI, functionName: 'orderTiming', args: [orderId], blockNumber: block.number })
  if (timing.executionDeadline === 0n) throw new Error('This order is no longer pending. Refresh its authenticated result.')
  let data: Hex[] = []
  let value = 0n
  const expired = block.timestamp > timing.executionDeadline
  if (cleanupOnly && !expired) throw new Error('Cleanup is available strictly after the execution deadline.')
  if (!expired) {
    const head = await client.readContract({ address: manifest.orderRouter, abi: routerReadAbi, functionName: 'nextExecuteId', blockNumber: block.number })
    if (head !== orderId) throw new NativeQueueBlockedError(head)
    let cleanupReady = false
    try {
      const attempt = await client.simulateContract({ account: signer, address: manifest.orderRouter, abi: PERPS_ORDER_ROUTER_ABI, functionName: 'executeOrder', args: [orderId, []] })
      cleanupReady = attempt.result.status === 2 || attempt.result.status === 3
    } catch { /* Live orders normally require historical price bytes. */ }
    if (!cleanupReady) {
      const oracle = await client.readContract({ address: manifest.orderRouter, abi: routerReadAbi, functionName: 'pletherOracle' })
      const frozen = await client.readContract({ address: oracle, abi: oracleReadAbi, functionName: 'isOracleFrozen' })
      if (!frozen) {
        const window = await client.readContract({ address: oracle, abi: oracleReadAbi, functionName: 'orderSettlementWindow' })
        data = await historicalClosePayload(client, oracle, timing.commitTimestamp + 1n, timing.commitTimestamp + window)
      }
      value = await client.readContract({ address: oracle, abi: oracleReadAbi, functionName: 'getOrderExecutionFee', args: [data] })
    }
  }
  const calldata = expired ? encodeFunctionData({ abi: PERPS_ORDER_ROUTER_ABI, functionName: 'expireOrder', args: [orderId] }) : encodeFunctionData({ abi: PERPS_ORDER_ROUTER_ABI, functionName: 'executeOrder', args: [orderId, data] })
  await client.call({ account: signer, to: manifest.orderRouter, data: calldata, value })
  const hash = await wallet.sendTransaction({ account: signer, chain: wallet.chain, to: manifest.orderRouter, data: calldata, value })
  const transaction = await client.waitForTransactionReceipt({ hash })
  if (transaction.status !== 'success') throw new Error(`Order attempt reverted: ${hash}`)
  await requireCanonicalInclusion(client, transaction)
  const events = parseEventLogs({ abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI, logs: transaction.logs.filter(log => isAddressEqual(log.address, manifest.orderLifecycleBook)), eventName: 'OrderFinalized', strict: true })
  const event = events.find(event => event.args.orderId === orderId)
  if (!event) throw new Error(`Order remains pending after ${hash}. Refresh before retrying.`)
  const outcome = await client.readContract({ address: manifest.orderLifecycleBook, abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI, functionName: 'terminalOutcome', args: [orderId], blockNumber: transaction.blockNumber })
  if (outcome.receiptHash !== event.args.receiptHash || !isAddressEqual(outcome.account, event.args.account)) throw new Error(`Order receipt does not authenticate: ${hash}`)
  const verified = await client.readContract({ address: manifest.orderLifecycleBook, abi: orderLifecycleV5Abi, functionName: 'verifyReceipt', args: [event.args.receipt, event.args.terminalTime], blockNumber: transaction.blockNumber })
  if (!verified) throw new Error(`Order receipt failed verification: ${hash}`)
  return { hash, executionPrice: event.args.receipt.executionPrice, failedReason: event.args.receipt.status === 3 ? event.args.receipt.reason : undefined }
}

/** Explicit fallback for a user's existing protection; on-chain lifecycle rules remain authoritative. */
export async function nativeProtectionAction(context: NativeCloseContext, protectionId: bigint, action: 'cancel' | 'retry') {
  const { client, wallet, manifest, account } = context
  const signer = await verifyOwnerAccount(context)
  const protection = await client.readContract({ address: manifest.positionProtectionBook, abi: PERPS_POSITION_PROTECTION_BOOK_ABI, functionName: 'getPositionProtection', args: [protectionId] })
  if (!isAddressEqual(protection.account, account)) throw new Error('Protection belongs to a different Trading Account.')
  let hash: Hex
  if (action === 'cancel') {
    const data = encodeFunctionData({ abi: PERPS_POSITION_PROTECTION_BOOK_ABI, functionName: 'cancelPositionProtection', args: [protectionId] })
    const simulation = await client.simulateContract({ account: signer, address: account, abi: accountAbi, functionName: 'execute', args: [manifest.positionProtectionBook, 0n, data] })
    hash = await wallet.writeContract({ ...simulation.request, account: signer, chain: wallet.chain })
  } else {
    const simulation = await client.simulateContract({ account: signer, address: manifest.positionProtectionBook, abi: PERPS_POSITION_PROTECTION_BOOK_ABI, functionName: 'retryPositionProtectionClose', args: [protectionId] })
    hash = await wallet.writeContract({ ...simulation.request, account: signer, chain: wallet.chain })
  }
  const receipt = await client.waitForTransactionReceipt({ hash })
  if (receipt.status !== 'success') throw new Error(`Protection action reverted: ${hash}`)
  await requireCanonicalInclusion(client, receipt)
  const after = await client.readContract({ address: manifest.positionProtectionBook, abi: PERPS_POSITION_PROTECTION_BOOK_ABI, functionName: 'getPositionProtection', args: [protectionId], blockNumber: receipt.blockNumber })
  if (!isAddressEqual(after.account, account) || (action === 'cancel' ? after.status !== 6 : after.status !== 3 || after.linkedOrderId === 0n || after.linkedOrderId === protection.linkedOrderId)) throw new Error(`Protection action needs reconciliation: ${hash}`)
  return { hash }
}
