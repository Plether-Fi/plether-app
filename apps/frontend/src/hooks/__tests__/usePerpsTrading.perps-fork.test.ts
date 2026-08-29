import { afterAll, beforeAll, describe, expect, it } from 'vitest'
import { spawn, type ChildProcessWithoutNullStreams } from 'node:child_process'
import {
  createPublicClient,
  createTestClient,
  createWalletClient,
  http,
  parseEther,
  parseEventLogs,
  parseUnits,
  type Address,
  type Hex,
  type PublicClient,
  type TestClient,
  type WalletClient,
} from 'viem'
import { privateKeyToAccount, type PrivateKeyAccount } from 'viem/accounts'
import { arbitrumSepolia } from 'viem/chains'
import {
  ERC20_ABI,
  PERPS_CFD_ENGINE_ABI,
  PERPS_CFD_ENGINE_LENS_ABI,
  PERPS_MARGIN_CLEARINGHOUSE_ABI,
  PERPS_ORDER_ROUTER_ABI,
  PERPS_PLETHER_ORACLE_ABI,
  PERPS_PUBLIC_LENS_ABI,
} from '../../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA } from '../../contracts/perpsAddresses'
import { formatPerpsUsdc, getPerpsTargetPrice, notionalUsdcToSizeDelta, sizeDeltaToNotionalUsdc } from '../../utils/perps'
import { getPerpsErrorMessage, getPerpsOrderFailureMessage } from '../../utils/perpsErrors'
import { resolvePerpsSizeDelta } from '../../utils/perpsOrder'

interface BackendPythUpdateResponse {
  data?: {
    updateData?: string[]
    fetchedAt?: number
    publishTimes?: number[]
  }
  error?: {
    message?: string
  }
}

interface ForkHarness {
  account: PrivateKeyAccount
  publicClient: PublicClient
  testClient: TestClient
  walletClient: WalletClient
}

interface PositionView {
  exists: boolean
  side: number
  size: bigint
  entryPrice: bigint
  marginUsdc: bigint
}

interface CommitAndExecuteResult {
  orderId: bigint
  commitHash: Hex
  executeHash: Hex
  executionPrice: bigint
  failedReason?: number
}

// The public manifest still points at the pre-V2 deployment. Keep its fork
// coverage executable until the V2 deployment addresses are published; the
// bounded V2 request path is covered by unit tests in the meantime.
const LEGACY_COMMIT_ORDER_ABI = [{
  type: 'function',
  name: 'commitOrder',
  stateMutability: 'nonpayable',
  inputs: [
    { name: 'side', type: 'uint8' },
    { name: 'sizeDelta', type: 'uint256' },
    { name: 'marginDelta', type: 'uint256' },
    { name: 'targetPrice', type: 'uint256' },
    { name: 'isClose', type: 'bool' },
  ],
  outputs: [],
}] as const
const LEGACY_ORDER_TERMINAL_EVENTS_ABI = [
  {
    type: 'event',
    name: 'OrderCommitted',
    inputs: [
      { name: 'orderId', type: 'uint64', indexed: true },
      { name: 'account', type: 'address', indexed: true },
      { name: 'side', type: 'uint8', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'OrderExecuted',
    inputs: [
      { name: 'orderId', type: 'uint64', indexed: true },
      { name: 'executionPrice', type: 'uint256', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'OrderFailed',
    inputs: [
      { name: 'orderId', type: 'uint64', indexed: true },
      { name: 'reason', type: 'uint8', indexed: false },
    ],
  },
] as const
const SIDE_LONG = 0
const SIDE_SHORT = 1
const USDC = 1_000_000n
const DEFAULT_OPEN_NOTIONAL_USDC = parseUnits(process.env.PERPS_FORK_TEST_NOTIONAL_USDC ?? '1000', 6)
const DEFAULT_LEVERAGE = BigInt(process.env.PERPS_FORK_TEST_LEVERAGE ?? '5')
const FORK_TEST_SLIPPAGE_PERCENT = Number(process.env.PERPS_FORK_TEST_SLIPPAGE_PERCENT ?? '5')
const forkUrl = process.env.ARB_SEPOLIA_RPC_URL ?? process.env.ARBITRUM_SEPOLIA_RPC_URL
const rawPrivateKey = process.env.TEST_PRIVATE_KEY ?? process.env.PRIVATE_KEY
const backendApiUrl = process.env.PERPS_FORK_API_URL ?? process.env.VITE_API_URL ?? 'http://127.0.0.1:3001/api'
const anvilPort = Number(process.env.PERPS_FORK_ANVIL_PORT ?? '18546')
const forkBlockLag = BigInt(process.env.PERPS_FORK_BLOCK_LAG ?? '180')
const anvilUrl = `http://127.0.0.1:${anvilPort}`
const shouldRunForkTest = Boolean(forkUrl && rawPrivateKey)

let anvil: ChildProcessWithoutNullStreams | undefined
let anvilStartError: Error | undefined

function normalizePrivateKey(value: string): Hex {
  return value.startsWith('0x') ? value as Hex : `0x${value}` as Hex
}

function skipForkTest(ctx: { skip: () => void }, reason: string): void {
  console.warn(`[perps-fork] skipped: ${reason}`)
  ctx.skip()
}

function tupleValue(value: unknown, index: number, key: string): unknown {
  if (value && typeof value === 'object' && key in value) {
    return (value as Record<string, unknown>)[key]
  }
  if (Array.isArray(value)) return value[index]
  return undefined
}

function readBigInt(value: unknown, index: number, key: string): bigint {
  const item = tupleValue(value, index, key)
  if (typeof item === 'bigint') return item
  if (typeof item === 'number') return BigInt(item)
  if (typeof item === 'string') return BigInt(item)
  return 0n
}

function readBoolean(value: unknown, index: number, key: string): boolean {
  return Boolean(tupleValue(value, index, key))
}

function delay(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms))
}

function ceilDiv(value: bigint, divisor: bigint): bigint {
  return (value + divisor - 1n) / divisor
}

async function anvilRpc<T = unknown>(method: string, params: unknown[] = []): Promise<T> {
  const response = await fetch(anvilUrl, {
    method: 'POST',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ jsonrpc: '2.0', id: 1, method, params }),
  })
  const payload = await response.json() as { result?: T; error?: { message?: string } }
  if (payload.error) throw new Error(payload.error.message ?? `${method} failed`)
  return payload.result as T
}

async function withForkSnapshot<T>(callback: () => Promise<T>): Promise<T> {
  const snapshotId = await anvilRpc<string>('evm_snapshot')
  try {
    return await callback()
  } finally {
    await anvilRpc<boolean>('evm_revert', [snapshotId])
  }
}

async function waitForAnvil(): Promise<void> {
  const startedAt = Date.now()
  while (Date.now() - startedAt < 30_000) {
    if (anvilStartError) {
      throw new Error(`Could not start Anvil: ${anvilStartError.message}`)
    }
    if (anvil?.exitCode !== null) {
      throw new Error(`Anvil exited before becoming ready with code ${anvil?.exitCode}`)
    }

    try {
      const response = await fetch(anvilUrl, {
        method: 'POST',
        headers: { 'content-type': 'application/json' },
        body: JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'eth_chainId', params: [] }),
      })
      if (response.ok) return
    } catch {
      // Keep polling until the fork is ready.
    }

    await delay(500)
  }

  throw new Error('Timed out waiting for Anvil fork to start')
}

async function getRpcBlockNumber(rpcUrl: string): Promise<bigint> {
  const chainId = await getRpcChainId(rpcUrl)
  if (chainId !== BigInt(arbitrumSepolia.id)) {
    throw new Error(
      `Perps fork tests require an Arbitrum Sepolia RPC (chain ${arbitrumSepolia.id}), but the provided RPC is chain ${chainId.toString()}. Use an arb-sepolia RPC URL, not eth-sepolia.`
    )
  }

  let response: Response
  try {
    response = await fetch(rpcUrl, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'eth_blockNumber', params: [] }),
    })
  } catch (error) {
    throw new Error(`Could not reach Arbitrum Sepolia RPC for fork setup: ${error instanceof Error ? error.message : String(error)}`)
  }
  const payload = await response.json().catch(() => undefined) as { result?: string; error?: { message?: string } } | undefined
  if (!response.ok || payload?.error) {
    throw new Error(payload?.error?.message ?? `Arbitrum Sepolia RPC returned HTTP ${response.status}`)
  }
  if (!payload.result) throw new Error('Could not read Arbitrum Sepolia block number for fork setup')
  return BigInt(payload.result)
}

async function getRpcChainId(rpcUrl: string): Promise<bigint> {
  let response: Response
  try {
    response = await fetch(rpcUrl, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'eth_chainId', params: [] }),
    })
  } catch (error) {
    throw new Error(`Could not reach RPC for fork setup: ${error instanceof Error ? error.message : String(error)}`)
  }
  const payload = await response.json().catch(() => undefined) as { result?: string; error?: { message?: string } } | undefined
  if (!response.ok || payload?.error) {
    throw new Error(payload?.error?.message ?? `RPC returned HTTP ${response.status}`)
  }
  if (!payload?.result) throw new Error('Could not read RPC chain id for fork setup')
  return BigInt(payload.result)
}

async function fetchBackendPythPayload(publishTime?: number): Promise<{ updateData: Hex[]; publishTimes: number[] }> {
  const requestUrl = new URL(`${backendApiUrl.replace(/\/$/, '')}/perps/pyth/update`)
  if (publishTime !== undefined) requestUrl.searchParams.set('publishTime', String(publishTime))

  const response = await fetch(requestUrl)
  const payload = await response.json().catch(() => undefined) as BackendPythUpdateResponse | undefined
  if (!response.ok) {
    throw new Error(payload?.error?.message ?? `HTTP ${response.status}`)
  }

  const updateData = payload?.data?.updateData
  const publishTimes = payload?.data?.publishTimes ?? []
  if (!updateData?.length || !publishTimes.length) {
    throw new Error(`backend returned publish times ${publishTimes.join(', ') || '<empty>'}`)
  }

  return {
    updateData: updateData.map((item) => item.startsWith('0x') ? item as Hex : `0x${item}` as Hex),
    publishTimes,
  }
}

async function fetchHistoricalPythPayloadAfter(minPublishTime: bigint): Promise<{ updateData: Hex[]; publishTimes: number[] }> {
  let lastError = ''

  for (let attempt = 0; attempt < 12; attempt += 1) {
    try {
      const latest = await fetchBackendPythPayload()
      const earliest = BigInt(Math.min(...latest.publishTimes))
      if (earliest > minPublishTime) {
        return await fetchBackendPythPayload(Number(earliest))
      }
      lastError = `latest publish time ${earliest.toString()} was not after ${minPublishTime.toString()}`
    } catch (error) {
      lastError = error instanceof Error ? error.message : String(error)
      if (lastError.toLowerCase().includes('rate limit')) {
        throw new Error(`Backend Pyth update request failed: ${lastError}`)
      }
    }

    await delay(1_000)
  }

  throw new Error(`No historical backend Pyth update data found after ${minPublishTime.toString()}. Last error: ${lastError}`)
}

function createHarness(): ForkHarness {
  const account = privateKeyToAccount(normalizePrivateKey(rawPrivateKey!))
  const transport = http(anvilUrl)
  return {
    account,
    publicClient: createPublicClient({ chain: arbitrumSepolia, transport }),
    testClient: createTestClient({ chain: arbitrumSepolia, mode: 'anvil', transport }),
    walletClient: createWalletClient({ account, chain: arbitrumSepolia, transport }),
  }
}

async function readProtocolPrice({ publicClient }: ForkHarness): Promise<bigint> {
  const status = await publicClient.readContract({
    address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
    abi: PERPS_PUBLIC_LENS_ABI,
    functionName: 'getProtocolStatus',
  })
  return readBigInt(status, 1, 'lastMarkPrice')
}

async function readPosition({ account, publicClient }: ForkHarness): Promise<PositionView> {
  const position = await publicClient.readContract({
    address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
    abi: PERPS_PUBLIC_LENS_ABI,
    functionName: 'getPosition',
    args: [account.address as Address],
  })
  return {
    exists: readBoolean(position, 0, 'exists'),
    side: Number(tupleValue(position, 1, 'side') ?? 0),
    size: readBigInt(position, 2, 'size'),
    entryPrice: readBigInt(position, 3, 'entryPrice'),
    marginUsdc: readBigInt(position, 4, 'marginUsdc'),
  }
}

async function readPendingOrderCount({ account, publicClient }: ForkHarness): Promise<number> {
  const pendingOrders = await publicClient.readContract({
    address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
    abi: PERPS_PUBLIC_LENS_ABI,
    functionName: 'getPendingOrders',
    args: [account.address as Address],
  })
  return Array.isArray(pendingOrders) ? pendingOrders.length : 0
}

async function readFreeMargin({ account, publicClient }: ForkHarness): Promise<bigint> {
  const accountView = await publicClient.readContract({
    address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
    abi: PERPS_PUBLIC_LENS_ABI,
    functionName: 'getTraderAccount',
    args: [account.address as Address],
  })
  return readBigInt(accountView, 1, 'withdrawableUsdc')
}

async function readEngineMinimumOpenNotional({ publicClient }: ForkHarness): Promise<bigint> {
  const riskParams = await publicClient.readContract({
    address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
    abi: PERPS_CFD_ENGINE_ABI,
    functionName: 'riskParams',
  })
  const minBountyUsdc = readBigInt(riskParams, 6, 'minBountyUsdc')
  const bountyBps = readBigInt(riskParams, 7, 'bountyBps')
  if (minBountyUsdc <= 0n || bountyBps <= 0n) return DEFAULT_OPEN_NOTIONAL_USDC

  return ceilDiv(minBountyUsdc * 10_000n, bountyBps)
}

async function readMinimumOpenNotional(harness: ForkHarness): Promise<bigint> {
  const minNotional = await readEngineMinimumOpenNotional(harness) + USDC
  return minNotional > DEFAULT_OPEN_NOTIONAL_USDC ? minNotional : DEFAULT_OPEN_NOTIONAL_USDC
}

async function ensureEth(harness: ForkHarness): Promise<void> {
  await harness.testClient.setBalance({ address: harness.account.address, value: parseEther('10') })
}

async function ensureMargin(harness: ForkHarness, requiredFreeMargin: bigint): Promise<boolean> {
  const currentFreeMargin = await readFreeMargin(harness)
  if (currentFreeMargin >= requiredFreeMargin) return true

  const missing = requiredFreeMargin - currentFreeMargin
  const walletUsdc = await harness.publicClient.readContract({
    address: PERPS_ARBITRUM_SEPOLIA.usdc,
    abi: ERC20_ABI,
    functionName: 'balanceOf',
    args: [harness.account.address as Address],
  })
  if (walletUsdc < missing) {
    console.warn(
      `[perps-fork] skipped: wallet has ${formatPerpsUsdc(walletUsdc)} USDC, but needs ${formatPerpsUsdc(missing)} more USDC to reach required free margin.`
    )
    return false
  }

  await harness.walletClient.writeContract({
    address: PERPS_ARBITRUM_SEPOLIA.usdc,
    abi: ERC20_ABI,
    functionName: 'approve',
    args: [PERPS_ARBITRUM_SEPOLIA.marginClearinghouse, missing],
  })
  await harness.walletClient.writeContract({
    address: PERPS_ARBITRUM_SEPOLIA.marginClearinghouse,
    abi: PERPS_MARGIN_CLEARINGHOUSE_ABI,
    functionName: 'depositMargin',
    args: [missing],
  })
  return (await readFreeMargin(harness)) >= requiredFreeMargin
}

async function previewOpenIsValid(
  harness: ForkHarness,
  side: number,
  sizeDelta: bigint,
  marginDelta: bigint,
  oraclePrice: bigint
): Promise<boolean> {
  const latestBlock = await harness.publicClient.getBlock({ blockTag: 'latest' })
  const code = await harness.publicClient.readContract({
    address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens,
    abi: PERPS_CFD_ENGINE_LENS_ABI,
    functionName: 'previewOpenRevertCode',
    args: [harness.account.address as Address, side, sizeDelta, marginDelta, oraclePrice, latestBlock.timestamp],
  })
  if (code !== 0) {
    console.warn(
      `[perps-fork] skipped: previewOpenRevertCode returned ${code.toString()} for ${side === SIDE_SHORT ? 'short' : 'long'} open.`
    )
  }
  return code === 0
}

async function commitOrder(
  harness: ForkHarness,
  side: number,
  sizeDelta: bigint,
  marginDelta: bigint,
  targetPrice: bigint,
  isClose: boolean
): Promise<{ orderId: bigint; hash: Hex; commitTime: bigint }> {
  const hash = await harness.walletClient.writeContract({
    address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
    abi: LEGACY_COMMIT_ORDER_ABI,
    functionName: 'commitOrder',
    args: [side, sizeDelta, marginDelta, targetPrice, isClose],
  })
  const receipt = await harness.publicClient.waitForTransactionReceipt({ hash })
  expect(receipt.status).toBe('success')

  const [committed] = parseEventLogs({
    abi: LEGACY_ORDER_TERMINAL_EVENTS_ABI,
    eventName: 'OrderCommitted',
    logs: receipt.logs,
  }).filter((event) => event.args.account.toLowerCase() === harness.account.address.toLowerCase())
  expect(committed?.args.orderId).toBeDefined()
  const orderId = committed!.args.orderId

  const pendingOrderView = await harness.publicClient.readContract({
    address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
    abi: PERPS_ORDER_ROUTER_ABI,
    functionName: 'getPendingOrderView',
    args: [orderId],
  })
  const pending = tupleValue(pendingOrderView, 0, 'pending')
  const commitTime = readBigInt(pending, 6, 'commitTime')
  expect(commitTime).toBeGreaterThan(0n)

  return { orderId, hash, commitTime }
}

async function executeOrder(
  harness: ForkHarness,
  orderId: bigint,
  pythPayload: { updateData: Hex[]; publishTimes: number[] }
): Promise<{ hash: Hex; executionPrice: bigint; failedReason?: number }> {
  const earliestPublishTime = BigInt(Math.min(...pythPayload.publishTimes))
  await harness.testClient.setNextBlockTimestamp({ timestamp: earliestPublishTime })
  const updateFee = await harness.publicClient.readContract({
    address: PERPS_ARBITRUM_SEPOLIA.pletherOracle,
    abi: PERPS_PLETHER_ORACLE_ABI,
    functionName: 'getUpdateFee',
    args: [pythPayload.updateData],
  })
  const hash = await harness.walletClient.writeContract({
    address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
    abi: PERPS_ORDER_ROUTER_ABI,
    functionName: 'executeOrder',
    args: [orderId, pythPayload.updateData],
    value: updateFee,
  })
  const receipt = await harness.publicClient.waitForTransactionReceipt({ hash })
  expect(receipt.status).toBe('success')

  const [executed] = parseEventLogs({
    abi: LEGACY_ORDER_TERMINAL_EVENTS_ABI,
    eventName: 'OrderExecuted',
    logs: receipt.logs,
  }).filter((event) => event.args.orderId === orderId)
  const [failed] = parseEventLogs({
    abi: LEGACY_ORDER_TERMINAL_EVENTS_ABI,
    eventName: 'OrderFailed',
    logs: receipt.logs,
  }).filter((event) => event.args.orderId === orderId)

  expect(failed, failed ? getPerpsOrderFailureMessage(Number(failed.args.reason)) : undefined).toBeUndefined()
  expect(executed?.args.executionPrice).toBeGreaterThan(0n)

  return {
    hash,
    executionPrice: executed!.args.executionPrice,
    failedReason: failed?.args.reason === undefined ? undefined : Number(failed.args.reason),
  }
}

async function executeOrderBatch(
  harness: ForkHarness,
  maxOrderId: bigint,
  pythPayload: { updateData: Hex[]; publishTimes: number[] },
  expectedOrderIds: bigint[]
): Promise<{ hash: Hex; executionPrices: Map<bigint, bigint> }> {
  const earliestPublishTime = BigInt(Math.min(...pythPayload.publishTimes))
  await harness.testClient.setNextBlockTimestamp({ timestamp: earliestPublishTime })
  const updateFee = await harness.publicClient.readContract({
    address: PERPS_ARBITRUM_SEPOLIA.pletherOracle,
    abi: PERPS_PLETHER_ORACLE_ABI,
    functionName: 'getUpdateFee',
    args: [pythPayload.updateData],
  })
  const hash = await harness.walletClient.writeContract({
    address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
    abi: PERPS_ORDER_ROUTER_ABI,
    functionName: 'executeOrderBatch',
    args: [maxOrderId, pythPayload.updateData],
    value: updateFee * BigInt(Math.max(expectedOrderIds.length, 1)),
  })
  const receipt = await harness.publicClient.waitForTransactionReceipt({ hash })
  expect(receipt.status).toBe('success')

  const executedEvents = parseEventLogs({
    abi: LEGACY_ORDER_TERMINAL_EVENTS_ABI,
    eventName: 'OrderExecuted',
    logs: receipt.logs,
  })
  const failedEvents = parseEventLogs({
    abi: LEGACY_ORDER_TERMINAL_EVENTS_ABI,
    eventName: 'OrderFailed',
    logs: receipt.logs,
  })

  const executionPrices = new Map<bigint, bigint>()
  for (const orderId of expectedOrderIds) {
    const failed = failedEvents.find((event) => event.args.orderId === orderId)
    expect(failed, failed ? getPerpsOrderFailureMessage(Number(failed.args.reason)) : undefined).toBeUndefined()

    const executed = executedEvents.find((event) => event.args.orderId === orderId)
    expect(executed?.args.executionPrice).toBeGreaterThan(0n)
    executionPrices.set(orderId, executed!.args.executionPrice)
  }

  return { hash, executionPrices }
}

async function commitAndExecute(
  harness: ForkHarness,
  side: number,
  sizeDelta: bigint,
  marginDelta: bigint,
  targetPrice: bigint,
  isClose: boolean
): Promise<CommitAndExecuteResult> {
  const latestForkBlock = await harness.publicClient.getBlock({ blockTag: 'latest' })
  const pythPayload = await fetchHistoricalPythPayloadAfter(latestForkBlock.timestamp + 1n)
  const earliestPublishTime = BigInt(Math.min(...pythPayload.publishTimes))
  await harness.testClient.setNextBlockTimestamp({ timestamp: earliestPublishTime - 1n })

  const committed = await commitOrder(harness, side, sizeDelta, marginDelta, targetPrice, isClose)
  expect(earliestPublishTime).toBeGreaterThanOrEqual(committed.commitTime + 1n)

  const executed = await executeOrder(harness, committed.orderId, pythPayload)
  return {
    orderId: committed.orderId,
    commitHash: committed.hash,
    executeHash: executed.hash,
    executionPrice: executed.executionPrice,
    failedReason: executed.failedReason,
  }
}

async function ensureFlat(harness: ForkHarness): Promise<boolean> {
  const position = await readPosition(harness)
  if (!position.exists || position.size <= 0n) return true

  const oraclePrice = await readProtocolPrice(harness)
  if (oraclePrice <= 0n) {
    console.warn('[perps-fork] skipped: protocol oracle price is zero, so existing position cannot be flattened.')
    return false
  }

  const currentNotional = sizeDeltaToNotionalUsdc(position.size, oraclePrice) ?? 0n
  const sizeDelta = resolvePerpsSizeDelta({
    isReducingCurrentPosition: true,
    currentPositionSize: position.size,
    notionalUsdc: currentNotional,
    maxNotionalUsdc: currentNotional,
    oraclePrice,
  })
  const targetPrice = getPerpsTargetPrice({
    direction: position.side === SIDE_SHORT ? 'short' : 'long',
    isClose: true,
    oraclePrice,
    slippagePercent: FORK_TEST_SLIPPAGE_PERCENT,
  })

  await commitAndExecute(harness, position.side, sizeDelta, 0n, targetPrice, true)
  const after = await readPosition(harness)
  return !after.exists || after.size === 0n
}

async function commitAndExecuteOpen(
  harness: ForkHarness,
  side: number,
  notionalUsdc?: bigint
): Promise<CommitAndExecuteResult | undefined> {
  const oraclePrice = await readProtocolPrice(harness)
  if (oraclePrice <= 0n) {
    console.warn('[perps-fork] skipped: protocol oracle price is zero, so open order cannot be prepared.')
    return undefined
  }

  const effectiveNotionalUsdc = notionalUsdc ?? await readMinimumOpenNotional(harness)
  if (effectiveNotionalUsdc > DEFAULT_OPEN_NOTIONAL_USDC) {
    console.warn(
      `[perps-fork] using ${formatPerpsUsdc(effectiveNotionalUsdc)} USDC notional because deployed engine minimum is above the default ${formatPerpsUsdc(DEFAULT_OPEN_NOTIONAL_USDC)} USDC.`
    )
  }

  const marginDelta = effectiveNotionalUsdc / DEFAULT_LEVERAGE
  const requiredFreeMargin = marginDelta + 1n * USDC
  if (!(await ensureMargin(harness, requiredFreeMargin))) return undefined

  const sizeDelta = notionalUsdcToSizeDelta(effectiveNotionalUsdc, oraclePrice)
  if (!(await previewOpenIsValid(harness, side, sizeDelta, marginDelta, oraclePrice))) return undefined

  const targetPrice = getPerpsTargetPrice({
    direction: side === SIDE_SHORT ? 'short' : 'long',
    isClose: false,
    oraclePrice,
    slippagePercent: FORK_TEST_SLIPPAGE_PERCENT,
  })
  return commitAndExecute(harness, side, sizeDelta, marginDelta, targetPrice, false)
}

beforeAll(async () => {
  if (!shouldRunForkTest) return

  const latestBlockNumber = await getRpcBlockNumber(forkUrl!)
  const forkBlockNumber = latestBlockNumber > forkBlockLag ? latestBlockNumber - forkBlockLag : latestBlockNumber
  const anvilArgs = [
    '--fork-url',
    forkUrl!,
    '--fork-block-number',
    forkBlockNumber.toString(),
    '--chain-id',
    String(arbitrumSepolia.id),
    '--host',
    '127.0.0.1',
    '--port',
    String(anvilPort),
    '--silent',
  ]

  anvil = spawn('anvil', anvilArgs)
  anvil.on('error', (error) => {
    anvilStartError = error
  })
  anvil.stderr.on('data', (chunk) => {
    process.stderr.write(chunk)
  })

  await waitForAnvil()
})

afterAll(() => {
  anvil?.kill()
})

describe('perps fork lifecycle', () => {
  it('opens a long position and verifies the resulting position', async (ctx) => {
    if (!shouldRunForkTest) {
      skipForkTest(ctx, 'missing ARB_SEPOLIA_RPC_URL/ARBITRUM_SEPOLIA_RPC_URL or TEST_PRIVATE_KEY/PRIVATE_KEY.')
      return
    }

    await withForkSnapshot(async () => {
      const harness = createHarness()
      await ensureEth(harness)
      if (await readPendingOrderCount(harness) > 0) {
        skipForkTest(ctx, 'test account has pending orders on the fork; clean them up before running lifecycle assertions.')
        return
      }
      if (!(await ensureFlat(harness))) {
        skipForkTest(ctx, 'test account has an existing position that could not be flattened on the fork.')
        return
      }

      const result = await commitAndExecuteOpen(harness, SIDE_LONG)
      if (!result) {
        skipForkTest(ctx, 'long open preconditions were not met.')
        return
      }

      const position = await readPosition(harness)
      expect(position.exists).toBe(true)
      expect(position.side).toBe(SIDE_LONG)
      expect(position.size).toBeGreaterThan(0n)
      expect(position.entryPrice).toBe(result.executionPrice)
      expect(position.marginUsdc).toBeGreaterThan(0n)
    })
  })

  it('opens a short position and verifies the resulting position', async (ctx) => {
    if (!shouldRunForkTest) {
      skipForkTest(ctx, 'missing ARB_SEPOLIA_RPC_URL/ARBITRUM_SEPOLIA_RPC_URL or TEST_PRIVATE_KEY/PRIVATE_KEY.')
      return
    }

    await withForkSnapshot(async () => {
      const harness = createHarness()
      await ensureEth(harness)
      if (await readPendingOrderCount(harness) > 0) {
        skipForkTest(ctx, 'test account has pending orders on the fork; clean them up before running lifecycle assertions.')
        return
      }
      if (!(await ensureFlat(harness))) {
        skipForkTest(ctx, 'test account has an existing position that could not be flattened on the fork.')
        return
      }

      const result = await commitAndExecuteOpen(harness, SIDE_SHORT)
      if (!result) {
        skipForkTest(ctx, 'short open preconditions were not met.')
        return
      }

      const position = await readPosition(harness)
      expect(position.exists).toBe(true)
      expect(position.side).toBe(SIDE_SHORT)
      expect(position.size).toBeGreaterThan(0n)
      expect(position.entryPrice).toBe(result.executionPrice)
      expect(position.marginUsdc).toBeGreaterThan(0n)
    })
  })

  it('commits and executes an exact full close', async (ctx) => {
    if (!shouldRunForkTest) {
      skipForkTest(ctx, 'missing ARB_SEPOLIA_RPC_URL/ARBITRUM_SEPOLIA_RPC_URL or TEST_PRIVATE_KEY/PRIVATE_KEY.')
      return
    }

    await withForkSnapshot(async () => {
      const harness = createHarness()
      await ensureEth(harness)
      if (await readPendingOrderCount(harness) > 0) {
        skipForkTest(ctx, 'test account has pending orders on the fork; clean them up before running lifecycle assertions.')
        return
      }
      if (!(await ensureFlat(harness))) {
        skipForkTest(ctx, 'test account has an existing position that could not be flattened on the fork.')
        return
      }
      const opened = await commitAndExecuteOpen(harness, SIDE_LONG)
      if (!opened) {
        skipForkTest(ctx, 'initial long open preconditions were not met.')
        return
      }

      const position = await readPosition(harness)
      const oraclePrice = await readProtocolPrice(harness)
      const currentNotional = sizeDeltaToNotionalUsdc(position.size, oraclePrice) ?? 0n
      const sizeDelta = resolvePerpsSizeDelta({
        isReducingCurrentPosition: true,
        currentPositionSize: position.size,
        notionalUsdc: currentNotional,
        maxNotionalUsdc: currentNotional,
        oraclePrice,
      })
      expect(sizeDelta).toBe(position.size)

      const targetPrice = getPerpsTargetPrice({
        direction: 'long',
        isClose: true,
        oraclePrice,
        slippagePercent: FORK_TEST_SLIPPAGE_PERCENT,
      })
      await commitAndExecute(harness, SIDE_LONG, sizeDelta, 0n, targetPrice, true)

      const after = await readPosition(harness)
      expect(after.exists ? after.size : 0n).toBe(0n)
    })
  })

  it('partially reduces a position and keeps the same side open', async (ctx) => {
    if (!shouldRunForkTest) {
      skipForkTest(ctx, 'missing ARB_SEPOLIA_RPC_URL/ARBITRUM_SEPOLIA_RPC_URL or TEST_PRIVATE_KEY/PRIVATE_KEY.')
      return
    }

    await withForkSnapshot(async () => {
      const harness = createHarness()
      await ensureEth(harness)
      if (await readPendingOrderCount(harness) > 0) {
        skipForkTest(ctx, 'test account has pending orders on the fork; clean them up before running lifecycle assertions.')
        return
      }
      if (!(await ensureFlat(harness))) {
        skipForkTest(ctx, 'test account has an existing position that could not be flattened on the fork.')
        return
      }
      const opened = await commitAndExecuteOpen(harness, SIDE_LONG, (await readMinimumOpenNotional(harness)) * 3n)
      if (!opened) {
        skipForkTest(ctx, 'initial long open preconditions were not met.')
        return
      }

      const before = await readPosition(harness)
      const reduceSize = before.size / 2n
      expect(reduceSize).toBeGreaterThan(0n)
      const oraclePrice = await readProtocolPrice(harness)
      const targetPrice = getPerpsTargetPrice({
        direction: 'long',
        isClose: true,
        oraclePrice,
        slippagePercent: FORK_TEST_SLIPPAGE_PERCENT,
      })

      await commitAndExecute(harness, SIDE_LONG, reduceSize, 0n, targetPrice, true)

      const after = await readPosition(harness)
      expect(after.exists).toBe(true)
      expect(after.side).toBe(SIDE_LONG)
      expect(after.size).toBeLessThan(before.size)
      expect(after.size).toBeGreaterThan(0n)
    })
  })

  it('rejects an open below the deployed minimum position size', async (ctx) => {
    if (!shouldRunForkTest) {
      skipForkTest(ctx, 'missing ARB_SEPOLIA_RPC_URL/ARBITRUM_SEPOLIA_RPC_URL or TEST_PRIVATE_KEY/PRIVATE_KEY.')
      return
    }

    await withForkSnapshot(async () => {
      const harness = createHarness()
      await ensureEth(harness)
      if (await readPendingOrderCount(harness) > 0) {
        skipForkTest(ctx, 'test account has pending orders on the fork; clean them up before running lifecycle assertions.')
        return
      }
      if (!(await ensureFlat(harness))) {
        skipForkTest(ctx, 'test account has an existing position that could not be flattened on the fork.')
        return
      }

      const oraclePrice = await readProtocolPrice(harness)
      const engineMinimum = await readEngineMinimumOpenNotional(harness)
      const tooSmallNotional = engineMinimum > USDC ? engineMinimum - USDC : engineMinimum - 1n
      const latestBlock = await harness.publicClient.getBlock({ blockTag: 'latest' })
      const code = await harness.publicClient.readContract({
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens,
        abi: PERPS_CFD_ENGINE_LENS_ABI,
        functionName: 'previewOpenRevertCode',
        args: [
          harness.account.address as Address,
          SIDE_LONG,
          notionalUsdcToSizeDelta(tooSmallNotional, oraclePrice),
          tooSmallNotional / DEFAULT_LEVERAGE,
          oraclePrice,
          latestBlock.timestamp,
        ],
      })

      expect(Number(code)).toBe(3)
    })
  })

  it('rejects reduce-only commit when there is no position to close', async (ctx) => {
    if (!shouldRunForkTest) {
      skipForkTest(ctx, 'missing ARB_SEPOLIA_RPC_URL/ARBITRUM_SEPOLIA_RPC_URL or TEST_PRIVATE_KEY/PRIVATE_KEY.')
      return
    }

    await withForkSnapshot(async () => {
      const harness = createHarness()
      await ensureEth(harness)
      if (await readPendingOrderCount(harness) > 0) {
        skipForkTest(ctx, 'test account has pending orders on the fork; clean them up before running lifecycle assertions.')
        return
      }
      if (!(await ensureFlat(harness))) {
        skipForkTest(ctx, 'test account has an existing position that could not be flattened on the fork.')
        return
      }

      await expect(
        harness.walletClient.writeContract({
          address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
          abi: LEGACY_COMMIT_ORDER_ABI,
          functionName: 'commitOrder',
          args: [SIDE_LONG, 1n, 0n, 0n, true],
        })
      ).rejects.toSatisfy((error: unknown) =>
        getPerpsErrorMessage(error, 'commit').includes('There is no queued or live position to reduce')
      )
    })
  })

  it('rejects a partial close below the minimum executable size', async (ctx) => {
    if (!shouldRunForkTest) {
      skipForkTest(ctx, 'missing ARB_SEPOLIA_RPC_URL/ARBITRUM_SEPOLIA_RPC_URL or TEST_PRIVATE_KEY/PRIVATE_KEY.')
      return
    }

    await withForkSnapshot(async () => {
      const harness = createHarness()
      await ensureEth(harness)
      if (await readPendingOrderCount(harness) > 0) {
        skipForkTest(ctx, 'test account has pending orders on the fork; clean them up before running lifecycle assertions.')
        return
      }
      if (!(await ensureFlat(harness))) {
        skipForkTest(ctx, 'test account has an existing position that could not be flattened on the fork.')
        return
      }
      const opened = await commitAndExecuteOpen(harness, SIDE_LONG, (await readMinimumOpenNotional(harness)) * 3n)
      if (!opened) {
        skipForkTest(ctx, 'initial long open preconditions were not met.')
        return
      }

      const oraclePrice = await readProtocolPrice(harness)
      const engineMinimum = await readEngineMinimumOpenNotional(harness)
      const dustSize = notionalUsdcToSizeDelta(engineMinimum / 2n, oraclePrice)
      const targetPrice = getPerpsTargetPrice({
        direction: 'long',
        isClose: true,
        oraclePrice,
        slippagePercent: FORK_TEST_SLIPPAGE_PERCENT,
      })

      await expect(
        harness.walletClient.writeContract({
          address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
          abi: LEGACY_COMMIT_ORDER_ABI,
          functionName: 'commitOrder',
          args: [SIDE_LONG, dustSize, 0n, targetPrice, true],
        })
      ).rejects.toSatisfy((error: unknown) =>
        getPerpsErrorMessage(error, 'commit').includes('below the minimum executable size')
      )
    })
  })

  it('classifies too-early reveal as retryable', async (ctx) => {
    if (!shouldRunForkTest) {
      skipForkTest(ctx, 'missing ARB_SEPOLIA_RPC_URL/ARBITRUM_SEPOLIA_RPC_URL or TEST_PRIVATE_KEY/PRIVATE_KEY.')
      return
    }

    await withForkSnapshot(async () => {
      const harness = createHarness()
      await ensureEth(harness)
      if (await readPendingOrderCount(harness) > 0) {
        skipForkTest(ctx, 'test account has pending orders on the fork; clean them up before running lifecycle assertions.')
        return
      }
      if (!(await ensureFlat(harness))) {
        skipForkTest(ctx, 'test account has an existing position that could not be flattened on the fork.')
        return
      }

      const oraclePrice = await readProtocolPrice(harness)
      const notionalUsdc = await readMinimumOpenNotional(harness)
      const marginDelta = notionalUsdc / DEFAULT_LEVERAGE
      if (!(await ensureMargin(harness, marginDelta + USDC))) {
        skipForkTest(ctx, 'test account cannot reach required free margin.')
        return
      }
      const sizeDelta = notionalUsdcToSizeDelta(notionalUsdc, oraclePrice)
      if (!(await previewOpenIsValid(harness, SIDE_LONG, sizeDelta, marginDelta, oraclePrice))) {
        skipForkTest(ctx, 'previewOpenRevertCode rejected the initial long open.')
        return
      }

      const latestForkBlock = await harness.publicClient.getBlock({ blockTag: 'latest' })
      const pythPayload = await fetchHistoricalPythPayloadAfter(latestForkBlock.timestamp + 1n)
      const earliestPublishTime = BigInt(Math.min(...pythPayload.publishTimes))
      await harness.testClient.setNextBlockTimestamp({ timestamp: earliestPublishTime - 1n })
      const committed = await commitOrder(harness, SIDE_LONG, sizeDelta, marginDelta, 0n, false)

      await harness.testClient.setNextBlockTimestamp({ timestamp: committed.commitTime })
      const updateFee = await harness.publicClient.readContract({
        address: PERPS_ARBITRUM_SEPOLIA.pletherOracle,
        abi: PERPS_PLETHER_ORACLE_ABI,
        functionName: 'getUpdateFee',
        args: [pythPayload.updateData],
      })

      await expect(
        harness.walletClient.writeContract({
          address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
          abi: PERPS_ORDER_ROUTER_ABI,
          functionName: 'executeOrder',
          args: [committed.orderId, pythPayload.updateData],
          value: updateFee,
        })
      ).rejects.toSatisfy((error: unknown) => {
        const message = getPerpsErrorMessage(error, 'execute').toLowerCase()
        const rawMessage = String(error).toLowerCase()
        return message.includes('reveal is not ready yet') ||
          message.includes('execution must happen after the commit block') ||
          message.includes('order reveal is not ready yet') ||
          rawMessage.includes('0xf4a25e0f')
      })
    })
  })

  it('cleans up an expired order and removes it from pending orders', async (ctx) => {
    if (!shouldRunForkTest) {
      skipForkTest(ctx, 'missing ARB_SEPOLIA_RPC_URL/ARBITRUM_SEPOLIA_RPC_URL or TEST_PRIVATE_KEY/PRIVATE_KEY.')
      return
    }

    await withForkSnapshot(async () => {
      const harness = createHarness()
      await ensureEth(harness)
      if (await readPendingOrderCount(harness) > 0) {
        skipForkTest(ctx, 'test account has pending orders on the fork; clean them up before running lifecycle assertions.')
        return
      }
      if (!(await ensureFlat(harness))) {
        skipForkTest(ctx, 'test account has an existing position that could not be flattened on the fork.')
        return
      }

      const oraclePrice = await readProtocolPrice(harness)
      const notionalUsdc = await readMinimumOpenNotional(harness)
      const marginDelta = notionalUsdc / DEFAULT_LEVERAGE
      if (!(await ensureMargin(harness, marginDelta + USDC))) {
        skipForkTest(ctx, 'test account cannot reach required free margin.')
        return
      }
      const sizeDelta = notionalUsdcToSizeDelta(notionalUsdc, oraclePrice)
      if (!(await previewOpenIsValid(harness, SIDE_LONG, sizeDelta, marginDelta, oraclePrice))) {
        skipForkTest(ctx, 'previewOpenRevertCode rejected the initial long open.')
        return
      }

      const committed = await commitOrder(harness, SIDE_LONG, sizeDelta, marginDelta, 0n, false)
      const maxOrderAge = await harness.publicClient.readContract({
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'maxOrderAge',
      })
      await harness.testClient.setNextBlockTimestamp({ timestamp: committed.commitTime + maxOrderAge + 1n })

      const hash = await harness.walletClient.writeContract({
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'executeOrder',
        args: [committed.orderId, []],
        value: 0n,
      })
      const receipt = await harness.publicClient.waitForTransactionReceipt({ hash })
      expect(receipt.status).toBe('success')

      expect(await readPendingOrderCount(harness)).toBe(0)
    })
  })

  it('executes multiple user orders in one batch keeper transaction', async (ctx) => {
    if (!shouldRunForkTest) {
      skipForkTest(ctx, 'missing ARB_SEPOLIA_RPC_URL/ARBITRUM_SEPOLIA_RPC_URL or TEST_PRIVATE_KEY/PRIVATE_KEY.')
      return
    }

    await withForkSnapshot(async () => {
      const harness = createHarness()
      await ensureEth(harness)
      if (await readPendingOrderCount(harness) > 0) {
        skipForkTest(ctx, 'test account has pending orders on the fork; clean them up before running lifecycle assertions.')
        return
      }
      if (!(await ensureFlat(harness))) {
        skipForkTest(ctx, 'test account has an existing position that could not be flattened on the fork.')
        return
      }

      const oraclePrice = await readProtocolPrice(harness)
      if (oraclePrice <= 0n) {
        skipForkTest(ctx, 'protocol oracle price is zero, so batch orders cannot be prepared.')
        return
      }

      const notionalUsdc = await readMinimumOpenNotional(harness)
      const marginDelta = notionalUsdc / DEFAULT_LEVERAGE
      if (!(await ensureMargin(harness, marginDelta * 2n + USDC))) {
        skipForkTest(ctx, 'test account cannot reach required free margin for two batched orders.')
        return
      }

      const sizeDelta = notionalUsdcToSizeDelta(notionalUsdc, oraclePrice)
      if (!(await previewOpenIsValid(harness, SIDE_LONG, sizeDelta, marginDelta, oraclePrice))) {
        skipForkTest(ctx, 'previewOpenRevertCode rejected the first batched long open.')
        return
      }

      const latestForkBlock = await harness.publicClient.getBlock({ blockTag: 'latest' })
      const pythPayload = await fetchHistoricalPythPayloadAfter(latestForkBlock.timestamp + 2n)
      const earliestPublishTime = BigInt(Math.min(...pythPayload.publishTimes))

      await harness.testClient.setNextBlockTimestamp({ timestamp: earliestPublishTime - 2n })
      const first = await commitOrder(harness, SIDE_LONG, sizeDelta, marginDelta, 0n, false)
      await harness.testClient.setNextBlockTimestamp({ timestamp: earliestPublishTime - 1n })
      const second = await commitOrder(harness, SIDE_LONG, sizeDelta, marginDelta, 0n, false)

      expect(first.orderId).toBeLessThan(second.orderId)
      expect(await readPendingOrderCount(harness)).toBe(2)
      expect(earliestPublishTime).toBeGreaterThan(first.commitTime)
      expect(earliestPublishTime).toBeGreaterThan(second.commitTime)

      const batch = await executeOrderBatch(harness, second.orderId, pythPayload, [first.orderId, second.orderId])
      expect(batch.hash).toMatch(/^0x/)
      expect(batch.executionPrices.get(first.orderId)).toBeGreaterThan(0n)
      expect(batch.executionPrices.get(second.orderId)).toBeGreaterThan(0n)

      const position = await readPosition(harness)
      expect(position.exists).toBe(true)
      expect(position.side).toBe(SIDE_LONG)
      expect(position.size).toBeGreaterThanOrEqual(sizeDelta * 2n)
      expect(position.marginUsdc).toBeGreaterThanOrEqual(marginDelta * 2n)
      expect(await readPendingOrderCount(harness)).toBe(0)
    })
  })
})
