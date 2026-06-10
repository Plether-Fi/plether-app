import { afterAll, beforeAll, describe, expect, it } from 'vitest'
import { spawn, type ChildProcessWithoutNullStreams } from 'node:child_process'
import {
  createPublicClient,
  createTestClient,
  createWalletClient,
  http,
  parseEther,
  parseEventLogs,
  type Address,
  type Hex,
} from 'viem'
import { privateKeyToAccount } from 'viem/accounts'
import { arbitrumSepolia } from 'viem/chains'
import {
  PERPS_ORDER_ROUTER_ABI,
  PERPS_PLETHER_ORACLE_ABI,
  PERPS_PUBLIC_LENS_ABI,
} from '../../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA } from '../../contracts/perpsAddresses'
import { getPerpsOrderFailureMessage } from '../../utils/perpsErrors'
import { sizeDeltaToNotionalUsdc } from '../../utils/perps'
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

const forkUrl = process.env.ARB_SEPOLIA_RPC_URL ?? process.env.ARBITRUM_SEPOLIA_RPC_URL
const rawPrivateKey = process.env.TEST_PRIVATE_KEY ?? process.env.PRIVATE_KEY
const backendApiUrl = process.env.PERPS_FORK_API_URL ?? process.env.VITE_API_URL ?? 'http://127.0.0.1:3001/api'
const anvilPort = Number(process.env.PERPS_FORK_ANVIL_PORT ?? '18546')
const forkBlockLag = BigInt(process.env.PERPS_FORK_BLOCK_LAG ?? '180')
const anvilUrl = `http://127.0.0.1:${anvilPort}`
const shouldRunForkTest = Boolean(forkUrl && rawPrivateKey)

let anvil: ChildProcessWithoutNullStreams | undefined

function normalizePrivateKey(value: string): Hex {
  return value.startsWith('0x') ? value as Hex : `0x${value}` as Hex
}

function tupleValue(value: unknown, index: number, key: string): unknown {
  if (value && typeof value === 'object' && key in value) {
    return (value as Record<string, unknown>)[key]
  }
  if (Array.isArray(value)) return value[index]
  return undefined
}

function delay(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms))
}

async function waitForAnvil(): Promise<void> {
  const startedAt = Date.now()
  while (Date.now() - startedAt < 30_000) {
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
  const response = await fetch(rpcUrl, {
    method: 'POST',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'eth_blockNumber', params: [] }),
  })
  const payload = await response.json() as { result?: string }
  if (!payload.result) throw new Error('Could not read Arbitrum Sepolia block number for fork setup')
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
  anvil.stderr.on('data', (chunk) => {
    process.stderr.write(chunk)
  })

  await waitForAnvil()
})

afterAll(() => {
  anvil?.kill()
})

describe('perps fork lifecycle', () => {
  it('commits and executes an exact full close on an Arbitrum Sepolia fork', async (ctx) => {
    if (!shouldRunForkTest) {
      ctx.skip()
      return
    }

    const account = privateKeyToAccount(normalizePrivateKey(rawPrivateKey!))
    const transport = http(anvilUrl)
    const publicClient = createPublicClient({ chain: arbitrumSepolia, transport })
    const walletClient = createWalletClient({ account, chain: arbitrumSepolia, transport })
    const testClient = createTestClient({ chain: arbitrumSepolia, mode: 'anvil', transport })

    await testClient.setBalance({ address: account.address, value: parseEther('10') })

    const [status, position] = await Promise.all([
      publicClient.readContract({
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getProtocolStatus',
      }),
      publicClient.readContract({
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getPosition',
        args: [account.address as Address],
      }),
    ])

    const exists = Boolean(tupleValue(position, 0, 'exists'))
    const side = Number(tupleValue(position, 1, 'side') ?? 0)
    const currentSize = tupleValue(position, 2, 'size') as bigint | undefined ?? 0n
    const oraclePrice = tupleValue(status, 1, 'lastMarkPrice') as bigint | undefined ?? 0n
    if (!exists || currentSize <= 0n || oraclePrice <= 0n) {
      ctx.skip()
      return
    }

    const currentNotional = sizeDeltaToNotionalUsdc(currentSize, oraclePrice) ?? 0n
    const sizeDelta = resolvePerpsSizeDelta({
      isReducingCurrentPosition: true,
      currentPositionSize: currentSize,
      notionalUsdc: currentNotional,
      maxNotionalUsdc: currentNotional,
      oraclePrice,
    })
    expect(sizeDelta).toBe(currentSize)

    const latestForkBlock = await publicClient.getBlock({ blockTag: 'latest' })
    const pythPayload = await fetchHistoricalPythPayloadAfter(latestForkBlock.timestamp + 1n)
    const earliestPublishTime = BigInt(Math.min(...pythPayload.publishTimes))
    await testClient.setNextBlockTimestamp({ timestamp: earliestPublishTime - 1n })

    const commitHash = await walletClient.writeContract({
      address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'commitOrder',
      args: [side, sizeDelta, 0n, 0n, true],
    })
    const commitReceipt = await publicClient.waitForTransactionReceipt({ hash: commitHash })
    expect(commitReceipt.status).toBe('success')

    const [committed] = parseEventLogs({
      abi: PERPS_ORDER_ROUTER_ABI,
      eventName: 'OrderCommitted',
      logs: commitReceipt.logs,
    }).filter((event) => event.args.account.toLowerCase() === account.address.toLowerCase())
    expect(committed?.args.orderId).toBeDefined()
    const orderId = committed!.args.orderId

    const pendingOrderView = await publicClient.readContract({
      address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'getPendingOrderView',
      args: [orderId],
    })
    const pending = tupleValue(pendingOrderView, 0, 'pending')
    const commitTime = tupleValue(pending, 6, 'commitTime') as bigint | undefined
    expect(commitTime).toBeDefined()
    expect(earliestPublishTime).toBe(commitTime! + 1n)

    await testClient.setNextBlockTimestamp({ timestamp: earliestPublishTime })

    const updateFee = await publicClient.readContract({
      address: PERPS_ARBITRUM_SEPOLIA.pletherOracle,
      abi: PERPS_PLETHER_ORACLE_ABI,
      functionName: 'getUpdateFee',
      args: [pythPayload.updateData],
    })
    const executeHash = await walletClient.writeContract({
      address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'executeOrder',
      args: [orderId, pythPayload.updateData],
      value: updateFee,
    })
    const executeReceipt = await publicClient.waitForTransactionReceipt({ hash: executeHash })
    expect(executeReceipt.status).toBe('success')

    const [executed] = parseEventLogs({
      abi: PERPS_ORDER_ROUTER_ABI,
      eventName: 'OrderExecuted',
      logs: executeReceipt.logs,
    }).filter((event) => event.args.orderId === orderId)
    const [failed] = parseEventLogs({
      abi: PERPS_ORDER_ROUTER_ABI,
      eventName: 'OrderFailed',
      logs: executeReceipt.logs,
    }).filter((event) => event.args.orderId === orderId)

    expect(failed, failed ? getPerpsOrderFailureMessage(Number(failed.args.reason)) : undefined).toBeUndefined()
    expect(executed?.args.executionPrice).toBeGreaterThan(0n)
  })
})
