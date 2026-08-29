import { afterAll, beforeAll, describe, expect, it } from 'vitest'
import { spawn, type ChildProcessWithoutNullStreams } from 'node:child_process'
import { createPublicClient, http, zeroAddress } from 'viem'
import { arbitrumSepolia } from 'viem/chains'
import {
  PERPS_HOUSE_POOL_ABI,
  PERPS_PUBLIC_LENS_ABI,
  TRANCHE_VAULT_READ_ABI,
} from './abis'
import { PERPS_ARBITRUM_SEPOLIA } from './perpsAddresses'
import {
  decodePoolLiquidityView,
  decodeProtocolStatusView,
  decodeTrancheQueueView,
  decodeTrancheView,
} from './vaultViewAdapters'

const forkUrl = process.env.ARB_SEPOLIA_RPC_URL ?? process.env.ARBITRUM_SEPOLIA_RPC_URL
const anvilPort = Number(process.env.VAULT_FORK_ANVIL_PORT ?? '18547')
const anvilUrl = `http://127.0.0.1:${anvilPort}`
const shareProbe = 10n ** 27n
let anvil: ChildProcessWithoutNullStreams | undefined

async function rpc<T>(url: string, method: string, params: unknown[] = []): Promise<T> {
  const response = await fetch(url, {
    method: 'POST',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ jsonrpc: '2.0', id: 1, method, params }),
  })
  const payload = await response.json() as { result?: T; error?: { message?: string } }
  if (payload.error) throw new Error(payload.error.message ?? `${method} failed`)
  return payload.result as T
}

async function waitForAnvil(): Promise<void> {
  for (let attempt = 0; attempt < 100; attempt += 1) {
    try {
      await rpc(anvilUrl, 'eth_chainId')
      return
    } catch {
      await new Promise((resolve) => setTimeout(resolve, 100))
    }
  }
  throw new Error('Timed out waiting for the vault fork')
}

beforeAll(async () => {
  if (!forkUrl) return
  const latestHex = await rpc<string>(forkUrl, 'eth_blockNumber')
  const latest = BigInt(latestHex)
  const forkBlock = latest > 180n ? latest - 180n : latest
  anvil = spawn('anvil', [
    '--fork-url', forkUrl,
    '--fork-block-number', forkBlock.toString(),
    '--chain-id', String(arbitrumSepolia.id),
    '--host', '127.0.0.1',
    '--port', String(anvilPort),
    '--silent',
  ])
  anvil.stderr.on('data', (chunk) => process.stderr.write(chunk))
  await waitForAnvil()
})

afterAll(() => anvil?.kill())

describe('v1.2.0 vault deployment fork reads', () => {
  it('decodes every vault-facing lens tuple without a legacy fallback', async (ctx) => {
    if (!forkUrl) {
      ctx.skip()
      return
    }

    const client = createPublicClient({ chain: arbitrumSepolia, transport: http(anvilUrl) })
    const [pool, protocol, lpStatus, senior, junior, seniorQueue, juniorQueue, requestState] =
      await Promise.all([
        client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.housePool,
          abi: PERPS_HOUSE_POOL_ABI,
          functionName: 'getPoolLiquidityView',
        }),
        client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
          abi: PERPS_PUBLIC_LENS_ABI,
          functionName: 'getProtocolStatus',
        }),
        client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
          abi: PERPS_PUBLIC_LENS_ABI,
          functionName: 'getLpStatus',
        }),
        client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
          abi: PERPS_PUBLIC_LENS_ABI,
          functionName: 'getSeniorTranche',
        }),
        client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
          abi: PERPS_PUBLIC_LENS_ABI,
          functionName: 'getJuniorTranche',
        }),
        client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
          abi: PERPS_PUBLIC_LENS_ABI,
          functionName: 'getTrancheQueues',
          args: [true],
        }),
        client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
          abi: PERPS_PUBLIC_LENS_ABI,
          functionName: 'getTrancheQueues',
          args: [false],
        }),
        client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
          abi: PERPS_PUBLIC_LENS_ABI,
          functionName: 'getLpRequestState',
          args: [true, 0n, zeroAddress],
        }),
      ])

    expect(decodePoolLiquidityView(pool)).toBeDefined()
    expect(decodeProtocolStatusView(protocol)).toBeDefined()
    expect(typeof lpStatus.lpEpochSettlementPaused).toBe('boolean')
    expect(decodeTrancheView(senior)).toBeDefined()
    expect(decodeTrancheView(junior)).toBeDefined()
    expect(decodeTrancheQueueView(seniorQueue)).toBeDefined()
    expect(decodeTrancheQueueView(juniorQueue)).toBeDefined()
    expect(requestState.vault.toLowerCase()).toBe(PERPS_ARBITRUM_SEPOLIA.seniorVault.toLowerCase())
  })

  it.each([
    ['Senior', PERPS_ARBITRUM_SEPOLIA.seniorVault],
    ['Junior', PERPS_ARBITRUM_SEPOLIA.juniorVault],
  ] as const)('reads canonical %s vault pricing and estimate methods', async (_name, vault) => {
    if (!forkUrl) return
    const client = createPublicClient({ chain: arbitrumSepolia, transport: http(anvilUrl) })
    const [assets, supply, converted, depositEstimate, mintEstimate, redeemEstimate, window] =
      await Promise.all([
        client.readContract({ address: vault, abi: TRANCHE_VAULT_READ_ABI, functionName: 'totalAssets' }),
        client.readContract({ address: vault, abi: TRANCHE_VAULT_READ_ABI, functionName: 'totalSupply' }),
        client.readContract({ address: vault, abi: TRANCHE_VAULT_READ_ABI, functionName: 'convertToAssets', args: [shareProbe] }),
        client.readContract({ address: vault, abi: TRANCHE_VAULT_READ_ABI, functionName: 'estimateDepositShares', args: [1_000_000n] }),
        client.readContract({ address: vault, abi: TRANCHE_VAULT_READ_ABI, functionName: 'estimateMintAssets', args: [1_000_000_000n] }),
        client.readContract({ address: vault, abi: TRANCHE_VAULT_READ_ABI, functionName: 'estimateRedeemAssets', args: [1_000_000_000n] }),
        client.readContract({ address: vault, abi: TRANCHE_VAULT_READ_ABI, functionName: 'getRequestEpochWindow' }),
      ])

    expect(assets).toBeGreaterThanOrEqual(0n)
    expect(supply).toBeGreaterThan(0n)
    expect(converted).toBeGreaterThan(0n)
    expect(depositEstimate).toBeGreaterThanOrEqual(0n)
    expect(mintEstimate).toBeGreaterThanOrEqual(0n)
    expect(redeemEstimate).toBeGreaterThanOrEqual(0n)
    // Functions with multiple outputs decode as a positional tuple in viem.
    expect(window[0]).toBeGreaterThan(0n)
    expect(window[1]).toBeGreaterThan(0n)
  })
})
