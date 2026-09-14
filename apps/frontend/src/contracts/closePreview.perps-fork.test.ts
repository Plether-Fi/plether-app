import { describe, expect, it } from 'vitest'
import { createPublicClient, http, type PublicClient } from 'viem'
import { arbitrumSepolia } from 'viem/chains'
import rawManifest from '../../public/perps-aa-manifest.json'
import { parsePerpsAaManifest } from '../perps-aa/manifest'
import { preparePerpsOrderV2 } from './preparePerpsOrderV2'
import { verifyClosePreviewDeployment } from './verifyPerpsV2Bindings'

const url = process.env.ARB_SEPOLIA_RPC_URL
const manifest = parsePerpsAaManifest(rawManifest)
// Public sample from the source-verified lens deployment packet. All calls are
// pinned read-only calls, including commit simulation; no wallet is constructed.
const account = '0x434034d4706173a9f9902eb00f1f80e59da3b82a' as const

describe.skipIf(!url)('deployed close-preview frontend compatibility', () => {
  it('verifies the lens and completes a real reservation-aware review plus commit simulation', async () => {
    const rpc = createPublicClient({ chain: arbitrumSepolia, transport: http(url) })
    const block = await rpc.getBlock({ blockNumber: 308935132n })
    const client = rpc.extend(() => ({ getBlock: async () => block })) as unknown as PublicClient
    await expect(verifyClosePreviewDeployment(client, manifest, block.number)).resolves.toBe('0x202A2C5156563Ec4fEF7D3997771bBCa90e98117')
    const prepared = await preparePerpsOrderV2(client, manifest, {
      account, direction: 'long', side: 0, isClose: true,
      sizeDelta: 1200n * 10n ** 18n, marginDelta: 0n,
      slippagePercent: 0.1, selectedMaxLeverageBps: 0,
    })
    expect(prepared.reviewedBlockNumber).toBe(block.number)
    expect(prepared.account).toBe(account)
    expect(prepared.reviewSummary?.commitmentCarryUsdc).toBe(100n)
    expect(prepared.executionBountyUsdc).toBe(200_000n)
    expect(prepared.reviewSummary?.currentAssessment.postPositionSize).toBe(0n)
    expect(prepared.reviewSummary?.currentAssessment.carryUsdc).toBe(0n)
  })
})
