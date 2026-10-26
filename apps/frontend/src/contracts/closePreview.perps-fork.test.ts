import { describe, expect, it } from 'vitest'
import { createPublicClient, http, type Address } from 'viem'
import { arbitrumSepolia } from 'viem/chains'
import rawManifest from '../../public/perps-aa-manifest.json'
import { parsePerpsAaManifest } from '../perps-aa/manifest'
import { preparePerpsOrderV3 } from './preparePerpsOrderV3'
import { verifyClosePreviewDeployment } from './verifyPerpsV3Bindings'
import { PERPS_RELEASE_IS_DEPLOYED, PERPS_CLOSE_PREVIEW_ADDRESS } from './perpsAddresses'

const url = process.env.ARB_SEPOLIA_RPC_URL
const account = process.env.PERPS_INTEGRATION_ACCOUNT as Address | undefined
const size = process.env.PERPS_INTEGRATION_CLOSE_SIZE
const manifest = PERPS_RELEASE_IS_DEPLOYED ? parsePerpsAaManifest(rawManifest) : undefined

// Read-only current-release check. Prepared configuration has no chain target;
// historical samples require their matching archived consumer and ABI.
describe.skipIf(!url || !account || !size || !manifest)('deployed close-preview frontend compatibility', () => {
  it('verifies the current lens and reviews an explicitly supplied close', async () => {
    if (!url || !account || !size || !manifest) throw new Error('Current deployment inputs are required')
    const client = createPublicClient({ chain: arbitrumSepolia, transport: http(url) })
    const block = await client.getBlock({ blockTag: 'latest' })
    await expect(verifyClosePreviewDeployment(client, manifest, block.number)).resolves.toBe(PERPS_CLOSE_PREVIEW_ADDRESS)
    const prepared = await preparePerpsOrderV3(client, manifest, {
      account, direction: 'long', side: 0, isClose: true,
      sizeDelta: BigInt(size), marginDelta: 0n,
      slippagePercent: 0.1, selectedMaxLeverageBps: 0,
    })
    expect(prepared.account).toBe(account)
    expect(prepared.executionBountyAssets).toBe(0n)
    expect(prepared.request.isClose).toBe(true)
  })
})
