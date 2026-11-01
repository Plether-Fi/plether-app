import { describe, expect, it } from 'vitest'
import { createPublicClient, http, isAddress, type Address } from 'viem'
import { arbitrumSepolia } from 'viem/chains'
import rawManifest from '../../../public/perps-aa-manifest.json'
import { PERPS_PUBLIC_LENS_ABI } from '../../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_RELEASE_IS_DEPLOYED } from '../../contracts/perpsAddresses'
import { preparePerpsOrderV3 } from '../../contracts/preparePerpsOrderV3'
import { parsePerpsAaManifest } from '../../perps-aa/manifest'
import { sizeDeltaToNotionalAssets } from '../../utils/perps'
import { resolvePerpsSizeDelta } from '../../utils/perpsOrder'
import { integrationPrerequisitesAvailable } from '../../test/integrationPrerequisites'

const rpcUrl = process.env.ARB_SEPOLIA_RPC_URL ?? process.env.ARBITRUM_SEPOLIA_RPC_URL
const account = process.env.PERPS_INTEGRATION_ACCOUNT as Address | undefined
const manifest = PERPS_RELEASE_IS_DEPLOYED ? parsePerpsAaManifest(rawManifest) : undefined
const configured = integrationPrerequisitesAvailable('PERPS_FORK_REQUIRED', 'Perps full-close integration', [
  { available: Boolean(rpcUrl), requirement: 'ARB_SEPOLIA_RPC_URL or ARBITRUM_SEPOLIA_RPC_URL' },
  { available: Boolean(manifest), requirement: 'the current deployed canonical release and matching manifest' },
  { available: Boolean(account && isAddress(account)), requirement: 'an explicit valid PERPS_INTEGRATION_ACCOUNT' },
])

const client = rpcUrl
  ? createPublicClient({
      chain: arbitrumSepolia,
      transport: http(rpcUrl),
    })
  : undefined

function tupleValue(value: unknown, index: number, key: string): unknown {
  if (value && typeof value === 'object' && key in value) {
    return (value as Record<string, unknown>)[key]
  }
  if (Array.isArray(value)) return value[index]
  return undefined
}

describe.skipIf(!configured)('perps full-close integration', () => {
  it('uses exact position size and passes close preview/commit simulation', async (ctx) => {
    if (!client || !manifest || !account) throw new Error('Current deployment inputs are required')

    const [status, position] = await Promise.all([
      client.readContract({
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getProtocolStatus',
      }),
      client.readContract({
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getPosition',
        args: [account],
      }),
    ])

    const exists = Boolean(tupleValue(position, 0, 'exists'))
    const side = Number(tupleValue(position, 1, 'side') ?? 0)
    const size = tupleValue(position, 2, 'size') as bigint | undefined ?? 0n
    const oraclePrice = tupleValue(status, 1, 'lastMarkPrice') as bigint | undefined ?? 0n

    if (!exists || size <= 0n || oraclePrice <= 0n) {
      integrationPrerequisitesAvailable('PERPS_FORK_REQUIRED', 'Perps full-close integration', [{
        available: false,
        requirement: 'an existing positive-size position for PERPS_INTEGRATION_ACCOUNT and a positive oracle mark',
      }])
      ctx.skip()
      return
    }

    const positionNotional = sizeDeltaToNotionalAssets(size, oraclePrice) ?? 0n
    const resolvedSizeDelta = resolvePerpsSizeDelta({
      isReducingCurrentPosition: true,
      currentPositionSize: size,
      notionalAssets: positionNotional,
      maxNotionalAssets: positionNotional,
      oraclePrice,
    })
    expect(resolvedSizeDelta).toBe(size)

    const prepared = await preparePerpsOrderV3(client, manifest, {
      account,
      direction: side === 1 ? 'short' : 'long',
      side,
      sizeDelta: resolvedSizeDelta,
      marginDelta: 0n,
      slippagePercent: 0.1,
      isClose: true,
      selectedMaxLeverageBps: 330_000,
    })

    expect(prepared.request.isClose).toBe(true)
    expect(prepared.request.sizeDelta).toBe(size)
    expect(prepared.request.bounds.expectedConfigHash).not.toBe(
      `0x${'0'.repeat(64)}`
    )
    expect(prepared.request.bounds.allowedExecutionModes).toBeOneOf([1, 2, 4])
  })
})
