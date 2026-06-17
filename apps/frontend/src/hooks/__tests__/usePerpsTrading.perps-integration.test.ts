import { describe, expect, it } from 'vitest'
import { createPublicClient, http, type Address } from 'viem'
import { arbitrumSepolia } from 'viem/chains'
import {
  PERPS_CFD_ENGINE_LENS_ABI,
  PERPS_ORDER_ROUTER_ABI,
  PERPS_PUBLIC_LENS_ABI,
} from '../../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA } from '../../contracts/perpsAddresses'
import { getPerpsTargetPrice, sizeDeltaToNotionalUsdc } from '../../utils/perps'
import { resolvePerpsSizeDelta } from '../../utils/perpsOrder'

const rpcUrl = process.env.ARBITRUM_SEPOLIA_RPC_URL
const account = (process.env.PERPS_INTEGRATION_ACCOUNT ??
  '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B') as Address

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

describe('perps full-close integration', () => {
  it('uses exact position size and passes close preview/commit simulation', async (ctx) => {
    if (!client) {
      ctx.skip()
      return
    }

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
      ctx.skip()
      return
    }

    const positionNotional = sizeDeltaToNotionalUsdc(size, oraclePrice) ?? 0n
    const resolvedSizeDelta = resolvePerpsSizeDelta({
      isReducingCurrentPosition: true,
      currentPositionSize: size,
      notionalUsdc: positionNotional,
      maxNotionalUsdc: positionNotional,
      oraclePrice,
    })
    const targetPrice = getPerpsTargetPrice({
      direction: side === 1 ? 'short' : 'long',
      isClose: true,
      oraclePrice,
      slippagePercent: 0.1,
    })

    expect(resolvedSizeDelta).toBe(size)

    const closePreview = await client.readContract({
      address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens,
      abi: PERPS_CFD_ENGINE_LENS_ABI,
      functionName: 'previewClose',
      args: [account, resolvedSizeDelta, oraclePrice],
    })
    expect(tupleValue(closePreview, 0, 'valid')).toBe(true)

    await expect(
      client.simulateContract({
        account,
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'commitOrder',
        args: [side, resolvedSizeDelta, 0n, targetPrice, true],
      })
    ).resolves.toBeDefined()
  })
})
