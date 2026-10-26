import { beforeEach, describe, expect, it, vi } from 'vitest'
import { keccak256, type Address, type PublicClient } from 'viem'
import sourceAbis from './fixtures/perps-canonical-source-abis.json'
import rawManifest from './fixtures/perps-canonical-aa.json'
import { parsePerpsAaManifest } from '../../perps-aa/manifest'
import { PERPS_CFD_CLOSE_PREVIEW_ABI } from '../abis'
import { PERPS_RELEASE } from '../perpsAddresses'
import { verifyClosePreviewDeployment } from '../verifyPerpsV3Bindings'

const deployment = vi.hoisted(() => ({ available: true }))
vi.mock('../perpsAddresses', async importOriginal => {
  const actual = await importOriginal<typeof import('../perpsAddresses')>()
  const { default: fixture } = await import('./fixtures/perps-canonical-release.json')
  const { default: manifest } = await import('./fixtures/perps-canonical-aa.json')
  const { parsePerpsReleaseConfiguration } = await import('../perpsRelease')
  return {
    ...actual,
    PERPS_RELEASE: parsePerpsReleaseConfiguration(fixture),
    get PERPS_RELEASE_IS_DEPLOYED() { return deployment.available },
    PERPS_ARBITRUM_SEPOLIA: {
      ...actual.PERPS_ARBITRUM_SEPOLIA,
      ...manifest,
      cfdEngine: manifest.cfdEngine,
      orderRouter: manifest.orderRouter,
      policyEvaluator: manifest.policyEvaluator,
    },
  }
})

const manifest = parsePerpsAaManifest(rawManifest)
const pin = PERPS_RELEASE.contracts.cfdClosePreview
const originalPin = structuredClone(pin)
const code = '0x6001600055' as const
beforeEach(() => {
  deployment.available = true
  Object.assign(pin, structuredClone(originalPin))
  pin.runtimeCodeHash = keccak256(code)
})
function client(chain = 421614, bytecode: string | undefined = code) {
  return { getChainId: vi.fn(async () => chain), getCode: vi.fn(async () => bytecode) } as unknown as PublicClient
}

describe('canonical close preview preparation', () => {
  it('includes both compiled preview overloads and current errors', () => {
    const compiled = sourceAbis.PERPS_CFD_CLOSE_PREVIEW_ABI.filter(item => item.type === 'error' || item.name === 'previewClose')
    const publicAbi = JSON.parse(JSON.stringify(compiled, (key, value: unknown) => key === 'internalType' ? undefined : value)) as unknown
    expect(PERPS_CFD_CLOSE_PREVIEW_ABI).toEqual(publicAbi)
    expect(PERPS_CFD_CLOSE_PREVIEW_ABI.filter(item => item.type === 'function' && item.name === 'previewClose')).toHaveLength(2)
  })

  it('does not query RPC for a prepared, unavailable release', async () => {
    deployment.available = false
    const rpc = client()
    await expect(verifyClosePreviewDeployment(rpc, manifest, 1n)).rejects.toThrow('prepared and unavailable')
    expect(rpc.getChainId).not.toHaveBeenCalled()
    expect(rpc.getCode).not.toHaveBeenCalled()
  })

  it('verifies an explicitly activated synthetic pin at the requested block', async () => {
    const rpc = client()
    await expect(verifyClosePreviewDeployment(rpc, manifest, 123n)).resolves.toBe(pin.address)
    expect(rpc.getCode).toHaveBeenCalledWith({ address: pin.address, blockNumber: 123n })
  })

  it.each(['0x', '0x6002'])('rejects missing or mismatched code %s', async bytecode => {
    await expect(verifyClosePreviewDeployment(client(421614, bytecode), manifest, 1n)).rejects.toThrow('bytecode')
  })

  it('rejects wrong RPC chain and manifest chain', async () => {
    await expect(verifyClosePreviewDeployment(client(1), manifest, 1n)).rejects.toThrow('Arbitrum Sepolia')
    await expect(verifyClosePreviewDeployment(client(), { ...manifest, chainId: 1 }, 1n)).rejects.toThrow('Arbitrum Sepolia')
  })

  it('rejects evaluator alias and missing pins', async () => {
    pin.address = manifest.policyEvaluator
    await expect(verifyClosePreviewDeployment(client(), manifest, 1n)).rejects.toThrow('aliases')
    pin.address = '' as Address
    await expect(verifyClosePreviewDeployment(client(), manifest, 1n)).rejects.toThrow('pins are missing')
    pin.address = originalPin.address
    pin.runtimeCodeHash = null
    await expect(verifyClosePreviewDeployment(client(), manifest, 1n)).rejects.toThrow('pins are missing')
  })
})
