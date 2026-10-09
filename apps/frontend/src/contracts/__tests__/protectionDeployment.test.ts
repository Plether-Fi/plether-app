import { describe, expect, it, vi } from 'vitest'
import { keccak256, type PublicClient } from 'viem'
import rawManifest from '../../../public/perps-aa-manifest.json'
import { parsePerpsAaManifest } from '../../perps-aa/manifest'
import { PERPS_DEFAULT_SEPOLIA_DEPLOYMENT } from '../perpsAddresses'
import { verifyProtectionDeployment } from '../verifyPerpsV2Bindings'

function setup() {
  const code = '0x6001600055' as const
  const deployment = structuredClone(PERPS_DEFAULT_SEPOLIA_DEPLOYMENT)
  deployment.chainId = 42161
  const manifest = { ...parsePerpsAaManifest(rawManifest), chainId: deployment.chainId }
  const keys = ['positionProtectionBook', 'orderRouter', 'orderLifecycleBook'] as const
  keys.forEach((key, index) => {
    // Synthetic test addresses distinguish a selected release from the shipped one.
    manifest[key] = `0x${(index + 1).toString().padStart(40, '0')}`
    deployment.contracts[key] = manifest[key]
    deployment.runtimeCodeHashes[key] = keccak256(code)
  })
  const getChainId = vi.fn(async () => deployment.chainId)
  const getCode = vi.fn(async () => code)
  const client = { getChainId, getCode } as unknown as PublicClient
  return { client, getCode, getChainId, deployment, manifest, keys }
}

describe('protection release verification', () => {
  it('verifies configured mainnet addresses and code at the reviewed block', async () => {
    const { client, getCode, deployment, manifest, keys } = setup()
    await verifyProtectionDeployment(client, manifest, 123n, deployment)
    expect(getCode).toHaveBeenCalledTimes(3)
    for (const key of keys) {
      expect(getCode).toHaveBeenCalledWith({ address: deployment.contracts[key], blockNumber: 123n })
    }
  })

  it('rejects a testnet manifest and the wrong RPC before reading code', async () => {
    const { client, getCode, getChainId, deployment, manifest } = setup()
    await expect(verifyProtectionDeployment(client, { ...manifest, chainId: 421614 }, 123n, deployment)).rejects.toThrow('active reviewed')
    getChainId.mockResolvedValueOnce(421614)
    await expect(verifyProtectionDeployment(client, manifest, 123n, deployment)).rejects.toThrow('active reviewed')
    expect(getCode).not.toHaveBeenCalled()
  })

  it('rejects an address from a different release and a mismatched code hash', async () => {
    const { client, getCode, deployment, manifest } = setup()
    await expect(verifyProtectionDeployment(client, { ...manifest, orderRouter: PERPS_DEFAULT_SEPOLIA_DEPLOYMENT.contracts.orderRouter }, 123n, deployment)).rejects.toThrow('active reviewed')
    expect(getCode).not.toHaveBeenCalled()
    deployment.runtimeCodeHashes.positionProtectionBook = `0x${'22'.repeat(32)}`
    await expect(verifyProtectionDeployment(client, manifest, 123n, deployment)).rejects.toThrow('bytecode')
  })
})
