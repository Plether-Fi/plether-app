import { beforeEach, describe, expect, it, vi } from 'vitest'
import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { keccak256, type PublicClient } from 'viem'
import pin from '../../../../../config/perps/close-preview/arbitrum-sepolia.json'
import rawManifest from '../../../public/perps-aa-manifest.json'
import { parsePerpsAaManifest } from '../../perps-aa/manifest'
import { PERPS_CFD_CLOSE_PREVIEW_ABI } from '../abis'
import { verifyClosePreviewDeployment } from '../verifyPerpsV2Bindings'
import { PERPS_DEFAULT_SEPOLIA_DEPLOYMENT } from '../perpsAddresses'

const manifest = parsePerpsAaManifest(rawManifest)
let deployment = structuredClone(PERPS_DEFAULT_SEPOLIA_DEPLOYMENT)
beforeEach(() => { deployment = structuredClone(PERPS_DEFAULT_SEPOLIA_DEPLOYMENT) })
const code = '0x6001600055' as const
function client(chain = 421614, bytecode: string | undefined = code) {
  return { getChainId: vi.fn(async () => chain), getCode: vi.fn(async () => bytecode) } as unknown as PublicClient
}

describe('supplemental close preview deployment', () => {
  it('pins the delivered artifact and generated ABI without relabeling the evaluator', () => {
    const bytes = readFileSync('../../config/perps/close-preview/CfdClosePreview.abi.json')
    expect(createHash('sha256').update(bytes).digest('hex')).toBe(pin.contracts.cfdClosePreview.abiSha256)
    expect(PERPS_CFD_CLOSE_PREVIEW_ABI).toEqual(JSON.parse(bytes.toString()).filter((item: { type: string; name?: string }) => item.type === 'error' || item.name === 'previewClose'))
    expect(pin.contracts.cfdClosePreview.runtimeCodeHash).toBe('0x2f8f5cf607ddcd71f3bafd166e3fa3d20980a4077eaa28b8508b2a0ac1c29b16')
    expect(pin.existingProtocol.policyEvaluator.toLowerCase()).toBe(manifest.policyEvaluator.toLowerCase())
    expect(pin.contracts.cfdClosePreview.address.toLowerCase()).not.toBe(manifest.policyEvaluator.toLowerCase())
  })
  it('verifies runtime at the reviewed block', async () => {
    deployment.closePreview.runtimeCodeHash = keccak256(code)
    const rpc = client()
    await expect(verifyClosePreviewDeployment(rpc, manifest, 308935132n, deployment)).resolves.toBe(pin.contracts.cfdClosePreview.address)
    expect(rpc.getCode).toHaveBeenCalledWith({ address: pin.contracts.cfdClosePreview.address, blockNumber: 308935132n })
  })
  it.each(['0x', '0x6002'])('rejects missing or mismatched code %s', async bytecode => {
    await expect(verifyClosePreviewDeployment(client(421614, bytecode), manifest, 1n, deployment)).rejects.toThrow('bytecode')
  })
  it('rejects wrong RPC chain and manifest chain', async () => {
    await expect(verifyClosePreviewDeployment(client(1), manifest, 1n, deployment)).rejects.toThrow('active perps deployment')
    await expect(verifyClosePreviewDeployment(client(), { ...manifest, chainId: 1 }, 1n, deployment)).rejects.toThrow('active perps deployment')
  })
  it('rejects evaluator alias and invalid address', async () => {
    deployment.closePreview.address = manifest.policyEvaluator
    await expect(verifyClosePreviewDeployment(client(), manifest, 1n, deployment)).rejects.toThrow('aliases')
    deployment.closePreview.address = '' as `0x${string}`
    await expect(verifyClosePreviewDeployment(client(), manifest, 1n, deployment)).rejects.toThrow('configuration')
  })
  it('requires the configured mainnet preview and never reads the shipped Sepolia lens', async () => {
    deployment.chainId = 42161
    deployment.closePreview.chainId = 42161
    deployment.closePreview.address = '0x0000000000000000000000000000000000000099'
    deployment.closePreview.runtimeCodeHash = keccak256(code)
    const rpc = client(42161)
    await expect(verifyClosePreviewDeployment(rpc, { ...manifest, chainId: 42161 }, 123n, deployment))
      .resolves.toBe(deployment.closePreview.address)
    expect(rpc.getCode).toHaveBeenCalledExactlyOnceWith({ address: deployment.closePreview.address, blockNumber: 123n })
    expect(rpc.getCode).not.toHaveBeenCalledWith(expect.objectContaining({ address: pin.contracts.cfdClosePreview.address }))
    await expect(verifyClosePreviewDeployment(rpc, { ...manifest, chainId: 42161 }, 123n))
      .rejects.toThrow('active perps deployment')
  })

})
