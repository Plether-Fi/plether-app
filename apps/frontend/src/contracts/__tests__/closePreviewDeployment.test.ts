import { afterEach, describe, expect, it, vi } from 'vitest'
import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { keccak256, type PublicClient } from 'viem'
import pin from '../../../../../config/perps/close-preview/arbitrum-sepolia.json'
import rawManifest from '../../../public/perps-aa-manifest.json'
import { parsePerpsAaManifest } from '../../perps-aa/manifest'
import { PERPS_CFD_CLOSE_PREVIEW_ABI } from '../abis'
import { verifyClosePreviewDeployment } from '../verifyPerpsV2Bindings'

const manifest = parsePerpsAaManifest(rawManifest)
const originalPin = structuredClone(pin)
afterEach(() => { Object.assign(pin, structuredClone(originalPin)) })
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
    pin.contracts.cfdClosePreview.runtimeCodeHash = keccak256(code)
    const rpc = client()
    await expect(verifyClosePreviewDeployment(rpc, manifest, 308935132n)).resolves.toBe(pin.contracts.cfdClosePreview.address)
    expect(rpc.getCode).toHaveBeenCalledWith({ address: pin.contracts.cfdClosePreview.address, blockNumber: 308935132n })
  })
  it.each(['0x', '0x6002'])('rejects missing or mismatched code %s', async bytecode => {
    await expect(verifyClosePreviewDeployment(client(421614, bytecode), manifest, 1n)).rejects.toThrow('bytecode')
  })
  it('rejects wrong RPC chain and manifest chain', async () => {
    await expect(verifyClosePreviewDeployment(client(1), manifest, 1n)).rejects.toThrow('Arbitrum Sepolia')
    await expect(verifyClosePreviewDeployment(client(), { ...manifest, chainId: 1 }, 1n)).rejects.toThrow('Arbitrum Sepolia')
  })
  it('rejects evaluator alias and invalid address', async () => {
    pin.contracts.cfdClosePreview.address = manifest.policyEvaluator
    await expect(verifyClosePreviewDeployment(client(), manifest, 1n)).rejects.toThrow('aliases')
    pin.contracts.cfdClosePreview.address = ''
    await expect(verifyClosePreviewDeployment(client(), manifest, 1n)).rejects.toThrow('configuration')
  })
})
