import { readFileSync } from 'node:fs'
import path from 'node:path'
import { describe, expect, it } from 'vitest'
import publicManifest from '../../../public/perps-aa-manifest.json'
import release from '../../../../../config/perps/arbitrum-sepolia-v2.0.0.json'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_DEPLOYMENT_BLOCK, PERPS_RELEASE, PERPS_RELEASE_IS_DEPLOYED } from '../perpsAddresses'

const root = path.resolve(process.cwd(), '../..')
describe('v2.0.0 canonical release', () => {
  it('selects the complete deployed MockUSDG graph', () => {
    expect(PERPS_RELEASE_IS_DEPLOYED).toBe(true)
    expect(PERPS_RELEASE.release.version).toBe('v2.0.0')
    expect(PERPS_ARBITRUM_SEPOLIA_DEPLOYMENT_BLOCK).toBe(323658447)
    expect(PERPS_ARBITRUM_SEPOLIA.settlementToken).toBe('0x3d051dbbf5ff427a4ae8ad09746902666af8fb9d')
    expect(PERPS_ARBITRUM_SEPOLIA.cfdEnginePlanningReader).toBe(release.contracts.cfdEnginePlanningReader.address)
    expect(Object.keys(PERPS_RELEASE.contracts)).toHaveLength(30)
    for (const [key, pin] of Object.entries(release.contracts)) {
      expect(PERPS_RELEASE.contracts[key]).toMatchObject({ address: pin.address, runtimeCodeHash: pin.runtimeCodeHash })
    }
  })
  it('binds AA to the new market with the reviewed paymaster', () => {
    expect(publicManifest.settlementToken).toBe(release.contracts.settlementToken.address)
    expect(publicManifest.orderRouter).toBe(release.contracts.orderRouter.address)
    expect(publicManifest.version).toBe(release.integration.aaManifestVersion)
    expect(publicManifest.sponsorshipEnabled).toBe(true)
    expect(publicManifest.paymasterAddress).toBe('0x9761091045616A388f5fE1433721B272c78fe31b')
    expect(publicManifest.settlementTokenSupportsEip3009).toBe(false)
  })
  it('retains historical deployment artifacts without selecting them', () => {
    const historical = JSON.parse(readFileSync(path.join(root, 'config/perps/arbitrum-sepolia-v3.json'), 'utf8')) as { release: { version: string; sourceCommit: string } }
    expect(historical.release.version).toBe('v1.3.1')
    expect(historical.release.sourceCommit).toBe('cf86189bcca4813f39d6405a338b77176783eb42')
    const registry = readFileSync(path.join(root, 'apps/frontend/src/contracts/perpsAddresses.ts'), 'utf8')
    expect(registry).toContain('arbitrum-sepolia-v2.0.0.json')
    expect(registry).not.toContain('arbitrum-sepolia-v3.json')
  })
})
