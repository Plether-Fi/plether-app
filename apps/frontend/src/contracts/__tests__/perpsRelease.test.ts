import { readFileSync } from 'node:fs'
import path from 'node:path'
import { describe, expect, it } from 'vitest'
import { zeroAddress } from 'viem'
import publicManifest from '../../../public/perps-aa-manifest.json'
import prepared from '../../../../../config/perps/mock-usdg-prepared.json'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_DEPLOYMENT_BLOCK, PERPS_RELEASE, PERPS_RELEASE_IS_DEPLOYED } from '../perpsAddresses'

const root = path.resolve(process.cwd(), '../..')
describe('prepared canonical release safety', () => {
  it('does not present an undeployed graph as active', () => {
    expect(PERPS_RELEASE_IS_DEPLOYED).toBe(false)
    expect(PERPS_RELEASE.release.phase).toBe('prepared')
    expect(PERPS_ARBITRUM_SEPOLIA_DEPLOYMENT_BLOCK).toBe(0)
    expect(PERPS_ARBITRUM_SEPOLIA.settlementToken).toBe(zeroAddress)
    for (const pin of Object.values(prepared.contracts)) {
      expect(pin.address).toBeNull()
      expect(pin.runtimeCodeHash).toBeNull()
    }
    expect(publicManifest.sponsorshipEnabled).toBe(false)
    expect(publicManifest.settlementToken).toBeNull()
    expect(publicManifest.paymasterAddress).toBeNull()
  })
  it('retains historical deployment artifacts without selecting them', () => {
    const historical = JSON.parse(readFileSync(path.join(root, 'config/perps/arbitrum-sepolia-v3.json'), 'utf8')) as { release: { version: string; sourceCommit: string } }
    expect(historical.release.version).toBe('v1.3.1')
    expect(historical.release.sourceCommit).toBe('cf86189bcca4813f39d6405a338b77176783eb42')
    const registry = readFileSync(path.join(root, 'apps/frontend/src/contracts/perpsAddresses.ts'), 'utf8')
    expect(registry).toContain('mock-usdg-prepared.json')
    expect(registry).not.toContain('arbitrum-sepolia-v3.json')
  })
})
