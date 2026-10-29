import { execFileSync } from 'node:child_process'
import path from 'node:path'
import canonicalRelease from '../contracts/__tests__/fixtures/perps-canonical-release.json'
import { describe, expect, it, vi } from 'vitest'
import rawManifest from '../contracts/__tests__/fixtures/perps-canonical-aa.json'
vi.mock('../contracts/perpsAddresses', async () => (await import('../test/perpsReleaseFixture')).fixturePerpsAddresses)
import { parsePerpsAaManifest } from './manifest'

describe('MockUSDG sponsorship manifest', () => {
  it('parses a schema-4 candidate emitted by the actual offline release generator', () => {
    const program = `
      import { prepareRelease } from ${JSON.stringify(path.resolve(process.cwd(), '../../scripts/prepare-perps-release.mjs'))};
      const release = ${JSON.stringify(canonicalRelease)};
      const profile = {
        integration: { orderInterfaceVersion: 3, aaManifestVersion: 'perps-aa-arbitrum-sepolia-mock-usdg-v2', volumeHistoryStartTimestamp: 120 },
        bundleSha256: 'a'.repeat(64),
        aa: ${JSON.stringify(rawManifest)}
      };
      process.stdout.write(JSON.stringify(prepareRelease(release, profile, 'b'.repeat(64))['perps-aa-manifest.json']));
    `
    const candidate: unknown = JSON.parse(execFileSync(process.execPath, ['--input-type=module', '-e', program], { encoding: 'utf8' }))
    expect(parsePerpsAaManifest(candidate)).toMatchObject({
      version: 'perps-aa-arbitrum-sepolia-mock-usdg-v2',
      orderInterfaceVersion: 3,
      settlementTokenSupportsEip3009: false, settlementTokenEip712Name: null, settlementTokenEip712Version: null,
      sponsorshipEnabled: false,
    })
  })

  it('uses only the canonical token input and disables owner authorization', () => {
    expect(parsePerpsAaManifest(rawManifest)).toMatchObject({
      settlementToken: expect.stringMatching(/^0x/), settlementTokenSupportsEip3009: false, settlementTokenEip712Name: null, settlementTokenEip712Version: null,
    })
  })
  it('rejects a signed-deposit capability that conflicts with release metadata', () => {
    expect(() => parsePerpsAaManifest({ ...rawManifest, settlementTokenSupportsEip3009: true, settlementTokenEip712Name: 'Mock USDG', settlementTokenEip712Version: '1' }))
      .toThrow('must match the configured USDG token capabilities')
  })
  it('rejects a stale token address', () => {
    expect(() => parsePerpsAaManifest({ ...rawManifest, settlementToken: '0x1111111111111111111111111111111111111111' }))
      .toThrow('must be the reviewed deployment')
  })
})
