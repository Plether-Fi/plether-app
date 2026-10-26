import { execFileSync } from 'node:child_process'
import path from 'node:path'
import legacyRelease from '../../../../config/perps/arbitrum-sepolia-v3.json'
import { describe, expect, it, vi } from 'vitest'
import rawManifest from '../../public/perps-aa-manifest.json'
vi.mock('../contracts/perpsAddresses', async (importOriginal) => {
  const actual = await importOriginal<typeof import('../contracts/perpsAddresses')>()
  return { ...actual, PERPS_RELEASE: { ...actual.PERPS_RELEASE, schemaVersion: 4 },
    PERPS_SETTLEMENT_TOKEN: { name: 'Mock USDG', symbol: 'USDG', decimals: 6, isMock: true, supportsReceiveWithAuthorization: false, supportsBridgeFunding: false } }
})
import { parsePerpsAaManifest } from './manifest'

describe('MockUSDG sponsorship manifest', () => {
  it('parses a schema-4 candidate emitted by the actual offline release generator', () => {
    const program = `
      import { prepareRelease } from ${JSON.stringify(path.resolve(process.cwd(), '../../scripts/prepare-perps-release.mjs'))};
      const release = ${JSON.stringify(legacyRelease)};
      release.schemaVersion = 4;
      release.contracts.settlementToken = { ...release.contracts.mockUsdc, artifact: 'script/DeployPerpsArbitrumSepolia.s.sol:MockUSDG' };
      delete release.contracts.mockUsdc;
      release.settlementToken = { name: 'Mock USDG', symbol: 'USDG', decimals: 6, isMock: true, supportsReceiveWithAuthorization: false, supportsBridgeFunding: false };
      release.rewards = { status: 'not_applicable', holdingContracts: ['marginClearinghouse', 'housePool'], payoutAddress: null, payoutController: null };
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
      usdcSupportsEip3009: false, usdcEip712Name: null, usdcEip712Version: null,
      sponsorshipEnabled: false,
    })
  })

  it('keeps the legacy token input key while disabling owner authorization', () => {
    expect(parsePerpsAaManifest(rawManifest)).toMatchObject({
      usdc: expect.stringMatching(/^0x/), usdcSupportsEip3009: false, usdcEip712Name: null, usdcEip712Version: null,
    })
  })
  it('rejects a signed-deposit capability that conflicts with release metadata', () => {
    expect(() => parsePerpsAaManifest({ ...rawManifest, usdcSupportsEip3009: true, usdcEip712Name: 'Mock USDG', usdcEip712Version: '1' }))
      .toThrow('must match the configured settlement-token capabilities')
  })
  it('rejects a stale token address', () => {
    expect(() => parsePerpsAaManifest({ ...rawManifest, usdc: '0x1111111111111111111111111111111111111111' }))
      .toThrow('must be the reviewed deployment')
  })
})
