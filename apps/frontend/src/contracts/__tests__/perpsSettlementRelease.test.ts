import { describe, expect, it } from 'vitest'
import legacyRelease from '../../../../../config/perps/arbitrum-sepolia-v2.json'
import currentRelease from '../../../../../config/perps/arbitrum-sepolia-v3.json'
import { parsePerpsReleaseConfiguration, settlementTokenContract } from '../perpsRelease'

function mockUsdgRelease() {
  const { mockUsdc, ...contracts } = currentRelease.contracts
  return {
    ...currentRelease,
    schemaVersion: 4,
    contracts: { ...contracts, settlementToken: { ...mockUsdc,
      address: '0x1111111111111111111111111111111111111111',
      artifact: 'script/DeployPerpsArbitrumSepolia.s.sol:MockUSDG',
    } },
    settlementToken: { name: 'Mock USDG', symbol: 'USDG', decimals: 6, isMock: true,
      supportsReceiveWithAuthorization: false, supportsBridgeFunding: false },
  }
}

describe('perps settlement release identity', () => {
  it('keeps the historical USDC identity and address', () => {
    const parsed = parsePerpsReleaseConfiguration(legacyRelease)
    expect(parsed.settlementToken.symbol).toBe('USDC')
    expect(settlementTokenContract(parsed)).toMatchObject(legacyRelease.contracts.mockUsdc)
  })

  it('preserves the current v3 graph and USDC identity', () => {
    const parsed = parsePerpsReleaseConfiguration(currentRelease)
    expect(parsed.contracts.cfdClosePreview).toEqual(currentRelease.contracts.cfdClosePreview)
    expect(settlementTokenContract(parsed)).toEqual(currentRelease.contracts.mockUsdc)
  })

  it('reads schema 4 MockUSDG identity, address, codehash and capabilities together', () => {
    const release = mockUsdgRelease()
    const parsed = parsePerpsReleaseConfiguration(release)
    expect(parsed.settlementToken).toEqual(release.settlementToken)
    expect(settlementTokenContract(parsed)).toEqual(release.contracts.settlementToken)
    expect(parsed.settlementToken.supportsReceiveWithAuthorization).toBe(false)
    expect(parsed.settlementToken.supportsBridgeFunding).toBe(false)
  })

  it.each([undefined, 2])('rejects a canonical release with order interface %s', (orderInterfaceVersion) => {
    expect(() => parsePerpsReleaseConfiguration({ ...mockUsdgRelease(), orderInterfaceVersion }))
      .toThrow('requires order interface version 3')
  })

  it('rejects prepared manifests instead of treating them as an active deployment', () => {
    const release = mockUsdgRelease()
    release.release = { ...release.release, phase: 'prepared' }
    expect(() => parsePerpsReleaseConfiguration(release)).toThrow('not deployed')
  })

  it.each([
    ['name', 'USDC'], ['decimals', 18], ['isMock', false],
    ['supportsReceiveWithAuthorization', true], ['supportsBridgeFunding', true],
  ])('rejects incompatible metadata %s', (field, value) => {
    const release = mockUsdgRelease()
    Object.assign(release.settlementToken, { [field]: value })
    expect(() => parsePerpsReleaseConfiguration(release)).toThrow('Invalid MockUSDG metadata')
  })

  it('rejects relabeling a historical USDC release', () => {
    expect(() => parsePerpsReleaseConfiguration({ ...legacyRelease, settlementToken: mockUsdgRelease().settlementToken }))
      .toThrow('Historical release requires its mockUsdc contract')
  })

  it('rejects mixed canonical and historical token keys', () => {
    const release = mockUsdgRelease()
    Object.assign(release.contracts, { mockUsdc: legacyRelease.contracts.mockUsdc })
    expect(() => parsePerpsReleaseConfiguration(release)).toThrow('legacy mockUsdc')
  })

  it.each(['address', 'runtimeCodeHash'])('rejects a missing token %s', (field) => {
    const release = mockUsdgRelease()
    Object.assign(release.contracts.settlementToken, { [field]: null })
    expect(() => parsePerpsReleaseConfiguration(release)).toThrow()
  })
})
