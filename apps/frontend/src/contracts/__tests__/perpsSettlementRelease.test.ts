import { describe, expect, it } from 'vitest'
import prepared from '../../../../../config/perps/mock-usdg-prepared.json'
import historical from '../../../../../config/perps/arbitrum-sepolia-v3.json'
import deployed from './fixtures/perps-canonical-release.json'
import { parsePerpsReleaseConfiguration, settlementTokenContract } from '../perpsRelease'

describe('canonical perps settlement release', () => {
  it('keeps prepared pins null and explicitly unavailable', () => {
    const parsed = parsePerpsReleaseConfiguration(prepared)
    expect(parsed.isDeployed).toBe(false)
    expect(parsed.release.deploymentBlock).toBeNull()
    expect(settlementTokenContract(parsed).address).toBeNull()
    expect(settlementTokenContract(parsed).runtimeCodeHash).toBeNull()
  })
  it('accepts only complete deployed canonical metadata', () => {
    const parsed = parsePerpsReleaseConfiguration(deployed)
    expect(parsed.isDeployed).toBe(true)
    expect(parsed.settlementToken.symbol).toBe('USDG')
    expect(settlementTokenContract(parsed)).toMatchObject(deployed.contracts.settlementToken)
  })
  it('rejects historical interfaces without relabeling or adapting them', () => {
    expect(() => parsePerpsReleaseConfiguration(historical)).toThrow('schema 4')
  })
  it.each([undefined, 2])('rejects wrong order interface %s', (orderInterfaceVersion) => {
    expect(() => parsePerpsReleaseConfiguration({ ...deployed, orderInterfaceVersion })).toThrow('interface version 3')
  })
  it.each(['name', 'decimals', 'isMock', 'supportsReceiveWithAuthorization', 'supportsBridgeFunding'])('rejects mismatched token %s', field => {
    const release = structuredClone(deployed)
    Object.assign(release.settlementToken, { [field]: 'invalid' })
    expect(() => parsePerpsReleaseConfiguration(release)).toThrow('metadata')
  })
  it.each(['address', 'runtimeCodeHash'])('rejects missing deployed %s', field => {
    const release = structuredClone(deployed)
    Object.assign(release.contracts.settlementToken, { [field]: null })
    expect(() => parsePerpsReleaseConfiguration(release)).toThrow()
  })
  it('rejects a prepared manifest that borrows old deployment pins', () => {
    const release = structuredClone(prepared)
    Object.assign(release.contracts.settlementToken, deployed.contracts.settlementToken)
    expect(() => parsePerpsReleaseConfiguration(release)).toThrow('null deployment pins')
  })
})
