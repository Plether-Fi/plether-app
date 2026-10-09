import { afterEach, describe, expect, it, vi } from 'vitest'
import { arbitrum } from 'viem/chains'
import { PERPS_DEFAULT_SEPOLIA_DEPLOYMENT } from '../perpsAddresses'
import {
  isPerpsManifestForDeployment, parsePerpsDeployment, parsePerpsDeploymentJson,
  PERPS_CONTRACT_KEYS, resolvePerpsDeployment, type PerpsDeployment,
} from '../perpsDeployment'

function mainnetFixture(): PerpsDeployment {
  // Deliberately synthetic test addresses: this is not a deployable mainnet manifest.
  const deployment = structuredClone(PERPS_DEFAULT_SEPOLIA_DEPLOYMENT)
  deployment.chainId = 42161
  deployment.releaseId = 'test-mainnet-fixture'
  deployment.deploymentBlock = 123
  PERPS_CONTRACT_KEYS.forEach((key, index) => {
    deployment.contracts[key] = `0x${(index + 1).toString(16).padStart(40, '0')}`
  })
  deployment.closePreview = {
    ...deployment.closePreview,
    chainId: deployment.chainId,
    address: '0x0000000000000000000000000000000000000099',
    cfdEngine: deployment.contracts.cfdEngine,
    orderRouter: deployment.contracts.orderRouter,
    policyEvaluator: deployment.contracts.policyEvaluator,
  }
  return deployment
}

afterEach(() => { vi.unstubAllEnvs(); vi.resetModules() })

describe('perps release configuration', () => {
  it('retains the shipped release only when the override is unset', () => {
    expect(resolvePerpsDeployment(undefined, PERPS_DEFAULT_SEPOLIA_DEPLOYMENT)).toBe(PERPS_DEFAULT_SEPOLIA_DEPLOYMENT)
    expect(PERPS_DEFAULT_SEPOLIA_DEPLOYMENT.chainId).toBe(421614)
    expect(PERPS_DEFAULT_SEPOLIA_DEPLOYMENT.deploymentBlock).toBe(307397196)
  })

  it.each(['', ' ', 'invalid', '{}', '{"chainId":42161}', 'null', '[]'])('rejects incomplete override %j without falling back', value => {
    expect(() => resolvePerpsDeployment(value, PERPS_DEFAULT_SEPOLIA_DEPLOYMENT)).toThrow('Invalid perps deployment')
  })

  it('switches every compatibility export, history start block, chain and close preview together', async () => {
    const deployment = mainnetFixture()
    vi.stubEnv('VITE_PERPS_DEPLOYMENT_JSON', JSON.stringify(deployment))
    vi.resetModules()
    const registry = await import('../perpsAddresses')
    expect(registry.PERPS_ACTIVE_DEPLOYMENT).toEqual(deployment)
    expect(registry.PERPS_CHAIN).toEqual(arbitrum)
    expect(registry.PERPS_CHAIN_ID).toBe(42161)
    expect(registry.PERPS_ARBITRUM_SEPOLIA_CHAIN_ID).toBe(42161)
    expect(registry.PERPS_ARBITRUM_SEPOLIA).toEqual(deployment.contracts)
    expect(registry.PERPS_ARBITRUM_SEPOLIA_DEPLOYMENT_BLOCK).toBe(deployment.deploymentBlock)
    expect(registry.PERPS_CLOSE_PREVIEW_ADDRESS).toBe(deployment.closePreview.address)
  })

  it('accepts a complete release with a matching preview', () => {
    const deployment = mainnetFixture()
    expect(parsePerpsDeploymentJson(JSON.stringify(deployment))).toEqual(deployment)
  })

  it.each(PERPS_CONTRACT_KEYS)('requires the %s address instead of a testnet fallback', key => {
    const deployment = mainnetFixture()
    delete (deployment.contracts as Record<string, unknown>)[key]
    expect(() => parsePerpsDeployment(deployment)).toThrow('contracts must contain exactly')
  })

  it.each([1, 31337, 42161.5, '42161'])('rejects unsupported chain %j', chainId => {
    expect(() => parsePerpsDeployment({ ...mainnetFixture(), chainId })).toThrow('chainId')
  })

  it.each([0, -1, 1.5, Number.MAX_SAFE_INTEGER + 1, '123'])('rejects invalid deployment block %j', deploymentBlock => {
    expect(() => parsePerpsDeployment({ ...mainnetFixture(), deploymentBlock })).toThrow('deploymentBlock')
  })

  it.each(['', '0x123', `0x${'0'.repeat(40)}`])('rejects an invalid settlement address %j', usdc => {
    const deployment = mainnetFixture()
    expect(() => parsePerpsDeployment({ ...deployment, contracts: { ...deployment.contracts, usdc } })).toThrow('contracts.usdc')
  })

  it.each(['orderRouter', 'orderLifecycleBook', 'positionProtectionBook'] as const)('requires the %s bytecode pin', key => {
    const deployment = mainnetFixture()
    deployment.runtimeCodeHashes[key] = '0x'
    expect(() => parsePerpsDeployment(deployment)).toThrow(`runtimeCodeHashes.${key}`)
  })

  it.each(['chainId', 'cfdEngine', 'orderRouter', 'policyEvaluator'] as const)('rejects a stale preview %s binding', key => {
    const deployment = mainnetFixture()
    Object.assign(deployment.closePreview, { [key]: PERPS_DEFAULT_SEPOLIA_DEPLOYMENT.closePreview[key] })
    expect(() => parsePerpsDeployment(deployment)).toThrow(`closePreview.${key} must match`)
  })

  it('rejects a missing preview, malformed hash, evaluator alias and unknown keys', () => {
    const deployment = mainnetFixture()
    expect(() => parsePerpsDeployment({ ...deployment, closePreview: undefined })).toThrow('closePreview')
    expect(() => parsePerpsDeployment({ ...deployment, closePreview: { ...deployment.closePreview, runtimeCodeHash: '0x' } })).toThrow('runtimeCodeHash')
    expect(() => parsePerpsDeployment({ ...deployment, closePreview: { ...deployment.closePreview, address: deployment.contracts.policyEvaluator } })).toThrow('alias')
    expect(() => parsePerpsDeployment({ ...deployment, chainID: 42161 })).toThrow('must contain exactly')
    expect(() => parsePerpsDeployment({ ...deployment, contracts: { ...deployment.contracts, unknownLens: deployment.contracts.cfdEngine } })).toThrow('must contain exactly')
  })

  it('requires chain and every AA identity binding to match the active graph', () => {
    const deployment = mainnetFixture()
    const manifest = { chainId: deployment.chainId, ...deployment.contracts }
    expect(isPerpsManifestForDeployment(manifest, deployment)).toBe(true)
    expect(isPerpsManifestForDeployment({ ...manifest, chainId: 421614 }, deployment)).toBe(false)
    for (const key of ['usdc', 'marginClearinghouse', 'cfdEngine', 'orderRouter', 'orderLifecycleBook', 'positionProtectionBook', 'policyEvaluator'] as const) {
      expect(isPerpsManifestForDeployment({ ...manifest, [key]: PERPS_DEFAULT_SEPOLIA_DEPLOYMENT.contracts[key] }, deployment)).toBe(false)
    }
  })
})
