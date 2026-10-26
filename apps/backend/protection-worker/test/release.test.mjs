import { it } from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { requireDeployedRelease } from '../release.mjs'
const prepared = JSON.parse(fs.readFileSync(new URL('../../../../config/perps/mock-usdg-prepared.json', import.meta.url)))
it('rejects prepared metadata before any RPC, signing, or database work', () => {
  assert.throws(() => requireDeployedRelease(prepared), /deployment is required/)
  assert.throws(() => requireDeployedRelease({ ...prepared, release: { ...prepared.release, phase: 'deployed' } }), /deployment boundary/)
})
it('requires all supplied contract pins and current token capabilities', () => {
  const deployed = { ...prepared, release: { ...prepared.release, phase: 'deployed', deploymentBlock: 123 },
    contracts: Object.fromEntries(Object.keys(prepared.contracts).map(name => [name, {address: `0x${'1'.repeat(40)}`, runtimeCodeHash: `0x${'2'.repeat(64)}`}])) }
  assert.doesNotThrow(() => requireDeployedRelease(deployed))
  assert.throws(() => requireDeployedRelease({ ...deployed, contracts: { ...deployed.contracts, settlementToken: { address: null, runtimeCodeHash: null } } }), /Undeployed/)
  assert.throws(() => requireDeployedRelease({ ...deployed, schemaVersion: 3 }), /unsupported/)
  assert.throws(() => requireDeployedRelease({ ...deployed, settlementToken: { ...deployed.settlementToken, supportsBridgeFunding: true } }), /capabilities/)
})
