import assert from 'node:assert/strict'
import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import test from 'node:test'

const read = file => readFileSync(new URL(`../${file}`, import.meta.url))
const bytes = read('config/perps/arbitrum-sepolia-v3.json')
const manifest = JSON.parse(bytes)
const provenance = JSON.parse(read('config/perps/arbitrum-sepolia-v3.provenance.json'))

test('V3 address manifest matches the checksum published in Core PR 111', () => {
  assert.equal(createHash('sha256').update(bytes).digest('hex'), provenance.manifestSha256)
  assert.equal(provenance.manifestSha256, '6a27adcadf3a0ffda00d0061559c8820b6932a37a0cc6596e4088c7d0f3a7389')
  assert.equal(provenance.recordCommit, 'c9b121702e7228259706e1229a0300605efa8e89')
  assert.equal(manifest.release.sourceCommit, 'cf86189bcca4813f39d6405a338b77176783eb42')
  assert.equal(manifest.release.version, 'v1.3.0')
  assert.equal(manifest.network.chainId, 421614)
  assert.equal(manifest.release.deploymentBlock, 318367917)
  assert.equal(manifest.release.deploymentEndBlock, 318367988)
  assert.equal(Object.keys(manifest.contracts).length, 29)
  const addresses = Object.values(manifest.contracts).map(contract => contract.address.toLowerCase())
  assert.equal(new Set(addresses).size, 29)
  for (const contract of Object.values(manifest.contracts)) {
    assert.match(contract.address, /^0x[0-9a-f]{40}$/i)
    assert.match(contract.runtimeCodeHash, /^0x[0-9a-f]{64}$/i)
  }
})

test('pending V3 configuration does not claim activation or consumer compatibility', () => {
  assert.equal(provenance.consumerCutover, 'pending-v3-migration')
  assert.equal(provenance.tradingEnabled, false)
  assert.equal(provenance.sponsorshipEnabled, false)
  assert.equal(provenance.bridgeFundingEnabled, false)
  assert.equal(manifest.verification.seededPhase.passed, false)
  assert.equal(manifest.verification.activePhase.passed, false)
  assert.equal(manifest.release.guardian, '0x0000000000000000000000000000000000000000')

  const active = JSON.parse(read('config/perps/arbitrum-sepolia-v2.json'))
  const aa = JSON.parse(read('apps/frontend/public/perps-aa-manifest.json'))
  const overlay = JSON.parse(read('infra/terraform/sepolia-core-v1.2.3.tfvars.json'))
  assert.equal(aa.orderRouter, active.contracts.orderRouter.address)
  assert.equal(overlay.perps_order_router, active.contracts.orderRouter.address)
  assert.notEqual(aa.orderRouter, manifest.contracts.orderRouter.address)
})
