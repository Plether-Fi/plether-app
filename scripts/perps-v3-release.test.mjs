import assert from 'node:assert/strict'
import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import test from 'node:test'

const read = file => readFileSync(new URL(`../${file}`, import.meta.url))
const bytes = read('config/perps/evidence/v1.3.1/manifest.json')
const manifest = JSON.parse(bytes)
const provenance = JSON.parse(read('config/perps/arbitrum-sepolia-v3.provenance.json'))

test('V3 address manifest matches the checksum published in Core PR 112', () => {
  assert.equal(createHash('sha256').update(bytes).digest('hex'), provenance.manifestSha256)
  assert.equal(provenance.manifestSha256, '843c8b0fae594d8e4de63e6b22c62e23750866164c700d2deefcf588df0f40b7')
  assert.equal(provenance.recordCommit, '22f4b70ba97ebed98a2c821958caf25313771faf')
  assert.equal(manifest.release.sourceCommit, 'cf86189bcca4813f39d6405a338b77176783eb42')
  assert.equal(manifest.release.version, 'v1.3.1')
  assert.equal(manifest.network.chainId, 421614)
  assert.equal(manifest.release.deploymentBlock, 318398363)
  assert.equal(manifest.release.deploymentEndBlock, 318398657)
  assert.equal(Object.keys(manifest.contracts).length, 29)
  const addresses = Object.values(manifest.contracts).map(contract => contract.address.toLowerCase())
  assert.equal(new Set(addresses).size, 29)
  for (const contract of Object.values(manifest.contracts)) {
    assert.match(contract.address, /^0x[0-9a-f]{40}$/i)
    assert.match(contract.runtimeCodeHash, /^0x[0-9a-f]{64}$/i)
  }
})

test('active consumers all select the replacement V3 deployment', () => {
  assert.equal(provenance.consumerCutover, 'v1.3.1')
  assert.equal(manifest.verification.seededPhase.passed, true)
  assert.equal(manifest.verification.activePhase.passed, true)
  assert.equal(manifest.release.guardian, '0x0000000000000000000000000000000000000000')

  const active = JSON.parse(read('config/perps/arbitrum-sepolia-v3.json'))
  const aa = JSON.parse(read('apps/frontend/public/perps-aa-manifest.json'))
  const overlay = JSON.parse(read('infra/terraform/sepolia-core-v1.3.1.tfvars.json'))
  assert.equal(aa.orderRouter, active.contracts.orderRouter.address)
  assert.equal(overlay.perps_order_router, active.contracts.orderRouter.address)
  assert.equal(aa.orderRouter, manifest.contracts.orderRouter.address)
  assert.equal(aa.orderInterfaceVersion, 3)
})
