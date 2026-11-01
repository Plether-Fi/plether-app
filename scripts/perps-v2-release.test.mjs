import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { createHash } from 'node:crypto'
import { prepareRelease, validateCoreRelease } from './prepare-perps-release.mjs'
import { validateAaDeployment } from './validate-perps-deployment.mjs'

const read = file => fs.readFileSync(new URL('../' + file, import.meta.url))
const json = file => JSON.parse(read(file))
const sha = bytes => createHash('sha256').update(bytes).digest('hex')
const evidence = 'config/perps/evidence/v2.0.0/'
const core = json(evidence + 'manifest.json')
const release = json('config/perps/arbitrum-sepolia-v2.0.0.json')
const aa = json('apps/frontend/public/perps-aa-manifest.json')

test('published v2 asset and app consumer inventory retain exact checksums', () => {
  assert.equal(sha(read(evidence + 'manifest.json')), '584cdacada35f8225d2a88811edfbd9622764fe0e6da4b275ba8f33d6f8c0c74')
  const bundle = json(evidence + 'consumer-bundle.json')
  assert.equal(bundle.sourceCommit, core.release.sourceCommit)
  assert.equal(release.release.bundleSha256, sha(read(evidence + 'consumer-bundle.json')))
  for (const [file, digest] of Object.entries(bundle.files)) assert.equal(sha(read(file)), digest, file)
  const sdk = json(evidence + 'sdk-compatibility.json')
  assert.equal(sdk.artifactSourceCommit, json('config/perps-aa-client-release.json').sourceCommit)
  assert.equal(sdk.protocolSourceCommit, core.release.sourceCommit)
  assert.equal(sdk.runtimeSourcePath, 'packages/perps-aa-client/src')
})

test('one evidence/profile pair reproduces frontend, backend and Terraform configurations', () => {
  const candidate = prepareRelease(core, json(evidence + 'integration-profile.json'), sha(read(evidence + 'manifest.json')))
  assert.deepEqual(candidate['arbitrum-sepolia-v2.0.0.json'], release)
  assert.deepEqual(candidate['perps-aa-manifest.json'], aa)
  assert.deepEqual(candidate['perps-core.tfvars.json'], json('infra/terraform/sepolia-core-v2.0.0.tfvars.json'))
  assert.deepEqual(candidate['perps-backend.env.json'], json(evidence + 'backend.env.json'))
  validateAaDeployment(release, aa)
  assert.equal(aa.sponsorshipEnabled, false)
  assert.equal(aa.paymasterAddress, null)
})

test('new history begins at the observed deployment block and first following whole minute', () => {
  const block = json(evidence + 'deployment-block.json')
  assert.equal(Number(BigInt(block.number)), release.release.deploymentBlock)
  assert.equal((Number(BigInt(block.timestamp)) - Number(BigInt(block.timestamp)) % 60) + 60, release.integration.volumeHistoryStartTimestamp)
  const previous = json('config/perps/arbitrum-sepolia-v3.json')
  assert.notEqual(release.contracts.orderRouter.address, previous.contracts.orderRouter.address)
  assert.notEqual(aa.version, json('config/perps/evidence/v1.3.1/perps-aa-manifest.json').version)
})

test('complete v2 graph agrees with observed runtimes and includes the planning reader', () => {
  validateCoreRelease(core)
  assert.equal(Object.keys(core.contracts).length, 30)
  const observed = json(evidence + 'runtime-verification.json')
  assert.equal(observed.chainId, release.network.chainId)
  for (const [name, pin] of Object.entries(core.contracts)) {
    assert.deepEqual(observed.contracts[name], { address: pin.address, runtimeCodeHash: pin.runtimeCodeHash })
  }
  assert.equal(observed.bindings.planningReader, core.contracts.cfdEnginePlanningReader.address)
  assert.equal(observed.bindings.settlementTokenDecimals, 6)
  const incomplete = structuredClone(core)
  delete incomplete.contracts.cfdEnginePlanningReader
  assert.throws(() => validateCoreRelease(incomplete), /contract graph/)
})
