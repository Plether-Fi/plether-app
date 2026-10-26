import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { prepareRelease, validateCoreRelease, writeCandidate, terraformBindings } from './prepare-perps-release.mjs'

const read = name => JSON.parse(fs.readFileSync(new URL(`../${name}`, import.meta.url)))
const legacy = read('config/perps/arbitrum-sepolia-v3.json')
const activeAa = read('apps/frontend/public/perps-aa-manifest.json')
const sha = 'a'.repeat(64)
const profile = () => ({ integration: { orderInterfaceVersion: 3, aaManifestVersion: 'perps-aa-arbitrum-sepolia-mock-usdg-v2', volumeHistoryStartTimestamp: 1800000000 }, bundleSha256: sha, aa: activeAa })
function mockRelease() {
  // Pure unit-test data. No invented address is emitted to an active release.
  const release = structuredClone(legacy)
  release.schemaVersion = 4
  release.contracts.settlementToken = { ...release.contracts.mockUsdc, artifact: 'script/DeployPerpsArbitrumSepolia.s.sol:MockUSDG', address: `0x${'1'.repeat(40)}`, runtimeCodeHash: `0x${'2'.repeat(64)}` }
  delete release.contracts.mockUsdc
  release.settlementToken = { name: 'Mock USDG', symbol: 'USDG', decimals: 6, isMock: true, supportsReceiveWithAuthorization: false, supportsBridgeFunding: false }
  release.rewards = { status: 'not_applicable', holdingContracts: ['marginClearinghouse', 'housePool'], payoutAddress: null, payoutController: null }
  return release
}

test('one canonical token binds frontend AA, backend, indexer and Terraform candidates', () => {
  const manifest = mockRelease()
  const before = JSON.stringify(manifest)
  const candidates = prepareRelease(manifest, profile(), sha)
  const token = manifest.contracts.settlementToken
  assert.equal(candidates['perps-aa-manifest.json'].usdc, token.address)
  assert.equal(candidates['perps-core.tfvars.json'].perps_usdc, token.address)
  assert.equal(candidates['perps-backend.env.json'].PERPS_USDC, token.address)
  assert.equal(candidates['activation-review.json'].settlementToken.runtimeCodeHash, token.runtimeCodeHash)
  const app = candidates['arbitrum-sepolia-v3.json']
  assert.deepEqual(app.contracts, manifest.contracts)
  assert.equal(app.release.manifestSha256, sha)
  assert.equal(app.release.bundleSha256, sha)
  assert.equal(app.integration.orderInterfaceVersion, 3)
  assert.equal(candidates['perps-aa-manifest.json'].orderInterfaceVersion, 3)
  assert.equal(app.integration.aaManifestVersion, 'perps-aa-arbitrum-sepolia-mock-usdg-v2')
  for (const [key, name] of Object.entries(terraformBindings)) assert.equal(candidates['perps-core.tfvars.json'][key], manifest.contracts[name].address)
  assert.equal(candidates['perps-backend.env.json'].PERPS_INDEXER_START_BLOCK, String(manifest.release.deploymentBlock))
  assert.equal(candidates['perps-backend.env.json'].VAULT_HISTORY_DEPLOYMENT_BLOCK, String(manifest.release.deploymentBlock))
  assert.equal(JSON.stringify(manifest), before)
})

test('candidate AA funding stays approve/deposit only and sponsorship stays disabled', () => {
  const aa = prepareRelease(mockRelease(), profile(), sha)['perps-aa-manifest.json']
  assert.equal(aa.usdcSupportsEip3009, false)
  assert.equal(aa.usdcEip712Name, null)
  assert.equal(aa.usdcEip712Version, null)
  assert.equal(aa.testnetFaucet, null)
  assert.equal(aa.sponsorshipEnabled, false)
  assert.equal(aa.pimlicoRpcUrl, undefined)
})

test('historical mockUsdc manifests continue to resolve the original token', () => {
  assert.deepEqual(validateCoreRelease(legacy).settlementToken, legacy.contracts.mockUsdc)
  assert.equal(prepareRelease(legacy, profile(), sha)['perps-aa-manifest.json'].usdc, legacy.contracts.mockUsdc.address)
  const relabeled = structuredClone(legacy); relabeled.contracts.settlementToken = relabeled.contracts.mockUsdc
  assert.throws(() => validateCoreRelease(relabeled), /Historical/)
  delete relabeled.contracts.settlementToken; relabeled.settlementToken = mockRelease().settlementToken
  assert.throws(() => validateCoreRelease(relabeled), /Historical/)
})

test('prepared, missing, unverified, or mismatched deployment data cannot be staged', () => {
  const mutations = [
    release => { delete release.orderInterfaceVersion },
    release => { release.orderInterfaceVersion = 2 },
    release => { release.release.phase = 'prepared' },
    release => { release.verification.deployedPhase.passed = false },
    release => { release.release.deploymentBlock = null },
    release => { release.contracts.settlementToken.address = null },
    release => { release.contracts.settlementToken.address = `0x${'0'.repeat(40)}` },
    release => { release.contracts.settlementToken.runtimeCodeHash = null },
    release => { release.contracts.settlementToken.runtimeCodeHash = `0x${'0'.repeat(64)}` },
    release => { release.contracts.settlementToken.artifact = 'MockUSDC' },
    release => { release.contracts.mockUsdc = legacy.contracts.mockUsdc },
    release => { release.contracts.mockUsdc = release.contracts.settlementToken; delete release.contracts.settlementToken },
    release => { release.settlementToken.supportsReceiveWithAuthorization = true },
    release => { release.settlementToken.supportsBridgeFunding = true },
    release => { release.settlementToken.decimals = 18 },
    release => { release.rewards.payoutAddress = activeAa.usdc },
    release => { release.contracts.orderRouter.address = null },
    release => { release.contracts.cfdEnginePlanner.runtimeCodeHash = null },
    release => { release.release.pyth = null },
  ]
  for (const mutate of mutations) {
    const release = mockRelease(); mutate(release)
    assert.throws(() => prepareRelease(release, profile(), sha))
  }
})

test('profile template cannot accidentally inherit old AA infrastructure', () => {
  assert.throws(() => prepareRelease(mockRelease(), read('config/perps/mock-usdg-integration-profile.example.json'), sha))
  const missing = profile(); missing.aa = { ...missing.aa, paymasterAddress: null }
  assert.throws(() => prepareRelease(mockRelease(), missing, sha), /paymasterAddress/)
  for (const version of ['unit-test-mock-usdg', 'perps-aa-test-v1', '', 42]) {
    const invalid = profile(); invalid.integration.aaManifestVersion = version
    assert.throws(() => prepareRelease(mockRelease(), invalid, sha), /manifest version/)
  }
  for (const key of ['entryPoint', 'smartAccountFactory']) {
    const invalid = profile(); invalid.aa = { ...invalid.aa, [key]: `0x${'3'.repeat(40)}` }
    assert.throws(() => prepareRelease(mockRelease(), invalid, sha), /EntryPoint or Trading Account factory/)
  }
  for (const version of [undefined, 2, 4]) {
    const invalid = profile(); invalid.integration.orderInterfaceVersion = version
    assert.throws(() => prepareRelease(mockRelease(), invalid, sha), /orderInterfaceVersion 3/)
  }
  const incompleteGraph = mockRelease(); delete incompleteGraph.contracts.cfdClosePreview
  assert.throws(() => prepareRelease(incompleteGraph, profile(), sha), /cfdClosePreview/)
  const timestamp = profile(); timestamp.integration.volumeHistoryStartTimestamp += 1
  assert.throws(() => prepareRelease(mockRelease(), timestamp, sha), /first whole minute/)

})

test('candidate writing refuses existing paths and leaves active release bytes untouched', () => {
  const temp = fs.mkdtempSync(path.join(os.tmpdir(), 'perps-usdg-candidate-test-'))
  const activePath = new URL('../config/perps/arbitrum-sepolia-v3.json', import.meta.url)
  const before = fs.readFileSync(activePath)
  try {
    const target = path.join(temp, 'candidate')
    const files = prepareRelease(mockRelease(), profile(), sha)
    writeCandidate(target, files)
    assert.deepEqual(JSON.parse(fs.readFileSync(path.join(target, 'perps-aa-manifest.json'))), files['perps-aa-manifest.json'])
    assert.throws(() => writeCandidate(target, files), /EEXIST/)
    assert.throws(() => writeCandidate(new URL('../config/perps/', import.meta.url), files), /EEXIST/)
    assert.deepEqual(fs.readFileSync(activePath), before)
  } finally { fs.rmSync(temp, { recursive: true, force: true }) }
})
