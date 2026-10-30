import test from 'node:test'
import { prepareRelease, terraformBindings } from './prepare-perps-release.mjs'
import { validateAaDeployment, validateBackendDeployment } from './validate-perps-deployment.mjs'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { createHash } from 'node:crypto'
import { spawnSync } from 'node:child_process'

const read = file => readFileSync(new URL(`../${file}`, import.meta.url), 'utf8')
const manifestText = read('apps/frontend/public/perps-aa-manifest.json')
const manifest = JSON.parse(manifestText)
const coreManifest = JSON.parse(read('config/perps/mock-usdg-prepared.json'))
const runbook = read('docs/runbooks/self-hosted-aa-rollout.md')

test('client provenance matches the dependency, lockfile and installed artifact', () => {
  const record = JSON.parse(read('config/perps-aa-client-release.json'))
  const pkg = JSON.parse(read('apps/frontend/package.json'))
  const lock = JSON.parse(read('apps/frontend/package-lock.json'))
  const installed = JSON.parse(read(`apps/frontend/node_modules/${record.package}/package.json`))
  const locked = lock.packages[`node_modules/${record.package}`]
  assert.equal(record.package, '@plether-fi/perps-aa-client')
  assert.equal(pkg.dependencies[record.package], record.distribution === 'vendored' ? record.tarball : record.version)
  assert.equal(pkg.dependencies['@plether/perps-aa-client'], undefined)
  assert.equal(locked.version, record.version)
  assert.equal(locked.resolved, record.tarball)
  assert.equal(locked.integrity, record.integrity)
  assert.equal(locked.link, undefined)
  assert.equal(installed.version, record.version)
  assert.equal(installed.gitHead, record.sourceCommit)
  const provenance = record.distribution === 'vendored' ? read('config/perps/ORDER_V3_TIMING.md') : runbook
  assert.ok(provenance.includes(record.sourceCommit))
  assert.ok(provenance.includes(record.integrity))
  if (record.distribution === 'vendored') {
    const checked = spawnSync(process.execPath, ['scripts/verify-vendored-perps-aa-client.mjs'], { encoding: 'utf8' })
    assert.equal(checked.status, 0, checked.stderr)
  }
})

const deployed = () => {
  const core = JSON.parse(read('apps/frontend/src/contracts/__tests__/fixtures/perps-canonical-release.json'))
  core.release.manifestSha256 = 'a'.repeat(64)
  core.release.bundleSha256 = 'b'.repeat(64)
  core.integration = { orderInterfaceVersion: 3, aaManifestVersion: 'perps-aa-canonical-test-v2', volumeHistoryStartTimestamp: 1800000000 }
  const aa = JSON.parse(read('apps/frontend/src/contracts/__tests__/fixtures/perps-canonical-aa.json'))
  return { core, aa: prepareRelease(core, { integration: core.integration, bundleSha256: core.release.bundleSha256, aa }, core.release.manifestSha256)['perps-aa-manifest.json'] }
}

test('prepared release cannot pass either deployment guard', () => {
  assert.equal(coreManifest.release.phase, 'prepared')
  assert.equal(manifest.sponsorshipEnabled, false)
  assert.equal(manifest.settlementToken, null)
  assert.equal(manifest.paymasterAddress, null)
  assert.throws(() => validateAaDeployment(coreManifest, manifest), /Prepared manifests/)
  assert.throws(() => validateBackendDeployment(coreManifest, {}, {}), /Prepared manifests/)
})

test('frontend deployment validates the complete canonical AA identity', () => {
  const { core, aa } = deployed()
  validateAaDeployment(core, aa, aa)
  assert.throws(() => validateAaDeployment(core, { ...aa, paymasterAddress: '0x' + 'e'.repeat(40) }, aa), /reviewed source/)
  validateAaDeployment(core, { ...aa, sponsorshipEnabled: true })
  for (const invalid of [
    { ...aa, settlementToken: '0x' + 'f'.repeat(40) },
    { ...aa, settlementTokenSupportsEip3009: true },
    { ...aa, orderInterfaceVersion: 2 },
    { ...aa, preparationRpcVersion: 2 },
    { ...aa, unknown: true },
    { ...aa, usdc: aa.settlementToken },
  ]) assert.throws(() => validateAaDeployment(core, invalid))
  assert.match(read('.github/workflows/deploy-frontend.yml'), /validate-perps-deployment\.mjs aa/)
})

test('backend deployment rejects mixed releases, retired keys and duplicate configuration', () => {
  const { core } = deployed()
  const env = Object.fromEntries(Object.entries(terraformBindings).map(([key, name]) => [key.toUpperCase(), core.contracts[name].address]))
  Object.assign(env, { PERPS_CHAIN_ID: '421614', PERPS_INDEXER_START_BLOCK: String(core.release.deploymentBlock), VAULT_HISTORY_DEPLOYMENT_BLOCK: String(core.release.deploymentBlock), INSIGHTS_ACTIVE_COMPETITION_SLUG: 'future-competition', INSIGHTS_COMPETITION_RELEASE_ID: 'future-competition' })
  const task = () => ({ containerDefinitions: [{ name: 'plether-api', environment: Object.entries(env).map(([name, value]) => ({ name, value })) }] })
  validateBackendDeployment(core, task(), task())
  const wrong = task(); wrong.containerDefinitions[0].environment.find(entry => entry.name === 'PERPS_SETTLEMENT_TOKEN').value = '0x' + 'f'.repeat(40)
  assert.throws(() => validateBackendDeployment(core, wrong, task()), /PERPS_SETTLEMENT_TOKEN/)
  const duplicate = task(); duplicate.containerDefinitions[0].environment.push(duplicate.containerDefinitions[0].environment[0])
  assert.throws(() => validateBackendDeployment(core, duplicate, task()), /Duplicate/)
  const legacy = task(); legacy.containerDefinitions[0].environment.push({ name: 'PERPS_USDC', value: env.PERPS_SETTLEMENT_TOKEN })
  assert.throws(() => validateBackendDeployment(core, legacy, task()), /Retired/)
  const competition = task(); competition.containerDefinitions[0].environment.find(entry => entry.name === 'INSIGHTS_COMPETITION_RELEASE_ID').value = 'old-competition'
  assert.throws(() => validateBackendDeployment(core, competition, task()), /INSIGHTS_COMPETITION_RELEASE_ID/)
  assert.match(read('.github/workflows/deploy-backend.yml'), /validate-perps-deployment\.mjs backend/)
})

test('historical rollout checksum remains verifiable without selecting it at runtime', () => {
  const historical = read('config/perps/evidence/v1.3.1/perps-aa-manifest.json')
  const checksum = createHash('sha256').update(historical).digest('hex')
  assert.ok(runbook.includes(`  ${checksum}\n`))
  assert.notEqual(JSON.parse(historical).settlementToken, manifest.settlementToken)
})
