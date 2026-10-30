// Offline deployment guard. Does not query RPC, modify configuration, or deploy.
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { pathToFileURL } from 'node:url'
import { prepareRelease, terraformBindings, validateCoreRelease } from './prepare-perps-release.mjs'

const sameAddress = (actual, expected, label) => assert.equal(actual?.toLowerCase(), expected?.toLowerCase(), label)

export function validateAaDeployment(core, aa, reviewedAa = aa) {
  assert.deepEqual(aa, reviewedAa, 'Built AA manifest differs from the reviewed source')
  validateCoreRelease(core)
  const candidates = prepareRelease(core, {
    integration: core.integration,
    bundleSha256: core.release.bundleSha256,
    aa,
  }, core.release.manifestSha256)
  const expected = candidates['perps-aa-manifest.json']
  assert.deepEqual(Object.keys(aa).sort(), Object.keys(expected).sort(), 'Unexpected AA manifest fields')
  assert.equal(typeof aa.sponsorshipEnabled, 'boolean')
  assert.deepEqual({ ...aa, sponsorshipEnabled: false }, expected, 'AA manifest does not match the canonical release')
}

function environment(container) {
  const entries = container.environment ?? []
  assert.equal(new Set(entries.map(value => value.name)).size, entries.length, 'Duplicate environment entries')
  return Object.fromEntries(entries.map(value => [value.name, value.value]))
}

export function validateBackendDeployment(core, task, canonicalTask) {
  validateCoreRelease(core)
  assert.equal(core.orderInterfaceVersion, 3)
  const apis = canonicalTask.containerDefinitions.filter(value => value.name === 'plether-api')
  assert.equal(apis.length, 1, 'Expected one canonical API container')
  const canonical = environment(apis[0])
  const expected = Object.fromEntries(Object.entries(terraformBindings)
    .map(([key, name]) => [key.toUpperCase(), core.contracts[name].address]))
  expected.PERPS_CHAIN_ID = String(core.network.chainId)
  expected.PERPS_INDEXER_START_BLOCK = String(core.release.deploymentBlock)
  expected.VAULT_HISTORY_DEPLOYMENT_BLOCK = String(core.release.deploymentBlock)
  const checkBindings = env => {
    assert.equal(env.PERPS_USDC, undefined, 'Retired settlement-token environment key')
    for (const [key, value] of Object.entries(expected)) {
      if (key.endsWith('_BLOCK') || key === 'PERPS_CHAIN_ID') assert.equal(env[key], value, key)
      else sameAddress(env[key], value, key)
    }
  }
  checkBindings(canonical)
  const competitionKeys = ['INSIGHTS_ACTIVE_COMPETITION_SLUG', 'INSIGHTS_COMPETITION_RELEASE_ID']
  if (canonical.INSIGHTS_COMPETITION_RELEASE_ID) {
    assert.ok(canonical.INSIGHTS_ACTIVE_COMPETITION_SLUG, 'Bound competition requires an active slug')
    assert.equal(canonical.INSIGHTS_COMPETITION_RELEASE_ID, canonical.INSIGHTS_ACTIVE_COMPETITION_SLUG)
  }
  for (const container of task.containerDefinitions) {
    const env = environment(container)
    assert.equal(env.PERPS_USDC, undefined, 'Retired settlement-token environment key')
    if (container.name === 'plether-api' || Object.keys(expected).some(key => key in env)) {
      // Perps workers receive subsets; the API owns the complete deployment graph.
      if (container.name === 'plether-api') checkBindings(env)
      else for (const [key, value] of Object.entries(expected)) {
        if (key in env) sameAddress(env[key], value, `${container.name}.${key}`)
      }
      if (['plether-api', 'plether-perps-indexer', 'plether-insights-worker'].includes(container.name)) {
        for (const key of competitionKeys) assert.equal(env[key] ?? '', canonical[key] ?? '', key)
      }
    }
  }
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  const [mode, corePath, artifactPath, canonicalPath, ...extra] = process.argv.slice(2)
  assert.ok(corePath && artifactPath && extra.length === 0, 'Usage: validate-perps-deployment.mjs aa|backend core.json artifact.json reviewed-aa.json|canonical-task.json')
  const read = filename => JSON.parse(fs.readFileSync(filename, 'utf8'))
  if (mode === 'aa' && canonicalPath) validateAaDeployment(read(corePath), read(artifactPath), read(canonicalPath))
  else if (mode === 'backend' && canonicalPath) validateBackendDeployment(read(corePath), read(artifactPath), read(canonicalPath))
  else throw Error('Unknown deployment validation mode')
}
