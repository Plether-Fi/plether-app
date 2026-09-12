import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { createHash } from 'node:crypto'
import { spawnSync } from 'node:child_process'
import { protectionWorkerProvenance } from './migrate-perps-aa-client.mjs'

const read = file => readFileSync(new URL(`../${file}`, import.meta.url), 'utf8')
const manifestText = read('apps/frontend/public/perps-aa-manifest.json')
const manifest = JSON.parse(manifestText)
const { release, contracts } = JSON.parse(read('config/perps/arbitrum-sepolia-v2.json'))
const runbook = read('docs/runbooks/self-hosted-aa-rollout.md')

test('published client provenance matches the dependency, lockfile and installed artifact', () => {
  const record = JSON.parse(read('config/perps-aa-client-release.json'))
  const pkg = JSON.parse(read('apps/frontend/package.json'))
  const lock = JSON.parse(read('apps/frontend/package-lock.json'))
  const installed = JSON.parse(read(`apps/frontend/node_modules/${record.package}/package.json`))
  const locked = lock.packages[`node_modules/${record.package}`]
  assert.equal(record.package, '@plether-fi/perps-aa-client')
  assert.equal(pkg.dependencies[record.package], record.version)
  assert.equal(pkg.dependencies['@plether/perps-aa-client'], undefined)
  assert.equal(locked.version, record.version)
  assert.equal(locked.resolved, record.tarball)
  assert.equal(locked.integrity, record.integrity)
  assert.equal(locked.link, undefined)
  assert.equal(installed.version, record.version)
  assert.equal(installed.gitHead, record.sourceCommit)
  assert.ok(runbook.includes(record.sourceCommit))
  assert.ok(runbook.includes(record.integrity))
})

test('native activation preserves every reviewed candidate binding and enables fast preparation', () => {
  const candidates = [...runbook.matchAll(/```json\n([\s\S]*?)\n```/g)]
    .map(match => JSON.parse(match[1]))
    .filter(value => value.paymasterVersion === 'plether-verifying-v1')
  assert.equal(candidates.length, 1)
  assert.equal('pimlicoRpcUrl' in manifest, false)
  assert.deepEqual(manifest, {
    ...candidates[0],
    paymasterAddress: '0x9761091045616A388f5fE1433721B272c78fe31b',
    preparationRpcVersion: 1,
  })
  assert.equal(manifest.bundlerRpcUrl, '/api/perps/v1/aa/rpc')
  assert.equal(manifest.paymasterRpcUrl, manifest.bundlerRpcUrl)
})

test('deployment validator accepts the activated capability and rejects altered native profiles', () => {
  const workflow = read('.github/workflows/deploy-frontend.yml')
  const section = workflow.split('name: Validate testnet AA manifest')[1].split('name: Validate testnet backend origin')[0]
  const filter = section.match(/jq -e '([\s\S]*?)' dist\/perps-aa-manifest.json/)[1]
  const accepts = value => spawnSync('jq', ['-e', filter], { input: JSON.stringify(value), encoding: 'utf8' }).status === 0
  assert.equal(accepts(manifest), true)
  const { preparationRpcVersion, ...withoutCapability } = manifest
  for (const invalid of [withoutCapability, { ...manifest, preparationRpcVersion: 2 },
    { ...manifest, paymasterAddress: '0x1111111111111111111111111111111111111111' },
    { ...manifest, pimlicoRpcUrl: '/api/perps/v1/aa/pimlico' },
    { ...manifest, unknown: true }]) assert.equal(accepts(invalid), false)
})

test('rollout identity and public artifact checksum match the deployment', () => {
  for (const [label, contract] of Object.entries({
    USDC: 'mockUsdc', 'Order router': 'orderRouter', 'CFD engine': 'cfdEngine',
    'Margin clearinghouse': 'marginClearinghouse', 'Order lifecycle book': 'orderLifecycleBook',
    'Policy evaluator': 'cfdOrderPolicyEvaluator', 'Position protection book': 'positionProtectionBook',
  })) assert.ok(runbook.includes(`| ${label} | \`${contracts[contract].address}\` |`), label)
  assert.ok(runbook.includes(`Core deployment: **${release.version}**`))
  assert.ok(runbook.includes(`deployment block \`${release.deploymentBlock}\``))
  assert.ok(runbook.includes(`\`${release.sourceCommit}\``))
  const versions = [...runbook.matchAll(/perps-aa-arbitrum-sepolia-\d{8}-v2/g)].map(match => match[0])
  assert.ok(versions.length > 0)
  assert.deepEqual([...new Set(versions)], [manifest.version])
  const checksum = createHash('sha256').update(manifestText).digest('hex')
  assert.ok(runbook.includes(`  ${checksum}\n`))
})

test('package migration retains current checksum-verified worker provenance', () => {
  const provenance = protectionWorkerProvenance(release)
  assert.ok(read('docs/runbooks/position-protection.md').includes(provenance))
  assert.ok(provenance.includes(`/path/to/perps-${release.version}-arbitrum-sepolia.tar.gz`))
  assert.ok(provenance.includes(release.bundleSha256))
  assert.ok(provenance.includes(release.sourceCommit))
  const future = { ...release, version: 'v9.0.0', sourceCommit: 'a'.repeat(40), bundleSha256: 'b'.repeat(64) }
  assert.ok(protectionWorkerProvenance(future).includes('perps-v9.0.0-arbitrum-sepolia.tar.gz'))
  assert.throws(() => protectionWorkerProvenance({ ...release, bundleSha256: null }))
})
