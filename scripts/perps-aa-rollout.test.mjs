import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { createHash } from 'node:crypto'
import { protectionWorkerProvenance } from './migrate-perps-aa-client.mjs'

const read = file => readFileSync(new URL(`../${file}`, import.meta.url), 'utf8')
const manifestText = read('apps/frontend/public/perps-aa-manifest.json')
const manifest = JSON.parse(manifestText)
const { release, contracts } = JSON.parse(read('config/perps/arbitrum-sepolia-v2.json'))
const runbook = read('docs/runbooks/self-hosted-aa-rollout.md')

test('rollout native candidate preserves every public manifest binding', () => {
  const candidates = [...runbook.matchAll(/```json\n([\s\S]*?)\n```/g)]
    .map(match => JSON.parse(match[1]))
    .filter(value => value.paymasterVersion === 'plether-verifying-v1')
  assert.equal(candidates.length, 1)
  const { pimlicoRpcUrl, ...common } = manifest
  assert.equal(pimlicoRpcUrl, '/api/perps/v1/aa/pimlico')
  assert.deepEqual(candidates[0], {
    ...common,
    bundlerRpcUrl: '/api/perps/v1/aa/rpc',
    paymasterRpcUrl: '/api/perps/v1/aa/rpc',
    paymasterAddress: '0x_REPLACE_WITH_DEPLOYED_ADDRESS',
    paymasterVersion: 'plether-verifying-v1',
  })
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
