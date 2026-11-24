import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import vm from 'node:vm'

const source = readFileSync(new URL('../.github/workflows/deploy-backend.yml', import.meta.url), 'utf8')
const expression = source.match(/include: \$\{\{ (fromJson\(.+) \}\}/)?.[1]
assert.ok(expression, 'exercise the actual deployment matrix')
const format = (template, ...args) => template.replaceAll('{{', '\uE000').replaceAll('}}', '\uE001')
  .replace(/\{(\d+)\}/g, (_, n) => args[Number(n)]).replaceAll('\uE000', '{').replaceAll('\uE001', '}')
const matrix = (environment, deployment_scope) => JSON.parse(JSON.stringify(vm.runInNewContext(expression, {
  inputs: { environment, deployment_scope }, fromJson: JSON.parse, format,
})))

test('workers-only scope targets the consolidated service for both sepolia and mainnet', () => {
  for (const environment of ['sepolia', 'mainnet']) {
    assert.deepEqual(matrix(environment, 'workers'), [{ environment, task_suffix: '-workers', service_name: 'plether-workers', container_name: 'plether-keeper' }])
  }
})

test('all-service and API scopes preserve parity across mainnet and sepolia', () => {
  const expected = ['plether-api', 'plether-workers', 'plether-basket-worker', 'plether-perps-indexer', 'plether-liquidation-worker', 'plether-aa-reconciler', 'plether-position-protection-worker']
  for (const environment of ['mainnet', 'sepolia']) {
    assert.deepEqual(matrix(environment, 'api'), [{ environment, task_suffix: '', service_name: 'plether-api', container_name: 'plether-api' }])
    assert.deepEqual(matrix(environment, 'all').map(x => x.service_name), expected)
  }
})

test('Pyth and LP settlement preflights allow empty contract addresses only on dark off-mode bootstrap', () => {
  const pythFilter = source.match(/--arg allow_bootstrap "\$ALLOW_MISSING_TASK_DEFINITIONS" \\\n\s+'([\s\S]*?)' \\\n\s+"\$task_def_file"/)?.[1]
  assert.ok(pythFilter, 'extract actual Pyth admission jq filter')
  const lpSection = source.split('Validate deployed LP settlement topology')[1]
  const lpFilter = lpSection?.match(/--arg expected_signer_suffix ":parameter\/plether\/\$\{TARGET_ENV\}\/lp-settlement-private-key" \\\n\s+'([\s\S]*?)' "\$task_def_file"/)?.[1]
  assert.ok(lpFilter, 'extract actual LP settlement jq filter')

  const pythDarkFixture = [{
    name: 'plether-api',
    secrets: [{ name: 'PERPS_RPC_URL', valueFrom: 'arn:aws:ssm:ap-southeast-1:366562432386:parameter/plether/mainnet/perps-rpc-url' }],
    environment: [
      { name: 'PERPS_CHAIN_ID', value: '42161' },
      { name: 'PERPS_ORDER_ROUTER', value: '' },
      { name: 'PERPS_PLETHER_ORACLE', value: '' },
    ],
  }]
  const pythConfiguredFixture = [{
    name: 'plether-api',
    secrets: [{ name: 'PERPS_RPC_URL', valueFrom: 'arn:aws:ssm:ap-southeast-1:366562432386:parameter/plether/mainnet/perps-rpc-url' }],
    environment: [
      { name: 'PERPS_CHAIN_ID', value: '42161' },
      { name: 'PERPS_ORDER_ROUTER', value: '0x1111111111111111111111111111111111111111' },
      { name: 'PERPS_PLETHER_ORACLE', value: '0x2222222222222222222222222222222222222222' },
    ],
  }]
  const runPyth = (fixture, allowBootstrap) => spawnSync('jq', [
    '-e', '--arg', 'container', 'plether-api', '--arg', 'allow_bootstrap', allowBootstrap, pythFilter,
  ], { input: JSON.stringify(fixture), encoding: 'utf8' })

  assert.equal(runPyth(pythDarkFixture, 'true').status, 0)
  assert.notEqual(runPyth(pythDarkFixture, 'false').status, 0)
  assert.equal(runPyth(pythConfiguredFixture, 'false').status, 0)
  const pythPartial = structuredClone(pythDarkFixture)
  pythPartial[0].environment[1].value = '0x1111111111111111111111111111111111111111'
  assert.notEqual(runPyth(pythPartial, 'true').status, 0)

  const lpDarkFixture = [{
    name: 'plether-keeper',
    secrets: [
      { name: 'DATABASE_URL', valueFrom: 'arn:aws:ssm:ap-southeast-1:366562432386:parameter/plether/mainnet/database-url' },
      { name: 'PERPS_RPC_URL', valueFrom: 'arn:aws:ssm:ap-southeast-1:366562432386:parameter/plether/mainnet/perps-rpc-url' },
      { name: 'KEEPER_PRIVATE_KEY', valueFrom: 'arn:aws:ssm:ap-southeast-1:366562432386:parameter/plether/mainnet/keeper-private-key' },
    ],
    environment: [
      { name: 'PERPS_CHAIN_ID', value: '42161' },
      { name: 'PERPS_HOUSE_POOL', value: '' },
      { name: 'PERPS_SENIOR_VAULT', value: '' },
      { name: 'PERPS_JUNIOR_VAULT', value: '' },
      { name: 'PERPS_SETTLEMENT_MONITOR_LENS', value: '' },
      { name: 'PERPS_ORDER_ROUTER', value: '' },
      { name: 'PERPS_CFD_ENGINE', value: '' },
      { name: 'PERPS_PLETHER_ORACLE', value: '' },
      { name: 'KEEPER_CONFIRMATIONS', value: '1' },
      { name: 'KEEPER_GAS_BUFFER_BPS', value: '12000' },
      { name: 'KEEPER_FEE_BUFFER_BPS', value: '12000' },
      { name: 'LP_SETTLEMENT_MODE', value: 'off' },
      { name: 'LP_SETTLEMENT_POLL_SECONDS', value: '15' },
      { name: 'LP_SETTLEMENT_MAX_DRAIN_TRANSACTIONS', value: '1' },
      { name: 'LP_SETTLEMENT_PENDING_REPLACEMENT_SECONDS', value: '180' },
      { name: 'LP_SETTLEMENT_MAX_REPLACEMENTS', value: '1' },
      { name: 'LP_SETTLEMENT_MAX_TX_COST_WEI', value: '0' },
    ],
  }]
  const runLp = (fixture, allowBootstrap) => spawnSync('jq', [
    '--exit-status', '--arg', 'target_env', 'mainnet', '--arg', 'allow_bootstrap', allowBootstrap,
    '--arg', 'expected_signer_suffix', ':parameter/plether/mainnet/lp-settlement-private-key', lpFilter,
  ], { input: JSON.stringify(fixture), encoding: 'utf8' })

  assert.equal(runLp(lpDarkFixture, 'true').status, 0)
  assert.notEqual(runLp(lpDarkFixture, 'false').status, 0)
  const lpObserve = structuredClone(lpDarkFixture)
  lpObserve[0].environment.find(e => e.name === 'LP_SETTLEMENT_MODE').value = 'observe'
  lpObserve[0].secrets.push({ name: 'LP_SETTLEMENT_PRIVATE_KEY', valueFrom: 'arn:aws:ssm:ap-southeast-1:366562432386:parameter/plether/mainnet/lp-settlement-private-key' })
  assert.notEqual(runLp(lpObserve, 'true').status, 0)
})

test('ECR vulnerability scan gate resolves linux/arm64 child manifest from OCI image index', () => {
  const scanSection = source.split('Verify ECR vulnerability scans (zero CRITICAL findings)')[1]
  const scanFilter = scanSection?.match(/jq -er --arg fallback "\$digest" '\n([\s\S]*?)\n\s+'\) /m)?.[1]
    ?? scanSection?.match(/jq -er --arg fallback "\$digest" '([\s\S]*?)'\)/)?.[1]
  assert.ok(scanFilter, 'extract actual ECR scan child-manifest jq filter')

  const indexDigest = 'sha256:' + '1'.repeat(64)
  const childDigest = 'sha256:' + '2'.repeat(64)
  const attestationDigest = 'sha256:' + '3'.repeat(64)
  const runSelect = (manifest) => spawnSync('jq', [
    '-er', '--arg', 'fallback', indexDigest, scanFilter,
  ], { input: JSON.stringify(manifest), encoding: 'utf8' })

  const ociIndex = {
    schemaVersion: 2,
    mediaType: 'application/vnd.oci.image.index.v1+json',
    manifests: [
      { digest: childDigest, platform: { os: 'linux', architecture: 'arm64' } },
      { digest: attestationDigest, platform: { os: 'unknown', architecture: 'unknown' } },
    ],
  }
  const indexRes = runSelect(ociIndex)
  assert.equal(indexRes.status, 0, indexRes.stderr)
  assert.equal(indexRes.stdout.trim(), childDigest)

  const singleManifest = { schemaVersion: 2, config: { digest: childDigest }, layers: [] }
  const singleRes = runSelect(singleManifest)
  assert.equal(singleRes.status, 0, singleRes.stderr)
  assert.equal(singleRes.stdout.trim(), indexDigest)

  const missingArm64 = { schemaVersion: 2, manifests: [{ digest: attestationDigest, platform: { os: 'linux', architecture: 'amd64' } }] }
  assert.notEqual(runSelect(missingArm64).status, 0)
})
