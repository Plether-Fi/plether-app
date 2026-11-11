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

test('workers-only scope targets exactly the existing consolidated Sepolia service', () => {
  assert.deepEqual(matrix('sepolia', 'workers'), [{ environment: 'sepolia', task_suffix: '-workers', service_name: 'plether-workers', container_name: 'plether-keeper' }])
})

test('existing all-service and API scopes preserve their deployment targets', () => {
  for (const environment of ['mainnet', 'sepolia']) {
    assert.deepEqual(matrix(environment, 'api'), [{ environment, task_suffix: '', service_name: 'plether-api', container_name: 'plether-api' }])
    const expected = environment === 'mainnet'
      ? ['plether-api', 'plether-workers', 'plether-basket-worker', 'plether-perps-indexer', 'plether-liquidation-worker', 'plether-insights-worker']
      : ['plether-api', 'plether-workers', 'plether-basket-worker', 'plether-perps-indexer', 'plether-liquidation-worker', 'plether-aa-reconciler', 'plether-position-protection-worker']
    assert.deepEqual(matrix(environment, 'all').map(x => x.service_name), expected)
  }
})

test('actual scope guard rejects mainnet workers before build acquires AWS credentials', () => {
  const build = source.split('\n  build:\n')[1].split('\n  deploy:\n')[0]
  const start = build.indexOf('- name: Validate Sepolia-only workers scope')
  const end = build.indexOf('- name: Configure AWS credentials')
  assert.ok(start > 0 && end > start)
  const body = build.slice(start, end).split('        run: |\n')[1].split('\n').map(line => line.replace(/^          /, '')).join('\n')
  for (const [environment, scope, expected] of [['sepolia', 'workers', 0], ['mainnet', 'workers', 1], ['mainnet', 'all', 0], ['mainnet', 'api', 0]]) {
    const result = spawnSync('bash', ['-c', body], { env: { ...process.env, TARGET_ENV: environment, DEPLOYMENT_SCOPE: scope }, encoding: 'utf8' })
    assert.equal(result.status, expected, result.stderr)
  }
})
