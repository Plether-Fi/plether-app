import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
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
