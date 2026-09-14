import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { spawnSync } from 'node:child_process'

const workflow = readFileSync(new URL('../.github/workflows/candle-admin.yml', import.meta.url), 'utf8')
const apiStep = workflow.split('- name: Resolve deployed task and network')[1]
const filter = apiStep?.match(/'\(\[\.containerDefinitions\[\][\s\S]*?' \\\n\s+"\$task_definition_file"/)?.[0].split("' \\\n")[0].slice(1)
assert.ok(filter, 'extract the actual API task identity check from the workflow')

function taskDefinition(environment = 'sepolia', roleKind = 'api') {
  const prefix = `arn:aws:iam::932542905614:role/plether-${environment}-${roleKind}`
  return {
    taskRoleArn: `${prefix}-task`,
    executionRoleArn: `${prefix}-execution`,
    containerDefinitions: [{
      name: 'plether-api',
      image: `932542905614.dkr.ecr.ap-southeast-1.amazonaws.com/plether-api-${environment}:${'a'.repeat(40)}`,
    }],
  }
}

function accepts(definition, environment = 'sepolia') {
  const result = spawnSync('jq', ['--exit-status', '--arg', 'container', 'plether-api',
    '--arg', 'environment', environment, filter], {
    input: JSON.stringify(definition), encoding: 'utf8',
  })
  assert.ifError(result.error)
  assert.ok([0, 1, 5].includes(result.status), result.stderr)
  return result.status === 0
}

for (const environment of ['sepolia', 'mainnet']) {
  for (const roleKind of ['api', 'ecs']) {
    test(`accepts ${environment} ${roleKind} role pair with a commit-pinned image`, () => {
      assert.equal(accepts(taskDefinition(environment, roleKind), environment), true)
    })
  }
}

test('accepts a deployed digest and required sidecar', () => {
  const definition = taskDefinition()
  definition.containerDefinitions[0].image = `example.com/api@sha256:${'b'.repeat(64)}`
  definition.containerDefinitions.push({ name: 'otel-log-router', image: 'example.com/logs:latest' })
  assert.equal(accepts(definition), true)
})

for (const [name, mutate] of [
  ['legacy task role with API execution role', d => { d.taskRoleArn = d.taskRoleArn.replace('-api-task', '-ecs-task') }],
  ['API task role with legacy execution role', d => { d.executionRoleArn = d.executionRoleArn.replace('-api-execution', '-ecs-execution') }],
  ['other environment', d => { d.taskRoleArn = d.taskRoleArn.replace('sepolia', 'mainnet'); d.executionRoleArn = d.executionRoleArn.replace('sepolia', 'mainnet') }],
  ['execution role from another account', d => { d.executionRoleArn = d.executionRoleArn.replace('932542905614', '111111111111') }],
  ['unrelated role pair', d => { d.taskRoleArn = d.taskRoleArn.replace('-api-task', '-admin-task'); d.executionRoleArn = d.executionRoleArn.replace('-api-execution', '-admin-execution') }],
  ['missing role', d => { delete d.taskRoleArn }],
  ['missing execution role', d => { delete d.executionRoleArn }],
  ['duplicate API containers', d => { d.containerDefinitions.push(d.containerDefinitions[0]) }],
  ['missing API container', d => { d.containerDefinitions = [] }],
  ...['latest', 'master', 'abc1234', 'a'.repeat(39)].map(tag => [
    `mutable or invalid image tag ${tag}`, d => { d.containerDefinitions[0].image = `example.com/api:${tag}` },
  ]),
]) {
  test(`rejects ${name}`, () => {
    const definition = taskDefinition()
    mutate(definition)
    assert.equal(accepts(definition), false)
  })
}
