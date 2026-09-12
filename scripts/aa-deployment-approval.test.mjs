import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { spawnSync } from 'node:child_process'

const root = new URL('../', import.meta.url)
const workflows = ['deploy-backend', 'deploy-alto', 'aa-admin']

test('AA admin topology accepts ECS omitted-disabled fault injection and rejects unsafe drift', () => {
  const source = readFileSync(new URL('.github/workflows/aa-admin.yml', root), 'utf8')
  const filter = source.match(/'\n(\s+def containers\(\$name\):[\s\S]*?)'\s+\\\n\s+"\$source_file"/)?.[1]
  assert.ok(filter, 'execute the complete actual capability topology gate')
  const fixture = JSON.parse(readFileSync(new URL('scripts/fixtures/aa-admin-kms-topology.json', root), 'utf8'))
  const args = {
    action: 'attest-kms', family: fixture.taskDefinition.family,
    execution_role: fixture.taskDefinition.executionRoleArn,
    task_role: fixture.taskDefinition.taskRoleArn,
    capability: 'fixed-digest-kms-attestation', app: 'plether-aa-admin',
    tmp: 'aa-admin-tmp-init', router: 'otel-log-router',
    kms: 'arn:aws:kms:ap-southeast-1:123456789012:key/00000000-0000-4000-8000-000000000001',
    database: 'arn:aws:ssm:ap-southeast-1:123456789012:parameter/plether/sepolia/database-url',
    log_parameter: 'arn:aws:ssm:ap-southeast-1:123456789012:parameter/plether/sepolia/posthog-otlp-authorization-header',
  }
  const cases = [
    ['omitted disabled field', () => {}, true],
    ['explicit false', value => { value.taskDefinition.enableFaultInjection = false }, true],
    ...[true, null, 'false', 0, {}, []].map(field => [
      `invalid fault injection ${JSON.stringify(field)}`,
      value => { value.taskDefinition.enableFaultInjection = field }, false,
    ]),
    ['wrong role', value => { value.taskDefinition.taskRoleArn += '-other' }, false],
    ['wrong family', value => { value.taskDefinition.family += '-other' }, false],
    ['unexpected tag', value => { value.tags.push({key: 'Other', value: 'yes'}) }, false],
    ['root main process', value => { value.taskDefinition.containerDefinitions[0].user = '0' }, false],
    ['extra secret', value => { value.taskDefinition.containerDefinitions[0].secrets.push({name: 'DATABASE_URL', valueFrom: args.database}) }, false],
    ['extra container', value => { value.taskDefinition.containerDefinitions.push({name: 'unexpected'}) }, false],
    ['wrong signer', value => { value.taskDefinition.containerDefinitions[0].environment.find(item => item.name === 'AA_PAYMASTER_SIGNER_ADDRESS').value = '0x' + '0'.repeat(40) }, false],
  ]
  for (const [label, mutate, expected] of cases) {
    const value = structuredClone(fixture)
    mutate(value)
    const result = spawnSync('jq', ['--exit-status', ...Object.entries(args).flatMap(([key, value]) => ['--arg', key, value]), filter], {
      input: JSON.stringify(value), encoding: 'utf8', env: {...process.env, AWS_REGION: 'ap-southeast-1'},
    })
    assert.ifError(result.error)
    assert.equal(result.status === 0, expected, `${label}: ${result.stderr}`)
  }
})

test('frontend health gate supports the exact legacy wrapper without accepting unhealthy bodies', () => {
  const source = readFileSync(new URL('.github/workflows/deploy-frontend.yml', root), 'utf8')
  const filter = source.match(/! jq --exit-status '([\s\S]*?)' "\$health_body"/)?.[1]
  assert.ok(filter, 'extract the actual frontend health gate')
  assert.match(source, /\[ "\$health_status" != "200" \]/)
  for (const [body, expected] of [
    [{ status: 'ok' }, true],
    [JSON.stringify({ status: 'ok' }), true],
    [{ status: 'failed' }, false],
    [JSON.stringify({ status: 'failed' }), false],
    [JSON.stringify(JSON.stringify({ status: 'ok' })), false],
    ['not json', false],
    ['ok', false],
    [null, false],
    [[], false],
    [true, false],
    [{}, false],
  ]) {
    const result = spawnSync('jq', ['--exit-status', filter], {
      input: JSON.stringify(body), encoding: 'utf8',
    })
    assert.ifError(result.error)
    assert.equal(result.status === 0, expected, result.stderr)
  }
  const malformed = spawnSync('jq', ['--exit-status', filter], { input: '{', encoding: 'utf8' })
  assert.notEqual(malformed.status, 0)
})

for (const workflow of workflows) {
  const source = readFileSync(new URL(`.github/workflows/${workflow}.yml`, root), 'utf8')
  // Execute the actual inline jq filters, both before and after human approval.
  const filters = [...source.matchAll(/'(\.name == \$name\n[\s\S]*?)'\s+\\/g)].map(match => match[1])
  test(`${workflow}: checks the environment before and after approval`, () => {
    assert.equal(filters.length, 2)
    assert.match(source, /GITHUB_REF.*refs\/heads\/master/)
  })
  for (const [index, filter] of filters.entries()) {
    const valid = () => ({
      name: 'test-admin-sepolia',
      can_admins_bypass: false,
      protection_rules: [{
        type: 'required_reviewers',
        prevent_self_review: false,
        reviewers: [{ type: 'User', reviewer: { id: 41941, login: 'Stanley' } }],
      }],
      deployment_branch_policy: { custom_branch_policies: true },
    })
    const cases = [
      ['sole maintainer explicitly approves', () => {}, true],
      ['administrator bypass', value => { value.can_admins_bypass = true }, false],
      ['no reviewers', value => { value.protection_rules[0].reviewers = [] }, false],
      ['no review rule', value => { value.protection_rules = [] }, false],
      ['duplicate review rules', value => { value.protection_rules.push(value.protection_rules[0]) }, false],
      ['wrong environment', value => { value.name = 'test-admin-mainnet' }, false],
      ['unrestricted branches', value => { value.deployment_branch_policy.custom_branch_policies = false }, false],
      ['missing self-review policy', value => { delete value.protection_rules[0].prevent_self_review }, false],
      ['incompatible independent-only policy', value => { value.protection_rules[0].prevent_self_review = true }, false],
    ]
    for (const [label, mutate, expected] of cases) {
      test(`${workflow} gate ${index + 1}: ${label}`, () => {
        const value = valid()
        mutate(value)
        const result = spawnSync('jq', ['--exit-status', '--arg', 'name', 'test-admin-sepolia', filter], {
          input: JSON.stringify(value), encoding: 'utf8',
        })
        assert.ifError(result.error)
        assert.ok([0, 1].includes(result.status), result.stderr)
        assert.equal(result.status === 0, expected, result.stderr)
      })
    }
  }
}
