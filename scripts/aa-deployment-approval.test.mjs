import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync, mkdtempSync, rmSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { tmpdir } from 'node:os'
import { join } from 'node:path'

const root = new URL('../', import.meta.url)
const workflows = ['deploy-backend', 'deploy-alto', 'aa-admin']

test('Alto verifies raw registry bytes against the pinned root and child digests', () => {
  const source = readFileSync(new URL('.github/workflows/deploy-alto.yml', root), 'utf8')
  const helper = source.match(/          fetch_verified_manifest\(\) \{[\s\S]*?\n          \}/)?.[0]
  assert.ok(helper, 'execute the actual workflow checksum gate')
  assert.match(source, /fetch_verified_manifest "\$ALTO_UPSTREAM_IMAGE" "\$source_root_manifest"/)
  assert.match(source, /fetch_verified_manifest "\$selected_source_image" "\$source_image_manifest"/)
  assert.doesNotMatch(source, /^\s+docker manifest inspect /m)
  const directory = mkdtempSync(join(tmpdir(), 'alto-manifest-test-'))
  try {
    const cases = [
      ['OCI image', '{"schemaVersion":2,"mediaType":"application/vnd.oci.image.manifest.v1+json"}', null, 0, true],
      ['OCI index', '{"schemaVersion":2,"mediaType":"application/vnd.oci.image.index.v1+json"}\n', null, 0, true],
      ['altered registry bytes', '{"schemaVersion":2}', 'sha256:' + '0'.repeat(64), 0, false],
      ['mutable tag without digest', '{}', 'v1.2.7', 0, false],
      ['malformed digest', '{}', 'sha256:bad', 0, false],
      ['registry command failed after output', '{}', null, 1, false],
    ]
    for (const [label, body, digest, exitCode, expected] of cases) {
      const pinned = digest ?? 'sha256:' + createHash('sha256').update(body).digest('hex')
      const reference = pinned === 'v1.2.7' ? 'ghcr.io/pimlicolabs/alto:v1.2.7' : 'ghcr.io/pimlicolabs/alto@' + pinned
      const result = spawnSync('bash', ['-c', `
        set -euo pipefail
        docker() {
          test "$1 $2 $3 $4" = "buildx imagetools inspect --raw" || return 42
          test "$5" = "$REFERENCE" || return 43
          printf '%s' "$MANIFEST_BODY"
          return "$REGISTRY_EXIT"
        }
        ${helper}
        fetch_verified_manifest "$REFERENCE" "$DESTINATION"
      `], {encoding: 'utf8', env: {...process.env, REFERENCE: reference, MANIFEST_BODY: body, REGISTRY_EXIT: String(exitCode), DESTINATION: join(directory, 'manifest.json')}})
      assert.ifError(result.error)
      assert.equal(result.status === 0, expected, `${label}: ${result.stdout} ${result.stderr}`)
      if (expected) assert.equal(readFileSync(join(directory, 'manifest.json'), 'utf8'), body)
    }
  } finally {
    rmSync(directory, {recursive: true, force: true})
  }
})

const adminSource = readFileSync(new URL('.github/workflows/aa-admin.yml', root), 'utf8')
const taskFilters = [...adminSource.matchAll(/'([^']*)'\s+\\\n\s+"\$(run_result|status_file)"/g)]
test('AA admin validates overrides at both task startup and terminal readback', () => {
  assert.deepEqual(taskFilters.map(match => match[2]), ['run_result', 'status_file'])
  const helpers = taskFilters.map(match => match[1].match(/def no_effective_overrides:[\s\S]*?end end;/)?.[0])
  assert.ok(helpers[0])
  assert.equal(helpers[0], helpers[1])
  const startStep = adminSource.split('- name: Start exact one-off task without overrides')[1].split('- name: Wait for terminal state')[0]
  assert.doesNotMatch(startStep, /^\s+--overrides\b/m)
  assert.doesNotMatch(startStep, /^\s+--enable-execute-command\b/m)
})
for (const [, filter, name] of taskFilters) {
  test(`AA admin ${name}: permits only empty ECS override placeholders`, () => {
    const args = {
      definition: 'arn:aws:ecs:ap-southeast-1:123456789012:task-definition/plether-sepolia-aa-admin-kms-attest:1',
      arn: 'arn:aws:ecs:ap-southeast-1:123456789012:task/plether-sepolia/fixture',
      owner: 'aa-admin-fixture', capability: 'fixed-digest-kms-attestation',
      app: 'plether-aa-admin', init: 'aa-admin-tmp-init',
    }
    const fixture = () => ({failures: [], tasks: [{
      taskArn: args.arn, taskDefinitionArn: args.definition, startedBy: args.owner,
      launchType: 'FARGATE', platformVersion: '1.4.0', desiredStatus: 'STOPPED', lastStatus: 'STOPPED',
      tags: [{key: 'Capability', value: args.capability}, {key: 'WorkflowOwner', value: args.owner}],
      containers: [args.app, 'otel-log-router', args.init].map(name => ({name, lastStatus: 'STOPPED', exitCode: 0})),
      // Exact non-mutating shape observed from ECS despite omitting --overrides.
      overrides: {containerOverrides: [args.app, 'otel-log-router', args.init].map(name => ({name})), inferenceAcceleratorOverrides: []},
    }]})
    const cases = [
      ['ECS name-only placeholders', () => {}, true],
      ['absent overrides', t => {delete t.overrides}, true],
      ['empty overrides object', t => {t.overrides = {}}, true],
      ['empty container overrides', t => {t.overrides.containerOverrides = []}, true],
      ['subset of name-only placeholders', t => {t.overrides.containerOverrides = [{name: args.app}]}, true],
      ...[null, [], 'invalid', true].map(value => ['malformed overrides', t => {t.overrides = value}, false]),
      ...[null, {}, 'invalid', true].map(value => ['malformed container overrides', t => {t.overrides.containerOverrides = value}, false]),
      ['null container', t => {t.overrides.containerOverrides = [null]}, false],
      ['missing container name', t => {t.overrides.containerOverrides = [{}]}, false],
      ['unknown container', t => {t.overrides.containerOverrides.push({name: 'unknown'})}, false],
      ['duplicate container', t => {t.overrides.containerOverrides.push({name: args.app})}, false],
      ...Object.entries({command: ['sh'], environment: [{name: 'UNREVIEWED', value: 'yes'}], environmentFiles: [], cpu: 512, memory: 1024, memoryReservation: 128, resourceRequirements: [], unknown: true}).map(([key, value]) => [
        `container ${key} override`, t => {t.overrides.containerOverrides[0][key] = value}, false,
      ]),
      ['null command still rejected', t => {t.overrides.containerOverrides[0].command = null}, false],
      ...Object.entries({taskRoleArn: 'other-role', executionRoleArn: 'other-role', cpu: '512', memory: '1024', ephemeralStorage: {sizeInGiB: 50}, unknown: true}).map(([key, value]) => [
        `task ${key} override`, t => {t.overrides[key] = value}, false,
      ]),
      ['nonempty inference override', t => {t.overrides.inferenceAcceleratorOverrides = [{}]}, false],
      ['null inference override', t => {t.overrides.inferenceAcceleratorOverrides = null}, false],
      ['wrong immutable definition', t => {t.taskDefinitionArn += '-other'}, false],
      ['wrong platform', t => {t.platformVersion = 'LATEST'}, false],
    ]
    if (name === 'status_file') cases.push(
      ['failed main container', t => {t.containers[0].exitCode = 1}, false],
      ['failed init container', t => {t.containers[2].exitCode = 1}, false],
      ['wrong owner tag', t => {t.tags[1].value = 'other'}, false],
      ['not terminal', t => {t.lastStatus = 'RUNNING'}, false],
    )
    for (const [label, mutate, expected] of cases) {
      const value = fixture(); mutate(value.tasks[0])
      const result = spawnSync('jq', ['--exit-status', ...Object.entries(args).flatMap(([key, value]) => ['--arg', key, value]), filter], {input: JSON.stringify(value), encoding: 'utf8'})
      assert.ifError(result.error)
      assert.equal(result.status === 0, expected, `${name}, ${label}: ${result.stderr}`)
    }
  })
}

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
