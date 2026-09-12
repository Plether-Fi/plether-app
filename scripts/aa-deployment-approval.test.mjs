import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync, mkdtempSync, rmSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { tmpdir } from 'node:os'
import { join } from 'node:path'

const root = new URL('../', import.meta.url)
const workflows = ['deploy-backend', 'deploy-alto', 'aa-admin']

test('Alto checks out the exact reviewed policy before invoking repository files', () => {
  const source = readFileSync(new URL('.github/workflows/deploy-alto.yml', root), 'utf8')
  const deploy = source.split('\n  deploy:\n')[1]
  const checkout = deploy.indexOf('- name: Check out reviewed deployment policy')
  const credentials = deploy.indexOf('- name: Configure AWS credentials')
  const validation = deploy.indexOf('jq -e -f .github/scripts/validate-aa-public-alto.jq')
  assert.ok(checkout > deploy.indexOf('- name: Revalidate protected environment before credentials'))
  assert.ok(credentials > checkout && validation > credentials)
  const step = deploy.slice(checkout, credentials)
  assert.match(step, /uses: actions\/checkout@[0-9a-f]{40}/)
  assert.match(step, /ref: \$\{\{ github.sha \}\}/)
  assert.match(step, /persist-credentials: false/)
  assert.match(step, /sparse-checkout: \.github\/scripts/)
})

test('Alto scan exception is exact, temporary, visible and fail-closed', () => {
  const source = readFileSync(new URL('.github/workflows/deploy-alto.yml', root), 'utf8')
  const section = source.split('scan_policy=$(jq -ce')[1]
  const filter = section?.match(/'([\s\S]*?)' <<< "\$scan_findings"/)?.[1]
  assert.ok(filter, 'execute the actual workflow scan policy')
  assert.match(section, /::warning title=Approved Sepolia image exception/)
  assert.match(section, /GITHUB_STEP_SUMMARY/)
  assert.match(source, /aws ecr wait image-scan-complete/)
  const args = {
    environment: 'sepolia', region: 'ap-southeast-1', account: '932542905614', repository: 'plether-alto-sepolia',
    digest: 'sha256:9db94fbd439a26f01b0ece3cc5f76b3791c1e1660b990d74892274267096f12a',
    upstream: 'sha256:28cee87ea6b58ba10a37273e58602b50321516c36a81d0c35d50526d1f06995d', now: 1789224700,
  }
  const fixture = () => ({registryId: args.account, repositoryName: args.repository,
    imageId: {imageDigest: args.digest}, imageScanStatus: {status: 'COMPLETE'},
    imageScanFindings: {findingSeverityCounts: {CRITICAL: 1}, findings: [{name: 'CVE-2024-5535', severity: 'CRITICAL',
      attributes: [{key: 'package_name', value: 'openssl'}, {key: 'package_version', value: '3.1.4-r5'}]}]},
  })
  const cases = [
    ['approved exact finding', () => {}, true, 1],
    ['clean completed scan', v => {v.imageScanFindings = {findingSeverityCounts: {}, findings: []}}, true, 0],
    ['other critical', v => {v.imageScanFindings.findings[0].name = 'CVE-OTHER'}, false],
    ['another critical alongside accepted one', v => {v.imageScanFindings.findings.push({name: 'CVE-OTHER', severity: 'CRITICAL'}); v.imageScanFindings.findingSeverityCounts.CRITICAL = 2}, false],
    ['duplicate accepted finding', v => {v.imageScanFindings.findings.push(structuredClone(v.imageScanFindings.findings[0])); v.imageScanFindings.findingSeverityCounts.CRITICAL = 2}, false],
    ['different package', v => {v.imageScanFindings.findings[0].attributes[0].value = 'libssl'}, false],
    ['different package version', v => {v.imageScanFindings.findings[0].attributes[1].value = '3.1.5-r0'}, false],
    ['missing package evidence', v => {delete v.imageScanFindings.findings[0].attributes}, false],
    ['duplicate package evidence', v => {v.imageScanFindings.findings[0].attributes.push({key: 'package_name', value: 'openssl'})}, false],
    ['missing scan', v => {delete v.imageScanFindings}, false],
    ['missing status', v => {delete v.imageScanStatus}, false],
    ['in-progress scan', v => {v.imageScanStatus.status = 'IN_PROGRESS'}, false],
    ['failed scan', v => {v.imageScanStatus.status = 'FAILED'}, false],
    ['pagination incomplete', v => {v.nextToken = 'more'}, false],
    ['missing details', v => {delete v.imageScanFindings.findings}, false],
    ['missing counts', v => {delete v.imageScanFindings.findingSeverityCounts}, false],
    ['missing critical count but critical detail', v => {v.imageScanFindings.findingSeverityCounts = {}}, false],
    ['summary conceals critical', v => {v.imageScanFindings.findingSeverityCounts.CRITICAL = 0}, false],
    ['details omit critical', v => {v.imageScanFindings.findings = []}, false],
    ['malformed critical count', v => {v.imageScanFindings.findingSeverityCounts.CRITICAL = '1'}, false],
    ['null critical count', v => {v.imageScanFindings.findingSeverityCounts.CRITICAL = null}, false],
    ['unknown severity', v => {v.imageScanFindings.findings[0].severity = 'critical'}, false],
    ['wrong scan image', v => {v.imageId.imageDigest = 'sha256:' + 'a'.repeat(64)}, false],
    ['wrong scan account', v => {v.registryId = '111111111111'}, false],
    ['wrong scan repository', v => {v.repositoryName = 'other'}, false],
    ...Object.entries({environment: 'mainnet', region: 'us-east-1', account: '111111111111', repository: 'other', digest: 'sha256:' + 'b'.repeat(64), upstream: 'sha256:' + 'c'.repeat(64), now: 1789776000}).map(([key, value]) => [
      `exception rejects ${key}`, (v, a) => {a[key] = value; if(key === 'account') v.registryId = value; if(key === 'repository') v.repositoryName = value; if(key === 'digest') v.imageId.imageDigest = value}, false,
    ]),
    ['last second before expiry', (_, a) => {a.now = 1789775999}, true, 1],
    ['before approval date', (_, a) => {a.now = 1789171199}, false],
    ['missing clock value', (_, a) => {a.now = null}, false],
  ]
  for (const [label, mutate, passed, accepted] of cases) {
    const value = fixture(), current = {...args}; mutate(value, current)
    const result = spawnSync('jq', ['-ce', ...Object.entries(current).flatMap(([key, value]) => key === 'now' ? ['--argjson', key, JSON.stringify(value)] : ['--arg', key, value]), filter], {input: JSON.stringify(value), encoding: 'utf8'})
    assert.ifError(result.error)
    assert.equal(result.status === 0, passed, `${label}: ${result.stderr}`)
    if (passed) assert.equal(JSON.parse(result.stdout).acceptedCriticalCount, accepted, label)
  }
})

test('Alto viem probes resolve dependencies from the packaged source workspace', () => {
  const source = readFileSync(new URL('.github/workflows/deploy-alto.yml', root), 'utf8')
  const probes = source.split(/runtime_code_hashes=\$\(|wallet_addresses=\$\(/).slice(1, 3)
  assert.equal(probes.length, 2)
  for (const probe of probes) {
    const command = probe.split("--eval '")[0]
    assert.match(command, /--workdir \/app\/src \\\n/)
    assert.match(command, /--read-only/)
    assert.match(command, /--cap-drop ALL/)
    assert.match(command, /--security-opt no-new-privileges/)
  }
})

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
