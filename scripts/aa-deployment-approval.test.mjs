import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync, mkdtempSync, rmSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { tmpdir } from 'node:os'
import { join } from 'node:path'

const root = new URL('../', import.meta.url)
const workflows = ['deploy-backend', 'deploy-alto', 'aa-admin']

test('Alto recovery only accepts an unavailable single completed deployment', () => {
  const source = readFileSync(new URL('.github/workflows/deploy-alto.yml', root), 'utf8')
  const section = source.split('--arg recover "$RECOVER_UNAVAILABLE"')[1]
  const filter = section?.match(/'([\s\S]*?)' \\\n\s+"\$service_file"/)?.[1]
  assert.ok(filter, 'execute the actual ECS preflight')
  assert.match(source, /recoverUnavailable:\$recover_unavailable/)
  const fixture = () => ({
    status: 'ACTIVE', serviceName: 'plether-alto', clusterArn: 'arn:aws:ecs:ap-southeast-1:932542905614:cluster/plether-sepolia',
    platformVersion: '1.4.0', desiredCount: 1, runningCount: 0, pendingCount: 0,
    deploymentConfiguration: {minimumHealthyPercent: 0, maximumPercent: 100, deploymentCircuitBreaker: {enable: true, rollback: true}},
    networkConfiguration: {awsvpcConfiguration: {subnets: ['subnet-test'], securityGroups: ['sg-test'], assignPublicIp: 'ENABLED'}},
    deployments: [{status: 'PRIMARY', rolloutState: 'COMPLETED'}],
  })
  const cases = [
    ['explicit unavailable recovery', () => {}, true],
    ['normal deployment rejects outage', (_, a) => {a.recover = 'false'}, false],
    ['normal deployment accepts healthy service', (v, a) => {a.recover = 'false'; v.runningCount = 1}, true],
    ['normal deployment accepts stopped service', (v, a) => {a.recover = 'false'; v.desiredCount = 0}, true],
    ['recovery rejects healthy service', v => {v.runningCount = 1}, false],
    ['recovery rejects stopped service', v => {v.desiredCount = 0}, false],
    ['pending task', v => {v.pendingCount = 1}, false],
    ['in-progress rollout', v => {v.deployments[0].rolloutState = 'IN_PROGRESS'}, false],
    ['missing rollout state', v => {delete v.deployments[0].rolloutState}, false],
    ['other active deployment', v => {v.deployments.push({status: 'ACTIVE'})}, false],
    ['overlapping replicas', v => {v.deploymentConfiguration.maximumPercent = 200}, false],
    ['circuit breaker disabled', v => {v.deploymentConfiguration.deploymentCircuitBreaker.enable = false}, false],
    ['rollback disabled', v => {v.deploymentConfiguration.deploymentCircuitBreaker.rollback = false}, false],
    ['bootstrap recovery', (_, a) => {a.action = 'bootstrap-simulations'}, false],
    ['rollback recovery', (_, a) => {a.action = 'rollback'}, false],
    ['unknown recovery option', (_, a) => {a.recover = ''}, false],
  ]
  for (const [label, mutate, expected] of cases) {
    const value = fixture(), args = {cluster: 'plether-sepolia', service: 'plether-alto', action: 'deploy', recover: 'true'}
    mutate(value, args)
    const result = spawnSync('jq', ['-e', ...Object.entries(args).flatMap(([key, value]) => ['--arg', key, value]), filter], {input: JSON.stringify(value), encoding: 'utf8'})
    assert.ifError(result.error)
    assert.equal(result.status === 0, expected, `${label}: ${result.stderr}`)
  }
})

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

test('legacy Alto refuses v0.9 deploy, bootstrap and rollback before credentials', () => {
  const source = readFileSync(new URL('.github/workflows/deploy-alto.yml', root), 'utf8')
  const selection = source.match(/^\s+case "\$ALTO_ENTRYPOINT_VERSION" in\n[\s\S]*?^\s+esac$/m)?.[0]
  const refusal = source.match(/^([ \t]+)if \[ "\$ALTO_ENTRYPOINT_VERSION" != "0.8" \]; then\n[\s\S]*?^\1fi$/m)?.[0]
  assert.ok(selection, 'execute the actual workflow profile selection')
  assert.ok(refusal, 'execute the actual early workflow refusal')
  assert.ok(source.indexOf(refusal) < source.indexOf('gh api'))
  assert.ok(source.indexOf(refusal) < source.indexOf('- name: Configure AWS credentials'))
  assert.doesNotMatch(source.split('      action:')[0], /^\s+- '0.9'$/m)
  const entrypointV8 = '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108'
  const legacySelection = [entrypointV8, 'ALTO_ENTRYPOINT_SIMULATION_CONTRACT_V8', '/api/aa/rpc']
  const cases = [
    ['0.8', 'deploy', legacySelection],
    ['0.8', 'rollback', legacySelection],
    ['0.9', 'deploy', null],
    ['0.9', 'bootstrap-simulations', null],
    ['0.9', 'rollback', null],
    ['0.7', 'deploy', null],
  ]
  for (const [version, action, expected] of cases) {
    const script = [refusal, selection, 'printf "%s\\n" "$expected_entrypoint" "$simulation_name" "$smoke_path"'].join('\n')
    const result = spawnSync('bash', ['-euc', script], {
      encoding: 'utf8',
      env: {...process.env, ALTO_ENTRYPOINT_VERSION: version, ALTO_ACTION: action, ALTO_ENTRYPOINT_V8: entrypointV8},
    })
    assert.ifError(result.error)
    assert.equal(result.status === 0, expected !== null, result.stderr + result.stdout)
    if (expected) assert.deepEqual(result.stdout.trim().split('\n'), expected)
  }
})

test('Alto smoke rejects ambiguous supported EntryPoint responses', () => {
  const source = readFileSync(new URL('.github/workflows/deploy-alto.yml', root), 'utf8')
  const smoke = source.split(/^\s+authenticated_status=\$\(curl/m)[1]?.split('gas_price_status=$(curl')[0]
  const filter = smoke?.match(/'([^']+)' \\\n\s+"\$authenticated_body"/)?.[1]
  assert.ok(filter, 'execute the actual workflow smoke filter')
  const ep = '0x433709009b8330fda32311df1c2afa402ed8d009'
  const other = '0x4337084d9e255ff0702461cf8895ce9e3b5ff108'
  const cases = [
    [{result: [ep.toUpperCase()]}, true],
    [{result: [other]}, false],
    [{result: [ep, other]}, false],
    [{result: [ep, ep]}, false],
    [{result: []}, false],
    [{result: ep}, false],
    [{result: {entrypoint: ep}}, false],
    [{result: [null]}, false],
    [{error: {code: -32602}, result: [ep]}, false],
  ]
  for (const [body, expected] of cases) {
    const result = spawnSync('jq', ['-e', '--arg', 'entrypoint', ep, filter], {input: JSON.stringify(body), encoding: 'utf8'})
    assert.ifError(result.error)
    assert.equal(result.status === 0, expected, result.stderr + JSON.stringify(body))
  }
})

test('legacy Alto refuses the authenticated profile before loading any secrets', () => {
  const source = readFileSync(new URL('.github/workflows/deploy-alto.yml', root), 'utf8')
  const preflight = source.split('      - name: Validate infrastructure and Alto runtime configuration')[1]
  const refusal = preflight.match(/^([ \t]+)if jq -e --arg alto "\$ALTO_CONTAINER" '\n\s+any\(\.containerDefinitions[\s\S]*?^\1fi$/m)?.[0]
  assert.ok(refusal, 'execute the actual authenticated task refusal')
  assert.ok(preflight.indexOf(refusal) < preflight.indexOf('aws ssm get-parameter'))
  const directory = mkdtempSync(join(tmpdir(), 'plether-alto-auth-preflight-'))
  try {
    for (const useAuth of [false, true]) {
      const secrets = [{name: 'ALTO_RPC_URL', valueFrom: 'synthetic-primary'}]
      if (useAuth) secrets.push({name: 'PLETHER_ALTO_RPC_AUTH_TOKEN', valueFrom: 'synthetic-token-reference'})
      const task = {containerDefinitions: [{name: 'plether-alto', secrets}]}
      const script = ['task_definition_file="$RUNNER_TEMP/task.json"', 'cat > "$task_definition_file"', refusal].join('\n')
      const result = spawnSync('bash', ['-euc', script], {
        input: JSON.stringify(task),
        encoding: 'utf8',
        env: {...process.env, RUNNER_TEMP: directory, ALTO_CONTAINER: 'plether-alto'},
      })
      assert.ifError(result.error)
      assert.equal(result.status === 0, !useAuth, result.stdout + result.stderr)
      assert.doesNotMatch(result.stdout + result.stderr, /synthetic-token-reference/)
    }
  } finally {
    rmSync(directory, {recursive: true, force: true})
  }
})

test('backend public AA gate loads policy without cleaning generated task files', () => {
  const source = readFileSync(new URL('.github/workflows/deploy-backend.yml', root), 'utf8')
  const deploy = source.split('\n  deploy:\n')[1]
  const checkout = deploy.indexOf('- name: Check out reviewed public AA deployment policy')
  const gate = deploy.indexOf('- name: Require active Alto with reviewed validation policy for public native API')
  assert.ok(checkout > 0 && gate > checkout)
  const step = deploy.slice(checkout, gate)
  assert.match(step, /uses: actions\/checkout@[0-9a-f]{40}/)
  assert.match(step, /ref: \$\{\{ github.sha \}\}/)
  assert.match(step, /persist-credentials: false/)
  assert.match(step, /sparse-checkout: \|\n\s+\.github\/scripts\n\s+apps\/backend\/deployments/)
  assert.match(step, /path: deployment-policy/)
  assert.match(step, /matrix.environment == 'sepolia' && matrix.task_suffix == '' && steps.task-definition-status.outputs.exists == 'true'/)
  assert.match(deploy.slice(gate), /jq -e -f deployment-policy\/\.github\/scripts\/validate-aa-public-alto\.jq public-alto-task.json/)
})

test('backend consumer preflight requires the independent RPC reference for Kernel API tasks', () => {
  const source = readFileSync(new URL('.github/workflows/deploy-backend.yml', root), 'utf8')
  const inspect = source.match(/^([ \t]+)inspect_consumer\(\) \{\n[\s\S]*?^\1\}$/m)?.[0]
  assert.ok(inspect, 'execute the actual workflow consumer inspection')
  const directory = mkdtempSync(join(tmpdir(), 'plether-kernel-rpc-consumer-'))
  try {
    const script = [
      'aws() { [ "$1 $2" = "ecs describe-task-definition" ] && printf "%s\\n" "$TASK_FIXTURE"; }',
      'parameter_refs=(); execution_roles=(); rpc_modes=()',
      inspect,
      'inspect_consumer plether-sepolia plether-api false',
    ].join('\n')
    for (const [profileSource, verification, accepted] of [
      [null, null, true],
      ['environment', 'AA_SECURITY_RPC_URL', true],
      ['environment', null, false],
      ['environment', 'AA_RECONCILER_SECONDARY_RPC_URL', false],
      ['secrets', 'AA_RECONCILER_SECONDARY_RPC_URL', false],
    ]) {
      const container = {
        name: 'plether-api',
        environment: [{name: 'PERPS_CHAIN_ID', value: '421614'}, {name: 'AA_RPC_MODE', value: 'dual-independent'}],
        secrets: [{name: 'PERPS_RPC_URL', valueFrom: 'synthetic-primary'}],
      }
      if (profileSource) container[profileSource].push({name: 'AA_KERNEL_PROFILE_JSON', value: '{}', valueFrom: 'synthetic-profile'})
      if (verification) container.secrets.push({name: verification, valueFrom: 'synthetic-independent'})
      const fixture = {family: 'plether-sepolia', status: 'ACTIVE', executionRoleArn: 'synthetic-role', containerDefinitions: [container]}
      const result = spawnSync('bash', ['-euc', script], {
        cwd: root,
        encoding: 'utf8',
        env: {...process.env, RUNNER_TEMP: directory, TASK_FIXTURE: JSON.stringify(fixture)},
      })
      assert.ifError(result.error)
      assert.equal(result.status === 0, accepted, JSON.stringify([profileSource, verification, result.stdout, result.stderr]))
    }
  } finally {
    rmSync(directory, {recursive: true, force: true})
  }
})

test('backend reconciler topology enforces the attested Kernel profile and runtime limits', () => {
  const source = readFileSync(new URL('.github/workflows/deploy-backend.yml', root), 'utf8')
  const policy = source.split('name: Validate AA reconciler task topology')[1]?.match(/^\s+(def container\(\$name\):[\s\S]*?)^\s+\.family == \$family$/m)?.[1]
  assert.ok(policy, 'execute the actual workflow topology functions')
  const {profile, safeHeadLagSeconds: {minimum, maximum}} = JSON.parse(
    readFileSync(new URL('apps/backend/test/fixtures/kernel-deployment.json', root), 'utf8'),
  )
  const {maxLogRangeBlocks} = JSON.parse(readFileSync(new URL('config/rpc-log-range-policy.json', root), 'utf8'))
  for (const [batch, lag, policyMax, profileOverrides, accepted] of [
    ['2000', maximum, maxLogRangeBlocks, {}, true],
    ['2000', minimum, maxLogRangeBlocks, {}, true],
    ['2001', maximum, maxLogRangeBlocks, {}, false],
    ['1800', maximum, 1000, {}, false],
    ['2000', maximum + 1, maxLogRangeBlocks, {}, false],
    ['2000', minimum - 1, maxLogRangeBlocks, {}, false],
    ['2000', maximum, maxLogRangeBlocks, {factory: undefined}, false],
    ['2000', maximum, maxLogRangeBlocks, {staker: profile.factory}, false],
    ['2000', maximum, maxLogRangeBlocks, {staker: profile.factory, stakerCodeHash: profile.factoryCodeHash}, false],
    ['2000', maximum, maxLogRangeBlocks, {factoryCodeHash: '0x' + '0'.repeat(64)}, false],
  ]) {
    const fixture = {
      environment: Object.entries({
        HOME: '/tmp', TMPDIR: '/tmp', PERPS_CHAIN_ID: '421614', AA_RPC_MODE: 'dual-independent',
        AA_KERNEL_PROFILE_JSON: JSON.stringify({...profile, ...profileOverrides}), AA_KERNEL_SPONSORSHIP_JSON: '',
        AA_KERNEL_MAX_SAFE_LAG_SECONDS: String(lag), AA_KERNEL_RECONCILER_POLL_SECONDS: '10',
        AA_KERNEL_RECONCILER_BATCH_BLOCKS: batch, AA_KERNEL_RECONCILER_FAILURE_PAUSE_SECONDS: '60',
      }).map(([name, value]) => ({name, value})),
      secrets: ['AA_SECURITY_RPC_URL', 'DATABASE_URL', 'PERPS_RPC_URL'].map(name => ({name})),
    }
    const result = spawnSync('jq', ['-e', '--arg', 'desired_count', '0', '--argjson', 'max_log_range', String(policyMax),
      policy + ' kernel_configuration(.; envmap(.))'], {input: JSON.stringify(fixture), encoding: 'utf8'})
    assert.ifError(result.error)
    assert.equal(result.status === 0, accepted, JSON.stringify([batch, lag, policyMax, profileOverrides, result.stderr]))
  }
})

test('Kernel Alto mirrors the pinned upstream ARM64 image and scans the exact ECR digest', () => {
  const source = readFileSync(new URL('.github/workflows/build-kernel-alto.yml', root), 'utf8')
  const {alto} = JSON.parse(readFileSync(new URL('scripts/kernel-qualification/pins.json', root), 'utf8'))
  assert.equal(source.match(/^  UPSTREAM_ALTO_IMAGE: (.+)$/m)?.[1], alto.image)
  assert.equal(source.match(/^  UPSTREAM_COMMIT: (.+)$/m)?.[1], alto.commit)
  assert.match(alto.image, /^ghcr\.io\/pimlicolabs\/alto:v\d+\.\d+\.\d+@sha256:[0-9a-f]{64}$/u)
  assert.match(source, /skopeo manifest-digest/)
  assert.match(source, /skopeo copy --preserve-digests/)
  assert.match(source, /\[ "\$digest" = "\$expected_digest" \]/)
  const scan = source.split('name: Require a complete scan without critical findings')[1]
  const filter = scan?.match(/'(\.repositoryName[\s\S]*?)' \\\n\s+"\$RUNNER_TEMP\/alto-image-scan\.json"/)?.[1]
  assert.ok(filter, 'execute the exact mirrored-image scan policy')
  const repository = 'plether-kernel-alto-sepolia'
  const account = '932542905614'
  const digest = 'sha256:' + 'a'.repeat(64)
  const findings = {repositoryName: repository, registryId: account, imageId: {imageDigest: digest},
    imageScanStatus: {status: 'COMPLETE'}, imageScanFindings: {findingSeverityCounts: {CRITICAL: 0}}}
  for (const [overrides, expected] of [
    [{}, true],
    [{imageScanFindings: {findingSeverityCounts: {CRITICAL: 1}}}, false],
    [{imageScanFindings: null}, false],
    [{imageScanStatus: {status: 'IN_PROGRESS'}}, false],
    [{imageId: {imageDigest: 'sha256:' + 'b'.repeat(64)}}, false],
    [{registryId: '111111111111'}, false],
  ]) {
    const result = spawnSync('jq', ['-e', '--arg', 'digest', digest, '--arg', 'repository', repository,
      '--arg', 'account', account, filter], {input: JSON.stringify({...findings, ...overrides}), encoding: 'utf8'})
    assert.ifError(result.error)
    assert.equal(result.status === 0, expected, result.stderr)
  }
})

test('active Kernel Pages requires ETH readiness on the requested direct deployment', () => {
  const source = readFileSync(new URL('.github/workflows/deploy-kernel-canary-frontend.yml', root), 'utf8')
  const filter = source.match(/'(\s*\.jsonrpc == "2\.0" and \.id == 1[\s\S]*?)' "\$RUNNER_TEMP\/kernel-manifest\.json"/)?.[1]
  assert.ok(filter, 'execute the exact active preview manifest check')
  const {profile} = JSON.parse(readFileSync(new URL('apps/backend/test/fixtures/kernel-deployment.json', root), 'utf8'))
  const manifest = {version: 1, profile, readiness: {eth: true, sponsored: false}}
  for (const [result, expected] of [
    [manifest, true],
    [{...manifest, profile: {...profile, deployment: 'kernel-sepolia-canary-20261121'}}, false],
    [{...manifest, profile: {...profile, staker: profile.factory, stakerCodeHash: profile.factoryCodeHash}}, false],
    [{...manifest, readiness: {eth: false, sponsored: false}}, false],
  ]) {
    const outcome = spawnSync('jq', ['-e', '--arg', 'deployment', profile.deployment, filter], {
      input: JSON.stringify({jsonrpc: '2.0', id: 1, result}), encoding: 'utf8',
    })
    assert.ifError(outcome.error)
    assert.equal(outcome.status === 0, expected, outcome.stderr)
  }
})

test('Alto image scans fail closed on critical findings and incomplete or mismatched evidence', () => {
  const source = readFileSync(new URL('.github/workflows/deploy-alto.yml', root), 'utf8')
  const section = source.split('scan_findings=$(aws ecr describe-image-scan-findings')[1]
  const filter = section?.match(/'([\s\S]*?)' <<< "\$scan_findings"/)?.[1]
  assert.ok(filter, 'execute the actual workflow scan policy')
  assert.match(source, /aws ecr wait image-scan-complete/)
  const args = {
    account: '932542905614', repository: 'plether-alto-sepolia',
    digest: 'sha256:9db94fbd439a26f01b0ece3cc5f76b3791c1e1660b990d74892274267096f12a',
  }
  const fixture = () => ({
    registryId: args.account, repositoryName: args.repository,
    imageId: {imageDigest: args.digest}, imageScanStatus: {status: 'COMPLETE'},
    imageScanFindings: {findingSeverityCounts: {}, findings: []},
  })
  const criticalFinding = {name: 'CVE-2024-5535', severity: 'CRITICAL',
    attributes: [{key: 'package_name', value: 'openssl'}, {key: 'package_version', value: '3.1.4-r5'}]}
  const cases = [
    ['clean completed scan', () => {}, true],
    ['noncritical finding', v => {v.imageScanFindings.findings.push({name: 'CVE-LOW', severity: 'LOW'})}, true],
    ['former exception is rejected', v => {v.imageScanFindings = {findingSeverityCounts: {CRITICAL: 1}, findings: [criticalFinding]}}, false],
    ['missing scan', v => {delete v.imageScanFindings}, false],
    ['missing status', v => {delete v.imageScanStatus}, false],
    ['in-progress scan', v => {v.imageScanStatus.status = 'IN_PROGRESS'}, false],
    ['failed scan', v => {v.imageScanStatus.status = 'FAILED'}, false],
    ['pagination incomplete', v => {v.nextToken = 'more'}, false],
    ['missing details', v => {delete v.imageScanFindings.findings}, false],
    ['missing counts', v => {delete v.imageScanFindings.findingSeverityCounts}, false],
    ['summary omits critical', v => {v.imageScanFindings.findings.push(criticalFinding)}, false],
    ['details omit critical', v => {v.imageScanFindings.findingSeverityCounts.CRITICAL = 1}, false],
    ['malformed critical count', v => {v.imageScanFindings.findingSeverityCounts.CRITICAL = '1'}, false],
    ['null critical count', v => {v.imageScanFindings.findingSeverityCounts.CRITICAL = null}, false],
    ['unknown severity', v => {v.imageScanFindings.findings.push({name: 'CVE-BAD', severity: 'critical'})}, false],
    ['wrong scan image', v => {v.imageId.imageDigest = 'sha256:' + 'a'.repeat(64)}, false],
    ['wrong scan account', v => {v.registryId = '111111111111'}, false],
    ['wrong scan repository', v => {v.repositoryName = 'other'}, false],
  ]
  for (const [label, mutate, passed] of cases) {
    const value = fixture()
    mutate(value)
    const result = spawnSync('jq', ['-e', ...Object.entries(args).flatMap(([key, value]) => ['--arg', key, value]), filter],
      {input: JSON.stringify(value), encoding: 'utf8'})
    assert.ifError(result.error)
    assert.equal(result.status === 0, passed, label + ': ' + result.stderr)
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

test('AA admin KMS policy gate preserves document checks alongside statement bindings', () => {
  const source = readFileSync(new URL('.github/workflows/aa-admin.yml', root), 'utf8')
  const filter = source.match(/'(\s+def array:[^\n]*\n\s+def statement\(\$sid\):[\s\S]*?)' "\$task_policy_file"/)?.[1]
  assert.ok(filter, 'execute the actual KMS role policy gate')
  const kms = 'arn:aws:kms:ap-southeast-1:123456789012:key/test'
  const logs = 'arn:aws:logs:ap-southeast-1:123456789012:log-group:/ecs/plether-sepolia:log-stream:*'
  const fixture = {PolicyDocument: {Version: '2012-10-17', Statement: [
    {Sid: 'ReadPaymasterSignerPublicKey', Effect: 'Allow', Action: ['kms:DescribeKey', 'kms:GetPublicKey'], Resource: kms},
    {Sid: 'SignFixedAttestationDigest', Effect: 'Allow', Action: 'kms:Sign', Resource: kms, Condition: {StringEquals: {'kms:SigningAlgorithm': 'ECDSA_SHA_256'}}},
    {Sid: 'WriteFireLensCloudWatchCopy', Effect: 'Allow', Action: ['logs:CreateLogStream', 'logs:DescribeLogStreams', 'logs:PutLogEvents'], Resource: logs},
  ]}}
  for (const [label, mutate, expected] of [
    ['reviewed policy', () => {}, true],
    ['wrong version', v => {v.PolicyDocument.Version = 'wrong'}, false],
    ['extra statement', v => {v.PolicyDocument.Statement.push({Effect: 'Allow', Action: '*', Resource: '*'})}, false],
    ['unrestricted key', v => {v.PolicyDocument.Statement[1].Resource = '*'}, false],
    ['missing signing condition', v => {delete v.PolicyDocument.Statement[1].Condition}, false],
  ]) {
    const value = structuredClone(fixture); mutate(value)
    const result = spawnSync('jq', ['--exit-status', '--arg', 'kms', kms, '--arg', 'logs', logs, filter], {input: JSON.stringify(value), encoding: 'utf8'})
    assert.ifError(result.error)
    assert.equal(result.status === 0, expected, `${label}: ${result.stderr}`)
  }
})

for (const action of ['attest-kms', 'resume-issuance']) {
  test(`AA admin ${action} topology accepts ECS omitted-disabled fault injection and rejects unsafe drift`, () => {
    const source = readFileSync(new URL('.github/workflows/aa-admin.yml', root), 'utf8')
    const filter = source.match(/'\n(\s+def containers\(\$name\):[\s\S]*?)'\s+\\\n\s+"\$source_file"/)?.[1]
    assert.ok(filter, 'execute the complete actual capability topology gate')
    const fixture = JSON.parse(readFileSync(new URL('scripts/fixtures/aa-admin-kms-topology.json', root), 'utf8'))
    const args = {
      action, family: fixture.taskDefinition.family,
      execution_role: fixture.taskDefinition.executionRoleArn,
      task_role: fixture.taskDefinition.taskRoleArn,
      capability: 'fixed-digest-kms-attestation', app: 'plether-aa-admin',
      tmp: 'aa-admin-tmp-init', router: 'otel-log-router',
      kms: 'arn:aws:kms:ap-southeast-1:123456789012:key/00000000-0000-4000-8000-000000000001',
      database: 'arn:aws:ssm:ap-southeast-1:123456789012:parameter/plether/sepolia/database-url',
      log_parameter: 'arn:aws:ssm:ap-southeast-1:123456789012:parameter/plether/sepolia/posthog-otlp-authorization-header',
    }
    if (action === 'resume-issuance') {
      args.family = fixture.taskDefinition.family = 'plether-sepolia-aa-admin-resume-issuance'
      args.capability = 'issuance-control'
      fixture.tags.find(tag => tag.key === 'Capability').value = args.capability
      const main = fixture.taskDefinition.containerDefinitions.find(container => container.name === args.app)
      main.command = ['/usr/local/bin/plether-aa-admin', 'resume-issuance', '--expected-pause-event-id', '__WORKFLOW_REQUIRED__', '--paymaster', '__WORKFLOW_REQUIRED__', '--expected-reason', '__WORKFLOW_REQUIRED__', '--operator-note', '__WORKFLOW_REQUIRED__']
      main.environment = [{name: 'HOME', value: '/tmp'}, {name: 'TMPDIR', value: '/tmp'}]
      main.secrets = [{name: 'DATABASE_URL', valueFrom: args.database}]
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
      ['extra secret', value => { value.taskDefinition.containerDefinitions[0].secrets.push({name: 'EXTRA_SECRET', valueFrom: args.database}) }, false],
      ['extra container', value => { value.taskDefinition.containerDefinitions.push({name: 'unexpected'}) }, false],
      ...(action === 'attest-kms' ? [
        ['wrong signer', value => { value.taskDefinition.containerDefinitions[0].environment.find(item => item.name === 'AA_PAYMASTER_SIGNER_ADDRESS').value = '0x' + '0'.repeat(40) }, false],
      ] : [
        ['missing database secret', value => { value.taskDefinition.containerDefinitions[0].secrets = [] }, false],
        ['unexpected command', value => { value.taskDefinition.containerDefinitions[0].command = ['/bin/sh'] }, false],
      ]),
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
}

test('AA admin audit filters retain exact task scope and fixed-digest evidence', () => {
  const scope = adminSource.match(/'(\[\.events\[\]\?[\s\S]*?)' \\\n\s+"\$log_events_file"/)?.[1]
  const signature = adminSource.match(/'(any\(\.\[\];\n\s+\(\.signer_address[\s\S]*?)' \\\n\s+"\$evidence_file"/)?.[1]
  assert.ok(scope && signature, 'execute actual CloudWatch scope and attestation matchers')
  const args = {task: 'exact-task', container: 'plether-aa-admin', event: 'aa_admin_kms_attested', signer: '0x' + 'a'.repeat(40)}
  const jq = (filter, value) => spawnSync('jq', ['-ce', ...Object.entries(args).flatMap(([k,v]) => ['--arg', k, v]), filter], {input: JSON.stringify(value), encoding: 'utf8'})
  const record = {ecs_task_arn: args.task, container_name: args.container, event: args.event, log_schema_version: 1, level: 'INFO', signer_address: args.signer, signature_length: 65, recovery_parity: 0}
  assert.equal(JSON.parse(jq(scope, {events: [{message: JSON.stringify(record)}]}).stdout).length, 1)
  assert.equal(jq(signature, [record]).status, 0)
  for (const [field, value] of Object.entries({signer_address: 'other', signature_length: 64, recovery_parity: 2})) {
    assert.notEqual(jq(signature, [{...record, [field]: value}]).status, 0, field)
  }
})

test('AA admin registration changes only command lifecycle while preserving the essential collector', () => {
  const render = adminSource.match(/'(\.taskDefinition\n\s+\| del\([\s\S]*?)'\s+\\\n\s+"\$source_file" > "\$registration_file"/)?.[1]
  const normalize = adminSource.match(/normalization_filter='([^']*)'/)?.[1]
  const guard = adminSource.match(/'(\.family == \$family[\s\S]*?)'\s+\\\n\s+"\$registration_file"/)?.[1]
  const readback = adminSource.match(/'(\.taskDefinition.taskDefinitionArn == \$arn[\s\S]*?)'\s+\\\n\s+"\$registered_file"/)?.[1]
  assert.ok(render && normalize && guard && readback, 'execute actual registration and readback guards')
  const fixture = JSON.parse(readFileSync(new URL('scripts/fixtures/aa-admin-kms-topology.json', root), 'utf8'))
  const args = {
    family: fixture.taskDefinition.family, app: 'plether-aa-admin', tmp: 'aa-admin-tmp-init', router: 'otel-log-router',
    app_image: 'example/app@sha256:' + 'a'.repeat(64), router_image: 'example/router@sha256:' + 'b'.repeat(64),
    owner: 'aa-admin-12345-1', capability: 'fixed-digest-kms-attestation',
    arn: fixture.taskDefinition.taskDefinitionArn + '1', source: fixture.taskDefinition.taskDefinitionArn,
  }
  const command = ['/usr/local/bin/plether-aa-admin', 'attest-kms']
  const jq = (filter, value) => {
    const result = spawnSync('jq', ['-ce', ...Object.entries(args).flatMap(([k, v]) => ['--arg', k, v]), '--argjson', 'command', JSON.stringify(command), filter], {input: JSON.stringify(value), encoding: 'utf8'})
    assert.ifError(result.error)
    return result
  }
  const rendered = jq(render, fixture)
  assert.equal(rendered.status, 0, rendered.stderr)
  const registration = JSON.parse(rendered.stdout)
  assert.equal(registration.containerDefinitions.find(v => v.name === args.app).essential, false)
  assert.equal(registration.containerDefinitions.find(v => v.name === args.router).essential, true)
  assert.equal(fixture.taskDefinition.containerDefinitions.find(v => v.name === args.app).essential, true)
  assert.equal(jq(guard, registration).status, 0)
  const normalizedSource = jq('.taskDefinition | ' + normalize, fixture).stdout
  assert.deepEqual(JSON.parse(jq(normalize, registration).stdout), JSON.parse(normalizedSource))
  for (const [label, mutate] of [
    ['main made essential again', v => {v.containerDefinitions.find(c => c.name === args.app).essential = true}],
    ['router made nonessential', v => {v.containerDefinitions.find(c => c.name === args.router).essential = false}],
    ['unexpected task role', v => {v.taskRoleArn += '-other'}],
    ['altered command', v => {v.containerDefinitions.find(c => c.name === args.app).command = ['sh']}],
  ]) {
    const value = structuredClone(registration); mutate(value)
    const equivalent = JSON.stringify(JSON.parse(jq(normalize, value).stdout)) === JSON.stringify(JSON.parse(normalizedSource))
    assert.equal(jq(guard, value).status === 0 && equivalent, false, label)
  }
  const registered = {tags: registration.tags, taskDefinition: {...registration, taskDefinitionArn: args.arn, status: 'ACTIVE'}}
  assert.equal(jq(readback, registered).status, 0)
  registered.taskDefinition.containerDefinitions.find(c => c.name === args.app).essential = true
  assert.notEqual(jq(readback, registered).status, 0, 'ECS must preserve nonessential command lifecycle')
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
