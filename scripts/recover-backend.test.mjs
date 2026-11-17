import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { createHash } from 'node:crypto'
import { spawnSync } from 'node:child_process'
import {
  ACCOUNT, REGION, CLUSTER, SERVICES, qualifiedRelease, validateInputs,
  inputFingerprint, validateProtectionPolicy, validateServiceSnapshot,
  validateTaskDefinition, configurationFingerprint, renderRecoveredTaskDefinition,
  validateImageOnlyChange, verifyImageGraph, verifyRunningTask, progressEvidence,
  manifestFromBatchGetImage,
} from './recover-backend.mjs'
import { terraformBindings } from './prepare-perps-release.mjs'

const root = new URL('../', import.meta.url)
const core = JSON.parse(fs.readFileSync(new URL('config/perps/arbitrum-sepolia-v2.0.0.json', root)))
const catalog = JSON.parse(fs.readFileSync(new URL('config/operations/backend-recovery-releases.json', root)))
const serviceName = 'plether-aa-reconciler'
const arn = `arn:aws:ecs:${REGION}:${ACCOUNT}:task-definition/plether-sepolia-aa-reconciler:67`
const clone = value => structuredClone(value)
const release = () => qualifiedRelease(catalog, 'sepolia-recovery-2026-11-17', serviceName, core)
const input = () => ({ service: serviceName, releaseId: 'sepolia-recovery-2026-11-17', expectedTaskDefinition: arn, expectedConfigurationSha256: 'a'.repeat(64), confirmation: 'RECOVER BACKEND ON SEPOLIA', operatorNote: 'Reviewed missing-image incident recovery' })
const image = `${ACCOUNT}.dkr.ecr.${REGION}.amazonaws.com/plether-api-sepolia:old`
const canonical = () => ({ containerDefinitions: [{ name: 'plether-api', environment: [
  ...Object.entries(terraformBindings).map(([key, name]) => ({ name: key.toUpperCase(), value: core.contracts[name].address })),
  { name: 'PERPS_CHAIN_ID', value: '421614' },
  { name: 'PERPS_INDEXER_START_BLOCK', value: String(core.release.deploymentBlock) },
  { name: 'VAULT_HISTORY_DEPLOYMENT_BLOCK', value: String(core.release.deploymentBlock) },
] }] })
const task = () => ({
  taskDefinitionArn: arn, revision: 67, family: 'plether-sepolia-aa-reconciler', status: 'ACTIVE',
  networkMode: 'awsvpc', cpu: '256', memory: '512', requiresCompatibilities: ['FARGATE'],
  runtimePlatform: { cpuArchitecture: 'ARM64', operatingSystemFamily: 'LINUX' },
  executionRoleArn: `arn:aws:iam::${ACCOUNT}:role/plether-sepolia-aa-reconciler-execution`,
  taskRoleArn: `arn:aws:iam::${ACCOUNT}:role/plether-sepolia-aa-reconciler-task`,
  containerDefinitions: [
    { name: serviceName, image, command: SERVICES[serviceName].command, essential: true, readonlyRootFilesystem: true,
      environment: [{ name: 'PERPS_CHAIN_ID', value: '421614' }],
      secrets: [{ name: 'DATABASE_URL', valueFrom: `arn:aws:ssm:${REGION}:${ACCOUNT}:parameter/plether/sepolia/database-url` }] },
    { name: 'aa-reconciler-tmp-init', image, essential: false, readonlyRootFilesystem: true, user: '0:0', entryPoint: ['/bin/sh', '-c'], command: ['chmod 1777 /tmp'] },
    { name: 'otel-log-router', image: `${ACCOUNT}.dkr.ecr.${REGION}.amazonaws.com/plether-otel-log-router-sepolia:old`, essential: true, firelensConfiguration: { type: 'fluentbit' }, environment: [{ name: 'SERVICE_VERSION', value: 'b'.repeat(40) }] },
  ],
})
const snapshot = () => ({ failures: [], services: [{
  serviceArn: `arn:aws:ecs:${REGION}:${ACCOUNT}:service/${CLUSTER}/${serviceName}`,
  serviceName, clusterArn: `arn:aws:ecs:${REGION}:${ACCOUNT}:cluster/${CLUSTER}`, status: 'ACTIVE',
  taskDefinition: arn, launchType: 'FARGATE', platformVersion: '1.4.0', desiredCount: 1, runningCount: 0, pendingCount: 0,
  deploymentController: { type: 'ECS' }, schedulingStrategy: 'REPLICA',
  deploymentConfiguration: { minimumHealthyPercent: 0, maximumPercent: 100, deploymentCircuitBreaker: { enable: true, rollback: true } },
  networkConfiguration: { awsvpcConfiguration: { subnets: ['subnet-abcdef'], securityGroups: ['sg-abcdef'], assignPublicIp: 'ENABLED' } },
  deployments: [{ status: 'PRIMARY', rolloutState: 'COMPLETED', taskDefinition: arn }],
}] })

test('qualified recovery catalog binds the exact service, source, region and Core release', () => {
  assert.equal(release().sourceCommit, '0713acef754f1413352bcf1ece9d350ee717dd17')
  for (const mutate of [
    value => { value.accountId = '111111111111' },
    value => { value.region = 'us-east-1' },
    value => { value.releases[0].coreManifestSha256 = 'f'.repeat(64) },
    value => { value.releases[0].services = ['plether-api'] },
    value => { value.releases[0].applicationImage = image },
    value => { value.releases.push(value.releases[0]) },
    value => { value.releases[0].qualificationEvidence = [] },
  ]) {
    const value = clone(catalog); mutate(value)
    assert.throws(() => qualifiedRelease(value, 'sepolia-recovery-2026-11-17', serviceName, core))
  }
})

test('approval fingerprint binds service, baseline, exact release digests and incident note', () => {
  const original = inputFingerprint(input(), release())
  for (const field of ['expectedConfigurationSha256', 'operatorNote']) {
    const value = input(); value[field] = field === 'operatorNote' ? 'Different incident' : 'c'.repeat(64)
    assert.notEqual(inputFingerprint(value, release()), original)
  }
  const other = clone(release()); other.applicationImage = other.applicationImage.replace(/b901c9b3/, 'c901c9b3')
  assert.notEqual(inputFingerprint(input(), other), original)
  const wrong = input(); wrong.expectedTaskDefinition = wrong.expectedTaskDefinition.replace(':67', ':68')
  assert.notEqual(inputFingerprint(wrong, release()), original)
  for (const mutate of [
    value => { value.service = 'plether-api' },
    value => { value.expectedTaskDefinition = 'arn:aws:ecs:us-east-1:111111111111:task-definition/other:1' },
    value => { value.confirmation = 'RECOVER MAINNET' },
    value => { value.operatorNote = 'hidden\ncommand' },
  ]) { const value = input(); mutate(value); assert.throws(() => validateInputs(value)) }
})

test('the protected environment requires maintainer approval and exactly master', () => {
  const environment = { name: 'backend-admin-sepolia', can_admins_bypass: false,
    deployment_branch_policy: { protected_branches: false, custom_branch_policies: true },
    protection_rules: [{ type: 'required_reviewers', prevent_self_review: false, reviewers: [{ type: 'User', reviewer: { login: 'Stanley' } }] }] }
  const branches = { total_count: 1, branch_policies: [{ name: 'master', type: 'branch' }] }
  validateProtectionPolicy(environment, branches)
  for (const mutate of [
    value => { value.can_admins_bypass = true },
    value => { value.protection_rules[0].reviewers = [] },
    value => { value.protection_rules = [] },
    value => { value.deployment_branch_policy.custom_branch_policies = false },
  ]) { const value = clone(environment); mutate(value); assert.throws(() => validateProtectionPolicy(value, branches)) }
  assert.throws(() => validateProtectionPolicy(environment, { total_count: 2, branch_policies: [...branches.branch_policies, { name: '*', type: 'branch' }] }))
})

test('a stopped-by-failure singleton is recoverable but concurrent or different services are refused', () => {
  validateServiceSnapshot(snapshot(), input())
  for (const [label, mutate] of [
    ['wrong family', value => { value.taskDefinition = arn.replace('aa-reconciler', 'liquidation-worker') }],
    ['changed revision', value => { value.taskDefinition = arn.replace(':67', ':68') }],
    ['stopped by operator', value => { value.desiredCount = 0 }],
    ['pending placement', value => { value.pendingCount = 1 }],
    ['overlapping replicas', value => { value.runningCount = 2 }],
    ['overlap deployment', value => { value.deploymentConfiguration.maximumPercent = 200 }],
    ['concurrent rollout', value => { value.deployments[0].rolloutState = 'IN_PROGRESS' }],
    ['second deployment', value => { value.deployments.push({ status: 'ACTIVE' }) }],
    ['wrong region', value => { value.clusterArn = value.clusterArn.replace(REGION, 'us-east-1') }],
    ['disabled breaker', value => { value.deploymentConfiguration.deploymentCircuitBreaker.enable = false }],
    ['external deployment controller', value => { value.deploymentController.type = 'CODE_DEPLOY' }],
  ]) {
    const value = snapshot(); mutate(value.services[0])
    assert.throws(() => validateServiceSnapshot(value, input()), undefined, label)
  }
})

test('baseline fingerprint includes all immutable task settings and mutable service network', () => {
  const service = snapshot().services[0], baseline = task(), original = configurationFingerprint(service, baseline)
  const reordered = Object.fromEntries(Object.entries(service).reverse())
  assert.equal(configurationFingerprint(reordered, baseline), original)
  for (const mutate of [
    value => { value.taskRoleArn = 'another role' },
    value => { value.containerDefinitions[0].secrets[0].valueFrom += '-changed' },
    value => { value.containerDefinitions[0].command = ['different'] },
  ]) { const value = task(); mutate(value); assert.notEqual(configurationFingerprint(service, value), original) }
  const network = clone(service); network.networkConfiguration.awsvpcConfiguration.securityGroups = ['sg-deadbeef']
  assert.notEqual(configurationFingerprint(network, baseline), original)
})

test('task policy rejects foreign roles, commands, secrets and incompatible Core pins', () => {
  validateTaskDefinition(task(), serviceName, core, canonical())
  for (const mutate of [
    value => { value.taskRoleArn = `arn:aws:iam::${ACCOUNT}:role/administrator` },
    value => { value.family = 'plether-sepolia-other' },
    value => { value.runtimePlatform.cpuArchitecture = 'X86_64' },
    value => { value.containerDefinitions[0].command = ['sh'] },
    value => { value.containerDefinitions[0].entryPoint = ['/bin/sh'] },
    value => { value.containerDefinitions[0].secrets[0].valueFrom = 'arn:aws:ssm:us-east-1:111111111111:parameter/private' },
    value => { value.containerDefinitions[0].environment[0].value = '42161' },
    value => { value.containerDefinitions.push({ name: 'unreviewed-sidecar' }) },
    value => { value.containerDefinitions[1].image += '-different' },
  ]) { const value = task(); mutate(value); assert.throws(() => validateTaskDefinition(value, serviceName, core, canonical())) }
})

test('rendering changes only app/init/router images and truthful version metadata', () => {
  const before = task(), after = renderRecoveredTaskDefinition(before, serviceName, release())
  assert.equal(after.taskDefinitionArn, undefined)
  assert.equal(after.containerDefinitions[0].image, release().applicationImage)
  assert.equal(after.containerDefinitions[1].image, release().applicationImage)
  assert.equal(after.containerDefinitions[2].image, release().routerImage)
  assert.equal(after.containerDefinitions[2].environment[0].value, release().sourceCommit)
  validateImageOnlyChange(before, after, serviceName, release())
  for (const mutate of [
    value => { value.taskRoleArn += '-wider' },
    value => { value.containerDefinitions[0].environment[0].value = '42161' },
    value => { value.containerDefinitions[0].secrets = [] },
    value => { value.containerDefinitions[0].command = ['sh'] },
  ]) { const value = clone(after); mutate(value); assert.throws(() => validateImageOnlyChange(before, value, serviceName, release())) }
})

const manifest = value => {
  const raw = JSON.stringify(value)
  return { raw, digest: `sha256:${createHash('sha256').update(raw).digest('hex')}` }
}
const imageGraph = () => {
  const leaf = manifest({ schemaVersion: 2, config: { digest: `sha256:${'1'.repeat(64)}` }, layers: [] })
  const attestation = manifest({ schemaVersion: 2, config: { digest: `sha256:${'2'.repeat(64)}` }, layers: [] })
  const index = manifest({ schemaVersion: 2, manifests: [
    { digest: leaf.digest, platform: { architecture: 'arm64', os: 'linux' } },
    { digest: attestation.digest, platform: { architecture: 'unknown', os: 'unknown' } },
  ] })
  return { leaf, attestation, index, values: new Map([[index.digest, index.raw], [leaf.digest, leaf.raw], [attestation.digest, attestation.raw]]) }
}

test('image qualification traverses child/attestation manifests and refuses missing or wrong-platform bytes', async () => {
  const fixture = imageGraph(), image = `${ACCOUNT}.dkr.ecr.${REGION}.amazonaws.com/plether-api-sepolia@${fixture.index.digest}`
  const read = async (_, digest) => { assert.ok(fixture.values.has(digest), 'Missing child manifest'); return fixture.values.get(digest) }
  const graph = await verifyImageGraph(image, 'plether-api-sepolia', read)
  assert.equal(graph.allManifestDigests.length, 3)
  assert.ok(graph.runtimeDigests.includes(fixture.leaf.digest))
  assert.ok(!graph.runtimeDigests.includes(fixture.attestation.digest))
  fixture.values.delete(fixture.attestation.digest)
  await assert.rejects(verifyImageGraph(image, 'plether-api-sepolia', read))
  await assert.rejects(verifyImageGraph(image, 'plether-api-sepolia', async () => '{}'), /digest/)
  const other = manifest({ manifests: [{ digest: fixture.leaf.digest, platform: { architecture: 'amd64', os: 'linux' } }] })
  fixture.values.set(other.digest, other.raw)
  await assert.rejects(verifyImageGraph(image.replace(fixture.index.digest, other.digest), 'plether-api-sepolia', read), /ARM64/)
})

test('digest lookups accept identical tag aliases but refuse conflicting or unrequested ECR entries', async () => {
  const fixture = imageGraph()
  const entry = tag => ({ imageId: { imageDigest: fixture.index.digest, imageTag: tag }, imageManifest: fixture.index.raw, imageManifestMediaType: 'application/vnd.oci.image.index.v1+json' })
  const response = { failures: [], images: [entry('latest'), entry('ae312cc2455d20975c501f1ce4521bfaefffce7b')] }
  assert.equal(manifestFromBatchGetImage(response, fixture.index.digest), fixture.index.raw)
  for (const mutate of [
    value => { value.images[1].imageId.imageDigest = `sha256:${'f'.repeat(64)}` },
    value => { value.images[1].imageManifest = '{}' },
    value => { value.images[1].imageManifestMediaType = 'application/vnd.docker.distribution.manifest.v2+json' },
    value => { value.failures.push({ failureCode: 'ImageNotFound' }) },
    value => { value.images = [] },
  ]) { const value = clone(response); mutate(value); assert.throws(() => manifestFromBatchGetImage(value, fixture.index.digest)) }
  const image = `${ACCOUNT}.dkr.ecr.${REGION}.amazonaws.com/plether-api-sepolia@${fixture.index.digest}`
  const graph = await verifyImageGraph(image, 'plether-api-sepolia', async (_, digest) => digest === fixture.index.digest
    ? manifestFromBatchGetImage(response, digest)
    : manifestFromBatchGetImage({ failures: [], images: [{ imageId: { imageDigest: digest }, imageManifest: fixture.values.get(digest) }] }, digest))
  assert.equal(graph.allManifestDigests.length, 3)
})

test('actual task must match the registered revision, approved digests and successful init', () => {
  const target = release(), revision = arn.replace(':67', ':68')
  const app = { runtimeDigests: [target.applicationImage.split('@')[1]] }, router = { runtimeDigests: [target.routerImage.split('@')[1]] }
  const running = { taskDefinitionArn: revision, lastStatus: 'RUNNING', desiredStatus: 'RUNNING', containers: [
    { name: serviceName, image: target.applicationImage, imageDigest: app.runtimeDigests[0], lastStatus: 'RUNNING' },
    { name: 'aa-reconciler-tmp-init', image: target.applicationImage, imageDigest: app.runtimeDigests[0], lastStatus: 'STOPPED', exitCode: 0 },
    { name: 'otel-log-router', image: target.routerImage, imageDigest: router.runtimeDigests[0], lastStatus: 'RUNNING' },
  ] }
  verifyRunningTask(running, serviceName, revision, target, app, router)
  for (const mutate of [
    value => { value.taskDefinitionArn = arn },
    value => { value.containers[0].imageDigest = `sha256:${'f'.repeat(64)}` },
    value => { value.containers[1].exitCode = 1 },
    value => { value.containers[2].lastStatus = 'STOPPED' },
  ]) { const value = clone(running); mutate(value); assert.throws(() => verifyRunningTask(value, serviceName, revision, target, app, router)) }
})

test('worker progress must be fresh and caught-up heartbeat spans more than the stale threshold', () => {
  const now = 1_000_000
  const event = (timestamp, data) => ({ timestamp, message: JSON.stringify({ container_name: serviceName, event: 'aa_reconciler_heartbeat', level: 'INFO', ...data }) })
  assert.equal(progressEvidence([event(now - 60_000)], serviceName, now), undefined)
  assert.ok(progressEvidence([event(now - 125_000), event(now - 1_000)], serviceName, now))
  assert.equal(progressEvidence([event(now - 500_000), event(now - 125_000)], serviceName, now), undefined)
  assert.throws(() => progressEvidence([event(now - 125_000), event(now - 1_000), event(now, { event: 'aa_reconciler_failed', level: 'ERROR' })], serviceName, now))
  const protection = (caughtUp, lagBlocks) => [{ timestamp: now, message: JSON.stringify({ container_name: 'plether-position-protection-worker', event: 'protection_worker_heartbeat', caughtUp, lagBlocks }) }]
  assert.equal(progressEvidence(protection(false, '1'), 'plether-position-protection-worker', now), undefined)
  assert.ok(progressEvidence(protection(true, '0'), 'plether-position-protection-worker', now))
  const sweep = completed => [{ timestamp: now, message: JSON.stringify({ container_name: 'plether-liquidation-worker', event: 'liquidation_sweep_finished', completed }) }]
  assert.equal(progressEvidence(sweep(false), 'plether-liquidation-worker', now), undefined)
  assert.ok(progressEvidence(sweep(true), 'plether-liquidation-worker', now))
})

test('workflow retains exact-run approval and does not build, auto-approve, unpause or accept task overrides', () => {
  const source = fs.readFileSync(new URL('.github/workflows/recover-backend.yml', root), 'utf8')
  assert.match(source, /environment: backend-admin-sepolia/)
  assert.match(source, /plether-backend-mutation-sepolia/)
  assert.match(source, /EXPECTED_INPUT_FINGERPRINT: \$\{\{ needs.preflight.outputs.fingerprint \}\}/)
  const run = source.split('\n  recover:\n')[1]
  assert.ok(run.indexOf('Revalidate protected policy before credentials') < run.indexOf('Configure AWS credentials'))
  assert.doesNotMatch(source, /build-push-action|pending_deployments|resume-issuance|--overrides|ecr:BatchDeleteImage/)
})

test('CLI policy refusals never print raw input values or task assertion diffs', () => {
  const secret = 'private-marker-do-not-log'
  const result = spawnSync(process.execPath, ['scripts/recover-backend.mjs', 'fingerprint'], {
    cwd: root,
    env: { ...process.env, RECOVERY_SERVICE: secret, OPERATOR_NOTE: secret },
    encoding: 'utf8',
  })
  assert.equal(result.status, 1)
  assert.match(result.stderr, /Backend image recovery refused/)
  assert.doesNotMatch(result.stdout + result.stderr, new RegExp(secret))
})

test('CLI preflight emits the reviewed catalog/input fingerprint without any AWS operation', () => {
  const value = input()
  const result = spawnSync(process.execPath, ['scripts/recover-backend.mjs', 'fingerprint'], {
    cwd: root,
    env: { ...process.env,
      RECOVERY_SERVICE: value.service, RECOVERY_RELEASE: value.releaseId,
      EXPECTED_TASK_DEFINITION: value.expectedTaskDefinition,
      EXPECTED_CONFIGURATION_SHA256: value.expectedConfigurationSha256,
      CONFIRMATION: value.confirmation, OPERATOR_NOTE: value.operatorNote,
      AWS_PROFILE: 'no-such-profile-for-read-only-fingerprint',
    },
    encoding: 'utf8',
  })
  assert.equal(result.status, 0, result.stderr)
  assert.equal(result.stdout.trim(), `Dispatch input fingerprint: sha256:${inputFingerprint(value, release())}`)
})
