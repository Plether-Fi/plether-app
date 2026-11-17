import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { createHash } from 'node:crypto'
import { runImageRecovery } from './backend-recovery-engine.mjs'
import { ACCOUNT, REGION, SERVICES, configurationFingerprint, verifyRunningTask } from './recover-backend.mjs'
import { DRILL, DRILL_POLICY, validateDrillConfig, validateDrillInputs, validateDrillSnapshot, validateDrillTask, drillInputFingerprint, drillProgressEvidence, drillSnapshotEvidence, validateRuntimeAuditIdentity, diagnoseDrillBaseline, validDrillCircuitBreaker } from './drill-backend-recovery.mjs'

const clone = value => structuredClone(value)
const oldArn = `arn:aws:ecs:${REGION}:${ACCOUNT}:task-definition/${DRILL.family}:1`
const newArn = oldArn.replace(/:1$/, ':2')
const taskArn = `arn:aws:ecs:${REGION}:${ACCOUNT}:task/${DRILL.cluster}/0123456789abcdef0123456789abcdef`
const fixtureTask = () => ({
  taskDefinitionArn: oldArn, family: DRILL.family, status: 'ACTIVE',
  networkMode: 'awsvpc', cpu: '256', memory: '512', requiresCompatibilities: ['FARGATE'],
  runtimePlatform: { cpuArchitecture: 'ARM64', operatingSystemFamily: 'LINUX' },
  executionRoleArn: `arn:aws:iam::${ACCOUNT}:role/${DRILL.executionRole}`, volumes: [],
  containerDefinitions: Object.entries(DRILL.containers).map(([kind, container]) => ({
    ...clone(container), essential: kind !== 'init', image: kind === 'router' ? DRILL.release.routerImage : DRILL.missingApplicationImage,
    readonlyRootFilesystem: true, user: '65534:65534', privileged: false,
    environment: kind === 'router' ? [{ name: 'SERVICE_VERSION', value: DRILL.release.sourceCommit }] : clone(container.environment ?? []),
    secrets: [], mountPoints: [], portMappings: [],
    linuxParameters: { capabilities: { drop: ['ALL'] }, initProcessEnabled: true },
    logConfiguration: { logDriver: 'awslogs', options: { 'awslogs-group': DRILL.logGroup, 'awslogs-region': REGION, 'awslogs-stream-prefix': DRILL.logStreamPrefix } },
  })),
})
const fixtureService = () => ({
  serviceName: DRILL.service, serviceArn: `arn:aws:ecs:${REGION}:${ACCOUNT}:service/${DRILL.cluster}/${DRILL.service}`,
  clusterArn: `arn:aws:ecs:${REGION}:${ACCOUNT}:cluster/${DRILL.cluster}`, status: 'ACTIVE', taskDefinition: oldArn,
  launchType: 'FARGATE', platformVersion: '1.4.0', deploymentController: { type: 'ECS' }, schedulingStrategy: 'REPLICA',
  desiredCount: 1, runningCount: 0, pendingCount: 0, enableExecuteCommand: false,
  deploymentConfiguration: { minimumHealthyPercent: 0, maximumPercent: 100, deploymentCircuitBreaker: { enable: true, rollback: false } },
  deployments: [{ status: 'PRIMARY', rolloutState: 'FAILED', taskDefinition: oldArn }],
  networkConfiguration: { awsvpcConfiguration: { assignPublicIp: 'ENABLED', subnets: ['subnet-abcdef'], securityGroups: ['sg-abcdef'] } },
})
const input = () => ({ service: DRILL.service, releaseId: DRILL.release.id,
  expectedTaskDefinition: oldArn, expectedConfigurationSha256: configurationFingerprint(fixtureService(), fixtureTask()),
  confirmation: 'RECOVER ISOLATED FIXTURE ON SEPOLIA', operatorNote: 'Non-signing missing-image qualification',
})

test('the fixture has no financial capability and cannot enter the production allowlist', () => {
  validateDrillConfig()
  assert.ok(!Object.hasOwn(SERVICES, DRILL.service))
  assert.throws(() => verifyRunningTask({}, DRILL.service, '', {}, {}, {}), /Unknown production/)
  validateDrillInputs(input())
  validateDrillTask(fixtureTask())
  const ecsDefaults = fixtureTask()
  for (const container of ecsDefaults.containerDefinitions) container.linuxParameters.capabilities.add = []
  validateDrillTask(ecsDefaults)
  validateDrillSnapshot({ failures: [], services: [fixtureService()] }, input())
  for (const mutate of [
    value => { value.taskRoleArn = `arn:aws:iam::${ACCOUNT}:role/plether-sepolia-ecs-task` },
    value => { value.executionRoleArn = `arn:aws:iam::${ACCOUNT}:role/plether-sepolia-ecs-execution` },
    value => { value.containerDefinitions[0].command = ['plether-liquidation-worker'] },
    value => { value.containerDefinitions[0].environment.push({ name: 'DATABASE_URL', value: 'fixture-must-have-no-database' }) },
    value => { value.containerDefinitions[0].secrets = [{ name: 'PRIVATE_KEY', valueFrom: 'arn:aws:ssm:example' }] },
    value => { value.containerDefinitions[0].mountPoints = [{ sourceVolume: 'host', containerPath: '/keys' }] },
    value => { value.containerDefinitions[0].portMappings = [{ containerPort: 3001 }] },
    value => { value.containerDefinitions[0].privileged = true },
    value => { value.containerDefinitions[0].linuxParameters.capabilities.add = ['NET_ADMIN'] },
    value => { value.containerDefinitions[0].linuxParameters.sharedMemorySize = 64 },
    value => { value.containerDefinitions[0].image = DRILL.release.applicationImage },
  ]) { const value = fixtureTask(); mutate(value); assert.throws(() => validateDrillTask(value)) }
  for (const mutate of [
    value => { value.serviceName = 'plether-aa-reconciler' },
    value => { value.clusterArn = value.clusterArn.replace(DRILL.cluster, 'plether-sepolia') },
    value => { value.runningCount = 1 },
    value => { value.deployments[0].rolloutState = 'IN_PROGRESS' },
    value => { value.enableExecuteCommand = true },
  ]) { const value = fixtureService(); mutate(value); assert.throws(() => validateDrillSnapshot({ failures: [], services: [value] }, input())) }
})

test('the drill fingerprint binds its disjoint capability and fixed non-signing commands', () => {
  const original = drillInputFingerprint(input())
  const other = input(); other.operatorNote = 'Different approved drill'
  assert.notEqual(drillInputFingerprint(other), original)
  for (const mutate of [
    value => { value.cluster = 'plether-sepolia' },
    value => { value.executionRole = 'plether-sepolia-ecs-execution' },
    value => { value.containers.main.command = ['plether-keeper'] },
  ]) { const value = clone(DRILL); mutate(value); assert.throws(() => validateDrillConfig(value)) }
  const production = input(); production.expectedTaskDefinition = production.expectedTaskDefinition.replace(DRILL.family, 'plether-sepolia-aa-reconciler')
  assert.throws(() => validateDrillInputs(production))
})

test('circuit-breaker compatibility accepts only the known legacy policy and documented new defaults', () => {
  const legacy = { enable: true, rollback: false }
  const current = { ...legacy, resetOnHealthyTask: true, thresholdConfiguration: { type: 'BOUNDED_PERCENT', value: 50 } }
  for (const value of [legacy, current]) {
    assert.equal(validDrillCircuitBreaker(value), true)
    const service = fixtureService(); service.deploymentConfiguration.deploymentCircuitBreaker = value
    validateDrillSnapshot({ failures: [], services: [service] }, input())
    assert.equal(drillSnapshotEvidence({ failures: [], services: [service] }, input()).checks.exactCircuitBreaker, true)
  }
  for (const value of [
    undefined, null, [], true, {}, { enable: true }, { ...legacy, enable: false }, { ...legacy, rollback: true },
    { ...current, resetOnHealthyTask: false }, { ...current, resetOnHealthyTask: 'true' },
    { ...current, thresholdConfiguration: null }, { ...current, thresholdConfiguration: [] },
    { ...current, thresholdConfiguration: { type: 'BOUNDED_PERCENT', value: 51 } },
    { ...current, thresholdConfiguration: { type: 'BOUNDED_PERCENT', value: '50' } },
    { ...current, thresholdConfiguration: { type: 'OTHER', value: 50 } },
    { ...current, thresholdConfiguration: { type: 'BOUNDED_PERCENT' } },
    { ...current, thresholdConfiguration: { type: 'BOUNDED_PERCENT', value: 50, unreviewed: true } },
    { ...current, futureControl: true },
  ]) {
    assert.equal(validDrillCircuitBreaker(value), false)
    const service = fixtureService(); service.deploymentConfiguration.deploymentCircuitBreaker = value
    assert.throws(() => validateDrillSnapshot({ failures: [], services: [service] }, input()))
    assert.equal(drillSnapshotEvidence({ failures: [], services: [service] }, input()).checks.exactCircuitBreaker, false)
  }
})

test('isolated progress requires real-time non-signing heartbeats over two minutes', () => {
  const now = 1_000_000
  const heartbeat = (timestamp, extra = {}) => ({ timestamp, message: JSON.stringify({ event: 'recovery_drill_heartbeat', container_name: DRILL.service, signingEnabled: false, level: 'INFO', ...extra }) })
  assert.equal(drillProgressEvidence([heartbeat(now)], DRILL.service, now), undefined)
  assert.ok(drillProgressEvidence([heartbeat(now - 121_000), heartbeat(now - 1_000)], DRILL.service, now))
  assert.equal(drillProgressEvidence([heartbeat(now - 121_000), heartbeat(now - 1_000, { signingEnabled: true })], DRILL.service, now), undefined)
  assert.equal(drillProgressEvidence([heartbeat(now - 200_000), heartbeat(now - 60_000)], DRILL.service, now), undefined)
})

test('bounded snapshot evidence identifies each strict refusal without changing any guard', () => {
  const response = () => ({ failures: [], services: [fixtureService()] })
  const good = drillSnapshotEvidence(response(), input())
  assert.equal(good.failureList, 'ARRAY')
  assert.equal(good.serviceCount, 1)
  assert.ok(Object.values(good.checks).every(value => value === true))
  const cases = [
    ['emptyFailureList', value => { value.failures = [{ reason: 'MISSING' }] }],
    ['exactlyOneService', value => { value.services = [] }],
    ['serviceName', value => { value.services[0].serviceName = 'other' }],
    ['serviceArn', value => { value.services[0].serviceArn = 'other' }],
    ['clusterArn', value => { value.services[0].clusterArn = 'other' }],
    ['activeStatus', value => { value.services[0].status = 'INACTIVE' }],
    ['selectedTaskDefinition', value => { value.services[0].taskDefinition = 'other' }],
    ['fargateLaunchType', value => { delete value.services[0].launchType }],
    ['platformVersion', value => { delete value.services[0].platformVersion }],
    ['ecsDeploymentController', value => { delete value.services[0].deploymentController }],
    ['replicaScheduling', value => { delete value.services[0].schedulingStrategy }],
    ['desiredOne', value => { value.services[0].desiredCount = 0 }],
    ['runningZero', value => { value.services[0].runningCount = 1 }],
    ['pendingZero', value => { value.services[0].pendingCount = 1 }],
    ['execDisabled', value => { delete value.services[0].enableExecuteCommand }],
    ['minimumHealthyZero', value => { value.services[0].deploymentConfiguration.minimumHealthyPercent = 100 }],
    ['maximumPercentHundred', value => { value.services[0].deploymentConfiguration.maximumPercent = 200 }],
    ['exactCircuitBreaker', value => { value.services[0].deploymentConfiguration.deploymentCircuitBreaker.rollback = true }],
    ['exactlyOneDeployment', value => { value.services[0].deployments.push({ status: 'ACTIVE' }) }],
    ['primaryStatus', value => { value.services[0].deployments[0].status = 'ACTIVE' }],
    ['failedRollout', value => { value.services[0].deployments[0].rolloutState = 'IN_PROGRESS' }],
    ['primaryTaskDefinition', value => { value.services[0].deployments[0].taskDefinition = 'other' }],
    ['publicIpEnabled', value => { value.services[0].networkConfiguration.awsvpcConfiguration.assignPublicIp = 'DISABLED' }],
    ['boundedSubnetIdentifiers', value => { value.services[0].networkConfiguration.awsvpcConfiguration.subnets = ['private-marker'] }],
    ['exactlyOneSecurityGroup', value => { value.services[0].networkConfiguration.awsvpcConfiguration.securityGroups = [] }],
    ['securityGroupIdentifier', value => { value.services[0].networkConfiguration.awsvpcConfiguration.securityGroups = ['private-marker'] }],
  ]
  for (const [guard, mutate] of cases) {
    const value = response(); mutate(value)
    assert.equal(drillSnapshotEvidence(value, input()).checks[guard], false, guard)
    assert.throws(() => validateDrillSnapshot(value, input()), undefined, guard)
  }
})

test('snapshot diagnostics distinguish absent failures and API denial while never retaining credential strings', () => {
  const secret = 'https://private.invalid/rpc?token=must-never-appear'
  const absent = drillSnapshotEvidence({ services: [fixtureService()] }, input())
  assert.equal(absent.failureList, 'ABSENT')
  assert.equal(absent.failureCount, null)
  assert.equal(absent.checks.emptyFailureList, false)
  assert.throws(() => validateDrillSnapshot({ services: [fixtureService()] }, input()))
  const denied = drillSnapshotEvidence({ services: [], failures: [
    { reason: 'ACCESS_DENIED', arn: secret, detail: secret },
    { reason: 'MISSING', arn: secret }, { reason: secret, detail: secret },
  ] }, input())
  assert.deepEqual(denied.failureCodes, ['ACCESS_DENIED', 'MISSING', 'OTHER'])
  assert.equal(denied.failureCount, 3)
  assert.equal(denied.serviceCount, 0)
  assert.ok(!JSON.stringify(denied).includes(secret))
  const malformed = drillSnapshotEvidence({ failures: secret, services: secret }, input())
  assert.equal(malformed.failureList, 'INVALID')
  assert.equal(malformed.serviceList, 'INVALID')
  assert.ok(!JSON.stringify(malformed).includes(secret))
})

test('pre-approval diagnosis uses only the exact read-only role and fixed fixture metadata', () => {
  const identity = { Account: ACCOUNT, Arn: `arn:aws:sts::${ACCOUNT}:assumed-role/plether-sepolia-runtime-image-audit/readonly-inspect` }
  validateRuntimeAuditIdentity(identity)
  for (const value of [
    { ...identity, Account: '111111111111' },
    { ...identity, Arn: `arn:aws:sts::${ACCOUNT}:assumed-role/${DRILL.deploymentRole}/mutator` },
    { ...identity, Arn: `arn:aws:iam::${ACCOUNT}:user/plether-admin` },
  ]) assert.throws(() => validateRuntimeAuditIdentity(value))
  for (const response of [
    { failures: [], services: [fixtureService()] },
    { services: [fixtureService()] },
    { failures: [{ reason: 'ACCESS_DENIED', detail: 'token=do-not-retain' }], services: [] },
  ]) {
    const calls = []
    const result = diagnoseDrillBaseline(args => {
      calls.push(args)
      if (args[0] === 'sts') return identity
      if (args[1] === 'describe-task-definition') return { taskDefinition: fixtureTask() }
      assert.deepEqual(args, ['ecs', 'describe-services', '--cluster', DRILL.cluster, '--services', DRILL.service])
      return response
    }, input())
    const expected = [['sts', 'get-caller-identity'], ['ecs', 'describe-services', '--cluster', DRILL.cluster, '--services', DRILL.service]]
    if (result.baselineChecksPassed) expected.push(['ecs', 'describe-task-definition', '--task-definition', oldArn])
    assert.deepEqual(calls, expected)
    assert.equal(result.evidenceSource, 'read-only-runtime-image-audit')
    assert.equal(result.baselineChecksPassed, Array.isArray(response.failures) && response.failures.length === 0 && response.services.length === 1)
    assert.equal(result.preflightPassed, result.baselineChecksPassed)
    assert.ok(!JSON.stringify(result).includes('do-not-retain'))
  }
  let describeCalled = false
  assert.throws(() => diagnoseDrillBaseline(args => {
    if (args[0] === 'sts') return { ...identity, Arn: `arn:aws:sts::${ACCOUNT}:assumed-role/${DRILL.deploymentRole}/mutator` }
    describeCalled = true
  }, input()))
  assert.equal(describeCalled, false)
})

test('Linux preflight hashes every new safety field and refuses the older CLI baseline before approval', () => {
  const identity = { Account: ACCOUNT, Arn: `arn:aws:sts::${ACCOUNT}:assumed-role/plether-sepolia-runtime-image-audit/readonly-inspect` }
  const service = fixtureService()
  service.deploymentConfiguration.deploymentCircuitBreaker = {
    enable: true, rollback: false, resetOnHealthyTask: true,
    thresholdConfiguration: { type: 'BOUNDED_PERCENT', value: 50 },
  }
  const completeHash = configurationFingerprint(service, fixtureTask())
  const approved = input(), originalInput = clone(approved)
  assert.notEqual(completeHash, approved.expectedConfigurationSha256)
  const reader = args => args[0] === 'sts' ? identity : args[1] === 'describe-services'
    ? { failures: [], services: [service] } : { taskDefinition: fixtureTask() }
  const mismatch = diagnoseDrillBaseline(reader, approved)
  assert.equal(mismatch.baselineChecksPassed, true)
  assert.equal(mismatch.taskDefinitionChecksPassed, true)
  assert.equal(mismatch.actualConfigurationSha256, completeHash)
  assert.equal(mismatch.expectedFingerprintMatches, false)
  assert.equal(mismatch.preflightPassed, false)
  assert.deepEqual(approved, originalInput, 'Never auto-adopt the discovered hash')
  const reviewed = { ...approved, expectedConfigurationSha256: completeHash }
  assert.equal(diagnoseDrillBaseline(reader, reviewed).preflightPassed, true)
  const badTask = fixtureTask(); badTask.taskRoleArn = 'secret-token-must-not-appear'
  const refused = diagnoseDrillBaseline(args => args[1] === 'describe-task-definition' ? { taskDefinition: badTask } : reader(args), reviewed)
  assert.equal(refused.taskDefinitionChecksPassed, false)
  assert.equal(refused.preflightPassed, false)
  assert.equal(refused.actualConfigurationSha256, null)
  assert.ok(!JSON.stringify(refused).includes('secret-token'))
})

const manifest = value => {
  const raw = JSON.stringify(value)
  return { raw, digest: `sha256:${createHash('sha256').update(raw).digest('hex')}` }
}
function fakeRuntime({ concurrentChange = false, missingReplacement = false } = {}) {
  const leaf = manifest({ config: { digest: `sha256:${'1'.repeat(64)}` }, layers: [] })
  const index = manifest({ manifests: [{ digest: leaf.digest, platform: { architecture: 'arm64', os: 'linux' } }] })
  const release = { ...DRILL.release,
    applicationImage: DRILL.release.applicationImage.replace(/sha256:[0-9a-f]{64}/, index.digest),
    routerImage: DRILL.release.routerImage.replace(/sha256:[0-9a-f]{64}/, index.digest),
  }
  let selected = oldArn, registered, serviceReads = 0, time = 1_000_000
  const calls = []
  const aws = args => {
    calls.push(args)
    const [kind, action] = args
    assert.ok(['ecs', 'ecr', 'logs'].includes(kind))
    if (action === 'describe-services') {
      assert.equal(args[args.indexOf('--cluster') + 1], DRILL.cluster)
      assert.equal(args[args.indexOf('--services') + 1], DRILL.service)
      const service = fixtureService(); serviceReads++
      if (selected === newArn) { service.taskDefinition = newArn; service.runningCount = 1; service.deployments = [{ status: 'PRIMARY', rolloutState: 'COMPLETED', taskDefinition: newArn }] }
      if (concurrentChange && serviceReads === 2) service.networkConfiguration.awsvpcConfiguration.securityGroups = ['sg-123456']
      return { failures: [], services: [service] }
    }
    if (action === 'describe-task-definition') return { taskDefinition: fixtureTask() }
    if (action === 'batch-get-image') {
      const digest = args.find(value => value.startsWith('imageDigest=')).split('=')[1]
      if (missingReplacement) return { images: [], failures: [{ failureCode: 'ImageNotFound' }] }
      return { failures: [], images: [{ imageId: { imageDigest: digest }, imageManifest: digest === index.digest ? index.raw : leaf.raw }] }
    }
    if (action === 'register-task-definition') {
      registered = JSON.parse(fs.readFileSync(args[args.indexOf('--cli-input-json') + 1].slice('file://'.length)))
      return { taskDefinition: { ...registered, taskDefinitionArn: newArn, revision: 2, status: 'ACTIVE' } }
    }
    if (action === 'update-service') {
      assert.equal(args[args.indexOf('--cluster') + 1], DRILL.cluster)
      assert.equal(args[args.indexOf('--service') + 1], DRILL.service)
      assert.equal(args[args.indexOf('--task-definition') + 1], newArn)
      selected = newArn; time += 130_000
      return { service: { serviceName: DRILL.service, taskDefinition: selected } }
    }
    if (action === 'list-tasks') return { taskArns: [taskArn] }
    if (action === 'describe-tasks') return { failures: [], tasks: [{ taskDefinitionArn: newArn, lastStatus: 'RUNNING', desiredStatus: 'RUNNING', containers: registered.containerDefinitions.map(container => ({
      name: container.name, image: container.image, imageDigest: index.digest,
      lastStatus: container.name === DRILL.containers.init.name ? 'STOPPED' : 'RUNNING',
      ...(container.name === DRILL.containers.init.name ? { exitCode: 0 } : {}),
    })) }] }
    if (action === 'filter-log-events') {
      assert.equal(args[args.indexOf('--log-group-name') + 1], DRILL.logGroup)
      return { events: [time - 125_000, time - 1_000].map(timestamp => ({ timestamp, message: JSON.stringify({ event: 'recovery_drill_heartbeat', container_name: DRILL.service, signingEnabled: false, level: 'INFO' }) })) }
    }
    throw Error(`Unexpected mocked API: ${kind} ${action}`)
  }
  return { aws, release, calls, now: () => time }
}

test('the actual shared engine recovers only the isolated service and preserves refusal evidence', async () => {
  for (const options of [{}, { concurrentChange: true }, { missingReplacement: true }]) {
    const runtime = fakeRuntime(options), directory = fs.mkdtempSync(path.join(os.tmpdir(), 'isolated-recovery-engine-test-'))
    try {
      const run = runImageRecovery({ input: input(), release: runtime.release, inputFingerprint: drillInputFingerprint(input()), directory, policy: DRILL_POLICY, aws: runtime.aws, now: runtime.now, pause: async () => {} })
      if (options.concurrentChange || options.missingReplacement) {
        await assert.rejects(run)
        assert.equal(runtime.calls.filter(args => args[1] === 'update-service').length, 0)
      } else {
        const completed = await run
        assert.equal(completed.taskDefinition, newArn)
        assert.equal(runtime.calls.filter(args => args[1] === 'update-service').length, 1)
        assert.equal(completed.progress.signingEnabled, false)
      }
      const evidence = JSON.parse(fs.readFileSync(path.join(directory, 'recovery.json')))
      assert.equal(evidence.capability, DRILL.capability)
      assert.equal(evidence.serviceBaseline.serviceCount, 1)
      assert.ok(Object.values(evidence.serviceBaseline.checks).every(value => value === true))
      assert.equal(fs.existsSync(path.join(directory, 'task-definition.private.json')), false)
      assert.ok(runtime.calls.every(args => !['stop-task', 'run-task', 'delete-repository', 'batch-delete-image'].includes(args[1])))
    } finally { fs.rmSync(directory, { recursive: true, force: true }) }
  }
})

test('production and drill entrypoints use the same engine with disjoint workflows/capabilities', () => {
  const production = fs.readFileSync(new URL('./recover-backend.mjs', import.meta.url), 'utf8')
  const drill = fs.readFileSync(new URL('./drill-backend-recovery.mjs', import.meta.url), 'utf8')
  const workflow = fs.readFileSync(new URL('../.github/workflows/drill-backend-recovery.yml', import.meta.url), 'utf8')
  for (const source of [production, drill]) assert.match(source, /await runImageRecovery\(/)
  assert.doesNotMatch(production, /DRILL_POLICY|backend-recovery-drill.json/)
  assert.doesNotMatch(production, /snapshotEvidence/)
  assert.match(workflow, /environment: backend-admin-sepolia/)
  assert.match(workflow, /plether-backend-recovery-drill/)
  assert.match(workflow, /AWS_RECOVERY_DRILL_ROLE_ARN/)
  const preflight = workflow.split('\n  preflight:\n')[1].split('\n  recover-isolated-fixture:\n')[0]
  assert.match(preflight, /AWS_RUNTIME_IMAGE_AUDIT_ROLE_ARN/)
  assert.match(preflight, /plether-sepolia-runtime-image-audit/)
  assert.match(preflight, /drill-backend-recovery\.mjs diagnose/)
  assert.match(preflight, /Preserve bounded read-only baseline evidence/)
  assert.doesNotMatch(preflight, /AWS_RECOVERY_DRILL_ROLE_ARN|drill-backend-recovery\.mjs recover/)
  assert.doesNotMatch(workflow, /AWS_DEPLOY_ROLE_ARN|pending_deployments|--overrides|resume-issuance|SetAlarmState|PutMetricData/)
})
