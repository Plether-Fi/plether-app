import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { createHash } from 'node:crypto'
import { runImageRecovery } from './backend-recovery-engine.mjs'
import { ACCOUNT, REGION, SERVICES, configurationFingerprint, verifyRunningTask } from './recover-backend.mjs'
import { DRILL, DRILL_POLICY, validateDrillConfig, validateDrillInputs, validateDrillSnapshot, validateDrillTask, drillInputFingerprint, drillProgressEvidence } from './drill-backend-recovery.mjs'

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

test('isolated progress requires real-time non-signing heartbeats over two minutes', () => {
  const now = 1_000_000
  const heartbeat = (timestamp, extra = {}) => ({ timestamp, message: JSON.stringify({ event: 'recovery_drill_heartbeat', container_name: DRILL.service, signingEnabled: false, level: 'INFO', ...extra }) })
  assert.equal(drillProgressEvidence([heartbeat(now)], DRILL.service, now), undefined)
  assert.ok(drillProgressEvidence([heartbeat(now - 121_000), heartbeat(now - 1_000)], DRILL.service, now))
  assert.equal(drillProgressEvidence([heartbeat(now - 121_000), heartbeat(now - 1_000, { signingEnabled: true })], DRILL.service, now), undefined)
  assert.equal(drillProgressEvidence([heartbeat(now - 200_000), heartbeat(now - 60_000)], DRILL.service, now), undefined)
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
  assert.match(workflow, /environment: backend-admin-sepolia/)
  assert.match(workflow, /plether-backend-recovery-drill/)
  assert.match(workflow, /AWS_RECOVERY_DRILL_ROLE_ARN/)
  assert.doesNotMatch(workflow, /AWS_DEPLOY_ROLE_ARN|pending_deployments|--overrides|resume-issuance|SetAlarmState|PutMetricData/)
})
