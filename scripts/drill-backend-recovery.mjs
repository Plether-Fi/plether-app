// Fixed non-signing AWS fixture; cannot select a production service or role.
// The mutation/verification engine is identical to recover-backend.mjs.
import assert from 'node:assert/strict'
import { createHash } from 'node:crypto'
import { execFileSync } from 'node:child_process'
import fs from 'node:fs'
import path from 'node:path'
import { pathToFileURL } from 'node:url'
import { runImageRecovery } from './backend-recovery-engine.mjs'
import {
  ACCOUNT, REGION, aws, validateProtectionPolicy, configurationFingerprint,
  renderRecoveredTaskDefinition, validateImageOnlyChange, verifyImageGraph,
  manifestFromBatchGetImage, verifyRunningTaskForPolicy,
} from './recover-backend.mjs'

export const DRILL = JSON.parse(fs.readFileSync(new URL('../config/operations/backend-recovery-drill.json', import.meta.url)))
const one = (values, message) => { assert.equal(values.length, 1, message); return values[0] }
const sorted = value => Array.isArray(value) ? value.map(sorted) : value && typeof value === 'object' ? Object.fromEntries(Object.keys(value).sort().map(key => [key, sorted(value[key])])) : value
const hash = value => createHash('sha256').update(JSON.stringify(sorted(value))).digest('hex')

export function validateDrillConfig(config = DRILL) {
  assert.equal(config.schemaVersion, 1)
  assert.equal(config.capability, 'isolated-non-signing-recovery-drill')
  assert.equal(config.accountId, ACCOUNT)
  assert.equal(config.region, REGION)
  assert.equal(config.cluster, 'plether-sepolia-recovery-drill')
  assert.equal(config.service, 'plether-image-recovery-drill')
  assert.equal(config.family, 'plether-sepolia-image-recovery-drill')
  assert.equal(config.executionRole, 'plether-sepolia-image-recovery-drill-execution')
  assert.equal(config.deploymentRole, 'plether-sepolia-image-recovery-drill-deploy')
  assert.equal(config.logGroup, '/ecs/plether-sepolia-recovery-drill')
  assert.equal(config.logStreamPrefix, 'drill')
  assert.deepEqual(config.containers.main.command, ['node', '-e', "const emit=()=>console.log(JSON.stringify({event:'recovery_drill_heartbeat',level:'INFO',container_name:'plether-image-recovery-drill',signingEnabled:false}));emit();setInterval(emit,15000)"])
  assert.deepEqual(config.containers.init.command, ['node', '-e', 'process.exit(0)'])
  assert.deepEqual(config.containers.router.entryPoint, ['/bin/sh', '-c'])
  assert.deepEqual(config.containers.router.command, ['exec sleep 86400'])
  assert.match(config.release.sourceCommit, /^[0-9a-f]{40}$/)
  assert.match(config.release.routerSourceCommit, /^[0-9a-f]{40}$/)
  assert.match(config.missingApplicationImage, new RegExp(`^${ACCOUNT}\\.dkr\\.ecr\\.${REGION}\\.amazonaws\\.com/plether-api-sepolia@sha256:0{64}$`))
  return config
}

export function validateDrillInputs(input, config = DRILL) {
  validateDrillConfig(config)
  assert.equal(input.service, config.service)
  assert.equal(input.releaseId, config.release.id)
  assert.match(input.expectedTaskDefinition, new RegExp(`^arn:aws:ecs:${REGION}:${ACCOUNT}:task-definition/${config.family}:[1-9][0-9]*$`))
  assert.match(input.expectedConfigurationSha256, /^[0-9a-f]{64}$/)
  assert.equal(input.confirmation, 'RECOVER ISOLATED FIXTURE ON SEPOLIA')
  assert.ok(typeof input.operatorNote === 'string' && /^[\x20-\x7e]{1,512}$/.test(input.operatorNote) && input.operatorNote.trim() === input.operatorNote)
}

export function drillInputFingerprint(input, config = DRILL) {
  validateDrillInputs(input, config)
  return hash({ schemaVersion: 1, capability: config.capability, input, reviewedFixture: config })
}

export function drillSnapshotEvidence(response, input) {
  // Only fixed guard names, booleans, bounded counts and closed failure codes
  // leave this callback. Never retain API detail strings or service objects.
  const failures = response?.failures, services = response?.services
  const service = Array.isArray(services) && services.length === 1 && services[0] && typeof services[0] === 'object' ? services[0] : {}
  const deployments = service.deployments
  const primary = Array.isArray(deployments) && deployments.length === 1 && deployments[0] && typeof deployments[0] === 'object' ? deployments[0] : {}
  const deployment = service.deploymentConfiguration ?? {}
  const network = service.networkConfiguration?.awsvpcConfiguration ?? {}
  const equal = (actual, expected) => { try { assert.deepEqual(actual, expected); return true } catch { return false } }
  const failureCodes = Array.isArray(failures) ? [...new Set(failures.slice(0, 10).map(failure =>
    ['MISSING', 'ACCESS_DENIED'].includes(failure?.reason) ? failure.reason : 'OTHER'))].sort() : []
  return {
    schemaVersion: 1,
    failureList: failures === undefined ? 'ABSENT' : Array.isArray(failures) ? 'ARRAY' : 'INVALID',
    serviceList: services === undefined ? 'ABSENT' : Array.isArray(services) ? 'ARRAY' : 'INVALID',
    failureCount: Array.isArray(failures) ? Math.min(failures.length, 100) : null,
    serviceCount: Array.isArray(services) ? Math.min(services.length, 100) : null,
    failureCodes,
    checks: {
      emptyFailureList: equal(failures, []),
      exactlyOneService: Array.isArray(services) && services.length === 1,
      serviceName: service.serviceName === DRILL.service,
      serviceArn: service.serviceArn === `arn:aws:ecs:${REGION}:${ACCOUNT}:service/${DRILL.cluster}/${DRILL.service}`,
      clusterArn: service.clusterArn === `arn:aws:ecs:${REGION}:${ACCOUNT}:cluster/${DRILL.cluster}`,
      activeStatus: service.status === 'ACTIVE',
      selectedTaskDefinition: service.taskDefinition === input.expectedTaskDefinition,
      fargateLaunchType: service.launchType === 'FARGATE',
      platformVersion: service.platformVersion === '1.4.0',
      ecsDeploymentController: service.deploymentController?.type === 'ECS',
      replicaScheduling: service.schedulingStrategy === 'REPLICA',
      desiredOne: service.desiredCount === 1,
      runningZero: service.runningCount === 0,
      pendingZero: service.pendingCount === 0,
      execDisabled: service.enableExecuteCommand === false,
      minimumHealthyZero: deployment.minimumHealthyPercent === 0,
      maximumPercentHundred: deployment.maximumPercent === 100,
      exactCircuitBreaker: equal(deployment.deploymentCircuitBreaker, { enable: true, rollback: false }),
      exactlyOneDeployment: Array.isArray(deployments) && deployments.length === 1,
      primaryStatus: primary.status === 'PRIMARY',
      failedRollout: primary.rolloutState === 'FAILED',
      primaryTaskDefinition: primary.taskDefinition === input.expectedTaskDefinition,
      publicIpEnabled: network.assignPublicIp === 'ENABLED',
      boundedSubnetIdentifiers: Array.isArray(network.subnets) && network.subnets.length > 0 && network.subnets.every(value => typeof value === 'string' && /^subnet-[0-9a-f]+$/.test(value)),
      exactlyOneSecurityGroup: Array.isArray(network.securityGroups) && network.securityGroups.length === 1,
      securityGroupIdentifier: Array.isArray(network.securityGroups) && network.securityGroups.length === 1 && typeof network.securityGroups[0] === 'string' && /^sg-[0-9a-f]+$/.test(network.securityGroups[0]),
    },
  }
}

export function validateRuntimeAuditIdentity(identity) {
  assert.equal(identity.Account, ACCOUNT)
  assert.ok(typeof identity.Arn === 'string' && identity.Arn.startsWith(`arn:aws:sts::${ACCOUNT}:assumed-role/plether-sepolia-runtime-image-audit/`), 'Use the existing read-only inventory capability')
}

export function diagnoseDrillBaseline(readAws, input) {
  validateDrillInputs(input)
  validateRuntimeAuditIdentity(readAws(['sts', 'get-caller-identity']))
  const response = readAws(['ecs', 'describe-services', '--cluster', DRILL.cluster, '--services', DRILL.service])
  const serviceBaseline = drillSnapshotEvidence(response, input)
  return {
    schemaVersion: 1, capability: DRILL.capability, evidenceSource: 'read-only-runtime-image-audit',
    baselineChecksPassed: Object.values(serviceBaseline.checks).every(value => value === true),
    serviceBaseline,
  }
}

export function validateDrillSnapshot(response, input) {
  assert.deepEqual(response.failures, [])
  const service = one(response.services, 'Expected only the isolated fixture')
  assert.equal(service.serviceName, DRILL.service)
  assert.equal(service.serviceArn, `arn:aws:ecs:${REGION}:${ACCOUNT}:service/${DRILL.cluster}/${DRILL.service}`)
  assert.equal(service.clusterArn, `arn:aws:ecs:${REGION}:${ACCOUNT}:cluster/${DRILL.cluster}`)
  assert.equal(service.status, 'ACTIVE')
  assert.equal(service.taskDefinition, input.expectedTaskDefinition)
  assert.equal(service.launchType, 'FARGATE')
  assert.equal(service.platformVersion, '1.4.0')
  assert.equal(service.deploymentController?.type, 'ECS')
  assert.equal(service.schedulingStrategy, 'REPLICA')
  assert.equal(service.desiredCount, 1)
  assert.equal(service.runningCount, 0, 'A healthy fixture is not the missing-image drill')
  assert.equal(service.pendingCount, 0, 'Wait until the deliberately missing deployment fails')
  assert.equal(service.enableExecuteCommand, false)
  assert.equal(service.deploymentConfiguration.minimumHealthyPercent, 0)
  assert.equal(service.deploymentConfiguration.maximumPercent, 100)
  assert.deepEqual(service.deploymentConfiguration.deploymentCircuitBreaker, { enable: true, rollback: false })
  const primary = one(service.deployments, 'An overlapping fixture rollout is unsupported')
  assert.equal(primary.status, 'PRIMARY')
  assert.equal(primary.rolloutState, 'FAILED', 'Missing-image failure must be observed before recovery')
  assert.equal(primary.taskDefinition, input.expectedTaskDefinition)
  const network = service.networkConfiguration?.awsvpcConfiguration
  assert.equal(network?.assignPublicIp, 'ENABLED')
  assert.ok(network.subnets.length > 0 && network.subnets.every(value => /^subnet-[0-9a-f]+$/.test(value)))
  assert.equal(network.securityGroups.length, 1)
  assert.match(network.securityGroups[0], /^sg-[0-9a-f]+$/)
  return service
}

export function validateDrillTask(task) {
  assert.equal(task.family, DRILL.family)
  assert.equal(task.status, 'ACTIVE')
  assert.equal(task.networkMode, 'awsvpc')
  assert.ok(task.requiresCompatibilities.includes('FARGATE'))
  assert.deepEqual(task.runtimePlatform, { cpuArchitecture: 'ARM64', operatingSystemFamily: 'LINUX' })
  assert.equal(task.executionRoleArn, `arn:aws:iam::${ACCOUNT}:role/${DRILL.executionRole}`)
  assert.ok(!task.taskRoleArn, 'The fixture cannot receive AWS task credentials')
  assert.equal(task.cpu, '256')
  assert.equal(task.memory, '512')
  assert.deepEqual(task.volumes ?? [], [])
  assert.deepEqual(task.containerDefinitions.map(container => container.name).sort(), Object.values(DRILL.containers).map(container => container.name).sort())
  for (const [kind, expected] of Object.entries(DRILL.containers)) {
    const container = one(task.containerDefinitions.filter(value => value.name === expected.name), 'Missing fixed fixture container')
    assert.deepEqual(container.command, expected.command)
    assert.deepEqual(container.entryPoint, expected.entryPoint)
    assert.equal(container.essential, kind !== 'init')
    assert.equal(container.readonlyRootFilesystem, true)
    assert.equal(container.user, '65534:65534')
    assert.equal(container.privileged ?? false, false)
    const linux = container.linuxParameters
    assert.deepEqual(Object.keys(linux ?? {}).sort(), ['capabilities', 'initProcessEnabled'])
    assert.equal(linux.initProcessEnabled, true)
    assert.ok(Object.keys(linux.capabilities ?? {}).every(key => ['add', 'drop'].includes(key)))
    assert.deepEqual(linux.capabilities.drop, ['ALL'])
    // ECS may materialize an omitted add list as []. Both forms grant no
    // capability; reject any nonempty addition or other Linux override.
    assert.deepEqual(linux.capabilities.add ?? [], [])
    assert.deepEqual(container.secrets ?? [], [])
    assert.deepEqual(container.mountPoints ?? [], [])
    assert.deepEqual(container.portMappings ?? [], [])
    assert.equal(container.firelensConfiguration, undefined)
    assert.deepEqual(container.dependsOn ?? [], expected.dependsOn ?? [])
    if (kind === 'main') assert.deepEqual(container.environment, expected.environment)
    else if (kind === 'init') assert.deepEqual(container.environment ?? [], [])
    else assert.deepEqual(container.environment, [{ name: 'SERVICE_VERSION', value: DRILL.release.sourceCommit }])
    assert.equal(container.logConfiguration?.logDriver, 'awslogs')
    assert.deepEqual(container.logConfiguration.secretOptions ?? [], [])
    assert.deepEqual(container.logConfiguration.options, {
      'awslogs-group': DRILL.logGroup, 'awslogs-region': REGION,
      'awslogs-stream-prefix': DRILL.logStreamPrefix,
    })
    assert.equal(container.image, kind === 'router' ? DRILL.release.routerImage : DRILL.missingApplicationImage)
  }
}

export function drillProgressEvidence(events, _, now = Date.now()) {
  const records = events.map(event => { try { return { ...JSON.parse(event.message), timestamp: event.timestamp } } catch { return {} } })
  const heartbeats = records.filter(record => record.event === 'recovery_drill_heartbeat' && record.container_name === DRILL.service && record.signingEnabled === false)
  const latest = heartbeats.at(-1)
  if (!latest || latest.timestamp < now - 45_000 || latest.timestamp > now + 5_000) return undefined
  if (latest.timestamp - heartbeats[0].timestamp < 120_000) return undefined
  assert.ok(!records.some(record => record.timestamp >= heartbeats[0].timestamp && ['ERROR', 'error', 'FATAL', 'fatal'].includes(record.level)))
  return { event: latest.event, timestamp: latest.timestamp, healthySpanSeconds: (latest.timestamp - heartbeats[0].timestamp) / 1000, signingEnabled: false }
}

export const DRILL_POLICY = Object.freeze({
  name: DRILL.capability, cluster: DRILL.cluster, logGroup: DRILL.logGroup,
  applicationRepository: 'plether-api-sepolia', routerRepository: 'plether-otel-log-router-sepolia',
  validateSnapshot: validateDrillSnapshot, validateTask: validateDrillTask,
  snapshotEvidence: drillSnapshotEvidence,
  configurationFingerprint, manifestResponse: manifestFromBatchGetImage, verifyImageGraph,
  render: renderRecoveredTaskDefinition, validateImageOnlyChange,
  registeredRevisionPattern: new RegExp(`^arn:aws:ecs:${REGION}:${ACCOUNT}:task-definition/${DRILL.family}:[1-9][0-9]*$`),
  verifyRunningTask: (...args) => verifyRunningTaskForPolicy(...args, { init: DRILL.containers.init.name }),
  logStream: taskArn => `${DRILL.logStreamPrefix}/${DRILL.service}/${taskArn.split('/').at(-1)}`,
  progressEvidence: drillProgressEvidence,
})

async function main() {
  const [mode] = process.argv.slice(2)
  validateDrillConfig()
  if (mode === 'environment' || mode === 'environment-credentials') {
    assert.equal(process.env.GITHUB_REF, 'refs/heads/master')
    assert.equal(process.env.GITHUB_REPOSITORY, 'Plether-Fi/plether-app')
    if (mode === 'environment-credentials') assert.equal(process.env.AWS_RECOVERY_DRILL_ROLE_ARN, `arn:aws:iam::${ACCOUNT}:role/${DRILL.deploymentRole}`)
    const gh = endpoint => JSON.parse(execFileSync('gh', ['api', endpoint], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }))
    const endpoint = 'repos/Plether-Fi/plether-app/environments/backend-admin-sepolia'
    validateProtectionPolicy(gh(endpoint), gh(`${endpoint}/deployment-branch-policies?per_page=100`))
    console.log('Protected master-only isolated recovery capability verified.')
    return
  }
  const input = {
    service: DRILL.service, releaseId: DRILL.release.id,
    expectedTaskDefinition: process.env.EXPECTED_TASK_DEFINITION,
    expectedConfigurationSha256: process.env.EXPECTED_CONFIGURATION_SHA256,
    confirmation: process.env.CONFIRMATION, operatorNote: process.env.OPERATOR_NOTE,
  }
  if (mode === 'diagnose') {
    assert.equal(process.env.GITHUB_REF, 'refs/heads/master')
    assert.equal(process.env.GITHUB_REPOSITORY, 'Plether-Fi/plether-app')
    const result = diagnoseDrillBaseline(aws, input)
    console.log(JSON.stringify(result, null, 2))
    if (!result.baselineChecksPassed) process.exitCode = 1
    return
  }
  if (mode === 'inspect') {
    assert.equal(aws(['sts', 'get-caller-identity']).Account, ACCOUNT)
    const response = aws(['ecs', 'describe-services', '--cluster', DRILL.cluster, '--services', DRILL.service])
    assert.deepEqual(response.failures, [])
    const service = one(response.services, 'Expected isolated fixture')
    const task = aws(['ecs', 'describe-task-definition', '--task-definition', service.taskDefinition]).taskDefinition
    validateDrillSnapshot(response, { ...input, expectedTaskDefinition: service.taskDefinition })
    validateDrillTask(task)
    console.log(JSON.stringify({ capability: DRILL.capability, expectedTaskDefinition: service.taskDefinition, expectedConfigurationSha256: configurationFingerprint(service, task) }, null, 2))
    return
  }
  const fingerprint = drillInputFingerprint(input)
  console.log(`Dispatch input fingerprint: sha256:${fingerprint}`)
  if (mode === 'fingerprint') return
  assert.equal(mode, 'recover')
  assert.equal(process.env.GITHUB_REF, 'refs/heads/master')
  assert.equal(process.env.EXPECTED_INPUT_FINGERPRINT, fingerprint)
  const identity = aws(['sts', 'get-caller-identity'])
  assert.equal(identity.Account, ACCOUNT)
  assert.ok(identity.Arn.startsWith(`arn:aws:sts::${ACCOUNT}:assumed-role/${DRILL.deploymentRole}/`), 'Use the dedicated fixture-only capability')
  const completed = await runImageRecovery({ input, release: DRILL.release, inputFingerprint: fingerprint,
    directory: process.env.RECOVERY_EVIDENCE_DIR, policy: DRILL_POLICY, aws })
  console.log(JSON.stringify({ capability: DRILL.capability, ...completed, signingEnabled: false }))
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  main().catch(() => { console.error('Isolated image-recovery drill refused; inspect the credential-free evidence. No production service or financial state was changed by this entrypoint.'); process.exitCode = 1 })
}
