// Protected Sepolia image recovery. No build, arbitrary override, signing or unpause capability.
import assert from 'node:assert/strict'
import { createHash } from 'node:crypto'
import { execFileSync } from 'node:child_process'
import fs from 'node:fs'
import path from 'node:path'
import { pathToFileURL } from 'node:url'
import { validateBackendDeployment } from './validate-perps-deployment.mjs'

export const ACCOUNT = '932542905614'
export const REGION = 'ap-southeast-1'
export const CLUSTER = 'plether-sepolia'
const REGISTRY = `${ACCOUNT}.dkr.ecr.${REGION}.amazonaws.com`
const DIGEST = /^sha256:[0-9a-f]{64}$/
const SHA = /^[0-9a-f]{40}$/
const SERVER_FIELDS = new Set(['taskDefinitionArn', 'revision', 'status', 'requiresAttributes', 'compatibilities', 'registeredAt', 'registeredBy', 'deregisteredAt'])
export const SERVICES = {
  'plether-aa-reconciler': { family: 'plether-sepolia-aa-reconciler', command: ['/usr/local/bin/plether-aa-reconciler'], init: 'aa-reconciler-tmp-init', progress: 'aa_reconciler_heartbeat', roles: ['aa-reconciler'] },
  'plether-liquidation-worker': { family: 'plether-sepolia-liquidation-worker', command: ['plether-liquidation-worker'], progress: 'liquidation_sweep_finished', roles: ['ecs', 'api'] },
  'plether-position-protection-worker': { family: 'plether-sepolia-protection-worker', command: ['node', '/app/protection/main.mjs'], progress: 'protection_worker_heartbeat', roles: ['ecs', 'api'] },
}
const clone = value => structuredClone(value)
const sorted = value => Array.isArray(value) ? value.map(sorted) : value && typeof value === 'object'
  ? Object.fromEntries(Object.keys(value).sort().map(key => [key, sorted(value[key])])) : value
const hash = value => createHash('sha256').update(JSON.stringify(sorted(value))).digest('hex')
const one = (values, message) => { assert.equal(values.length, 1, message); return values[0] }
const printable = value => typeof value === 'string' && /^[\x20-\x7e]{1,512}$/.test(value) && value.trim() === value

export function normalizeTaskDefinition(task) {
  return Object.fromEntries(Object.entries(task).filter(([key]) => !SERVER_FIELDS.has(key)))
}

export function validateInputs(input) {
  assert.ok(Object.hasOwn(SERVICES, input.service), 'Unknown recovery service')
  assert.match(input.releaseId, /^[a-z0-9][a-z0-9-]{0,79}$/)
  assert.match(input.expectedTaskDefinition, new RegExp(`^arn:aws:ecs:${REGION}:${ACCOUNT}:task-definition/${SERVICES[input.service].family}:[1-9][0-9]*$`))
  assert.match(input.expectedConfigurationSha256, /^[0-9a-f]{64}$/)
  assert.equal(input.confirmation, 'RECOVER BACKEND ON SEPOLIA')
  assert.ok(printable(input.operatorNote), 'A bounded non-secret incident note is required')
  return input
}

export function inputFingerprint(input, release) {
  validateInputs(input)
  return hash({ schema: 1, ...input, release })
}

export function validateProtectionPolicy(environment, branches) {
  assert.equal(environment.name, 'backend-admin-sepolia')
  assert.equal(environment.can_admins_bypass, false)
  assert.equal(environment.deployment_branch_policy?.protected_branches, false)
  assert.equal(environment.deployment_branch_policy?.custom_branch_policies, true)
  const reviewers = one(environment.protection_rules.filter(rule => rule.type === 'required_reviewers'), 'One required-reviewer rule is mandatory')
  assert.equal(reviewers.prevent_self_review, false)
  assert.ok(reviewers.reviewers.length > 0, 'A maintainer must approve this exact run')
  assert.equal(branches.total_count, 1)
  assert.deepEqual(branches.branch_policies.map(({ name, type }) => ({ name, type })), [{ name: 'master', type: 'branch' }])
}

export function qualifiedRelease(catalog, id, service, core) {
  assert.equal(catalog.schemaVersion, 1)
  assert.equal(catalog.accountId, ACCOUNT)
  assert.equal(catalog.region, REGION)
  assert.equal(new Set(catalog.releases.map(release => release.id)).size, catalog.releases.length, 'Duplicate release IDs')
  const release = one(catalog.releases.filter(value => value.id === id), 'Unknown qualified release')
  assert.ok(release.services.includes(service), 'Release is not qualified for this service')
  assert.ok(release.services.every(name => Object.hasOwn(SERVICES, name)))
  assert.match(release.sourceCommit, SHA)
  assert.match(release.routerSourceCommit, SHA)
  assert.equal(release.coreManifestSha256, core.release.manifestSha256, 'Recovery release belongs to another Core deployment')
  assert.equal(core.network.chainId, 421614)
  parseImage(release.applicationImage, 'plether-api-sepolia')
  parseImage(release.routerImage, 'plether-otel-log-router-sepolia')
  assert.ok(Array.isArray(release.qualificationEvidence) && release.qualificationEvidence.length > 0, 'Qualification evidence is required')
  return release
}

export function parseImage(image, repository) {
  assert.equal(typeof image, 'string')
  const prefix = `${REGISTRY}/${repository}@`
  assert.ok(image.startsWith(prefix), 'Image must be a digest in the expected Singapore repository')
  const digest = image.slice(prefix.length)
  assert.match(digest, DIGEST)
  return { repository, digest }
}

export function validateServiceSnapshot(response, input) {
  assert.deepEqual(response.failures, [], 'ECS service lookup failed')
  const service = one(response.services, 'Expected one service')
  assert.equal(service.serviceName, input.service)
  assert.equal(service.status, 'ACTIVE')
  assert.equal(service.serviceArn, `arn:aws:ecs:${REGION}:${ACCOUNT}:service/${CLUSTER}/${input.service}`)
  assert.equal(service.clusterArn, `arn:aws:ecs:${REGION}:${ACCOUNT}:cluster/${CLUSTER}`)
  assert.equal(service.taskDefinition, input.expectedTaskDefinition, 'Service changed since the approved recovery input')
  assert.equal(service.launchType, 'FARGATE')
  assert.equal(service.platformVersion, '1.4.0')
  assert.equal(service.deploymentController?.type, 'ECS')
  assert.equal(service.schedulingStrategy, 'REPLICA')
  assert.equal(service.desiredCount, 1, 'Recovery cannot activate a stopped service')
  assert.ok([0, 1].includes(service.runningCount), 'Overlapping active tasks')
  assert.equal(service.pendingCount, 0, 'Wait for or resolve pending tasks before recovery')
  assert.equal(service.deploymentConfiguration.minimumHealthyPercent, 0)
  assert.equal(service.deploymentConfiguration.maximumPercent, 100, 'Recovery must preserve single-active deployment')
  const deployment = one(service.deployments, 'Concurrent deployment is not recoverable by this action')
  assert.equal(deployment.status, 'PRIMARY')
  assert.ok(['COMPLETED', 'FAILED'].includes(deployment.rolloutState), 'A rollout is already in progress')
  assert.equal(deployment.taskDefinition, input.expectedTaskDefinition)
  if (input.service === 'plether-aa-reconciler') {
    assert.equal(service.deploymentConfiguration.deploymentCircuitBreaker?.enable, true)
    assert.equal(service.deploymentConfiguration.deploymentCircuitBreaker?.rollback, true)
  }
  const network = service.networkConfiguration?.awsvpcConfiguration
  assert.equal(network?.assignPublicIp, 'ENABLED')
  assert.ok(network.subnets.length > 0 && network.subnets.every(value => /^subnet-[0-9a-f]+$/.test(value)))
  assert.ok(network.securityGroups.length > 0 && network.securityGroups.every(value => /^sg-[0-9a-f]+$/.test(value)))
  return service
}

export function validateTaskDefinition(task, service, core, canonicalTask) {
  const policy = SERVICES[service]
  assert.equal(task.family, policy.family)
  assert.equal(task.status, 'ACTIVE')
  assert.equal(task.networkMode, 'awsvpc')
  assert.ok(task.requiresCompatibilities.includes('FARGATE'))
  assert.deepEqual(task.runtimePlatform, { cpuArchitecture: 'ARM64', operatingSystemFamily: 'LINUX' })
  const rolePrefix = `arn:aws:iam::${ACCOUNT}:role/plether-sepolia-`
  assert.ok(policy.roles.some(role => task.executionRoleArn === `${rolePrefix}${role}-execution` && task.taskRoleArn === `${rolePrefix}${role}-task`), 'Unexpected capability roles')
  const names = [service, 'otel-log-router', ...(policy.init ? [policy.init] : [])].sort()
  assert.deepEqual(task.containerDefinitions.map(container => container.name).sort(), names, 'Unexpected containers')
  const main = one(task.containerDefinitions.filter(container => container.name === service), 'Missing main container')
  assert.deepEqual(main.command, policy.command)
  assert.equal(main.entryPoint, undefined, 'Entrypoint override is unsupported')
  assert.equal(main.essential, true)
  for (const container of task.containerDefinitions) {
    assert.equal(new Set((container.environment ?? []).map(value => value.name)).size, (container.environment ?? []).length, 'Duplicate environment entries')
    assert.equal(new Set((container.secrets ?? []).map(value => value.name)).size, (container.secrets ?? []).length, 'Duplicate secret entries')
    assert.ok(container.image.startsWith(`${REGISTRY}/`), 'Foreign image repository')
    for (const secret of container.secrets ?? []) assert.ok(secret.valueFrom.startsWith(`arn:aws:ssm:${REGION}:${ACCOUNT}:parameter/plether/sepolia/`), 'Foreign secret reference')
  }
  const router = task.containerDefinitions.find(container => container.name === 'otel-log-router')
  assert.equal(router.essential, true)
  assert.equal(router.firelensConfiguration?.type, 'fluentbit')
  assert.equal((router.environment ?? []).filter(value => value.name === 'SERVICE_VERSION').length, 1, 'Missing service version metadata')
  if (policy.init) {
    const init = task.containerDefinitions.find(container => container.name === policy.init)
    assert.equal(init.essential, false)
    assert.equal(init.readonlyRootFilesystem, true)
    assert.equal(init.user, '0:0')
    assert.deepEqual(init.entryPoint, ['/bin/sh', '-c'])
    assert.deepEqual(init.command, ['chmod 1777 /tmp'])
    assert.equal(init.image, main.image)
  }
  validateBackendDeployment(core, task, canonicalTask)
}

export function configurationFingerprint(service, task) {
  // Task definitions are immutable; selected revision plus the service's mutable
  // deployment/network/capacity settings bind the approval to the whole baseline.
  const keys = ['serviceArn', 'clusterArn', 'serviceName', 'taskDefinition', 'desiredCount', 'launchType', 'platformVersion', 'deploymentController', 'deploymentConfiguration', 'networkConfiguration', 'capacityProviderStrategy', 'loadBalancers', 'serviceRegistries', 'enableExecuteCommand', 'enableECSManagedTags', 'propagateTags', 'schedulingStrategy']
  return hash({ service: Object.fromEntries(keys.filter(key => key in service).map(key => [key, service[key]])), task: normalizeTaskDefinition(task) })
}

export function renderRecoveredTaskDefinition(task, service, release) {
  const recovered = clone(normalizeTaskDefinition(task))
  for (const container of recovered.containerDefinitions) {
    container.image = container.name === 'otel-log-router' ? release.routerImage : release.applicationImage
    if (container.name === 'otel-log-router') {
      for (const entry of container.environment) if (entry.name === 'SERVICE_VERSION') entry.value = release.sourceCommit
    }
  }
  validateImageOnlyChange(task, recovered, service, release)
  return recovered
}

export function validateImageOnlyChange(before, after, service, release) {
  const expected = clone(normalizeTaskDefinition(before))
  for (const container of expected.containerDefinitions) {
    container.image = container.name === 'otel-log-router' ? release.routerImage : release.applicationImage
    if (container.name === 'otel-log-router') for (const entry of container.environment) if (entry.name === 'SERVICE_VERSION') entry.value = release.sourceCommit
  }
  assert.deepEqual(normalizeTaskDefinition(after), expected, `Recovery changed configuration beyond ${service} image references and version metadata`)
}

export async function verifyImageGraph(image, repository, readManifest) {
  const { digest } = parseImage(image, repository)
  const visited = new Set(), runtimeDigests = new Set([digest])
  let arm64 = false
  async function visit(current, platform, depth = 0) {
    assert.ok(depth <= 4 && visited.size < 100, 'Manifest graph exceeds recovery bounds')
    if (platform?.os === 'linux' && platform.architecture === 'arm64') { arm64 = true; runtimeDigests.add(current) }
    if (visited.has(current)) return
    visited.add(current)
    const raw = await readManifest(repository, current)
    assert.equal(`sha256:${createHash('sha256').update(raw).digest('hex')}`, current, 'Manifest digest does not match returned bytes')
    const manifest = JSON.parse(raw)
    if (Array.isArray(manifest.manifests)) {
      assert.ok(manifest.manifests.length > 0)
      for (const child of manifest.manifests) { assert.match(child.digest, DIGEST); await visit(child.digest, child.platform, depth + 1) }
    } else {
      assert.ok(Array.isArray(manifest.layers) && manifest.config?.digest, 'Invalid image manifest')
    }
  }
  await visit(digest)
  assert.ok(arm64, 'Recovery requires a verified Linux ARM64 image index')
  return { digest, allManifestDigests: [...visited].sort(), runtimeDigests: [...runtimeDigests].sort() }
}

export function manifestFromBatchGetImage(response, digest) {
  assert.deepEqual(response.failures, [], 'Required manifest lookup failed')
  assert.ok(Array.isArray(response.images) && response.images.length > 0, 'Required manifest is unavailable')
  // ECR can return one identical entry per tag even for a digest lookup. Those
  // aliases do not represent different images; any disagreement still fails
  // closed before the manifest hash or runtime architecture is trusted.
  for (const image of response.images) {
    assert.equal(image.imageId?.imageDigest, digest, 'ECR returned an unrequested image')
    assert.equal(typeof image.imageManifest, 'string')
    assert.ok(image.imageManifest.length > 0)
    assert.equal(image.imageManifest, response.images[0].imageManifest, 'ECR returned conflicting manifest bytes')
    assert.equal(image.imageManifestMediaType, response.images[0].imageManifestMediaType, 'ECR returned conflicting manifest media types')
  }
  return response.images[0].imageManifest
}

export function verifyRunningTask(task, service, selectedRevision, release, applicationGraph, routerGraph) {
  assert.equal(task.taskDefinitionArn, selectedRevision)
  assert.equal(task.lastStatus, 'RUNNING')
  assert.equal(task.desiredStatus, 'RUNNING')
  const policy = SERVICES[service]
  assert.deepEqual(task.containers.map(container => container.name).sort(), [service, 'otel-log-router', ...(policy.init ? [policy.init] : [])].sort())
  for (const container of task.containers) {
    const graph = container.name === 'otel-log-router' ? routerGraph : applicationGraph
    const image = container.name === 'otel-log-router' ? release.routerImage : release.applicationImage
    assert.equal(container.image, image)
    assert.ok(graph.runtimeDigests.includes(container.imageDigest), 'Running image does not match qualified digest')
    if (container.name === policy.init) { assert.equal(container.lastStatus, 'STOPPED'); assert.equal(container.exitCode, 0) }
    else assert.equal(container.lastStatus, 'RUNNING')
  }
}

export function progressEvidence(events, service, now = Date.now()) {
  const policy = SERVICES[service]
  const records = events.map(event => { try { return { ...JSON.parse(event.message), timestamp: event.timestamp } } catch { return {} } })
  const matching = records.filter(record => record.event === policy.progress && record.container_name === service
    && (service !== 'plether-position-protection-worker' || (record.caughtUp === true && String(record.lagBlocks) === '0'))
    && (service !== 'plether-liquidation-worker' || record.completed === true))
  const event = matching.at(-1)
  if (!event) return undefined
  if (event.timestamp < now - 120_000 || event.timestamp > now + 5_000) return undefined
  if (service === 'plether-aa-reconciler' && event.timestamp - matching[0].timestamp < 120_000) return undefined
  const start = event.timestamp
  assert.ok(!records.some(record => record.timestamp >= start && ['ERROR', 'error', 'FATAL', 'fatal'].includes(record.level)), 'Worker failed after progress evidence')
  return { event: event.event, timestamp: event.timestamp }
}

function aws(args) {
  try {
    const mutation = args[0] === 'ecs' && ['register-task-definition', 'update-service'].includes(args[1])
    return JSON.parse(execFileSync('aws', ['--region', REGION, ...args, '--output', 'json', '--no-cli-pager', '--cli-connect-timeout', '10', '--cli-read-timeout', '20'], {
      encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'], maxBuffer: 16 * 1024 * 1024,
      env: { ...process.env, ...(mutation ? { AWS_MAX_ATTEMPTS: '1' } : {}) },
    }))
  } catch { throw Error(`AWS ${args[0]} ${args[1]} failed; inspect restricted operator evidence`) }
}
const sleep = milliseconds => new Promise(resolve => setTimeout(resolve, milliseconds))
const responseService = name => aws(['ecs', 'describe-services', '--cluster', CLUSTER, '--services', name])
const definition = arn => aws(['ecs', 'describe-task-definition', '--task-definition', arn]).taskDefinition
const readJson = filename => JSON.parse(fs.readFileSync(filename, 'utf8'))

async function main() {
  const [mode] = process.argv.slice(2)
  if (mode === 'environment') {
    assert.equal(process.env.GITHUB_REF, 'refs/heads/master')
    assert.equal(process.env.GITHUB_REPOSITORY, 'Plether-Fi/plether-app')
    const gh = endpoint => JSON.parse(execFileSync('gh', ['api', endpoint], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }))
    const endpoint = 'repos/Plether-Fi/plether-app/environments/backend-admin-sepolia'
    validateProtectionPolicy(gh(endpoint), gh(`${endpoint}/deployment-branch-policies?per_page=100`))
    console.log('Protected master-only backend approval policy verified.')
    return
  }
  const input = {
    service: process.env.RECOVERY_SERVICE, releaseId: process.env.RECOVERY_RELEASE,
    expectedTaskDefinition: process.env.EXPECTED_TASK_DEFINITION,
    expectedConfigurationSha256: process.env.EXPECTED_CONFIGURATION_SHA256,
    confirmation: process.env.CONFIRMATION, operatorNote: process.env.OPERATOR_NOTE,
  }
  const core = readJson('config/perps/arbitrum-sepolia-v2.0.0.json')
  const catalog = readJson('config/operations/backend-recovery-releases.json')
  if (mode === 'inspect') {
    assert.ok(Object.hasOwn(SERVICES, input.service))
    assert.equal(aws(['sts', 'get-caller-identity']).Account, ACCOUNT)
    const response = responseService(input.service)
    assert.deepEqual(response.failures, [])
    const service = one(response.services, 'Expected one service')
    const task = definition(service.taskDefinition)
    console.log(JSON.stringify({ service: input.service, expectedTaskDefinition: service.taskDefinition, expectedConfigurationSha256: configurationFingerprint(service, task) }, null, 2))
    return
  }
  validateInputs(input)
  const release = qualifiedRelease(catalog, input.releaseId, input.service, core)
  const fingerprint = inputFingerprint(input, release)
  console.log(`Dispatch input fingerprint: sha256:${fingerprint}`)
  if (mode === 'fingerprint') return
  assert.equal(mode, 'recover', 'Usage: recover-backend.mjs inspect|fingerprint|recover')
  assert.equal(process.env.GITHUB_REF, 'refs/heads/master')
  assert.equal(process.env.EXPECTED_INPUT_FINGERPRINT, fingerprint, 'Inputs or qualification changed after preflight approval')
  assert.equal(aws(['sts', 'get-caller-identity']).Account, ACCOUNT)
  const directory = process.env.RECOVERY_EVIDENCE_DIR
  assert.ok(directory && path.isAbsolute(directory), 'An absolute evidence directory is required')
  fs.mkdirSync(directory, { recursive: true, mode: 0o700 })
  const evidenceFile = path.join(directory, 'recovery.json')
  const evidence = { schemaVersion: 1, inputFingerprint: fingerprint, service: input.service, releaseId: release.id, previousTaskDefinition: input.expectedTaskDefinition, operatorNoteLength: input.operatorNote.length, applicationImage: release.applicationImage, routerImage: release.routerImage, outcome: 'preflight' }
  const persist = () => fs.writeFileSync(evidenceFile, `${JSON.stringify(evidence, null, 2)}\n`, { mode: 0o600 })
  persist()
  evidence.check = 'service-baseline'; persist()
  const snapshot = validateServiceSnapshot(responseService(input.service), input)
  const previous = definition(input.expectedTaskDefinition)
  assert.equal(previous.taskDefinitionArn, input.expectedTaskDefinition)
  const apiResponse = responseService('plether-api')
  assert.deepEqual(apiResponse.failures, [])
  const api = one(apiResponse.services, 'Canonical API is unavailable')
  const canonical = definition(api.taskDefinition)
  evidence.check = 'task-configuration'; persist()
  validateTaskDefinition(previous, input.service, core, canonical)
  assert.equal(configurationFingerprint(snapshot, previous), input.expectedConfigurationSha256, 'Approved configuration fingerprint changed')
  const manifestReader = async (repository, digest) => {
    const response = aws(['ecr', 'batch-get-image', '--repository-name', repository, '--image-ids', `imageDigest=${digest}`])
    return manifestFromBatchGetImage(response, digest)
  }
  evidence.check = 'qualified-image-manifests'; persist()
  const applicationGraph = await verifyImageGraph(release.applicationImage, 'plether-api-sepolia', manifestReader)
  const routerGraph = await verifyImageGraph(release.routerImage, 'plether-otel-log-router-sepolia', manifestReader)
  evidence.verifiedManifests = { application: applicationGraph.allManifestDigests, router: routerGraph.allManifestDigests }
  const rendered = renderRecoveredTaskDefinition(previous, input.service, release)
  const taskFile = path.join(directory, 'task-definition.private.json')
  let newRevision
  try {
    fs.writeFileSync(taskFile, JSON.stringify(rendered), { mode: 0o600 })
    const registered = aws(['ecs', 'register-task-definition', '--cli-input-json', `file://${taskFile}`]).taskDefinition
    newRevision = registered.taskDefinitionArn
    assert.match(newRevision, new RegExp(`^arn:aws:ecs:${REGION}:${ACCOUNT}:task-definition/${SERVICES[input.service].family}:[1-9][0-9]*$`))
    validateImageOnlyChange(previous, registered, input.service, release)
    evidence.newTaskDefinition = newRevision
    evidence.outcome = 'registered'
    persist() // Retain a recovery handle before the ambiguous service mutation.
    const current = validateServiceSnapshot(responseService(input.service), input)
    assert.equal(configurationFingerprint(current, definition(current.taskDefinition)), input.expectedConfigurationSha256, 'Service/configuration changed before promotion')
    evidence.updateRequestedAt = Date.now()
    evidence.outcome = 'update-requested'
    evidence.check = 'stable-worker-progress'
    persist()
    // ECS has no compare-and-swap parameter. All normal/recovery mutations use
    // the same workflow concurrency group; the final read detects intervening
    // changes before this single update. Do not retry an ambiguous update.
    aws(['ecs', 'update-service', '--cluster', CLUSTER, '--service', input.service, '--task-definition', newRevision])
    const deadline = Date.now() + 15 * 60_000
    let completed
    while (Date.now() < deadline) {
      const response = responseService(input.service)
      assert.deepEqual(response.failures, [])
      const service = one(response.services, 'Recovery service disappeared')
      assert.equal(service.taskDefinition, newRevision, 'Another mutation selected a different task definition')
      assert.equal(service.desiredCount, 1)
      assert.equal(service.deploymentConfiguration.minimumHealthyPercent, 0)
      assert.equal(service.deploymentConfiguration.maximumPercent, 100)
      const primary = one(service.deployments.filter(deployment => deployment.status === 'PRIMARY'), 'Missing PRIMARY deployment')
      assert.notEqual(primary.rolloutState, 'FAILED', 'Recovery deployment failed')
      if (service.runningCount === 1 && service.pendingCount === 0 && service.deployments.length === 1 && primary.rolloutState === 'COMPLETED') {
        const listing = aws(['ecs', 'list-tasks', '--cluster', CLUSTER, '--service-name', input.service, '--desired-status', 'RUNNING'])
        const taskArn = one(listing.taskArns, 'Expected one running task')
        const tasks = aws(['ecs', 'describe-tasks', '--cluster', CLUSTER, '--tasks', taskArn])
        assert.deepEqual(tasks.failures, [])
        const task = one(tasks.tasks, 'Expected one task')
        verifyRunningTask(task, input.service, newRevision, release, applicationGraph, routerGraph)
        const stream = `firelens-cloudwatch.${input.service}-firelens-${taskArn.split('/').at(-1)}`
        const logs = aws(['logs', 'filter-log-events', '--log-group-name', '/ecs/plether-sepolia', '--log-stream-names', stream, '--start-time', String(evidence.updateRequestedAt)])
        const progress = progressEvidence(logs.events, input.service)
        if (progress) { completed = { taskArn, taskDefinition: newRevision, progress }; break }
      }
      await sleep(15_000)
    }
    assert.ok(completed, 'Recovery timed out without stable worker progress')
    evidence.outcome = 'recovered'
    evidence.completed = completed
    persist()
    console.log(JSON.stringify(completed))
    console.log('Image recovery complete. This workflow never resumes sponsorship.')
  } catch (error) {
    evidence.outcome = 'failed-inspect-required'
    evidence.failureClass = error?.code === 'ERR_ASSERTION' ? 'POLICY_CHECK_FAILED' : 'OPERATION_FAILED'
    persist()
    // A prior image may be unavailable. Never roll back blindly, stop a possibly
    // accepted task, deregister its revision, or clear the financial breaker.
    throw error
  } finally { fs.rmSync(taskFile, { force: true }) }
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  // Assertion diffs can include complete task environments. Never put them, raw
  // AWS responses or provider details into the workflow's broadly visible log.
  main().catch(() => { console.error('Backend image recovery refused. Inspect the credential-free recovery artifact and restricted task diagnostics. No automatic rollback or sponsorship resume was attempted.'); process.exitCode = 1 })
}
