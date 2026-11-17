// The production and isolated non-signing recovery entrypoints share this exact
// mutation/verification sequence. Each entrypoint supplies its fixed, reviewed
// capability policy; workflow inputs cannot select a different policy.
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'

const one = (values, message) => { assert.equal(values.length, 1, message); return values[0] }
const sleep = milliseconds => new Promise(resolve => setTimeout(resolve, milliseconds))

export async function runImageRecovery({ input, release, inputFingerprint, directory, policy, aws, now = Date.now, pause = sleep }) {
  assert.ok(Object.isFrozen(policy), 'Recovery policy must be fixed by the reviewed entrypoint')
  assert.ok(directory && path.isAbsolute(directory), 'An absolute evidence directory is required')
  fs.mkdirSync(directory, { recursive: true, mode: 0o700 })
  const evidenceFile = path.join(directory, 'recovery.json')
  const taskFile = path.join(directory, 'task-definition.private.json')
  const evidence = {
    schemaVersion: 1, capability: policy.name, inputFingerprint, service: input.service,
    releaseId: release.id, previousTaskDefinition: input.expectedTaskDefinition,
    operatorNoteLength: input.operatorNote.length, applicationImage: release.applicationImage,
    routerImage: release.routerImage, outcome: 'preflight',
  }
  const persist = () => fs.writeFileSync(evidenceFile, `${JSON.stringify(evidence, null, 2)}\n`, { mode: 0o600 })
  const responseService = () => aws(['ecs', 'describe-services', '--cluster', policy.cluster, '--services', input.service])
  const definition = arn => aws(['ecs', 'describe-task-definition', '--task-definition', arn]).taskDefinition
  persist()
  try {
    evidence.check = 'service-baseline'; persist()
    const initialResponse = responseService()
    if (policy.snapshotEvidence) {
      evidence.serviceBaseline = policy.snapshotEvidence(initialResponse, input)
      persist()
    }
    const snapshot = policy.validateSnapshot(initialResponse, input)
    evidence.check = 'task-definition-lookup'; persist()
    const previous = definition(input.expectedTaskDefinition)
    evidence.check = 'task-definition-identity'; persist()
    assert.equal(previous.taskDefinitionArn, input.expectedTaskDefinition)
    evidence.check = 'task-configuration'; persist()
    await policy.validateTask(previous)
    assert.equal(policy.configurationFingerprint(snapshot, previous), input.expectedConfigurationSha256, 'Approved configuration fingerprint changed')
    const manifestReader = async (repository, digest) => policy.manifestResponse(aws(['ecr', 'batch-get-image', '--repository-name', repository, '--image-ids', `imageDigest=${digest}`]), digest)
    evidence.check = 'qualified-image-manifests'; persist()
    const applicationGraph = await policy.verifyImageGraph(release.applicationImage, policy.applicationRepository, manifestReader)
    const routerGraph = await policy.verifyImageGraph(release.routerImage, policy.routerRepository, manifestReader)
    evidence.verifiedManifests = { application: applicationGraph.allManifestDigests, router: routerGraph.allManifestDigests }
    const rendered = policy.render(previous, input.service, release)
    fs.writeFileSync(taskFile, JSON.stringify(rendered), { mode: 0o600 })
    const registered = aws(['ecs', 'register-task-definition', '--cli-input-json', `file://${taskFile}`]).taskDefinition
    const newRevision = registered.taskDefinitionArn
    assert.match(newRevision, policy.registeredRevisionPattern)
    policy.validateImageOnlyChange(previous, registered, input.service, release)
    evidence.newTaskDefinition = newRevision
    evidence.outcome = 'registered'
    persist() // Retain a recovery handle before the ambiguous service mutation.
    const current = policy.validateSnapshot(responseService(), input)
    assert.equal(policy.configurationFingerprint(current, definition(current.taskDefinition)), input.expectedConfigurationSha256, 'Service/configuration changed before promotion')
    evidence.updateRequestedAt = now()
    evidence.outcome = 'update-requested'
    evidence.check = 'stable-worker-progress'
    persist()
    // ECS has no compare-and-swap parameter. Both workflows use the same mutation
    // lock, restrict their IAM capabilities, and perform this final baseline read.
    // Do not retry an ambiguous registration/update or roll back to a missing image.
    aws(['ecs', 'update-service', '--cluster', policy.cluster, '--service', input.service, '--task-definition', newRevision])
    const deadline = now() + 15 * 60_000
    let completed
    while (now() < deadline) {
      const response = responseService()
      assert.deepEqual(response.failures, [])
      const service = one(response.services, 'Recovery service disappeared')
      assert.equal(service.taskDefinition, newRevision, 'Another mutation selected a different task definition')
      assert.equal(service.desiredCount, 1)
      assert.equal(service.deploymentConfiguration.minimumHealthyPercent, 0)
      assert.equal(service.deploymentConfiguration.maximumPercent, 100)
      const primary = one(service.deployments.filter(deployment => deployment.status === 'PRIMARY'), 'Missing PRIMARY deployment')
      assert.notEqual(primary.rolloutState, 'FAILED', 'Recovery deployment failed')
      if (service.runningCount === 1 && service.pendingCount === 0 && service.deployments.length === 1 && primary.rolloutState === 'COMPLETED') {
        const listing = aws(['ecs', 'list-tasks', '--cluster', policy.cluster, '--service-name', input.service, '--desired-status', 'RUNNING'])
        const taskArn = one(listing.taskArns, 'Expected one running task')
        const tasks = aws(['ecs', 'describe-tasks', '--cluster', policy.cluster, '--tasks', taskArn])
        assert.deepEqual(tasks.failures, [])
        const task = one(tasks.tasks, 'Expected one task')
        policy.verifyRunningTask(task, input.service, newRevision, release, applicationGraph, routerGraph)
        const logs = aws(['logs', 'filter-log-events', '--log-group-name', policy.logGroup, '--log-stream-names', policy.logStream(taskArn), '--start-time', String(evidence.updateRequestedAt)])
        const progress = policy.progressEvidence(logs.events, input.service, now())
        if (progress) { completed = { taskArn, taskDefinition: newRevision, progress }; break }
      }
      await pause(15_000)
    }
    assert.ok(completed, 'Recovery timed out without stable worker progress')
    evidence.outcome = 'recovered'
    evidence.completed = completed
    persist()
    return completed
  } catch (error) {
    evidence.outcome = 'failed-inspect-required'
    evidence.failureClass = error?.code === 'ERR_ASSERTION' ? 'POLICY_CHECK_FAILED' : 'OPERATION_FAILED'
    persist()
    throw error
  } finally { fs.rmSync(taskFile, { force: true }) }
}
