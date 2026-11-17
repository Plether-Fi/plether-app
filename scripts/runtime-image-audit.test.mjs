import assert from 'node:assert/strict'
import { createHash } from 'node:crypto'
import fs from 'node:fs/promises'
import os from 'node:os'
import path from 'node:path'
import test from 'node:test'
import { auditRuntimeImages, loadAuditConfig, validateAuditConfig } from './runtime-image-audit.mjs'

const ACCOUNT = '932542905614'
const REGION = 'ap-southeast-1'
const registry = `${ACCOUNT}.dkr.ecr.${REGION}.amazonaws.com`
const taskArn = (family, revision = 1) => `arn:aws:ecs:${REGION}:${ACCOUNT}:task-definition/${family}:${revision}`
const serviceArn = name => `arn:aws:ecs:${REGION}:${ACCOUNT}:service/cluster/${name}`
const runningArn = name => `arn:aws:ecs:${REGION}:${ACCOUNT}:task/cluster/${name}`
const descriptor = raw => ({ digest: `sha256:${createHash('sha256').update(JSON.stringify(raw)).digest('hex')}`, raw: JSON.stringify(raw) })
const image = label => descriptor({ schemaVersion: 2, mediaType: 'application/vnd.oci.image.manifest.v1+json',
  config: { digest: `sha256:${'a'.repeat(64)}` }, layers: [{ digest: `sha256:${'b'.repeat(64)}` }], annotations: { label } })
const app = image('app'), attestation = image('attestation'), router = image('router')
const index = descriptor({ schemaVersion: 2, mediaType: 'application/vnd.oci.image.index.v1+json', manifests: [
  { digest: app.digest, platform: { architecture: 'arm64', os: 'linux' } },
  { digest: attestation.digest, platform: { architecture: 'unknown', os: 'unknown' } },
] })
const appRef = `${registry}/app@${index.digest}`
const config = () => ({ version: 1, accountId: ACCOUNT, region: REGION, clusters: ['cluster'], repositories: ['app', 'router'],
  taskDefinitionFamilies: [], rollbackTaskDefinitions: [], eventBuses: [], schedulerGroups: [] })
const task = (family, revision = 1, images = [appRef, `${registry}/router:release`]) => ({
  taskDefinitionArn: taskArn(family, revision),
  containerDefinitions: images.map((image, i) => ({ name: i ? 'router' : 'application', image })),
})
const service = (name, family = name, revision = 1) => ({ serviceArn: serviceArn(name), serviceName: name,
  desiredCount: 1, taskDefinition: taskArn(family, revision), deployments: [{ id: `deploy-${name}`, taskDefinition: taskArn(family, revision) }] })
const flag = (args, name) => args[args.indexOf(name) + 1]
const values = (args, name) => {
  const start = args.indexOf(name) + 1; const end = args.findIndex((v, index) => index >= start && v.startsWith('--'))
  return args.slice(start, end < 0 ? args.length : end)
}

function fixture() {
  const state = { services: [service('api')], tasks: [], definitions: new Map([[taskArn('api'), task('api')]]),
    manifests: new Map([[`app@${app.digest}`, app], [`app@${index.digest}`, index], [`app@${attestation.digest}`, attestation], [`router@${router.digest}`, router]]),
    calls: [], intercept: undefined, buses: [], groups: [], targets: [], schedules: [] }
  const aws = async (kind, operation, args = []) => {
    state.calls.push({ kind, operation, args })
    const respond = () => {
      if (kind === 'sts' && operation === 'get-caller-identity') return { Account: ACCOUNT }
      if (kind === 'ecs' && operation === 'list-services') return { serviceArns: state.services.map(s => s.serviceArn) }
      if (kind === 'ecs' && operation === 'describe-services') return { services: state.services.filter(s => values(args, '--services').includes(s.serviceArn)), failures: [] }
      if (kind === 'ecs' && operation === 'list-tasks') return { taskArns: state.tasks.filter(t => t.desiredStatus === flag(args, '--desired-status')).map(t => t.taskArn) }
      if (kind === 'ecs' && operation === 'describe-tasks') return { tasks: state.tasks.filter(t => values(args, '--tasks').includes(t.taskArn)), failures: [] }
      if (kind === 'ecs' && operation === 'list-task-definitions') return { taskDefinitionArns: [...state.definitions.keys()] }
      if (kind === 'ecs' && operation === 'describe-task-definition') {
        const result = state.definitions.get(flag(args, '--task-definition'))
        if (!result) throw Object.assign(new Error('private provider message'), { code: 'ClientException' })
        return { taskDefinition: result }
      }
      if (kind === 'events' && operation === 'list-event-buses') return { EventBuses: state.buses }
      if (kind === 'events' && operation === 'list-rules') return { Rules: [{ Name: 'rule', State: 'DISABLED' }] }
      if (kind === 'events' && operation === 'list-targets-by-rule') return { Targets: state.targets }
      if (kind === 'scheduler' && operation === 'list-schedule-groups') return { ScheduleGroups: state.groups }
      if (kind === 'scheduler' && operation === 'list-schedules') return { Schedules: state.schedules.map(s => ({ Name: s.Name })) }
      if (kind === 'scheduler' && operation === 'get-schedule') return state.schedules.find(s => s.Name === flag(args, '--name'))
      if (kind === 'ecr' && operation === 'describe-images') return { imageDetails: [{ imageDigest: flag(args, '--repository-name') === 'app' ? index.digest : router.digest }] }
      if (kind === 'ecr' && operation === 'batch-get-image') {
        const repository = flag(args, '--repository-name'), digest = flag(args, '--image-ids').split('=')[1]
        const value = state.manifests.get(`${repository}@${digest}`)
        return value ? { images: [{ imageId: { imageDigest: digest }, imageManifest: value.raw }], failures: [] }
          : { images: [], failures: [{ failureCode: 'ImageNotFound' }] }
      }
      throw new Error(`Unexpected fixture call ${kind} ${operation}`)
    }
    return structuredClone(state.intercept ? await state.intercept(kind, operation, args, respond) : respond())
  }
  return { state, aws }
}

test('audits all service roots including unchanged and dormant consumers and OCI children', async () => {
  const { state, aws } = fixture()
  state.services.push({ ...service('liquidation'), desiredCount: 0 })
  state.definitions.set(taskArn('liquidation'), task('liquidation'))
  const report = await auditRuntimeImages(config(), { aws })
  assert.equal(report.healthy, true)
  assert.equal(report.complete, true)
  assert.equal(report.coverage.services, 2)
  assert.equal(report.images.length, 4)
  const child = report.images.find(v => v.digest === app.digest)
  assert.ok(child.sources.includes('service:liquidation:selected:container:application'))
  assert.ok(child.sources.includes('service:api:selected:container:application'))
  assert.equal(state.calls.filter(c => c.kind === 'ecr' && c.operation === 'batch-get-image').length, 4)
  assert.equal(state.calls.some(c => /delete|put-|update|register|create/.test(c.operation)), false)
})

test('a missing index child fails the audit even when the tagged release exists', async () => {
  const { state, aws } = fixture()
  state.manifests.delete(`app@${attestation.digest}`)
  const report = await auditRuntimeImages(config(), { aws })
  assert.equal(report.healthy, false)
  assert.equal(report.complete, false)
  assert.ok(report.findings.some(v => v.source.includes(index.digest) && v.detail === 'ECR_IMAGE_NOT_FOUND'))
})

test('accepts digest lookups returned once per identical ECR tag alias', async () => {
  const { state, aws } = fixture()
  state.intercept = (kind, operation, args, next) => {
    const response = next()
    if (kind === 'ecr' && operation === 'batch-get-image' && flag(args, '--image-ids') === `imageDigest=${index.digest}`) {
      const original = response.images[0]
      response.images = [
        { ...original, imageId: { ...original.imageId, imageTag: 'latest' } },
        { ...original, imageId: { ...original.imageId, imageTag: 'ae312cc2455d20975c501f1ce4521bfaefffce7b' } },
      ]
    }
    return response
  }
  const report = await auditRuntimeImages(config(), { aws })
  assert.equal(report.healthy, true)
  assert.equal(report.images.length, 4)
  assert.ok(report.images.find(v => v.digest === index.digest).children.includes(attestation.digest))
})

for (const variant of ['conflicting-bytes', 'unexpected-digest', 'malformed-alias']) {
  test(`rejects ${variant} in a multi-entry digest response`, async () => {
    const { state, aws } = fixture()
    state.intercept = (kind, operation, args, next) => {
      const response = next()
      if (kind === 'ecr' && operation === 'batch-get-image' && flag(args, '--image-ids') === `imageDigest=${index.digest}`) {
        const original = response.images[0]
        const alias = variant === 'conflicting-bytes' ? { ...original, imageManifest: `${original.imageManifest} ` }
          : variant === 'unexpected-digest' ? { ...original, imageId: { imageDigest: app.digest } }
            : null
        response.images = [original, alias]
      }
      return response
    }
    const report = await auditRuntimeImages(config(), { aws })
    assert.equal(report.healthy, false)
    assert.equal(report.complete, false)
    assert.ok(report.findings.some(v => v.detail === 'MANIFEST_RESPONSE_CONFLICT'))
  })
}

test('a replacement digest cannot hide the image of a running task', async () => {
  const { state, aws } = fixture()
  const removedDigest = `sha256:${'d'.repeat(64)}`
  state.tasks.push({ taskArn: runningArn('worker'), desiredStatus: 'RUNNING', lastStatus: 'RUNNING', taskDefinitionArn: taskArn('api'),
    containers: [{ name: 'application', image: `${registry}/app:release`, imageDigest: removedDigest }] })
  const report = await auditRuntimeImages(config(), { aws })
  assert.equal(report.healthy, false)
  assert.ok(report.roots.some(v => v.kind === 'image' && v.reference.endsWith(removedDigest)))
})

test('includes pending, draining and deployment task definitions but ignores terminal tasks', async () => {
  const { state, aws } = fixture()
  state.services[0].deployments.push({ id: 'prior', taskDefinition: taskArn('prior') })
  for (const family of ['prior', 'pending', 'draining']) state.definitions.set(taskArn(family), task(family))
  for (const [family, desiredStatus, lastStatus] of [['pending', 'RUNNING', 'PENDING'], ['draining', 'STOPPED', 'STOPPING'], ['terminal', 'STOPPED', 'STOPPED']]) {
    state.tasks.push({ taskArn: runningArn(family), desiredStatus, lastStatus, taskDefinitionArn: taskArn(family), containers: [] })
  }
  const report = await auditRuntimeImages(config(), { aws })
  assert.equal(report.healthy, true)
  assert.equal(report.coverage.liveTasks, 2)
  assert.deepEqual(report.roots.filter(v => v.kind === 'task-definition').map(v => v.reference).sort(), ['api', 'draining', 'pending', 'prior'].map(v => taskArn(v)).sort())
})

test('follows service pagination and does not accept repeated pagination tokens', async () => {
  const { state, aws } = fixture()
  state.services.push(service('worker')); state.definitions.set(taskArn('worker'), task('worker'))
  state.intercept = (kind, operation, args, next) => operation === 'list-services'
    ? args.includes('--next-token') ? { serviceArns: [serviceArn('worker')] } : { serviceArns: [serviceArn('api')], nextToken: 'page2' }
    : next()
  assert.equal((await auditRuntimeImages(config(), { aws })).coverage.services, 2)
  state.intercept = (kind, operation, args, next) => operation === 'list-services' ? { serviceArns: [], nextToken: 'loop' } : next()
  const incomplete = await auditRuntimeImages(config(), { aws })
  assert.equal(incomplete.healthy, false)
  assert.ok(incomplete.findings.some(v => v.detail === 'REPEATED_PAGINATION_TOKEN'))
})

test('fails closed on a later page permission error and does not expose error bodies', async () => {
  const { state, aws } = fixture()
  state.intercept = (kind, operation, args, next) => {
    if (operation !== 'list-services') return next()
    if (!args.includes('--next-token')) return { serviceArns: [serviceArn('api')], nextToken: 'page2' }
    throw Object.assign(new Error('https://provider/SECRET?token=SECRET'), { code: 'AccessDeniedException' })
  }
  const report = await auditRuntimeImages(config(), { aws })
  assert.equal(report.complete, false)
  assert.ok(report.findings.some(v => v.detail === 'AccessDeniedException'))
  assert.equal(JSON.stringify(report).includes('SECRET'), false)
})

test('partial describe responses and wrong task definition identities are incomplete', async () => {
  const { state, aws } = fixture()
  state.intercept = (kind, operation, args, next) => operation === 'describe-services' ? { services: [], failures: [] } : next()
  assert.equal((await auditRuntimeImages(config(), { aws })).complete, false)
  state.intercept = (kind, operation, args, next) => operation === 'describe-task-definition' ? { taskDefinition: task('different') } : next()
  const report = await auditRuntimeImages(config(), { aws })
  assert.ok(report.findings.some(v => v.detail === 'TASK_DEFINITION_IDENTITY_MISMATCH'))
})

test('discovers disabled scheduled ECS targets and latest exact admin family', async () => {
  const { state, aws } = fixture(); const input = config()
  input.taskDefinitionFamilies = ['admin']
  for (const [family, revision] of [['admin', 1], ['admin', 3], ['admin-similar-prefix', 99], ['rule-task', 1], ['schedule-task', 1]]) {
    state.definitions.set(taskArn(family, revision), task(family, revision))
  }
  state.buses.push({ Name: 'new-bus' }); state.targets.push({ Id: 'target', EcsParameters: { TaskDefinitionArn: taskArn('rule-task') } })
  state.groups.push({ Name: 'new-group' }); state.schedules.push({ Name: 'nightly', State: 'DISABLED', Target: { EcsParameters: { TaskDefinitionArn: taskArn('schedule-task') } } })
  const report = await auditRuntimeImages(input, { aws })
  assert.equal(report.healthy, true)
  const roots = report.roots.filter(v => v.kind === 'task-definition').map(v => v.reference)
  assert.ok(roots.includes(taskArn('admin', 3)))
  assert.equal(roots.includes(taskArn('admin', 1)), false)
  assert.equal(roots.includes(taskArn('admin-similar-prefix', 99)), false)
  assert.ok(roots.includes(taskArn('rule-task')) && roots.includes(taskArn('schedule-task')))
})

test('wrong account, unmanaged repositories and undeclared external images fail closed', async () => {
  const { state, aws } = fixture()
  state.intercept = (kind, operation, args, next) => operation === 'get-caller-identity' ? { Account: '111111111111' } : next()
  assert.equal((await auditRuntimeImages(config(), { aws })).findings[0].code, 'WRONG_AWS_ACCOUNT')
  state.intercept = undefined
  state.definitions.set(taskArn('api'), task('api', 1, ['nginx:latest']))
  const rejected = await auditRuntimeImages(config(), { aws })
  assert.ok(rejected.findings.some(v => v.detail === 'UNREVIEWED_IMAGE_REGISTRY'))
  const input = config(); input.excludedImages = [{ image: 'nginx:latest', reason: 'Fixture-only explicit exclusion' }]
  const excluded = await auditRuntimeImages(input, { aws })
  assert.equal(excluded.healthy, true)
  assert.equal(excluded.exclusions[0].reason, 'Fixture-only explicit exclusion')
})

test('universal ECS scheduled tasks are roots and unknown launchers fail closed', async () => {
  const { state, aws } = fixture()
  state.groups.push({ Name: 'default' }); state.definitions.set(taskArn('universal'), task('universal'))
  state.schedules.push({ Name: 'universal', Target: { Arn: 'arn:aws:scheduler:::aws-sdk:ecs:runTask', Input: JSON.stringify({ TaskDefinition: taskArn('universal') }) } })
  const report = await auditRuntimeImages(config(), { aws })
  assert.equal(report.healthy, true)
  assert.ok(report.roots.some(v => v.kind === 'task-definition' && v.reference === taskArn('universal')))
  state.schedules[0].Target.Arn = 'arn:aws:scheduler:::aws-sdk:ecs:createService'
  const unsupported = await auditRuntimeImages(config(), { aws })
  assert.equal(unsupported.complete, false)
  assert.ok(unsupported.findings.some(v => v.detail === 'UNSUPPORTED_ECS_SCHEDULE_TARGET'))
})

test('paginates discovered buses and Scheduler groups using their capitalized tokens', async () => {
  const { state, aws } = fixture()
  state.intercept = (kind, operation, args, next) => {
    if (operation === 'list-event-buses') return args.includes('--next-token')
      ? { EventBuses: [{ Name: 'second-page-bus' }] } : { EventBuses: [], NextToken: 'bus-page-2' }
    if (operation === 'list-schedule-groups') return args.includes('--next-token')
      ? { ScheduleGroups: [{ Name: 'second-page-group' }] } : { ScheduleGroups: [], NextToken: 'group-page-2' }
    return next()
  }
  const report = await auditRuntimeImages(config(), { aws })
  assert.equal(report.healthy, true)
  assert.ok(state.calls.some(c => c.operation === 'list-rules' && c.args.includes('second-page-bus')))
  assert.ok(state.calls.some(c => c.operation === 'list-schedules' && c.args.includes('second-page-group')))
})

test('registry manifest byte mismatch is detected', async () => {
  const { state, aws } = fixture()
  state.manifests.set(`app@${index.digest}`, { ...index, raw: `${index.raw} ` })
  const report = await auditRuntimeImages(config(), { aws })
  assert.ok(report.findings.some(v => v.detail === 'MANIFEST_DIGEST_MISMATCH'))
})

test('loads approved rollback images as pinned roots and rejects wrong catalog scope', async t => {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), 'image-audit-test-')); t.after(() => fs.rm(directory, { recursive: true, force: true }))
  const input = { ...config(), rollbackCatalog: 'catalog.json' }
  const catalog = { schemaVersion: 1, accountId: ACCOUNT, region: REGION, releases: [{ id: 'qualified', applicationImage: appRef, routerImage: `${registry}/router@${router.digest}` }] }
  await fs.writeFile(path.join(directory, 'config.json'), JSON.stringify(input)); await fs.writeFile(path.join(directory, 'catalog.json'), JSON.stringify(catalog))
  const loaded = await loadAuditConfig(path.join(directory, 'config.json')); const { aws } = fixture()
  const report = await auditRuntimeImages(loaded, { aws })
  assert.equal(report.healthy, true)
  assert.ok(report.roots.some(v => v.sources.includes('rollback:qualified:applicationImage')))
  catalog.accountId = '111111111111'; await fs.writeFile(path.join(directory, 'catalog.json'), JSON.stringify(catalog))
  await assert.rejects(loadAuditConfig(path.join(directory, 'config.json')), /INVALID_ROLLBACK_CATALOG/)
  assert.throws(() => validateAuditConfig({ ...config(), schedulerGroups: undefined }), /INVALID_CONFIG/)
})
