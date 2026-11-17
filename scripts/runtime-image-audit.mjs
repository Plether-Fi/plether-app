#!/usr/bin/env node
// Read-only dependency inventory. A successful report is scoped evidence, never
// permission to delete an image: cleanup needs its own complete reference gate.
import { execFile } from 'node:child_process'
import { createHash } from 'node:crypto'
import fs from 'node:fs/promises'
import path from 'node:path'
import { promisify } from 'node:util'
import { pathToFileURL } from 'node:url'

const execFileAsync = promisify(execFile)
const DIGEST = /^sha256:[a-f0-9]{64}$/
const REQUIRED_ARRAYS = ['clusters', 'repositories', 'taskDefinitionFamilies', 'rollbackTaskDefinitions', 'eventBuses', 'schedulerGroups']
const MAX_MANIFESTS = 10_000

function requireValue(condition, code = 'INVALID_RESPONSE') {
  if (!condition) throw Object.assign(new Error(code), { code })
}
function array(value) {
  requireValue(Array.isArray(value))
  return value
}
function nonempty(value) { return typeof value === 'string' && value.length > 0 && value.length <= 2048 }
function unique(values) { return [...new Set(values)] }
function taskFamily(arn) { return /^arn:[^:]+:ecs:[^:]+:[0-9]{12}:task-definition\/([^:]+):([0-9]+)$/.exec(arn) }

export function validateAuditConfig(config) {
  requireValue(config?.version === 1 && /^[0-9]{12}$/.test(config.accountId) && /^[a-z]{2}(?:-[a-z]+)+-[0-9]+$/.test(config.region), 'INVALID_CONFIG')
  for (const key of REQUIRED_ARRAYS) {
    requireValue(Array.isArray(config[key]) && config[key].every(nonempty) && unique(config[key]).length === config[key].length, 'INVALID_CONFIG')
  }
  requireValue(config.clusters.length > 0 && config.repositories.length > 0, 'INVALID_CONFIG')
  requireValue(config.repositories.every(value => /^[a-z0-9][a-z0-9._/-]*$/.test(value)), 'INVALID_CONFIG')
  for (const excluded of config.excludedImages ?? []) requireValue(nonempty(excluded.image) && nonempty(excluded.reason), 'INVALID_CONFIG')
  for (const extra of config.extraImageReferences ?? []) requireValue(nonempty(extra.image) && nonempty(extra.source), 'INVALID_CONFIG')
  return config
}

export async function loadAuditConfig(filename) {
  const config = validateAuditConfig(JSON.parse(await fs.readFile(filename, 'utf8')))
  if (config.rollbackCatalog) {
    const catalog = JSON.parse(await fs.readFile(path.resolve(path.dirname(filename), config.rollbackCatalog), 'utf8'))
    requireValue(catalog.schemaVersion === 1 && catalog.accountId === config.accountId && catalog.region === config.region && Array.isArray(catalog.releases), 'INVALID_ROLLBACK_CATALOG')
    config.extraImageReferences = [...(config.extraImageReferences ?? [])]
    for (const release of catalog.releases) {
      requireValue(nonempty(release.id) && nonempty(release.applicationImage) && nonempty(release.routerImage), 'INVALID_ROLLBACK_CATALOG')
      for (const key of ['applicationImage', 'routerImage']) {
        // An approved rollback must not depend on a movable tag.
        requireValue(DIGEST.test(release[key].split('@')[1] ?? ''), 'INVALID_ROLLBACK_CATALOG')
        config.extraImageReferences.push({ image: release[key], source: `rollback:${release.id}:${key}` })
      }
    }
  }
  return validateAuditConfig(config)
}

export function createAwsRunner({ region, profile } = {}) {
  return async (service, operation, args = []) => {
    const command = [...(profile ? ['--profile', profile] : []), '--region', region, service, operation, ...args, '--output', 'json', '--no-cli-pager', '--no-paginate']
    try {
      const { stdout } = await execFileAsync('aws', command, { maxBuffer: 32 * 1024 * 1024, env: { ...process.env, AWS_PAGER: '' } })
      try { return JSON.parse(stdout) } catch { throw Object.assign(new Error('AWS_INVALID_JSON'), { code: 'AWS_INVALID_JSON' }) }
    } catch (error) {
      // Never forward raw AWS output, endpoints, environment or task definitions.
      const awsCode = /An error occurred \(([A-Za-z0-9]+)\)/.exec(error.stderr ?? '')?.[1]
      throw Object.assign(new Error('AWS request failed'), { code: awsCode ?? (error.code === 'AWS_INVALID_JSON' ? error.code : 'AWS_COMMAND_FAILED') })
    }
  }
}

export async function auditRuntimeImages(input, { aws, now = () => new Date().toISOString() } = {}) {
  const config = validateAuditConfig(input)
  requireValue(typeof aws === 'function', 'AWS_RUNNER_REQUIRED')
  const report = {
    version: 1, observedAt: now(), accountId: config.accountId, region: config.region,
    complete: false, healthy: false, readOnly: true,
    checks: { registryManifests: 'availability-and-sha256-integrity', layerPullability: 'not-tested', cleanupAuthorization: false },
    scope: { clusters: config.clusters, repositories: config.repositories, taskDefinitionFamilies: config.taskDefinitionFamilies,
      rollbackTaskDefinitions: config.rollbackTaskDefinitions, scheduledTargets: 'all discovered EventBridge buses and Scheduler groups' },
    coverage: { services: 0, liveTasks: 0, eventRules: 0, schedulerSchedules: 0, taskDefinitions: 0 },
    roots: [], images: [], exclusions: [], findings: [],
  }
  let complete = true
  const taskRoots = new Map()
  const imageRoots = new Map()
  function finding(code, source, detail) { report.findings.push({ code, source, ...(detail ? { detail } : {}) }) }
  async function stage(source, fn) {
    try { return await fn() } catch (error) {
      complete = false
      const code = /^[A-Za-z0-9_]+$/.test(error.code ?? '') ? error.code : 'INVENTORY_FAILED'
      finding('INVENTORY_INCOMPLETE', source, code)
      return undefined
    }
  }
  async function pages(service, operation, args, key, tokenKey = 'nextToken') {
    const result = []; const seen = new Set(); let token
    do {
      const response = await aws(service, operation, [...args, ...(token ? ['--next-token', token] : [])])
      result.push(...array(response[key]))
      token = response[tokenKey]
      requireValue(token === undefined || token === null || nonempty(token), 'INVALID_PAGINATION')
      if (token) { requireValue(!seen.has(token), 'REPEATED_PAGINATION_TOKEN'); seen.add(token) }
    } while (token)
    return result
  }
  function root(map, reference, source) {
    requireValue(nonempty(reference))
    if (!map.has(reference)) map.set(reference, new Set())
    map.get(reference).add(source)
  }
  function addTask(reference, source) { root(taskRoots, reference, source) }
  function addImage(reference, source) { root(imageRoots, reference, source) }
  function cleanFailures(response) { requireValue(Array.isArray(response.failures) && response.failures.length === 0, 'AWS_PARTIAL_FAILURE') }
  function scheduledTask(target, source) {
    if (target.EcsParameters) addTask(target.EcsParameters.TaskDefinitionArn, source)
    else if (/:aws-sdk:ecs:/.test(target.Arn ?? '')) {
      // Universal Scheduler targets can launch ECS without EcsParameters. A
      // dynamic/unrecognized launcher is incomplete coverage, not "no roots".
      requireValue(/:aws-sdk:ecs:(?:runTask|startTask)$/.test(target.Arn), 'UNSUPPORTED_ECS_SCHEDULE_TARGET')
      const input = JSON.parse(target.Input)
      const reference = input.taskDefinition ?? input.TaskDefinition
      requireValue(nonempty(reference), 'UNRESOLVED_ECS_SCHEDULE_TARGET')
      addTask(reference, source)
    }
  }

  const identity = await stage('identity', () => aws('sts', 'get-caller-identity'))
  if (!identity || identity.Account !== config.accountId) {
    if (identity) finding('WRONG_AWS_ACCOUNT', 'identity')
    return report
  }
  for (const cluster of config.clusters) {
    await stage(`cluster:${cluster}:services`, async () => {
      const arns = unique(await pages('ecs', 'list-services', ['--cluster', cluster, '--max-results', '100'], 'serviceArns'))
      for (let offset = 0; offset < arns.length; offset += 10) {
        const batch = arns.slice(offset, offset + 10)
        const response = await aws('ecs', 'describe-services', ['--cluster', cluster, '--services', ...batch])
        cleanFailures(response)
        const services = array(response.services)
        requireValue(services.length === batch.length && services.every(s => batch.includes(s.serviceArn)) && unique(services.map(s => s.serviceArn)).length === batch.length)
        for (const service of services) {
          requireValue(nonempty(service.serviceName))
          report.coverage.services++
          addTask(service.taskDefinition, `service:${service.serviceName}:selected`)
          for (const deployment of array(service.deployments)) {
            requireValue(nonempty(deployment.id))
            addTask(deployment.taskDefinition, `service:${service.serviceName}:deployment:${deployment.id}`)
          }
        }
      }
    })
    await stage(`cluster:${cluster}:tasks`, async () => {
      // desired RUNNING includes tasks whose current state is PENDING. Include
      // desired STOPPED too, because a task can still be draining/stopping.
      const arns = unique((await pages('ecs', 'list-tasks', ['--cluster', cluster, '--desired-status', 'RUNNING', '--max-results', '100'], 'taskArns'))
        .concat(await pages('ecs', 'list-tasks', ['--cluster', cluster, '--desired-status', 'STOPPED', '--max-results', '100'], 'taskArns')))
      for (let offset = 0; offset < arns.length; offset += 100) {
        const batch = arns.slice(offset, offset + 100)
        const response = await aws('ecs', 'describe-tasks', ['--cluster', cluster, '--tasks', ...batch])
        cleanFailures(response)
        const tasks = array(response.tasks)
        requireValue(tasks.length === batch.length && tasks.every(t => batch.includes(t.taskArn)) && unique(tasks.map(t => t.taskArn)).length === batch.length)
        for (const task of tasks) {
          if (task.lastStatus === 'STOPPED') continue
          report.coverage.liveTasks++
          addTask(task.taskDefinitionArn, `task:${task.taskArn}`)
          for (const container of array(task.containers)) {
            if (!container.imageDigest) continue // Pending pulls are covered by their task definition.
            requireValue(DIGEST.test(container.imageDigest))
            const repository = container.image?.split('@')[0].replace(/:[^/:]+$/, '')
            requireValue(nonempty(repository))
            addImage(`${repository}@${container.imageDigest}`, `task:${task.taskArn}:container:${container.name}`)
          }
        }
      }
    })
  }
  for (const family of config.taskDefinitionFamilies) {
    await stage(`family:${family}`, async () => {
      const arns = await pages('ecs', 'list-task-definitions', ['--family-prefix', family, '--status', 'ACTIVE', '--sort', 'DESC', '--max-results', '100'], 'taskDefinitionArns')
      const candidates = arns.map(arn => ({ arn, match: taskFamily(arn) })).filter(v => v.match?.[1] === family)
        .sort((a, b) => Number(b.match[2]) - Number(a.match[2]))
      requireValue(candidates.length > 0, 'TASK_FAMILY_MISSING')
      addTask(candidates[0].arn, `family:${family}:latest-active`)
    })
  }
  for (const arn of config.rollbackTaskDefinitions) addTask(arn, 'approved-rollback-task-definition')
  for (const extra of config.extraImageReferences ?? []) addImage(extra.image, extra.source)

  const buses = await stage('eventbridge:buses', () => pages('events', 'list-event-buses', ['--limit', '100'], 'EventBuses', 'NextToken'))
  if (buses) for (const bus of unique(config.eventBuses.concat(buses.map(v => v.Name)))) {
    await stage(`eventbridge:${bus}`, async () => {
      const rules = await pages('events', 'list-rules', ['--event-bus-name', bus, '--limit', '100'], 'Rules', 'NextToken')
      for (const rule of rules) {
        requireValue(nonempty(rule.Name)); report.coverage.eventRules++
        const targets = await pages('events', 'list-targets-by-rule', ['--event-bus-name', bus, '--rule', rule.Name, '--limit', '100'], 'Targets', 'NextToken')
        for (const target of targets) scheduledTask(target, `eventbridge:${bus}/${rule.Name}:${target.Id}`)
      }
    })
  }
  const groups = await stage('scheduler:groups', () => pages('scheduler', 'list-schedule-groups', ['--max-results', '100'], 'ScheduleGroups', 'NextToken'))
  if (groups) for (const group of unique(config.schedulerGroups.concat(groups.map(v => v.Name)))) {
    await stage(`scheduler:${group}`, async () => {
      const schedules = await pages('scheduler', 'list-schedules', ['--group-name', group, '--max-results', '100'], 'Schedules', 'NextToken')
      for (const schedule of schedules) {
        requireValue(nonempty(schedule.Name)); report.coverage.schedulerSchedules++
        const detail = await aws('scheduler', 'get-schedule', ['--group-name', group, '--name', schedule.Name])
        requireValue(detail.Target && typeof detail.Target === 'object')
        scheduledTask(detail.Target, `scheduler:${group}/${schedule.Name}`)
      }
    })
  }

  for (const [reference, sources] of taskRoots) {
    report.roots.push({ kind: 'task-definition', reference, sources: [...sources].sort() })
    await stage(`task-definition:${reference}`, async () => {
      const response = await aws('ecs', 'describe-task-definition', ['--task-definition', reference])
      const task = response.taskDefinition
      requireValue(nonempty(task?.taskDefinitionArn) && array(task.containerDefinitions).length > 0)
      requireValue(taskFamily(task.taskDefinitionArn) && task.taskDefinitionArn.split(':')[3] === config.region && task.taskDefinitionArn.split(':')[4] === config.accountId, 'TASK_DEFINITION_WRONG_BOUNDARY')
      requireValue(reference.startsWith('arn:') ? task.taskDefinitionArn === reference : task.taskDefinitionArn.endsWith(`/${reference}`), 'TASK_DEFINITION_IDENTITY_MISMATCH')
      report.coverage.taskDefinitions++
      for (const container of task.containerDefinitions) {
        requireValue(nonempty(container.name))
        for (const source of sources) addImage(container.image, `${source}:container:${container.name}`)
      }
    })
  }

  const manifestNodes = new Map()
  async function manifest(repository, digest, sources, ancestors = new Set()) {
    const reference = `${repository}@${digest}`
    requireValue(!ancestors.has(reference), 'MANIFEST_CYCLE')
    if (manifestNodes.has(reference)) {
      const node = manifestNodes.get(reference)
      node.sources = unique(node.sources.concat(sources)).sort()
      for (const child of node.children) await manifest(repository, child, sources, new Set([...ancestors, reference]))
      return
    }
    requireValue(manifestNodes.size < MAX_MANIFESTS && ancestors.size < 8, 'MANIFEST_GRAPH_LIMIT')
    const response = await aws('ecr', 'batch-get-image', ['--repository-name', repository, '--image-ids', `imageDigest=${digest}`])
    requireValue(!response.failures?.some(v => v.failureCode === 'ImageNotFound'), 'ECR_IMAGE_NOT_FOUND')
    cleanFailures(response)
    const images = array(response.images)
    requireValue(images.length > 0 && typeof images[0]?.imageManifest === 'string' && images[0].imageManifest.length > 0, 'MANIFEST_MISSING')
    const raw = images[0].imageManifest
    // ECR can return one entry per tag for a digest lookup. Those aliases are
    // equivalent only when every entry has the requested digest and identical
    // raw bytes; accepting an arbitrary first entry would conceal conflicts.
    requireValue(images.every(image => image?.imageId?.imageDigest === digest && image.imageManifest === raw), 'MANIFEST_RESPONSE_CONFLICT')
    requireValue(`sha256:${createHash('sha256').update(raw).digest('hex')}` === digest, 'MANIFEST_DIGEST_MISMATCH')
    const decoded = JSON.parse(raw)
    requireValue(decoded.schemaVersion === 2, 'UNSUPPORTED_MANIFEST')
    const node = { repository, digest, mediaType: decoded.mediaType, sources: [...sources].sort(), children: [] }
    manifestNodes.set(reference, node)
    if (decoded.manifests) {
      requireValue(Array.isArray(decoded.manifests) && decoded.manifests.length > 0, 'INVALID_MANIFEST_INDEX')
      for (const child of decoded.manifests) {
        requireValue(DIGEST.test(child.digest), 'INVALID_MANIFEST_DESCRIPTOR')
        node.children.push(child.digest)
        await manifest(repository, child.digest, sources, new Set([...ancestors, reference]))
      }
    } else {
      requireValue(DIGEST.test(decoded.config?.digest) && Array.isArray(decoded.layers) && decoded.layers.every(v => DIGEST.test(v.digest)), 'INVALID_IMAGE_MANIFEST')
    }
  }
  for (const [reference, sources] of imageRoots) {
    report.roots.push({ kind: 'image', reference, sources: [...sources].sort() })
    const exclusion = (config.excludedImages ?? []).find(v => v.image === reference)
    if (exclusion) { report.exclusions.push({ image: reference, reason: exclusion.reason, sources: [...sources].sort() }); continue }
    await stage(`image:${reference}`, async () => {
      const match = /^([0-9]{12})\.dkr\.ecr\.([a-z0-9-]+)\.amazonaws\.com(?:\.cn)?\/(.+)$/.exec(reference)
      requireValue(match && match[1] === config.accountId && match[2] === config.region, 'UNREVIEWED_IMAGE_REGISTRY')
      const [named, pinned, ...extra] = match[3].split('@'); requireValue(extra.length === 0)
      const tagMatch = /^(.*):([^/:]+)$/.exec(named)
      const repository = tagMatch?.[1] ?? named
      requireValue(config.repositories.includes(repository), 'UNREVIEWED_IMAGE_REPOSITORY')
      let digest = pinned
      if (digest) requireValue(DIGEST.test(digest), 'INVALID_IMAGE_DIGEST')
      else {
        requireValue(tagMatch && /^[A-Za-z0-9_][A-Za-z0-9_.-]{0,127}$/.test(tagMatch[2]), 'IMAGE_REFERENCE_UNRESOLVED')
        const response = await aws('ecr', 'describe-images', ['--repository-name', repository, '--image-ids', `imageTag=${tagMatch[2]}`])
        const details = array(response.imageDetails)
        requireValue(details.length === 1 && DIGEST.test(details[0].imageDigest), 'IMAGE_TAG_MISSING')
        digest = details[0].imageDigest
      }
      await manifest(repository, digest, [...sources])
    })
  }
  report.images = [...manifestNodes.values()].sort((a, b) => `${a.repository}@${a.digest}`.localeCompare(`${b.repository}@${b.digest}`))
  report.roots.sort((a, b) => `${a.kind}:${a.reference}`.localeCompare(`${b.kind}:${b.reference}`))
  report.complete = complete
  report.healthy = complete && report.findings.length === 0
  return report
}

async function main(args) {
  const options = {}
  for (let index = 0; index < args.length; index += 2) {
    requireValue(['--config', '--profile', '--output'].includes(args[index]) && nonempty(args[index + 1]), 'USAGE: --config FILE [--profile PROFILE] [--output FILE]')
    requireValue(!options[args[index]], 'DUPLICATE_OPTION')
    options[args[index]] = args[index + 1]
  }
  requireValue(nonempty(options['--config']), 'CONFIG_REQUIRED')
  const config = await loadAuditConfig(options['--config'])
  const report = await auditRuntimeImages(config, { aws: createAwsRunner({ region: config.region, profile: options['--profile'] }) })
  const output = `${JSON.stringify(report, null, 2)}\n`
  if (options['--output']) await fs.writeFile(options['--output'], output)
  else process.stdout.write(output)
  if (!report.healthy) process.exitCode = 1
}
if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  main(process.argv.slice(2)).catch(error => {
    process.stderr.write(`Runtime image audit failed: ${/^[A-Z_0-9 :[\]-]+$/.test(error.code ?? '') ? error.code : 'INVENTORY_INCOMPLETE'}\n`)
    process.exitCode = 1
  })
}
