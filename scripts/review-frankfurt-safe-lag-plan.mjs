#!/usr/bin/env node
// Read-only review of the exact two-definition safe-lag rollout.
import assert from 'node:assert/strict'
import { execFileSync } from 'node:child_process'
import { readFileSync } from 'node:fs'
import { createHash } from 'node:crypto'
import { validateImageRecord } from './aa-frankfurt-images.mjs'

try {
  const [cwd, recordPath] = process.argv.slice(2)
  assert(cwd && recordPath && process.argv.length === 4)
  const record = validateImageRecord(JSON.parse(readFileSync(recordPath)))
  const image = record.images.find(x => x.name === 'api').reference
  const plan = JSON.parse(execFileSync(process.env.TERRAFORM_BIN || 'terraform',
    ['show', '-json', 'frankfurt.tfplan'], { cwd, encoding: 'utf8', maxBuffer: 64 * 1024 * 1024 }))
  const vars = Object.fromEntries(Object.entries(plan.variables).map(([k, v]) => [k, v.value]))
  assert.equal(vars.deployment_id, 'sepolia-aa-temp')
  assert.equal(vars.aws_region, 'eu-central-1')
  assert.equal(vars.aa_reconciler_max_safe_lag_seconds, '1800')
  for (const name of ['enable_native_aa_sponsorship', 'enable_native_aa_submission', 'aa_native_global_rollout_enabled'])
    assert.equal(String(vars[name]), 'false')
  const managed = plan.resource_changes.filter(x => x.mode === 'managed')
  const services = managed.filter(x => x.type === 'aws_ecs_service')
  assert.equal(services.length, 10)
  for (const service of services) {
    assert.deepEqual(service.change.actions, ['no-op'])
    assert.equal(service.change.after.desired_count,
      ['aws_ecs_service.api', 'aws_ecs_service.alto[0]', 'aws_ecs_service.aa_reconciler[0]'].includes(service.address) ? 1 : 0)
  }
  const changes = managed.filter(x => x.change.actions.join() !== 'no-op')
  assert.equal(changes.length, 2)
  for (const resource of changes) {
    assert(['aws_ecs_task_definition.api', 'aws_ecs_task_definition.aa_reconciler[0]'].includes(resource.address))
    assert.deepEqual(resource.change.actions, ['delete', 'create'])
    const before = JSON.parse(resource.change.before.container_definitions)
    const after = JSON.parse(resource.change.after.container_definitions)
    assert.equal(before.length, after.length)
    let imageChanges = 0, lagChanges = 0
    for (const next of after) {
      const old = before.find(x => x.name === next.name)
      assert(old)
      if (next.image !== old.image) {
        assert(['plether-api', 'plether-aa-reconciler', 'aa-reconciler-tmp-init'].includes(next.name))
        assert.equal(next.image, image)
        next.image = old.image
        imageChanges++
      }
      const setting = next.environment?.find(x => x.name === 'AA_RECONCILER_MAX_SAFE_LAG_SECONDS')
      const previous = old.environment?.find(x => x.name === 'AA_RECONCILER_MAX_SAFE_LAG_SECONDS')
      if (setting && setting.value !== previous?.value) {
        assert(['plether-api', 'plether-aa-reconciler'].includes(next.name))
        assert.equal(setting.value, '1800')
        if (previous) { assert.equal(previous.value, '600'); setting.value = previous.value }
        else next.environment = next.environment.filter(x => x !== setting)
        lagChanges++
      }
      for (const container of [next, old]) {
        container.environment?.sort((a, b) => a.name.localeCompare(b.name))
        for (const key of ['mountPoints', 'portMappings', 'systemControls', 'volumesFrom']) container[key] ??= []
      }
      assert.deepEqual(next, old)
    }
    assert.equal(lagChanges, 1)
    assert.equal(imageChanges, resource.address === 'aws_ecs_task_definition.api' ? 1 : 2)
    const normalize = value => {
      const v = structuredClone(value)
      for (const k of ['arn', 'arn_without_revision', 'id', 'revision', 'container_definitions', 'tags']) delete v[k]
      for (const k of ['ipc_mode', 'pid_mode']) v[k] ??= ''
      v.volume = v.volume.map(x => ({ ...x, configure_at_launch: x.configure_at_launch ?? false }))
      return v
    }
    assert.deepEqual(normalize(resource.change.after), normalize(resource.change.before))
  }
  console.log(JSON.stringify({ review: 'passed', changes: changes.map(x => x.address), image,
    sha256: createHash('sha256').update(readFileSync(cwd + '/frankfurt.tfplan')).digest('hex') }))
} catch (error) {
  console.error(`Safe-lag plan rejected (${error.code ?? 'validation'}); values withheld.`)
  process.exitCode = 1
}
