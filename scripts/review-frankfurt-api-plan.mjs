#!/usr/bin/env node
// Strict incremental plan review. Never emits secrets or applies a plan.
import assert from 'node:assert/strict'
import { execFileSync } from 'node:child_process'
import { readFileSync, statSync } from 'node:fs'
import { createHash } from 'node:crypto'
import { resolve, join } from 'node:path'
import { fileURLToPath } from 'node:url'

const registry = '932542905614.dkr.ecr.eu-central-1.amazonaws.com'
const images = {
  api: `${registry}/plether-api-sepolia-aa-temp@sha256:4629cc418d52f183b5fdd21a2d4409124eaf7ea4e72229bd983c1602f67bb3be`,
  log: `${registry}/plether-otel-log-router-sepolia-aa-temp@sha256:f442ecc9aff9c3fc768ba76a200690c33c4cb0ffd49b9660b09ce7738ba213cc`,
  alto: `${registry}/plether-alto-sepolia-aa-temp@sha256:9db94fbd439a26f01b0ece3cc5f76b3791c1e1660b990d74892274267096f12a`,
}
export function reviewApiPlan(plan) {
  assert.equal(plan.errored, false)
  const vars = Object.fromEntries(Object.entries(plan.variables).map(([k,v]) => [k,v.value]))
  assert.equal(vars.deployment_id, 'sepolia-aa-temp')
  assert.equal(vars.aws_region, 'eu-central-1')
  assert.equal(vars.expected_aws_account_id, '932542905614')
  assert.equal(vars.perps_chain_id, '421614')
  assert(['dormant','api-readonly'].includes(vars.frankfurt_activation_stage))
  const count = vars.frankfurt_activation_stage === 'api-readonly' ? 1 : 0
  assert.equal(vars.api_desired_count, count)
  for (const key of ['configure_native_aa_backend','enable_native_aa_sponsorship','enable_native_aa_submission',
    'aa_native_global_rollout_enabled','enable_aa_sponsorship','provision_aa_proxy','provision_insights_registration',
    'enable_insights_registration','protection_worker_execution_enabled','aa_protection_commits_enabled']) assert.equal(vars[key], false, key)
  assert.equal(vars.faucet_private_key, '')
  assert.equal(vars.lp_settlement_mode, 'off')
  const managed = plan.resource_changes.filter(c => c.mode === 'managed')
  const services = managed.filter(c => c.type === 'aws_ecs_service')
  assert.equal(services.length, 10)
  for (const c of services) assert.equal(c.change.after.desired_count, c.address === 'aws_ecs_service.api' ? count : 0, c.address)
  const changes = managed.filter(c => c.change.actions.join() !== 'no-op')
  for (const c of changes) {
    const {before,after,actions} = c.change
    if (c.type === 'aws_ecs_task_definition') {
      assert.deepEqual(actions, ['delete','create'])
      assert(before.family === 'plether-sepolia-aa-temp' || before.family.startsWith('plether-sepolia-aa-temp-'))
      const oldContainers = JSON.parse(before.container_definitions)
      const newContainers = JSON.parse(after.container_definitions)
      assert.equal(newContainers.length, oldContainers.length)
      for (const container of newContainers) {
        const old = oldContainers.find(x => x.name === container.name)
        assert(old)
        const expected = old.image.includes('/plether-api-sepolia-aa-temp:') ? images.api
          : old.image.includes('/plether-otel-log-router-sepolia-aa-temp:') ? images.log
          : old.image.includes('/plether-alto-sepolia-aa-temp:') ? images.alto : old.image
        assert.equal(container.image, expected)
        container.image = old.image
        if (container.name === 'plether-api' && !old.environment.some(e => e.name === 'PYTH_INGESTION_ENABLED')) {
          assert.deepEqual(container.environment.filter(e => e.name === 'PYTH_INGESTION_ENABLED'), [{name:'PYTH_INGESTION_ENABLED',value:'false'}])
          container.environment = container.environment.filter(e => e.name !== 'PYTH_INGESTION_ENABLED')
        }
        // ECS materializes these absent optional lists as empty arrays.
        for (const key of ['mountPoints','portMappings','systemControls','volumesFrom']) {
          container[key] ??= []
          old[key] ??= []
        }
        assert.deepEqual(container, old, `Unexpected container change: ${c.address}/${container.name}`)
      }
      const normalize = value => {
        const copy = {...value}
        for (const key of ['arn','arn_without_revision','id','revision','tags','container_definitions']) delete copy[key]
        for (const key of ['ipc_mode','pid_mode']) copy[key] ??= ''
        copy.enable_fault_injection ??= false
        copy.volume = copy.volume.map(v => ({...v, configure_at_launch:v.configure_at_launch ?? false}))
        return copy
      }
      assert.deepEqual(normalize(after), normalize(before), c.address)
    } else if (c.address === 'terraform_data.perps_candle_rollout_guard') {
      assert.deepEqual(actions, ['update'])
      assert.equal(before.input.api_desired_count,0)
      assert.equal(after.input.api_desired_count,1)
      assert.deepEqual({...after.input,api_desired_count:0},before.input)
      assert.equal(after.id,before.id)
      assert.deepEqual(after.triggers_replace,before.triggers_replace)
    } else {
      assert.equal(c.address, 'aws_ecs_service.api', `Unexpected mutation: ${c.address}`)
      assert.deepEqual(actions, ['update'])
      assert.equal(before.desired_count, 0)
      assert.equal(after.desired_count, 1)
      assert.deepEqual({...after, desired_count:0}, before, 'Only API desired count may change')
    }
  }
  return {stage:vars.frankfurt_activation_stage, changes:changes.map(c=>({address:c.address,actions:c.change.actions})), serviceCounts:services.map(c=>({name:c.change.after.name,desired:c.change.after.desired_count}))}
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  assert.equal(process.argv.length,3)
  const staging=resolve(process.argv[2]), directory=join(staging,'infra/terraform'), file=join(directory,'frankfurt.tfplan')
  assert.equal(statSync(file).mode & 0o077,0)
  const plan=JSON.parse(execFileSync(process.env.TERRAFORM_BIN || 'terraform',['show','-json',file],{
    cwd:directory,env:{...process.env,AWS_PROFILE:'plether',TF_DATA_DIR:join(staging,'terraform-data')},encoding:'utf8',maxBuffer:32*1024*1024,
  }))
  try {
    console.log(JSON.stringify({...reviewApiPlan(plan),planSha256:createHash('sha256').update(readFileSync(file)).digest('hex')},null,2))
  } catch {
    console.error('Plan rejected: unexpected incremental changes. Inspect the private plan locally; do not publish raw values.')
    process.exitCode = 1
  }
}
