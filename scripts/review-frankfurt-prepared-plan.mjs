import { execFileSync } from 'node:child_process'
import assert from 'node:assert/strict'
import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
const root = process.argv[2], cwd = root + '/infra/terraform'
const env = { ...process.env, TF_DATA_DIR: root + '/terraform-data', AWS_PROFILE: 'plether' }
try {
  const p = JSON.parse(execFileSync(process.env.TERRAFORM_BIN || 'terraform', ['show', '-json', 'frankfurt.tfplan'], { cwd, env, maxBuffer: 32 * 1024 * 1024 }))
  const vars = Object.fromEntries(Object.entries(p.variables).map(([k, v]) => [k, v.value]))
  assert.equal(vars.frankfurt_activation_stage, 'aa-prepared')
  assert.equal(vars.deployment_id, 'sepolia-aa-temp')
  assert.equal(vars.aws_region, 'eu-central-1')
  assert.equal(vars.expected_aws_account_id, '932542905614')
  for (const k of ['configure_native_aa_backend', 'enable_native_aa_sponsorship', 'enable_native_aa_submission', 'aa_native_global_rollout_enabled']) assert.equal(String(vars[k]), 'false')
  const values = {
    AA_PAYMASTER_ADDRESS: vars.aa_paymaster_address,
    AA_PAYMASTER_SIGNER_ADDRESS: vars.aa_paymaster_signer_address,
    AA_PAYMASTER_CODE_HASH: vars.aa_paymaster_code_hash,
    AA_RECONCILER_START_BLOCK: vars.aa_reconciler_start_block,
    AA_RECONCILER_START_BLOCK_HASH: vars.aa_reconciler_start_block_hash,
    ALTO_ENTRYPOINT_SIMULATION_CONTRACT_V8: vars.alto_entrypoint_simulation_contract_v8,
    ALTO_PIMLICO_SIMULATION_CONTRACT: vars.alto_pimlico_simulation_contract,
  }
  const managed = p.resource_changes.filter(c => c.mode === 'managed')
  const services = managed.filter(c => c.type === 'aws_ecs_service')
  assert.equal(services.length, 10)
  for (const s of services) {
    assert.equal(s.change.after.desired_count, s.address === 'aws_ecs_service.api' ? 1 : 0)
    assert.deepEqual(s.change.actions, ['no-op'])
  }
  const changes = managed.filter(c => c.change.actions.join() !== 'no-op')
  assert.equal(changes.length, 4)
  for (const c of changes) {
    if (c.address === 'terraform_data.self_hosted_aa_guard') {
      assert.deepEqual(c.change.actions, ['update'])
      const before = c.change.before.input, after = c.change.after.input
      for (const key of Object.keys(after)) {
        if (JSON.stringify(before[key]) !== JSON.stringify(after[key])) {
          assert(['aa_paymaster_address', 'aa_paymaster_signer_address', 'aa_paymaster_code_hash', 'aa_reconciler_start_block', 'aa_reconciler_start_block_hash'].includes(key))
          assert.equal(after[key], vars[key])
        }
      }
      continue
    }
    assert(['aws_ecs_task_definition.aa_admin_kms_attest[0]', 'aws_ecs_task_definition.aa_reconciler[0]', 'aws_ecs_task_definition.alto[0]'].includes(c.address))
    assert.deepEqual(c.change.actions, ['delete', 'create'])
    const before = JSON.parse(c.change.before.container_definitions), after = JSON.parse(c.change.after.container_definitions)
    assert.equal(before.length, after.length)
    const diffs = []
    for (const n of after) {
      const o = before.find(x => x.name === n.name); assert(o)
      for (const e of n.environment ?? []) {
        const old = o.environment?.find(x => x.name === e.name)
        if (e.value !== old?.value) {
          assert.equal(e.value, values[e.name], e.name)
          diffs.push(e.name); e.value = old.value
        }
      }
      n.environment?.sort((a, b) => a.name.localeCompare(b.name))
      o.environment?.sort((a, b) => a.name.localeCompare(b.name))
      for (const key of ['mountPoints', 'portMappings', 'systemControls', 'volumesFrom']) { n[key] ??= []; o[key] ??= [] }
      assert.deepEqual(n, o)
    }
    const norm = value => {
      const v = structuredClone(value)
      for (const k of ['arn', 'arn_without_revision', 'id', 'revision', 'container_definitions', 'tags']) delete v[k]
      for (const k of ['ipc_mode', 'pid_mode']) v[k] ??= ''
      v.volume = v.volume.map(x => ({ ...x, configure_at_launch: x.configure_at_launch ?? false }))
      return v
    }
    assert.deepEqual(norm(c.change.after), norm(c.change.before))
    console.log(JSON.stringify({ address: c.address, approvedEnvironmentChanges: diffs }))
  }
  console.log(JSON.stringify({ review: 'passed', services: 'unchanged; API 1, all others 0', sha256: createHash('sha256').update(readFileSync(cwd + '/frankfurt.tfplan')).digest('hex') }))
} catch (error) {
  console.error(`Prepared plan rejected (${error.code ?? 'validation'}); values withheld. Inspect locally.`)
  process.exitCode = 1
}
