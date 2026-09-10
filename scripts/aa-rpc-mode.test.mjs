import test from 'node:test'
import assert from 'node:assert/strict'
import { spawnSync } from 'node:child_process'
import { readFileSync, mkdtempSync, writeFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'

const root = new URL('../', import.meta.url)
const read = path => readFileSync(new URL(path, root), 'utf8')
const primary = 'arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/perps-rpc-url'
const secondary = 'arn:aws:ssm:ap-southeast-1:932542905614:parameter/plether/sepolia/aa-reconciler-secondary-rpc-url'
const owner = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
function fixture(mode = 'dual-independent') {
  return { containerDefinitions: [{ name: 'plether-api', environment: [
    { name: 'AA_RPC_MODE', value: mode },
    { name: 'PERPS_CHAIN_ID', value: '421614' },
    { name: 'AA_NATIVE_GLOBAL_ROLLOUT_ENABLED', value: 'false' },
    { name: 'AA_NATIVE_CANARY_OWNERS', value: owner },
  ], secrets: [
    { name: 'PERPS_RPC_URL', valueFrom: primary },
    { name: 'AA_RECONCILER_SECONDARY_RPC_URL', valueFrom: mode === 'single-provider-sepolia' ? primary : secondary },
  ] }] }
}
function validate(data) {
  const result = spawnSync('jq', ['-er', '--arg', 'container', 'plether-api', '-f',
    new URL('.github/scripts/validate-aa-rpc-mode.jq', root).pathname],
  { input: JSON.stringify(data), encoding: 'utf8' })
  assert.ifError(result.error)
  return result
}
test('deployment mode gate accepts default dual and explicit single-provider configurations', () => {
  for (const mode of ['dual-independent', 'single-provider-sepolia']) {
    const result = validate(fixture(mode))
    assert.equal(result.status, 0, result.stderr)
    assert.equal(result.stdout.trim(), mode)
  }
  const legacy = fixture()
  legacy.containerDefinitions[0].environment.shift()
  assert.equal(validate(legacy).stdout.trim(), 'dual-independent')
})
test('deployment gate rejects unsafe single-provider modes and malformed cohorts', () => {
  for (const [name, value] of [
    ['AA_RPC_MODE', 'unknown'], ['PERPS_CHAIN_ID', '1'], ['PERPS_CHAIN_ID', '42161'],
    ['AA_NATIVE_GLOBAL_ROLLOUT_ENABLED', 'true'], ['AA_NATIVE_CANARY_OWNERS', ''],
    ['AA_NATIVE_CANARY_OWNERS', '0x' + '0'.repeat(40)],
    ['AA_NATIVE_CANARY_OWNERS', `${owner},${owner.toLowerCase()}`],
  ]) {
    const data = fixture('single-provider-sepolia')
    data.containerDefinitions[0].environment.find(env => env.name === name).value = value
    assert.notEqual(validate(data).status, 0, `${name}=${value}`)
  }
})
test('deployment gate rejects accidental shared dual RPCs and non-primary single RPCs', () => {
  const dual = fixture()
  dual.containerDefinitions[0].secrets[1].valueFrom = primary
  assert.notEqual(validate(dual).status, 0)
  const single = fixture('single-provider-sepolia')
  single.containerDefinitions[0].secrets[1].valueFrom = secondary
  assert.notEqual(validate(single).status, 0)
  single.containerDefinitions[0].secrets[0].valueFrom = secondary
  assert.notEqual(validate(single).status, 0)
})
test('deployment gate rejects duplicate and missing security fields', () => {
  for (const kind of ['environment', 'secrets']) {
    const data = fixture('single-provider-sepolia')
    data.containerDefinitions[0][kind].push(data.containerDefinitions[0][kind][0])
    assert.notEqual(validate(data).status, 0)
  }
  const data = fixture('single-provider-sepolia')
  data.containerDefinitions[0].secrets.pop()
  assert.notEqual(validate(data).status, 0)
})
test('API, reconciler, Terraform and workflow use the explicit mode contract', () => {
  const config = read('apps/backend/src/Plether/Config.hs')
  const reconciler = read('apps/backend/app/AaReconciler.hs')
  for (const source of [config, reconciler]) {
    assert.match(source, /fromMaybe "dual-independent".*lookupEnv "AA_RPC_MODE"/)
    assert.match(source, /resolveAaSecurityRpc/)
  }
  for (const path of ['infra/terraform/ecs.tf', 'infra/terraform/aa_reconciler.tf']) {
    assert.match(read(path), /name = "AA_RPC_MODE", value = var\.aa_rpc_mode/)
  }
  assert.match(read('.github/workflows/deploy-backend.yml'), /-f \.github\/scripts\/validate-aa-rpc-mode\.jq/)
  assert.match(read('infra/terraform/variables.tf'), /variable "aa_rpc_mode" \{[\s\S]*?default\s*= "dual-independent"/)
})

// Exercise the actual production preconditions in a disposable module using
// only Terraform's built-in terraform_data provider: no AWS/state access.
test('Terraform mode preconditions reject unsafe plans before provisioning', t => {
  const terraform = process.env.TERRAFORM_BIN || 'terraform'
  if (spawnSync(terraform, ['version'], { encoding: 'utf8' }).status !== 0) {
    if (process.env.AA_TERRAFORM_TEST_REQUIRED === '1') assert.fail('Terraform required')
    t.skip('Terraform unavailable; run with AA_TERRAFORM_TEST_REQUIRED=1 to require this check')
    return
  }
  const production = read('infra/terraform/rollout_guards.tf')
  const guards = [...production.matchAll(/    precondition \{[\s\S]*?\n    \}/g)]
    .map(match => match[0]).filter(block => block.includes('var.aa_rpc_mode'))
  assert.equal(guards.length, 2)
  const defaults = {
    aa_rpc_mode: 'dual-independent', environment: 'sepolia', perps_chain_id: '421614',
    aa_native_global_rollout_enabled: false, aa_native_canary_owners: owner,
    aa_reconciler_secondary_rpc_url_ssm_parameter_name: '/plether/sepolia/aa-reconciler-secondary-rpc-url',
    aa_reconciler_secondary_rpc_url_kms_key_arn: '',
    alto_rpc_url_ssm_parameter_name: '/plether/sepolia/perps-rpc-url', provision_self_hosted_aa: true,
  }
  const directory = mkdtempSync(join(tmpdir(), 'plether-aa-rpc-guards-'))
  const run = args => spawnSync(terraform, args, { cwd: directory, encoding: 'utf8',
    env: { ...process.env, TF_IN_AUTOMATION: '1' } })
  try {
    const declarations = Object.entries(defaults).map(([key, value]) =>
      `variable "${key}" {\n type = ${typeof value === 'boolean' ? 'bool' : 'string'}\n default = ${JSON.stringify(value)}\n}`).join('\n')
    writeFileSync(join(directory, 'main.tf'), `${declarations}\nresource "terraform_data" "mode" {\n lifecycle {\n${guards.join('\n')}\n }\n}\n`)
    const init = run(['init', '-backend=false', '-input=false', '-no-color'])
    assert.equal(init.status, 0, init.stderr)
    const single = { aa_rpc_mode: 'single-provider-sepolia',
      aa_reconciler_secondary_rpc_url_ssm_parameter_name: '/plether/sepolia/perps-rpc-url' }
    for (const [overrides, accepted] of [
      [{}, true], [single, true], [{ ...single, provision_self_hosted_aa: false }, true],
      [{ ...single, environment: 'mainnet', provision_self_hosted_aa: false }, false],
      [{ ...single, perps_chain_id: '42161' }, false],
      [{ ...single, aa_native_global_rollout_enabled: true }, false],
      [{ ...single, aa_native_canary_owners: '' }, false],
      [{ ...single, aa_reconciler_secondary_rpc_url_kms_key_arn: 'wrong' }, false],
      [{ aa_rpc_mode: 'single-provider-sepolia' }, false],
      [{ aa_reconciler_secondary_rpc_url_ssm_parameter_name: '/plether/sepolia/perps-rpc-url' }, false],
    ]) {
      writeFileSync(join(directory, 'case.auto.tfvars.json'), JSON.stringify(overrides))
      const result = run(['plan', '-input=false', '-lock=false', '-no-color'])
      assert.equal(result.status === 0, accepted, JSON.stringify(overrides) + '\n' + result.stderr)
      if (!accepted) assert.match(result.stderr, /Resource precondition failed/)
    }
  } finally {
    rmSync(directory, { recursive: true, force: true })
  }
})
