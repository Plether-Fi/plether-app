import test from 'node:test'
import assert from 'node:assert/strict'
import { spawnSync } from 'node:child_process'
import { readFileSync, mkdtempSync, mkdirSync, writeFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'

const root = new URL('../', import.meta.url)
const read = path => readFileSync(new URL(path, root), 'utf8')
const policy = JSON.parse(read('config/rpc-log-range-policy.json'))

test('the reviewed range contract fixes inclusive semantics and a smaller default catch-up batch', () => {
  assert.deepEqual(policy, { version: 1, maxLogRangeBlocks: 5000, reconcilerDefaultBatchBlocks: 1000 })
  const variables = read('infra/terraform/variables.tf')
  const declaration = variables.match(/variable "aa_reconciler_batch_blocks" \{[\s\S]*?\n\}/)?.[0]
  assert.ok(declaration)
  assert.match(declaration, new RegExp(`default\\s*= "${policy.reconcilerDefaultBatchBlocks}"`))
  assert.match(read('infra/rpc/gateway/Dockerfile'), /COPY[^\n]*config\/rpc-log-range-policy\.json \/config\/rpc-log-range-policy\.json/)
})

// Execute the production resource preconditions with only Terraform's built-in
// provider. No AWS provider, credentials, remote state or live mutation is used.
test('Terraform rejects unsupported reconciler ranges, including stopped-service configurations', t => {
  const terraform = process.env.TERRAFORM_BIN || 'terraform'
  if (spawnSync(terraform, ['version'], { encoding: 'utf8' }).status !== 0) {
    if (process.env.AA_TERRAFORM_TEST_REQUIRED === '1') assert.fail('Terraform required')
    t.skip('Terraform unavailable; AA_TERRAFORM_TEST_REQUIRED=1 makes this check required')
    return
  }
  const directory = mkdtempSync(join(tmpdir(), 'plether-rpc-range-policy-'))
  const moduleDirectory = join(directory, 'infra', 'terraform')
  const run = args => spawnSync(terraform, args, {
    cwd: moduleDirectory, encoding: 'utf8', env: { ...process.env, TF_IN_AUTOMATION: '1' },
  })
  try {
    mkdirSync(moduleDirectory, { recursive: true })
    mkdirSync(join(directory, 'config'))
    writeFileSync(join(directory, 'config', 'rpc-log-range-policy.json'), JSON.stringify(policy))
    const variables = read('infra/terraform/variables.tf')
    const batchVariable = variables.match(/variable "aa_reconciler_batch_blocks" \{[\s\S]*?\n\}/)?.[0]
    const entryPointVariable = variables.match(/variable "alto_entrypoint_version" \{[\s\S]*?\n\}/)?.[0]
    assert.ok(batchVariable && entryPointVariable, 'load both production inputs for the range guard')
    writeFileSync(join(moduleDirectory, 'main.tf'), `${batchVariable}\n${entryPointVariable}\n${read('infra/terraform/rpc_log_range_policy.tf')}`)
    const init = run(['init', '-backend=false', '-input=false', '-no-color'])
    assert.equal(init.status, 0, init.stderr)
    for (const [batch, accepted] of [
      ['1', true], ['1000', true], ['5000', true],
      ['5001', false], ['10000', false], ['0', false], ['-1', false], ['1.5', false], ['01', false],
    ]) {
      const result = run(['plan', '-input=false', '-lock=false', '-no-color', `-var=aa_reconciler_batch_blocks=${batch}`])
      assert.equal(result.status === 0, accepted, `batch=${batch}\n${result.stderr}`)
      if (!accepted) assert.match(result.stderr, /Resource precondition failed/)
    }
    for (const [batch, accepted] of [['2000', true], ['2001', false]]) {
      const result = run(['plan', '-input=false', '-lock=false', '-no-color', '-var=alto_entrypoint_version=0.9', `-var=aa_reconciler_batch_blocks=${batch}`])
      assert.equal(result.status === 0, accepted, `Kernel batch=${batch}\n${result.stderr}`)
      if (!accepted) assert.match(result.stderr, /cannot exceed the runtime's 2000-block\s+limit/)
    }
    for (const malformed of [{ ...policy, version: 2 }, { ...policy, maxLogRangeBlocks: 0 }, { ...policy, reconcilerDefaultBatchBlocks: 5001 }]) {
      writeFileSync(join(directory, 'config', 'rpc-log-range-policy.json'), JSON.stringify(malformed))
      const result = run(['plan', '-input=false', '-lock=false', '-no-color'])
      assert.notEqual(result.status, 0, JSON.stringify(malformed))
      assert.match(result.stderr, /Resource precondition failed/)
    }
  } finally {
    rmSync(directory, { recursive: true, force: true })
  }
})
