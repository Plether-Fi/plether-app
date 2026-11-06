import test from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync, mkdtempSync, writeFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { spawnSync } from 'node:child_process'

// Exercise the production Terraform variable validation without providers,
// backend state, credentials, or any cloud mutation.
test('recovery origin configuration permits explicit Sepolia loopback only', () => {
  const source = readFileSync(new URL('../infra/terraform/variables.tf', import.meta.url), 'utf8')
  const start = source.indexOf('variable "aa_preparation_recovery_extra_origins" {')
  const end = source.indexOf('\nvariable ', start + 1)
  assert.ok(start >= 0 && end > start)
  const variable = source.slice(start, end)
  const guardSource = readFileSync(new URL('../infra/terraform/rollout_guards.tf', import.meta.url), 'utf8')
  const guards = [...guardSource.matchAll(/    precondition \{[\s\S]*?\n    \}/g)]
    .map(match => match[0]).filter(block => block.includes('var.aa_preparation_recovery_extra_origins'))
  assert.equal(guards.length, 1)
  const directory = mkdtempSync(join(tmpdir(), 'plether-recovery-origins-'))
  const run = args => spawnSync('terraform', args, { cwd: directory, encoding: 'utf8',
    env: { ...process.env, TF_IN_AUTOMATION: '1' } })
  try {
    writeFileSync(join(directory, 'main.tf'), `
variable "environment" { default = "sepolia" }
variable "perps_chain_id" { default = "421614" }
variable "aa_preparation_recovery_origin" { default = "https://app.sepolia.plether.com" }
${variable}
resource "terraform_data" "origins_guard" {
 lifecycle {
${guards[0]}
 }
}
output "origins" { value = var.aa_preparation_recovery_extra_origins }
`)
    const init = run(['init', '-backend=false', '-input=false', '-no-color'])
    assert.equal(init.status, 0, init.stderr)
    for (const [overrides, accepted] of [
      [{}, true],
      [{ aa_preparation_recovery_extra_origins: ['http://127.0.0.1:5192'] }, true],
      [{ aa_preparation_recovery_extra_origins: ['http://localhost:5192', 'http://[::1]:5192'] }, true],
      [{ environment: 'mainnet', perps_chain_id: '1', aa_preparation_recovery_extra_origins: ['http://127.0.0.1:5192'] }, false],
      [{ perps_chain_id: '42161', aa_preparation_recovery_extra_origins: ['http://127.0.0.1:5192'] }, false],
      [{ aa_preparation_recovery_origin: '', aa_preparation_recovery_extra_origins: ['http://127.0.0.1:5192'] }, false],
      ...['http://127.0.0.1.evil.example:5192', 'http://example.com', 'http://0.0.0.0:5192',
        'http://127.0.0.1:5192/path', 'http://owner@127.0.0.1:5192', 'http://*.localhost:5192']
        .map(origin => [{ aa_preparation_recovery_extra_origins: [origin] }, false]),
      [{ aa_preparation_recovery_extra_origins: ['http://127.0.0.1:5192', 'http://127.0.0.1:5192'] }, false],
    ]) {
      writeFileSync(join(directory, 'case.auto.tfvars.json'), JSON.stringify(overrides))
      const result = run(['plan', '-input=false', '-lock=false', '-no-color'])
      assert.equal(result.status === 0, accepted, `${JSON.stringify(overrides)}\n${result.stderr}`)
    }
  } finally { rmSync(directory, { recursive: true, force: true }) }
})
