// Offline test runner: use deployed synthetic pins only in a disposable copy.
// Never copy state, environment variable files or credentials into that copy.
import assert from 'node:assert/strict'
import { copyFileSync, mkdirSync, mkdtempSync, readdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'

assert.equal(process.argv.length, 2, 'Usage: node scripts/test-terraform-bindings.mjs')
const root = fileURLToPath(new URL('..', import.meta.url))
const source = join(root, 'infra/terraform')
const directory = mkdtempSync(join(tmpdir(), 'plether-deployed-terraform-'))
const terraform = process.env.TERRAFORM_BIN ?? 'terraform'
try {
  const run = (module, args) => {
    const result = spawnSync(terraform, args, {
      cwd: module, stdio: 'inherit',
      env: { ...process.env, TF_DATA_DIR: join(module, 'terraform-data'), TF_IN_AUTOMATION: '1' },
    })
    if (result.error) throw result.error
    assert.equal(result.signal, null, `Terraform terminated: ${result.signal}`)
    return result.status ?? 1
  }
  const plugins = process.env.TERRAFORM_PLUGIN_DIR
  const suite = readFileSync(join(source, 'tests/fixtures/deployed-bindings.tftest.hcl'), 'utf8')
  const results = []
  for (const scope of ['guard-unit', 'full-module']) {
    const tree = join(directory, scope)
    const module = join(tree, 'infra/terraform')
    mkdirSync(join(module, 'tests'), { recursive: true })
    mkdirSync(join(tree, 'config/perps'), { recursive: true })
    copyFileSync(join(source, 'tests/fixtures/deployed-core.json'), join(tree, 'config/perps/mock-usdg-prepared.json'))
    if (scope === 'guard-unit') {
      // Execute the entire actual target guard, with only its AA activation
      // context stubbed inactive. Nonempty canonical bindings activate it.
      for (const file of ['variables.tf', 'deployment_target.tf']) copyFileSync(join(source, file), join(module, file))
      writeFileSync(join(module, 'context.tf'), 'locals { native_aa_backend_configured = false }\n')
      writeFileSync(join(module, 'tests/deployed-bindings.tftest.hcl'), suite.slice(suite.indexOf('variables {'))
        .replace(', terraform_data.perps_candle_rollout_guard', '')
        .replace(', terraform_data.lp_settlement_keeper_guard', ''))
    } else {
      for (const file of readdirSync(source)) {
        if (file.endsWith('.tf') || file === '.terraform.lock.hcl') copyFileSync(join(source, file), join(module, file))
      }
      writeFileSync(join(module, 'tests/deployed-bindings.tftest.hcl'), suite)
    }
    console.log(`Verifying deployed bindings: ${scope}`)
    const initialized = run(module, ['init', '-backend=false', '-input=false', '-no-color', ...(plugins && scope === 'full-module' ? [`-plugin-dir=${plugins}`] : [])])
    results.push(initialized === 0 ? run(module, ['test', '-filter=tests/deployed-bindings.tftest.hcl', '-no-color']) : initialized)
  }
  process.exitCode = results.find(status => status !== 0) ?? 0
} finally {
  rmSync(directory, { recursive: true, force: true })
}
